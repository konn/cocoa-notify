{-# LANGUAGE DataKinds, OverloadedStrings, QuasiQuotes, RecordWildCards #-}
{-# LANGUAGE TemplateHaskell                                            #-}
module System.Notification.Cocoa
    ( Notification (..)
    , newNotification
    , defaultNotificationSound
    , deliver
    , schedule
    ) where

import           Control.Monad         (when)
import           Data.ByteString       (ByteString)
import           Data.Maybe            (fromJust, fromMaybe, isJust)
import           Data.Monoid           ((<>))
import           Data.String           (IsString (fromString))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time             (DiffTime, ZonedTime, timeZoneMinutes)
import           Data.Time             (zonedTimeToUTC, zonedTimeZone)
import           Data.Time             (getCurrentTime)
import           Data.Time             (utcToZonedTime)
import           Data.Time             (getCurrentTimeZone)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Foreign.C             (CDouble (..), CInt (..), CLong (..))
import           Language.ObjC.Inline  (ObjC, block, defClass)
import           Language.ObjC.Inline  (objcCtxWithClasses)
import           Language.ObjC.Inline  (toObjC')
import qualified Language.ObjC.Inline  as C
import           System.Random         (randomIO)

type NSUserNotification = ObjC "NSUserNotification"
type NSUserNotificationCenter = ObjC "NSUserNotificationCenter"

C.context (objcCtxWithClasses [defClass "NSUserNotificationCenter"
                              ,defClass "NSUserNotification"
                              ])
C.import_ "<Foundation/Foundation.h>"
C.import_ "BundleSnatcher.h"

data Notification = Notification
                    { notifTitle                  :: Text
                    , notifSubtitle               :: Maybe Text
                    , notifInformativeText        :: Text
                    , notifImage                  :: Maybe ByteString
                    , notifIdentifier             :: Maybe Text
                    , notifActionButton           :: Maybe Text
                    , notifOtherButton            :: Maybe Text
                    , notifHasReplyButton         :: Bool
                    , notifDeliveryDate           :: Maybe ZonedTime
                    , notifDeliveryRepeatInterval :: Maybe DiffTime
                    , notifSoundName              :: Maybe Text
                    , notifBundleName             :: Text
                    }
                  deriving (Show)

newNotification :: Notification
newNotification = Notification { notifTitle = ""
                               , notifSubtitle = Nothing
                               , notifInformativeText = ""
                               , notifImage = Nothing
                               , notifIdentifier = Nothing
                               , notifActionButton = Nothing
                               , notifOtherButton = Nothing
                               , notifHasReplyButton = False
                               , notifDeliveryDate = Nothing
                               , notifDeliveryRepeatInterval = Nothing
                               , notifSoundName  = Nothing
                               , notifBundleName = "com.konn-san.dummy-notifier"
                               }

instance IsString Notification where
  fromString str = newNotification { notifTitle = T.pack str }

defaultCenter :: NSUserNotificationCenter
defaultCenter = [C.pure|NSUserNotificationCenter * {
                         [NSUserNotificationCenter defaultUserNotificationCenter]
                        }|]

newUserNotification :: Notification -> IO NSUserNotification
newUserNotification Notification{..} = do
  i <- randomIO
  let subt = fromMaybe "" notifSubtitle
      hasAct = fromIntegral $ fromEnum $ isJust notifActionButton
      hasRepl = fromIntegral $ fromEnum notifHasReplyButton
      actBName = fromMaybe "" notifActionButton
      othBName = fromMaybe "" notifOtherButton
      ident = fromMaybe ("haskell-cocoa-notif-" <> T.pack (show (i :: Int))) notifIdentifier
  sound <- maybe [C.exp|NSString * { nil }|] toObjC' notifSoundName
  notif <- [block| NSUserNotification * {
                 NSUserNotification *notif = [NSUserNotification new];
                 notif.title = $obj:(NSString *notifTitle);
                 notif.subtitle  = $obj:(NSString *subt);
                 notif.informativeText = $obj:(NSString *notifInformativeText);
                 notif.identifier = $obj:(NSString *ident);
                 notif.hasActionButton = $(BOOL hasAct);
                 notif.actionButtonTitle = $obj:(NSString *actBName);
                 notif.otherButtonTitle = $obj:(NSString *othBName);
                 notif.hasReplyButton = $(BOOL hasRepl);
                 notif.soundName = $raw:(NSString *sound);
                 return notif;
               } |]
  when (isJust notifDeliveryDate) $ do
    let date = fromJust notifDeliveryDate
        zone = toEnum $ timeZoneMinutes (zonedTimeZone date) * 60
        secs = fromRational $ toRational $ utcTimeToPOSIXSeconds $ zonedTimeToUTC date
    [block| void {
              NSUserNotification *notif = $raw:(NSUserNotification *notif);
              notif.deliveryDate = [NSDate dateWithTimeIntervalSince1970: $(double secs)];
              NSLog(@"deliv Date set");
              notif.deliveryTimeZone = [NSTimeZone timeZoneForSecondsFromGMT: $(NSInteger zone)];
              NSLog(@"deliv Zone set");
            } |]
  return notif

defaultNotificationSound :: Text
defaultNotificationSound = [pure'| NSString * { NSUserNotificationDefaultSoundName } |]


deliver :: Notification -> IO ()
deliver notif = do
  putStrLn "initing..."
  obj <- newUserNotification notif
  putStrLn "inited. passing..."
  [C.exp| void { [[NSUserNotificationCenter defaultUserNotificationCenter] deliverNotification: $raw:(NSUserNotification *obj)] } |]

schedule :: Notification -> IO ()
schedule notif = do
  putStrLn "initing..."
  zone <- getCurrentTimeZone
  now <- utcToZonedTime zone <$> getCurrentTime
  let notif' | isJust (notifDeliveryDate notif) = notif
             | otherwise = notif { notifDeliveryDate = Just now }
  obj <- newUserNotification notif'
  let name = notifBundleName notif
  putStrLn "inited. passing..."
  [C.exp| void {
              snatchingBundle($obj:(NSString *name),
                              ^void {[[NSUserNotificationCenter defaultUserNotificationCenter] scheduleNotification: $raw:(NSUserNotification *obj)];}) } |]
