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
import           Data.Foldable         (for_)
import           Data.Maybe            (fromJust, fromMaybe, isJust)
import           Data.Monoid           ((<>))
import           Data.String           (IsString (fromString))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time             (DiffTime, ZonedTime, getCurrentTime)
import           Data.Time             (getCurrentTimeZone, timeZoneMinutes)
import           Data.Time             (utcToZonedTime, zonedTimeToUTC)
import           Data.Time             (zonedTimeZone)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Foreign.C             (CDouble (..), CInt (..), CLong (..))
import           Language.ObjC.Inline  (ObjC, block, defClass)
import           Language.ObjC.Inline  (objcCtxWithClasses, pure', toObjC')
import qualified Language.ObjC.Inline  as C
import           System.Random         (randomIO)

type NSUserNotification = ObjC "NSUserNotification"
type NSUserNotificationCenter = ObjC "NSUserNotificationCenter"

C.context (objcCtxWithClasses [defClass "NSUserNotificationCenter"
                              ,defClass "NSUserNotification"
                              ])
C.import_ "<Foundation/Foundation.h>"
C.import_ "<AppKit/AppKit.h>"
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
                    , notifAppIcon                :: Maybe ByteString
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
                               , notifAppIcon = Nothing
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
  for_ notifAppIcon $ \icon ->
    [block| void { [($raw:(NSUserNotification *notif))
                      setValue: [[NSImage alloc] initWithData: $obj:(NSData *icon)]
                      forKey: @"_identityImage"];
                   [($raw:(NSUserNotification *notif))
                      setValue: @(false)
                      forKey: @"_identityImageHasBorder"];
                  }
          |]

  for_ notifImage $ \img ->
    [C.block| void { ($raw:(NSUserNotification *notif)).contentImage =
                         [[NSImage alloc] initWithData: $obj:(NSData *img)];
     }|]

  for_ notifDeliveryDate $ \ date -> do
    let zone = toEnum $ timeZoneMinutes (zonedTimeZone date) * 60
        secs = fromRational $ toRational $ utcTimeToPOSIXSeconds $ zonedTimeToUTC date
    [block| void {
              NSUserNotification *notif = $raw:(NSUserNotification *notif);
              notif.deliveryDate = [NSDate dateWithTimeIntervalSince1970: $(double secs)];
              notif.deliveryTimeZone = [NSTimeZone timeZoneForSecondsFromGMT: $(NSInteger zone)];
            } |]
  return notif

defaultNotificationSound :: Text
defaultNotificationSound = [pure'| NSString * { NSUserNotificationDefaultSoundName } |]


deliver :: Notification -> IO ()
deliver notif = do
  obj <- newUserNotification notif
  [C.exp| void { [[NSUserNotificationCenter defaultUserNotificationCenter] deliverNotification: $raw:(NSUserNotification *obj)] } |]

schedule :: Notification -> IO ()
schedule notif = do
  zone <- getCurrentTimeZone
  now <- utcToZonedTime zone <$> getCurrentTime
  let notif' | isJust (notifDeliveryDate notif) = notif
             | otherwise = notif { notifDeliveryDate = Just now }
  obj <- newUserNotification notif'
  let name = notifBundleName notif
  [C.exp| void {
              snatchingBundle($obj:(NSString *name),
                              ^void {[[NSUserNotificationCenter defaultUserNotificationCenter] scheduleNotification: $raw:(NSUserNotification *obj)];}) } |]
