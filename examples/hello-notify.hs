{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where
import Data.ByteString           (ByteString)
import Data.FileEmbed            (embedFile)
import System.Notification.Cocoa

icon :: ByteString
icon = $(embedFile "data/icon.png")

content = $(embedFile "data/content.png")

main :: IO ()
main =
  schedule "Hello!" { notifSubtitle = Just "How do you do?"
                    , notifInformativeText = "Nice!"
                    , notifSoundName = Just defaultNotificationSound
                    , notifAppIcon   = Just icon
                    , notifImage     = Just content
                    }
