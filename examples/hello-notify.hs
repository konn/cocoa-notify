{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Concurrent
import System.Notification.Cocoa

main :: IO ()
main = do
  schedule "Hello!" { notifSubtitle = Just "How do you do?"
                   , notifInformativeText = "Nice!"
                   }
  threadDelay $ 10 * 10^6
