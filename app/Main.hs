{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Yesod
import Packuilon.Monitor.Log
import Packuilon.Monitor.Generate
import Data.Machine
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Text.Cassius

data Monitor = Monitor
instance Yesod Monitor

mkYesod "Monitor" [parseRoutes|
/                  DashboardR  GET
/build/#T.Text     BuildR      GET
-- /style.css         StyleR      GET
|]

getDashboardR :: Handler Html
getDashboardR = defaultLayout $ do
  setTitle "Packuilon Monitor"
  toWidget monitorStyle
  (tabulateBuilds <$> getBuilds) >>= toWidgetBody

getBuilds :: WidgetT Monitor IO [Build]
getBuilds = liftIO . fmap (fmap $ Build "xxx" 0) . runT $ filesInDir "/home/htv52873/logs" ~> file ~> parsedLog

getBuildR :: T.Text -> Handler Html
getBuildR b = defaultLayout [whamlet|This should show the log file for #{b}.|]

main :: IO ()
main = warp 3000 Monitor
