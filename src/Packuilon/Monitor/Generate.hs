{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Packuilon.Monitor.Generate
  (
    monitorStyle
  , tabulateBuilds
  ) where

import Packuilon.Monitor.Log
import qualified Data.Text as T
import Text.Blaze.Html5
import Control.Monad (forM_, mapM_)
import Text.Hamlet (shamlet)
import Text.Cassius

passedIcon = [shamlet|<span class="passed">&#10004;|]

failedIcon = [shamlet|<span class="failed">&#10008;|]

runningIcon = [shamlet|<span class="running">&#8634;|]

monitorStyle = [cassius|
                  *
                    font-family: "Lucida Sans Typewriter", "Lucida Console", monospace;
                  body
                    padding-left: 30%
                  .passed
                    color: green
                    font-size: 20px
                  .failed
                    color: red
                    font-size: 20px
                  .running
                    color: orange
                    font-size: 20px
                  table
                    border-collapse: separate
                    border-spacing: 15px 10px
               |]

buildToRow :: Build -> Html
buildToRow b = tr $ do
  td $ case buildStatus b of
    Running -> runningIcon
    Passed _ -> passedIcon
    Failed _ _ -> failedIcon
  td $ toHtml $ buildName b
  td $ toHtml $ buildStartTime b
  td $ case buildStatus b of
    Running -> toHtml ("-" :: T.Text)
    Passed t -> toHtml t
    Failed t e -> [shamlet|#{t} with exit code #{e}|]

tabulateBuilds :: [Build] -> Html
tabulateBuilds = table . mapM_ buildToRow
