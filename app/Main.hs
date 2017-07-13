{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Packuilon.Monitor.Log
import Data.Machine
import Data.Text.Lazy

main :: IO ()
main = print =<< (runT $ construct (yield "/home/daniel/t/yyy") ~> file ~> parser logParser)
