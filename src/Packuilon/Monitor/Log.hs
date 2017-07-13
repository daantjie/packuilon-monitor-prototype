{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Packuilon.Monitor.Log
  ( filesInDir
  , file
  , logNameParser
  ) where

import Control.Applicative
import Control.Monad (filterM)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Attoparsec.Text.Lazy as A
import Data.Foldable (traverse_)
import Data.Machine
import Data.Monoid ((<>))
import qualified Data.Text as TS (Text)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T
import System.Directory (doesFileExist, listDirectory)
import System.FilePath.Posix ((</>))
import System.IO.Error (tryIOError)

-- | Abstraction to handle IO, in case of errors.
handleIO :: MonadIO m => (IOError -> IO b) -> (a -> m b) -> IO a -> m b
handleIO l r m = (liftIO . tryIOError $ m) >>= either (liftIO . l) r

loop f = construct go
  where
    go = do
      x <- await <|> stop
      f x
      go

-- | Streams out the @FilePath@s of all files in a directory.
-- Doesn't do tilde expansion -- so "~/..." won't work properly.
filesInDir :: MonadIO m => FilePath -> SourceT m FilePath
filesInDir d =
  construct $ do
    handleIO print (traverse_ yield) $
      filterM doesFileExist =<< fmap (d </>) <$> listDirectory d
    stop

-- | Get the contents of files.
file :: MonadIO m => ProcessT m FilePath Text
file = construct go
  where
    go = do
      f <- await <|> stop
      handleIO print yield (T.readFile f)
      go

-- | Parse the name of a log file.
-- | E.g.:
-- |@
-- λ> A.parseOnly logNameParser "inventory-sl6x.unmanaged.json.1499870992.log"
-- Right ("inventory-sl6x.unmanaged",1499870992)
-- |@
logNameParser :: A.Parser (TS.Text, Int)
logNameParser = do
  personality <- tillDot
  "."
  managedStatus <- tillDot
  ".json."
  timeStamp <- A.decimal
  ".log"
  return (personality <> "." <> managedStatus, timeStamp)
  where
    tillDot = A.takeWhile (/= '.')
