{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Packuilon.Monitor.Log
  ( filesInDir
  , file
  ) where

import System.FilePath.Posix ((</>))
import Control.Monad (filterM)
import Control.Applicative
import Data.Machine
import Data.Foldable (traverse_)
import System.Directory (listDirectory, doesFileExist)
import System.IO.Error (tryIOError)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T

-- | Abstraction to handle IO, in case of errors.
handleIO :: MonadIO m => (IOError -> IO b) -> (a -> m b) -> IO a -> m b
handleIO l r m = (liftIO . tryIOError $ m) >>= either (liftIO . l) r

-- | Streams out the @FilePath@s of all files in a directory.
-- Doesn't do tilde expansion -- so "~/..." won't work properly.
filesInDir :: MonadIO m => FilePath -> SourceT m FilePath
filesInDir d = construct $ do
  handleIO print (traverse_ yield) $
    filterM doesFileExist =<<
    fmap (d </>) <$>
    listDirectory d
  stop

-- | Get the contents of files.
file :: MonadIO m => ProcessT m FilePath Text
file = construct go where
  go = do
    f <- await <|> stop
    handleIO print yield (T.readFile f)
    go
