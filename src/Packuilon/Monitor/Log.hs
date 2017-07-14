{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Packuilon.Monitor.Log
  ( filesInDir
  , file
  , logNameParser
  , parsedLog
  , parseOnly
  , Build(..)
  , BuildStatus(..)
  ) where

import Control.Applicative
import Control.Monad (filterM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans
import qualified Data.Attoparsec.Text.Lazy as A
import Data.Foldable (traverse_)
import Data.Machine
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.Directory (doesFileExist, listDirectory)
import System.FilePath.Posix ((</>))
import System.IO.Error (tryIOError)

-- | Representation of a build run.
data Build = Build { buildName :: T.Text
                   , buildStartTime :: EpochTime
                   , buildStatus :: BuildStatus}
  deriving (Show)

-- | Whether the build has finished, and, if so, when and with what success.
data BuildStatus = Running
                 | Passed EpochTime
                 | Failed EpochTime ExitCode
  deriving (Show)

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
file :: MonadIO m => ProcessT m FilePath TL.Text
file = construct go
  where
    go = do
      f <- await <|> stop
      handleIO print yield (TL.readFile f)
      go

-- | Lazy version of @Data.Attoparsec.Text.parseOnly@.
parseOnly :: A.Parser t -> TL.Text -> Either String t
parseOnly p = A.eitherResult . A.parse p

-- | Machinify the parser -- XXX barfs on an error.
parser :: MonadIO m => A.Parser t -> ProcessT m TL.Text t
parser p = loop go
  where
    go x = either (liftIO . putStrLn) yield $ parseOnly p x

type EpochTime = Int

type ExitCode = Int

-- | Parse the name of a log file.
-- | E.g.:
-- |@
-- λ> A.parseOnly logNameParser "inventory-sl6x.unmanaged.json.1499870992.log"
-- Right ("inventory-sl6x.unmanaged",1499870992)
-- |@
logNameParser :: A.Parser (T.Text, EpochTime)
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

-- rabbit2packer: Build finished at 1499871318 (epoch) with exit code 0
-- XXX Doesn't garbage collect properly

-- | Parse a log file for exit code and epoch time.

-- *Warning: does not currently garbage collect properly, because of
-- Attoparsec's backtracking.* Recommend switching to @Scanner@.
logParser :: A.Parser BuildStatus
logParser =
  do
     "rabbit2packer: Build finished at "
     timeStamp <- A.decimal
     " (epoch) with exit code "
     exitCode <- A.decimal
     return $ mkBuildStatus timeStamp exitCode
  <|> (A.anyChar >> logParser)

mkBuildStatus :: EpochTime -> ExitCode -> BuildStatus
mkBuildStatus t e =
  if e == 0
  then Passed t
  else Failed t e

parsedLog :: Process TL.Text BuildStatus
parsedLog = loop go where
  go x = either (yield . const Running) yield $ parseOnly logParser x
