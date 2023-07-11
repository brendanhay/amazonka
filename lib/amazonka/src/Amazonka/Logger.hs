-- |
-- Module      : Amazonka.Logger
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Types and functions for constructing loggers and emitting log messages.
module Amazonka.Logger
  ( -- * Constructing a Logger
    Logger,
    newLogger,

    -- * Levels
    LogLevel (..),
    logError,
    logInfo,
    logDebug,
    logTrace,

    -- * Building Messages
    ToLog (..),
    buildLines,
  )
where

import Amazonka.Data
import Amazonka.Prelude
import qualified Control.Monad as Monad
import qualified Data.ByteString.Builder as Build
import qualified System.IO as IO

-- | A logging function called by various default hooks to log
-- informational and debug messages.
type Logger = LogLevel -> ByteStringBuilder -> IO ()

-- | This is a primitive logger which can be used to log builds to a 'Handle'.
--
-- /Note:/ A more sophisticated logging library such as
-- <http://hackage.haskell.org/package/tinylog tinylog> or
-- <http://hackage.haskell.org/package/fast-logger fast-logger>
-- should be used in production code.
newLogger :: MonadIO m => LogLevel -> IO.Handle -> m Logger
newLogger x hd =
  liftIO $ do
    IO.hSetBuffering hd IO.LineBuffering

    pure $ \y b ->
      Monad.when (x >= y) $
        Build.hPutBuilder hd (b <> "\n")

data LogLevel
  = -- | Info messages supplied by the user - this level is not emitted by the library.
    Info
  | -- | Error messages only.
    Error
  | -- | Useful debug information + info + error levels.
    Debug
  | -- | Includes potentially sensitive signing metadata, and non-streaming response bodies.
    Trace
  deriving stock (Eq, Ord, Enum, Show, Generic)

instance FromText LogLevel where
  fromText = \case
    "info" -> pure Info
    "error" -> pure Error
    "debug" -> pure Debug
    "trace" -> pure Trace
    other -> Left ("Failure parsing LogLevel from " ++ show other)

instance ToText LogLevel where
  toText = \case
    Info -> "info"
    Error -> "error"
    Debug -> "debug"
    Trace -> "trace"

instance ToByteString LogLevel

logError, logInfo, logDebug, logTrace :: (MonadIO m, ToLog a) => Logger -> a -> m ()
logError f = liftIO . f Error . build
logInfo f = liftIO . f Info . build
logDebug f = liftIO . f Debug . build
logTrace f = liftIO . f Trace . build
