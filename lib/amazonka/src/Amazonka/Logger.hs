-- |
-- Module      : Amazonka.Logger
-- Copyright   : (c) 2013-2021 Brendan Hay
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
import Amazonka.Types
import qualified Control.Monad as Monad
import qualified Data.ByteString.Builder as Build
import qualified System.IO as IO

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

logError, logInfo, logDebug, logTrace :: (MonadIO m, ToLog a) => Logger -> a -> m ()
logError f = liftIO . f Error . build
logInfo f = liftIO . f Info . build
logDebug f = liftIO . f Debug . build
logTrace f = liftIO . f Trace . build
