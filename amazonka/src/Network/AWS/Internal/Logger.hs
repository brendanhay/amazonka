{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.Internal.Logger
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Types and functions for constructing loggers and emitting log messages.
module Network.AWS.Internal.Logger
    (
    -- * Constructing a Logger
      Logger
    , newLogger

    -- * Levels
    , LogLevel (..)
    , logError
    , logInfo
    , logDebug
    , logTrace

    -- * Building Messages
    , ToLog    (..)
    , buildLines
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Builder as Build
import           Data.Monoid
import           Network.AWS.Data.Log
import           Network.AWS.Types
import           System.IO

-- | This is a primitive logger which can be used to log builds to a 'Handle'.
--
-- /Note:/ A more sophisticated logging library such as
-- <http://hackage.haskell.org/package/tinylog tinylog> or
-- <http://hackage.haskell.org/package/fast-logger fast-logger>
-- should be used in production code.
newLogger :: MonadIO m => LogLevel -> Handle -> m Logger
newLogger x hd = liftIO $ do
    hSetBuffering  hd LineBuffering
    return $ \y b ->
        when (x >= y) $
            Build.hPutBuilder hd (b <> "\n")

logError, logInfo, logDebug, logTrace
 :: (MonadIO m, ToLog a) => Logger -> a -> m ()
logError f = liftIO . f Error . build
logInfo  f = liftIO . f Info  . build
logDebug f = liftIO . f Debug . build
logTrace f = liftIO . f Trace . build
