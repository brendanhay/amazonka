{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.Logger
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Types and functions for optional logging machinery used during the
-- request, response, and signing life-cycles.
module Network.AWS.Logger
    (
    -- * Logging
      Logger
    , newLogger
    -- ** Levels
    , LogLevel  (..)
    , logError
    , logInfo
    , logDebug
    , logTrace
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString.Builder      (Builder)
import qualified Data.ByteString.Lazy.Builder as Build
import           Data.Monoid
import           Network.AWS.Data.ByteString
import           System.IO

data LogLevel
    = Trace -- ^ Includes potentially sensitive signing metadata, and non-streaming response bodies.
    | Debug -- ^ Useful debug information + info + error levels.
    | Info  -- ^ Info messages supplied by the user - this level is not emitted by the library.
    | Error -- ^ Error messages only.
      deriving (Eq, Ord, Enum, Show)

type Logger = LogLevel -> Builder -> IO ()

-- | This is a primitive logger which can be used to log messages to a 'Handle'.
--
-- /Note/: A more sophisticated logging library such as tinylog or FastLogger
-- should be used in production code.
newLogger :: MonadIO m => LogLevel -> Handle -> m Logger
newLogger x hd = liftIO $ do
    hSetBinaryMode hd True
    hSetBuffering  hd LineBuffering
    return $ \y b ->
        when (x >= y) $
            Build.hPutBuilder hd (b <> "\n")

logError, logInfo, logDebug, logTrace :: (MonadIO m, ToBuilder a) => Logger -> a -> m ()
logError f = liftIO . f Error . build
logInfo  f = liftIO . f Info  . build
logDebug f = liftIO . f Debug . build
logTrace f = liftIO . f Trace . build
