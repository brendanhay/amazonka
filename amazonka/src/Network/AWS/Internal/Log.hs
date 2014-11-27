{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Internal.Log
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Types and functions for optional logging machinery used during the
-- request, response, and signing life-cycles.
module Network.AWS.Internal.Log where

import Control.Monad.IO.Class
import Data.ByteString.Builder
import Data.Monoid
import System.IO

data LogLevel
    = Info  -- ^ Informational messages + debug messages + non-streaming response bodies.
    | Debug -- ^ Info level + potentiall sensitive signing metadata.
      deriving (Eq, Ord, Enum, Show)

type Logger = LogLevel -> Builder -> IO ()

-- | This is a primitive logger which can be used to log messages to a 'Handle'.
-- A more sophisticated logging library such as tinylog or FastLogger should be
-- used in production code.
newLogger :: MonadIO m => LogLevel -> Handle -> m Logger
newLogger x hd = liftIO $ do
    hSetBinaryMode hd True
    hSetBuffering  hd LineBuffering -- ^ Should be BlockBuffering, but .. concurrency.
    return $ \y b ->
        case (x, y) of
            (_,     Info)  -> hPutBuilder hd (b <> "\n")
            (Debug, Debug) -> hPutBuilder hd (b <> "\n")
            _              -> return ()

info :: MonadIO m => Logger -> Builder -> m ()
info f = liftIO . f Info
{-# INLINE info #-}

debug :: MonadIO m => Logger -> Builder -> m ()
debug f = liftIO . f Debug
{-# INLINE debug #-}
