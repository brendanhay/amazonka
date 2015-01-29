{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Internal.Log
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Builder
import Data.Monoid
import Network.AWS.Data
import Network.AWS.Types
import System.IO

-- | This is a primitive logger which can be used to log messages to a 'Handle'.
-- A more sophisticated logging library such as tinylog or FastLogger should be
-- used in production code.
newLogger :: MonadIO m => LogLevel -> Handle -> m Logger
newLogger x hd = liftIO $ do
    hSetBinaryMode hd True
    hSetBuffering  hd LineBuffering
    return $ \y b ->
        when (x >= y) $
            hPutBuilder hd (b <> "\n")

info :: (MonadIO m, ToBuilder a) => Logger -> a -> m ()
info f = liftIO . f Info . build
{-# INLINE info #-}

debug :: (MonadIO m, ToBuilder a) => Logger -> a -> m ()
debug f = liftIO . f Debug . build
{-# INLINE debug #-}

trace :: (MonadIO m, ToBuilder a) => Logger -> a -> m ()
trace f = liftIO . f Trace . build
{-# INLINE trace #-}
