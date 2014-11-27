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
import Data.ByteString.Builder (Builder)

data LogLevel
    = Info  -- ^ Informational messages + debug messages + non-streaming response bodies.
    | Debug -- ^ Info level + potentiall sensitive signing metadata.
      deriving (Eq, Ord, Enum, Show)

type Logger = LogLevel -> Builder -> IO ()

info :: MonadIO m => Logger -> Builder -> m ()
info f = liftIO . f Info
{-# INLINE info #-}

debug :: MonadIO m => Logger -> Builder -> m ()
debug f = liftIO . f Debug
{-# INLINE debug #-}
