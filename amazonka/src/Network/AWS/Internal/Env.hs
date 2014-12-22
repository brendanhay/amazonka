{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Network.AWS.Internal.Env
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Env where

import           Control.Lens
import           Control.Retry
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.List                  (intersperse)
import           Data.Monoid
import           Network.AWS.Data           (ToBuilder(..), buildBS)
import           Network.AWS.Types
import           Network.HTTP.Conduit

-- | The environment containing the parameters required to make AWS requests.
data Env = Env
    { _envRegion      :: !Region
    , _envLogger      :: Logger
    , _envRetryCheck  :: Int -> HttpException -> IO Bool
    , _envRetryPolicy :: Maybe RetryPolicy
    , _envManager     :: Manager
    , _envAuth        :: Auth
    }

-- | The current region.
envRegion :: Lens' Env Region
envRegion = lens _envRegion (\s a -> s { _envRegion = a })
{-# INLINE envRegion #-}

-- | The function used to output log messages.
envLogger :: Lens' Env Logger
envLogger = lens _envLogger (\s a -> s { _envLogger = a })
{-# INLINE envLogger #-}

-- | The function used to determine if an 'HttpException' should be retried.
envRetryCheck :: Lens' Env (Int -> HttpException -> IO Bool)
envRetryCheck = lens _envRetryCheck (\s a -> s { _envRetryCheck = a })
{-# INLINE envRetryCheck #-}

-- | The 'RetryPolicy' used to determine backoff/on and retry delay/growth.
envRetryPolicy :: Lens' Env (Maybe RetryPolicy)
envRetryPolicy = lens _envRetryPolicy (\s a -> s { _envRetryPolicy = a })
{-# INLINE envRetryPolicy #-}

-- | The 'Manager' used to create and manage open HTTP connections.
envManager :: Lens' Env Manager
envManager = lens _envManager (\s a -> s { _envManager = a })
{-# INLINE envManager #-}

-- | The credentials used to sign requests for authentication with AWS.
envAuth :: Lens' Env Auth
envAuth = lens _envAuth (\s a -> s { _envAuth = a })
{-# INLINE envAuth #-}

instance ToBuilder Env where
    build Env{..} = mconcat $ intersperse "\n"
        [ "[Amazonka Env] {"
        , "  region      = " <> build _envRegion
        , "  retry (n=0) = " <> maybe "Nothing" policy _envRetryPolicy
        , build . indent $ buildBS _envAuth
        , "}"
        ]
      where
        policy (RetryPolicy f) = "Just " <> build (f 0)

        indent = mappend "  "
               . LBS.concat
               . intersperse "\n  "
               . LBS.lines
