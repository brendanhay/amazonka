{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Network.AWS.Internal.Env
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
import           Data.Monoid
import           Network.AWS.Data.ByteString
import           Network.AWS.Logger
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

class AWSEnv a where
    env :: Lens' a Env
    {-# MINIMAL env #-}

    -- | The current region.
    envRegion      :: Lens' a Region

    -- | The function used to output log messages.
    envLogger      :: Lens' a Logger

    -- | The function used to determine if an 'HttpException' should be retried.
    envRetryCheck  :: Lens' a (Int -> HttpException -> IO Bool)

    -- | The 'RetryPolicy' used to determine backoff\/on and retry delay\/growth.
    envRetryPolicy :: Lens' a (Maybe RetryPolicy)

    -- | The 'Manager' used to create and manage open HTTP connections.
    envManager     :: Lens' a Manager

    -- | The credentials used to sign requests for authentication with AWS.
    envAuth        :: Lens' a Auth

    envRegion      = env . lens _envRegion      (\s a -> s { _envRegion      = a })
    envLogger      = env . lens _envLogger      (\s a -> s { _envLogger      = a })
    envRetryCheck  = env . lens _envRetryCheck  (\s a -> s { _envRetryCheck  = a })
    envRetryPolicy = env . lens _envRetryPolicy (\s a -> s { _envRetryPolicy = a })
    envManager     = env . lens _envManager     (\s a -> s { _envManager     = a })
    envAuth        = env . lens _envAuth        (\s a -> s { _envAuth        = a })

instance AWSEnv Env where
    env = id

instance ToBuilder Env where
    build Env{..} = b <> "\n" <> build _envAuth
      where
        b = buildLines
            [ "[Amazonka Env] {"
            , "  region      = " <> build _envRegion
            , "  retry (n=0) = " <> build _envRetryPolicy
            , "}"
            ]
