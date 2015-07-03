{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Network.AWS.Env
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Env
    (
    -- * Environment
      AWSEnv (..)
    , Env    (..)
    -- ** Creating the environment
    , newEnv
    , newEnvWith
    -- ** Response configuration
    , timeoutFor
    , noRetries
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Retry
import           Data.Monoid
import           Network.AWS.Auth
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Time
import           Network.AWS.Logger
import           Network.AWS.Types
import           Network.HTTP.Conduit

-- | The environment containing the parameters required to make AWS requests.
data Env = Env
    { _envRegion      :: !Region
    , _envLogger      :: Logger
    , _envRetryCheck  :: Int -> HttpException -> IO Bool
    , _envRetryPolicy :: Maybe RetryPolicy
    , _envTimeout     :: Maybe Seconds
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

    -- | A HTTP response timeout override to apply. This defaults to 'Nothing',
    -- and the timeout selection is outlined below.
    --
    -- /Note:/ This exists because the chosen 'Manager' timeout is not updateable
    -- after instantiation of the environment.
    --
    -- Timeouts are chosen by considering:
    --
    -- * This 'envTimeout' if set.
    --
    -- * The related 'Service' timeout for the sent request if set.
    --
    -- * The 'Env's HTTP 'Manager' timeout if set.
    --
    -- * The default 'ClientRequest' timeout (approximately 30s).
    envTimeout     :: Lens' a (Maybe Seconds)

    -- | The 'Manager' used to create and manage open HTTP connections.
    envManager     :: Lens' a Manager

    -- | The credentials used to sign requests for authentication with AWS.
    envAuth        :: Lens' a Auth

    envRegion      = env . lens _envRegion      (\s a -> s { _envRegion      = a })
    envLogger      = env . lens _envLogger      (\s a -> s { _envLogger      = a })
    envRetryCheck  = env . lens _envRetryCheck  (\s a -> s { _envRetryCheck  = a })
    envRetryPolicy = env . lens _envRetryPolicy (\s a -> s { _envRetryPolicy = a })
    envTimeout     = env . lens _envTimeout     (\s a -> s { _envTimeout     = a })
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
            , "  timeout     = " <> build _envTimeout
            , "}"
            ]

-- | This creates a new environment with a new 'Manager',
-- _without_ debug logging and uses 'getAuth'
-- to expand/discover the supplied 'Credentials'.
--
-- Lenses such as 'envLogger' can be used to configure the resulting 'Env'.
--
-- /See:/ 'newEnvWith' to supply an existing HTTP 'Manager'.
newEnv :: MonadIO m
       => Region
       -> Credentials
       -> m (Either String Env)
newEnv r c = liftIO (newManager conduitManagerSettings) >>= newEnvWith r c

-- | /See:/ 'newEnv'
newEnvWith :: MonadIO m
           => Region
           -> Credentials
           -> Manager
           -> m (Either String Env)
newEnvWith r c m = runExceptT $ env' `liftM` ExceptT (getAuth m c)
  where
    env' = Env r logger check Nothing Nothing m

    logger _ _ = return ()
-- FIXME: check the usage of .. check.
    check  _ _ = return True

-- | Returns the possible HTTP response timeout value in microseconds
-- given the timeout configuration sources.
timeoutFor :: AWSEnv a => a -> Service s -> Maybe Int
timeoutFor e s = microseconds <$> (e ^. envTimeout <|> _svcTimeout s)

-- | Updates the settings used by the retry logic to ensure no retries occur.
noRetries :: AWSEnv a => a -> a
noRetries  e = e
    & envRetryPolicy ?~ limitRetries 0
    & envRetryCheck  .~ (\_ _ -> return False)
