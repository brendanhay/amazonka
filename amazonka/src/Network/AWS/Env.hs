{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module      : Network.AWS.Env
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Environment and AWS specific configuration for the
-- 'Network.AWS.AWS' and 'Control.Monad.Trans.AWS.AWST' monads.
module Network.AWS.Env
    (
    -- * Creating the Environment
      newEnv
    , newEnvWith

    , Env      (..)
    , HasEnv   (..)

    -- * Overrides
    , Config (..)
    , configure
    , override

    -- * Scoped Actions
    , within
    , once
    , retries
    , timeout
    , endpoint
    , succeeds
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Retry
import           Data.IORef
import           Data.Monoid
import           Network.AWS.Auth
import           Network.AWS.Internal.Logger
import           Network.AWS.Types
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status

import           Prelude

-- | An override function to apply to service configuration before use.
newtype Config = Config { configure :: forall s. Service s -> Service s }

instance Monoid Config where
    mempty      = Config id
    mappend a b = Config (configure b . configure a)

-- | The environment containing the parameters required to make AWS requests.
data Env = Env
    { _envRegion      :: !Region
    , _envLogger      :: !Logger
    , _envRetryCheck  :: !(Int -> HttpException -> IO Bool)
    , _envRetryPolicy :: !(Maybe RetryPolicy)
    , _envConfig      :: !Config
    , _envManager     :: !Manager
    , _envEC2         :: !(IORef (Maybe Bool))
    , _envAuth        :: !Auth
    }

-- Note: The strictness annotations aobe are applied to ensure
-- total field initialisation.

class HasEnv a where
    environment   :: Lens' a Env
    {-# MINIMAL environment #-}

    -- | The current region.
    envRegion     :: Lens' a Region

    -- | The function used to output log messages.
    envLogger     :: Lens' a Logger

    -- | The function used to determine if an 'HttpException' should be retried.
    envRetryCheck :: Lens' a (Int -> HttpException -> IO Bool)

    envConfig     :: Lens' a Config

    -- | The 'Manager' used to create and manage open HTTP connections.
    envManager    :: Lens' a Manager

    -- | The credentials used to sign requests for authentication with AWS.
    envAuth       :: Lens' a Auth

    -- | A memoised predicate for whether the underlying host is an EC2 instance.
    envEC2        :: Getter a (IORef (Maybe Bool))

    envRegion     = environment . lens _envRegion      (\s a -> s { _envRegion      = a })
    envLogger     = environment . lens _envLogger      (\s a -> s { _envLogger      = a })
    envRetryCheck = environment . lens _envRetryCheck  (\s a -> s { _envRetryCheck  = a })
    envConfig     = environment . lens _envConfig    (\s a -> s { _envConfig    = a })
    envManager    = environment . lens _envManager     (\s a -> s { _envManager     = a })
    envAuth       = environment . lens _envAuth        (\s a -> s { _envAuth        = a })
    envEC2        = environment . to _envEC2

instance HasEnv Env where
    environment = id

instance ToLog Env where
    build Env{..} = b <> "\n" <> build _envAuth
      where
        b = buildLines
            [ "[Amazonka Env] {"
            , "  region      = " <> build _envRegion
            , "  retry (n=0) = " <> build (join $ ($ 0) . getRetryPolicy <$> _envRetryPolicy)
            , "}"
            ]

-- | Scope an action within the specific 'Region'.
within :: (MonadReader r m, HasEnv r) => Region -> m a -> m a
within r = local (envRegion .~ r)

-- | Scope an action such that any retry logic for the 'Service' is
-- ignored and any requests will at most be sent once.
once :: (MonadReader r m, HasEnv r) => m a -> m a
once = retries (retryAttempts .~ 0)

-- Better name.
retries :: (MonadReader r m, HasEnv r) => (Retry -> Retry) -> m a -> m a
retries f = override (svcRetry %~ f)

-- | Scope an action such that any HTTP response will use this timeout value.
--
-- Default timeouts are chosen by considering:
--
-- * This 'timeout', if set.
--
-- * The related 'Service' timeout for the sent request if set. (Usually 70s)
--
-- * The 'envManager' timeout if set.
--
-- * The default 'ClientRequest' timeout. (Approximately 30s)
--
timeout :: (MonadReader r m, HasEnv r) => Seconds -> m a -> m a
timeout s = override (svcTimeout ?~ s)

-- | Scope an action such that any HTTP requests and signing logic used
-- a modified endpoint.
endpoint :: (MonadReader r m, HasEnv r) => (Endpoint -> Endpoint) -> m a -> m a
endpoint f = override (svcEndpoint %~ (f .))

-- Better name.
succeeds :: (MonadReader r m, HasEnv r) => (Status -> Bool) -> m a -> m a
succeeds f = override (svcStatus .~ f)

-- | Apply a service configuration override.
override :: (MonadReader r m, HasEnv r)
         => (forall s. Service s -> Service s)
         -> m a
         -> m a
override f = local (envConfig <>~ Config f)

-- | Creates a new environment with a new 'Manager' without debug logging
-- and uses 'getAuth' to expand/discover the supplied 'Credentials'.
-- Lenses from 'HasEnv' can be used to further configure the resulting 'Env'.
--
-- Throws 'AuthError' when environment variables or IAM profiles cannot be read.
--
-- /See:/ 'newEnvWith'.
newEnv :: (Applicative m, MonadIO m, MonadCatch m)
       => Region      -- ^ Initial region to operate in.
       -> Credentials -- ^ Credential discovery mechanism.
       -> m Env
newEnv r c = liftIO (newManager conduitManagerSettings)
    >>= newEnvWith r c Nothing

-- | /See:/ 'newEnv'
--
-- Throws 'AuthError' when environment variables or IAM profiles cannot be read.
newEnvWith :: (Applicative m, MonadIO m, MonadCatch m)
           => Region      -- ^ Initial region to operate in.
           -> Credentials -- ^ Credential discovery mechanism.
           -> Maybe Bool  -- ^ Preload memoisation for the underlying EC2 instance check.
           -> Manager
           -> m Env
newEnvWith r c p m = Env r (\_ _ -> return ()) Nothing mempty m
    <$> liftIO (newIORef p)
    <*> getAuth m c
