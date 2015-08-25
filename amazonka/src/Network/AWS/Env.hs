{-# LANGUAGE FlexibleContexts  #-}
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

    -- * Configuration Overrides
    , override
    , configure

    -- * Scoped Actions
    , within
    , once
    , timeout
    , endpoint
    , signer
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

import           Prelude

-- | The environment containing the parameters required to make AWS requests.
data Env = Env
    { _envRegion     :: !Region
    , _envLogger     :: !Logger
    , _envRetryCheck :: !(Int -> HttpException -> Bool)
    , _envConfig     :: !Config
    , _envManager    :: !Manager
    , _envEC2        :: !(IORef (Maybe Bool))
    , _envAuth       :: !Auth
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
    envRetryCheck :: Lens' a (Int -> HttpException -> Bool)

    envConfig     :: Lens' a Config

    -- | The 'Manager' used to create and manage open HTTP connections.
    envManager    :: Lens' a Manager

    -- | The credentials used to sign requests for authentication with AWS.
    envAuth       :: Lens' a Auth

    -- | A memoised predicate for whether the underlying host is an EC2 instance.
    envEC2        :: Getter a (IORef (Maybe Bool))

    envRegion     = environment . lens _envRegion     (\s a -> s { _envRegion     = a })
    envLogger     = environment . lens _envLogger     (\s a -> s { _envLogger     = a })
    envRetryCheck = environment . lens _envRetryCheck (\s a -> s { _envRetryCheck = a })
    envConfig     = environment . lens _envConfig     (\s a -> s { _envConfig     = a })
    envManager    = environment . lens _envManager    (\s a -> s { _envManager    = a })
    envAuth       = environment . lens _envAuth       (\s a -> s { _envAuth       = a })
    envEC2        = environment . to _envEC2

instance HasEnv Env where
    environment = id

instance ToLog Env where
    build Env{..} = b <> "\n" <> build _envAuth
      where
        b = buildLines
            [ "[Amazonka Env] {"
            , "  region = " <> build _envRegion
            , "}"
            ]

-- | An override function to apply to service configuration before use.
newtype Config = Config { configure :: Service -> Service }

instance Monoid Config where
    mempty      = Config id
    mappend a b = Config (configure b . configure a)

-- | Add a service configuration override to the environment's current scope.
override :: (MonadReader r m, HasEnv r) => (Service -> Service) -> m a -> m a
override f = local (envConfig <>~ Config f)

-- | Scope an action within the specific 'Region'.
within :: (MonadReader r m, HasEnv r) => Region -> m a -> m a
within r = local (envRegion .~ r)

-- | Scope an action such that any retry logic for the 'Service' is
-- ignored and any requests will at most be sent once.
once :: (MonadReader r m, HasEnv r) => m a -> m a
once = override (svcRetry . retryAttempts .~ 0)

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

signer :: (MonadReader r m, HasEnv r) => (forall v. Signer v) -> m a -> m a
signer v = override (\x -> x { _svcSigner = v })

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
           -> Maybe Bool  -- ^ Dictate if the instance is running on EC2. (Preload memoisation.)
           -> Manager
           -> m Env
newEnvWith r c p m =
    Env r logger check mempty m <$> liftIO (newIORef p) <*> getAuth m c
  where
    logger _ _ = return ()
    check  _ _ = True
