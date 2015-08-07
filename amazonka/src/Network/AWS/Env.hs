{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module      : Network.AWS.Env
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Transformer environment and AWS specific configuration.
module Network.AWS.Env
    (
    -- * Creating the Environment
      newEnv
    , newEnvWith

    , Env    (..)
    , HasEnv (..)

    -- * Scoped Actions
    , within
    , once
    , timeout
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
import           Network.AWS.Logger
import           Network.AWS.Types
import           Network.HTTP.Conduit

import           Prelude

-- | The environment containing the parameters required to make AWS requests.
data Env = Env
    { _envRegion      :: !Region
    , _envLogger      :: !Logger
    , _envRetryCheck  :: !(Int -> HttpException -> IO Bool)
    , _envRetryPolicy :: !(Maybe RetryPolicy)
    , _envTimeout     :: !(Maybe Seconds)
    , _envManager     :: !Manager
    , _envEC2         :: !(IORef (Maybe Bool))
    , _envAuth        :: !Auth
    }

-- Note: The strictness annotations aobe are applied to ensure
-- total field initialisation.

class HasEnv a where
    env            :: Lens' a Env
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
    -- Timeouts are chosen by considering:
    --
    -- * This 'envTimeout', if set.
    --
    -- * The related 'Service' timeout for the sent request if set. (Usually 70s)
    --
    -- * The 'envManager' timeout if set.
    --
    -- * The default 'ClientRequest' timeout. (Approximately 30s)
    --
    envTimeout     :: Lens' a (Maybe Seconds)

    -- | The 'Manager' used to create and manage open HTTP connections.
    envManager     :: Lens' a Manager

    -- | The credentials used to sign requests for authentication with AWS.
    envAuth        :: Lens' a Auth

    -- | A memoised predicate for whether the underlying host an EC2 instance.
    envEC2         :: Getter a (IORef (Maybe Bool))

    envRegion      = env . lens _envRegion      (\s a -> s { _envRegion      = a })
    envLogger      = env . lens _envLogger      (\s a -> s { _envLogger      = a })
    envRetryCheck  = env . lens _envRetryCheck  (\s a -> s { _envRetryCheck  = a })
    envRetryPolicy = env . lens _envRetryPolicy (\s a -> s { _envRetryPolicy = a })
    envTimeout     = env . lens _envTimeout     (\s a -> s { _envTimeout     = a })
    envManager     = env . lens _envManager     (\s a -> s { _envManager     = a })
    envAuth        = env . lens _envAuth        (\s a -> s { _envAuth        = a })
    envEC2         = env . to _envEC2

instance HasEnv Env where
    env = id

instance ToLog Env where
    message Env{..} = b <> "\n" <> message _envAuth
      where
        b = buildLines
            [ "[Amazonka Env] {"
            , "  region      = " <> message _envRegion
            , "  retry (n=0) = " <> message (join $ ($ 0) . getRetryPolicy <$> _envRetryPolicy)
            , "  timeout     = " <> message _envTimeout
            , "}"
            ]

-- | Scope an action within the specific 'Region'.
within :: (MonadReader r m, HasEnv r) => Region -> m a -> m a
within r = local (envRegion .~ r)

-- | Scope an action such that any retry logic for the 'Service' is
-- ignored and any requests will at most be sent once.
once :: (MonadReader r m, HasEnv r) => m a -> m a
once = local $ \e -> e
    & envRetryPolicy ?~ limitRetries 0
    & envRetryCheck  .~ (\_ _ -> return False)

-- | Scope an action such that any HTTP response use this timeout value.
timeout :: (MonadReader r m, HasEnv r) => Seconds -> m a -> m a
timeout s = local (envTimeout ?~ s)

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
newEnvWith r c p m = Env r logger check Nothing Nothing m
    <$> liftIO (newIORef p)
    <*> getAuth m c
  where
    logger _ _ = return ()
    -- FIXME: verify the usage of check.
    check  _ _ = return True
