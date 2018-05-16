{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE CPP               #-}

-- |
-- Module      : Network.AWS.Env
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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

    -- * Overriding Default Configuration
    , override
    , configure

    -- * Scoped Actions
    , reconfigure
    , within
    , once
    , timeout

    -- * Retry HTTP Exceptions
    , retryConnectionFailure
    ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.Function (on)
import Data.IORef
import Data.Maybe    (fromMaybe)
import Data.Monoid

import Network.AWS.Auth
import Network.AWS.Internal.Logger
import Network.AWS.Lens            (Getter, Lens')
import Network.AWS.Lens            (lens, to)
import Network.AWS.Lens            ((.~), (<>~), (?~))
import Network.AWS.Types
import Network.HTTP.Conduit

-- | The environment containing the parameters required to make AWS requests.
data Env = Env
    { _envRegion     :: !Region
    , _envLogger     :: !Logger
    , _envRetryCheck :: !(Int -> HttpException -> Bool)
    , _envOverride   :: !(Dual (Endo Service))
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

    -- | The currently applied overrides to all 'Service' configuration.
    envOverride   :: Lens' a (Dual (Endo Service))

    -- | The 'Manager' used to create and manage open HTTP connections.
    envManager    :: Lens' a Manager

    -- | The credentials used to sign requests for authentication with AWS.
    envAuth       :: Lens' a Auth

    -- | A memoised predicate for whether the underlying host is an EC2 instance.
    envEC2        :: Getter a (IORef (Maybe Bool))

    envRegion     = environment . lens _envRegion     (\s a -> s { _envRegion     = a })
    envLogger     = environment . lens _envLogger     (\s a -> s { _envLogger     = a })
    envRetryCheck = environment . lens _envRetryCheck (\s a -> s { _envRetryCheck = a })
    envOverride   = environment . lens _envOverride   (\s a -> s { _envOverride   = a })
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

-- | Provide a function which will be added to the existing stack
-- of overrides applied to all service configuration.
--
-- To override a specific service, it's suggested you use
-- either 'configure' or 'reconfigure' with a modified version of the default
-- service, such as @Network.AWS.DynamoDB.dynamoDB@.
override :: HasEnv a => (Service -> Service) -> a -> a
override f = envOverride <>~ Dual (Endo f)

-- | Configure a specific service. All requests belonging to the
-- supplied service will use this configuration instead of the default.
--
-- It's suggested you use a modified version of the default service, such
-- as @Network.AWS.DynamoDB.dynamoDB@.
--
-- /See:/ 'reconfigure'.
configure :: HasEnv a => Service -> a -> a
configure s = override f
  where
    f x | on (==) _svcAbbrev s x = s
        | otherwise              = x

-- | Scope an action such that all requests belonging to the supplied service
-- will use this configuration instead of the default.
--
-- It's suggested you use a modified version of the default service, such
-- as @Network.AWS.DynamoDB.dynamoDB@.
--
-- /See:/ 'configure'.
reconfigure :: (MonadReader r m, HasEnv r) => Service -> m a -> m a
reconfigure = local . configure

-- | Scope an action within the specific 'Region'.
within :: (MonadReader r m, HasEnv r) => Region -> m a -> m a
within r = local (envRegion .~ r)

-- | Scope an action such that any retry logic for the 'Service' is
-- ignored and any requests will at most be sent once.
once :: (MonadReader r m, HasEnv r) => m a -> m a
once = local (override (serviceRetry . retryAttempts .~ 0))

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
timeout :: (MonadReader r m, HasEnv r) => Seconds -> m a -> m a
timeout s = local (override (serviceTimeout ?~ s))

-- | Creates a new environment with a new 'Manager' without debug logging
-- and uses 'getAuth' to expand/discover the supplied 'Credentials'.
-- Lenses from 'HasEnv' can be used to further configure the resulting 'Env'.
--
-- /Since:/ @1.5.0@ - The region is now retrieved from the @AWS_REGION@ environment
-- variable (identical to official SDKs), or defaults to @us-east-1@.
-- You can override the 'Env' region by using 'envRegion', or the current operation's
-- region by using 'within'.
--
-- /Since:/ @1.3.6@ - The default logic for retrying 'HttpException's now uses
-- 'retryConnectionFailure' to retry specific connection failure conditions up to 3 times.
-- Previously only service specific errors were automatically retried.
-- This can be reverted to the old behaviour by resetting the 'Env' using
-- 'envRetryCheck' lens to @(\\_ _ -> False)@.
--
-- Throws 'AuthError' when environment variables or IAM profiles cannot be read.
--
-- /See:/ 'newEnvWith'.
newEnv :: (Applicative m, MonadIO m, MonadCatch m)
       => Credentials -- ^ Credential discovery mechanism.
       -> m Env
newEnv c =
    liftIO (newManager tlsManagerSettings)
        >>= newEnvWith c Nothing

-- | /See:/ 'newEnv'
--
-- The 'Maybe' 'Bool' parameter is used by the EC2 instance check. By passing a
-- value of 'Nothing', the check will be performed. 'Just' 'True' would cause
-- the check to be skipped and the host treated as an EC2 instance.
--
-- Throws 'AuthError' when environment variables or IAM profiles cannot be read.
newEnvWith :: (Applicative m, MonadIO m, MonadCatch m)
           => Credentials -- ^ Credential discovery mechanism.
           -> Maybe Bool  -- ^ Preload the EC2 instance check.
           -> Manager
           -> m Env
newEnvWith c p m = do
    (a, fromMaybe NorthVirginia -> r) <- getAuth m c
    Env r (\_ _ -> pure ()) (retryConnectionFailure 3) mempty m
        <$> liftIO (newIORef p)
        <*> pure a

-- | Retry the subset of transport specific errors encompassing connection
-- failure up to the specific number of times.
retryConnectionFailure :: Int -> Int -> HttpException -> Bool
#if MIN_VERSION_http_client(0,5,0)
retryConnectionFailure _     _ InvalidUrlException {}      = False
retryConnectionFailure limit n (HttpExceptionRequest _ ex)
    | n >= limit = False
    | otherwise  =
        case ex of
            NoResponseDataReceived -> True
            ConnectionTimeout      -> True
            ConnectionClosed       -> True
            ConnectionFailure {}   -> True
            InternalException {}   -> True
            _                      -> False
#else
retryConnectionFailure limit n = \case
    _ | n >= limit                -> False
    NoResponseDataReceived        -> True
    FailedConnectionException  {} -> True
    FailedConnectionException2 {} -> True
    TlsException               {} -> True
    _                             -> False
#endif
