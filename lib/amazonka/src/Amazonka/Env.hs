-- |
-- Module      : Amazonka.Env
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Environment and AWS specific configuration needed to perform AWS
-- requests.
module Amazonka.Env
  ( -- * Creating the Environment
    newEnv,
    newEnvNoAuth,
    newEnvFromManager,
    Env' (..),
    Env,
    EnvNoAuth,
    envAuthMaybe,
    lookupRegion,

    -- * Overriding Default Configuration
    override,
    configure,

    -- * Scoped Actions
    within,
    once,
    timeout,

    -- * 'Env' Lenses
    -- $envLenses
    envRegion,
    envLogger,
    envRetryCheck,
    envOverride,
    envManager,
    envAuth,

    -- * Retry HTTP Exceptions
    retryConnectionFailure,
  )
where

import Amazonka.Lens ((.~), (?~))
import Amazonka.Prelude
import Amazonka.Types
import Control.Lens (Lens)
import qualified Data.Function as Function
import Data.Monoid (Dual (..), Endo (..))
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Conduit as Client.Conduit
import System.Environment as Environment

type Env = Env' Identity

type EnvNoAuth = Env' Proxy

-- | The environment containing the parameters required to make AWS requests.
--
-- This type tracks whether or not we have credentials at the type
-- level, to avoid "presigning" requests when we lack auth
-- information.
data Env' withAuth = Env
  { _envRegion :: Region,
    _envLogger :: Logger,
    _envRetryCheck :: Int -> Client.HttpException -> Bool,
    _envOverride :: Dual (Endo Service),
    _envManager :: Client.Manager,
    _envAuth :: withAuth Auth
  }
  deriving stock (Generic)

-- | Creates a new environment with a new 'Manager' without debug logging
-- and uses 'getAuth' to expand/discover the supplied 'Credentials'.
-- Lenses can be used to further configure the resulting 'Env'.
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
-- /See:/ 'newEnvFromManager'.
newEnv ::
  MonadIO m =>
  -- | Credential discovery mechanism.
  (EnvNoAuth -> m Env) ->
  m Env
newEnv authenticate =
  liftIO (Client.newManager Client.Conduit.tlsManagerSettings)
    >>= authenticate . newEnvFromManager

-- | Generate an environment without credentials, which may only make
-- unsigned requests. This sets the region based on the @AWS_REGION@
-- environment variable, or 'NorthVirginia' if unset.
--
-- This is useful for the STS
-- <https://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRoleWithWebIdentity.html AssumeRoleWithWebIdentity>
-- operation, which needs to make an unsigned request to pass the
-- token from an identity provider.
newEnvNoAuth :: MonadIO m => m EnvNoAuth
newEnvNoAuth = do
  manager <- liftIO $ Client.newManager Client.Conduit.tlsManagerSettings
  let env = newEnvFromManager manager
  mRegion <- lookupRegion
  pure $ case mRegion of
    Nothing -> env
    Just r -> env {_envRegion = r}

-- | Construct a default 'EnvNoAuth' from a HTTP 'Client.Manager'.
newEnvFromManager :: Client.Manager -> EnvNoAuth
newEnvFromManager m =
  Env
    { _envRegion = NorthVirginia,
      _envLogger = \_ _ -> pure (),
      _envRetryCheck = retryConnectionFailure 3,
      _envOverride = mempty,
      _envManager = m,
      _envAuth = Proxy
    }

-- | Get "the" 'Auth' from an 'Env'', if we can.
envAuthMaybe :: Foldable withAuth => Env' withAuth -> Maybe Auth
envAuthMaybe = foldr (const . Just) Nothing . _envAuth

-- | Look up the region in the @AWS_REGION@ environment variable.
lookupRegion :: MonadIO m => m (Maybe Region)
lookupRegion =
  liftIO $
    Environment.lookupEnv "AWS_REGION" <&> \case
      Nothing -> Nothing
      Just "" -> Nothing
      Just t -> Just . Region' $ Text.pack t

-- | Retry the subset of transport specific errors encompassing connection
-- failure up to the specific number of times.
retryConnectionFailure :: Int -> Int -> Client.HttpException -> Bool
retryConnectionFailure limit n = \case
  Client.InvalidUrlException {} -> False
  Client.HttpExceptionRequest _ ex
    | n >= limit -> False
    | otherwise ->
      case ex of
        Client.NoResponseDataReceived -> True
        Client.ConnectionTimeout -> True
        Client.ConnectionClosed -> True
        Client.ConnectionFailure {} -> True
        Client.InternalException {} -> True
        _other -> False

-- | Provide a function which will be added to the existing stack
-- of overrides applied to all service configurations.
override :: (Service -> Service) -> Env -> Env
override f env = env {_envOverride = _envOverride env <> Dual (Endo f)}

-- | Configure a specific service. All requests belonging to the
-- supplied service will use this configuration instead of the default.
--
-- It's suggested you modify the default service configuration,
-- such as @Amazonka.DynamoDB.dynamoDB@.
configure :: Service -> Env -> Env
configure s = override f
  where
    f x
      | Function.on (==) _serviceAbbrev s x = s
      | otherwise = x

-- | Scope an action within the specific 'Region'.
within :: Region -> Env -> Env
within r env = env {_envRegion = r}

-- | Scope an action such that any retry logic for the 'Service' is
-- ignored and any requests will at most be sent once.
once :: Env -> Env
once = override (serviceRetry . retryAttempts .~ 0)

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
timeout :: Seconds -> Env -> Env
timeout n = override (serviceTimeout ?~ n)

-- $envLenses
--
-- We provide lenses for 'Env'', though you are of course free to use
-- the @generic-lens@ package.

envRegion :: Lens' (Env' withAuth) Region
envRegion f env = f (_envRegion env) <&> \r -> env {_envRegion = r}

envLogger :: Lens' (Env' withAuth) Logger
envLogger f env = f (_envLogger env) <&> \l -> env {_envLogger = l}

envRetryCheck :: Lens' (Env' withAuth) (Int -> Client.HttpException -> Bool)
envRetryCheck f env =
  f (_envRetryCheck env) <&> \rc -> env {_envRetryCheck = rc}

envOverride :: Lens' (Env' withAuth) (Dual (Endo Service))
envOverride f env = f (_envOverride env) <&> \o -> env {_envOverride = o}

envManager :: Lens' (Env' withAuth) Client.Manager
envManager f env = f (_envManager env) <&> \m -> env {_envManager = m}

envAuth ::
  Lens (Env' withAuth) (Env' withAuth') (withAuth Auth) (withAuth' Auth)
envAuth f env = f (_envAuth env) <&> \a -> env {_envAuth = a}
