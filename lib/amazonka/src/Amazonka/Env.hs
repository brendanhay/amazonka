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
    newEnvWith,
    Env' (..),
    Env,
    EnvNoAuth,
    envAuthMaybe,

    -- * Overriding Default Configuration
    authenticate,
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

import Amazonka.Auth
import Amazonka.Lens ((.~), (?~))
import Amazonka.Prelude
import Amazonka.Types
import Control.Lens (Lens)
import qualified Data.Function as Function
import Data.Monoid (Dual (..), Endo (..))
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Conduit as Client.Conduit

type Env = Env' Identity

type EnvNoAuth = Env' Proxy

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
-- /See:/ 'newEnvWith'.
newEnv ::
  MonadIO m =>
  -- | Credential discovery mechanism.
  Credentials ->
  m Env
newEnv c =
  liftIO (Client.newManager Client.Conduit.tlsManagerSettings)
    >>= authenticate c . newEnvWith

-- | Generate an environment without credentials, which may only make
-- unsigned requests.
--
-- This is useful for the STS
-- <https://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRoleWithWebIdentity.html AssumeRoleWithWebIdentity>
-- operation, which needs to make an unsigned request to pass the
-- token from an identity provider.
newEnvNoAuth :: MonadIO m => m EnvNoAuth
newEnvNoAuth =
  newEnvWith <$> liftIO (Client.newManager Client.Conduit.tlsManagerSettings)

-- | Construct a default 'EnvNoAuth' from a HTTP 'Client.Manager'.
newEnvWith :: Client.Manager -> EnvNoAuth
newEnvWith m =
  Env
    { _envRegion = NorthVirginia,
      _envLogger = \_ _ -> pure (),
      _envRetryCheck = retryConnectionFailure 3,
      _envOverride = mempty,
      _envManager = m,
      _envAuth = Proxy
    }

-- | /See:/ 'newEnv'
--
-- Throws 'AuthError' when environment variables or IAM profiles cannot be read.
authenticate ::
  (MonadIO m, Foldable withAuth) =>
  -- | Credential discovery mechanism.
  Credentials ->
  -- | Previous environment.
  Env' withAuth ->
  m Env
authenticate c env@Env {..} = do
  (a, fromMaybe NorthVirginia -> r) <- getAuth env c

  pure $ Env {_envRegion = r, _envAuth = Identity a, ..}

-- | Get "the" 'Auth' from an 'Env'', if we can.
envAuthMaybe :: Foldable withAuth => Env' withAuth -> Maybe Auth
envAuthMaybe = foldr (const . Just) Nothing . _envAuth

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
