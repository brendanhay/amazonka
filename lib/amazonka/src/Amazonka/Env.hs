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

    -- * Retry HTTP Exceptions
    retryConnectionFailure,
  )
where

import Amazonka.Auth
import Amazonka.Lens ((.~), (?~))
import Amazonka.Logger
import Amazonka.Prelude
import Amazonka.Types
import qualified Data.Function as Function
import Data.Monoid (Dual (..), Endo (..))
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Conduit as Client.Conduit

-- | The environment containing the parameters required to make AWS requests.
--
-- This type tracks whether or not we have credentials at the type
-- level, to avoid "presigning" requests when we lack auth
-- information.
data Env' withAuth = Env
  { envRegion :: Region,
    envLogger :: Logger,
    envRetryCheck :: Int -> Client.HttpException -> Bool,
    envOverride :: Dual (Endo Service),
    envManager :: Client.Manager,
    envAuth :: withAuth Auth
  }
  deriving stock (Generic)

type Env = Env' Identity

type EnvNoAuth = Env' Proxy

-- | Creates a new environment with a new 'Manager' without debug logging
-- and uses 'getAuth' to expand/discover the supplied 'Credentials'.
-- Lenses from 'AWSEnv' can be used to further configure the resulting 'Env'.
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
    { envRegion = NorthVirginia,
      envLogger = \_ _ -> pure (),
      envRetryCheck = retryConnectionFailure 3,
      envOverride = mempty,
      envManager = m,
      envAuth = Proxy
    }

-- | /See:/ 'newEnv'
--
-- Throws 'AuthError' when environment variables or IAM profiles cannot be read.
authenticate ::
  MonadIO m =>
  -- | Credential discovery mechanism.
  Credentials ->
  -- | Previous environment.
  Env' a ->
  m Env
authenticate c Env{..} = do
  (a, fromMaybe NorthVirginia -> r) <- getAuth envManager c

  pure $ Env {envRegion = r, envAuth = Identity a, ..}

-- | Get "the" 'Auth' from an 'Env'', if we can.
envAuthMaybe :: Foldable withAuth => Env' withAuth -> Maybe Auth
envAuthMaybe = foldr (const . Just) Nothing . envAuth

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
override f env = env {envOverride = envOverride env <> Dual (Endo f)}

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
within r env = env {envRegion = r}

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
