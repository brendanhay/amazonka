-- |
-- Module      : Network.AWS.Internal.Env
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Environment and AWS specific configuration for the
-- 'Network.AWS.AWS' and 'Control.Monad.Trans.AWS.AWST' monads.
module Network.AWS.Internal.Env
  ( -- * Creating the Environment
    newEnv,
    newEnvWith,
    Env (..),

    -- * Overriding Default Configuration
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

import qualified Data.Function as Function
import Data.Monoid (Dual (..), Endo (..))
import Network.AWS.Auth
import Network.AWS.Internal.Logger
import Network.AWS.Internal.Lens ((.~), (?~))
import Network.AWS.Internal.Prelude
import Network.AWS.Types
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Conduit as Client.Conduit

-- | The environment containing the parameters required to make AWS requests.
data Env = Env
  { envRegion :: Region,
    envLogger :: Logger,
    envRetryCheck :: Int -> Client.HttpException -> Bool,
    envOverride :: Dual (Endo Service),
    envManager :: Client.Manager,
    envAuth :: Auth
  }
  deriving stock (Generic)

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
    >>= newEnvWith c

-- | /See:/ 'newEnv'
--
-- The 'Maybe' 'Bool' parameter is used by the EC2 instance check. By passing a
-- value of 'Nothing', the check will be performed. 'Just' 'True' would cause
-- the check to be skipped and the host treated as an EC2 instance.
--
-- Throws 'AuthError' when environment variables or IAM profiles cannot be read.
newEnvWith ::
  MonadIO m =>
  -- | Credential discovery mechanism.
  Credentials ->
  -- | Preload the EC2 instance check.
  Client.Manager ->
  m Env
newEnvWith c m = do
  (a, fromMaybe NorthVirginia -> r) <- getAuth m c

  pure $ Env r (\_ _ -> pure ()) (retryConnectionFailure 3) mempty m a

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
-- such as @Network.AWS.DynamoDB.dynamoDB@.
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
