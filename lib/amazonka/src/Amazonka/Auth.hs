-- |
-- Module      : Amazonka.Auth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Explicitly specify your Amazon AWS security credentials, or retrieve them
-- from the underlying OS.
--
-- The format of environment variables and the credentials file follows the official
-- <http://blogs.aws.amazon.com/security/post/Tx3D6U6WSFGOK2H/A-New-and-Standardized-Way-to-Manage-Credentials-in-the-AWS-SDKs AWS SDK guidelines>.
module Amazonka.Auth
  ( -- * Authentication

    -- ** Retrieving Authentication
    Auth (..),
    withAuth,

    -- ** Automatically Fetching Credentials
    discover,

    -- ** Credential Providers
    runCredentialChain,
    fromKeys,
    fromSession,
    fromTemporarySession,
    fromKeysEnv,
    fromFilePath,
    fromFileEnv,
    fromContainer,
    fromContainerEnv,
    fromAssumedRole,
    fromWebIdentity,
    fromWebIdentityEnv,
    fromDefaultInstanceProfile,
    fromNamedInstanceProfile,
    fromSSO,

    -- ** Keys
    AccessKey (..),
    SecretKey (..),
    SessionToken (..),

    -- ** Handling Errors
    AsAuthError (..),
    AuthError (..),

    -- * Env'
    -- $env
    Env,
    EnvNoAuth,
    Env' (..),
  )
where

import Amazonka.Auth.ConfigFile (fromFileEnv, fromFilePath)
import Amazonka.Auth.Container (fromContainer, fromContainerEnv)
import Amazonka.Auth.Exception
import Amazonka.Auth.InstanceProfile (fromDefaultInstanceProfile, fromNamedInstanceProfile)
import Amazonka.Auth.Keys (fromKeys, fromKeysEnv, fromSession, fromTemporarySession)
import Amazonka.Auth.SSO (fromSSO)
import Amazonka.Auth.STS (fromAssumedRole, fromWebIdentity, fromWebIdentityEnv)
import Amazonka.Env (Env, Env' (..), EnvNoAuth)
import Amazonka.Prelude
import Amazonka.Types
import Control.Monad.Catch (MonadCatch (..), throwM)

-- | Attempt to fetch credentials in a way similar to the official AWS
-- SDKs. The <https://github.com/aws/aws-sdk-cpp/blob/fb8cbebf2fd62720b65aeff841ad2950e73d8ebd/Docs/Credentials_Providers.md#default-credential-provider-chain C++ SDK>
-- lists the following sequence:
--
-- *   Check environment variables for keys provided directly
--     (@AWS_ACCESS_KEY_ID@, @AWS_SECRET_ACCESS_KEY@, optionally
--     @AWS_SESSION_TOKEN@)
--
-- *   Check credentials/config files for authentication information,
--     respecting the @AWS_PROFILE@ environment variable.
--
-- *   Exchange a Web Identity for AWS Credentials using
--     @sts:AssumeRoleWithWebIdentity@, respecting the
--     @AWS_WEB_IDENTITY_TOKEN_FILE@, @AWS_ROLE_ARN@, and optionally the
--     @AWS_ROLE_SESSION_NAME@ environment variables.
--
-- *   Retrieve credentials from the ECS Container Agent if the
--     @AWS_CONTAINER_CREDENTIALS_RELATIVE_URI@ environment variable is
--     set.
--
-- *   Attempt to retrieve the first available IAM profile from the instance
--     identity document, by trying to contact an IMDS at
--     http://169.254.169.254. On success, also use this to set the 'Region'.
--
--     __NOTE__: This is not 100% consistent with the AWS SDKs,
--     which does not attempt to query the ECS service if either
--     @AWS_CONTAINER_CREDENTIALS_RELATIVE_URI@ or
--     @AWS_CONTAINER_CREDENTIALS_FULL_URI@ are set.
--
--     /See:/ https://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/com/amazonaws/auth/EC2ContainerCredentialsProviderWrapper.html
discover ::
  (MonadCatch m, MonadIO m, Foldable withAuth) =>
  Env' withAuth ->
  m Env
discover =
  runCredentialChain
    [ fromKeysEnv,
      fromFileEnv,
      fromWebIdentityEnv,
      fromContainerEnv,
      fromDefaultInstanceProfile
    ]

-- | Compose a list of credential-providing functions by testing each
-- until one returns successfully. If they throw 'AuthError', the next
-- function in the chain will be tried. Throws
-- 'CredentialChainExhausted' if the list is exhausted.
runCredentialChain :: (MonadCatch m) => [a -> m b] -> a -> m b
runCredentialChain chain env =
  case chain of
    [] -> throwM CredentialChainExhausted
    provider : chain' ->
      provider env `catch` \(_ :: AuthError) -> runCredentialChain chain' env
