{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Amazonka.Auth
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    getAuth,
    Credentials (..),
    Auth (..),

    -- ** Credentials
    -- $credentials
    fromKeys,
    fromSession,
    fromTemporarySession,
    fromKeysEnv,
    fromFileEnv,
    fromContainerEnv,
    fromDefaultInstanceProfile,
    fromNamedInstanceProfile,

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
import Amazonka.Auth.Container (fromContainerEnv)
import Amazonka.Auth.Exception
import Amazonka.Auth.InstanceProfile (fromDefaultInstanceProfile, fromNamedInstanceProfile)
import Amazonka.Auth.Keys (fromKeys, fromKeysEnv, fromSession, fromTemporarySession)
import Amazonka.Auth.STS (fromWebIdentityEnv)
import Amazonka.Data
import Amazonka.EC2.Metadata
import Amazonka.Lens (catching, catching_, throwingM)
import Amazonka.Prelude
import Amazonka.Types
import Control.Monad.Catch (MonadCatch (..), throwM)
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.ByteString.Char8 as BS8
import Data.Monoid (Dual, Endo)
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Client
import qualified System.Environment as Environment

-- | Default region environment variable
envRegion ::
  -- | AWS_REGION
  Text
envRegion = "AWS_REGION"

-- $credentials
-- 'getAuth' is implemented using the following @from*@-styled functions below.
-- Both 'fromKeys' and 'fromSession' can be used directly to avoid the 'MonadIO'
-- constraint.

-- | Determines how AuthN/AuthZ information is retrieved.
data Credentials
  = -- | Explicit access and secret keys. See 'fromKeys'.
    FromKeys AccessKey SecretKey
  | -- | Explicit access key, secret key and a session token. See 'fromSession'.
    FromSession AccessKey SecretKey SessionToken
  | -- | Lookup environment variables for access key, secret key,
    -- an optional session token, and an optional region.
    FromEnv
  | -- | An IAM Profile name to lookup from the local EC2 instance-data.
    -- Environment variables to lookup for the access key, secret key and
    -- optional session token.
    FromProfile Text
  | -- | A credentials profile name (the INI section), the path to the AWS
    -- <http://blogs.aws.amazon.com/security/post/Tx3D6U6WSFGOK2H/A-New-and-Standardized-Way-to-Manage-Credentials-in-the-AWS-SDKs credentials> file,
    -- and the path to the @~/.aws/config@ file.
    FromFile Text FilePath FilePath
  | -- | Obtain credentials using STS:AssumeRoleWithWebIdentity
    -- See <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_oidc.html About web identity federation>
    -- in the AWS documentation for more information.
    FromWebIdentity
  | -- | Obtain credentials by attempting to contact the ECS container agent
    -- at <http://169.254.170.2> using the path in 'envContainerCredentialsURI'.
    -- See <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks>
    -- in the AWS documentation for more information.
    FromContainer
  | -- | Attempt credentials discovery via the following steps:
    --
    -- * Read the 'envAccessKey', 'envSecretKey', and 'envRegion' from the environment if they are set.
    --
    -- * Read the credentials file if 'credFile' exists.
    --
    -- * Try to exchange a Web Identity for AWS credentials using
    --   @sts:AssumeRoleWithWebIdentity@.
    --
    -- * Obtain credentials from the ECS container agent if
    -- 'envContainerCredentialsURI' is set.
    --
    -- * Retrieve the first available IAM profile and read
    -- the 'Region' from the instance identity document, if running on EC2.
    --
    -- An attempt is made to resolve <http://instance-data> rather than directly
    -- retrieving <http://169.254.169.254> for IAM profile information.
    -- This assists in ensuring the DNS lookup terminates promptly if not
    -- running on EC2.
    Discover
  deriving stock (Eq, Generic)

instance ToLog Credentials where
  build = \case
    FromKeys a _ ->
      "FromKeys " <> build a <> " ****"
    FromSession a _ _ ->
      "FromSession " <> build a <> " **** ****"
    FromEnv ->
      "FromEnv"
    FromProfile n ->
      "FromProfile " <> build n
    FromFile n f g ->
      "FromFile " <> build n <> " " <> build f <> " " <> build g
    FromWebIdentity ->
      "FromWebIdentity"
    FromContainer ->
      "FromContainer"
    Discover ->
      "Discover"

instance Show Credentials where
  show = BS8.unpack . toBS . build

-- discover :: Env' withAuth -> m (Auth, Region)
-- discover = tryFromEnv $ tryFromWebIdentity $ tryCredentialsFile $ const (throwM ...)

runCredentialChain :: MonadCatch m => [a -> m b] -> a -> m b
runCredentialChain chain env =
  case chain of
    [] -> throwM CredentialChainExhausted
    provider : chain' ->
      catching_ _AuthError (provider env) $ runCredentialChain chain' env

-- | Retrieve authentication information via the specified 'Credentials' mechanism.
--
-- Throws 'AuthError' when environment variables or IAM profiles cannot be read,
-- and credentials files are invalid or cannot be found.
getAuth ::
  (MonadIO m, Foldable withAuth) =>
  Env' withAuth ->
  Credentials ->
  m (Auth, Region)
getAuth env@Env {..} =
  liftIO . \case
    FromKeys a s -> pure $ fromKeys a s env
    FromSession a s t -> pure $ fromSession a s t env
    FromEnv -> fromKeysEnv env
    FromProfile n -> fromNamedInstanceProfile n env
    FromFile n cred conf -> fromFilePath n cred conf env
    FromContainer -> fromContainerEnv env
    FromWebIdentity -> fromWebIdentityEnv env
    Discover ->
      -- Don't try and catch InvalidFileError, or InvalidIAMProfile,
      -- let both errors propagate.
      catching_ _MissingEnvError (fromKeysEnv env) $
        -- proceed, missing env keys
        catching _MissingFileError (fromFileEnv env) $ \f ->
          -- proceed, missing credentials file
          catching_ _MissingEnvError (fromWebIdentityEnv env) $
            -- proceed, missing env keys
            catching_ _MissingEnvError (fromContainerEnv env) $ do
              -- proceed, missing env key
              p <- isEC2 _envManager

              unless p $
                -- not an EC2 instance, rethrow the previous error.
                throwingM _MissingFileError f

              -- proceed, check EC2 metadata for IAM information.
              fromDefaultInstanceProfile env

-- | Try to read the region from the 'envRegion' variable.
getRegion :: MonadIO m => m (Maybe Region)
getRegion = runMaybeT $ do
  mr <- MaybeT . liftIO $ Environment.lookupEnv (Text.unpack envRegion)

  either
    (const . MaybeT $ pure Nothing)
    pure
    (fromText (Text.pack mr))

-- $env
-- This really should be defined in @Amazonka.Env@, but we define it
-- here to break a gnarly module import cycle.

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
