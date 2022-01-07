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

    -- ** Defaults

    -- *** Environment
    envAccessKey,
    envSecretKey,
    envSessionToken,
    envWebIdentityTokenFile,
    envRole,
    envRoleSessionName,

    -- ** Configuration
    confRegion,
    confFile,

    -- *** Credentials File
    credAccessKey,
    credSecretKey,
    credSessionToken,
    credProfile,
    credFile,

    -- ** Credentials
    -- $credentials
    fromKeys,
    fromSession,
    fromTemporarySession,
    fromKeysEnv,
    fromFile,
    fromFilePath,
    fromDefaultInstanceProfile,
    fromNamedInstanceProfile,
    fromContainer,

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

import Amazonka.Auth.Background (fetchAuthInBackground)
import Amazonka.Auth.Exception
import Amazonka.Auth.InstanceProfile (fromDefaultInstanceProfile, fromNamedInstanceProfile)
import Amazonka.Auth.Keys (fromKeys, fromKeysEnv, fromSession, fromTemporarySession)
import Amazonka.Auth.STS (fromWebIdentityEnv)
import Amazonka.Data
import Amazonka.EC2.Metadata
import {-# SOURCE #-} Amazonka.HTTP (retryRequest)
import Amazonka.Lens (catching, catching_, throwingM, _IOException)
import Amazonka.Prelude
import qualified Amazonka.STS as STS
import qualified Amazonka.STS.AssumeRoleWithWebIdentity as STS
import Amazonka.Types
import qualified Control.Exception as Exception
import Control.Lens ((^.))
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Char as Char
import qualified Data.Ini as INI
import Data.Monoid (Dual, Endo)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.HTTP.Client as Client
import qualified System.Directory as Directory
import qualified System.Environment as Environment

-- | Default access key environment variable.
envAccessKey ::
  -- | AWS_ACCESS_KEY_ID
  Text
envAccessKey = "AWS_ACCESS_KEY_ID"

-- | Default secret key environment variable.
envSecretKey ::
  -- | AWS_SECRET_ACCESS_KEY
  Text
envSecretKey = "AWS_SECRET_ACCESS_KEY"

-- | Default session token environment variable.
envSessionToken ::
  -- | AWS_SESSION_TOKEN
  Text
envSessionToken = "AWS_SESSION_TOKEN"

-- | Default credentials profile environment variable.
envProfile ::
  -- | AWS_PROFILE
  Text
envProfile = "AWS_PROFILE"

-- | Default region environment variable
envRegion ::
  -- | AWS_REGION
  Text
envRegion = "AWS_REGION"

-- | Default web identity token file environment variable
envWebIdentityTokenFile ::
  -- | AWS_WEB_IDENTITY_TOKEN_FILE
  Text
envWebIdentityTokenFile = "AWS_WEB_IDENTITY_TOKEN_FILE"

-- | Default role environment variable
envRole ::
  -- | AWS_ROLE_ARN
  Text
envRole = "AWS_ROLE_ARN"

-- | Default role session name environment variable
envRoleSessionName ::
  -- | AWS_ROLE_SESSION_NAME
  Text
envRoleSessionName = "AWS_ROLE_SESSION_NAME"

-- | Path to obtain container credentials environment variable (see
-- 'FromContainer').
envContainerCredentialsURI ::
  -- | AWS_CONTAINER_CREDENTIALS_RELATIVE_URI
  Text
envContainerCredentialsURI = "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI"

-- | Credentials INI file access key variable.
credAccessKey ::
  -- | aws_access_key_id
  Text
credAccessKey = "aws_access_key_id"

-- | Credentials INI file secret key variable.
credSecretKey ::
  -- | aws_secret_access_key
  Text
credSecretKey = "aws_secret_access_key"

-- | Credentials INI file session token variable.
credSessionToken ::
  -- | aws_session_token
  Text
credSessionToken = "aws_session_token"

-- | Credentials INI default profile section variable.
credProfile ::
  -- | default
  Text
credProfile = "default"

-- | Default path for the credentials file. This looks in in the @HOME@ directory
-- as determined by the <http://hackage.haskell.org/package/directory directory>
-- library.
--
-- * UNIX/OSX: @$HOME/.aws/credentials@
--
-- * Windows: @C:\/Users\//\<user\>\.aws\credentials@
--
-- /Note:/ This does not match the default AWS SDK location of
-- @%USERPROFILE%\.aws\credentials@ on Windows. (Sorry.)
credFile :: MonadIO m => m FilePath
credFile = liftIO (catching_ _IOException dir err)
  where
    dir = (++ path) <$> Directory.getHomeDirectory
    err = Exception.throwIO $ MissingFileError ("$HOME" ++ path)

    path = "/.aws/credentials"

-- | Credentials INI default profile section variable.
confRegion ::
  -- | default
  Text
confRegion = "region"

-- | Default path for the configuration file. This looks in in the @HOME@ directory
-- as determined by the <http://hackage.haskell.org/package/directory directory>
-- library.
--
-- * UNIX/OSX: @$HOME/.aws/config@
--
-- * Windows: @C:\/Users\//\<user\>\.aws\config@
--
-- /Note:/ This does not match the default AWS SDK location of
-- @%USERPROFILE%\.aws\config@ on Windows. (Sorry.)
confFile :: MonadIO m => m FilePath
confFile = liftIO (catching_ _IOException dir err)
  where
    dir = (++ path) <$> liftIO Directory.getHomeDirectory
    err = Exception.throwIO $ MissingFileError ("$HOME" ++ path)

    path = "/.aws/config"

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

runCredentialChain :: MonadThrow m => [(a -> m (Maybe b))] -> a -> m b
runCredentialChain chain env = case chain of
  [] -> throwM CredentialChainExhausted
  provider : chain' ->
    provider env
      >>= maybe (runCredentialChain chain' env) pure

-- | Retrieve authentication information via the specified 'Credentials' mechanism.
--
-- Throws 'AuthError' when environment variables or IAM profiles cannot be read,
-- and credentials files are invalid or cannot be found.
getAuth ::
  (MonadIO m, Foldable withAuth) =>
  Env' withAuth ->
  Credentials ->
  m (Auth, Maybe Region)
getAuth env@Env {..} =
  liftIO . \case
    FromKeys a s -> pure (Just <$> fromKeys a s env)
    FromSession a s t -> pure (Just <$> fromSession a s t env)
    FromEnv -> fmap Just <$> fromKeysEnv env
    FromProfile n -> fmap Just <$> fromNamedInstanceProfile n env
    FromFile n cred conf -> fromFilePath n cred conf
    FromContainer -> fromContainer _envManager
    FromWebIdentity -> fromWebIdentity env
    Discover ->
      -- Don't try and catch InvalidFileError, or InvalidIAMProfile,
      -- let both errors propagate.
      catching_ _MissingEnvError (fmap Just <$> fromKeysEnv env) $
        -- proceed, missing env keys
        catching _MissingFileError fromFile $ \f ->
          -- proceed, missing credentials file
          catching_ _MissingEnvError (fromWebIdentity env) $
            -- proceed, missing env keys
            catching_ _MissingEnvError (fromContainer _envManager) $ do
              -- proceed, missing env key
              p <- isEC2 _envManager

              unless p $
                -- not an EC2 instance, rethrow the previous error.
                throwingM _MissingFileError f

              -- proceed, check EC2 metadata for IAM information.
              fmap Just <$> fromDefaultInstanceProfile env

-- | Loads the default @credentials@ INI file using the default profile name.
--
-- Throws 'MissingFileError' if 'credFile' is missing, or 'InvalidFileError'
-- if an error occurs during parsing.
--
-- /See:/ 'credProfile', 'credFile', and 'envProfile'
fromFile :: MonadIO m => m (Auth, Maybe Region)
fromFile = do
  mprofile <- liftIO (Environment.lookupEnv (Text.unpack envProfile))
  cred <- credFile
  conf <- confFile

  fromFilePath (maybe credProfile Text.pack mprofile) cred conf

fromFilePath ::
  MonadIO m =>
  Text ->
  FilePath ->
  FilePath ->
  m (Auth, Maybe Region)
fromFilePath profile cred conf =
  liftIO ((,) <$> lookupCredentials cred <*> lookupRegion conf)
  where
    lookupCredentials path = do
      exists <- Directory.doesFileExist path

      unless exists $
        Exception.throwIO (MissingFileError path)

      ini <- INI.readIniFile path >>= either (throwInvalid path Nothing) pure

      env <-
        AuthEnv
          <$> (required path credAccessKey ini <&> AccessKey)
          <*> (required path credSecretKey ini <&> Sensitive . SecretKey)
          <*> (optional credSessionToken ini <&> fmap (Sensitive . SessionToken))
          <*> pure Nothing

      pure (Auth env)

    lookupRegion path = do
      exists <- Directory.doesFileExist path

      if not exists
        then pure Nothing
        else do
          ini <- INI.readIniFile path >>= either (throwInvalid path Nothing) pure

          let configProfile =
                if profile == "default"
                  then profile
                  else "profile " <> profile

          case INI.lookupValue configProfile confRegion ini of
            Left _ -> pure Nothing
            Right regionValue ->
              case fromText regionValue of
                Left err -> liftIO (throwInvalid path (Just confRegion) err)
                Right ok -> pure (Just ok)

    required path key ini =
      case INI.lookupValue profile key ini of
        Left err -> throwInvalid path (Just key) err
        Right x
          | blank x -> throwInvalid path (Just key) "cannot be a blank string."
          | otherwise -> pure (Text.encodeUtf8 x)
      where
        blank x = Text.null x || Text.all Char.isSpace x

    optional key ini =
      pure $
        case INI.lookupValue profile key ini of
          Left _ -> Nothing
          Right x -> Just (Text.encodeUtf8 x)

    throwInvalid :: FilePath -> Maybe Text -> String -> IO a
    throwInvalid path mkey err =
      Exception.throwIO . InvalidFileError $
        Text.pack path
          <> maybe mempty (", key " <>) mkey
          <> ", "
          <> Text.pack err

-- | https://aws.amazon.com/blogs/opensource/introducing-fine-grained-iam-roles-service-accounts/
-- Obtain temporary credentials from STS:AssumeRoleWithWebIdentity
-- Token file is taken from env variable 'envWebIdentityTokenFile' and role from 'envRole'.
--
-- The STS service provides an access key, secret key, session token,
-- and expiration time, but it does not include a region, so the region will
-- attempt to be determined from the 'envRegion' environment variable if it is
-- set.
--
-- Like 'fromProfileName', additionally starts a refresh thread that will
-- periodically fetch fresh credentials before the current ones expire.
--
-- Throws 'MissingEnvError' if the 'envWebIdentityTokenFile' environment
-- variable is not set.
fromWebIdentity ::
  (MonadIO m, Foldable withAuth) =>
  Env' withAuth ->
  m (Auth, Maybe Region)
fromWebIdentity env = do
  -- Implementation modelled on the C++ SDK:
  -- https://github.com/aws/aws-sdk-cpp/blob/6d6dcdbfa377393306bf79585f61baea524ac124/aws-cpp-sdk-core/source/auth/STSCredentialsProvider.cpp#L33
  tokenFile <- liftIO $ reqEnv envWebIdentityTokenFile
  roleArn <- liftIO $ fromString <$> reqEnv envRole

  sessionName <-
    liftIO $
      optEnv envRoleSessionName >>= \case
        -- If no session name is set, we mimic the C++ SDK and use a
        -- random UUID.
        Nothing -> UUID.toText <$> UUID.nextRandom
        Just v -> pure (fromString v)

  reg <- getRegion
  let env' = case reg of
        Nothing -> env
        Just r -> env {_envRegion = r}

  -- We copy the behaviour of the C++ implementation: upon credential
  -- expiration, re-read the token file content but ignore any changes
  -- to the actual environment variables.
  let getCredentials = do
        token <- Text.readFile tokenFile
        let assumeWeb =
              STS.newAssumeRoleWithWebIdentity
                roleArn
                sessionName
                token
        eResponse <- runResourceT $ retryRequest env' assumeWeb
        clientResponse <- either (liftIO . Exception.throwIO) pure eResponse
        let mCredentials =
              Client.responseBody clientResponse
                ^. STS.assumeRoleWithWebIdentityResponse_credentials
        case mCredentials of
          Nothing ->
            fail "sts:AssumeRoleWithWebIdentity returned no credentials."
          Just c -> pure c

  -- As the credentials from STS are temporary, we start a thread that is able
  -- to fetch new ones automatically on expiry.
  auth <- liftIO $ fetchAuthInBackground getCredentials

  pure (auth, reg)

-- | Obtain credentials exposed to a task via the ECS container agent, as
-- described in the <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks>
-- section of the AWS ECS documentation. The credentials are obtained by making
-- a request to <http://169.254.170.2> at the path contained by the
-- 'envContainerCredentialsURI' environment variable.
--
-- The ECS container agent provides an access key, secret key, session token,
-- and expiration time, but it does not include a region, so the region will
-- attempt to be determined from the 'envRegion' environment variable if it is
-- set.
--
-- Like 'fromNamedInstanceProfile', additionally starts a refresh thread that
-- will periodically fetch fresh credentials before the current ones expire.
--
-- Throws 'MissingEnvError' if the 'envContainerCredentialsURI' environment
-- variable is not set or 'InvalidIAMError' if the payload returned by the ECS
-- container agent is not of the expected format.
fromContainer ::
  MonadIO m =>
  Client.Manager ->
  m (Auth, Maybe Region)
fromContainer m =
  liftIO $ do
    req <- getCredentialsURI
    auth <- fetchAuthInBackground (renew req)
    reg <- getRegion

    pure (auth, reg)
  where
    getCredentialsURI :: IO ClientRequest
    getCredentialsURI = do
      mp <- Environment.lookupEnv (Text.unpack envContainerCredentialsURI)
      p <-
        maybe
          (Exception.throwIO . MissingEnvError $ "Unable to read ENV variable: " <> envContainerCredentialsURI)
          pure
          mp

      Client.parseUrlThrow ("http://169.254.170.2" <> p)

    renew :: ClientRequest -> IO AuthEnv
    renew req = do
      rs <- Client.httpLbs req m

      either
        (Exception.throwIO . invalidIdentityErr)
        pure
        (eitherDecode (Client.responseBody rs))

    invalidIdentityErr =
      InvalidIAMError
        . mappend "Error parsing Task Identity Document "
        . Text.pack

reqEnv :: MonadIO m => Text -> m String
reqEnv k =
  optEnv k >>= \case
    Nothing ->
      liftIO . Exception.throwIO . MissingEnvError $
        "Unable to read ENV variable: " <> k
    Just v -> pure v

optEnv :: MonadIO m => Text.Text -> m (Maybe String)
optEnv k = liftIO . Environment.lookupEnv $ Text.unpack k

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
