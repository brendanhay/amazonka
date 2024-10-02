-- |
-- Module      : Amazonka.Auth.STS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Retrieve authentication credentials from Secure Token Service
module Amazonka.Auth.STS where

import Amazonka.Auth.Background (fetchAuthInBackground)
import Amazonka.Auth.Exception
import Amazonka.Core.Lens.Internal ((^.))
import Amazonka.Env (Env, Env' (..))
import Amazonka.Prelude
import qualified Amazonka.STS as STS
import qualified Amazonka.STS.AssumeRole as STS
import qualified Amazonka.STS.AssumeRoleWithWebIdentity as STS
import Amazonka.Send (send, sendUnsigned)
import Control.Exception (throw)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified System.Environment as Environment

-- | Assume a role using the @sts:AssumeRole@ API.
--
-- This is a simplified interface suitable for most purposes, but if
-- you need the full functionality of the @sts:AssumeRole@ API, you
-- will need to craft your own requests using @amazonka-sts@. If you
-- do this, remember to use 'fetchAuthInBackground' so that your
-- application does not get stuck holding temporary credentials which
-- have expired.
fromAssumedRole ::
  (MonadIO m) =>
  -- | Role ARN
  Text ->
  -- | Role session name
  Text ->
  Env ->
  m Env
fromAssumedRole roleArn roleSessionName env = do
  let getCredentials = do
        let assumeRole = STS.newAssumeRole roleArn roleSessionName
        resp <- runResourceT $ send env assumeRole
        pure $ resp ^. STS.assumeRoleResponse_credentials
  keys <- liftIO $ fetchAuthInBackground getCredentials
  pure env {auth = Identity keys}

-- | https://aws.amazon.com/blogs/opensource/introducing-fine-grained-iam-roles-service-accounts/
-- Obtain temporary credentials from @sts:AssumeRoleWithWebIdentity@.
--
-- The STS service provides an access key, secret key, session token,
-- and expiration time. Also spawns a refresh thread that will
-- periodically fetch fresh credentials before the current ones
-- expire.
--
-- The implementation is modelled on the C++ SDK:
-- https://github.com/aws/aws-sdk-cpp/blob/6d6dcdbfa377393306bf79585f61baea524ac124/aws-cpp-sdk-core/source/auth/STSCredentialsProvider.cpp#L33
fromWebIdentity ::
  (MonadIO m) =>
  -- | Path to token file
  FilePath ->
  -- | Role ARN
  Text ->
  -- | Role Session Name
  Maybe Text ->
  Env' withAuth ->
  m Env
fromWebIdentity tokenFile roleArn mSessionName env = do
  -- Mimic the C++ SDK; fall back to a random UUID if the session name is unset.
  sessionName <-
    liftIO $ maybe (UUID.toText <$> UUID.nextRandom) pure mSessionName

  -- We copy the behaviour of the C++ implementation: upon credential
  -- expiration, re-read the token file content but ignore any changes
  -- to environment variables.
  let getCredentials = do
        token <- Text.readFile tokenFile

        let assumeRoleWithWebIdentity =
              STS.newAssumeRoleWithWebIdentity
                roleArn
                sessionName
                token

        resp <- runResourceT $ sendUnsigned env assumeRoleWithWebIdentity
        pure $ resp ^. STS.assumeRoleWithWebIdentityResponse_credentials

  -- As the credentials from STS are temporary, we start a thread that is able
  -- to fetch new ones automatically on expiry.
  keys <- liftIO $ fetchAuthInBackground getCredentials

  pure env {auth = Identity keys}

-- | Obtain temporary credentials from
-- @sts:AssumeRoleWithWebIdentity@, sourcing arguments from standard
-- environment variables:
--
-- * @AWS_WEB_IDENTITY_TOKEN_FILE@
-- * @AWS_ROLE_ARN@
-- * @AWS_ROLE_SESSION_NAME@ (optional)
--
-- Throws 'MissingEnvError' if a required environment variable is
-- empty or unset.
fromWebIdentityEnv ::
  (MonadIO m) =>
  Env' withAuth ->
  m Env
fromWebIdentityEnv env = liftIO $ do
  tokenFile <- lookupTokenFile
  roleArn <- lookupRoleArn
  mSessionName <- lookupSessionName
  fromWebIdentity tokenFile roleArn mSessionName env
  where
    lookupTokenFile =
      nonEmptyEnv "AWS_WEB_IDENTITY_TOKEN_FILE" >>= \case
        Just v -> pure v
        Nothing ->
          throw
            $ MissingEnvError
              "Unable to read token file name from AWS_WEB_IDENTITY_TOKEN_FILE"

    lookupRoleArn =
      nonEmptyEnv "AWS_ROLE_ARN" >>= \case
        Just v -> pure $ Text.pack v
        Nothing ->
          throw
            $ MissingEnvError
              "Unable to read role ARN from AWS_ROLE_ARN"

    lookupSessionName = fmap Text.pack <$> nonEmptyEnv "AWS_ROLE_SESSION_NAME"

    nonEmptyEnv :: String -> IO (Maybe String)
    nonEmptyEnv var =
      Environment.lookupEnv var <&> \case
        Nothing -> Nothing
        Just "" -> Nothing
        Just v -> Just v
