-- |
-- Module      : Amazonka.Auth.STS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Retrieve authentication credentials from Secure Token Service
module Amazonka.Auth.STS where

import Amazonka.Auth.Background (fetchAuthInBackground)
import Amazonka.Auth.Exception
import Amazonka.Env (Env, Env' (..))
import Amazonka.HTTP (retryRequest)
import Amazonka.Lens (throwingM, (^.))
import Amazonka.Prelude
import qualified Amazonka.STS as STS
import qualified Amazonka.STS.AssumeRole as STS
import qualified Amazonka.STS.AssumeRoleWithWebIdentity as STS
import Amazonka.Send (sendUnsigned)
import qualified Control.Exception as Exception
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.HTTP.Client as Client
import qualified System.Environment as Environment

-- NOTE: The implementations in this file are ugly because they must
-- use primitive operations from Amazonka.HTTP to avoid circular
-- module dependencies. If you are writing your own functions in this
-- vein, you will have access to 'Amazonka.send', which should make
-- things nicer.

-- | Assume a role using the @sts:AssumeRole@ API.
--
-- This is a simplified interface suitable for most purposes, but if
-- you need the full functionality of the @sts:AssumeRole@ API, you
-- will need to craft your own requests using @amazonka-sts@. If you
-- do this, remember to use 'fetchAuthInBackground' so that your
-- application does not get stuck holding temporary credentials which
-- have expired.
fromAssumedRole ::
  MonadIO m =>
  -- | Role ARN
  Text ->
  -- | Role session name
  Text ->
  Env ->
  m Env
fromAssumedRole roleArn roleSessionName env = do
  let getCredentials = do
        let assumeRole = STS.newAssumeRole roleArn roleSessionName
        eResponse <- runResourceT $ retryRequest env assumeRole
        clientResponse <- either (liftIO . Exception.throwIO) pure eResponse
        let mCredentials =
              Client.responseBody clientResponse
                ^. STS.assumeRoleResponse_credentials
        case mCredentials of
          Nothing ->
            fail "sts:AssumeRole returned no credentials."
          Just c -> pure c
  auth <- liftIO $ fetchAuthInBackground getCredentials
  pure env {envAuth = Identity auth}

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
  MonadIO m =>
  FilePath ->
  Text ->
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
        let mCreds =
              resp ^. STS.assumeRoleWithWebIdentityResponse_credentials
        case mCreds of
          Nothing ->
            fail "sts:AssumeRoleWithWebIdentity returned no credentials."
          Just c -> pure c

  -- As the credentials from STS are temporary, we start a thread that is able
  -- to fetch new ones automatically on expiry.
  auth <- liftIO $ fetchAuthInBackground getCredentials

  pure env {envAuth = Identity auth}

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
  MonadIO m =>
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
          throwingM
            _MissingEnvError
            "Unable to read token file name from AWS_WEB_IDENTITY_TOKEN_FILE"

    lookupRoleArn =
      nonEmptyEnv "AWS_ROLE_ARN" >>= \case
        Just v -> pure $ Text.pack v
        Nothing ->
          throwingM
            _MissingEnvError
            "Unable to read role ARN from AWS_ROLE_ARN"

    lookupSessionName = fmap Text.pack <$> nonEmptyEnv "AWS_ROLE_SESSION_NAME"

    nonEmptyEnv :: String -> IO (Maybe String)
    nonEmptyEnv var =
      Environment.lookupEnv var <&> \case
        Nothing -> Nothing
        Just "" -> Nothing
        Just v -> Just v
