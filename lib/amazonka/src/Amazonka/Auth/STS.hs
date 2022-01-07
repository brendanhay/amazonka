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

import {-# SOURCE #-} Amazonka.Auth
import Amazonka.Auth.Background (fetchAuthInBackground)
import Amazonka.Auth.Exception
import {-# SOURCE #-} Amazonka.HTTP (retryRequest)
import Amazonka.Lens (throwingM, (^.))
import Amazonka.Prelude
import qualified Amazonka.STS as STS
import qualified Amazonka.STS.AssumeRoleWithWebIdentity as STS
import Amazonka.Types
import qualified Control.Exception as Exception
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.HTTP.Client as Client
import qualified System.Environment as Environment

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
  (MonadIO m, Foldable withAuth) =>
  FilePath ->
  Text ->
  Maybe Text ->
  Env' withAuth ->
  m (Auth, Region)
fromWebIdentity tokenFile roleArn mSessionName env = do
  -- Mimic the C++ SDK; fall back to a random UUID if the session name is unset.
  sessionName <-
    liftIO $ maybe (UUID.toText <$> UUID.nextRandom) pure mSessionName

  -- We copy the behaviour of the C++ implementation: upon credential
  -- expiration, re-read the token file content but ignore any changes
  -- to environment variables.
  let getCredentials = do
        token <- Text.readFile tokenFile
        let assumeWeb =
              STS.newAssumeRoleWithWebIdentity
                roleArn
                sessionName
                token
        eResponse <- runResourceT $ retryRequest env assumeWeb
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

  pure (auth, _envRegion env)

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
  (MonadIO m, Foldable withAuth) =>
  Env' withAuth ->
  m (Auth, Region)
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
