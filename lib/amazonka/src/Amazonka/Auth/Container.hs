-- |
-- Module      : Amazonka.Auth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Fetch credentials from a metadata service when running in an ECS
-- Container.
module Amazonka.Auth.Container where

import Amazonka.Auth.Background (fetchAuthInBackground)
import Amazonka.Auth.Exception
import Amazonka.Data
import Amazonka.Env (Env, Env' (..))
import Amazonka.Prelude
import Amazonka.Types
import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Client
import qualified System.Environment as Environment

-- | Obtain credentials exposed to a task via the ECS container agent, as
-- described in the <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks>
-- section of the AWS ECS documentation. The credentials are obtained by making
-- a request to the given URL.
--
-- The ECS container agent provides an access key, secret key, session token,
-- and expiration time. As these are temporary credentials, this function also
-- starts a refresh thread that will periodically fetch fresh credentials before
-- the current ones expire.
fromContainer ::
  MonadIO m =>
  -- | Absolute URL
  Text ->
  Env' withAuth ->
  m Env
fromContainer url env =
  liftIO $ do
    req <- Client.parseUrlThrow $ Text.unpack url
    keys <- fetchAuthInBackground (renew req)

    pure env {auth = Identity keys}
  where
    renew :: ClientRequest -> IO AuthEnv
    renew req = do
      rs <- Client.httpLbs req $ manager env

      either
        (Exception.throwIO . invalidIdentityErr)
        pure
        (eitherDecode (Client.responseBody rs))

    invalidIdentityErr =
      InvalidIAMError
        . mappend "Error parsing Task Identity Document "
        . Text.pack

-- | Obtain credentials from the ECS container agent, by querying
-- <http://169.254.170.2> at the path contained by the
-- @AWS_CONTAINER_CREDENTIALS_RELATIVE_URI@ environment variable.
--
-- Throws 'MissingEnvError' if the @AWS_CONTAINER_CREDENTIALS_RELATIVE_URI@
-- environment variable is not set or 'InvalidIAMError' if the payload returned
-- by the ECS container agent is not of the expected format.
--
-- __NOTE:__ We do not currently respect the
-- @AWS_CONTAINER_CREDENTIALS_FULL_URI@ or @AWS_CONTAINTER_AUTHORIZATION_TOKEN@
-- environment variable. If you need support for these, please file a PR.
fromContainerEnv ::
  MonadIO m =>
  Env' withAuth ->
  m Env
fromContainerEnv env = liftIO $ do
  uriRel <-
    Environment.lookupEnv "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI"
      >>= maybe
        (Exception.throwIO $ MissingEnvError "Unable to read AWS_CONTAINER_CREDENTIALS_RELATIVE_URI")
        pure
  fromContainer (Text.pack $ "http://169.254.170.2" <> uriRel) env
