-- |
-- Module      : Amazonka.Auth.InstanceProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Retrieve authentication from EC2 instance profiles.
module Amazonka.Auth.InstanceProfile where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Control.Exception as Exception
import Amazonka.Data
import Amazonka.Auth.Exception
import Amazonka.Auth.Background
import Amazonka.Prelude
import Amazonka.EC2.Metadata
import Amazonka.Types
import qualified Network.HTTP.Client as Client

-- | Retrieve the default IAM Profile from the local EC2 instance-data.
--
-- The default IAM profile is determined by Amazon as the first profile found
-- in the response from:
-- @http://169.254.169.254/latest/meta-data/iam/security-credentials/@
--
-- Throws 'RetrievalError' if the HTTP call fails, or 'InvalidIAMError' if
-- the default IAM profile cannot be read.
fromProfile ::
  MonadIO m =>
  Client.Manager ->
  m (Auth, Maybe Region)
fromProfile m =
  liftIO $ do
    ls <- Exception.try $ metadata m (IAM (SecurityCredentials Nothing))

    case BS8.lines <$> ls of
      Right (x : _) -> fromProfileName m (Text.decodeUtf8 x)
      Left e -> Exception.throwIO (RetrievalError e)
      _ ->
        Exception.throwIO $
          InvalidIAMError "Unable to get default IAM Profile from EC2 metadata"

-- | Lookup a specific IAM Profile by name from the local EC2 instance-data.
--
-- Additionally starts a refresh thread for the given authentication environment.
--
-- The resulting 'IORef' wrapper + timer is designed so that multiple concurrent
-- accesses of 'AuthEnv' from the 'AWS' environment are not required to calculate
-- expiry and sequentially queue to update it.
--
-- The forked timer ensures a singular owner and pre-emptive refresh of the
-- temporary session credentials before expiration.
--
-- A weak reference is used to ensure that the forked thread will eventually
-- terminate when 'Auth' is no longer referenced.
--
-- If no session token or expiration time is present the credentials will
-- be returned verbatim.
fromProfileName ::
  MonadIO m =>
  Client.Manager ->
  Text ->
  m (Auth, Maybe Region)
fromProfileName m name =
  liftIO $ do
    auth <- fetchAuthInBackground getCredentials
    reg <- getRegionFromIdentity

    pure (auth, Just reg)
  where
    getCredentials =
      Exception.try (metadata m (IAM . SecurityCredentials $ Just name))
        >>= handleErr (eitherDecode' . LBS8.fromStrict) invalidIAMErr

    getRegionFromIdentity =
      Exception.try (identity m)
        >>= handleErr (fmap _region) invalidIdentityErr

    handleErr f g = \case
      Left e -> Exception.throwIO (RetrievalError e)
      Right x -> either (Exception.throwIO . g) pure (f x)

    invalidIAMErr =
      InvalidIAMError
        . mappend ("Error parsing IAM profile '" <> name <> "' ")
        . Text.pack

    invalidIdentityErr =
      InvalidIAMError
        . mappend "Error parsing Instance Identity Document "
        . Text.pack
