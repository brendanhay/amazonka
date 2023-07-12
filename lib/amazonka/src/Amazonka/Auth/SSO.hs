-- |
-- Module      : Amazonka.Auth.SSO
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Auth.SSO where

import Amazonka.Auth.Background (fetchAuthInBackground)
import Amazonka.Auth.Exception
import Amazonka.Core.Lens.Internal ((^.))
import qualified Amazonka.Crypto as Crypto
import Amazonka.Data.Sensitive
import Amazonka.Data.Time (Time (..))
import Amazonka.Env (Env, Env' (..))
import Amazonka.Prelude
import Amazonka.SSO.GetRoleCredentials as SSO
import qualified Amazonka.SSO.Types as SSO (RoleCredentials (..))
import Amazonka.Send (sendUnsigned)
import Amazonka.Types
import qualified Control.Exception as Exception
import Control.Exception.Lens (handling_, _IOException)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (FromJSON, decodeFileStrict)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

data CachedAccessToken = CachedAccessToken
  { startUrl :: Text,
    region :: Region,
    accessToken :: Sensitive Text,
    expiresAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

{-# INLINE cachedAccessToken_startUrl #-}
cachedAccessToken_startUrl :: Lens' CachedAccessToken Text
cachedAccessToken_startUrl f c@CachedAccessToken {startUrl} = f startUrl <&> \startUrl' -> c {startUrl = startUrl'}

{-# INLINE cachedAccessToken_region #-}
cachedAccessToken_region :: Lens' CachedAccessToken Region
cachedAccessToken_region f c@CachedAccessToken {region} = f region <&> \region' -> (c :: CachedAccessToken) {region = region'}

{-# INLINE cachedAccessToken_accessToken #-}
cachedAccessToken_accessToken :: Lens' CachedAccessToken (Sensitive Text)
cachedAccessToken_accessToken f c@CachedAccessToken {accessToken} = f accessToken <&> \accessToken' -> (c :: CachedAccessToken) {accessToken = accessToken'}

{-# INLINE cachedAccessToken_expiresAt #-}
cachedAccessToken_expiresAt :: Lens' CachedAccessToken UTCTime
cachedAccessToken_expiresAt f c@CachedAccessToken {expiresAt} = f expiresAt <&> \expiresAt' -> c {expiresAt = expiresAt'}

-- | Assume a role using an SSO Token.
--
-- The user must have previously called @aws sso login@, and pass in the path to
-- the cached token file, along with SSO region, account ID and role name.
-- ('Amazonka.Auth.ConfigFile.fromFilePath' understands the @sso_@ variables
-- used by the official AWS CLI and will call 'fromSSO' for you.) This function
-- uses 'fetchAuthInBackground' to refresh the credentials as long as the token
-- in the @sso/cache@ file is not expired. When it has, the user will need to
-- @aws sso login@ again.
--
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-sso.html>
fromSSO ::
  forall m withAuth.
  MonadIO m =>
  FilePath ->
  Region ->
  -- | Account ID
  Text ->
  -- | Role Name
  Text ->
  Env' withAuth ->
  m Env
fromSSO cachedTokenFile ssoRegion accountId roleName env = do
  keys <- liftIO $ fetchAuthInBackground getCredentials
  pure $ env {auth = Identity keys}
  where
    getCredentials = do
      CachedAccessToken {..} <- readCachedAccessToken cachedTokenFile

      -- The Region you SSO through may differ from the Region you intend to
      -- interact with after. The former is handled here, the latter is taken
      -- care of later, in ConfigFile.
      let ssoEnv :: Env' withAuth
          ssoEnv = env {region = ssoRegion}
          getRoleCredentials =
            SSO.newGetRoleCredentials
              roleName
              accountId
              (fromSensitive accessToken)

      resp <- runResourceT $ sendUnsigned ssoEnv getRoleCredentials
      pure . roleCredentialsToAuthEnv $
        resp ^. SSO.getRoleCredentialsResponse_roleCredentials

-- | Return the cached token file for a given @sso_start_url@
--
-- Matches
-- [botocore](https://github.com/boto/botocore/blob/c02f3561f56085b8a3f98501d25b9857b916c10e/botocore/utils.py#L2596-L2597),
-- so that we find tokens produced by @aws sso login@.
relativeCachedTokenFile :: MonadIO m => Text -> m FilePath
relativeCachedTokenFile startUrl = do
  let sha1 = show . Crypto.hashSHA1 $ Text.encodeUtf8 startUrl
  pure $ "/.aws/sso/cache/" <> sha1 <> ".json"

readCachedAccessToken :: MonadIO m => FilePath -> m CachedAccessToken
readCachedAccessToken p = liftIO $
  handling_ _IOException err $ do
    mCache <- decodeFileStrict p
    maybe err pure mCache
  where
    err =
      Exception.throwIO $
        InvalidFileError $
          mconcat
            [ "Unable to read SSO cache. ",
              Text.pack p,
              " is missing or invalid."
            ]

roleCredentialsToAuthEnv :: SSO.RoleCredentials -> AuthEnv
roleCredentialsToAuthEnv rc =
  AuthEnv
    (SSO.accessKeyId rc)
    (SSO.secretAccessKey rc)
    (SSO.sessionToken rc)
    (Time . posixSecondsToUTCTime . fromInteger <$> SSO.expiration rc)
