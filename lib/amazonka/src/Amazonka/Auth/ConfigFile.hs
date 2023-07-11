-- |
-- Module      : Amazonka.Auth.ConfigFile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Retrieve authentication credentials from AWS config/credentials files.
module Amazonka.Auth.ConfigFile where

import Amazonka.Auth.Container (fromContainerEnv)
import Amazonka.Auth.Exception
import Amazonka.Auth.InstanceProfile (fromDefaultInstanceProfile)
import Amazonka.Auth.Keys (fromKeysEnv)
import Amazonka.Auth.SSO (fromSSO, relativeCachedTokenFile)
import Amazonka.Auth.STS (fromAssumedRole, fromWebIdentity)
import Amazonka.Data
import Amazonka.Env (Env, Env' (..), lookupRegion)
import Amazonka.Prelude
import Amazonka.Types
import qualified Control.Exception as Exception
import Control.Exception.Lens (handling_, _IOException)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.State (StateT, evalStateT, get, modify)
import Data.Foldable (asum)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Ini as INI
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import System.Info (os)

-- | Retrieve credentials from the AWS config/credentials files, as
-- Amazonka currently understands them:
--
-- * AWS recommends credentials do not live in the config file, but
--   allows it.
--
-- * Sections in the config file start should either be named
--   @[default]@ or @[profile foo]@. Unprefixed @[foo]@ currently
--   "happens to work" but is not officially supported, to match the
--   observed behaviour of the AWS SDK/CLI.
--
-- * Sections in the credentials file are always unprefixed -
--   @[default]@ or @[foo]@.
--
-- /See:/ the 'ConfigProfile' type, to understand the methods Amazonka
-- currently supports.
fromFilePath ::
  forall m withAuth.
  (MonadIO m, Foldable withAuth) =>
  -- | Profile name
  Text ->
  -- | Credentials file
  FilePath ->
  -- | Config file
  FilePath ->
  Env' withAuth ->
  m Env
fromFilePath profile credentialsFile configFile env = liftIO $ do
  credentialsIni <- loadIniFile credentialsFile
  -- If we fail to read the config file, assume it's empty and move
  -- on. It is valid to configure only a credentials file if you only
  -- want to set keys, for example.
  configIni <-
    Exception.catchJust
      (\(_ :: AuthError) -> Just mempty)
      (loadIniFile configFile)
      pure

  let config = mergeConfigs credentialsIni configIni
  env' <-
    evalConfig profile
      & (`runReaderT` config)
      & (`evalStateT` mempty)

  -- A number of settings in the AWS config files should be
  -- overridable by environment variables, but aren't. We make a point
  -- of at least respecting the AWS_REGION variable, but leave the
  -- rest to future work.
  --
  -- See: https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-files.html
  lookupRegion <&> \case
    Nothing -> env'
    Just region -> env' {region}
  where
    loadIniFile :: FilePath -> IO (HashMap Text [(Text, Text)])
    loadIniFile path = do
      exists <- Directory.doesFileExist path
      unless exists . Exception.throwIO $ MissingFileError path
      INI.readIniFile path >>= \case
        Left e ->
          Exception.throwIO . InvalidFileError . Text.pack $ path <> ": " <> e
        Right ini -> pure $ INI.iniSections ini

    -- Parse the matched config, and extract auth credentials from it,
    -- recursively if necessary.
    evalConfig ::
      Text ->
      ReaderT
        (HashMap Text (HashMap Text Text)) -- Map of profiles and their settings
        (StateT [Text] IO) -- List of source_profiles we've seen already
        Env
    evalConfig pName = do
      config <- ask
      case HashMap.lookup pName config of
        Nothing ->
          liftIO . Exception.throwIO . InvalidFileError $
            "Missing profile: " <> Text.pack (show pName)
        Just p -> case parseConfigProfile p of
          Nothing ->
            liftIO . Exception.throwIO . InvalidFileError $
              "Parse error in profile: " <> Text.pack (show pName)
          Just (cp, mRegion) -> do
            env' <- case cp of
              ExplicitKeys keys ->
                pure env {auth = Identity $ Auth keys}
              AssumeRoleFromProfile roleArn sourceProfileName -> do
                seenProfiles <- lift get
                if sourceProfileName `elem` seenProfiles
                  then
                    let trace = reverse seenProfiles ++ [last seenProfiles]
                        textTrace = Text.intercalate " -> " trace
                     in liftIO
                          . Exception.throwIO
                          . InvalidFileError
                          $ "Infinite source_profile loop: " <> textTrace
                  else do
                    lift . modify $ (sourceProfileName :)
                    sourceEnv <- evalConfig sourceProfileName
                    fromAssumedRole roleArn "amazonka-assumed-role" sourceEnv
              AssumeRoleFromCredentialSource roleArn source -> do
                sourceEnv <- case source of
                  Environment -> fromKeysEnv env
                  Ec2InstanceMetadata -> fromDefaultInstanceProfile env
                  EcsContainer -> fromContainerEnv env
                fromAssumedRole roleArn "amazonka-assumed-role" sourceEnv
              AssumeRoleWithWebIdentity roleArn mRoleSessionName tokenFile ->
                fromWebIdentity tokenFile roleArn mRoleSessionName env
              AssumeRoleViaSSO startUrl ssoRegion accountId roleName -> do
                cachedTokenFile <-
                  liftIO $
                    configPathRelative =<< relativeCachedTokenFile startUrl
                fromSSO cachedTokenFile ssoRegion accountId roleName env

            -- Once we have the env from the profile, apply the region
            -- if we parsed one out.
            pure . maybe env' (\region -> env' {region}) $ mRegion

mergeConfigs ::
  -- | Credentials
  HashMap Text [(Text, Text)] ->
  -- | Config
  HashMap Text [(Text, Text)] ->
  HashMap Text (HashMap Text Text)
mergeConfigs creds confs =
  HashMap.unionWith
    (HashMap.union)
    (HashMap.fromList <$> creds)
    (HashMap.fromList <$> stripProfiles confs)
  where
    stripProfiles :: HashMap Text v -> HashMap Text v
    stripProfiles = HashMap.mapKeys $ Text.unwords . stripProfile . Text.words

    stripProfile = \case
      [w] -> [w]
      ("profile" : ws) -> ws
      ws -> ws

parseConfigProfile :: HashMap Text Text -> Maybe (ConfigProfile, Maybe Region)
parseConfigProfile profile = parseProfile <&> \p -> (p, parseRegion)
  where
    parseProfile :: Maybe ConfigProfile
    parseProfile =
      asum
        [ explicitKey,
          assumeRoleFromProfile,
          assumeRoleFromCredentialSource,
          assumeRoleWithWebIdentity,
          assumeRoleViaSSO
        ]

    parseRegion :: Maybe Region
    parseRegion = Region' <$> HashMap.lookup "region" profile

    explicitKey =
      fmap ExplicitKeys $
        AuthEnv
          <$> ( AccessKey . Text.encodeUtf8
                  <$> HashMap.lookup "aws_access_key_id" profile
              )
          <*> ( Sensitive . SecretKey . Text.encodeUtf8
                  <$> HashMap.lookup "aws_secret_access_key" profile
              )
          <*> ( Just $
                  Sensitive . SessionToken . Text.encodeUtf8
                    <$> HashMap.lookup "aws_session_token" profile
              )
          <*> Just Nothing -- No token expiry in config file
    assumeRoleFromProfile =
      AssumeRoleFromProfile
        <$> HashMap.lookup "role_arn" profile
        <*> HashMap.lookup "source_profile" profile

    assumeRoleFromCredentialSource =
      AssumeRoleFromCredentialSource
        <$> HashMap.lookup "role_arn" profile
        <*> ( HashMap.lookup "credential_source" profile >>= \case
                "Environment" -> Just Environment
                "Ec2InstanceMetadata" -> Just Ec2InstanceMetadata
                "EcsContainer" -> Just EcsContainer
                _ -> Nothing
            )

    assumeRoleWithWebIdentity =
      AssumeRoleWithWebIdentity
        <$> HashMap.lookup "role_arn" profile
        <*> Just (HashMap.lookup "role_session_name" profile)
        <*> (Text.unpack <$> HashMap.lookup "web_identity_token_file" profile)

    assumeRoleViaSSO =
      AssumeRoleViaSSO
        <$> HashMap.lookup "sso_start_url" profile
        <*> (Region' <$> HashMap.lookup "sso_region" profile)
        <*> HashMap.lookup "sso_account_id" profile
        <*> HashMap.lookup "sso_role_name" profile

data ConfigProfile
  = -- | Recognizes @aws_access_key_id@, @aws_secret_access_key@, and
    -- optionally @aws_session_token@.
    ExplicitKeys AuthEnv
  | -- | Recognizes @role_arn@ and @source_profile@.
    AssumeRoleFromProfile Text Text
  | -- | Recognizes @role_arn@ and @credential_source@.
    AssumeRoleFromCredentialSource Text CredentialSource
  | -- | Recognizes @role_arn@, @role_session_name@, and
    -- @web_identity_token_file@.
    AssumeRoleWithWebIdentity Text (Maybe Text) FilePath
  | -- | Recognizes @sso_start_url@, @sso_region@, @sso_account_id@, and
    -- @sso_role_name@.
    AssumeRoleViaSSO Text Region Text Text
  deriving stock (Eq, Show, Generic)

data CredentialSource = Environment | Ec2InstanceMetadata | EcsContainer
  deriving stock (Eq, Show, Generic)

-- | Loads the default config/credentials INI files and selects a
-- profile by environment variable (@AWS_PROFILE@).
--
-- Throws 'MissingFileError' if 'credFile' is missing, or 'InvalidFileError'
-- if an error occurs during parsing.
--
-- This looks in in the @HOME@ directory as determined by the
-- <http://hackage.haskell.org/package/directory directory> library.
--
-- * Not Windows: @$HOME\/.aws\/credentials@
--
-- * Windows: @%USERPROFILE%\\.aws\\credentials@
fromFileEnv ::
  (MonadIO m, Foldable withAuth) => Env' withAuth -> m Env
fromFileEnv env = liftIO $ do
  mProfile <- Environment.lookupEnv "AWS_PROFILE"
  cred <- configPathRelative "/.aws/credentials"
  conf <- configPathRelative "/.aws/config"

  fromFilePath (maybe "default" Text.pack mProfile) cred conf env

configPathRelative :: String -> IO String
configPathRelative p = handling_ _IOException err dir
  where
    err = Exception.throwIO $ MissingFileError ("$HOME" ++ p)
    dir = case os of
      "mingw32" ->
        Environment.lookupEnv "USERPROFILE"
          >>= maybe (Exception.throwIO $ MissingFileError "%USERPROFILE%") pure
      _ -> Directory.getHomeDirectory <&> (++ p)
