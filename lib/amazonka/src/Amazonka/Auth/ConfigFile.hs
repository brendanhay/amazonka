-- |
-- Module      : Amazonka.Auth.ConfigFile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Retrieve authentication credentials from AWS config/credentials files.
module Amazonka.Auth.ConfigFile where

import {-# SOURCE #-} Amazonka.Auth
import Amazonka.Auth.Exception
import Amazonka.Auth.STS (fromAssumedRole, fromWebIdentity)
import Amazonka.Data
import Amazonka.Prelude
import Amazonka.Types
import qualified Control.Exception as Exception
import Control.Exception.Lens (catching_, _IOException)
import Data.Foldable (asum)
import Data.HashMap.Strict as HashMap
import qualified Data.Ini as INI
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified System.Directory as Directory
import qualified System.Environment as Environment

-- | Retrieve credentials from the AWS config/credentials files, as
-- Amazonka currently understands them:
--
-- * AWS recommends credentials do not live in the config file, but
--   allows it.
--
-- * Sections in the config file start should either be named
--   @[default]@ or @[profile foo]@, but unprefixed @[foo]@ is also
--   supported.
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
fromFilePath profile credentialsFile configFile env = do
  config <-
    liftIO $
      mergeConfigs <$> loadIniFile credentialsFile <*> loadIniFile configFile
  evalConfig config profile
  where
    -- Parse the matched config, and extract auth credentials from it,
    -- recursively if necessary.
    evalConfig ::
      HashMap Text (HashMap Text Text) ->
      Text ->
      m Env
    evalConfig config pName =
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
                      ExplicitKeys authEnv ->
                        pure env {_envAuth = Identity $ Auth authEnv}
                      AssumeRoleFromProfile roleArn sourceProfileName -> do
                        sourceEnv <- evalConfig config sourceProfileName
                        fromAssumedRole roleArn "amazonka-assumed-role" sourceEnv
                      AssumeRoleFromCredentialSource {} -> undefined -- TODO
                      AssumeRoleWithWebIdentity roleArn mRoleSessionName tokenFile ->
                        fromWebIdentity tokenFile roleArn mRoleSessionName env

            -- Once we have the env from the profile, apply the region
            -- if we parsed one out.
            pure $ maybe env' (\r -> env' { _envRegion = r}) mRegion

loadIniFile :: FilePath -> IO (HashMap Text [(Text, Text)])
loadIniFile path = do
  exists <- Directory.doesFileExist path
  unless exists . Exception.throwIO $ MissingFileError path
  INI.readIniFile path >>= \case
    Left e ->
      Exception.throwIO . InvalidFileError . Text.pack $ path <> ": " <> e
    Right ini -> pure $ INI.iniSections ini

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
          assumeRoleWithWebIdentity
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
-- * UNIX/OSX: @$HOME/.aws/credentials@
--
-- * Windows: @C:\/Users\//\<user\>\.aws\credentials@
--
-- /Note:/ This does not match the default AWS SDK location of
-- @%USERPROFILE%\.aws\credentials@ on Windows. (Sorry.)
fromFileEnv ::
  (MonadIO m, Foldable withAuth) => Env' withAuth -> m Env
fromFileEnv env = liftIO $ do
  mProfile <- Environment.lookupEnv "AWS_PROFILE"
  cred <- file "/.aws/credentials"
  conf <- file "/.aws/config"

  fromFilePath (maybe "default" Text.pack mProfile) cred conf env
  where
    file p = catching_ _IOException dir err
      where
        dir = Directory.getHomeDirectory <&> (++ p)
        err = Exception.throwIO $ MissingFileError ("$HOME" ++ p)
