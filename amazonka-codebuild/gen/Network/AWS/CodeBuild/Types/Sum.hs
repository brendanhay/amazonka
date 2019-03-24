{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.Sum where

import Network.AWS.Prelude

data ArtifactNamespace
  = ANBuildId
  | ANNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ArtifactNamespace where
    parser = takeLowerText >>= \case
        "build_id" -> pure ANBuildId
        "none" -> pure ANNone
        e -> fromTextError $ "Failure parsing ArtifactNamespace from value: '" <> e
           <> "'. Accepted values: build_id, none"

instance ToText ArtifactNamespace where
    toText = \case
        ANBuildId -> "BUILD_ID"
        ANNone -> "NONE"

instance Hashable     ArtifactNamespace
instance NFData       ArtifactNamespace
instance ToByteString ArtifactNamespace
instance ToQuery      ArtifactNamespace
instance ToHeader     ArtifactNamespace

instance ToJSON ArtifactNamespace where
    toJSON = toJSONText

instance FromJSON ArtifactNamespace where
    parseJSON = parseJSONText "ArtifactNamespace"

data ArtifactPackaging
  = None
  | Zip
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ArtifactPackaging where
    parser = takeLowerText >>= \case
        "none" -> pure None
        "zip" -> pure Zip
        e -> fromTextError $ "Failure parsing ArtifactPackaging from value: '" <> e
           <> "'. Accepted values: none, zip"

instance ToText ArtifactPackaging where
    toText = \case
        None -> "NONE"
        Zip -> "ZIP"

instance Hashable     ArtifactPackaging
instance NFData       ArtifactPackaging
instance ToByteString ArtifactPackaging
instance ToQuery      ArtifactPackaging
instance ToHeader     ArtifactPackaging

instance ToJSON ArtifactPackaging where
    toJSON = toJSONText

instance FromJSON ArtifactPackaging where
    parseJSON = parseJSONText "ArtifactPackaging"

data ArtifactsType
  = Codepipeline
  | NoArtifacts
  | S3
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ArtifactsType where
    parser = takeLowerText >>= \case
        "codepipeline" -> pure Codepipeline
        "no_artifacts" -> pure NoArtifacts
        "s3" -> pure S3
        e -> fromTextError $ "Failure parsing ArtifactsType from value: '" <> e
           <> "'. Accepted values: codepipeline, no_artifacts, s3"

instance ToText ArtifactsType where
    toText = \case
        Codepipeline -> "CODEPIPELINE"
        NoArtifacts -> "NO_ARTIFACTS"
        S3 -> "S3"

instance Hashable     ArtifactsType
instance NFData       ArtifactsType
instance ToByteString ArtifactsType
instance ToQuery      ArtifactsType
instance ToHeader     ArtifactsType

instance ToJSON ArtifactsType where
    toJSON = toJSONText

instance FromJSON ArtifactsType where
    parseJSON = parseJSONText "ArtifactsType"

data AuthType
  = ATBasicAuth
  | ATOauth
  | ATPersonalAccessToken
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AuthType where
    parser = takeLowerText >>= \case
        "basic_auth" -> pure ATBasicAuth
        "oauth" -> pure ATOauth
        "personal_access_token" -> pure ATPersonalAccessToken
        e -> fromTextError $ "Failure parsing AuthType from value: '" <> e
           <> "'. Accepted values: basic_auth, oauth, personal_access_token"

instance ToText AuthType where
    toText = \case
        ATBasicAuth -> "BASIC_AUTH"
        ATOauth -> "OAUTH"
        ATPersonalAccessToken -> "PERSONAL_ACCESS_TOKEN"

instance Hashable     AuthType
instance NFData       AuthType
instance ToByteString AuthType
instance ToQuery      AuthType
instance ToHeader     AuthType

instance ToJSON AuthType where
    toJSON = toJSONText

instance FromJSON AuthType where
    parseJSON = parseJSONText "AuthType"

data BuildPhaseType
  = Build
  | Completed
  | DownloadSource
  | Finalizing
  | Install
  | PostBuild
  | PreBuild
  | Provisioning
  | Queued
  | Submitted
  | UploadArtifacts
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BuildPhaseType where
    parser = takeLowerText >>= \case
        "build" -> pure Build
        "completed" -> pure Completed
        "download_source" -> pure DownloadSource
        "finalizing" -> pure Finalizing
        "install" -> pure Install
        "post_build" -> pure PostBuild
        "pre_build" -> pure PreBuild
        "provisioning" -> pure Provisioning
        "queued" -> pure Queued
        "submitted" -> pure Submitted
        "upload_artifacts" -> pure UploadArtifacts
        e -> fromTextError $ "Failure parsing BuildPhaseType from value: '" <> e
           <> "'. Accepted values: build, completed, download_source, finalizing, install, post_build, pre_build, provisioning, queued, submitted, upload_artifacts"

instance ToText BuildPhaseType where
    toText = \case
        Build -> "BUILD"
        Completed -> "COMPLETED"
        DownloadSource -> "DOWNLOAD_SOURCE"
        Finalizing -> "FINALIZING"
        Install -> "INSTALL"
        PostBuild -> "POST_BUILD"
        PreBuild -> "PRE_BUILD"
        Provisioning -> "PROVISIONING"
        Queued -> "QUEUED"
        Submitted -> "SUBMITTED"
        UploadArtifacts -> "UPLOAD_ARTIFACTS"

instance Hashable     BuildPhaseType
instance NFData       BuildPhaseType
instance ToByteString BuildPhaseType
instance ToQuery      BuildPhaseType
instance ToHeader     BuildPhaseType

instance FromJSON BuildPhaseType where
    parseJSON = parseJSONText "BuildPhaseType"

data CacheMode
  = LocalCustomCache
  | LocalDockerLayerCache
  | LocalSourceCache
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CacheMode where
    parser = takeLowerText >>= \case
        "local_custom_cache" -> pure LocalCustomCache
        "local_docker_layer_cache" -> pure LocalDockerLayerCache
        "local_source_cache" -> pure LocalSourceCache
        e -> fromTextError $ "Failure parsing CacheMode from value: '" <> e
           <> "'. Accepted values: local_custom_cache, local_docker_layer_cache, local_source_cache"

instance ToText CacheMode where
    toText = \case
        LocalCustomCache -> "LOCAL_CUSTOM_CACHE"
        LocalDockerLayerCache -> "LOCAL_DOCKER_LAYER_CACHE"
        LocalSourceCache -> "LOCAL_SOURCE_CACHE"

instance Hashable     CacheMode
instance NFData       CacheMode
instance ToByteString CacheMode
instance ToQuery      CacheMode
instance ToHeader     CacheMode

instance ToJSON CacheMode where
    toJSON = toJSONText

instance FromJSON CacheMode where
    parseJSON = parseJSONText "CacheMode"

data CacheType
  = CTLocal
  | CTNoCache
  | CTS3
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CacheType where
    parser = takeLowerText >>= \case
        "local" -> pure CTLocal
        "no_cache" -> pure CTNoCache
        "s3" -> pure CTS3
        e -> fromTextError $ "Failure parsing CacheType from value: '" <> e
           <> "'. Accepted values: local, no_cache, s3"

instance ToText CacheType where
    toText = \case
        CTLocal -> "LOCAL"
        CTNoCache -> "NO_CACHE"
        CTS3 -> "S3"

instance Hashable     CacheType
instance NFData       CacheType
instance ToByteString CacheType
instance ToQuery      CacheType
instance ToHeader     CacheType

instance ToJSON CacheType where
    toJSON = toJSONText

instance FromJSON CacheType where
    parseJSON = parseJSONText "CacheType"

data ComputeType
  = BuildGENERAL1Large
  | BuildGENERAL1Medium
  | BuildGENERAL1Small
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ComputeType where
    parser = takeLowerText >>= \case
        "build_general1_large" -> pure BuildGENERAL1Large
        "build_general1_medium" -> pure BuildGENERAL1Medium
        "build_general1_small" -> pure BuildGENERAL1Small
        e -> fromTextError $ "Failure parsing ComputeType from value: '" <> e
           <> "'. Accepted values: build_general1_large, build_general1_medium, build_general1_small"

instance ToText ComputeType where
    toText = \case
        BuildGENERAL1Large -> "BUILD_GENERAL1_LARGE"
        BuildGENERAL1Medium -> "BUILD_GENERAL1_MEDIUM"
        BuildGENERAL1Small -> "BUILD_GENERAL1_SMALL"

instance Hashable     ComputeType
instance NFData       ComputeType
instance ToByteString ComputeType
instance ToQuery      ComputeType
instance ToHeader     ComputeType

instance ToJSON ComputeType where
    toJSON = toJSONText

instance FromJSON ComputeType where
    parseJSON = parseJSONText "ComputeType"

data CredentialProviderType =
  SecretsManager
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CredentialProviderType where
    parser = takeLowerText >>= \case
        "secrets_manager" -> pure SecretsManager
        e -> fromTextError $ "Failure parsing CredentialProviderType from value: '" <> e
           <> "'. Accepted values: secrets_manager"

instance ToText CredentialProviderType where
    toText = \case
        SecretsManager -> "SECRETS_MANAGER"

instance Hashable     CredentialProviderType
instance NFData       CredentialProviderType
instance ToByteString CredentialProviderType
instance ToQuery      CredentialProviderType
instance ToHeader     CredentialProviderType

instance ToJSON CredentialProviderType where
    toJSON = toJSONText

instance FromJSON CredentialProviderType where
    parseJSON = parseJSONText "CredentialProviderType"

data EnvironmentType
  = LinuxContainer
  | WindowsContainer
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EnvironmentType where
    parser = takeLowerText >>= \case
        "linux_container" -> pure LinuxContainer
        "windows_container" -> pure WindowsContainer
        e -> fromTextError $ "Failure parsing EnvironmentType from value: '" <> e
           <> "'. Accepted values: linux_container, windows_container"

instance ToText EnvironmentType where
    toText = \case
        LinuxContainer -> "LINUX_CONTAINER"
        WindowsContainer -> "WINDOWS_CONTAINER"

instance Hashable     EnvironmentType
instance NFData       EnvironmentType
instance ToByteString EnvironmentType
instance ToQuery      EnvironmentType
instance ToHeader     EnvironmentType

instance ToJSON EnvironmentType where
    toJSON = toJSONText

instance FromJSON EnvironmentType where
    parseJSON = parseJSONText "EnvironmentType"

data EnvironmentVariableType
  = ParameterStore
  | Plaintext
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EnvironmentVariableType where
    parser = takeLowerText >>= \case
        "parameter_store" -> pure ParameterStore
        "plaintext" -> pure Plaintext
        e -> fromTextError $ "Failure parsing EnvironmentVariableType from value: '" <> e
           <> "'. Accepted values: parameter_store, plaintext"

instance ToText EnvironmentVariableType where
    toText = \case
        ParameterStore -> "PARAMETER_STORE"
        Plaintext -> "PLAINTEXT"

instance Hashable     EnvironmentVariableType
instance NFData       EnvironmentVariableType
instance ToByteString EnvironmentVariableType
instance ToQuery      EnvironmentVariableType
instance ToHeader     EnvironmentVariableType

instance ToJSON EnvironmentVariableType where
    toJSON = toJSONText

instance FromJSON EnvironmentVariableType where
    parseJSON = parseJSONText "EnvironmentVariableType"

data ImagePullCredentialsType
  = Codebuild
  | ServiceRole
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ImagePullCredentialsType where
    parser = takeLowerText >>= \case
        "codebuild" -> pure Codebuild
        "service_role" -> pure ServiceRole
        e -> fromTextError $ "Failure parsing ImagePullCredentialsType from value: '" <> e
           <> "'. Accepted values: codebuild, service_role"

instance ToText ImagePullCredentialsType where
    toText = \case
        Codebuild -> "CODEBUILD"
        ServiceRole -> "SERVICE_ROLE"

instance Hashable     ImagePullCredentialsType
instance NFData       ImagePullCredentialsType
instance ToByteString ImagePullCredentialsType
instance ToQuery      ImagePullCredentialsType
instance ToHeader     ImagePullCredentialsType

instance ToJSON ImagePullCredentialsType where
    toJSON = toJSONText

instance FromJSON ImagePullCredentialsType where
    parseJSON = parseJSONText "ImagePullCredentialsType"

data LanguageType
  = Android
  | Base
  | Docker
  | Dotnet
  | Golang
  | Java
  | NodeJs
  | PHP
  | Python
  | Ruby
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LanguageType where
    parser = takeLowerText >>= \case
        "android" -> pure Android
        "base" -> pure Base
        "docker" -> pure Docker
        "dotnet" -> pure Dotnet
        "golang" -> pure Golang
        "java" -> pure Java
        "node_js" -> pure NodeJs
        "php" -> pure PHP
        "python" -> pure Python
        "ruby" -> pure Ruby
        e -> fromTextError $ "Failure parsing LanguageType from value: '" <> e
           <> "'. Accepted values: android, base, docker, dotnet, golang, java, node_js, php, python, ruby"

instance ToText LanguageType where
    toText = \case
        Android -> "ANDROID"
        Base -> "BASE"
        Docker -> "DOCKER"
        Dotnet -> "DOTNET"
        Golang -> "GOLANG"
        Java -> "JAVA"
        NodeJs -> "NODE_JS"
        PHP -> "PHP"
        Python -> "PYTHON"
        Ruby -> "RUBY"

instance Hashable     LanguageType
instance NFData       LanguageType
instance ToByteString LanguageType
instance ToQuery      LanguageType
instance ToHeader     LanguageType

instance FromJSON LanguageType where
    parseJSON = parseJSONText "LanguageType"

data LogsConfigStatusType
  = Disabled
  | Enabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LogsConfigStatusType where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing LogsConfigStatusType from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText LogsConfigStatusType where
    toText = \case
        Disabled -> "DISABLED"
        Enabled -> "ENABLED"

instance Hashable     LogsConfigStatusType
instance NFData       LogsConfigStatusType
instance ToByteString LogsConfigStatusType
instance ToQuery      LogsConfigStatusType
instance ToHeader     LogsConfigStatusType

instance ToJSON LogsConfigStatusType where
    toJSON = toJSONText

instance FromJSON LogsConfigStatusType where
    parseJSON = parseJSONText "LogsConfigStatusType"

data PlatformType
  = AmazonLinux
  | Debian
  | Ubuntu
  | WindowsServer
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PlatformType where
    parser = takeLowerText >>= \case
        "amazon_linux" -> pure AmazonLinux
        "debian" -> pure Debian
        "ubuntu" -> pure Ubuntu
        "windows_server" -> pure WindowsServer
        e -> fromTextError $ "Failure parsing PlatformType from value: '" <> e
           <> "'. Accepted values: amazon_linux, debian, ubuntu, windows_server"

instance ToText PlatformType where
    toText = \case
        AmazonLinux -> "AMAZON_LINUX"
        Debian -> "DEBIAN"
        Ubuntu -> "UBUNTU"
        WindowsServer -> "WINDOWS_SERVER"

instance Hashable     PlatformType
instance NFData       PlatformType
instance ToByteString PlatformType
instance ToQuery      PlatformType
instance ToHeader     PlatformType

instance FromJSON PlatformType where
    parseJSON = parseJSONText "PlatformType"

data ProjectSortByType
  = CreatedTime
  | LastModifiedTime
  | Name
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProjectSortByType where
    parser = takeLowerText >>= \case
        "created_time" -> pure CreatedTime
        "last_modified_time" -> pure LastModifiedTime
        "name" -> pure Name
        e -> fromTextError $ "Failure parsing ProjectSortByType from value: '" <> e
           <> "'. Accepted values: created_time, last_modified_time, name"

instance ToText ProjectSortByType where
    toText = \case
        CreatedTime -> "CREATED_TIME"
        LastModifiedTime -> "LAST_MODIFIED_TIME"
        Name -> "NAME"

instance Hashable     ProjectSortByType
instance NFData       ProjectSortByType
instance ToByteString ProjectSortByType
instance ToQuery      ProjectSortByType
instance ToHeader     ProjectSortByType

instance ToJSON ProjectSortByType where
    toJSON = toJSONText

data ServerType
  = Bitbucket
  | Github
  | GithubEnterprise
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ServerType where
    parser = takeLowerText >>= \case
        "bitbucket" -> pure Bitbucket
        "github" -> pure Github
        "github_enterprise" -> pure GithubEnterprise
        e -> fromTextError $ "Failure parsing ServerType from value: '" <> e
           <> "'. Accepted values: bitbucket, github, github_enterprise"

instance ToText ServerType where
    toText = \case
        Bitbucket -> "BITBUCKET"
        Github -> "GITHUB"
        GithubEnterprise -> "GITHUB_ENTERPRISE"

instance Hashable     ServerType
instance NFData       ServerType
instance ToByteString ServerType
instance ToQuery      ServerType
instance ToHeader     ServerType

instance ToJSON ServerType where
    toJSON = toJSONText

instance FromJSON ServerType where
    parseJSON = parseJSONText "ServerType"

data SortOrderType
  = Ascending
  | Descending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SortOrderType where
    parser = takeLowerText >>= \case
        "ascending" -> pure Ascending
        "descending" -> pure Descending
        e -> fromTextError $ "Failure parsing SortOrderType from value: '" <> e
           <> "'. Accepted values: ascending, descending"

instance ToText SortOrderType where
    toText = \case
        Ascending -> "ASCENDING"
        Descending -> "DESCENDING"

instance Hashable     SortOrderType
instance NFData       SortOrderType
instance ToByteString SortOrderType
instance ToQuery      SortOrderType
instance ToHeader     SortOrderType

instance ToJSON SortOrderType where
    toJSON = toJSONText

data SourceAuthType =
  Oauth
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SourceAuthType where
    parser = takeLowerText >>= \case
        "oauth" -> pure Oauth
        e -> fromTextError $ "Failure parsing SourceAuthType from value: '" <> e
           <> "'. Accepted values: oauth"

instance ToText SourceAuthType where
    toText = \case
        Oauth -> "OAUTH"

instance Hashable     SourceAuthType
instance NFData       SourceAuthType
instance ToByteString SourceAuthType
instance ToQuery      SourceAuthType
instance ToHeader     SourceAuthType

instance ToJSON SourceAuthType where
    toJSON = toJSONText

instance FromJSON SourceAuthType where
    parseJSON = parseJSONText "SourceAuthType"

data SourceType
  = STBitbucket
  | STCodecommit
  | STCodepipeline
  | STGithub
  | STGithubEnterprise
  | STNoSource
  | STS3
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SourceType where
    parser = takeLowerText >>= \case
        "bitbucket" -> pure STBitbucket
        "codecommit" -> pure STCodecommit
        "codepipeline" -> pure STCodepipeline
        "github" -> pure STGithub
        "github_enterprise" -> pure STGithubEnterprise
        "no_source" -> pure STNoSource
        "s3" -> pure STS3
        e -> fromTextError $ "Failure parsing SourceType from value: '" <> e
           <> "'. Accepted values: bitbucket, codecommit, codepipeline, github, github_enterprise, no_source, s3"

instance ToText SourceType where
    toText = \case
        STBitbucket -> "BITBUCKET"
        STCodecommit -> "CODECOMMIT"
        STCodepipeline -> "CODEPIPELINE"
        STGithub -> "GITHUB"
        STGithubEnterprise -> "GITHUB_ENTERPRISE"
        STNoSource -> "NO_SOURCE"
        STS3 -> "S3"

instance Hashable     SourceType
instance NFData       SourceType
instance ToByteString SourceType
instance ToQuery      SourceType
instance ToHeader     SourceType

instance ToJSON SourceType where
    toJSON = toJSONText

instance FromJSON SourceType where
    parseJSON = parseJSONText "SourceType"

data StatusType
  = Failed
  | Fault
  | InProgress
  | Stopped
  | Succeeded
  | TimedOut
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StatusType where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "fault" -> pure Fault
        "in_progress" -> pure InProgress
        "stopped" -> pure Stopped
        "succeeded" -> pure Succeeded
        "timed_out" -> pure TimedOut
        e -> fromTextError $ "Failure parsing StatusType from value: '" <> e
           <> "'. Accepted values: failed, fault, in_progress, stopped, succeeded, timed_out"

instance ToText StatusType where
    toText = \case
        Failed -> "FAILED"
        Fault -> "FAULT"
        InProgress -> "IN_PROGRESS"
        Stopped -> "STOPPED"
        Succeeded -> "SUCCEEDED"
        TimedOut -> "TIMED_OUT"

instance Hashable     StatusType
instance NFData       StatusType
instance ToByteString StatusType
instance ToQuery      StatusType
instance ToHeader     StatusType

instance FromJSON StatusType where
    parseJSON = parseJSONText "StatusType"

data WebhookFilterType
  = ActorAccountId
  | BaseRef
  | Event
  | FilePath
  | HeadRef
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText WebhookFilterType where
    parser = takeLowerText >>= \case
        "actor_account_id" -> pure ActorAccountId
        "base_ref" -> pure BaseRef
        "event" -> pure Event
        "file_path" -> pure FilePath
        "head_ref" -> pure HeadRef
        e -> fromTextError $ "Failure parsing WebhookFilterType from value: '" <> e
           <> "'. Accepted values: actor_account_id, base_ref, event, file_path, head_ref"

instance ToText WebhookFilterType where
    toText = \case
        ActorAccountId -> "ACTOR_ACCOUNT_ID"
        BaseRef -> "BASE_REF"
        Event -> "EVENT"
        FilePath -> "FILE_PATH"
        HeadRef -> "HEAD_REF"

instance Hashable     WebhookFilterType
instance NFData       WebhookFilterType
instance ToByteString WebhookFilterType
instance ToQuery      WebhookFilterType
instance ToHeader     WebhookFilterType

instance ToJSON WebhookFilterType where
    toJSON = toJSONText

instance FromJSON WebhookFilterType where
    parseJSON = parseJSONText "WebhookFilterType"
