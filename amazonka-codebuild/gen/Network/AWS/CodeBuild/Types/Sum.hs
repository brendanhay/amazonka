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

data BuildPhaseType
  = Build
  | Completed
  | DownloadSource
  | Finalizing
  | Install
  | PostBuild
  | PreBuild
  | Provisioning
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
        "submitted" -> pure Submitted
        "upload_artifacts" -> pure UploadArtifacts
        e -> fromTextError $ "Failure parsing BuildPhaseType from value: '" <> e
           <> "'. Accepted values: build, completed, download_source, finalizing, install, post_build, pre_build, provisioning, submitted, upload_artifacts"

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
        Submitted -> "SUBMITTED"
        UploadArtifacts -> "UPLOAD_ARTIFACTS"

instance Hashable     BuildPhaseType
instance NFData       BuildPhaseType
instance ToByteString BuildPhaseType
instance ToQuery      BuildPhaseType
instance ToHeader     BuildPhaseType

instance FromJSON BuildPhaseType where
    parseJSON = parseJSONText "BuildPhaseType"

data CacheType
  = CTNoCache
  | CTS3
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CacheType where
    parser = takeLowerText >>= \case
        "no_cache" -> pure CTNoCache
        "s3" -> pure CTS3
        e -> fromTextError $ "Failure parsing CacheType from value: '" <> e
           <> "'. Accepted values: no_cache, s3"

instance ToText CacheType where
    toText = \case
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

data EnvironmentType =
  LinuxContainer
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EnvironmentType where
    parser = takeLowerText >>= \case
        "linux_container" -> pure LinuxContainer
        e -> fromTextError $ "Failure parsing EnvironmentType from value: '" <> e
           <> "'. Accepted values: linux_container"

instance ToText EnvironmentType where
    toText = \case
        LinuxContainer -> "LINUX_CONTAINER"

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

data LanguageType
  = Android
  | Base
  | Docker
  | Dotnet
  | Golang
  | Java
  | NodeJs
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
        "python" -> pure Python
        "ruby" -> pure Ruby
        e -> fromTextError $ "Failure parsing LanguageType from value: '" <> e
           <> "'. Accepted values: android, base, docker, dotnet, golang, java, node_js, python, ruby"

instance ToText LanguageType where
    toText = \case
        Android -> "ANDROID"
        Base -> "BASE"
        Docker -> "DOCKER"
        Dotnet -> "DOTNET"
        Golang -> "GOLANG"
        Java -> "JAVA"
        NodeJs -> "NODE_JS"
        Python -> "PYTHON"
        Ruby -> "RUBY"

instance Hashable     LanguageType
instance NFData       LanguageType
instance ToByteString LanguageType
instance ToQuery      LanguageType
instance ToHeader     LanguageType

instance FromJSON LanguageType where
    parseJSON = parseJSONText "LanguageType"

data PlatformType
  = AmazonLinux
  | Debian
  | Ubuntu
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PlatformType where
    parser = takeLowerText >>= \case
        "amazon_linux" -> pure AmazonLinux
        "debian" -> pure Debian
        "ubuntu" -> pure Ubuntu
        e -> fromTextError $ "Failure parsing PlatformType from value: '" <> e
           <> "'. Accepted values: amazon_linux, debian, ubuntu"

instance ToText PlatformType where
    toText = \case
        AmazonLinux -> "AMAZON_LINUX"
        Debian -> "DEBIAN"
        Ubuntu -> "UBUNTU"

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
  | STS3
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SourceType where
    parser = takeLowerText >>= \case
        "bitbucket" -> pure STBitbucket
        "codecommit" -> pure STCodecommit
        "codepipeline" -> pure STCodepipeline
        "github" -> pure STGithub
        "github_enterprise" -> pure STGithubEnterprise
        "s3" -> pure STS3
        e -> fromTextError $ "Failure parsing SourceType from value: '" <> e
           <> "'. Accepted values: bitbucket, codecommit, codepipeline, github, github_enterprise, s3"

instance ToText SourceType where
    toText = \case
        STBitbucket -> "BITBUCKET"
        STCodecommit -> "CODECOMMIT"
        STCodepipeline -> "CODEPIPELINE"
        STGithub -> "GITHUB"
        STGithubEnterprise -> "GITHUB_ENTERPRISE"
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
