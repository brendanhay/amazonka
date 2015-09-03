{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.Sum where

import           Network.AWS.Prelude

data ActionCategory
    = Build
    | Deploy
    | Invoke
    | Source
    | Test
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ActionCategory where
    parser = takeLowerText >>= \case
        "build" -> pure Build
        "deploy" -> pure Deploy
        "invoke" -> pure Invoke
        "source" -> pure Source
        "test" -> pure Test
        e -> fromTextError $ "Failure parsing ActionCategory from value: '" <> e
           <> "'. Accepted values: Build, Deploy, Invoke, Source, Test"

instance ToText ActionCategory where
    toText = \case
        Build -> "Build"
        Deploy -> "Deploy"
        Invoke -> "Invoke"
        Source -> "Source"
        Test -> "Test"

instance Hashable     ActionCategory
instance ToByteString ActionCategory
instance ToQuery      ActionCategory
instance ToHeader     ActionCategory

instance ToJSON ActionCategory where
    toJSON = toJSONText

instance FromJSON ActionCategory where
    parseJSON = parseJSONText "ActionCategory"

data ActionConfigurationPropertyType
    = Boolean
    | Number
    | String
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ActionConfigurationPropertyType where
    parser = takeLowerText >>= \case
        "boolean" -> pure Boolean
        "number" -> pure Number
        "string" -> pure String
        e -> fromTextError $ "Failure parsing ActionConfigurationPropertyType from value: '" <> e
           <> "'. Accepted values: Boolean, Number, String"

instance ToText ActionConfigurationPropertyType where
    toText = \case
        Boolean -> "Boolean"
        Number -> "Number"
        String -> "String"

instance Hashable     ActionConfigurationPropertyType
instance ToByteString ActionConfigurationPropertyType
instance ToQuery      ActionConfigurationPropertyType
instance ToHeader     ActionConfigurationPropertyType

instance ToJSON ActionConfigurationPropertyType where
    toJSON = toJSONText

instance FromJSON ActionConfigurationPropertyType where
    parseJSON = parseJSONText "ActionConfigurationPropertyType"

data ActionExecutionStatus
    = Failed
    | InProgress
    | Succeeded
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ActionExecutionStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "inprogress" -> pure InProgress
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing ActionExecutionStatus from value: '" <> e
           <> "'. Accepted values: Failed, InProgress, Succeeded"

instance ToText ActionExecutionStatus where
    toText = \case
        Failed -> "Failed"
        InProgress -> "InProgress"
        Succeeded -> "Succeeded"

instance Hashable     ActionExecutionStatus
instance ToByteString ActionExecutionStatus
instance ToQuery      ActionExecutionStatus
instance ToHeader     ActionExecutionStatus

instance FromJSON ActionExecutionStatus where
    parseJSON = parseJSONText "ActionExecutionStatus"

data ActionOwner
    = AWS
    | Custom
    | ThirdParty
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ActionOwner where
    parser = takeLowerText >>= \case
        "aws" -> pure AWS
        "custom" -> pure Custom
        "thirdparty" -> pure ThirdParty
        e -> fromTextError $ "Failure parsing ActionOwner from value: '" <> e
           <> "'. Accepted values: AWS, Custom, ThirdParty"

instance ToText ActionOwner where
    toText = \case
        AWS -> "AWS"
        Custom -> "Custom"
        ThirdParty -> "ThirdParty"

instance Hashable     ActionOwner
instance ToByteString ActionOwner
instance ToQuery      ActionOwner
instance ToHeader     ActionOwner

instance ToJSON ActionOwner where
    toJSON = toJSONText

instance FromJSON ActionOwner where
    parseJSON = parseJSONText "ActionOwner"

data ArtifactLocationType =
    ALTS3
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ArtifactLocationType where
    parser = takeLowerText >>= \case
        "s3" -> pure ALTS3
        e -> fromTextError $ "Failure parsing ArtifactLocationType from value: '" <> e
           <> "'. Accepted values: S3"

instance ToText ArtifactLocationType where
    toText = \case
        ALTS3 -> "S3"

instance Hashable     ArtifactLocationType
instance ToByteString ArtifactLocationType
instance ToQuery      ArtifactLocationType
instance ToHeader     ArtifactLocationType

instance FromJSON ArtifactLocationType where
    parseJSON = parseJSONText "ArtifactLocationType"

data ArtifactStoreType =
    S3
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ArtifactStoreType where
    parser = takeLowerText >>= \case
        "s3" -> pure S3
        e -> fromTextError $ "Failure parsing ArtifactStoreType from value: '" <> e
           <> "'. Accepted values: S3"

instance ToText ArtifactStoreType where
    toText = \case
        S3 -> "S3"

instance Hashable     ArtifactStoreType
instance ToByteString ArtifactStoreType
instance ToQuery      ArtifactStoreType
instance ToHeader     ArtifactStoreType

instance ToJSON ArtifactStoreType where
    toJSON = toJSONText

instance FromJSON ArtifactStoreType where
    parseJSON = parseJSONText "ArtifactStoreType"

data BlockerType =
    Schedule
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText BlockerType where
    parser = takeLowerText >>= \case
        "schedule" -> pure Schedule
        e -> fromTextError $ "Failure parsing BlockerType from value: '" <> e
           <> "'. Accepted values: Schedule"

instance ToText BlockerType where
    toText = \case
        Schedule -> "Schedule"

instance Hashable     BlockerType
instance ToByteString BlockerType
instance ToQuery      BlockerType
instance ToHeader     BlockerType

instance ToJSON BlockerType where
    toJSON = toJSONText

instance FromJSON BlockerType where
    parseJSON = parseJSONText "BlockerType"

data EncryptionKeyType =
    KMS
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText EncryptionKeyType where
    parser = takeLowerText >>= \case
        "kms" -> pure KMS
        e -> fromTextError $ "Failure parsing EncryptionKeyType from value: '" <> e
           <> "'. Accepted values: KMS"

instance ToText EncryptionKeyType where
    toText = \case
        KMS -> "KMS"

instance Hashable     EncryptionKeyType
instance ToByteString EncryptionKeyType
instance ToQuery      EncryptionKeyType
instance ToHeader     EncryptionKeyType

instance ToJSON EncryptionKeyType where
    toJSON = toJSONText

instance FromJSON EncryptionKeyType where
    parseJSON = parseJSONText "EncryptionKeyType"

data FailureType
    = ConfigurationError
    | JobFailed
    | PermissionError
    | RevisionOutOfSync
    | RevisionUnavailable
    | SystemUnavailable
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText FailureType where
    parser = takeLowerText >>= \case
        "configurationerror" -> pure ConfigurationError
        "jobfailed" -> pure JobFailed
        "permissionerror" -> pure PermissionError
        "revisionoutofsync" -> pure RevisionOutOfSync
        "revisionunavailable" -> pure RevisionUnavailable
        "systemunavailable" -> pure SystemUnavailable
        e -> fromTextError $ "Failure parsing FailureType from value: '" <> e
           <> "'. Accepted values: ConfigurationError, JobFailed, PermissionError, RevisionOutOfSync, RevisionUnavailable, SystemUnavailable"

instance ToText FailureType where
    toText = \case
        ConfigurationError -> "ConfigurationError"
        JobFailed -> "JobFailed"
        PermissionError -> "PermissionError"
        RevisionOutOfSync -> "RevisionOutOfSync"
        RevisionUnavailable -> "RevisionUnavailable"
        SystemUnavailable -> "SystemUnavailable"

instance Hashable     FailureType
instance ToByteString FailureType
instance ToQuery      FailureType
instance ToHeader     FailureType

instance ToJSON FailureType where
    toJSON = toJSONText

data JobStatus
    = JSCreated
    | JSDispatched
    | JSFailed
    | JSInProgress
    | JSQueued
    | JSSucceeded
    | JSTimedOut
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText JobStatus where
    parser = takeLowerText >>= \case
        "created" -> pure JSCreated
        "dispatched" -> pure JSDispatched
        "failed" -> pure JSFailed
        "inprogress" -> pure JSInProgress
        "queued" -> pure JSQueued
        "succeeded" -> pure JSSucceeded
        "timedout" -> pure JSTimedOut
        e -> fromTextError $ "Failure parsing JobStatus from value: '" <> e
           <> "'. Accepted values: Created, Dispatched, Failed, InProgress, Queued, Succeeded, TimedOut"

instance ToText JobStatus where
    toText = \case
        JSCreated -> "Created"
        JSDispatched -> "Dispatched"
        JSFailed -> "Failed"
        JSInProgress -> "InProgress"
        JSQueued -> "Queued"
        JSSucceeded -> "Succeeded"
        JSTimedOut -> "TimedOut"

instance Hashable     JobStatus
instance ToByteString JobStatus
instance ToQuery      JobStatus
instance ToHeader     JobStatus

instance FromJSON JobStatus where
    parseJSON = parseJSONText "JobStatus"

data StageTransitionType
    = Inbound
    | Outbound
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StageTransitionType where
    parser = takeLowerText >>= \case
        "inbound" -> pure Inbound
        "outbound" -> pure Outbound
        e -> fromTextError $ "Failure parsing StageTransitionType from value: '" <> e
           <> "'. Accepted values: Inbound, Outbound"

instance ToText StageTransitionType where
    toText = \case
        Inbound -> "Inbound"
        Outbound -> "Outbound"

instance Hashable     StageTransitionType
instance ToByteString StageTransitionType
instance ToQuery      StageTransitionType
instance ToHeader     StageTransitionType

instance ToJSON StageTransitionType where
    toJSON = toJSONText
