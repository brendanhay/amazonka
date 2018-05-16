{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.Sum where

import Network.AWS.Prelude

data ActionCategory
  = Approval
  | Build
  | Deploy
  | Invoke
  | Source
  | Test
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ActionCategory where
    parser = takeLowerText >>= \case
        "approval" -> pure Approval
        "build" -> pure Build
        "deploy" -> pure Deploy
        "invoke" -> pure Invoke
        "source" -> pure Source
        "test" -> pure Test
        e -> fromTextError $ "Failure parsing ActionCategory from value: '" <> e
           <> "'. Accepted values: approval, build, deploy, invoke, source, test"

instance ToText ActionCategory where
    toText = \case
        Approval -> "Approval"
        Build -> "Build"
        Deploy -> "Deploy"
        Invoke -> "Invoke"
        Source -> "Source"
        Test -> "Test"

instance Hashable     ActionCategory
instance NFData       ActionCategory
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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ActionConfigurationPropertyType where
    parser = takeLowerText >>= \case
        "boolean" -> pure Boolean
        "number" -> pure Number
        "string" -> pure String
        e -> fromTextError $ "Failure parsing ActionConfigurationPropertyType from value: '" <> e
           <> "'. Accepted values: boolean, number, string"

instance ToText ActionConfigurationPropertyType where
    toText = \case
        Boolean -> "Boolean"
        Number -> "Number"
        String -> "String"

instance Hashable     ActionConfigurationPropertyType
instance NFData       ActionConfigurationPropertyType
instance ToByteString ActionConfigurationPropertyType
instance ToQuery      ActionConfigurationPropertyType
instance ToHeader     ActionConfigurationPropertyType

instance ToJSON ActionConfigurationPropertyType where
    toJSON = toJSONText

instance FromJSON ActionConfigurationPropertyType where
    parseJSON = parseJSONText "ActionConfigurationPropertyType"

data ActionExecutionStatus
  = AESFailed
  | AESInProgress
  | AESSucceeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ActionExecutionStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure AESFailed
        "inprogress" -> pure AESInProgress
        "succeeded" -> pure AESSucceeded
        e -> fromTextError $ "Failure parsing ActionExecutionStatus from value: '" <> e
           <> "'. Accepted values: failed, inprogress, succeeded"

instance ToText ActionExecutionStatus where
    toText = \case
        AESFailed -> "Failed"
        AESInProgress -> "InProgress"
        AESSucceeded -> "Succeeded"

instance Hashable     ActionExecutionStatus
instance NFData       ActionExecutionStatus
instance ToByteString ActionExecutionStatus
instance ToQuery      ActionExecutionStatus
instance ToHeader     ActionExecutionStatus

instance FromJSON ActionExecutionStatus where
    parseJSON = parseJSONText "ActionExecutionStatus"

data ActionOwner
  = AWS
  | Custom
  | ThirdParty
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ActionOwner where
    parser = takeLowerText >>= \case
        "aws" -> pure AWS
        "custom" -> pure Custom
        "thirdparty" -> pure ThirdParty
        e -> fromTextError $ "Failure parsing ActionOwner from value: '" <> e
           <> "'. Accepted values: aws, custom, thirdparty"

instance ToText ActionOwner where
    toText = \case
        AWS -> "AWS"
        Custom -> "Custom"
        ThirdParty -> "ThirdParty"

instance Hashable     ActionOwner
instance NFData       ActionOwner
instance ToByteString ActionOwner
instance ToQuery      ActionOwner
instance ToHeader     ActionOwner

instance ToJSON ActionOwner where
    toJSON = toJSONText

instance FromJSON ActionOwner where
    parseJSON = parseJSONText "ActionOwner"

data ApprovalStatus
  = Approved
  | Rejected
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ApprovalStatus where
    parser = takeLowerText >>= \case
        "approved" -> pure Approved
        "rejected" -> pure Rejected
        e -> fromTextError $ "Failure parsing ApprovalStatus from value: '" <> e
           <> "'. Accepted values: approved, rejected"

instance ToText ApprovalStatus where
    toText = \case
        Approved -> "Approved"
        Rejected -> "Rejected"

instance Hashable     ApprovalStatus
instance NFData       ApprovalStatus
instance ToByteString ApprovalStatus
instance ToQuery      ApprovalStatus
instance ToHeader     ApprovalStatus

instance ToJSON ApprovalStatus where
    toJSON = toJSONText

data ArtifactLocationType =
  ALTS3
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ArtifactLocationType where
    parser = takeLowerText >>= \case
        "s3" -> pure ALTS3
        e -> fromTextError $ "Failure parsing ArtifactLocationType from value: '" <> e
           <> "'. Accepted values: s3"

instance ToText ArtifactLocationType where
    toText = \case
        ALTS3 -> "S3"

instance Hashable     ArtifactLocationType
instance NFData       ArtifactLocationType
instance ToByteString ArtifactLocationType
instance ToQuery      ArtifactLocationType
instance ToHeader     ArtifactLocationType

instance FromJSON ArtifactLocationType where
    parseJSON = parseJSONText "ArtifactLocationType"

data ArtifactStoreType =
  S3
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ArtifactStoreType where
    parser = takeLowerText >>= \case
        "s3" -> pure S3
        e -> fromTextError $ "Failure parsing ArtifactStoreType from value: '" <> e
           <> "'. Accepted values: s3"

instance ToText ArtifactStoreType where
    toText = \case
        S3 -> "S3"

instance Hashable     ArtifactStoreType
instance NFData       ArtifactStoreType
instance ToByteString ArtifactStoreType
instance ToQuery      ArtifactStoreType
instance ToHeader     ArtifactStoreType

instance ToJSON ArtifactStoreType where
    toJSON = toJSONText

instance FromJSON ArtifactStoreType where
    parseJSON = parseJSONText "ArtifactStoreType"

data BlockerType =
  Schedule
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BlockerType where
    parser = takeLowerText >>= \case
        "schedule" -> pure Schedule
        e -> fromTextError $ "Failure parsing BlockerType from value: '" <> e
           <> "'. Accepted values: schedule"

instance ToText BlockerType where
    toText = \case
        Schedule -> "Schedule"

instance Hashable     BlockerType
instance NFData       BlockerType
instance ToByteString BlockerType
instance ToQuery      BlockerType
instance ToHeader     BlockerType

instance ToJSON BlockerType where
    toJSON = toJSONText

instance FromJSON BlockerType where
    parseJSON = parseJSONText "BlockerType"

data EncryptionKeyType =
  KMS
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EncryptionKeyType where
    parser = takeLowerText >>= \case
        "kms" -> pure KMS
        e -> fromTextError $ "Failure parsing EncryptionKeyType from value: '" <> e
           <> "'. Accepted values: kms"

instance ToText EncryptionKeyType where
    toText = \case
        KMS -> "KMS"

instance Hashable     EncryptionKeyType
instance NFData       EncryptionKeyType
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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FailureType where
    parser = takeLowerText >>= \case
        "configurationerror" -> pure ConfigurationError
        "jobfailed" -> pure JobFailed
        "permissionerror" -> pure PermissionError
        "revisionoutofsync" -> pure RevisionOutOfSync
        "revisionunavailable" -> pure RevisionUnavailable
        "systemunavailable" -> pure SystemUnavailable
        e -> fromTextError $ "Failure parsing FailureType from value: '" <> e
           <> "'. Accepted values: configurationerror, jobfailed, permissionerror, revisionoutofsync, revisionunavailable, systemunavailable"

instance ToText FailureType where
    toText = \case
        ConfigurationError -> "ConfigurationError"
        JobFailed -> "JobFailed"
        PermissionError -> "PermissionError"
        RevisionOutOfSync -> "RevisionOutOfSync"
        RevisionUnavailable -> "RevisionUnavailable"
        SystemUnavailable -> "SystemUnavailable"

instance Hashable     FailureType
instance NFData       FailureType
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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
           <> "'. Accepted values: created, dispatched, failed, inprogress, queued, succeeded, timedout"

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
instance NFData       JobStatus
instance ToByteString JobStatus
instance ToQuery      JobStatus
instance ToHeader     JobStatus

instance FromJSON JobStatus where
    parseJSON = parseJSONText "JobStatus"

data PipelineExecutionStatus
  = Failed
  | InProgress
  | Succeeded
  | Superseded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PipelineExecutionStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "inprogress" -> pure InProgress
        "succeeded" -> pure Succeeded
        "superseded" -> pure Superseded
        e -> fromTextError $ "Failure parsing PipelineExecutionStatus from value: '" <> e
           <> "'. Accepted values: failed, inprogress, succeeded, superseded"

instance ToText PipelineExecutionStatus where
    toText = \case
        Failed -> "Failed"
        InProgress -> "InProgress"
        Succeeded -> "Succeeded"
        Superseded -> "Superseded"

instance Hashable     PipelineExecutionStatus
instance NFData       PipelineExecutionStatus
instance ToByteString PipelineExecutionStatus
instance ToQuery      PipelineExecutionStatus
instance ToHeader     PipelineExecutionStatus

instance FromJSON PipelineExecutionStatus where
    parseJSON = parseJSONText "PipelineExecutionStatus"

data StageExecutionStatus
  = SESFailed
  | SESInProgress
  | SESSucceeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StageExecutionStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure SESFailed
        "inprogress" -> pure SESInProgress
        "succeeded" -> pure SESSucceeded
        e -> fromTextError $ "Failure parsing StageExecutionStatus from value: '" <> e
           <> "'. Accepted values: failed, inprogress, succeeded"

instance ToText StageExecutionStatus where
    toText = \case
        SESFailed -> "Failed"
        SESInProgress -> "InProgress"
        SESSucceeded -> "Succeeded"

instance Hashable     StageExecutionStatus
instance NFData       StageExecutionStatus
instance ToByteString StageExecutionStatus
instance ToQuery      StageExecutionStatus
instance ToHeader     StageExecutionStatus

instance FromJSON StageExecutionStatus where
    parseJSON = parseJSONText "StageExecutionStatus"

data StageRetryMode =
  FailedActions
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StageRetryMode where
    parser = takeLowerText >>= \case
        "failed_actions" -> pure FailedActions
        e -> fromTextError $ "Failure parsing StageRetryMode from value: '" <> e
           <> "'. Accepted values: failed_actions"

instance ToText StageRetryMode where
    toText = \case
        FailedActions -> "FAILED_ACTIONS"

instance Hashable     StageRetryMode
instance NFData       StageRetryMode
instance ToByteString StageRetryMode
instance ToQuery      StageRetryMode
instance ToHeader     StageRetryMode

instance ToJSON StageRetryMode where
    toJSON = toJSONText

data StageTransitionType
  = Inbound
  | Outbound
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StageTransitionType where
    parser = takeLowerText >>= \case
        "inbound" -> pure Inbound
        "outbound" -> pure Outbound
        e -> fromTextError $ "Failure parsing StageTransitionType from value: '" <> e
           <> "'. Accepted values: inbound, outbound"

instance ToText StageTransitionType where
    toText = \case
        Inbound -> "Inbound"
        Outbound -> "Outbound"

instance Hashable     StageTransitionType
instance NFData       StageTransitionType
instance ToByteString StageTransitionType
instance ToQuery      StageTransitionType
instance ToHeader     StageTransitionType

instance ToJSON StageTransitionType where
    toJSON = toJSONText

data WebhookAuthenticationType
  = GithubHmac
  | IP
  | Unauthenticated
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText WebhookAuthenticationType where
    parser = takeLowerText >>= \case
        "github_hmac" -> pure GithubHmac
        "ip" -> pure IP
        "unauthenticated" -> pure Unauthenticated
        e -> fromTextError $ "Failure parsing WebhookAuthenticationType from value: '" <> e
           <> "'. Accepted values: github_hmac, ip, unauthenticated"

instance ToText WebhookAuthenticationType where
    toText = \case
        GithubHmac -> "GITHUB_HMAC"
        IP -> "IP"
        Unauthenticated -> "UNAUTHENTICATED"

instance Hashable     WebhookAuthenticationType
instance NFData       WebhookAuthenticationType
instance ToByteString WebhookAuthenticationType
instance ToQuery      WebhookAuthenticationType
instance ToHeader     WebhookAuthenticationType

instance ToJSON WebhookAuthenticationType where
    toJSON = toJSONText

instance FromJSON WebhookAuthenticationType where
    parseJSON = parseJSONText "WebhookAuthenticationType"
