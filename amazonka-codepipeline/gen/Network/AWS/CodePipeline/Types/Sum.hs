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
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.Sum where

import           Network.AWS.Prelude

data ActionCategory
    = Invoke
    | Build
    | Test
    | Source
    | Deploy
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ActionCategory where
    parser = takeLowerText >>= \case
        "build" -> pure Build
        "deploy" -> pure Deploy
        "invoke" -> pure Invoke
        "source" -> pure Source
        "test" -> pure Test
        e -> fromTextError $ "Failure parsing ActionCategory from value: '" <> e
           <> "'. Accepted values: build, deploy, invoke, source, test"

instance ToText ActionCategory where
    toText = \case
        Build -> "build"
        Deploy -> "deploy"
        Invoke -> "invoke"
        Source -> "source"
        Test -> "test"

instance Hashable ActionCategory
instance ToQuery ActionCategory
instance ToHeader ActionCategory

instance ToJSON ActionCategory where
    toJSON = toJSONText

instance FromJSON ActionCategory where
    parseJSON = parseJSONText "ActionCategory"

data ActionConfigurationPropertyType
    = String
    | Boolean
    | Number
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ActionConfigurationPropertyType where
    parser = takeLowerText >>= \case
        "boolean" -> pure Boolean
        "number" -> pure Number
        "string" -> pure String
        e -> fromTextError $ "Failure parsing ActionConfigurationPropertyType from value: '" <> e
           <> "'. Accepted values: boolean, number, string"

instance ToText ActionConfigurationPropertyType where
    toText = \case
        Boolean -> "boolean"
        Number -> "number"
        String -> "string"

instance Hashable ActionConfigurationPropertyType
instance ToQuery ActionConfigurationPropertyType
instance ToHeader ActionConfigurationPropertyType

instance ToJSON ActionConfigurationPropertyType where
    toJSON = toJSONText

instance FromJSON ActionConfigurationPropertyType where
    parseJSON = parseJSONText "ActionConfigurationPropertyType"

data ActionExecutionStatus
    = InProgress
    | Succeeded
    | Failed
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ActionExecutionStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "inprogress" -> pure InProgress
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing ActionExecutionStatus from value: '" <> e
           <> "'. Accepted values: failed, inprogress, succeeded"

instance ToText ActionExecutionStatus where
    toText = \case
        Failed -> "failed"
        InProgress -> "inprogress"
        Succeeded -> "succeeded"

instance Hashable ActionExecutionStatus
instance ToQuery ActionExecutionStatus
instance ToHeader ActionExecutionStatus

instance FromJSON ActionExecutionStatus where
    parseJSON = parseJSONText "ActionExecutionStatus"

data ActionOwner
    = AWS
    | ThirdParty
    | Custom
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ActionOwner where
    parser = takeLowerText >>= \case
        "aws" -> pure AWS
        "custom" -> pure Custom
        "thirdparty" -> pure ThirdParty
        e -> fromTextError $ "Failure parsing ActionOwner from value: '" <> e
           <> "'. Accepted values: aws, custom, thirdparty"

instance ToText ActionOwner where
    toText = \case
        AWS -> "aws"
        Custom -> "custom"
        ThirdParty -> "thirdparty"

instance Hashable ActionOwner
instance ToQuery ActionOwner
instance ToHeader ActionOwner

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
           <> "'. Accepted values: s3"

instance ToText ArtifactLocationType where
    toText = \case
        ALTS3 -> "s3"

instance Hashable ArtifactLocationType
instance ToQuery ArtifactLocationType
instance ToHeader ArtifactLocationType

instance FromJSON ArtifactLocationType where
    parseJSON = parseJSONText "ArtifactLocationType"

data ArtifactStoreType =
    S3
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ArtifactStoreType where
    parser = takeLowerText >>= \case
        "s3" -> pure S3
        e -> fromTextError $ "Failure parsing ArtifactStoreType from value: '" <> e
           <> "'. Accepted values: s3"

instance ToText ArtifactStoreType where
    toText = \case
        S3 -> "s3"

instance Hashable ArtifactStoreType
instance ToQuery ArtifactStoreType
instance ToHeader ArtifactStoreType

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
           <> "'. Accepted values: schedule"

instance ToText BlockerType where
    toText = \case
        Schedule -> "schedule"

instance Hashable BlockerType
instance ToQuery BlockerType
instance ToHeader BlockerType

instance ToJSON BlockerType where
    toJSON = toJSONText

instance FromJSON BlockerType where
    parseJSON = parseJSONText "BlockerType"

data FailureType
    = JobFailed
    | SystemUnavailable
    | PermissionError
    | ConfigurationError
    | RevisionOutOfSync
    | RevisionUnavailable
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
           <> "'. Accepted values: configurationerror, jobfailed, permissionerror, revisionoutofsync, revisionunavailable, systemunavailable"

instance ToText FailureType where
    toText = \case
        ConfigurationError -> "configurationerror"
        JobFailed -> "jobfailed"
        PermissionError -> "permissionerror"
        RevisionOutOfSync -> "revisionoutofsync"
        RevisionUnavailable -> "revisionunavailable"
        SystemUnavailable -> "systemunavailable"

instance Hashable FailureType
instance ToQuery FailureType
instance ToHeader FailureType

instance ToJSON FailureType where
    toJSON = toJSONText

data JobStatus
    = JSFailed
    | JSInProgress
    | JSCreated
    | JSQueued
    | JSSucceeded
    | JSDispatched
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
           <> "'. Accepted values: created, dispatched, failed, inprogress, queued, succeeded, timedout"

instance ToText JobStatus where
    toText = \case
        JSCreated -> "created"
        JSDispatched -> "dispatched"
        JSFailed -> "failed"
        JSInProgress -> "inprogress"
        JSQueued -> "queued"
        JSSucceeded -> "succeeded"
        JSTimedOut -> "timedout"

instance Hashable JobStatus
instance ToQuery JobStatus
instance ToHeader JobStatus

instance FromJSON JobStatus where
    parseJSON = parseJSONText "JobStatus"

data StageTransitionType
    = Outbound
    | Inbound
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StageTransitionType where
    parser = takeLowerText >>= \case
        "inbound" -> pure Inbound
        "outbound" -> pure Outbound
        e -> fromTextError $ "Failure parsing StageTransitionType from value: '" <> e
           <> "'. Accepted values: inbound, outbound"

instance ToText StageTransitionType where
    toText = \case
        Inbound -> "inbound"
        Outbound -> "outbound"

instance Hashable StageTransitionType
instance ToQuery StageTransitionType
instance ToHeader StageTransitionType

instance ToJSON StageTransitionType where
    toJSON = toJSONText
