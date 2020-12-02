{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.Sum where

import Network.AWS.Prelude

data ActionType
  = Connect
  | Publish
  | Receive
  | Subscribe
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ActionType where
    parser = takeLowerText >>= \case
        "connect" -> pure Connect
        "publish" -> pure Publish
        "receive" -> pure Receive
        "subscribe" -> pure Subscribe
        e -> fromTextError $ "Failure parsing ActionType from value: '" <> e
           <> "'. Accepted values: connect, publish, receive, subscribe"

instance ToText ActionType where
    toText = \case
        Connect -> "CONNECT"
        Publish -> "PUBLISH"
        Receive -> "RECEIVE"
        Subscribe -> "SUBSCRIBE"

instance Hashable     ActionType
instance NFData       ActionType
instance ToByteString ActionType
instance ToQuery      ActionType
instance ToHeader     ActionType

instance ToJSON ActionType where
    toJSON = toJSONText

instance FromJSON ActionType where
    parseJSON = parseJSONText "ActionType"

data AuthDecision
  = Allowed
  | ExplicitDeny
  | ImplicitDeny
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AuthDecision where
    parser = takeLowerText >>= \case
        "allowed" -> pure Allowed
        "explicit_deny" -> pure ExplicitDeny
        "implicit_deny" -> pure ImplicitDeny
        e -> fromTextError $ "Failure parsing AuthDecision from value: '" <> e
           <> "'. Accepted values: allowed, explicit_deny, implicit_deny"

instance ToText AuthDecision where
    toText = \case
        Allowed -> "ALLOWED"
        ExplicitDeny -> "EXPLICIT_DENY"
        ImplicitDeny -> "IMPLICIT_DENY"

instance Hashable     AuthDecision
instance NFData       AuthDecision
instance ToByteString AuthDecision
instance ToQuery      AuthDecision
instance ToHeader     AuthDecision

instance FromJSON AuthDecision where
    parseJSON = parseJSONText "AuthDecision"

data AuthorizerStatus
  = Active
  | Inactive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AuthorizerStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "inactive" -> pure Inactive
        e -> fromTextError $ "Failure parsing AuthorizerStatus from value: '" <> e
           <> "'. Accepted values: active, inactive"

instance ToText AuthorizerStatus where
    toText = \case
        Active -> "ACTIVE"
        Inactive -> "INACTIVE"

instance Hashable     AuthorizerStatus
instance NFData       AuthorizerStatus
instance ToByteString AuthorizerStatus
instance ToQuery      AuthorizerStatus
instance ToHeader     AuthorizerStatus

instance ToJSON AuthorizerStatus where
    toJSON = toJSONText

instance FromJSON AuthorizerStatus where
    parseJSON = parseJSONText "AuthorizerStatus"

data AutoRegistrationStatus
  = Disable
  | Enable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AutoRegistrationStatus where
    parser = takeLowerText >>= \case
        "disable" -> pure Disable
        "enable" -> pure Enable
        e -> fromTextError $ "Failure parsing AutoRegistrationStatus from value: '" <> e
           <> "'. Accepted values: disable, enable"

instance ToText AutoRegistrationStatus where
    toText = \case
        Disable -> "DISABLE"
        Enable -> "ENABLE"

instance Hashable     AutoRegistrationStatus
instance NFData       AutoRegistrationStatus
instance ToByteString AutoRegistrationStatus
instance ToQuery      AutoRegistrationStatus
instance ToHeader     AutoRegistrationStatus

instance ToJSON AutoRegistrationStatus where
    toJSON = toJSONText

instance FromJSON AutoRegistrationStatus where
    parseJSON = parseJSONText "AutoRegistrationStatus"

data CACertificateStatus
  = CACSActive
  | CACSInactive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CACertificateStatus where
    parser = takeLowerText >>= \case
        "active" -> pure CACSActive
        "inactive" -> pure CACSInactive
        e -> fromTextError $ "Failure parsing CACertificateStatus from value: '" <> e
           <> "'. Accepted values: active, inactive"

instance ToText CACertificateStatus where
    toText = \case
        CACSActive -> "ACTIVE"
        CACSInactive -> "INACTIVE"

instance Hashable     CACertificateStatus
instance NFData       CACertificateStatus
instance ToByteString CACertificateStatus
instance ToQuery      CACertificateStatus
instance ToHeader     CACertificateStatus

instance ToJSON CACertificateStatus where
    toJSON = toJSONText

instance FromJSON CACertificateStatus where
    parseJSON = parseJSONText "CACertificateStatus"

data CannedAccessControlList
  = AWSExecRead
  | AuthenticatedRead
  | BucketOwnerFullControl
  | BucketOwnerRead
  | LogDeliveryWrite
  | Private
  | PublicRead
  | PublicReadWrite
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CannedAccessControlList where
    parser = takeLowerText >>= \case
        "aws-exec-read" -> pure AWSExecRead
        "authenticated-read" -> pure AuthenticatedRead
        "bucket-owner-full-control" -> pure BucketOwnerFullControl
        "bucket-owner-read" -> pure BucketOwnerRead
        "log-delivery-write" -> pure LogDeliveryWrite
        "private" -> pure Private
        "public-read" -> pure PublicRead
        "public-read-write" -> pure PublicReadWrite
        e -> fromTextError $ "Failure parsing CannedAccessControlList from value: '" <> e
           <> "'. Accepted values: aws-exec-read, authenticated-read, bucket-owner-full-control, bucket-owner-read, log-delivery-write, private, public-read, public-read-write"

instance ToText CannedAccessControlList where
    toText = \case
        AWSExecRead -> "aws-exec-read"
        AuthenticatedRead -> "authenticated-read"
        BucketOwnerFullControl -> "bucket-owner-full-control"
        BucketOwnerRead -> "bucket-owner-read"
        LogDeliveryWrite -> "log-delivery-write"
        Private -> "private"
        PublicRead -> "public-read"
        PublicReadWrite -> "public-read-write"

instance Hashable     CannedAccessControlList
instance NFData       CannedAccessControlList
instance ToByteString CannedAccessControlList
instance ToQuery      CannedAccessControlList
instance ToHeader     CannedAccessControlList

instance ToJSON CannedAccessControlList where
    toJSON = toJSONText

instance FromJSON CannedAccessControlList where
    parseJSON = parseJSONText "CannedAccessControlList"

data CertificateStatus
  = CSActive
  | CSInactive
  | CSPendingActivation
  | CSPendingTransfer
  | CSRegisterInactive
  | CSRevoked
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CertificateStatus where
    parser = takeLowerText >>= \case
        "active" -> pure CSActive
        "inactive" -> pure CSInactive
        "pending_activation" -> pure CSPendingActivation
        "pending_transfer" -> pure CSPendingTransfer
        "register_inactive" -> pure CSRegisterInactive
        "revoked" -> pure CSRevoked
        e -> fromTextError $ "Failure parsing CertificateStatus from value: '" <> e
           <> "'. Accepted values: active, inactive, pending_activation, pending_transfer, register_inactive, revoked"

instance ToText CertificateStatus where
    toText = \case
        CSActive -> "ACTIVE"
        CSInactive -> "INACTIVE"
        CSPendingActivation -> "PENDING_ACTIVATION"
        CSPendingTransfer -> "PENDING_TRANSFER"
        CSRegisterInactive -> "REGISTER_INACTIVE"
        CSRevoked -> "REVOKED"

instance Hashable     CertificateStatus
instance NFData       CertificateStatus
instance ToByteString CertificateStatus
instance ToQuery      CertificateStatus
instance ToHeader     CertificateStatus

instance ToJSON CertificateStatus where
    toJSON = toJSONText

instance FromJSON CertificateStatus where
    parseJSON = parseJSONText "CertificateStatus"

data DynamoKeyType
  = Number
  | String
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DynamoKeyType where
    parser = takeLowerText >>= \case
        "number" -> pure Number
        "string" -> pure String
        e -> fromTextError $ "Failure parsing DynamoKeyType from value: '" <> e
           <> "'. Accepted values: number, string"

instance ToText DynamoKeyType where
    toText = \case
        Number -> "NUMBER"
        String -> "STRING"

instance Hashable     DynamoKeyType
instance NFData       DynamoKeyType
instance ToByteString DynamoKeyType
instance ToQuery      DynamoKeyType
instance ToHeader     DynamoKeyType

instance ToJSON DynamoKeyType where
    toJSON = toJSONText

instance FromJSON DynamoKeyType where
    parseJSON = parseJSONText "DynamoKeyType"

data EventType
  = Job
  | JobExecution
  | Thing
  | ThingGroup
  | ThingGroupHierarchy
  | ThingGroupMembership
  | ThingType
  | ThingTypeAssociation
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EventType where
    parser = takeLowerText >>= \case
        "job" -> pure Job
        "job_execution" -> pure JobExecution
        "thing" -> pure Thing
        "thing_group" -> pure ThingGroup
        "thing_group_hierarchy" -> pure ThingGroupHierarchy
        "thing_group_membership" -> pure ThingGroupMembership
        "thing_type" -> pure ThingType
        "thing_type_association" -> pure ThingTypeAssociation
        e -> fromTextError $ "Failure parsing EventType from value: '" <> e
           <> "'. Accepted values: job, job_execution, thing, thing_group, thing_group_hierarchy, thing_group_membership, thing_type, thing_type_association"

instance ToText EventType where
    toText = \case
        Job -> "JOB"
        JobExecution -> "JOB_EXECUTION"
        Thing -> "THING"
        ThingGroup -> "THING_GROUP"
        ThingGroupHierarchy -> "THING_GROUP_HIERARCHY"
        ThingGroupMembership -> "THING_GROUP_MEMBERSHIP"
        ThingType -> "THING_TYPE"
        ThingTypeAssociation -> "THING_TYPE_ASSOCIATION"

instance Hashable     EventType
instance NFData       EventType
instance ToByteString EventType
instance ToQuery      EventType
instance ToHeader     EventType

instance ToJSON EventType where
    toJSON = toJSONText

instance FromJSON EventType where
    parseJSON = parseJSONText "EventType"

data IndexStatus
  = ISActive
  | ISBuilding
  | ISRebuilding
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText IndexStatus where
    parser = takeLowerText >>= \case
        "active" -> pure ISActive
        "building" -> pure ISBuilding
        "rebuilding" -> pure ISRebuilding
        e -> fromTextError $ "Failure parsing IndexStatus from value: '" <> e
           <> "'. Accepted values: active, building, rebuilding"

instance ToText IndexStatus where
    toText = \case
        ISActive -> "ACTIVE"
        ISBuilding -> "BUILDING"
        ISRebuilding -> "REBUILDING"

instance Hashable     IndexStatus
instance NFData       IndexStatus
instance ToByteString IndexStatus
instance ToQuery      IndexStatus
instance ToHeader     IndexStatus

instance FromJSON IndexStatus where
    parseJSON = parseJSONText "IndexStatus"

data JobExecutionStatus
  = Canceled
  | Failed
  | InProgress
  | Queued
  | Rejected
  | Removed
  | Succeeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText JobExecutionStatus where
    parser = takeLowerText >>= \case
        "canceled" -> pure Canceled
        "failed" -> pure Failed
        "in_progress" -> pure InProgress
        "queued" -> pure Queued
        "rejected" -> pure Rejected
        "removed" -> pure Removed
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing JobExecutionStatus from value: '" <> e
           <> "'. Accepted values: canceled, failed, in_progress, queued, rejected, removed, succeeded"

instance ToText JobExecutionStatus where
    toText = \case
        Canceled -> "CANCELED"
        Failed -> "FAILED"
        InProgress -> "IN_PROGRESS"
        Queued -> "QUEUED"
        Rejected -> "REJECTED"
        Removed -> "REMOVED"
        Succeeded -> "SUCCEEDED"

instance Hashable     JobExecutionStatus
instance NFData       JobExecutionStatus
instance ToByteString JobExecutionStatus
instance ToQuery      JobExecutionStatus
instance ToHeader     JobExecutionStatus

instance ToJSON JobExecutionStatus where
    toJSON = toJSONText

instance FromJSON JobExecutionStatus where
    parseJSON = parseJSONText "JobExecutionStatus"

data JobStatus
  = JSCanceled
  | JSCompleted
  | JSInProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText JobStatus where
    parser = takeLowerText >>= \case
        "canceled" -> pure JSCanceled
        "completed" -> pure JSCompleted
        "in_progress" -> pure JSInProgress
        e -> fromTextError $ "Failure parsing JobStatus from value: '" <> e
           <> "'. Accepted values: canceled, completed, in_progress"

instance ToText JobStatus where
    toText = \case
        JSCanceled -> "CANCELED"
        JSCompleted -> "COMPLETED"
        JSInProgress -> "IN_PROGRESS"

instance Hashable     JobStatus
instance NFData       JobStatus
instance ToByteString JobStatus
instance ToQuery      JobStatus
instance ToHeader     JobStatus

instance ToJSON JobStatus where
    toJSON = toJSONText

instance FromJSON JobStatus where
    parseJSON = parseJSONText "JobStatus"

data LogLevel
  = Debug
  | Disabled
  | Error'
  | Info
  | Warn
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LogLevel where
    parser = takeLowerText >>= \case
        "debug" -> pure Debug
        "disabled" -> pure Disabled
        "error" -> pure Error'
        "info" -> pure Info
        "warn" -> pure Warn
        e -> fromTextError $ "Failure parsing LogLevel from value: '" <> e
           <> "'. Accepted values: debug, disabled, error, info, warn"

instance ToText LogLevel where
    toText = \case
        Debug -> "DEBUG"
        Disabled -> "DISABLED"
        Error' -> "ERROR"
        Info -> "INFO"
        Warn -> "WARN"

instance Hashable     LogLevel
instance NFData       LogLevel
instance ToByteString LogLevel
instance ToQuery      LogLevel
instance ToHeader     LogLevel

instance ToJSON LogLevel where
    toJSON = toJSONText

instance FromJSON LogLevel where
    parseJSON = parseJSONText "LogLevel"

data LogTargetType
  = LTTDefault
  | LTTThingGroup
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LogTargetType where
    parser = takeLowerText >>= \case
        "default" -> pure LTTDefault
        "thing_group" -> pure LTTThingGroup
        e -> fromTextError $ "Failure parsing LogTargetType from value: '" <> e
           <> "'. Accepted values: default, thing_group"

instance ToText LogTargetType where
    toText = \case
        LTTDefault -> "DEFAULT"
        LTTThingGroup -> "THING_GROUP"

instance Hashable     LogTargetType
instance NFData       LogTargetType
instance ToByteString LogTargetType
instance ToQuery      LogTargetType
instance ToHeader     LogTargetType

instance ToJSON LogTargetType where
    toJSON = toJSONText

instance FromJSON LogTargetType where
    parseJSON = parseJSONText "LogTargetType"

data MessageFormat
  = JSON
  | Raw
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MessageFormat where
    parser = takeLowerText >>= \case
        "json" -> pure JSON
        "raw" -> pure Raw
        e -> fromTextError $ "Failure parsing MessageFormat from value: '" <> e
           <> "'. Accepted values: json, raw"

instance ToText MessageFormat where
    toText = \case
        JSON -> "JSON"
        Raw -> "RAW"

instance Hashable     MessageFormat
instance NFData       MessageFormat
instance ToByteString MessageFormat
instance ToQuery      MessageFormat
instance ToHeader     MessageFormat

instance ToJSON MessageFormat where
    toJSON = toJSONText

instance FromJSON MessageFormat where
    parseJSON = parseJSONText "MessageFormat"

data OTAUpdateStatus
  = CreateComplete
  | CreateFailed
  | CreateInProgress
  | CreatePending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OTAUpdateStatus where
    parser = takeLowerText >>= \case
        "create_complete" -> pure CreateComplete
        "create_failed" -> pure CreateFailed
        "create_in_progress" -> pure CreateInProgress
        "create_pending" -> pure CreatePending
        e -> fromTextError $ "Failure parsing OTAUpdateStatus from value: '" <> e
           <> "'. Accepted values: create_complete, create_failed, create_in_progress, create_pending"

instance ToText OTAUpdateStatus where
    toText = \case
        CreateComplete -> "CREATE_COMPLETE"
        CreateFailed -> "CREATE_FAILED"
        CreateInProgress -> "CREATE_IN_PROGRESS"
        CreatePending -> "CREATE_PENDING"

instance Hashable     OTAUpdateStatus
instance NFData       OTAUpdateStatus
instance ToByteString OTAUpdateStatus
instance ToQuery      OTAUpdateStatus
instance ToHeader     OTAUpdateStatus

instance ToJSON OTAUpdateStatus where
    toJSON = toJSONText

instance FromJSON OTAUpdateStatus where
    parseJSON = parseJSONText "OTAUpdateStatus"

data ReportType
  = Errors
  | Results
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReportType where
    parser = takeLowerText >>= \case
        "errors" -> pure Errors
        "results" -> pure Results
        e -> fromTextError $ "Failure parsing ReportType from value: '" <> e
           <> "'. Accepted values: errors, results"

instance ToText ReportType where
    toText = \case
        Errors -> "ERRORS"
        Results -> "RESULTS"

instance Hashable     ReportType
instance NFData       ReportType
instance ToByteString ReportType
instance ToQuery      ReportType
instance ToHeader     ReportType

instance ToJSON ReportType where
    toJSON = toJSONText

instance FromJSON ReportType where
    parseJSON = parseJSONText "ReportType"

data TargetSelection
  = Continuous
  | Snapshot
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TargetSelection where
    parser = takeLowerText >>= \case
        "continuous" -> pure Continuous
        "snapshot" -> pure Snapshot
        e -> fromTextError $ "Failure parsing TargetSelection from value: '" <> e
           <> "'. Accepted values: continuous, snapshot"

instance ToText TargetSelection where
    toText = \case
        Continuous -> "CONTINUOUS"
        Snapshot -> "SNAPSHOT"

instance Hashable     TargetSelection
instance NFData       TargetSelection
instance ToByteString TargetSelection
instance ToQuery      TargetSelection
instance ToHeader     TargetSelection

instance ToJSON TargetSelection where
    toJSON = toJSONText

instance FromJSON TargetSelection where
    parseJSON = parseJSONText "TargetSelection"

data TaskStatus
  = TSCancelled
  | TSCancelling
  | TSCompleted
  | TSFailed
  | TSInProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TaskStatus where
    parser = takeLowerText >>= \case
        "cancelled" -> pure TSCancelled
        "cancelling" -> pure TSCancelling
        "completed" -> pure TSCompleted
        "failed" -> pure TSFailed
        "inprogress" -> pure TSInProgress
        e -> fromTextError $ "Failure parsing TaskStatus from value: '" <> e
           <> "'. Accepted values: cancelled, cancelling, completed, failed, inprogress"

instance ToText TaskStatus where
    toText = \case
        TSCancelled -> "Cancelled"
        TSCancelling -> "Cancelling"
        TSCompleted -> "Completed"
        TSFailed -> "Failed"
        TSInProgress -> "InProgress"

instance Hashable     TaskStatus
instance NFData       TaskStatus
instance ToByteString TaskStatus
instance ToQuery      TaskStatus
instance ToHeader     TaskStatus

instance ToJSON TaskStatus where
    toJSON = toJSONText

instance FromJSON TaskStatus where
    parseJSON = parseJSONText "TaskStatus"

data ThingIndexingMode
  = Off
  | Registry
  | RegistryAndShadow
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ThingIndexingMode where
    parser = takeLowerText >>= \case
        "off" -> pure Off
        "registry" -> pure Registry
        "registry_and_shadow" -> pure RegistryAndShadow
        e -> fromTextError $ "Failure parsing ThingIndexingMode from value: '" <> e
           <> "'. Accepted values: off, registry, registry_and_shadow"

instance ToText ThingIndexingMode where
    toText = \case
        Off -> "OFF"
        Registry -> "REGISTRY"
        RegistryAndShadow -> "REGISTRY_AND_SHADOW"

instance Hashable     ThingIndexingMode
instance NFData       ThingIndexingMode
instance ToByteString ThingIndexingMode
instance ToQuery      ThingIndexingMode
instance ToHeader     ThingIndexingMode

instance ToJSON ThingIndexingMode where
    toJSON = toJSONText

instance FromJSON ThingIndexingMode where
    parseJSON = parseJSONText "ThingIndexingMode"
