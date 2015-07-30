{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.Sum where

import           Network.AWS.Prelude

data ApplicationRevisionSortBy
    = RegisterTime
    | FirstUsedTime
    | LastUsedTime
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ApplicationRevisionSortBy where
    parser = takeLowerText >>= \case
        "firstusedtime" -> pure FirstUsedTime
        "lastusedtime" -> pure LastUsedTime
        "registertime" -> pure RegisterTime
        e -> fromTextError $ "Failure parsing ApplicationRevisionSortBy from value: '" <> e
           <> "'. Accepted values: firstusedtime, lastusedtime, registertime"

instance ToText ApplicationRevisionSortBy where
    toText = \case
        FirstUsedTime -> "firstusedtime"
        LastUsedTime -> "lastusedtime"
        RegisterTime -> "registertime"

instance Hashable     ApplicationRevisionSortBy
instance ToByteString ApplicationRevisionSortBy
instance ToQuery      ApplicationRevisionSortBy
instance ToHeader     ApplicationRevisionSortBy

instance ToJSON ApplicationRevisionSortBy where
    toJSON = toJSONText

data BundleType
    = Zip
    | TGZ
    | TAR
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText BundleType where
    parser = takeLowerText >>= \case
        "tar" -> pure TAR
        "tgz" -> pure TGZ
        "zip" -> pure Zip
        e -> fromTextError $ "Failure parsing BundleType from value: '" <> e
           <> "'. Accepted values: tar, tgz, zip"

instance ToText BundleType where
    toText = \case
        TAR -> "tar"
        TGZ -> "tgz"
        Zip -> "zip"

instance Hashable     BundleType
instance ToByteString BundleType
instance ToQuery      BundleType
instance ToHeader     BundleType

instance ToJSON BundleType where
    toJSON = toJSONText

instance FromJSON BundleType where
    parseJSON = parseJSONText "BundleType"

data DeployErrorCode
    = Throttled
    | HealthConstraints
    | OverMaxInstances
    | HealthConstraintsInvalid
    | NoInstances
    | ApplicationMissing
    | RevisionMissing
    | InternalError
    | DeploymentGroupMissing
    | IAMRoleMissing
    | Timeout
    | NoEC2Subscription
    | IAMRolePermissions
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText DeployErrorCode where
    parser = takeLowerText >>= \case
        "application_missing" -> pure ApplicationMissing
        "deployment_group_missing" -> pure DeploymentGroupMissing
        "health_constraints" -> pure HealthConstraints
        "health_constraints_invalid" -> pure HealthConstraintsInvalid
        "iam_role_missing" -> pure IAMRoleMissing
        "iam_role_permissions" -> pure IAMRolePermissions
        "internal_error" -> pure InternalError
        "no_ec2_subscription" -> pure NoEC2Subscription
        "no_instances" -> pure NoInstances
        "over_max_instances" -> pure OverMaxInstances
        "revision_missing" -> pure RevisionMissing
        "throttled" -> pure Throttled
        "timeout" -> pure Timeout
        e -> fromTextError $ "Failure parsing DeployErrorCode from value: '" <> e
           <> "'. Accepted values: application_missing, deployment_group_missing, health_constraints, health_constraints_invalid, iam_role_missing, iam_role_permissions, internal_error, no_ec2_subscription, no_instances, over_max_instances, revision_missing, throttled, timeout"

instance ToText DeployErrorCode where
    toText = \case
        ApplicationMissing -> "application_missing"
        DeploymentGroupMissing -> "deployment_group_missing"
        HealthConstraints -> "health_constraints"
        HealthConstraintsInvalid -> "health_constraints_invalid"
        IAMRoleMissing -> "iam_role_missing"
        IAMRolePermissions -> "iam_role_permissions"
        InternalError -> "internal_error"
        NoEC2Subscription -> "no_ec2_subscription"
        NoInstances -> "no_instances"
        OverMaxInstances -> "over_max_instances"
        RevisionMissing -> "revision_missing"
        Throttled -> "throttled"
        Timeout -> "timeout"

instance Hashable     DeployErrorCode
instance ToByteString DeployErrorCode
instance ToQuery      DeployErrorCode
instance ToHeader     DeployErrorCode

instance FromJSON DeployErrorCode where
    parseJSON = parseJSONText "DeployErrorCode"

data DeploymentCreator
    = Autoscaling
    | User
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText DeploymentCreator where
    parser = takeLowerText >>= \case
        "autoscaling" -> pure Autoscaling
        "user" -> pure User
        e -> fromTextError $ "Failure parsing DeploymentCreator from value: '" <> e
           <> "'. Accepted values: autoscaling, user"

instance ToText DeploymentCreator where
    toText = \case
        Autoscaling -> "autoscaling"
        User -> "user"

instance Hashable     DeploymentCreator
instance ToByteString DeploymentCreator
instance ToQuery      DeploymentCreator
instance ToHeader     DeploymentCreator

instance FromJSON DeploymentCreator where
    parseJSON = parseJSONText "DeploymentCreator"

data DeploymentStatus
    = Queued
    | Created
    | Stopped
    | InProgress
    | Succeeded
    | Failed
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText DeploymentStatus where
    parser = takeLowerText >>= \case
        "created" -> pure Created
        "failed" -> pure Failed
        "inprogress" -> pure InProgress
        "queued" -> pure Queued
        "stopped" -> pure Stopped
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing DeploymentStatus from value: '" <> e
           <> "'. Accepted values: created, failed, inprogress, queued, stopped, succeeded"

instance ToText DeploymentStatus where
    toText = \case
        Created -> "created"
        Failed -> "failed"
        InProgress -> "inprogress"
        Queued -> "queued"
        Stopped -> "stopped"
        Succeeded -> "succeeded"

instance Hashable     DeploymentStatus
instance ToByteString DeploymentStatus
instance ToQuery      DeploymentStatus
instance ToHeader     DeploymentStatus

instance ToJSON DeploymentStatus where
    toJSON = toJSONText

instance FromJSON DeploymentStatus where
    parseJSON = parseJSONText "DeploymentStatus"

data EC2TagFilterType
    = KeyAndValue
    | ValueOnly
    | KeyOnly
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText EC2TagFilterType where
    parser = takeLowerText >>= \case
        "key_and_value" -> pure KeyAndValue
        "key_only" -> pure KeyOnly
        "value_only" -> pure ValueOnly
        e -> fromTextError $ "Failure parsing EC2TagFilterType from value: '" <> e
           <> "'. Accepted values: key_and_value, key_only, value_only"

instance ToText EC2TagFilterType where
    toText = \case
        KeyAndValue -> "key_and_value"
        KeyOnly -> "key_only"
        ValueOnly -> "value_only"

instance Hashable     EC2TagFilterType
instance ToByteString EC2TagFilterType
instance ToQuery      EC2TagFilterType
instance ToHeader     EC2TagFilterType

instance ToJSON EC2TagFilterType where
    toJSON = toJSONText

instance FromJSON EC2TagFilterType where
    parseJSON = parseJSONText "EC2TagFilterType"

data InstanceStatus
    = ISInProgress
    | ISFailed
    | ISSucceeded
    | ISUnknown
    | ISSkipped
    | ISPending
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText InstanceStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure ISFailed
        "inprogress" -> pure ISInProgress
        "pending" -> pure ISPending
        "skipped" -> pure ISSkipped
        "succeeded" -> pure ISSucceeded
        "unknown" -> pure ISUnknown
        e -> fromTextError $ "Failure parsing InstanceStatus from value: '" <> e
           <> "'. Accepted values: failed, inprogress, pending, skipped, succeeded, unknown"

instance ToText InstanceStatus where
    toText = \case
        ISFailed -> "failed"
        ISInProgress -> "inprogress"
        ISPending -> "pending"
        ISSkipped -> "skipped"
        ISSucceeded -> "succeeded"
        ISUnknown -> "unknown"

instance Hashable     InstanceStatus
instance ToByteString InstanceStatus
instance ToQuery      InstanceStatus
instance ToHeader     InstanceStatus

instance ToJSON InstanceStatus where
    toJSON = toJSONText

instance FromJSON InstanceStatus where
    parseJSON = parseJSONText "InstanceStatus"

data LifecycleErrorCode
    = UnknownError
    | ScriptMissing
    | Success
    | ScriptFailed
    | ScriptNotExecutable
    | ScriptTimedOut
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText LifecycleErrorCode where
    parser = takeLowerText >>= \case
        "scriptfailed" -> pure ScriptFailed
        "scriptmissing" -> pure ScriptMissing
        "scriptnotexecutable" -> pure ScriptNotExecutable
        "scripttimedout" -> pure ScriptTimedOut
        "success" -> pure Success
        "unknownerror" -> pure UnknownError
        e -> fromTextError $ "Failure parsing LifecycleErrorCode from value: '" <> e
           <> "'. Accepted values: scriptfailed, scriptmissing, scriptnotexecutable, scripttimedout, success, unknownerror"

instance ToText LifecycleErrorCode where
    toText = \case
        ScriptFailed -> "scriptfailed"
        ScriptMissing -> "scriptmissing"
        ScriptNotExecutable -> "scriptnotexecutable"
        ScriptTimedOut -> "scripttimedout"
        Success -> "success"
        UnknownError -> "unknownerror"

instance Hashable     LifecycleErrorCode
instance ToByteString LifecycleErrorCode
instance ToQuery      LifecycleErrorCode
instance ToHeader     LifecycleErrorCode

instance FromJSON LifecycleErrorCode where
    parseJSON = parseJSONText "LifecycleErrorCode"

data LifecycleEventStatus
    = LESInProgress
    | LESFailed
    | LESSucceeded
    | LESSkipped
    | LESUnknown
    | LESPending
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText LifecycleEventStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure LESFailed
        "inprogress" -> pure LESInProgress
        "pending" -> pure LESPending
        "skipped" -> pure LESSkipped
        "succeeded" -> pure LESSucceeded
        "unknown" -> pure LESUnknown
        e -> fromTextError $ "Failure parsing LifecycleEventStatus from value: '" <> e
           <> "'. Accepted values: failed, inprogress, pending, skipped, succeeded, unknown"

instance ToText LifecycleEventStatus where
    toText = \case
        LESFailed -> "failed"
        LESInProgress -> "inprogress"
        LESPending -> "pending"
        LESSkipped -> "skipped"
        LESSucceeded -> "succeeded"
        LESUnknown -> "unknown"

instance Hashable     LifecycleEventStatus
instance ToByteString LifecycleEventStatus
instance ToQuery      LifecycleEventStatus
instance ToHeader     LifecycleEventStatus

instance FromJSON LifecycleEventStatus where
    parseJSON = parseJSONText "LifecycleEventStatus"

data ListStateFilterAction
    = Include
    | Ignore
    | Exclude
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ListStateFilterAction where
    parser = takeLowerText >>= \case
        "exclude" -> pure Exclude
        "ignore" -> pure Ignore
        "include" -> pure Include
        e -> fromTextError $ "Failure parsing ListStateFilterAction from value: '" <> e
           <> "'. Accepted values: exclude, ignore, include"

instance ToText ListStateFilterAction where
    toText = \case
        Exclude -> "exclude"
        Ignore -> "ignore"
        Include -> "include"

instance Hashable     ListStateFilterAction
instance ToByteString ListStateFilterAction
instance ToQuery      ListStateFilterAction
instance ToHeader     ListStateFilterAction

instance ToJSON ListStateFilterAction where
    toJSON = toJSONText

data MinimumHealthyHostsType
    = FleetPercent
    | HostCount
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText MinimumHealthyHostsType where
    parser = takeLowerText >>= \case
        "fleet_percent" -> pure FleetPercent
        "host_count" -> pure HostCount
        e -> fromTextError $ "Failure parsing MinimumHealthyHostsType from value: '" <> e
           <> "'. Accepted values: fleet_percent, host_count"

instance ToText MinimumHealthyHostsType where
    toText = \case
        FleetPercent -> "fleet_percent"
        HostCount -> "host_count"

instance Hashable     MinimumHealthyHostsType
instance ToByteString MinimumHealthyHostsType
instance ToQuery      MinimumHealthyHostsType
instance ToHeader     MinimumHealthyHostsType

instance ToJSON MinimumHealthyHostsType where
    toJSON = toJSONText

instance FromJSON MinimumHealthyHostsType where
    parseJSON = parseJSONText "MinimumHealthyHostsType"

data RegistrationStatus
    = Registered
    | Deregistered
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText RegistrationStatus where
    parser = takeLowerText >>= \case
        "deregistered" -> pure Deregistered
        "registered" -> pure Registered
        e -> fromTextError $ "Failure parsing RegistrationStatus from value: '" <> e
           <> "'. Accepted values: deregistered, registered"

instance ToText RegistrationStatus where
    toText = \case
        Deregistered -> "deregistered"
        Registered -> "registered"

instance Hashable     RegistrationStatus
instance ToByteString RegistrationStatus
instance ToQuery      RegistrationStatus
instance ToHeader     RegistrationStatus

instance ToJSON RegistrationStatus where
    toJSON = toJSONText

data RevisionLocationType
    = GitHub
    | S3
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText RevisionLocationType where
    parser = takeLowerText >>= \case
        "github" -> pure GitHub
        "s3" -> pure S3
        e -> fromTextError $ "Failure parsing RevisionLocationType from value: '" <> e
           <> "'. Accepted values: github, s3"

instance ToText RevisionLocationType where
    toText = \case
        GitHub -> "github"
        S3 -> "s3"

instance Hashable     RevisionLocationType
instance ToByteString RevisionLocationType
instance ToQuery      RevisionLocationType
instance ToHeader     RevisionLocationType

instance ToJSON RevisionLocationType where
    toJSON = toJSONText

instance FromJSON RevisionLocationType where
    parseJSON = parseJSONText "RevisionLocationType"

data SortOrder
    = Ascending
    | Descending
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText SortOrder where
    parser = takeLowerText >>= \case
        "ascending" -> pure Ascending
        "descending" -> pure Descending
        e -> fromTextError $ "Failure parsing SortOrder from value: '" <> e
           <> "'. Accepted values: ascending, descending"

instance ToText SortOrder where
    toText = \case
        Ascending -> "ascending"
        Descending -> "descending"

instance Hashable     SortOrder
instance ToByteString SortOrder
instance ToQuery      SortOrder
instance ToHeader     SortOrder

instance ToJSON SortOrder where
    toJSON = toJSONText

data StopStatus
    = SSSucceeded
    | SSPending
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StopStatus where
    parser = takeLowerText >>= \case
        "pending" -> pure SSPending
        "succeeded" -> pure SSSucceeded
        e -> fromTextError $ "Failure parsing StopStatus from value: '" <> e
           <> "'. Accepted values: pending, succeeded"

instance ToText StopStatus where
    toText = \case
        SSPending -> "pending"
        SSSucceeded -> "succeeded"

instance Hashable     StopStatus
instance ToByteString StopStatus
instance ToQuery      StopStatus
instance ToHeader     StopStatus

instance FromJSON StopStatus where
    parseJSON = parseJSONText "StopStatus"

data TagFilterType
    = TFTKeyAndValue
    | TFTValueOnly
    | TFTKeyOnly
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText TagFilterType where
    parser = takeLowerText >>= \case
        "key_and_value" -> pure TFTKeyAndValue
        "key_only" -> pure TFTKeyOnly
        "value_only" -> pure TFTValueOnly
        e -> fromTextError $ "Failure parsing TagFilterType from value: '" <> e
           <> "'. Accepted values: key_and_value, key_only, value_only"

instance ToText TagFilterType where
    toText = \case
        TFTKeyAndValue -> "key_and_value"
        TFTKeyOnly -> "key_only"
        TFTValueOnly -> "value_only"

instance Hashable     TagFilterType
instance ToByteString TagFilterType
instance ToQuery      TagFilterType
instance ToHeader     TagFilterType

instance ToJSON TagFilterType where
    toJSON = toJSONText

instance FromJSON TagFilterType where
    parseJSON = parseJSONText "TagFilterType"
