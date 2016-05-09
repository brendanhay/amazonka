{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.Sum where

import           Network.AWS.Prelude

data ApplicationRevisionSortBy
    = FirstUsedTime
    | LastUsedTime
    | RegisterTime
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ApplicationRevisionSortBy where
    parser = takeLowerText >>= \case
        "firstusedtime" -> pure FirstUsedTime
        "lastusedtime" -> pure LastUsedTime
        "registertime" -> pure RegisterTime
        e -> fromTextError $ "Failure parsing ApplicationRevisionSortBy from value: '" <> e
           <> "'. Accepted values: firstUsedTime, lastUsedTime, registerTime"

instance ToText ApplicationRevisionSortBy where
    toText = \case
        FirstUsedTime -> "firstUsedTime"
        LastUsedTime -> "lastUsedTime"
        RegisterTime -> "registerTime"

instance Hashable     ApplicationRevisionSortBy
instance NFData       ApplicationRevisionSortBy
instance ToByteString ApplicationRevisionSortBy
instance ToQuery      ApplicationRevisionSortBy
instance ToHeader     ApplicationRevisionSortBy

instance ToJSON ApplicationRevisionSortBy where
    toJSON = toJSONText

data BundleType
    = TAR
    | TGZ
    | Zip
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
instance NFData       BundleType
instance ToByteString BundleType
instance ToQuery      BundleType
instance ToHeader     BundleType

instance ToJSON BundleType where
    toJSON = toJSONText

instance FromJSON BundleType where
    parseJSON = parseJSONText "BundleType"

data DeployErrorCode
    = ApplicationMissing
    | DeploymentGroupMissing
    | HealthConstraints
    | HealthConstraintsInvalid
    | IAMRoleMissing
    | IAMRolePermissions
    | InternalError
    | NoEC2Subscription
    | NoInstances
    | OverMaxInstances
    | RevisionMissing
    | Throttled
    | Timeout
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
           <> "'. Accepted values: APPLICATION_MISSING, DEPLOYMENT_GROUP_MISSING, HEALTH_CONSTRAINTS, HEALTH_CONSTRAINTS_INVALID, IAM_ROLE_MISSING, IAM_ROLE_PERMISSIONS, INTERNAL_ERROR, NO_EC2_SUBSCRIPTION, NO_INSTANCES, OVER_MAX_INSTANCES, REVISION_MISSING, THROTTLED, TIMEOUT"

instance ToText DeployErrorCode where
    toText = \case
        ApplicationMissing -> "APPLICATION_MISSING"
        DeploymentGroupMissing -> "DEPLOYMENT_GROUP_MISSING"
        HealthConstraints -> "HEALTH_CONSTRAINTS"
        HealthConstraintsInvalid -> "HEALTH_CONSTRAINTS_INVALID"
        IAMRoleMissing -> "IAM_ROLE_MISSING"
        IAMRolePermissions -> "IAM_ROLE_PERMISSIONS"
        InternalError -> "INTERNAL_ERROR"
        NoEC2Subscription -> "NO_EC2_SUBSCRIPTION"
        NoInstances -> "NO_INSTANCES"
        OverMaxInstances -> "OVER_MAX_INSTANCES"
        RevisionMissing -> "REVISION_MISSING"
        Throttled -> "THROTTLED"
        Timeout -> "TIMEOUT"

instance Hashable     DeployErrorCode
instance NFData       DeployErrorCode
instance ToByteString DeployErrorCode
instance ToQuery      DeployErrorCode
instance ToHeader     DeployErrorCode

instance FromJSON DeployErrorCode where
    parseJSON = parseJSONText "DeployErrorCode"

data DeploymentCreator
    = Autoscaling
    | User
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
instance NFData       DeploymentCreator
instance ToByteString DeploymentCreator
instance ToQuery      DeploymentCreator
instance ToHeader     DeploymentCreator

instance FromJSON DeploymentCreator where
    parseJSON = parseJSONText "DeploymentCreator"

data DeploymentStatus
    = Created
    | Failed
    | InProgress
    | Queued
    | Stopped
    | Succeeded
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DeploymentStatus where
    parser = takeLowerText >>= \case
        "created" -> pure Created
        "failed" -> pure Failed
        "inprogress" -> pure InProgress
        "queued" -> pure Queued
        "stopped" -> pure Stopped
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing DeploymentStatus from value: '" <> e
           <> "'. Accepted values: Created, Failed, InProgress, Queued, Stopped, Succeeded"

instance ToText DeploymentStatus where
    toText = \case
        Created -> "Created"
        Failed -> "Failed"
        InProgress -> "InProgress"
        Queued -> "Queued"
        Stopped -> "Stopped"
        Succeeded -> "Succeeded"

instance Hashable     DeploymentStatus
instance NFData       DeploymentStatus
instance ToByteString DeploymentStatus
instance ToQuery      DeploymentStatus
instance ToHeader     DeploymentStatus

instance ToJSON DeploymentStatus where
    toJSON = toJSONText

instance FromJSON DeploymentStatus where
    parseJSON = parseJSONText "DeploymentStatus"

data EC2TagFilterType
    = KeyAndValue
    | KeyOnly
    | ValueOnly
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText EC2TagFilterType where
    parser = takeLowerText >>= \case
        "key_and_value" -> pure KeyAndValue
        "key_only" -> pure KeyOnly
        "value_only" -> pure ValueOnly
        e -> fromTextError $ "Failure parsing EC2TagFilterType from value: '" <> e
           <> "'. Accepted values: KEY_AND_VALUE, KEY_ONLY, VALUE_ONLY"

instance ToText EC2TagFilterType where
    toText = \case
        KeyAndValue -> "KEY_AND_VALUE"
        KeyOnly -> "KEY_ONLY"
        ValueOnly -> "VALUE_ONLY"

instance Hashable     EC2TagFilterType
instance NFData       EC2TagFilterType
instance ToByteString EC2TagFilterType
instance ToQuery      EC2TagFilterType
instance ToHeader     EC2TagFilterType

instance ToJSON EC2TagFilterType where
    toJSON = toJSONText

instance FromJSON EC2TagFilterType where
    parseJSON = parseJSONText "EC2TagFilterType"

data InstanceStatus
    = ISFailed
    | ISInProgress
    | ISPending
    | ISSkipped
    | ISSucceeded
    | ISUnknown
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText InstanceStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure ISFailed
        "inprogress" -> pure ISInProgress
        "pending" -> pure ISPending
        "skipped" -> pure ISSkipped
        "succeeded" -> pure ISSucceeded
        "unknown" -> pure ISUnknown
        e -> fromTextError $ "Failure parsing InstanceStatus from value: '" <> e
           <> "'. Accepted values: Failed, InProgress, Pending, Skipped, Succeeded, Unknown"

instance ToText InstanceStatus where
    toText = \case
        ISFailed -> "Failed"
        ISInProgress -> "InProgress"
        ISPending -> "Pending"
        ISSkipped -> "Skipped"
        ISSucceeded -> "Succeeded"
        ISUnknown -> "Unknown"

instance Hashable     InstanceStatus
instance NFData       InstanceStatus
instance ToByteString InstanceStatus
instance ToQuery      InstanceStatus
instance ToHeader     InstanceStatus

instance ToJSON InstanceStatus where
    toJSON = toJSONText

instance FromJSON InstanceStatus where
    parseJSON = parseJSONText "InstanceStatus"

data LifecycleErrorCode
    = ScriptFailed
    | ScriptMissing
    | ScriptNotExecutable
    | ScriptTimedOut
    | Success
    | UnknownError
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText LifecycleErrorCode where
    parser = takeLowerText >>= \case
        "scriptfailed" -> pure ScriptFailed
        "scriptmissing" -> pure ScriptMissing
        "scriptnotexecutable" -> pure ScriptNotExecutable
        "scripttimedout" -> pure ScriptTimedOut
        "success" -> pure Success
        "unknownerror" -> pure UnknownError
        e -> fromTextError $ "Failure parsing LifecycleErrorCode from value: '" <> e
           <> "'. Accepted values: ScriptFailed, ScriptMissing, ScriptNotExecutable, ScriptTimedOut, Success, UnknownError"

instance ToText LifecycleErrorCode where
    toText = \case
        ScriptFailed -> "ScriptFailed"
        ScriptMissing -> "ScriptMissing"
        ScriptNotExecutable -> "ScriptNotExecutable"
        ScriptTimedOut -> "ScriptTimedOut"
        Success -> "Success"
        UnknownError -> "UnknownError"

instance Hashable     LifecycleErrorCode
instance NFData       LifecycleErrorCode
instance ToByteString LifecycleErrorCode
instance ToQuery      LifecycleErrorCode
instance ToHeader     LifecycleErrorCode

instance FromJSON LifecycleErrorCode where
    parseJSON = parseJSONText "LifecycleErrorCode"

data LifecycleEventStatus
    = LESFailed
    | LESInProgress
    | LESPending
    | LESSkipped
    | LESSucceeded
    | LESUnknown
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText LifecycleEventStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure LESFailed
        "inprogress" -> pure LESInProgress
        "pending" -> pure LESPending
        "skipped" -> pure LESSkipped
        "succeeded" -> pure LESSucceeded
        "unknown" -> pure LESUnknown
        e -> fromTextError $ "Failure parsing LifecycleEventStatus from value: '" <> e
           <> "'. Accepted values: Failed, InProgress, Pending, Skipped, Succeeded, Unknown"

instance ToText LifecycleEventStatus where
    toText = \case
        LESFailed -> "Failed"
        LESInProgress -> "InProgress"
        LESPending -> "Pending"
        LESSkipped -> "Skipped"
        LESSucceeded -> "Succeeded"
        LESUnknown -> "Unknown"

instance Hashable     LifecycleEventStatus
instance NFData       LifecycleEventStatus
instance ToByteString LifecycleEventStatus
instance ToQuery      LifecycleEventStatus
instance ToHeader     LifecycleEventStatus

instance FromJSON LifecycleEventStatus where
    parseJSON = parseJSONText "LifecycleEventStatus"

data ListStateFilterAction
    = Exclude
    | Ignore
    | Include
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
instance NFData       ListStateFilterAction
instance ToByteString ListStateFilterAction
instance ToQuery      ListStateFilterAction
instance ToHeader     ListStateFilterAction

instance ToJSON ListStateFilterAction where
    toJSON = toJSONText

data MinimumHealthyHostsType
    = FleetPercent
    | HostCount
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText MinimumHealthyHostsType where
    parser = takeLowerText >>= \case
        "fleet_percent" -> pure FleetPercent
        "host_count" -> pure HostCount
        e -> fromTextError $ "Failure parsing MinimumHealthyHostsType from value: '" <> e
           <> "'. Accepted values: FLEET_PERCENT, HOST_COUNT"

instance ToText MinimumHealthyHostsType where
    toText = \case
        FleetPercent -> "FLEET_PERCENT"
        HostCount -> "HOST_COUNT"

instance Hashable     MinimumHealthyHostsType
instance NFData       MinimumHealthyHostsType
instance ToByteString MinimumHealthyHostsType
instance ToQuery      MinimumHealthyHostsType
instance ToHeader     MinimumHealthyHostsType

instance ToJSON MinimumHealthyHostsType where
    toJSON = toJSONText

instance FromJSON MinimumHealthyHostsType where
    parseJSON = parseJSONText "MinimumHealthyHostsType"

data RegistrationStatus
    = Deregistered
    | Registered
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText RegistrationStatus where
    parser = takeLowerText >>= \case
        "deregistered" -> pure Deregistered
        "registered" -> pure Registered
        e -> fromTextError $ "Failure parsing RegistrationStatus from value: '" <> e
           <> "'. Accepted values: Deregistered, Registered"

instance ToText RegistrationStatus where
    toText = \case
        Deregistered -> "Deregistered"
        Registered -> "Registered"

instance Hashable     RegistrationStatus
instance NFData       RegistrationStatus
instance ToByteString RegistrationStatus
instance ToQuery      RegistrationStatus
instance ToHeader     RegistrationStatus

instance ToJSON RegistrationStatus where
    toJSON = toJSONText

data RevisionLocationType
    = GitHub
    | S3
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText RevisionLocationType where
    parser = takeLowerText >>= \case
        "github" -> pure GitHub
        "s3" -> pure S3
        e -> fromTextError $ "Failure parsing RevisionLocationType from value: '" <> e
           <> "'. Accepted values: GitHub, S3"

instance ToText RevisionLocationType where
    toText = \case
        GitHub -> "GitHub"
        S3 -> "S3"

instance Hashable     RevisionLocationType
instance NFData       RevisionLocationType
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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
instance NFData       SortOrder
instance ToByteString SortOrder
instance ToQuery      SortOrder
instance ToHeader     SortOrder

instance ToJSON SortOrder where
    toJSON = toJSONText

data StopStatus
    = SSPending
    | SSSucceeded
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText StopStatus where
    parser = takeLowerText >>= \case
        "pending" -> pure SSPending
        "succeeded" -> pure SSSucceeded
        e -> fromTextError $ "Failure parsing StopStatus from value: '" <> e
           <> "'. Accepted values: Pending, Succeeded"

instance ToText StopStatus where
    toText = \case
        SSPending -> "Pending"
        SSSucceeded -> "Succeeded"

instance Hashable     StopStatus
instance NFData       StopStatus
instance ToByteString StopStatus
instance ToQuery      StopStatus
instance ToHeader     StopStatus

instance FromJSON StopStatus where
    parseJSON = parseJSONText "StopStatus"

data TagFilterType
    = TFTKeyAndValue
    | TFTKeyOnly
    | TFTValueOnly
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText TagFilterType where
    parser = takeLowerText >>= \case
        "key_and_value" -> pure TFTKeyAndValue
        "key_only" -> pure TFTKeyOnly
        "value_only" -> pure TFTValueOnly
        e -> fromTextError $ "Failure parsing TagFilterType from value: '" <> e
           <> "'. Accepted values: KEY_AND_VALUE, KEY_ONLY, VALUE_ONLY"

instance ToText TagFilterType where
    toText = \case
        TFTKeyAndValue -> "KEY_AND_VALUE"
        TFTKeyOnly -> "KEY_ONLY"
        TFTValueOnly -> "VALUE_ONLY"

instance Hashable     TagFilterType
instance NFData       TagFilterType
instance ToByteString TagFilterType
instance ToQuery      TagFilterType
instance ToHeader     TagFilterType

instance ToJSON TagFilterType where
    toJSON = toJSONText

instance FromJSON TagFilterType where
    parseJSON = parseJSONText "TagFilterType"

data TriggerEventType
    = DeploymentFailure
    | DeploymentStart
    | DeploymentStop
    | DeploymentSuccess
    | InstanceFailure
    | InstanceStart
    | InstanceSuccess
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText TriggerEventType where
    parser = takeLowerText >>= \case
        "deploymentfailure" -> pure DeploymentFailure
        "deploymentstart" -> pure DeploymentStart
        "deploymentstop" -> pure DeploymentStop
        "deploymentsuccess" -> pure DeploymentSuccess
        "instancefailure" -> pure InstanceFailure
        "instancestart" -> pure InstanceStart
        "instancesuccess" -> pure InstanceSuccess
        e -> fromTextError $ "Failure parsing TriggerEventType from value: '" <> e
           <> "'. Accepted values: DeploymentFailure, DeploymentStart, DeploymentStop, DeploymentSuccess, InstanceFailure, InstanceStart, InstanceSuccess"

instance ToText TriggerEventType where
    toText = \case
        DeploymentFailure -> "DeploymentFailure"
        DeploymentStart -> "DeploymentStart"
        DeploymentStop -> "DeploymentStop"
        DeploymentSuccess -> "DeploymentSuccess"
        InstanceFailure -> "InstanceFailure"
        InstanceStart -> "InstanceStart"
        InstanceSuccess -> "InstanceSuccess"

instance Hashable     TriggerEventType
instance NFData       TriggerEventType
instance ToByteString TriggerEventType
instance ToQuery      TriggerEventType
instance ToHeader     TriggerEventType

instance ToJSON TriggerEventType where
    toJSON = toJSONText

instance FromJSON TriggerEventType where
    parseJSON = parseJSONText "TriggerEventType"
