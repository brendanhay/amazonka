{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.Sum where

import Network.AWS.Prelude

data ActionOnFailure
  = CancelAndWait
  | Continue
  | TerminateCluster
  | TerminateJobFlow
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ActionOnFailure where
    parser = takeLowerText >>= \case
        "cancel_and_wait" -> pure CancelAndWait
        "continue" -> pure Continue
        "terminate_cluster" -> pure TerminateCluster
        "terminate_job_flow" -> pure TerminateJobFlow
        e -> fromTextError $ "Failure parsing ActionOnFailure from value: '" <> e
           <> "'. Accepted values: cancel_and_wait, continue, terminate_cluster, terminate_job_flow"

instance ToText ActionOnFailure where
    toText = \case
        CancelAndWait -> "CANCEL_AND_WAIT"
        Continue -> "CONTINUE"
        TerminateCluster -> "TERMINATE_CLUSTER"
        TerminateJobFlow -> "TERMINATE_JOB_FLOW"

instance Hashable     ActionOnFailure
instance NFData       ActionOnFailure
instance ToByteString ActionOnFailure
instance ToQuery      ActionOnFailure
instance ToHeader     ActionOnFailure

instance ToJSON ActionOnFailure where
    toJSON = toJSONText

instance FromJSON ActionOnFailure where
    parseJSON = parseJSONText "ActionOnFailure"

data AdjustmentType
  = ChangeInCapacity
  | ExactCapacity
  | PercentChangeInCapacity
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AdjustmentType where
    parser = takeLowerText >>= \case
        "change_in_capacity" -> pure ChangeInCapacity
        "exact_capacity" -> pure ExactCapacity
        "percent_change_in_capacity" -> pure PercentChangeInCapacity
        e -> fromTextError $ "Failure parsing AdjustmentType from value: '" <> e
           <> "'. Accepted values: change_in_capacity, exact_capacity, percent_change_in_capacity"

instance ToText AdjustmentType where
    toText = \case
        ChangeInCapacity -> "CHANGE_IN_CAPACITY"
        ExactCapacity -> "EXACT_CAPACITY"
        PercentChangeInCapacity -> "PERCENT_CHANGE_IN_CAPACITY"

instance Hashable     AdjustmentType
instance NFData       AdjustmentType
instance ToByteString AdjustmentType
instance ToQuery      AdjustmentType
instance ToHeader     AdjustmentType

instance ToJSON AdjustmentType where
    toJSON = toJSONText

instance FromJSON AdjustmentType where
    parseJSON = parseJSONText "AdjustmentType"

data AutoScalingPolicyState
  = Attached
  | Attaching
  | Detached
  | Detaching
  | Failed
  | Pending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AutoScalingPolicyState where
    parser = takeLowerText >>= \case
        "attached" -> pure Attached
        "attaching" -> pure Attaching
        "detached" -> pure Detached
        "detaching" -> pure Detaching
        "failed" -> pure Failed
        "pending" -> pure Pending
        e -> fromTextError $ "Failure parsing AutoScalingPolicyState from value: '" <> e
           <> "'. Accepted values: attached, attaching, detached, detaching, failed, pending"

instance ToText AutoScalingPolicyState where
    toText = \case
        Attached -> "ATTACHED"
        Attaching -> "ATTACHING"
        Detached -> "DETACHED"
        Detaching -> "DETACHING"
        Failed -> "FAILED"
        Pending -> "PENDING"

instance Hashable     AutoScalingPolicyState
instance NFData       AutoScalingPolicyState
instance ToByteString AutoScalingPolicyState
instance ToQuery      AutoScalingPolicyState
instance ToHeader     AutoScalingPolicyState

instance FromJSON AutoScalingPolicyState where
    parseJSON = parseJSONText "AutoScalingPolicyState"

data AutoScalingPolicyStateChangeReasonCode
  = CleanupFailure
  | ProvisionFailure
  | UserRequest
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AutoScalingPolicyStateChangeReasonCode where
    parser = takeLowerText >>= \case
        "cleanup_failure" -> pure CleanupFailure
        "provision_failure" -> pure ProvisionFailure
        "user_request" -> pure UserRequest
        e -> fromTextError $ "Failure parsing AutoScalingPolicyStateChangeReasonCode from value: '" <> e
           <> "'. Accepted values: cleanup_failure, provision_failure, user_request"

instance ToText AutoScalingPolicyStateChangeReasonCode where
    toText = \case
        CleanupFailure -> "CLEANUP_FAILURE"
        ProvisionFailure -> "PROVISION_FAILURE"
        UserRequest -> "USER_REQUEST"

instance Hashable     AutoScalingPolicyStateChangeReasonCode
instance NFData       AutoScalingPolicyStateChangeReasonCode
instance ToByteString AutoScalingPolicyStateChangeReasonCode
instance ToQuery      AutoScalingPolicyStateChangeReasonCode
instance ToHeader     AutoScalingPolicyStateChangeReasonCode

instance FromJSON AutoScalingPolicyStateChangeReasonCode where
    parseJSON = parseJSONText "AutoScalingPolicyStateChangeReasonCode"

data CancelStepsRequestStatus
  = CSRSFailed
  | CSRSSubmitted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CancelStepsRequestStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure CSRSFailed
        "submitted" -> pure CSRSSubmitted
        e -> fromTextError $ "Failure parsing CancelStepsRequestStatus from value: '" <> e
           <> "'. Accepted values: failed, submitted"

instance ToText CancelStepsRequestStatus where
    toText = \case
        CSRSFailed -> "FAILED"
        CSRSSubmitted -> "SUBMITTED"

instance Hashable     CancelStepsRequestStatus
instance NFData       CancelStepsRequestStatus
instance ToByteString CancelStepsRequestStatus
instance ToQuery      CancelStepsRequestStatus
instance ToHeader     CancelStepsRequestStatus

instance FromJSON CancelStepsRequestStatus where
    parseJSON = parseJSONText "CancelStepsRequestStatus"

data ClusterState
  = CSBootstrapping
  | CSRunning
  | CSStarting
  | CSTerminated
  | CSTerminatedWithErrors
  | CSTerminating
  | CSWaiting
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ClusterState where
    parser = takeLowerText >>= \case
        "bootstrapping" -> pure CSBootstrapping
        "running" -> pure CSRunning
        "starting" -> pure CSStarting
        "terminated" -> pure CSTerminated
        "terminated_with_errors" -> pure CSTerminatedWithErrors
        "terminating" -> pure CSTerminating
        "waiting" -> pure CSWaiting
        e -> fromTextError $ "Failure parsing ClusterState from value: '" <> e
           <> "'. Accepted values: bootstrapping, running, starting, terminated, terminated_with_errors, terminating, waiting"

instance ToText ClusterState where
    toText = \case
        CSBootstrapping -> "BOOTSTRAPPING"
        CSRunning -> "RUNNING"
        CSStarting -> "STARTING"
        CSTerminated -> "TERMINATED"
        CSTerminatedWithErrors -> "TERMINATED_WITH_ERRORS"
        CSTerminating -> "TERMINATING"
        CSWaiting -> "WAITING"

instance Hashable     ClusterState
instance NFData       ClusterState
instance ToByteString ClusterState
instance ToQuery      ClusterState
instance ToHeader     ClusterState

instance ToJSON ClusterState where
    toJSON = toJSONText

instance FromJSON ClusterState where
    parseJSON = parseJSONText "ClusterState"

data ClusterStateChangeReasonCode
  = CSCRCAllStepsCompleted
  | CSCRCBootstrapFailure
  | CSCRCInstanceFailure
  | CSCRCInstanceFleetTimeout
  | CSCRCInternalError
  | CSCRCStepFailure
  | CSCRCUserRequest
  | CSCRCValidationError
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ClusterStateChangeReasonCode where
    parser = takeLowerText >>= \case
        "all_steps_completed" -> pure CSCRCAllStepsCompleted
        "bootstrap_failure" -> pure CSCRCBootstrapFailure
        "instance_failure" -> pure CSCRCInstanceFailure
        "instance_fleet_timeout" -> pure CSCRCInstanceFleetTimeout
        "internal_error" -> pure CSCRCInternalError
        "step_failure" -> pure CSCRCStepFailure
        "user_request" -> pure CSCRCUserRequest
        "validation_error" -> pure CSCRCValidationError
        e -> fromTextError $ "Failure parsing ClusterStateChangeReasonCode from value: '" <> e
           <> "'. Accepted values: all_steps_completed, bootstrap_failure, instance_failure, instance_fleet_timeout, internal_error, step_failure, user_request, validation_error"

instance ToText ClusterStateChangeReasonCode where
    toText = \case
        CSCRCAllStepsCompleted -> "ALL_STEPS_COMPLETED"
        CSCRCBootstrapFailure -> "BOOTSTRAP_FAILURE"
        CSCRCInstanceFailure -> "INSTANCE_FAILURE"
        CSCRCInstanceFleetTimeout -> "INSTANCE_FLEET_TIMEOUT"
        CSCRCInternalError -> "INTERNAL_ERROR"
        CSCRCStepFailure -> "STEP_FAILURE"
        CSCRCUserRequest -> "USER_REQUEST"
        CSCRCValidationError -> "VALIDATION_ERROR"

instance Hashable     ClusterStateChangeReasonCode
instance NFData       ClusterStateChangeReasonCode
instance ToByteString ClusterStateChangeReasonCode
instance ToQuery      ClusterStateChangeReasonCode
instance ToHeader     ClusterStateChangeReasonCode

instance FromJSON ClusterStateChangeReasonCode where
    parseJSON = parseJSONText "ClusterStateChangeReasonCode"

data ComparisonOperator
  = GreaterThan
  | GreaterThanOrEqual
  | LessThan
  | LessThanOrEqual
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ComparisonOperator where
    parser = takeLowerText >>= \case
        "greater_than" -> pure GreaterThan
        "greater_than_or_equal" -> pure GreaterThanOrEqual
        "less_than" -> pure LessThan
        "less_than_or_equal" -> pure LessThanOrEqual
        e -> fromTextError $ "Failure parsing ComparisonOperator from value: '" <> e
           <> "'. Accepted values: greater_than, greater_than_or_equal, less_than, less_than_or_equal"

instance ToText ComparisonOperator where
    toText = \case
        GreaterThan -> "GREATER_THAN"
        GreaterThanOrEqual -> "GREATER_THAN_OR_EQUAL"
        LessThan -> "LESS_THAN"
        LessThanOrEqual -> "LESS_THAN_OR_EQUAL"

instance Hashable     ComparisonOperator
instance NFData       ComparisonOperator
instance ToByteString ComparisonOperator
instance ToQuery      ComparisonOperator
instance ToHeader     ComparisonOperator

instance ToJSON ComparisonOperator where
    toJSON = toJSONText

instance FromJSON ComparisonOperator where
    parseJSON = parseJSONText "ComparisonOperator"

data InstanceCollectionType
  = InstanceFleet
  | InstanceGroup
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceCollectionType where
    parser = takeLowerText >>= \case
        "instance_fleet" -> pure InstanceFleet
        "instance_group" -> pure InstanceGroup
        e -> fromTextError $ "Failure parsing InstanceCollectionType from value: '" <> e
           <> "'. Accepted values: instance_fleet, instance_group"

instance ToText InstanceCollectionType where
    toText = \case
        InstanceFleet -> "INSTANCE_FLEET"
        InstanceGroup -> "INSTANCE_GROUP"

instance Hashable     InstanceCollectionType
instance NFData       InstanceCollectionType
instance ToByteString InstanceCollectionType
instance ToQuery      InstanceCollectionType
instance ToHeader     InstanceCollectionType

instance FromJSON InstanceCollectionType where
    parseJSON = parseJSONText "InstanceCollectionType"

data InstanceFleetState
  = IFSBootstrapping
  | IFSProvisioning
  | IFSResizing
  | IFSRunning
  | IFSSuspended
  | IFSTerminated
  | IFSTerminating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceFleetState where
    parser = takeLowerText >>= \case
        "bootstrapping" -> pure IFSBootstrapping
        "provisioning" -> pure IFSProvisioning
        "resizing" -> pure IFSResizing
        "running" -> pure IFSRunning
        "suspended" -> pure IFSSuspended
        "terminated" -> pure IFSTerminated
        "terminating" -> pure IFSTerminating
        e -> fromTextError $ "Failure parsing InstanceFleetState from value: '" <> e
           <> "'. Accepted values: bootstrapping, provisioning, resizing, running, suspended, terminated, terminating"

instance ToText InstanceFleetState where
    toText = \case
        IFSBootstrapping -> "BOOTSTRAPPING"
        IFSProvisioning -> "PROVISIONING"
        IFSResizing -> "RESIZING"
        IFSRunning -> "RUNNING"
        IFSSuspended -> "SUSPENDED"
        IFSTerminated -> "TERMINATED"
        IFSTerminating -> "TERMINATING"

instance Hashable     InstanceFleetState
instance NFData       InstanceFleetState
instance ToByteString InstanceFleetState
instance ToQuery      InstanceFleetState
instance ToHeader     InstanceFleetState

instance FromJSON InstanceFleetState where
    parseJSON = parseJSONText "InstanceFleetState"

data InstanceFleetStateChangeReasonCode
  = IFSCRCClusterTerminated
  | IFSCRCInstanceFailure
  | IFSCRCInternalError
  | IFSCRCValidationError
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceFleetStateChangeReasonCode where
    parser = takeLowerText >>= \case
        "cluster_terminated" -> pure IFSCRCClusterTerminated
        "instance_failure" -> pure IFSCRCInstanceFailure
        "internal_error" -> pure IFSCRCInternalError
        "validation_error" -> pure IFSCRCValidationError
        e -> fromTextError $ "Failure parsing InstanceFleetStateChangeReasonCode from value: '" <> e
           <> "'. Accepted values: cluster_terminated, instance_failure, internal_error, validation_error"

instance ToText InstanceFleetStateChangeReasonCode where
    toText = \case
        IFSCRCClusterTerminated -> "CLUSTER_TERMINATED"
        IFSCRCInstanceFailure -> "INSTANCE_FAILURE"
        IFSCRCInternalError -> "INTERNAL_ERROR"
        IFSCRCValidationError -> "VALIDATION_ERROR"

instance Hashable     InstanceFleetStateChangeReasonCode
instance NFData       InstanceFleetStateChangeReasonCode
instance ToByteString InstanceFleetStateChangeReasonCode
instance ToQuery      InstanceFleetStateChangeReasonCode
instance ToHeader     InstanceFleetStateChangeReasonCode

instance FromJSON InstanceFleetStateChangeReasonCode where
    parseJSON = parseJSONText "InstanceFleetStateChangeReasonCode"

data InstanceFleetType
  = IFTCore
  | IFTMaster
  | IFTTask
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceFleetType where
    parser = takeLowerText >>= \case
        "core" -> pure IFTCore
        "master" -> pure IFTMaster
        "task" -> pure IFTTask
        e -> fromTextError $ "Failure parsing InstanceFleetType from value: '" <> e
           <> "'. Accepted values: core, master, task"

instance ToText InstanceFleetType where
    toText = \case
        IFTCore -> "CORE"
        IFTMaster -> "MASTER"
        IFTTask -> "TASK"

instance Hashable     InstanceFleetType
instance NFData       InstanceFleetType
instance ToByteString InstanceFleetType
instance ToQuery      InstanceFleetType
instance ToHeader     InstanceFleetType

instance ToJSON InstanceFleetType where
    toJSON = toJSONText

instance FromJSON InstanceFleetType where
    parseJSON = parseJSONText "InstanceFleetType"

data InstanceGroupState
  = Arrested
  | Bootstrapping
  | Ended
  | Provisioning
  | Resizing
  | Running
  | ShuttingDown
  | Suspended
  | Terminated
  | Terminating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceGroupState where
    parser = takeLowerText >>= \case
        "arrested" -> pure Arrested
        "bootstrapping" -> pure Bootstrapping
        "ended" -> pure Ended
        "provisioning" -> pure Provisioning
        "resizing" -> pure Resizing
        "running" -> pure Running
        "shutting_down" -> pure ShuttingDown
        "suspended" -> pure Suspended
        "terminated" -> pure Terminated
        "terminating" -> pure Terminating
        e -> fromTextError $ "Failure parsing InstanceGroupState from value: '" <> e
           <> "'. Accepted values: arrested, bootstrapping, ended, provisioning, resizing, running, shutting_down, suspended, terminated, terminating"

instance ToText InstanceGroupState where
    toText = \case
        Arrested -> "ARRESTED"
        Bootstrapping -> "BOOTSTRAPPING"
        Ended -> "ENDED"
        Provisioning -> "PROVISIONING"
        Resizing -> "RESIZING"
        Running -> "RUNNING"
        ShuttingDown -> "SHUTTING_DOWN"
        Suspended -> "SUSPENDED"
        Terminated -> "TERMINATED"
        Terminating -> "TERMINATING"

instance Hashable     InstanceGroupState
instance NFData       InstanceGroupState
instance ToByteString InstanceGroupState
instance ToQuery      InstanceGroupState
instance ToHeader     InstanceGroupState

instance FromJSON InstanceGroupState where
    parseJSON = parseJSONText "InstanceGroupState"

data InstanceGroupStateChangeReasonCode
  = ClusterTerminated
  | InstanceFailure
  | InternalError
  | ValidationError
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceGroupStateChangeReasonCode where
    parser = takeLowerText >>= \case
        "cluster_terminated" -> pure ClusterTerminated
        "instance_failure" -> pure InstanceFailure
        "internal_error" -> pure InternalError
        "validation_error" -> pure ValidationError
        e -> fromTextError $ "Failure parsing InstanceGroupStateChangeReasonCode from value: '" <> e
           <> "'. Accepted values: cluster_terminated, instance_failure, internal_error, validation_error"

instance ToText InstanceGroupStateChangeReasonCode where
    toText = \case
        ClusterTerminated -> "CLUSTER_TERMINATED"
        InstanceFailure -> "INSTANCE_FAILURE"
        InternalError -> "INTERNAL_ERROR"
        ValidationError -> "VALIDATION_ERROR"

instance Hashable     InstanceGroupStateChangeReasonCode
instance NFData       InstanceGroupStateChangeReasonCode
instance ToByteString InstanceGroupStateChangeReasonCode
instance ToQuery      InstanceGroupStateChangeReasonCode
instance ToHeader     InstanceGroupStateChangeReasonCode

instance FromJSON InstanceGroupStateChangeReasonCode where
    parseJSON = parseJSONText "InstanceGroupStateChangeReasonCode"

data InstanceGroupType
  = Core
  | Master
  | Task
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceGroupType where
    parser = takeLowerText >>= \case
        "core" -> pure Core
        "master" -> pure Master
        "task" -> pure Task
        e -> fromTextError $ "Failure parsing InstanceGroupType from value: '" <> e
           <> "'. Accepted values: core, master, task"

instance ToText InstanceGroupType where
    toText = \case
        Core -> "CORE"
        Master -> "MASTER"
        Task -> "TASK"

instance Hashable     InstanceGroupType
instance NFData       InstanceGroupType
instance ToByteString InstanceGroupType
instance ToQuery      InstanceGroupType
instance ToHeader     InstanceGroupType

instance ToJSON InstanceGroupType where
    toJSON = toJSONText

instance FromJSON InstanceGroupType where
    parseJSON = parseJSONText "InstanceGroupType"

data InstanceRoleType
  = IRTCore
  | IRTMaster
  | IRTTask
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceRoleType where
    parser = takeLowerText >>= \case
        "core" -> pure IRTCore
        "master" -> pure IRTMaster
        "task" -> pure IRTTask
        e -> fromTextError $ "Failure parsing InstanceRoleType from value: '" <> e
           <> "'. Accepted values: core, master, task"

instance ToText InstanceRoleType where
    toText = \case
        IRTCore -> "CORE"
        IRTMaster -> "MASTER"
        IRTTask -> "TASK"

instance Hashable     InstanceRoleType
instance NFData       InstanceRoleType
instance ToByteString InstanceRoleType
instance ToQuery      InstanceRoleType
instance ToHeader     InstanceRoleType

instance ToJSON InstanceRoleType where
    toJSON = toJSONText

data InstanceState
  = ISAwaitingFulfillment
  | ISBootstrapping
  | ISProvisioning
  | ISRunning
  | ISTerminated
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceState where
    parser = takeLowerText >>= \case
        "awaiting_fulfillment" -> pure ISAwaitingFulfillment
        "bootstrapping" -> pure ISBootstrapping
        "provisioning" -> pure ISProvisioning
        "running" -> pure ISRunning
        "terminated" -> pure ISTerminated
        e -> fromTextError $ "Failure parsing InstanceState from value: '" <> e
           <> "'. Accepted values: awaiting_fulfillment, bootstrapping, provisioning, running, terminated"

instance ToText InstanceState where
    toText = \case
        ISAwaitingFulfillment -> "AWAITING_FULFILLMENT"
        ISBootstrapping -> "BOOTSTRAPPING"
        ISProvisioning -> "PROVISIONING"
        ISRunning -> "RUNNING"
        ISTerminated -> "TERMINATED"

instance Hashable     InstanceState
instance NFData       InstanceState
instance ToByteString InstanceState
instance ToQuery      InstanceState
instance ToHeader     InstanceState

instance ToJSON InstanceState where
    toJSON = toJSONText

instance FromJSON InstanceState where
    parseJSON = parseJSONText "InstanceState"

data InstanceStateChangeReasonCode
  = ISCRCBootstrapFailure
  | ISCRCClusterTerminated
  | ISCRCInstanceFailure
  | ISCRCInternalError
  | ISCRCValidationError
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceStateChangeReasonCode where
    parser = takeLowerText >>= \case
        "bootstrap_failure" -> pure ISCRCBootstrapFailure
        "cluster_terminated" -> pure ISCRCClusterTerminated
        "instance_failure" -> pure ISCRCInstanceFailure
        "internal_error" -> pure ISCRCInternalError
        "validation_error" -> pure ISCRCValidationError
        e -> fromTextError $ "Failure parsing InstanceStateChangeReasonCode from value: '" <> e
           <> "'. Accepted values: bootstrap_failure, cluster_terminated, instance_failure, internal_error, validation_error"

instance ToText InstanceStateChangeReasonCode where
    toText = \case
        ISCRCBootstrapFailure -> "BOOTSTRAP_FAILURE"
        ISCRCClusterTerminated -> "CLUSTER_TERMINATED"
        ISCRCInstanceFailure -> "INSTANCE_FAILURE"
        ISCRCInternalError -> "INTERNAL_ERROR"
        ISCRCValidationError -> "VALIDATION_ERROR"

instance Hashable     InstanceStateChangeReasonCode
instance NFData       InstanceStateChangeReasonCode
instance ToByteString InstanceStateChangeReasonCode
instance ToQuery      InstanceStateChangeReasonCode
instance ToHeader     InstanceStateChangeReasonCode

instance FromJSON InstanceStateChangeReasonCode where
    parseJSON = parseJSONText "InstanceStateChangeReasonCode"

data MarketType
  = OnDemand
  | Spot
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MarketType where
    parser = takeLowerText >>= \case
        "on_demand" -> pure OnDemand
        "spot" -> pure Spot
        e -> fromTextError $ "Failure parsing MarketType from value: '" <> e
           <> "'. Accepted values: on_demand, spot"

instance ToText MarketType where
    toText = \case
        OnDemand -> "ON_DEMAND"
        Spot -> "SPOT"

instance Hashable     MarketType
instance NFData       MarketType
instance ToByteString MarketType
instance ToQuery      MarketType
instance ToHeader     MarketType

instance ToJSON MarketType where
    toJSON = toJSONText

instance FromJSON MarketType where
    parseJSON = parseJSONText "MarketType"

data RepoUpgradeOnBoot
  = RUOBNone
  | RUOBSecurity
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RepoUpgradeOnBoot where
    parser = takeLowerText >>= \case
        "none" -> pure RUOBNone
        "security" -> pure RUOBSecurity
        e -> fromTextError $ "Failure parsing RepoUpgradeOnBoot from value: '" <> e
           <> "'. Accepted values: none, security"

instance ToText RepoUpgradeOnBoot where
    toText = \case
        RUOBNone -> "NONE"
        RUOBSecurity -> "SECURITY"

instance Hashable     RepoUpgradeOnBoot
instance NFData       RepoUpgradeOnBoot
instance ToByteString RepoUpgradeOnBoot
instance ToQuery      RepoUpgradeOnBoot
instance ToHeader     RepoUpgradeOnBoot

instance ToJSON RepoUpgradeOnBoot where
    toJSON = toJSONText

instance FromJSON RepoUpgradeOnBoot where
    parseJSON = parseJSONText "RepoUpgradeOnBoot"

data ScaleDownBehavior
  = TerminateAtInstanceHour
  | TerminateAtTaskCompletion
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ScaleDownBehavior where
    parser = takeLowerText >>= \case
        "terminate_at_instance_hour" -> pure TerminateAtInstanceHour
        "terminate_at_task_completion" -> pure TerminateAtTaskCompletion
        e -> fromTextError $ "Failure parsing ScaleDownBehavior from value: '" <> e
           <> "'. Accepted values: terminate_at_instance_hour, terminate_at_task_completion"

instance ToText ScaleDownBehavior where
    toText = \case
        TerminateAtInstanceHour -> "TERMINATE_AT_INSTANCE_HOUR"
        TerminateAtTaskCompletion -> "TERMINATE_AT_TASK_COMPLETION"

instance Hashable     ScaleDownBehavior
instance NFData       ScaleDownBehavior
instance ToByteString ScaleDownBehavior
instance ToQuery      ScaleDownBehavior
instance ToHeader     ScaleDownBehavior

instance ToJSON ScaleDownBehavior where
    toJSON = toJSONText

instance FromJSON ScaleDownBehavior where
    parseJSON = parseJSONText "ScaleDownBehavior"

data SpotProvisioningTimeoutAction
  = SPTASwitchToOnDemand
  | SPTATerminateCluster
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SpotProvisioningTimeoutAction where
    parser = takeLowerText >>= \case
        "switch_to_on_demand" -> pure SPTASwitchToOnDemand
        "terminate_cluster" -> pure SPTATerminateCluster
        e -> fromTextError $ "Failure parsing SpotProvisioningTimeoutAction from value: '" <> e
           <> "'. Accepted values: switch_to_on_demand, terminate_cluster"

instance ToText SpotProvisioningTimeoutAction where
    toText = \case
        SPTASwitchToOnDemand -> "SWITCH_TO_ON_DEMAND"
        SPTATerminateCluster -> "TERMINATE_CLUSTER"

instance Hashable     SpotProvisioningTimeoutAction
instance NFData       SpotProvisioningTimeoutAction
instance ToByteString SpotProvisioningTimeoutAction
instance ToQuery      SpotProvisioningTimeoutAction
instance ToHeader     SpotProvisioningTimeoutAction

instance ToJSON SpotProvisioningTimeoutAction where
    toJSON = toJSONText

instance FromJSON SpotProvisioningTimeoutAction where
    parseJSON = parseJSONText "SpotProvisioningTimeoutAction"

data Statistic
  = Average
  | Maximum
  | Minimum
  | SampleCount
  | Sum
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Statistic where
    parser = takeLowerText >>= \case
        "average" -> pure Average
        "maximum" -> pure Maximum
        "minimum" -> pure Minimum
        "sample_count" -> pure SampleCount
        "sum" -> pure Sum
        e -> fromTextError $ "Failure parsing Statistic from value: '" <> e
           <> "'. Accepted values: average, maximum, minimum, sample_count, sum"

instance ToText Statistic where
    toText = \case
        Average -> "AVERAGE"
        Maximum -> "MAXIMUM"
        Minimum -> "MINIMUM"
        SampleCount -> "SAMPLE_COUNT"
        Sum -> "SUM"

instance Hashable     Statistic
instance NFData       Statistic
instance ToByteString Statistic
instance ToQuery      Statistic
instance ToHeader     Statistic

instance ToJSON Statistic where
    toJSON = toJSONText

instance FromJSON Statistic where
    parseJSON = parseJSONText "Statistic"

data StepState
  = SSCancelPending
  | SSCancelled
  | SSCompleted
  | SSFailed
  | SSInterrupted
  | SSPending
  | SSRunning
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StepState where
    parser = takeLowerText >>= \case
        "cancel_pending" -> pure SSCancelPending
        "cancelled" -> pure SSCancelled
        "completed" -> pure SSCompleted
        "failed" -> pure SSFailed
        "interrupted" -> pure SSInterrupted
        "pending" -> pure SSPending
        "running" -> pure SSRunning
        e -> fromTextError $ "Failure parsing StepState from value: '" <> e
           <> "'. Accepted values: cancel_pending, cancelled, completed, failed, interrupted, pending, running"

instance ToText StepState where
    toText = \case
        SSCancelPending -> "CANCEL_PENDING"
        SSCancelled -> "CANCELLED"
        SSCompleted -> "COMPLETED"
        SSFailed -> "FAILED"
        SSInterrupted -> "INTERRUPTED"
        SSPending -> "PENDING"
        SSRunning -> "RUNNING"

instance Hashable     StepState
instance NFData       StepState
instance ToByteString StepState
instance ToQuery      StepState
instance ToHeader     StepState

instance ToJSON StepState where
    toJSON = toJSONText

instance FromJSON StepState where
    parseJSON = parseJSONText "StepState"

data StepStateChangeReasonCode =
  SSCRCNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StepStateChangeReasonCode where
    parser = takeLowerText >>= \case
        "none" -> pure SSCRCNone
        e -> fromTextError $ "Failure parsing StepStateChangeReasonCode from value: '" <> e
           <> "'. Accepted values: none"

instance ToText StepStateChangeReasonCode where
    toText = \case
        SSCRCNone -> "NONE"

instance Hashable     StepStateChangeReasonCode
instance NFData       StepStateChangeReasonCode
instance ToByteString StepStateChangeReasonCode
instance ToQuery      StepStateChangeReasonCode
instance ToHeader     StepStateChangeReasonCode

instance FromJSON StepStateChangeReasonCode where
    parseJSON = parseJSONText "StepStateChangeReasonCode"

data Unit
  = Bits
  | BitsPerSecond
  | Bytes
  | BytesPerSecond
  | Count
  | CountPerSecond
  | GigaBits
  | GigaBitsPerSecond
  | GigaBytes
  | GigaBytesPerSecond
  | KiloBits
  | KiloBitsPerSecond
  | KiloBytes
  | KiloBytesPerSecond
  | MegaBits
  | MegaBitsPerSecond
  | MegaBytes
  | MegaBytesPerSecond
  | MicroSeconds
  | MilliSeconds
  | None
  | Percent
  | Seconds
  | TeraBits
  | TeraBitsPerSecond
  | TeraBytes
  | TeraBytesPerSecond
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Unit where
    parser = takeLowerText >>= \case
        "bits" -> pure Bits
        "bits_per_second" -> pure BitsPerSecond
        "bytes" -> pure Bytes
        "bytes_per_second" -> pure BytesPerSecond
        "count" -> pure Count
        "count_per_second" -> pure CountPerSecond
        "giga_bits" -> pure GigaBits
        "giga_bits_per_second" -> pure GigaBitsPerSecond
        "giga_bytes" -> pure GigaBytes
        "giga_bytes_per_second" -> pure GigaBytesPerSecond
        "kilo_bits" -> pure KiloBits
        "kilo_bits_per_second" -> pure KiloBitsPerSecond
        "kilo_bytes" -> pure KiloBytes
        "kilo_bytes_per_second" -> pure KiloBytesPerSecond
        "mega_bits" -> pure MegaBits
        "mega_bits_per_second" -> pure MegaBitsPerSecond
        "mega_bytes" -> pure MegaBytes
        "mega_bytes_per_second" -> pure MegaBytesPerSecond
        "micro_seconds" -> pure MicroSeconds
        "milli_seconds" -> pure MilliSeconds
        "none" -> pure None
        "percent" -> pure Percent
        "seconds" -> pure Seconds
        "tera_bits" -> pure TeraBits
        "tera_bits_per_second" -> pure TeraBitsPerSecond
        "tera_bytes" -> pure TeraBytes
        "tera_bytes_per_second" -> pure TeraBytesPerSecond
        e -> fromTextError $ "Failure parsing Unit from value: '" <> e
           <> "'. Accepted values: bits, bits_per_second, bytes, bytes_per_second, count, count_per_second, giga_bits, giga_bits_per_second, giga_bytes, giga_bytes_per_second, kilo_bits, kilo_bits_per_second, kilo_bytes, kilo_bytes_per_second, mega_bits, mega_bits_per_second, mega_bytes, mega_bytes_per_second, micro_seconds, milli_seconds, none, percent, seconds, tera_bits, tera_bits_per_second, tera_bytes, tera_bytes_per_second"

instance ToText Unit where
    toText = \case
        Bits -> "BITS"
        BitsPerSecond -> "BITS_PER_SECOND"
        Bytes -> "BYTES"
        BytesPerSecond -> "BYTES_PER_SECOND"
        Count -> "COUNT"
        CountPerSecond -> "COUNT_PER_SECOND"
        GigaBits -> "GIGA_BITS"
        GigaBitsPerSecond -> "GIGA_BITS_PER_SECOND"
        GigaBytes -> "GIGA_BYTES"
        GigaBytesPerSecond -> "GIGA_BYTES_PER_SECOND"
        KiloBits -> "KILO_BITS"
        KiloBitsPerSecond -> "KILO_BITS_PER_SECOND"
        KiloBytes -> "KILO_BYTES"
        KiloBytesPerSecond -> "KILO_BYTES_PER_SECOND"
        MegaBits -> "MEGA_BITS"
        MegaBitsPerSecond -> "MEGA_BITS_PER_SECOND"
        MegaBytes -> "MEGA_BYTES"
        MegaBytesPerSecond -> "MEGA_BYTES_PER_SECOND"
        MicroSeconds -> "MICRO_SECONDS"
        MilliSeconds -> "MILLI_SECONDS"
        None -> "NONE"
        Percent -> "PERCENT"
        Seconds -> "SECONDS"
        TeraBits -> "TERA_BITS"
        TeraBitsPerSecond -> "TERA_BITS_PER_SECOND"
        TeraBytes -> "TERA_BYTES"
        TeraBytesPerSecond -> "TERA_BYTES_PER_SECOND"

instance Hashable     Unit
instance NFData       Unit
instance ToByteString Unit
instance ToQuery      Unit
instance ToHeader     Unit

instance ToJSON Unit where
    toJSON = toJSONText

instance FromJSON Unit where
    parseJSON = parseJSONText "Unit"
