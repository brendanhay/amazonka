{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.Sum where

import           Network.AWS.Prelude

data ActionOnFailure
    = CancelAndWait
    | Continue
    | TerminateCluster
    | TerminateJobFlow
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ActionOnFailure where
    parser = takeLowerText >>= \case
        "cancel_and_wait" -> pure CancelAndWait
        "continue" -> pure Continue
        "terminate_cluster" -> pure TerminateCluster
        "terminate_job_flow" -> pure TerminateJobFlow
        e -> fromTextError $ "Failure parsing ActionOnFailure from value: '" <> e
           <> "'. Accepted values: CANCEL_AND_WAIT, CONTINUE, TERMINATE_CLUSTER, TERMINATE_JOB_FLOW"

instance ToText ActionOnFailure where
    toText = \case
        CancelAndWait -> "CANCEL_AND_WAIT"
        Continue -> "CONTINUE"
        TerminateCluster -> "TERMINATE_CLUSTER"
        TerminateJobFlow -> "TERMINATE_JOB_FLOW"

instance Hashable     ActionOnFailure
instance ToByteString ActionOnFailure
instance ToQuery      ActionOnFailure
instance ToHeader     ActionOnFailure

instance ToJSON ActionOnFailure where
    toJSON = toJSONText

instance FromJSON ActionOnFailure where
    parseJSON = parseJSONText "ActionOnFailure"

data ClusterState
    = CSBootstrapping
    | CSRunning
    | CSStarting
    | CSTerminated
    | CSTerminatedWithErrors
    | CSTerminating
    | CSWaiting
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
           <> "'. Accepted values: BOOTSTRAPPING, RUNNING, STARTING, TERMINATED, TERMINATED_WITH_ERRORS, TERMINATING, WAITING"

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
    | CSCRCInternalError
    | CSCRCStepFailure
    | CSCRCUserRequest
    | CSCRCValidationError
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ClusterStateChangeReasonCode where
    parser = takeLowerText >>= \case
        "all_steps_completed" -> pure CSCRCAllStepsCompleted
        "bootstrap_failure" -> pure CSCRCBootstrapFailure
        "instance_failure" -> pure CSCRCInstanceFailure
        "internal_error" -> pure CSCRCInternalError
        "step_failure" -> pure CSCRCStepFailure
        "user_request" -> pure CSCRCUserRequest
        "validation_error" -> pure CSCRCValidationError
        e -> fromTextError $ "Failure parsing ClusterStateChangeReasonCode from value: '" <> e
           <> "'. Accepted values: ALL_STEPS_COMPLETED, BOOTSTRAP_FAILURE, INSTANCE_FAILURE, INTERNAL_ERROR, STEP_FAILURE, USER_REQUEST, VALIDATION_ERROR"

instance ToText ClusterStateChangeReasonCode where
    toText = \case
        CSCRCAllStepsCompleted -> "ALL_STEPS_COMPLETED"
        CSCRCBootstrapFailure -> "BOOTSTRAP_FAILURE"
        CSCRCInstanceFailure -> "INSTANCE_FAILURE"
        CSCRCInternalError -> "INTERNAL_ERROR"
        CSCRCStepFailure -> "STEP_FAILURE"
        CSCRCUserRequest -> "USER_REQUEST"
        CSCRCValidationError -> "VALIDATION_ERROR"

instance Hashable     ClusterStateChangeReasonCode
instance ToByteString ClusterStateChangeReasonCode
instance ToQuery      ClusterStateChangeReasonCode
instance ToHeader     ClusterStateChangeReasonCode

instance FromJSON ClusterStateChangeReasonCode where
    parseJSON = parseJSONText "ClusterStateChangeReasonCode"

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
           <> "'. Accepted values: ARRESTED, BOOTSTRAPPING, ENDED, PROVISIONING, RESIZING, RUNNING, SHUTTING_DOWN, SUSPENDED, TERMINATED, TERMINATING"

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText InstanceGroupStateChangeReasonCode where
    parser = takeLowerText >>= \case
        "cluster_terminated" -> pure ClusterTerminated
        "instance_failure" -> pure InstanceFailure
        "internal_error" -> pure InternalError
        "validation_error" -> pure ValidationError
        e -> fromTextError $ "Failure parsing InstanceGroupStateChangeReasonCode from value: '" <> e
           <> "'. Accepted values: CLUSTER_TERMINATED, INSTANCE_FAILURE, INTERNAL_ERROR, VALIDATION_ERROR"

instance ToText InstanceGroupStateChangeReasonCode where
    toText = \case
        ClusterTerminated -> "CLUSTER_TERMINATED"
        InstanceFailure -> "INSTANCE_FAILURE"
        InternalError -> "INTERNAL_ERROR"
        ValidationError -> "VALIDATION_ERROR"

instance Hashable     InstanceGroupStateChangeReasonCode
instance ToByteString InstanceGroupStateChangeReasonCode
instance ToQuery      InstanceGroupStateChangeReasonCode
instance ToHeader     InstanceGroupStateChangeReasonCode

instance FromJSON InstanceGroupStateChangeReasonCode where
    parseJSON = parseJSONText "InstanceGroupStateChangeReasonCode"

data InstanceGroupType
    = Core
    | Master
    | Task
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText InstanceGroupType where
    parser = takeLowerText >>= \case
        "core" -> pure Core
        "master" -> pure Master
        "task" -> pure Task
        e -> fromTextError $ "Failure parsing InstanceGroupType from value: '" <> e
           <> "'. Accepted values: CORE, MASTER, TASK"

instance ToText InstanceGroupType where
    toText = \case
        Core -> "CORE"
        Master -> "MASTER"
        Task -> "TASK"

instance Hashable     InstanceGroupType
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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText InstanceRoleType where
    parser = takeLowerText >>= \case
        "core" -> pure IRTCore
        "master" -> pure IRTMaster
        "task" -> pure IRTTask
        e -> fromTextError $ "Failure parsing InstanceRoleType from value: '" <> e
           <> "'. Accepted values: CORE, MASTER, TASK"

instance ToText InstanceRoleType where
    toText = \case
        IRTCore -> "CORE"
        IRTMaster -> "MASTER"
        IRTTask -> "TASK"

instance Hashable     InstanceRoleType
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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText InstanceState where
    parser = takeLowerText >>= \case
        "awaiting_fulfillment" -> pure ISAwaitingFulfillment
        "bootstrapping" -> pure ISBootstrapping
        "provisioning" -> pure ISProvisioning
        "running" -> pure ISRunning
        "terminated" -> pure ISTerminated
        e -> fromTextError $ "Failure parsing InstanceState from value: '" <> e
           <> "'. Accepted values: AWAITING_FULFILLMENT, BOOTSTRAPPING, PROVISIONING, RUNNING, TERMINATED"

instance ToText InstanceState where
    toText = \case
        ISAwaitingFulfillment -> "AWAITING_FULFILLMENT"
        ISBootstrapping -> "BOOTSTRAPPING"
        ISProvisioning -> "PROVISIONING"
        ISRunning -> "RUNNING"
        ISTerminated -> "TERMINATED"

instance Hashable     InstanceState
instance ToByteString InstanceState
instance ToQuery      InstanceState
instance ToHeader     InstanceState

instance FromJSON InstanceState where
    parseJSON = parseJSONText "InstanceState"

data InstanceStateChangeReasonCode
    = ISCRCBootstrapFailure
    | ISCRCClusterTerminated
    | ISCRCInstanceFailure
    | ISCRCInternalError
    | ISCRCValidationError
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText InstanceStateChangeReasonCode where
    parser = takeLowerText >>= \case
        "bootstrap_failure" -> pure ISCRCBootstrapFailure
        "cluster_terminated" -> pure ISCRCClusterTerminated
        "instance_failure" -> pure ISCRCInstanceFailure
        "internal_error" -> pure ISCRCInternalError
        "validation_error" -> pure ISCRCValidationError
        e -> fromTextError $ "Failure parsing InstanceStateChangeReasonCode from value: '" <> e
           <> "'. Accepted values: BOOTSTRAP_FAILURE, CLUSTER_TERMINATED, INSTANCE_FAILURE, INTERNAL_ERROR, VALIDATION_ERROR"

instance ToText InstanceStateChangeReasonCode where
    toText = \case
        ISCRCBootstrapFailure -> "BOOTSTRAP_FAILURE"
        ISCRCClusterTerminated -> "CLUSTER_TERMINATED"
        ISCRCInstanceFailure -> "INSTANCE_FAILURE"
        ISCRCInternalError -> "INTERNAL_ERROR"
        ISCRCValidationError -> "VALIDATION_ERROR"

instance Hashable     InstanceStateChangeReasonCode
instance ToByteString InstanceStateChangeReasonCode
instance ToQuery      InstanceStateChangeReasonCode
instance ToHeader     InstanceStateChangeReasonCode

instance FromJSON InstanceStateChangeReasonCode where
    parseJSON = parseJSONText "InstanceStateChangeReasonCode"

data MarketType
    = OnDemand
    | Spot
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText MarketType where
    parser = takeLowerText >>= \case
        "on_demand" -> pure OnDemand
        "spot" -> pure Spot
        e -> fromTextError $ "Failure parsing MarketType from value: '" <> e
           <> "'. Accepted values: ON_DEMAND, SPOT"

instance ToText MarketType where
    toText = \case
        OnDemand -> "ON_DEMAND"
        Spot -> "SPOT"

instance Hashable     MarketType
instance ToByteString MarketType
instance ToQuery      MarketType
instance ToHeader     MarketType

instance ToJSON MarketType where
    toJSON = toJSONText

instance FromJSON MarketType where
    parseJSON = parseJSONText "MarketType"

data StepState
    = SSCancelled
    | SSCompleted
    | SSFailed
    | SSInterrupted
    | SSPending
    | SSRunning
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText StepState where
    parser = takeLowerText >>= \case
        "cancelled" -> pure SSCancelled
        "completed" -> pure SSCompleted
        "failed" -> pure SSFailed
        "interrupted" -> pure SSInterrupted
        "pending" -> pure SSPending
        "running" -> pure SSRunning
        e -> fromTextError $ "Failure parsing StepState from value: '" <> e
           <> "'. Accepted values: CANCELLED, COMPLETED, FAILED, INTERRUPTED, PENDING, RUNNING"

instance ToText StepState where
    toText = \case
        SSCancelled -> "CANCELLED"
        SSCompleted -> "COMPLETED"
        SSFailed -> "FAILED"
        SSInterrupted -> "INTERRUPTED"
        SSPending -> "PENDING"
        SSRunning -> "RUNNING"

instance Hashable     StepState
instance ToByteString StepState
instance ToQuery      StepState
instance ToHeader     StepState

instance ToJSON StepState where
    toJSON = toJSONText

instance FromJSON StepState where
    parseJSON = parseJSONText "StepState"

data StepStateChangeReasonCode =
    None
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText StepStateChangeReasonCode where
    parser = takeLowerText >>= \case
        "none" -> pure None
        e -> fromTextError $ "Failure parsing StepStateChangeReasonCode from value: '" <> e
           <> "'. Accepted values: NONE"

instance ToText StepStateChangeReasonCode where
    toText = \case
        None -> "NONE"

instance Hashable     StepStateChangeReasonCode
instance ToByteString StepStateChangeReasonCode
instance ToQuery      StepStateChangeReasonCode
instance ToHeader     StepStateChangeReasonCode

instance FromJSON StepStateChangeReasonCode where
    parseJSON = parseJSONText "StepStateChangeReasonCode"
