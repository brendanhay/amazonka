{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
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
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
        CancelAndWait -> "cancel_and_wait"
        Continue -> "continue"
        TerminateCluster -> "terminate_cluster"
        TerminateJobFlow -> "terminate_job_flow"

instance Hashable     ActionOnFailure
instance ToByteString ActionOnFailure
instance ToQuery      ActionOnFailure
instance ToHeader     ActionOnFailure

instance ToJSON ActionOnFailure where
    toJSON = toJSONText

instance FromJSON ActionOnFailure where
    parseJSON = parseJSONText "ActionOnFailure"

data ClusterState
    = Bootstrapping
    | Running
    | Starting
    | Terminated
    | TerminatedWithErrors
    | Terminating
    | Waiting
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ClusterState where
    parser = takeLowerText >>= \case
        "bootstrapping" -> pure Bootstrapping
        "running" -> pure Running
        "starting" -> pure Starting
        "terminated" -> pure Terminated
        "terminated_with_errors" -> pure TerminatedWithErrors
        "terminating" -> pure Terminating
        "waiting" -> pure Waiting
        e -> fromTextError $ "Failure parsing ClusterState from value: '" <> e
           <> "'. Accepted values: bootstrapping, running, starting, terminated, terminated_with_errors, terminating, waiting"

instance ToText ClusterState where
    toText = \case
        Bootstrapping -> "bootstrapping"
        Running -> "running"
        Starting -> "starting"
        Terminated -> "terminated"
        TerminatedWithErrors -> "terminated_with_errors"
        Terminating -> "terminating"
        Waiting -> "waiting"

instance Hashable     ClusterState
instance ToByteString ClusterState
instance ToQuery      ClusterState
instance ToHeader     ClusterState

instance ToJSON ClusterState where
    toJSON = toJSONText

instance FromJSON ClusterState where
    parseJSON = parseJSONText "ClusterState"

data ClusterStateChangeReasonCode
    = AllStepsCompleted
    | BootstrapFailure
    | InstanceFailure
    | InternalError
    | StepFailure
    | UserRequest
    | ValidationError
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ClusterStateChangeReasonCode where
    parser = takeLowerText >>= \case
        "all_steps_completed" -> pure AllStepsCompleted
        "bootstrap_failure" -> pure BootstrapFailure
        "instance_failure" -> pure InstanceFailure
        "internal_error" -> pure InternalError
        "step_failure" -> pure StepFailure
        "user_request" -> pure UserRequest
        "validation_error" -> pure ValidationError
        e -> fromTextError $ "Failure parsing ClusterStateChangeReasonCode from value: '" <> e
           <> "'. Accepted values: all_steps_completed, bootstrap_failure, instance_failure, internal_error, step_failure, user_request, validation_error"

instance ToText ClusterStateChangeReasonCode where
    toText = \case
        AllStepsCompleted -> "all_steps_completed"
        BootstrapFailure -> "bootstrap_failure"
        InstanceFailure -> "instance_failure"
        InternalError -> "internal_error"
        StepFailure -> "step_failure"
        UserRequest -> "user_request"
        ValidationError -> "validation_error"

instance Hashable     ClusterStateChangeReasonCode
instance ToByteString ClusterStateChangeReasonCode
instance ToQuery      ClusterStateChangeReasonCode
instance ToHeader     ClusterStateChangeReasonCode

instance FromJSON ClusterStateChangeReasonCode where
    parseJSON = parseJSONText "ClusterStateChangeReasonCode"

data InstanceGroupState
    = IGSArrested
    | IGSBootstrapping
    | IGSEnded
    | IGSProvisioning
    | IGSResizing
    | IGSRunning
    | IGSShuttingDown
    | IGSSuspended
    | IGSTerminated
    | IGSTerminating
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText InstanceGroupState where
    parser = takeLowerText >>= \case
        "arrested" -> pure IGSArrested
        "bootstrapping" -> pure IGSBootstrapping
        "ended" -> pure IGSEnded
        "provisioning" -> pure IGSProvisioning
        "resizing" -> pure IGSResizing
        "running" -> pure IGSRunning
        "shutting_down" -> pure IGSShuttingDown
        "suspended" -> pure IGSSuspended
        "terminated" -> pure IGSTerminated
        "terminating" -> pure IGSTerminating
        e -> fromTextError $ "Failure parsing InstanceGroupState from value: '" <> e
           <> "'. Accepted values: arrested, bootstrapping, ended, provisioning, resizing, running, shutting_down, suspended, terminated, terminating"

instance ToText InstanceGroupState where
    toText = \case
        IGSArrested -> "arrested"
        IGSBootstrapping -> "bootstrapping"
        IGSEnded -> "ended"
        IGSProvisioning -> "provisioning"
        IGSResizing -> "resizing"
        IGSRunning -> "running"
        IGSShuttingDown -> "shutting_down"
        IGSSuspended -> "suspended"
        IGSTerminated -> "terminated"
        IGSTerminating -> "terminating"

instance Hashable     InstanceGroupState
instance ToByteString InstanceGroupState
instance ToQuery      InstanceGroupState
instance ToHeader     InstanceGroupState

instance FromJSON InstanceGroupState where
    parseJSON = parseJSONText "InstanceGroupState"

data InstanceGroupStateChangeReasonCode
    = IGSCRCClusterTerminated
    | IGSCRCInstanceFailure
    | IGSCRCInternalError
    | IGSCRCValidationError
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText InstanceGroupStateChangeReasonCode where
    parser = takeLowerText >>= \case
        "cluster_terminated" -> pure IGSCRCClusterTerminated
        "instance_failure" -> pure IGSCRCInstanceFailure
        "internal_error" -> pure IGSCRCInternalError
        "validation_error" -> pure IGSCRCValidationError
        e -> fromTextError $ "Failure parsing InstanceGroupStateChangeReasonCode from value: '" <> e
           <> "'. Accepted values: cluster_terminated, instance_failure, internal_error, validation_error"

instance ToText InstanceGroupStateChangeReasonCode where
    toText = \case
        IGSCRCClusterTerminated -> "cluster_terminated"
        IGSCRCInstanceFailure -> "instance_failure"
        IGSCRCInternalError -> "internal_error"
        IGSCRCValidationError -> "validation_error"

instance Hashable     InstanceGroupStateChangeReasonCode
instance ToByteString InstanceGroupStateChangeReasonCode
instance ToQuery      InstanceGroupStateChangeReasonCode
instance ToHeader     InstanceGroupStateChangeReasonCode

instance FromJSON InstanceGroupStateChangeReasonCode where
    parseJSON = parseJSONText "InstanceGroupStateChangeReasonCode"

data InstanceGroupType
    = IGTCore
    | IGTMaster
    | IGTTask
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText InstanceGroupType where
    parser = takeLowerText >>= \case
        "core" -> pure IGTCore
        "master" -> pure IGTMaster
        "task" -> pure IGTTask
        e -> fromTextError $ "Failure parsing InstanceGroupType from value: '" <> e
           <> "'. Accepted values: core, master, task"

instance ToText InstanceGroupType where
    toText = \case
        IGTCore -> "core"
        IGTMaster -> "master"
        IGTTask -> "task"

instance Hashable     InstanceGroupType
instance ToByteString InstanceGroupType
instance ToQuery      InstanceGroupType
instance ToHeader     InstanceGroupType

instance ToJSON InstanceGroupType where
    toJSON = toJSONText

instance FromJSON InstanceGroupType where
    parseJSON = parseJSONText "InstanceGroupType"

data InstanceRoleType
    = Core
    | Master
    | Task
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText InstanceRoleType where
    parser = takeLowerText >>= \case
        "core" -> pure Core
        "master" -> pure Master
        "task" -> pure Task
        e -> fromTextError $ "Failure parsing InstanceRoleType from value: '" <> e
           <> "'. Accepted values: core, master, task"

instance ToText InstanceRoleType where
    toText = \case
        Core -> "core"
        Master -> "master"
        Task -> "task"

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
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
        ISAwaitingFulfillment -> "awaiting_fulfillment"
        ISBootstrapping -> "bootstrapping"
        ISProvisioning -> "provisioning"
        ISRunning -> "running"
        ISTerminated -> "terminated"

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
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
        ISCRCBootstrapFailure -> "bootstrap_failure"
        ISCRCClusterTerminated -> "cluster_terminated"
        ISCRCInstanceFailure -> "instance_failure"
        ISCRCInternalError -> "internal_error"
        ISCRCValidationError -> "validation_error"

instance Hashable     InstanceStateChangeReasonCode
instance ToByteString InstanceStateChangeReasonCode
instance ToQuery      InstanceStateChangeReasonCode
instance ToHeader     InstanceStateChangeReasonCode

instance FromJSON InstanceStateChangeReasonCode where
    parseJSON = parseJSONText "InstanceStateChangeReasonCode"

data MarketType
    = OnDemand
    | Spot
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText MarketType where
    parser = takeLowerText >>= \case
        "on_demand" -> pure OnDemand
        "spot" -> pure Spot
        e -> fromTextError $ "Failure parsing MarketType from value: '" <> e
           <> "'. Accepted values: on_demand, spot"

instance ToText MarketType where
    toText = \case
        OnDemand -> "on_demand"
        Spot -> "spot"

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
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StepState where
    parser = takeLowerText >>= \case
        "cancelled" -> pure SSCancelled
        "completed" -> pure SSCompleted
        "failed" -> pure SSFailed
        "interrupted" -> pure SSInterrupted
        "pending" -> pure SSPending
        "running" -> pure SSRunning
        e -> fromTextError $ "Failure parsing StepState from value: '" <> e
           <> "'. Accepted values: cancelled, completed, failed, interrupted, pending, running"

instance ToText StepState where
    toText = \case
        SSCancelled -> "cancelled"
        SSCompleted -> "completed"
        SSFailed -> "failed"
        SSInterrupted -> "interrupted"
        SSPending -> "pending"
        SSRunning -> "running"

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
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StepStateChangeReasonCode where
    parser = takeLowerText >>= \case
        "none" -> pure None
        e -> fromTextError $ "Failure parsing StepStateChangeReasonCode from value: '" <> e
           <> "'. Accepted values: none"

instance ToText StepStateChangeReasonCode where
    toText = \case
        None -> "none"

instance Hashable     StepStateChangeReasonCode
instance ToByteString StepStateChangeReasonCode
instance ToQuery      StepStateChangeReasonCode
instance ToHeader     StepStateChangeReasonCode

instance FromJSON StepStateChangeReasonCode where
    parseJSON = parseJSONText "StepStateChangeReasonCode"
