{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.Sum where

import Network.AWS.Prelude

data AcceptanceType
  = Accept
  | Reject
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AcceptanceType where
    parser = takeLowerText >>= \case
        "accept" -> pure Accept
        "reject" -> pure Reject
        e -> fromTextError $ "Failure parsing AcceptanceType from value: '" <> e
           <> "'. Accepted values: accept, reject"

instance ToText AcceptanceType where
    toText = \case
        Accept -> "ACCEPT"
        Reject -> "REJECT"

instance Hashable     AcceptanceType
instance NFData       AcceptanceType
instance ToByteString AcceptanceType
instance ToQuery      AcceptanceType
instance ToHeader     AcceptanceType

instance ToJSON AcceptanceType where
    toJSON = toJSONText

data BuildStatus
  = Failed
  | Initialized
  | Ready
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BuildStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "initialized" -> pure Initialized
        "ready" -> pure Ready
        e -> fromTextError $ "Failure parsing BuildStatus from value: '" <> e
           <> "'. Accepted values: failed, initialized, ready"

instance ToText BuildStatus where
    toText = \case
        Failed -> "FAILED"
        Initialized -> "INITIALIZED"
        Ready -> "READY"

instance Hashable     BuildStatus
instance NFData       BuildStatus
instance ToByteString BuildStatus
instance ToQuery      BuildStatus
instance ToHeader     BuildStatus

instance ToJSON BuildStatus where
    toJSON = toJSONText

instance FromJSON BuildStatus where
    parseJSON = parseJSONText "BuildStatus"

data ComparisonOperatorType
  = GreaterThanOrEqualToThreshold
  | GreaterThanThreshold
  | LessThanOrEqualToThreshold
  | LessThanThreshold
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ComparisonOperatorType where
    parser = takeLowerText >>= \case
        "greaterthanorequaltothreshold" -> pure GreaterThanOrEqualToThreshold
        "greaterthanthreshold" -> pure GreaterThanThreshold
        "lessthanorequaltothreshold" -> pure LessThanOrEqualToThreshold
        "lessthanthreshold" -> pure LessThanThreshold
        e -> fromTextError $ "Failure parsing ComparisonOperatorType from value: '" <> e
           <> "'. Accepted values: greaterthanorequaltothreshold, greaterthanthreshold, lessthanorequaltothreshold, lessthanthreshold"

instance ToText ComparisonOperatorType where
    toText = \case
        GreaterThanOrEqualToThreshold -> "GreaterThanOrEqualToThreshold"
        GreaterThanThreshold -> "GreaterThanThreshold"
        LessThanOrEqualToThreshold -> "LessThanOrEqualToThreshold"
        LessThanThreshold -> "LessThanThreshold"

instance Hashable     ComparisonOperatorType
instance NFData       ComparisonOperatorType
instance ToByteString ComparisonOperatorType
instance ToQuery      ComparisonOperatorType
instance ToHeader     ComparisonOperatorType

instance ToJSON ComparisonOperatorType where
    toJSON = toJSONText

instance FromJSON ComparisonOperatorType where
    parseJSON = parseJSONText "ComparisonOperatorType"

data EC2InstanceType
  = C3_2XLarge
  | C3_4XLarge
  | C3_8XLarge
  | C3_Large
  | C3_XLarge
  | C4_2XLarge
  | C4_4XLarge
  | C4_8XLarge
  | C4_Large
  | C4_XLarge
  | M3_2XLarge
  | M3_Large
  | M3_Medium
  | M3_XLarge
  | M4_10XLarge
  | M4_2XLarge
  | M4_4XLarge
  | M4_Large
  | M4_XLarge
  | R3_2XLarge
  | R3_4XLarge
  | R3_8XLarge
  | R3_Large
  | R3_XLarge
  | R4_16XLarge
  | R4_2XLarge
  | R4_4XLarge
  | R4_8XLarge
  | R4_Large
  | R4_XLarge
  | T2_Large
  | T2_Medium
  | T2_Micro
  | T2_Small
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EC2InstanceType where
    parser = takeLowerText >>= \case
        "c3.2xlarge" -> pure C3_2XLarge
        "c3.4xlarge" -> pure C3_4XLarge
        "c3.8xlarge" -> pure C3_8XLarge
        "c3.large" -> pure C3_Large
        "c3.xlarge" -> pure C3_XLarge
        "c4.2xlarge" -> pure C4_2XLarge
        "c4.4xlarge" -> pure C4_4XLarge
        "c4.8xlarge" -> pure C4_8XLarge
        "c4.large" -> pure C4_Large
        "c4.xlarge" -> pure C4_XLarge
        "m3.2xlarge" -> pure M3_2XLarge
        "m3.large" -> pure M3_Large
        "m3.medium" -> pure M3_Medium
        "m3.xlarge" -> pure M3_XLarge
        "m4.10xlarge" -> pure M4_10XLarge
        "m4.2xlarge" -> pure M4_2XLarge
        "m4.4xlarge" -> pure M4_4XLarge
        "m4.large" -> pure M4_Large
        "m4.xlarge" -> pure M4_XLarge
        "r3.2xlarge" -> pure R3_2XLarge
        "r3.4xlarge" -> pure R3_4XLarge
        "r3.8xlarge" -> pure R3_8XLarge
        "r3.large" -> pure R3_Large
        "r3.xlarge" -> pure R3_XLarge
        "r4.16xlarge" -> pure R4_16XLarge
        "r4.2xlarge" -> pure R4_2XLarge
        "r4.4xlarge" -> pure R4_4XLarge
        "r4.8xlarge" -> pure R4_8XLarge
        "r4.large" -> pure R4_Large
        "r4.xlarge" -> pure R4_XLarge
        "t2.large" -> pure T2_Large
        "t2.medium" -> pure T2_Medium
        "t2.micro" -> pure T2_Micro
        "t2.small" -> pure T2_Small
        e -> fromTextError $ "Failure parsing EC2InstanceType from value: '" <> e
           <> "'. Accepted values: c3.2xlarge, c3.4xlarge, c3.8xlarge, c3.large, c3.xlarge, c4.2xlarge, c4.4xlarge, c4.8xlarge, c4.large, c4.xlarge, m3.2xlarge, m3.large, m3.medium, m3.xlarge, m4.10xlarge, m4.2xlarge, m4.4xlarge, m4.large, m4.xlarge, r3.2xlarge, r3.4xlarge, r3.8xlarge, r3.large, r3.xlarge, r4.16xlarge, r4.2xlarge, r4.4xlarge, r4.8xlarge, r4.large, r4.xlarge, t2.large, t2.medium, t2.micro, t2.small"

instance ToText EC2InstanceType where
    toText = \case
        C3_2XLarge -> "c3.2xlarge"
        C3_4XLarge -> "c3.4xlarge"
        C3_8XLarge -> "c3.8xlarge"
        C3_Large -> "c3.large"
        C3_XLarge -> "c3.xlarge"
        C4_2XLarge -> "c4.2xlarge"
        C4_4XLarge -> "c4.4xlarge"
        C4_8XLarge -> "c4.8xlarge"
        C4_Large -> "c4.large"
        C4_XLarge -> "c4.xlarge"
        M3_2XLarge -> "m3.2xlarge"
        M3_Large -> "m3.large"
        M3_Medium -> "m3.medium"
        M3_XLarge -> "m3.xlarge"
        M4_10XLarge -> "m4.10xlarge"
        M4_2XLarge -> "m4.2xlarge"
        M4_4XLarge -> "m4.4xlarge"
        M4_Large -> "m4.large"
        M4_XLarge -> "m4.xlarge"
        R3_2XLarge -> "r3.2xlarge"
        R3_4XLarge -> "r3.4xlarge"
        R3_8XLarge -> "r3.8xlarge"
        R3_Large -> "r3.large"
        R3_XLarge -> "r3.xlarge"
        R4_16XLarge -> "r4.16xlarge"
        R4_2XLarge -> "r4.2xlarge"
        R4_4XLarge -> "r4.4xlarge"
        R4_8XLarge -> "r4.8xlarge"
        R4_Large -> "r4.large"
        R4_XLarge -> "r4.xlarge"
        T2_Large -> "t2.large"
        T2_Medium -> "t2.medium"
        T2_Micro -> "t2.micro"
        T2_Small -> "t2.small"

instance Hashable     EC2InstanceType
instance NFData       EC2InstanceType
instance ToByteString EC2InstanceType
instance ToQuery      EC2InstanceType
instance ToHeader     EC2InstanceType

instance ToJSON EC2InstanceType where
    toJSON = toJSONText

instance FromJSON EC2InstanceType where
    parseJSON = parseJSONText "EC2InstanceType"

data EventCode
  = FleetActivationFailed
  | FleetActivationFailedNoInstances
  | FleetBinaryDownloadFailed
  | FleetCreated
  | FleetCreationExtractingBuild
  | FleetCreationRunningInstaller
  | FleetCreationValidatingRuntimeConfig
  | FleetDeleted
  | FleetInitializationFailed
  | FleetNewGameSessionProtectionPolicyUpdated
  | FleetScalingEvent
  | FleetStateActivating
  | FleetStateActive
  | FleetStateBuilding
  | FleetStateDownloading
  | FleetStateError
  | FleetStateValidating
  | FleetVPCPeeringDeleted
  | FleetVPCPeeringFailed
  | FleetVPCPeeringSucceeded
  | FleetValidationExecutableRuntimeFailure
  | FleetValidationLaunchPathNotFound
  | FleetValidationTimedOut
  | GameSessionActivationTimeout
  | GenericEvent
  | InstanceInterrupted
  | ServerProcessCrashed
  | ServerProcessForceTerminated
  | ServerProcessInvalidPath
  | ServerProcessProcessExitTimeout
  | ServerProcessProcessReadyTimeout
  | ServerProcessSDKInitializationTimeout
  | ServerProcessTerminatedUnhealthy
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EventCode where
    parser = takeLowerText >>= \case
        "fleet_activation_failed" -> pure FleetActivationFailed
        "fleet_activation_failed_no_instances" -> pure FleetActivationFailedNoInstances
        "fleet_binary_download_failed" -> pure FleetBinaryDownloadFailed
        "fleet_created" -> pure FleetCreated
        "fleet_creation_extracting_build" -> pure FleetCreationExtractingBuild
        "fleet_creation_running_installer" -> pure FleetCreationRunningInstaller
        "fleet_creation_validating_runtime_config" -> pure FleetCreationValidatingRuntimeConfig
        "fleet_deleted" -> pure FleetDeleted
        "fleet_initialization_failed" -> pure FleetInitializationFailed
        "fleet_new_game_session_protection_policy_updated" -> pure FleetNewGameSessionProtectionPolicyUpdated
        "fleet_scaling_event" -> pure FleetScalingEvent
        "fleet_state_activating" -> pure FleetStateActivating
        "fleet_state_active" -> pure FleetStateActive
        "fleet_state_building" -> pure FleetStateBuilding
        "fleet_state_downloading" -> pure FleetStateDownloading
        "fleet_state_error" -> pure FleetStateError
        "fleet_state_validating" -> pure FleetStateValidating
        "fleet_vpc_peering_deleted" -> pure FleetVPCPeeringDeleted
        "fleet_vpc_peering_failed" -> pure FleetVPCPeeringFailed
        "fleet_vpc_peering_succeeded" -> pure FleetVPCPeeringSucceeded
        "fleet_validation_executable_runtime_failure" -> pure FleetValidationExecutableRuntimeFailure
        "fleet_validation_launch_path_not_found" -> pure FleetValidationLaunchPathNotFound
        "fleet_validation_timed_out" -> pure FleetValidationTimedOut
        "game_session_activation_timeout" -> pure GameSessionActivationTimeout
        "generic_event" -> pure GenericEvent
        "instance_interrupted" -> pure InstanceInterrupted
        "server_process_crashed" -> pure ServerProcessCrashed
        "server_process_force_terminated" -> pure ServerProcessForceTerminated
        "server_process_invalid_path" -> pure ServerProcessInvalidPath
        "server_process_process_exit_timeout" -> pure ServerProcessProcessExitTimeout
        "server_process_process_ready_timeout" -> pure ServerProcessProcessReadyTimeout
        "server_process_sdk_initialization_timeout" -> pure ServerProcessSDKInitializationTimeout
        "server_process_terminated_unhealthy" -> pure ServerProcessTerminatedUnhealthy
        e -> fromTextError $ "Failure parsing EventCode from value: '" <> e
           <> "'. Accepted values: fleet_activation_failed, fleet_activation_failed_no_instances, fleet_binary_download_failed, fleet_created, fleet_creation_extracting_build, fleet_creation_running_installer, fleet_creation_validating_runtime_config, fleet_deleted, fleet_initialization_failed, fleet_new_game_session_protection_policy_updated, fleet_scaling_event, fleet_state_activating, fleet_state_active, fleet_state_building, fleet_state_downloading, fleet_state_error, fleet_state_validating, fleet_vpc_peering_deleted, fleet_vpc_peering_failed, fleet_vpc_peering_succeeded, fleet_validation_executable_runtime_failure, fleet_validation_launch_path_not_found, fleet_validation_timed_out, game_session_activation_timeout, generic_event, instance_interrupted, server_process_crashed, server_process_force_terminated, server_process_invalid_path, server_process_process_exit_timeout, server_process_process_ready_timeout, server_process_sdk_initialization_timeout, server_process_terminated_unhealthy"

instance ToText EventCode where
    toText = \case
        FleetActivationFailed -> "FLEET_ACTIVATION_FAILED"
        FleetActivationFailedNoInstances -> "FLEET_ACTIVATION_FAILED_NO_INSTANCES"
        FleetBinaryDownloadFailed -> "FLEET_BINARY_DOWNLOAD_FAILED"
        FleetCreated -> "FLEET_CREATED"
        FleetCreationExtractingBuild -> "FLEET_CREATION_EXTRACTING_BUILD"
        FleetCreationRunningInstaller -> "FLEET_CREATION_RUNNING_INSTALLER"
        FleetCreationValidatingRuntimeConfig -> "FLEET_CREATION_VALIDATING_RUNTIME_CONFIG"
        FleetDeleted -> "FLEET_DELETED"
        FleetInitializationFailed -> "FLEET_INITIALIZATION_FAILED"
        FleetNewGameSessionProtectionPolicyUpdated -> "FLEET_NEW_GAME_SESSION_PROTECTION_POLICY_UPDATED"
        FleetScalingEvent -> "FLEET_SCALING_EVENT"
        FleetStateActivating -> "FLEET_STATE_ACTIVATING"
        FleetStateActive -> "FLEET_STATE_ACTIVE"
        FleetStateBuilding -> "FLEET_STATE_BUILDING"
        FleetStateDownloading -> "FLEET_STATE_DOWNLOADING"
        FleetStateError -> "FLEET_STATE_ERROR"
        FleetStateValidating -> "FLEET_STATE_VALIDATING"
        FleetVPCPeeringDeleted -> "FLEET_VPC_PEERING_DELETED"
        FleetVPCPeeringFailed -> "FLEET_VPC_PEERING_FAILED"
        FleetVPCPeeringSucceeded -> "FLEET_VPC_PEERING_SUCCEEDED"
        FleetValidationExecutableRuntimeFailure -> "FLEET_VALIDATION_EXECUTABLE_RUNTIME_FAILURE"
        FleetValidationLaunchPathNotFound -> "FLEET_VALIDATION_LAUNCH_PATH_NOT_FOUND"
        FleetValidationTimedOut -> "FLEET_VALIDATION_TIMED_OUT"
        GameSessionActivationTimeout -> "GAME_SESSION_ACTIVATION_TIMEOUT"
        GenericEvent -> "GENERIC_EVENT"
        InstanceInterrupted -> "INSTANCE_INTERRUPTED"
        ServerProcessCrashed -> "SERVER_PROCESS_CRASHED"
        ServerProcessForceTerminated -> "SERVER_PROCESS_FORCE_TERMINATED"
        ServerProcessInvalidPath -> "SERVER_PROCESS_INVALID_PATH"
        ServerProcessProcessExitTimeout -> "SERVER_PROCESS_PROCESS_EXIT_TIMEOUT"
        ServerProcessProcessReadyTimeout -> "SERVER_PROCESS_PROCESS_READY_TIMEOUT"
        ServerProcessSDKInitializationTimeout -> "SERVER_PROCESS_SDK_INITIALIZATION_TIMEOUT"
        ServerProcessTerminatedUnhealthy -> "SERVER_PROCESS_TERMINATED_UNHEALTHY"

instance Hashable     EventCode
instance NFData       EventCode
instance ToByteString EventCode
instance ToQuery      EventCode
instance ToHeader     EventCode

instance FromJSON EventCode where
    parseJSON = parseJSONText "EventCode"

data FleetAction =
  AutoScaling
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FleetAction where
    parser = takeLowerText >>= \case
        "auto_scaling" -> pure AutoScaling
        e -> fromTextError $ "Failure parsing FleetAction from value: '" <> e
           <> "'. Accepted values: auto_scaling"

instance ToText FleetAction where
    toText = \case
        AutoScaling -> "AUTO_SCALING"

instance Hashable     FleetAction
instance NFData       FleetAction
instance ToByteString FleetAction
instance ToQuery      FleetAction
instance ToHeader     FleetAction

instance ToJSON FleetAction where
    toJSON = toJSONText

instance FromJSON FleetAction where
    parseJSON = parseJSONText "FleetAction"

data FleetStatus
  = FSActivating
  | FSActive
  | FSBuilding
  | FSDeleting
  | FSDownloading
  | FSError'
  | FSNew
  | FSTerminated
  | FSValidating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FleetStatus where
    parser = takeLowerText >>= \case
        "activating" -> pure FSActivating
        "active" -> pure FSActive
        "building" -> pure FSBuilding
        "deleting" -> pure FSDeleting
        "downloading" -> pure FSDownloading
        "error" -> pure FSError'
        "new" -> pure FSNew
        "terminated" -> pure FSTerminated
        "validating" -> pure FSValidating
        e -> fromTextError $ "Failure parsing FleetStatus from value: '" <> e
           <> "'. Accepted values: activating, active, building, deleting, downloading, error, new, terminated, validating"

instance ToText FleetStatus where
    toText = \case
        FSActivating -> "ACTIVATING"
        FSActive -> "ACTIVE"
        FSBuilding -> "BUILDING"
        FSDeleting -> "DELETING"
        FSDownloading -> "DOWNLOADING"
        FSError' -> "ERROR"
        FSNew -> "NEW"
        FSTerminated -> "TERMINATED"
        FSValidating -> "VALIDATING"

instance Hashable     FleetStatus
instance NFData       FleetStatus
instance ToByteString FleetStatus
instance ToQuery      FleetStatus
instance ToHeader     FleetStatus

instance FromJSON FleetStatus where
    parseJSON = parseJSONText "FleetStatus"

data FleetType
  = OnDemand
  | Spot
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FleetType where
    parser = takeLowerText >>= \case
        "on_demand" -> pure OnDemand
        "spot" -> pure Spot
        e -> fromTextError $ "Failure parsing FleetType from value: '" <> e
           <> "'. Accepted values: on_demand, spot"

instance ToText FleetType where
    toText = \case
        OnDemand -> "ON_DEMAND"
        Spot -> "SPOT"

instance Hashable     FleetType
instance NFData       FleetType
instance ToByteString FleetType
instance ToQuery      FleetType
instance ToHeader     FleetType

instance ToJSON FleetType where
    toJSON = toJSONText

instance FromJSON FleetType where
    parseJSON = parseJSONText "FleetType"

data GameSessionPlacementState
  = Cancelled
  | Fulfilled
  | Pending
  | TimedOut
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText GameSessionPlacementState where
    parser = takeLowerText >>= \case
        "cancelled" -> pure Cancelled
        "fulfilled" -> pure Fulfilled
        "pending" -> pure Pending
        "timed_out" -> pure TimedOut
        e -> fromTextError $ "Failure parsing GameSessionPlacementState from value: '" <> e
           <> "'. Accepted values: cancelled, fulfilled, pending, timed_out"

instance ToText GameSessionPlacementState where
    toText = \case
        Cancelled -> "CANCELLED"
        Fulfilled -> "FULFILLED"
        Pending -> "PENDING"
        TimedOut -> "TIMED_OUT"

instance Hashable     GameSessionPlacementState
instance NFData       GameSessionPlacementState
instance ToByteString GameSessionPlacementState
instance ToQuery      GameSessionPlacementState
instance ToHeader     GameSessionPlacementState

instance FromJSON GameSessionPlacementState where
    parseJSON = parseJSONText "GameSessionPlacementState"

data GameSessionStatus
  = GSSActivating
  | GSSActive
  | GSSError'
  | GSSTerminated
  | GSSTerminating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText GameSessionStatus where
    parser = takeLowerText >>= \case
        "activating" -> pure GSSActivating
        "active" -> pure GSSActive
        "error" -> pure GSSError'
        "terminated" -> pure GSSTerminated
        "terminating" -> pure GSSTerminating
        e -> fromTextError $ "Failure parsing GameSessionStatus from value: '" <> e
           <> "'. Accepted values: activating, active, error, terminated, terminating"

instance ToText GameSessionStatus where
    toText = \case
        GSSActivating -> "ACTIVATING"
        GSSActive -> "ACTIVE"
        GSSError' -> "ERROR"
        GSSTerminated -> "TERMINATED"
        GSSTerminating -> "TERMINATING"

instance Hashable     GameSessionStatus
instance NFData       GameSessionStatus
instance ToByteString GameSessionStatus
instance ToQuery      GameSessionStatus
instance ToHeader     GameSessionStatus

instance FromJSON GameSessionStatus where
    parseJSON = parseJSONText "GameSessionStatus"

data GameSessionStatusReason =
  Interrupted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText GameSessionStatusReason where
    parser = takeLowerText >>= \case
        "interrupted" -> pure Interrupted
        e -> fromTextError $ "Failure parsing GameSessionStatusReason from value: '" <> e
           <> "'. Accepted values: interrupted"

instance ToText GameSessionStatusReason where
    toText = \case
        Interrupted -> "INTERRUPTED"

instance Hashable     GameSessionStatusReason
instance NFData       GameSessionStatusReason
instance ToByteString GameSessionStatusReason
instance ToQuery      GameSessionStatusReason
instance ToHeader     GameSessionStatusReason

instance FromJSON GameSessionStatusReason where
    parseJSON = parseJSONText "GameSessionStatusReason"

data IPProtocol
  = TCP
  | Udp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText IPProtocol where
    parser = takeLowerText >>= \case
        "tcp" -> pure TCP
        "udp" -> pure Udp
        e -> fromTextError $ "Failure parsing IPProtocol from value: '" <> e
           <> "'. Accepted values: tcp, udp"

instance ToText IPProtocol where
    toText = \case
        TCP -> "TCP"
        Udp -> "UDP"

instance Hashable     IPProtocol
instance NFData       IPProtocol
instance ToByteString IPProtocol
instance ToQuery      IPProtocol
instance ToHeader     IPProtocol

instance ToJSON IPProtocol where
    toJSON = toJSONText

instance FromJSON IPProtocol where
    parseJSON = parseJSONText "IPProtocol"

data InstanceStatus
  = ISActive
  | ISPending
  | ISTerminating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceStatus where
    parser = takeLowerText >>= \case
        "active" -> pure ISActive
        "pending" -> pure ISPending
        "terminating" -> pure ISTerminating
        e -> fromTextError $ "Failure parsing InstanceStatus from value: '" <> e
           <> "'. Accepted values: active, pending, terminating"

instance ToText InstanceStatus where
    toText = \case
        ISActive -> "ACTIVE"
        ISPending -> "PENDING"
        ISTerminating -> "TERMINATING"

instance Hashable     InstanceStatus
instance NFData       InstanceStatus
instance ToByteString InstanceStatus
instance ToQuery      InstanceStatus
instance ToHeader     InstanceStatus

instance FromJSON InstanceStatus where
    parseJSON = parseJSONText "InstanceStatus"

data MatchmakingConfigurationStatus
  = MCSCancelled
  | MCSCompleted
  | MCSFailed
  | MCSPlacing
  | MCSQueued
  | MCSRequiresAcceptance
  | MCSSearching
  | MCSTimedOut
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MatchmakingConfigurationStatus where
    parser = takeLowerText >>= \case
        "cancelled" -> pure MCSCancelled
        "completed" -> pure MCSCompleted
        "failed" -> pure MCSFailed
        "placing" -> pure MCSPlacing
        "queued" -> pure MCSQueued
        "requires_acceptance" -> pure MCSRequiresAcceptance
        "searching" -> pure MCSSearching
        "timed_out" -> pure MCSTimedOut
        e -> fromTextError $ "Failure parsing MatchmakingConfigurationStatus from value: '" <> e
           <> "'. Accepted values: cancelled, completed, failed, placing, queued, requires_acceptance, searching, timed_out"

instance ToText MatchmakingConfigurationStatus where
    toText = \case
        MCSCancelled -> "CANCELLED"
        MCSCompleted -> "COMPLETED"
        MCSFailed -> "FAILED"
        MCSPlacing -> "PLACING"
        MCSQueued -> "QUEUED"
        MCSRequiresAcceptance -> "REQUIRES_ACCEPTANCE"
        MCSSearching -> "SEARCHING"
        MCSTimedOut -> "TIMED_OUT"

instance Hashable     MatchmakingConfigurationStatus
instance NFData       MatchmakingConfigurationStatus
instance ToByteString MatchmakingConfigurationStatus
instance ToQuery      MatchmakingConfigurationStatus
instance ToHeader     MatchmakingConfigurationStatus

instance FromJSON MatchmakingConfigurationStatus where
    parseJSON = parseJSONText "MatchmakingConfigurationStatus"

data MetricName
  = ActivatingGameSessions
  | ActiveGameSessions
  | ActiveInstances
  | AvailableGameSessions
  | AvailablePlayerSessions
  | CurrentPlayerSessions
  | IdleInstances
  | PercentAvailableGameSessions
  | PercentIdleInstances
  | QueueDepth
  | WaitTime
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MetricName where
    parser = takeLowerText >>= \case
        "activatinggamesessions" -> pure ActivatingGameSessions
        "activegamesessions" -> pure ActiveGameSessions
        "activeinstances" -> pure ActiveInstances
        "availablegamesessions" -> pure AvailableGameSessions
        "availableplayersessions" -> pure AvailablePlayerSessions
        "currentplayersessions" -> pure CurrentPlayerSessions
        "idleinstances" -> pure IdleInstances
        "percentavailablegamesessions" -> pure PercentAvailableGameSessions
        "percentidleinstances" -> pure PercentIdleInstances
        "queuedepth" -> pure QueueDepth
        "waittime" -> pure WaitTime
        e -> fromTextError $ "Failure parsing MetricName from value: '" <> e
           <> "'. Accepted values: activatinggamesessions, activegamesessions, activeinstances, availablegamesessions, availableplayersessions, currentplayersessions, idleinstances, percentavailablegamesessions, percentidleinstances, queuedepth, waittime"

instance ToText MetricName where
    toText = \case
        ActivatingGameSessions -> "ActivatingGameSessions"
        ActiveGameSessions -> "ActiveGameSessions"
        ActiveInstances -> "ActiveInstances"
        AvailableGameSessions -> "AvailableGameSessions"
        AvailablePlayerSessions -> "AvailablePlayerSessions"
        CurrentPlayerSessions -> "CurrentPlayerSessions"
        IdleInstances -> "IdleInstances"
        PercentAvailableGameSessions -> "PercentAvailableGameSessions"
        PercentIdleInstances -> "PercentIdleInstances"
        QueueDepth -> "QueueDepth"
        WaitTime -> "WaitTime"

instance Hashable     MetricName
instance NFData       MetricName
instance ToByteString MetricName
instance ToQuery      MetricName
instance ToHeader     MetricName

instance ToJSON MetricName where
    toJSON = toJSONText

instance FromJSON MetricName where
    parseJSON = parseJSONText "MetricName"

data OperatingSystem
  = AmazonLinux
  | Windows2012
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OperatingSystem where
    parser = takeLowerText >>= \case
        "amazon_linux" -> pure AmazonLinux
        "windows_2012" -> pure Windows2012
        e -> fromTextError $ "Failure parsing OperatingSystem from value: '" <> e
           <> "'. Accepted values: amazon_linux, windows_2012"

instance ToText OperatingSystem where
    toText = \case
        AmazonLinux -> "AMAZON_LINUX"
        Windows2012 -> "WINDOWS_2012"

instance Hashable     OperatingSystem
instance NFData       OperatingSystem
instance ToByteString OperatingSystem
instance ToQuery      OperatingSystem
instance ToHeader     OperatingSystem

instance ToJSON OperatingSystem where
    toJSON = toJSONText

instance FromJSON OperatingSystem where
    parseJSON = parseJSONText "OperatingSystem"

data PlayerSessionCreationPolicy
  = AcceptAll
  | DenyAll
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PlayerSessionCreationPolicy where
    parser = takeLowerText >>= \case
        "accept_all" -> pure AcceptAll
        "deny_all" -> pure DenyAll
        e -> fromTextError $ "Failure parsing PlayerSessionCreationPolicy from value: '" <> e
           <> "'. Accepted values: accept_all, deny_all"

instance ToText PlayerSessionCreationPolicy where
    toText = \case
        AcceptAll -> "ACCEPT_ALL"
        DenyAll -> "DENY_ALL"

instance Hashable     PlayerSessionCreationPolicy
instance NFData       PlayerSessionCreationPolicy
instance ToByteString PlayerSessionCreationPolicy
instance ToQuery      PlayerSessionCreationPolicy
instance ToHeader     PlayerSessionCreationPolicy

instance ToJSON PlayerSessionCreationPolicy where
    toJSON = toJSONText

instance FromJSON PlayerSessionCreationPolicy where
    parseJSON = parseJSONText "PlayerSessionCreationPolicy"

data PlayerSessionStatus
  = PSSActive
  | PSSCompleted
  | PSSReserved
  | PSSTimedout
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PlayerSessionStatus where
    parser = takeLowerText >>= \case
        "active" -> pure PSSActive
        "completed" -> pure PSSCompleted
        "reserved" -> pure PSSReserved
        "timedout" -> pure PSSTimedout
        e -> fromTextError $ "Failure parsing PlayerSessionStatus from value: '" <> e
           <> "'. Accepted values: active, completed, reserved, timedout"

instance ToText PlayerSessionStatus where
    toText = \case
        PSSActive -> "ACTIVE"
        PSSCompleted -> "COMPLETED"
        PSSReserved -> "RESERVED"
        PSSTimedout -> "TIMEDOUT"

instance Hashable     PlayerSessionStatus
instance NFData       PlayerSessionStatus
instance ToByteString PlayerSessionStatus
instance ToQuery      PlayerSessionStatus
instance ToHeader     PlayerSessionStatus

instance FromJSON PlayerSessionStatus where
    parseJSON = parseJSONText "PlayerSessionStatus"

data PolicyType
  = RuleBased
  | TargetBased
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PolicyType where
    parser = takeLowerText >>= \case
        "rulebased" -> pure RuleBased
        "targetbased" -> pure TargetBased
        e -> fromTextError $ "Failure parsing PolicyType from value: '" <> e
           <> "'. Accepted values: rulebased, targetbased"

instance ToText PolicyType where
    toText = \case
        RuleBased -> "RuleBased"
        TargetBased -> "TargetBased"

instance Hashable     PolicyType
instance NFData       PolicyType
instance ToByteString PolicyType
instance ToQuery      PolicyType
instance ToHeader     PolicyType

instance ToJSON PolicyType where
    toJSON = toJSONText

instance FromJSON PolicyType where
    parseJSON = parseJSONText "PolicyType"

data ProtectionPolicy
  = FullProtection
  | NoProtection
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProtectionPolicy where
    parser = takeLowerText >>= \case
        "fullprotection" -> pure FullProtection
        "noprotection" -> pure NoProtection
        e -> fromTextError $ "Failure parsing ProtectionPolicy from value: '" <> e
           <> "'. Accepted values: fullprotection, noprotection"

instance ToText ProtectionPolicy where
    toText = \case
        FullProtection -> "FullProtection"
        NoProtection -> "NoProtection"

instance Hashable     ProtectionPolicy
instance NFData       ProtectionPolicy
instance ToByteString ProtectionPolicy
instance ToQuery      ProtectionPolicy
instance ToHeader     ProtectionPolicy

instance ToJSON ProtectionPolicy where
    toJSON = toJSONText

instance FromJSON ProtectionPolicy where
    parseJSON = parseJSONText "ProtectionPolicy"

data RoutingStrategyType
  = Simple
  | Terminal
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RoutingStrategyType where
    parser = takeLowerText >>= \case
        "simple" -> pure Simple
        "terminal" -> pure Terminal
        e -> fromTextError $ "Failure parsing RoutingStrategyType from value: '" <> e
           <> "'. Accepted values: simple, terminal"

instance ToText RoutingStrategyType where
    toText = \case
        Simple -> "SIMPLE"
        Terminal -> "TERMINAL"

instance Hashable     RoutingStrategyType
instance NFData       RoutingStrategyType
instance ToByteString RoutingStrategyType
instance ToQuery      RoutingStrategyType
instance ToHeader     RoutingStrategyType

instance ToJSON RoutingStrategyType where
    toJSON = toJSONText

instance FromJSON RoutingStrategyType where
    parseJSON = parseJSONText "RoutingStrategyType"

data ScalingAdjustmentType
  = ChangeInCapacity
  | ExactCapacity
  | PercentChangeInCapacity
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ScalingAdjustmentType where
    parser = takeLowerText >>= \case
        "changeincapacity" -> pure ChangeInCapacity
        "exactcapacity" -> pure ExactCapacity
        "percentchangeincapacity" -> pure PercentChangeInCapacity
        e -> fromTextError $ "Failure parsing ScalingAdjustmentType from value: '" <> e
           <> "'. Accepted values: changeincapacity, exactcapacity, percentchangeincapacity"

instance ToText ScalingAdjustmentType where
    toText = \case
        ChangeInCapacity -> "ChangeInCapacity"
        ExactCapacity -> "ExactCapacity"
        PercentChangeInCapacity -> "PercentChangeInCapacity"

instance Hashable     ScalingAdjustmentType
instance NFData       ScalingAdjustmentType
instance ToByteString ScalingAdjustmentType
instance ToQuery      ScalingAdjustmentType
instance ToHeader     ScalingAdjustmentType

instance ToJSON ScalingAdjustmentType where
    toJSON = toJSONText

instance FromJSON ScalingAdjustmentType where
    parseJSON = parseJSONText "ScalingAdjustmentType"

data ScalingStatusType
  = Active
  | DeleteRequested
  | Deleted
  | Deleting
  | Error'
  | UpdateRequested
  | Updating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ScalingStatusType where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "delete_requested" -> pure DeleteRequested
        "deleted" -> pure Deleted
        "deleting" -> pure Deleting
        "error" -> pure Error'
        "update_requested" -> pure UpdateRequested
        "updating" -> pure Updating
        e -> fromTextError $ "Failure parsing ScalingStatusType from value: '" <> e
           <> "'. Accepted values: active, delete_requested, deleted, deleting, error, update_requested, updating"

instance ToText ScalingStatusType where
    toText = \case
        Active -> "ACTIVE"
        DeleteRequested -> "DELETE_REQUESTED"
        Deleted -> "DELETED"
        Deleting -> "DELETING"
        Error' -> "ERROR"
        UpdateRequested -> "UPDATE_REQUESTED"
        Updating -> "UPDATING"

instance Hashable     ScalingStatusType
instance NFData       ScalingStatusType
instance ToByteString ScalingStatusType
instance ToQuery      ScalingStatusType
instance ToHeader     ScalingStatusType

instance ToJSON ScalingStatusType where
    toJSON = toJSONText

instance FromJSON ScalingStatusType where
    parseJSON = parseJSONText "ScalingStatusType"
