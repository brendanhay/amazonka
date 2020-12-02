{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.EventCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.EventCode where

import Network.AWS.Prelude

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
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText EventCode where
  parser =
    takeLowerText >>= \case
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
      e ->
        fromTextError $
          "Failure parsing EventCode from value: '" <> e
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

instance Hashable EventCode

instance NFData EventCode

instance ToByteString EventCode

instance ToQuery EventCode

instance ToHeader EventCode

instance FromJSON EventCode where
  parseJSON = parseJSONText "EventCode"
