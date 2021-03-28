{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.EventCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.EventCode
  ( EventCode
    ( EventCode'
    , EventCodeGenericEvent
    , EventCodeFleetCreated
    , EventCodeFleetDeleted
    , EventCodeFleetScalingEvent
    , EventCodeFleetStateDownloading
    , EventCodeFleetStateValidating
    , EventCodeFleetStateBuilding
    , EventCodeFleetStateActivating
    , EventCodeFleetStateActive
    , EventCodeFleetStateError
    , EventCodeFleetInitializationFailed
    , EventCodeFleetBinaryDownloadFailed
    , EventCodeFleetValidationLaunchPathNotFound
    , EventCodeFleetValidationExecutableRuntimeFailure
    , EventCodeFleetValidationTimedOut
    , EventCodeFleetActivationFailed
    , EventCodeFleetActivationFailedNoInstances
    , EventCodeFleetNewGameSessionProtectionPolicyUpdated
    , EventCodeServerProcessInvalidPath
    , EventCodeServerProcessSdkInitializationTimeout
    , EventCodeServerProcessProcessReadyTimeout
    , EventCodeServerProcessCrashed
    , EventCodeServerProcessTerminatedUnhealthy
    , EventCodeServerProcessForceTerminated
    , EventCodeServerProcessProcessExitTimeout
    , EventCodeGameSessionActivationTimeout
    , EventCodeFleetCreationExtractingBuild
    , EventCodeFleetCreationRunningInstaller
    , EventCodeFleetCreationValidatingRuntimeConfig
    , EventCodeFleetVpcPeeringSucceeded
    , EventCodeFleetVpcPeeringFailed
    , EventCodeFleetVpcPeeringDeleted
    , EventCodeInstanceInterrupted
    , fromEventCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype EventCode = EventCode'{fromEventCode :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern EventCodeGenericEvent :: EventCode
pattern EventCodeGenericEvent = EventCode' "GENERIC_EVENT"

pattern EventCodeFleetCreated :: EventCode
pattern EventCodeFleetCreated = EventCode' "FLEET_CREATED"

pattern EventCodeFleetDeleted :: EventCode
pattern EventCodeFleetDeleted = EventCode' "FLEET_DELETED"

pattern EventCodeFleetScalingEvent :: EventCode
pattern EventCodeFleetScalingEvent = EventCode' "FLEET_SCALING_EVENT"

pattern EventCodeFleetStateDownloading :: EventCode
pattern EventCodeFleetStateDownloading = EventCode' "FLEET_STATE_DOWNLOADING"

pattern EventCodeFleetStateValidating :: EventCode
pattern EventCodeFleetStateValidating = EventCode' "FLEET_STATE_VALIDATING"

pattern EventCodeFleetStateBuilding :: EventCode
pattern EventCodeFleetStateBuilding = EventCode' "FLEET_STATE_BUILDING"

pattern EventCodeFleetStateActivating :: EventCode
pattern EventCodeFleetStateActivating = EventCode' "FLEET_STATE_ACTIVATING"

pattern EventCodeFleetStateActive :: EventCode
pattern EventCodeFleetStateActive = EventCode' "FLEET_STATE_ACTIVE"

pattern EventCodeFleetStateError :: EventCode
pattern EventCodeFleetStateError = EventCode' "FLEET_STATE_ERROR"

pattern EventCodeFleetInitializationFailed :: EventCode
pattern EventCodeFleetInitializationFailed = EventCode' "FLEET_INITIALIZATION_FAILED"

pattern EventCodeFleetBinaryDownloadFailed :: EventCode
pattern EventCodeFleetBinaryDownloadFailed = EventCode' "FLEET_BINARY_DOWNLOAD_FAILED"

pattern EventCodeFleetValidationLaunchPathNotFound :: EventCode
pattern EventCodeFleetValidationLaunchPathNotFound = EventCode' "FLEET_VALIDATION_LAUNCH_PATH_NOT_FOUND"

pattern EventCodeFleetValidationExecutableRuntimeFailure :: EventCode
pattern EventCodeFleetValidationExecutableRuntimeFailure = EventCode' "FLEET_VALIDATION_EXECUTABLE_RUNTIME_FAILURE"

pattern EventCodeFleetValidationTimedOut :: EventCode
pattern EventCodeFleetValidationTimedOut = EventCode' "FLEET_VALIDATION_TIMED_OUT"

pattern EventCodeFleetActivationFailed :: EventCode
pattern EventCodeFleetActivationFailed = EventCode' "FLEET_ACTIVATION_FAILED"

pattern EventCodeFleetActivationFailedNoInstances :: EventCode
pattern EventCodeFleetActivationFailedNoInstances = EventCode' "FLEET_ACTIVATION_FAILED_NO_INSTANCES"

pattern EventCodeFleetNewGameSessionProtectionPolicyUpdated :: EventCode
pattern EventCodeFleetNewGameSessionProtectionPolicyUpdated = EventCode' "FLEET_NEW_GAME_SESSION_PROTECTION_POLICY_UPDATED"

pattern EventCodeServerProcessInvalidPath :: EventCode
pattern EventCodeServerProcessInvalidPath = EventCode' "SERVER_PROCESS_INVALID_PATH"

pattern EventCodeServerProcessSdkInitializationTimeout :: EventCode
pattern EventCodeServerProcessSdkInitializationTimeout = EventCode' "SERVER_PROCESS_SDK_INITIALIZATION_TIMEOUT"

pattern EventCodeServerProcessProcessReadyTimeout :: EventCode
pattern EventCodeServerProcessProcessReadyTimeout = EventCode' "SERVER_PROCESS_PROCESS_READY_TIMEOUT"

pattern EventCodeServerProcessCrashed :: EventCode
pattern EventCodeServerProcessCrashed = EventCode' "SERVER_PROCESS_CRASHED"

pattern EventCodeServerProcessTerminatedUnhealthy :: EventCode
pattern EventCodeServerProcessTerminatedUnhealthy = EventCode' "SERVER_PROCESS_TERMINATED_UNHEALTHY"

pattern EventCodeServerProcessForceTerminated :: EventCode
pattern EventCodeServerProcessForceTerminated = EventCode' "SERVER_PROCESS_FORCE_TERMINATED"

pattern EventCodeServerProcessProcessExitTimeout :: EventCode
pattern EventCodeServerProcessProcessExitTimeout = EventCode' "SERVER_PROCESS_PROCESS_EXIT_TIMEOUT"

pattern EventCodeGameSessionActivationTimeout :: EventCode
pattern EventCodeGameSessionActivationTimeout = EventCode' "GAME_SESSION_ACTIVATION_TIMEOUT"

pattern EventCodeFleetCreationExtractingBuild :: EventCode
pattern EventCodeFleetCreationExtractingBuild = EventCode' "FLEET_CREATION_EXTRACTING_BUILD"

pattern EventCodeFleetCreationRunningInstaller :: EventCode
pattern EventCodeFleetCreationRunningInstaller = EventCode' "FLEET_CREATION_RUNNING_INSTALLER"

pattern EventCodeFleetCreationValidatingRuntimeConfig :: EventCode
pattern EventCodeFleetCreationValidatingRuntimeConfig = EventCode' "FLEET_CREATION_VALIDATING_RUNTIME_CONFIG"

pattern EventCodeFleetVpcPeeringSucceeded :: EventCode
pattern EventCodeFleetVpcPeeringSucceeded = EventCode' "FLEET_VPC_PEERING_SUCCEEDED"

pattern EventCodeFleetVpcPeeringFailed :: EventCode
pattern EventCodeFleetVpcPeeringFailed = EventCode' "FLEET_VPC_PEERING_FAILED"

pattern EventCodeFleetVpcPeeringDeleted :: EventCode
pattern EventCodeFleetVpcPeeringDeleted = EventCode' "FLEET_VPC_PEERING_DELETED"

pattern EventCodeInstanceInterrupted :: EventCode
pattern EventCodeInstanceInterrupted = EventCode' "INSTANCE_INTERRUPTED"

{-# COMPLETE 
  EventCodeGenericEvent,

  EventCodeFleetCreated,

  EventCodeFleetDeleted,

  EventCodeFleetScalingEvent,

  EventCodeFleetStateDownloading,

  EventCodeFleetStateValidating,

  EventCodeFleetStateBuilding,

  EventCodeFleetStateActivating,

  EventCodeFleetStateActive,

  EventCodeFleetStateError,

  EventCodeFleetInitializationFailed,

  EventCodeFleetBinaryDownloadFailed,

  EventCodeFleetValidationLaunchPathNotFound,

  EventCodeFleetValidationExecutableRuntimeFailure,

  EventCodeFleetValidationTimedOut,

  EventCodeFleetActivationFailed,

  EventCodeFleetActivationFailedNoInstances,

  EventCodeFleetNewGameSessionProtectionPolicyUpdated,

  EventCodeServerProcessInvalidPath,

  EventCodeServerProcessSdkInitializationTimeout,

  EventCodeServerProcessProcessReadyTimeout,

  EventCodeServerProcessCrashed,

  EventCodeServerProcessTerminatedUnhealthy,

  EventCodeServerProcessForceTerminated,

  EventCodeServerProcessProcessExitTimeout,

  EventCodeGameSessionActivationTimeout,

  EventCodeFleetCreationExtractingBuild,

  EventCodeFleetCreationRunningInstaller,

  EventCodeFleetCreationValidatingRuntimeConfig,

  EventCodeFleetVpcPeeringSucceeded,

  EventCodeFleetVpcPeeringFailed,

  EventCodeFleetVpcPeeringDeleted,

  EventCodeInstanceInterrupted,
  EventCode'
  #-}
