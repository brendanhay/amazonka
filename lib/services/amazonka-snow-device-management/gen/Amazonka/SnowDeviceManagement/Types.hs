{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SnowDeviceManagement.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SnowDeviceManagement.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * AttachmentStatus
    AttachmentStatus (..),

    -- * ExecutionState
    ExecutionState (..),

    -- * InstanceStateName
    InstanceStateName (..),

    -- * IpAddressAssignment
    IpAddressAssignment (..),

    -- * PhysicalConnectorType
    PhysicalConnectorType (..),

    -- * TaskState
    TaskState (..),

    -- * UnlockState
    UnlockState (..),

    -- * Capacity
    Capacity (..),
    newCapacity,
    capacity_used,
    capacity_name,
    capacity_total,
    capacity_unit,
    capacity_available,

    -- * Command
    Command (..),
    newCommand,
    command_unlock,
    command_reboot,

    -- * CpuOptions
    CpuOptions (..),
    newCpuOptions,
    cpuOptions_coreCount,
    cpuOptions_threadsPerCore,

    -- * DeviceSummary
    DeviceSummary (..),
    newDeviceSummary,
    deviceSummary_associatedWithJob,
    deviceSummary_managedDeviceId,
    deviceSummary_managedDeviceArn,
    deviceSummary_tags,

    -- * EbsInstanceBlockDevice
    EbsInstanceBlockDevice (..),
    newEbsInstanceBlockDevice,
    ebsInstanceBlockDevice_deleteOnTermination,
    ebsInstanceBlockDevice_status,
    ebsInstanceBlockDevice_volumeId,
    ebsInstanceBlockDevice_attachTime,

    -- * ExecutionSummary
    ExecutionSummary (..),
    newExecutionSummary,
    executionSummary_executionId,
    executionSummary_state,
    executionSummary_taskId,
    executionSummary_managedDeviceId,

    -- * Instance
    Instance (..),
    newInstance,
    instance_instanceId,
    instance_state,
    instance_securityGroups,
    instance_createdAt,
    instance_cpuOptions,
    instance_rootDeviceName,
    instance_instanceType,
    instance_imageId,
    instance_privateIpAddress,
    instance_updatedAt,
    instance_blockDeviceMappings,
    instance_publicIpAddress,
    instance_amiLaunchIndex,

    -- * InstanceBlockDeviceMapping
    InstanceBlockDeviceMapping (..),
    newInstanceBlockDeviceMapping,
    instanceBlockDeviceMapping_ebs,
    instanceBlockDeviceMapping_deviceName,

    -- * InstanceState
    InstanceState (..),
    newInstanceState,
    instanceState_name,
    instanceState_code,

    -- * InstanceSummary
    InstanceSummary (..),
    newInstanceSummary,
    instanceSummary_lastUpdatedAt,
    instanceSummary_instance,

    -- * PhysicalNetworkInterface
    PhysicalNetworkInterface (..),
    newPhysicalNetworkInterface,
    physicalNetworkInterface_ipAddress,
    physicalNetworkInterface_macAddress,
    physicalNetworkInterface_ipAddressAssignment,
    physicalNetworkInterface_defaultGateway,
    physicalNetworkInterface_physicalNetworkInterfaceId,
    physicalNetworkInterface_netmask,
    physicalNetworkInterface_physicalConnectorType,

    -- * Reboot
    Reboot (..),
    newReboot,

    -- * ResourceSummary
    ResourceSummary (..),
    newResourceSummary,
    resourceSummary_arn,
    resourceSummary_id,
    resourceSummary_resourceType,

    -- * SecurityGroupIdentifier
    SecurityGroupIdentifier (..),
    newSecurityGroupIdentifier,
    securityGroupIdentifier_groupId,
    securityGroupIdentifier_groupName,

    -- * SoftwareInformation
    SoftwareInformation (..),
    newSoftwareInformation,
    softwareInformation_installedVersion,
    softwareInformation_installState,
    softwareInformation_installingVersion,

    -- * TaskSummary
    TaskSummary (..),
    newTaskSummary,
    taskSummary_state,
    taskSummary_taskArn,
    taskSummary_tags,
    taskSummary_taskId,

    -- * Unlock
    Unlock (..),
    newUnlock,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.SnowDeviceManagement.Types.AttachmentStatus
import Amazonka.SnowDeviceManagement.Types.Capacity
import Amazonka.SnowDeviceManagement.Types.Command
import Amazonka.SnowDeviceManagement.Types.CpuOptions
import Amazonka.SnowDeviceManagement.Types.DeviceSummary
import Amazonka.SnowDeviceManagement.Types.EbsInstanceBlockDevice
import Amazonka.SnowDeviceManagement.Types.ExecutionState
import Amazonka.SnowDeviceManagement.Types.ExecutionSummary
import Amazonka.SnowDeviceManagement.Types.Instance
import Amazonka.SnowDeviceManagement.Types.InstanceBlockDeviceMapping
import Amazonka.SnowDeviceManagement.Types.InstanceState
import Amazonka.SnowDeviceManagement.Types.InstanceStateName
import Amazonka.SnowDeviceManagement.Types.InstanceSummary
import Amazonka.SnowDeviceManagement.Types.IpAddressAssignment
import Amazonka.SnowDeviceManagement.Types.PhysicalConnectorType
import Amazonka.SnowDeviceManagement.Types.PhysicalNetworkInterface
import Amazonka.SnowDeviceManagement.Types.Reboot
import Amazonka.SnowDeviceManagement.Types.ResourceSummary
import Amazonka.SnowDeviceManagement.Types.SecurityGroupIdentifier
import Amazonka.SnowDeviceManagement.Types.SoftwareInformation
import Amazonka.SnowDeviceManagement.Types.TaskState
import Amazonka.SnowDeviceManagement.Types.TaskSummary
import Amazonka.SnowDeviceManagement.Types.Unlock
import Amazonka.SnowDeviceManagement.Types.UnlockState

-- | API version @2021-08-04@ of the Amazon Snow Device Management SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "SnowDeviceManagement",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix =
        "snow-device-management",
      Core._serviceSigningName = "snow-device-management",
      Core._serviceVersion = "2021-08-04",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "SnowDeviceManagement",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The input fails to satisfy the constraints specified by an Amazon Web
-- Services service.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | You don\'t have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request would cause a service quota to be exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | An unexpected error occurred while processing the request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The request references a resource that doesn\'t exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
