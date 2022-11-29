{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.BackupGateway.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BackupGateway.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ConflictException,
    _ThrottlingException,
    _ValidationException,

    -- * GatewayType
    GatewayType (..),

    -- * HypervisorState
    HypervisorState (..),

    -- * Gateway
    Gateway (..),
    newGateway,
    gateway_gatewayType,
    gateway_gatewayArn,
    gateway_hypervisorId,
    gateway_lastSeenTime,
    gateway_gatewayDisplayName,

    -- * GatewayDetails
    GatewayDetails (..),
    newGatewayDetails,
    gatewayDetails_gatewayType,
    gatewayDetails_gatewayArn,
    gatewayDetails_nextUpdateAvailabilityTime,
    gatewayDetails_maintenanceStartTime,
    gatewayDetails_hypervisorId,
    gatewayDetails_lastSeenTime,
    gatewayDetails_gatewayDisplayName,
    gatewayDetails_vpcEndpoint,

    -- * Hypervisor
    Hypervisor (..),
    newHypervisor,
    hypervisor_name,
    hypervisor_host,
    hypervisor_state,
    hypervisor_kmsKeyArn,
    hypervisor_hypervisorArn,

    -- * MaintenanceStartTime
    MaintenanceStartTime (..),
    newMaintenanceStartTime,
    maintenanceStartTime_dayOfWeek,
    maintenanceStartTime_dayOfMonth,
    maintenanceStartTime_hourOfDay,
    maintenanceStartTime_minuteOfHour,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * VirtualMachine
    VirtualMachine (..),
    newVirtualMachine,
    virtualMachine_name,
    virtualMachine_path,
    virtualMachine_hostName,
    virtualMachine_resourceArn,
    virtualMachine_hypervisorId,
    virtualMachine_lastBackupDate,

    -- * VirtualMachineDetails
    VirtualMachineDetails (..),
    newVirtualMachineDetails,
    virtualMachineDetails_name,
    virtualMachineDetails_path,
    virtualMachineDetails_hostName,
    virtualMachineDetails_resourceArn,
    virtualMachineDetails_hypervisorId,
    virtualMachineDetails_lastBackupDate,
  )
where

import Amazonka.BackupGateway.Types.Gateway
import Amazonka.BackupGateway.Types.GatewayDetails
import Amazonka.BackupGateway.Types.GatewayType
import Amazonka.BackupGateway.Types.Hypervisor
import Amazonka.BackupGateway.Types.HypervisorState
import Amazonka.BackupGateway.Types.MaintenanceStartTime
import Amazonka.BackupGateway.Types.Tag
import Amazonka.BackupGateway.Types.VirtualMachine
import Amazonka.BackupGateway.Types.VirtualMachineDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-01-01@ of the Amazon Backup Gateway SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "BackupGateway",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "backup-gateway",
      Core.signingName = "backup-gateway",
      Core.version = "2021-01-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "BackupGateway",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The operation cannot proceed because you have insufficient permissions.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The operation did not succeed because an internal error occurred. Try
-- again later.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | A resource that is required for the action wasn\'t found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The operation cannot proceed because it is not supported.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | TPS has been limited to protect against intentional or unintentional
-- high request volumes.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The operation did not succeed because a validation error occurred.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
