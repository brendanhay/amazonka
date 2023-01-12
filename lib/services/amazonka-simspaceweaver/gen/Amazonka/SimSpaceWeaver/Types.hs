{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SimSpaceWeaver.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _TooManyTagsException,
    _ValidationException,

    -- * ClockStatus
    ClockStatus (..),

    -- * ClockTargetStatus
    ClockTargetStatus (..),

    -- * LifecycleManagementStrategy
    LifecycleManagementStrategy (..),

    -- * SimulationAppStatus
    SimulationAppStatus (..),

    -- * SimulationAppTargetStatus
    SimulationAppTargetStatus (..),

    -- * SimulationStatus
    SimulationStatus (..),

    -- * SimulationTargetStatus
    SimulationTargetStatus (..),

    -- * CloudWatchLogsLogGroup
    CloudWatchLogsLogGroup (..),
    newCloudWatchLogsLogGroup,
    cloudWatchLogsLogGroup_logGroupArn,

    -- * Domain
    Domain (..),
    newDomain,
    domain_lifecycle,
    domain_name,

    -- * LaunchOverrides
    LaunchOverrides (..),
    newLaunchOverrides,
    launchOverrides_launchCommands,

    -- * LiveSimulationState
    LiveSimulationState (..),
    newLiveSimulationState,
    liveSimulationState_clocks,
    liveSimulationState_domains,

    -- * LogDestination
    LogDestination (..),
    newLogDestination,
    logDestination_cloudWatchLogsLogGroup,

    -- * LoggingConfiguration
    LoggingConfiguration (..),
    newLoggingConfiguration,
    loggingConfiguration_destinations,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_bucketName,
    s3Location_objectKey,

    -- * SimulationAppEndpointInfo
    SimulationAppEndpointInfo (..),
    newSimulationAppEndpointInfo,
    simulationAppEndpointInfo_address,
    simulationAppEndpointInfo_ingressPortMappings,

    -- * SimulationAppMetadata
    SimulationAppMetadata (..),
    newSimulationAppMetadata,
    simulationAppMetadata_domain,
    simulationAppMetadata_name,
    simulationAppMetadata_simulation,
    simulationAppMetadata_status,
    simulationAppMetadata_targetStatus,

    -- * SimulationAppPortMapping
    SimulationAppPortMapping (..),
    newSimulationAppPortMapping,
    simulationAppPortMapping_actual,
    simulationAppPortMapping_declared,

    -- * SimulationClock
    SimulationClock (..),
    newSimulationClock,
    simulationClock_status,
    simulationClock_targetStatus,

    -- * SimulationMetadata
    SimulationMetadata (..),
    newSimulationMetadata,
    simulationMetadata_arn,
    simulationMetadata_creationTime,
    simulationMetadata_name,
    simulationMetadata_status,
    simulationMetadata_targetStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.SimSpaceWeaver.Types.ClockStatus
import Amazonka.SimSpaceWeaver.Types.ClockTargetStatus
import Amazonka.SimSpaceWeaver.Types.CloudWatchLogsLogGroup
import Amazonka.SimSpaceWeaver.Types.Domain
import Amazonka.SimSpaceWeaver.Types.LaunchOverrides
import Amazonka.SimSpaceWeaver.Types.LifecycleManagementStrategy
import Amazonka.SimSpaceWeaver.Types.LiveSimulationState
import Amazonka.SimSpaceWeaver.Types.LogDestination
import Amazonka.SimSpaceWeaver.Types.LoggingConfiguration
import Amazonka.SimSpaceWeaver.Types.S3Location
import Amazonka.SimSpaceWeaver.Types.SimulationAppEndpointInfo
import Amazonka.SimSpaceWeaver.Types.SimulationAppMetadata
import Amazonka.SimSpaceWeaver.Types.SimulationAppPortMapping
import Amazonka.SimSpaceWeaver.Types.SimulationAppStatus
import Amazonka.SimSpaceWeaver.Types.SimulationAppTargetStatus
import Amazonka.SimSpaceWeaver.Types.SimulationClock
import Amazonka.SimSpaceWeaver.Types.SimulationMetadata
import Amazonka.SimSpaceWeaver.Types.SimulationStatus
import Amazonka.SimSpaceWeaver.Types.SimulationTargetStatus

-- | API version @2022-10-28@ of the Amazon SimSpace Weaver SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "SimSpaceWeaver",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "simspaceweaver",
      Core.signingName = "simspaceweaver",
      Core.version = "2022-10-28",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "SimSpaceWeaver",
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
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- |
_AccessDeniedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- |
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- |
_InternalServerException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- |
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- |
_ServiceQuotaExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- |
_TooManyTagsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"
    Prelude.. Core.hasStatus 400

-- |
_ValidationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
