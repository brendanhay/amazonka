{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTJobsData.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _TerminalStateException,
    _CertificateValidationException,
    _ServiceUnavailableException,
    _ThrottlingException,
    _InvalidRequestException,
    _InvalidStateTransitionException,
    _ResourceNotFoundException,

    -- * JobExecutionStatus
    JobExecutionStatus (..),

    -- * JobExecution
    JobExecution (..),
    newJobExecution,
    jobExecution_startedAt,
    jobExecution_status,
    jobExecution_statusDetails,
    jobExecution_thingName,
    jobExecution_queuedAt,
    jobExecution_versionNumber,
    jobExecution_executionNumber,
    jobExecution_jobDocument,
    jobExecution_approximateSecondsBeforeTimedOut,
    jobExecution_lastUpdatedAt,
    jobExecution_jobId,

    -- * JobExecutionState
    JobExecutionState (..),
    newJobExecutionState,
    jobExecutionState_status,
    jobExecutionState_statusDetails,
    jobExecutionState_versionNumber,

    -- * JobExecutionSummary
    JobExecutionSummary (..),
    newJobExecutionSummary,
    jobExecutionSummary_startedAt,
    jobExecutionSummary_queuedAt,
    jobExecutionSummary_versionNumber,
    jobExecutionSummary_executionNumber,
    jobExecutionSummary_lastUpdatedAt,
    jobExecutionSummary_jobId,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTJobsData.Types.JobExecution
import Network.AWS.IoTJobsData.Types.JobExecutionState
import Network.AWS.IoTJobsData.Types.JobExecutionStatus
import Network.AWS.IoTJobsData.Types.JobExecutionSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-09-29@ of the Amazon IoT Jobs Data Plane SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "IoTJobsData",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "data.jobs.iot",
      Core._serviceSigningName = "iot-jobs-data",
      Core._serviceVersion = "2017-09-29",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "IoTJobsData",
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
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | The job is in a terminal state.
_TerminalStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TerminalStateException =
  Core._MatchServiceError
    defaultService
    "TerminalStateException"
    Core.. Core.hasStatus 410

-- | The certificate is invalid.
_CertificateValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CertificateValidationException =
  Core._MatchServiceError
    defaultService
    "CertificateValidationException"
    Core.. Core.hasStatus 400

-- | The service is temporarily unavailable.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Core.. Core.hasStatus 503

-- | The rate exceeds the limit.
_ThrottlingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Core.. Core.hasStatus 429

-- | The contents of the request were invalid. For example, this code is
-- returned when an UpdateJobExecution request contains invalid status
-- details. The message contains details about the error.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Core.. Core.hasStatus 400

-- | An update attempted to change the job execution to a state that is
-- invalid because of the job execution\'s current state (for example, an
-- attempt to change a request in state SUCCESS to state IN_PROGRESS). In
-- this case, the body of the error message also contains the
-- executionState field.
_InvalidStateTransitionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidStateTransitionException =
  Core._MatchServiceError
    defaultService
    "InvalidStateTransitionException"
    Core.. Core.hasStatus 409

-- | The specified resource does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Core.. Core.hasStatus 404
