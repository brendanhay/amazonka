{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTJobsData.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTJobsData.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _CertificateValidationException,
    _InvalidRequestException,
    _InvalidStateTransitionException,
    _ResourceNotFoundException,
    _ServiceUnavailableException,
    _TerminalStateException,
    _ThrottlingException,

    -- * JobExecutionStatus
    JobExecutionStatus (..),

    -- * JobExecution
    JobExecution (..),
    newJobExecution,
    jobExecution_approximateSecondsBeforeTimedOut,
    jobExecution_executionNumber,
    jobExecution_jobDocument,
    jobExecution_jobId,
    jobExecution_lastUpdatedAt,
    jobExecution_queuedAt,
    jobExecution_startedAt,
    jobExecution_status,
    jobExecution_statusDetails,
    jobExecution_thingName,
    jobExecution_versionNumber,

    -- * JobExecutionState
    JobExecutionState (..),
    newJobExecutionState,
    jobExecutionState_status,
    jobExecutionState_statusDetails,
    jobExecutionState_versionNumber,

    -- * JobExecutionSummary
    JobExecutionSummary (..),
    newJobExecutionSummary,
    jobExecutionSummary_executionNumber,
    jobExecutionSummary_jobId,
    jobExecutionSummary_lastUpdatedAt,
    jobExecutionSummary_queuedAt,
    jobExecutionSummary_startedAt,
    jobExecutionSummary_versionNumber,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTJobsData.Types.JobExecution
import Amazonka.IoTJobsData.Types.JobExecutionState
import Amazonka.IoTJobsData.Types.JobExecutionStatus
import Amazonka.IoTJobsData.Types.JobExecutionSummary
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-09-29@ of the Amazon IoT Jobs Data Plane SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "IoTJobsData",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "data.jobs.iot",
      Core.signingName = "iot-jobs-data",
      Core.version = "2017-09-29",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "IoTJobsData",
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

-- | The certificate is invalid.
_CertificateValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CertificateValidationException =
  Core._MatchServiceError
    defaultService
    "CertificateValidationException"
    Prelude.. Core.hasStatus 400

-- | The contents of the request were invalid. For example, this code is
-- returned when an UpdateJobExecution request contains invalid status
-- details. The message contains details about the error.
_InvalidRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | An update attempted to change the job execution to a state that is
-- invalid because of the job execution\'s current state (for example, an
-- attempt to change a request in state SUCCESS to state IN_PROGRESS). In
-- this case, the body of the error message also contains the
-- executionState field.
_InvalidStateTransitionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidStateTransitionException =
  Core._MatchServiceError
    defaultService
    "InvalidStateTransitionException"
    Prelude.. Core.hasStatus 409

-- | The specified resource does not exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The service is temporarily unavailable.
_ServiceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The job is in a terminal state.
_TerminalStateException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TerminalStateException =
  Core._MatchServiceError
    defaultService
    "TerminalStateException"
    Prelude.. Core.hasStatus 410

-- | The rate exceeds the limit.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429
