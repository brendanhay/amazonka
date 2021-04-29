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

import Network.AWS.IoTJobsData.Types.JobExecution
import Network.AWS.IoTJobsData.Types.JobExecutionState
import Network.AWS.IoTJobsData.Types.JobExecutionStatus
import Network.AWS.IoTJobsData.Types.JobExecutionSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-09-29@ of the Amazon IoT Jobs Data Plane SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "IoTJobsData",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "data.jobs.iot",
      Prelude._svcVersion = "2017-09-29",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "IoTJobsData",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The job is in a terminal state.
_TerminalStateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TerminalStateException =
  Prelude._MatchServiceError
    defaultService
    "TerminalStateException"
    Prelude.. Prelude.hasStatus 410

-- | The certificate is invalid.
_CertificateValidationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CertificateValidationException =
  Prelude._MatchServiceError
    defaultService
    "CertificateValidationException"
    Prelude.. Prelude.hasStatus 400

-- | The service is temporarily unavailable.
_ServiceUnavailableException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServiceUnavailableException =
  Prelude._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Prelude.hasStatus 503

-- | The rate exceeds the limit.
_ThrottlingException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ThrottlingException =
  Prelude._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Prelude.hasStatus 429

-- | The contents of the request were invalid. For example, this code is
-- returned when an UpdateJobExecution request contains invalid status
-- details. The message contains details about the error.
_InvalidRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRequestException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Prelude.hasStatus 400

-- | An update attempted to change the job execution to a state that is
-- invalid because of the job execution\'s current state (for example, an
-- attempt to change a request in state SUCCESS to state IN_PROGRESS). In
-- this case, the body of the error message also contains the
-- executionState field.
_InvalidStateTransitionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidStateTransitionException =
  Prelude._MatchServiceError
    defaultService
    "InvalidStateTransitionException"
    Prelude.. Prelude.hasStatus 409

-- | The specified resource does not exist.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Prelude.hasStatus 404
