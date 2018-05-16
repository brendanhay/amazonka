{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTJobsData.Types
    (
    -- * Service Configuration
      ioTJobsData

    -- * Errors
    , _TerminalStateException
    , _InvalidRequestException
    , _CertificateValidationException
    , _ThrottlingException
    , _ServiceUnavailableException
    , _InvalidStateTransitionException
    , _ResourceNotFoundException

    -- * JobExecutionStatus
    , JobExecutionStatus (..)

    -- * JobExecution
    , JobExecution
    , jobExecution
    , jeStatus
    , jeJobId
    , jeLastUpdatedAt
    , jeQueuedAt
    , jeJobDocument
    , jeStatusDetails
    , jeExecutionNumber
    , jeVersionNumber
    , jeStartedAt
    , jeThingName

    -- * JobExecutionState
    , JobExecutionState
    , jobExecutionState
    , jesStatus
    , jesStatusDetails
    , jesVersionNumber

    -- * JobExecutionSummary
    , JobExecutionSummary
    , jobExecutionSummary
    , jJobId
    , jLastUpdatedAt
    , jQueuedAt
    , jExecutionNumber
    , jVersionNumber
    , jStartedAt
    ) where

import Network.AWS.IoTJobsData.Types.Product
import Network.AWS.IoTJobsData.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-09-29@ of the Amazon IoT Jobs Data Plane SDK configuration.
ioTJobsData :: Service
ioTJobsData =
  Service
    { _svcAbbrev = "IoTJobsData"
    , _svcSigner = v4
    , _svcPrefix = "data.jobs.iot"
    , _svcVersion = "2017-09-29"
    , _svcEndpoint = defaultEndpoint ioTJobsData
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "IoTJobsData"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The job is in a terminal state.
--
--
_TerminalStateException :: AsError a => Getting (First ServiceError) a ServiceError
_TerminalStateException =
  _MatchServiceError ioTJobsData "TerminalStateException" . hasStatus 410


-- | The contents of the request were invalid. For example, this code is returned when an UpdateJobExecution request contains invalid status details. The message contains details about the error.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException =
  _MatchServiceError ioTJobsData "InvalidRequestException" . hasStatus 400


-- | The certificate is invalid.
--
--
_CertificateValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_CertificateValidationException =
  _MatchServiceError ioTJobsData "CertificateValidationException" .
  hasStatus 400


-- | The rate exceeds the limit.
--
--
_ThrottlingException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottlingException =
  _MatchServiceError ioTJobsData "ThrottlingException" . hasStatus 429


-- | The service is temporarily unavailable.
--
--
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
  _MatchServiceError ioTJobsData "ServiceUnavailableException" . hasStatus 503


-- | An update attempted to change the job execution to a state that is invalid because of the job execution's current state (for example, an attempt to change a request in state SUCCESS to state IN_PROGRESS). In this case, the body of the error message also contains the executionState field.
--
--
_InvalidStateTransitionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidStateTransitionException =
  _MatchServiceError ioTJobsData "InvalidStateTransitionException" .
  hasStatus 409


-- | The specified resource does not exist.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError ioTJobsData "ResourceNotFoundException" . hasStatus 404

