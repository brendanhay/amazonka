-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTJobsData.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _TerminalStateException
    , _InvalidRequestException
    , _CertificateValidationException
    , _ThrottlingException
    , _ServiceUnavailableException
    , _InvalidStateTransitionException
    , _ResourceNotFoundException

    -- * DescribeJobExecutionJobId
    , DescribeJobExecutionJobId (..)

    -- * JobExecutionState
    , JobExecutionState (..)
    , mkJobExecutionState
    , jesStatus
    , jesStatusDetails
    , jesVersionNumber

    -- * JobId
    , JobId (..)

    -- * DetailsKey
    , DetailsKey (..)

    -- * JobDocument
    , JobDocument (..)

    -- * JobExecutionSummary
    , JobExecutionSummary (..)
    , mkJobExecutionSummary
    , jExecutionNumber
    , jJobId
    , jLastUpdatedAt
    , jQueuedAt
    , jStartedAt
    , jVersionNumber

    -- * JobExecution
    , JobExecution (..)
    , mkJobExecution
    , jeApproximateSecondsBeforeTimedOut
    , jeExecutionNumber
    , jeJobDocument
    , jeJobId
    , jeLastUpdatedAt
    , jeQueuedAt
    , jeStartedAt
    , jeStatus
    , jeStatusDetails
    , jeThingName
    , jeVersionNumber

    -- * ThingName
    , ThingName (..)

    -- * DetailsValue
    , DetailsValue (..)

    -- * JobExecutionStatus
    , JobExecutionStatus (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
  
  
import Network.AWS.IoTJobsData.Types.DescribeJobExecutionJobId
  
import Network.AWS.IoTJobsData.Types.JobExecutionState
  
import Network.AWS.IoTJobsData.Types.JobId
  
import Network.AWS.IoTJobsData.Types.DetailsKey
  
import Network.AWS.IoTJobsData.Types.JobDocument
  
  
import Network.AWS.IoTJobsData.Types.JobExecutionSummary
  
  
import Network.AWS.IoTJobsData.Types.JobExecution
  
  
import Network.AWS.IoTJobsData.Types.ThingName
  
  
  
import Network.AWS.IoTJobsData.Types.DetailsValue
  
import Network.AWS.IoTJobsData.Types.JobExecutionStatus
  

-- | API version @2017-09-29@ of the Amazon IoT Jobs Data Plane SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "IoTJobsData",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "data.jobs.iot",
                 Core._svcVersion = "2017-09-29", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "IoTJobsData",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The job is in a terminal state.
_TerminalStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TerminalStateException
  = Core._MatchServiceError mkServiceConfig "TerminalStateException"
      Core.. Core.hasStatues 410
{-# INLINEABLE _TerminalStateException #-}
{-# DEPRECATED _TerminalStateException "Use generic-lens or generic-optics instead"  #-}

-- | The contents of the request were invalid. For example, this code is returned when an UpdateJobExecution request contains invalid status details. The message contains details about the error.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException
  = Core._MatchServiceError mkServiceConfig "InvalidRequestException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidRequestException #-}
{-# DEPRECATED _InvalidRequestException "Use generic-lens or generic-optics instead"  #-}

-- | The certificate is invalid.
_CertificateValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CertificateValidationException
  = Core._MatchServiceError mkServiceConfig
      "CertificateValidationException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CertificateValidationException #-}
{-# DEPRECATED _CertificateValidationException "Use generic-lens or generic-optics instead"  #-}

-- | The rate exceeds the limit.
_ThrottlingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThrottlingException
  = Core._MatchServiceError mkServiceConfig "ThrottlingException"
      Core.. Core.hasStatues 429
{-# INLINEABLE _ThrottlingException #-}
{-# DEPRECATED _ThrottlingException "Use generic-lens or generic-optics instead"  #-}

-- | The service is temporarily unavailable.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException
  = Core._MatchServiceError mkServiceConfig
      "ServiceUnavailableException"
      Core.. Core.hasStatues 503
{-# INLINEABLE _ServiceUnavailableException #-}
{-# DEPRECATED _ServiceUnavailableException "Use generic-lens or generic-optics instead"  #-}

-- | An update attempted to change the job execution to a state that is invalid because of the job execution's current state (for example, an attempt to change a request in state SUCCESS to state IN_PROGRESS). In this case, the body of the error message also contains the executionState field.
_InvalidStateTransitionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidStateTransitionException
  = Core._MatchServiceError mkServiceConfig
      "InvalidStateTransitionException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _InvalidStateTransitionException #-}
{-# DEPRECATED _InvalidStateTransitionException "Use generic-lens or generic-optics instead"  #-}

-- | The specified resource does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}
