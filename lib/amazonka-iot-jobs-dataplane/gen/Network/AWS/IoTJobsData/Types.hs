-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTJobsData.Types
  ( -- * Service configuration
    ioTJobsDataService,

    -- * Errors

    -- * JobExecutionStatus
    JobExecutionStatus (..),

    -- * JobExecution
    JobExecution (..),
    mkJobExecution,
    jeStatus,
    jeJobId,
    jeLastUpdatedAt,
    jeApproximateSecondsBeforeTimedOut,
    jeQueuedAt,
    jeJobDocument,
    jeStatusDetails,
    jeExecutionNumber,
    jeVersionNumber,
    jeStartedAt,
    jeThingName,

    -- * JobExecutionState
    JobExecutionState (..),
    mkJobExecutionState,
    jesStatus,
    jesStatusDetails,
    jesVersionNumber,

    -- * JobExecutionSummary
    JobExecutionSummary (..),
    mkJobExecutionSummary,
    jJobId,
    jLastUpdatedAt,
    jQueuedAt,
    jExecutionNumber,
    jVersionNumber,
    jStartedAt,
  )
where

import Network.AWS.IoTJobsData.Types.JobExecution
import Network.AWS.IoTJobsData.Types.JobExecutionState
import Network.AWS.IoTJobsData.Types.JobExecutionStatus
import Network.AWS.IoTJobsData.Types.JobExecutionSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-09-29@ of the Amazon IoT Jobs Data Plane SDK configuration.
ioTJobsDataService :: Lude.Service
ioTJobsDataService =
  Lude.Service
    { Lude._svcAbbrev = "IoTJobsData",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "data.jobs.iot",
      Lude._svcVersion = "2017-09-29",
      Lude._svcEndpoint = Lude.defaultEndpoint ioTJobsDataService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "IoTJobsData",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
