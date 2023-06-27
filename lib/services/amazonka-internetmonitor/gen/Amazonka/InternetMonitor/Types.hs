{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.InternetMonitor.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.InternetMonitor.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _BadRequestException,
    _ConflictException,
    _InternalServerErrorException,
    _InternalServerException,
    _LimitExceededException,
    _NotFoundException,
    _ResourceNotFoundException,
    _ThrottlingException,
    _TooManyRequestsException,
    _ValidationException,

    -- * HealthEventImpactType
    HealthEventImpactType (..),

    -- * HealthEventStatus
    HealthEventStatus (..),

    -- * LogDeliveryStatus
    LogDeliveryStatus (..),

    -- * MonitorConfigState
    MonitorConfigState (..),

    -- * MonitorProcessingStatusCode
    MonitorProcessingStatusCode (..),

    -- * TriangulationEventType
    TriangulationEventType (..),

    -- * AvailabilityMeasurement
    AvailabilityMeasurement (..),
    newAvailabilityMeasurement,
    availabilityMeasurement_experienceScore,
    availabilityMeasurement_percentOfClientLocationImpacted,
    availabilityMeasurement_percentOfTotalTrafficImpacted,

    -- * HealthEvent
    HealthEvent (..),
    newHealthEvent,
    healthEvent_createdAt,
    healthEvent_endedAt,
    healthEvent_percentOfTotalTrafficImpacted,
    healthEvent_eventArn,
    healthEvent_eventId,
    healthEvent_startedAt,
    healthEvent_lastUpdatedAt,
    healthEvent_impactedLocations,
    healthEvent_status,
    healthEvent_impactType,

    -- * ImpactedLocation
    ImpactedLocation (..),
    newImpactedLocation,
    impactedLocation_causedBy,
    impactedLocation_city,
    impactedLocation_countryCode,
    impactedLocation_internetHealth,
    impactedLocation_latitude,
    impactedLocation_longitude,
    impactedLocation_metro,
    impactedLocation_serviceLocation,
    impactedLocation_subdivision,
    impactedLocation_subdivisionCode,
    impactedLocation_aSName,
    impactedLocation_aSNumber,
    impactedLocation_country,
    impactedLocation_status,

    -- * InternetHealth
    InternetHealth (..),
    newInternetHealth,
    internetHealth_availability,
    internetHealth_performance,

    -- * InternetMeasurementsLogDelivery
    InternetMeasurementsLogDelivery (..),
    newInternetMeasurementsLogDelivery,
    internetMeasurementsLogDelivery_s3Config,

    -- * Monitor
    Monitor (..),
    newMonitor,
    monitor_processingStatus,
    monitor_monitorName,
    monitor_monitorArn,
    monitor_status,

    -- * Network
    Network (..),
    newNetwork,
    network_aSName,
    network_aSNumber,

    -- * NetworkImpairment
    NetworkImpairment (..),
    newNetworkImpairment,
    networkImpairment_networks,
    networkImpairment_asPath,
    networkImpairment_networkEventType,

    -- * PerformanceMeasurement
    PerformanceMeasurement (..),
    newPerformanceMeasurement,
    performanceMeasurement_experienceScore,
    performanceMeasurement_percentOfClientLocationImpacted,
    performanceMeasurement_percentOfTotalTrafficImpacted,
    performanceMeasurement_roundTripTime,

    -- * RoundTripTime
    RoundTripTime (..),
    newRoundTripTime,
    roundTripTime_p50,
    roundTripTime_p90,
    roundTripTime_p95,

    -- * S3Config
    S3Config (..),
    newS3Config,
    s3Config_bucketName,
    s3Config_bucketPrefix,
    s3Config_logDeliveryStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.InternetMonitor.Types.AvailabilityMeasurement
import Amazonka.InternetMonitor.Types.HealthEvent
import Amazonka.InternetMonitor.Types.HealthEventImpactType
import Amazonka.InternetMonitor.Types.HealthEventStatus
import Amazonka.InternetMonitor.Types.ImpactedLocation
import Amazonka.InternetMonitor.Types.InternetHealth
import Amazonka.InternetMonitor.Types.InternetMeasurementsLogDelivery
import Amazonka.InternetMonitor.Types.LogDeliveryStatus
import Amazonka.InternetMonitor.Types.Monitor
import Amazonka.InternetMonitor.Types.MonitorConfigState
import Amazonka.InternetMonitor.Types.MonitorProcessingStatusCode
import Amazonka.InternetMonitor.Types.Network
import Amazonka.InternetMonitor.Types.NetworkImpairment
import Amazonka.InternetMonitor.Types.PerformanceMeasurement
import Amazonka.InternetMonitor.Types.RoundTripTime
import Amazonka.InternetMonitor.Types.S3Config
import Amazonka.InternetMonitor.Types.TriangulationEventType
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-06-03@ of the Amazon CloudWatch Internet Monitor SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "InternetMonitor",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "internetmonitor",
      Core.signingName = "internetmonitor",
      Core.version = "2021-06-03",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "InternetMonitor",
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

-- | You don\'t have sufficient permission to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | A bad request was received.
_BadRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | The requested resource is in use.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | There was an internal server error.
_InternalServerErrorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | An internal error occurred.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The request exceeded a service quota.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 403

-- | The request specifies something that doesn\'t exist.
_NotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request specifies a resource that doesn\'t exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request was denied due to request throttling.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | There were too many requests.
_TooManyRequestsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | Invalid request.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
