{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Rum.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rum.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * CustomEventsStatus
    CustomEventsStatus (..),

    -- * MetricDestination
    MetricDestination (..),

    -- * StateEnum
    StateEnum (..),

    -- * Telemetry
    Telemetry (..),

    -- * AppMonitor
    AppMonitor (..),
    newAppMonitor,
    appMonitor_appMonitorConfiguration,
    appMonitor_created,
    appMonitor_customEvents,
    appMonitor_dataStorage,
    appMonitor_domain,
    appMonitor_id,
    appMonitor_lastModified,
    appMonitor_name,
    appMonitor_state,
    appMonitor_tags,

    -- * AppMonitorConfiguration
    AppMonitorConfiguration (..),
    newAppMonitorConfiguration,
    appMonitorConfiguration_allowCookies,
    appMonitorConfiguration_enableXRay,
    appMonitorConfiguration_excludedPages,
    appMonitorConfiguration_favoritePages,
    appMonitorConfiguration_guestRoleArn,
    appMonitorConfiguration_identityPoolId,
    appMonitorConfiguration_includedPages,
    appMonitorConfiguration_sessionSampleRate,
    appMonitorConfiguration_telemetries,

    -- * AppMonitorDetails
    AppMonitorDetails (..),
    newAppMonitorDetails,
    appMonitorDetails_id,
    appMonitorDetails_name,
    appMonitorDetails_version,

    -- * AppMonitorSummary
    AppMonitorSummary (..),
    newAppMonitorSummary,
    appMonitorSummary_created,
    appMonitorSummary_id,
    appMonitorSummary_lastModified,
    appMonitorSummary_name,
    appMonitorSummary_state,

    -- * BatchCreateRumMetricDefinitionsError
    BatchCreateRumMetricDefinitionsError (..),
    newBatchCreateRumMetricDefinitionsError,
    batchCreateRumMetricDefinitionsError_errorCode,
    batchCreateRumMetricDefinitionsError_errorMessage,
    batchCreateRumMetricDefinitionsError_metricDefinition,

    -- * BatchDeleteRumMetricDefinitionsError
    BatchDeleteRumMetricDefinitionsError (..),
    newBatchDeleteRumMetricDefinitionsError,
    batchDeleteRumMetricDefinitionsError_errorCode,
    batchDeleteRumMetricDefinitionsError_errorMessage,
    batchDeleteRumMetricDefinitionsError_metricDefinitionId,

    -- * CustomEvents
    CustomEvents (..),
    newCustomEvents,
    customEvents_status,

    -- * CwLog
    CwLog (..),
    newCwLog,
    cwLog_cwLogEnabled,
    cwLog_cwLogGroup,

    -- * DataStorage
    DataStorage (..),
    newDataStorage,
    dataStorage_cwLog,

    -- * MetricDefinition
    MetricDefinition (..),
    newMetricDefinition,
    metricDefinition_dimensionKeys,
    metricDefinition_eventPattern,
    metricDefinition_unitLabel,
    metricDefinition_valueKey,
    metricDefinition_metricDefinitionId,
    metricDefinition_name,

    -- * MetricDefinitionRequest
    MetricDefinitionRequest (..),
    newMetricDefinitionRequest,
    metricDefinitionRequest_dimensionKeys,
    metricDefinitionRequest_eventPattern,
    metricDefinitionRequest_unitLabel,
    metricDefinitionRequest_valueKey,
    metricDefinitionRequest_name,

    -- * MetricDestinationSummary
    MetricDestinationSummary (..),
    newMetricDestinationSummary,
    metricDestinationSummary_destination,
    metricDestinationSummary_destinationArn,
    metricDestinationSummary_iamRoleArn,

    -- * QueryFilter
    QueryFilter (..),
    newQueryFilter,
    queryFilter_name,
    queryFilter_values,

    -- * RumEvent
    RumEvent (..),
    newRumEvent,
    rumEvent_metadata,
    rumEvent_details,
    rumEvent_id,
    rumEvent_timestamp,
    rumEvent_type,

    -- * TimeRange
    TimeRange (..),
    newTimeRange,
    timeRange_before,
    timeRange_after,

    -- * UserDetails
    UserDetails (..),
    newUserDetails,
    userDetails_sessionId,
    userDetails_userId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rum.Types.AppMonitor
import Amazonka.Rum.Types.AppMonitorConfiguration
import Amazonka.Rum.Types.AppMonitorDetails
import Amazonka.Rum.Types.AppMonitorSummary
import Amazonka.Rum.Types.BatchCreateRumMetricDefinitionsError
import Amazonka.Rum.Types.BatchDeleteRumMetricDefinitionsError
import Amazonka.Rum.Types.CustomEvents
import Amazonka.Rum.Types.CustomEventsStatus
import Amazonka.Rum.Types.CwLog
import Amazonka.Rum.Types.DataStorage
import Amazonka.Rum.Types.MetricDefinition
import Amazonka.Rum.Types.MetricDefinitionRequest
import Amazonka.Rum.Types.MetricDestination
import Amazonka.Rum.Types.MetricDestinationSummary
import Amazonka.Rum.Types.QueryFilter
import Amazonka.Rum.Types.RumEvent
import Amazonka.Rum.Types.StateEnum
import Amazonka.Rum.Types.Telemetry
import Amazonka.Rum.Types.TimeRange
import Amazonka.Rum.Types.UserDetails
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-05-10@ of the Amazon CloudWatch RUM SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Rum",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "rum",
      Core.signingName = "rum",
      Core.version = "2018-05-10",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Rum",
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

-- | You don\'t have sufficient permissions to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | This operation attempted to create a resource that already exists.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Internal service exception.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Resource not found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | This request exceeds a service quota.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was throttled because of quota limits.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | One of the arguments for the request is not valid.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
