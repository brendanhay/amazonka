{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ApplicationInsights.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationInsights.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _BadRequestException,
    _InternalServerException,
    _ResourceInUseException,
    _ResourceNotFoundException,
    _TagsAlreadyExistException,
    _TooManyTagsException,
    _ValidationException,

    -- * CloudWatchEventSource
    CloudWatchEventSource (..),

    -- * ConfigurationEventResourceType
    ConfigurationEventResourceType (..),

    -- * ConfigurationEventStatus
    ConfigurationEventStatus (..),

    -- * DiscoveryType
    DiscoveryType (..),

    -- * FeedbackKey
    FeedbackKey (..),

    -- * FeedbackValue
    FeedbackValue (..),

    -- * GroupingType
    GroupingType (..),

    -- * LogFilter
    LogFilter (..),

    -- * OsType
    OsType (..),

    -- * SeverityLevel
    SeverityLevel (..),

    -- * Status
    Status (..),

    -- * Tier
    Tier (..),

    -- * ApplicationComponent
    ApplicationComponent (..),
    newApplicationComponent,
    applicationComponent_componentName,
    applicationComponent_componentRemarks,
    applicationComponent_detectedWorkload,
    applicationComponent_monitor,
    applicationComponent_osType,
    applicationComponent_resourceType,
    applicationComponent_tier,

    -- * ApplicationInfo
    ApplicationInfo (..),
    newApplicationInfo,
    applicationInfo_autoConfigEnabled,
    applicationInfo_cWEMonitorEnabled,
    applicationInfo_discoveryType,
    applicationInfo_lifeCycle,
    applicationInfo_opsCenterEnabled,
    applicationInfo_opsItemSNSTopicArn,
    applicationInfo_remarks,
    applicationInfo_resourceGroupName,

    -- * ConfigurationEvent
    ConfigurationEvent (..),
    newConfigurationEvent,
    configurationEvent_eventDetail,
    configurationEvent_eventResourceName,
    configurationEvent_eventResourceType,
    configurationEvent_eventStatus,
    configurationEvent_eventTime,
    configurationEvent_monitoredResourceARN,

    -- * LogPattern
    LogPattern (..),
    newLogPattern,
    logPattern_pattern,
    logPattern_patternName,
    logPattern_patternSetName,
    logPattern_rank,

    -- * Observation
    Observation (..),
    newObservation,
    observation_cloudWatchEventDetailType,
    observation_cloudWatchEventId,
    observation_cloudWatchEventSource,
    observation_codeDeployApplication,
    observation_codeDeployDeploymentGroup,
    observation_codeDeployDeploymentId,
    observation_codeDeployInstanceGroupId,
    observation_codeDeployState,
    observation_ebsCause,
    observation_ebsEvent,
    observation_ebsRequestId,
    observation_ebsResult,
    observation_ec2State,
    observation_endTime,
    observation_healthEventArn,
    observation_healthEventDescription,
    observation_healthEventTypeCategory,
    observation_healthEventTypeCode,
    observation_healthService,
    observation_id,
    observation_lineTime,
    observation_logFilter,
    observation_logGroup,
    observation_logText,
    observation_metricName,
    observation_metricNamespace,
    observation_rdsEventCategories,
    observation_rdsEventMessage,
    observation_s3EventName,
    observation_sourceARN,
    observation_sourceType,
    observation_startTime,
    observation_statesArn,
    observation_statesExecutionArn,
    observation_statesInput,
    observation_statesStatus,
    observation_unit,
    observation_value,
    observation_xRayErrorPercent,
    observation_xRayFaultPercent,
    observation_xRayNodeName,
    observation_xRayNodeType,
    observation_xRayRequestAverageLatency,
    observation_xRayRequestCount,
    observation_xRayThrottlePercent,

    -- * Problem
    Problem (..),
    newProblem,
    problem_affectedResource,
    problem_endTime,
    problem_feedback,
    problem_id,
    problem_insights,
    problem_lastRecurrenceTime,
    problem_recurringCount,
    problem_resourceGroupName,
    problem_severityLevel,
    problem_startTime,
    problem_status,
    problem_title,

    -- * RelatedObservations
    RelatedObservations (..),
    newRelatedObservations,
    relatedObservations_observationList,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import Amazonka.ApplicationInsights.Types.ApplicationComponent
import Amazonka.ApplicationInsights.Types.ApplicationInfo
import Amazonka.ApplicationInsights.Types.CloudWatchEventSource
import Amazonka.ApplicationInsights.Types.ConfigurationEvent
import Amazonka.ApplicationInsights.Types.ConfigurationEventResourceType
import Amazonka.ApplicationInsights.Types.ConfigurationEventStatus
import Amazonka.ApplicationInsights.Types.DiscoveryType
import Amazonka.ApplicationInsights.Types.FeedbackKey
import Amazonka.ApplicationInsights.Types.FeedbackValue
import Amazonka.ApplicationInsights.Types.GroupingType
import Amazonka.ApplicationInsights.Types.LogFilter
import Amazonka.ApplicationInsights.Types.LogPattern
import Amazonka.ApplicationInsights.Types.Observation
import Amazonka.ApplicationInsights.Types.OsType
import Amazonka.ApplicationInsights.Types.Problem
import Amazonka.ApplicationInsights.Types.RelatedObservations
import Amazonka.ApplicationInsights.Types.SeverityLevel
import Amazonka.ApplicationInsights.Types.Status
import Amazonka.ApplicationInsights.Types.Tag
import Amazonka.ApplicationInsights.Types.Tier
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-11-25@ of the Amazon CloudWatch Application Insights SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ApplicationInsights",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "applicationinsights",
      Core.signingName = "applicationinsights",
      Core.version = "2018-11-25",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "ApplicationInsights",
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

-- | User does not have permissions to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The request is not understood by the server.
_BadRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"

-- | The server encountered an internal error and is unable to complete the
-- request.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The resource is already created or in use.
_ResourceInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The resource does not exist in the customer account.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Tags are already registered for the specified application ARN.
_TagsAlreadyExistException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TagsAlreadyExistException =
  Core._MatchServiceError
    defaultService
    "TagsAlreadyExistException"

-- | The number of the provided tags is beyond the limit, or the number of
-- total tags you are trying to attach to the specified resource exceeds
-- the limit.
_TooManyTagsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The parameter is not valid.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
