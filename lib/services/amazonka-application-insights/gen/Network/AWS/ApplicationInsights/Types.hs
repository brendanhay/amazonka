{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationInsights.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationInsights.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _TagsAlreadyExistException,
    _TooManyTagsException,
    _InternalServerException,
    _ResourceNotFoundException,
    _BadRequestException,
    _ResourceInUseException,

    -- * CloudWatchEventSource
    CloudWatchEventSource (..),

    -- * ConfigurationEventResourceType
    ConfigurationEventResourceType (..),

    -- * ConfigurationEventStatus
    ConfigurationEventStatus (..),

    -- * FeedbackKey
    FeedbackKey (..),

    -- * FeedbackValue
    FeedbackValue (..),

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
    applicationComponent_osType,
    applicationComponent_resourceType,
    applicationComponent_detectedWorkload,
    applicationComponent_monitor,
    applicationComponent_tier,
    applicationComponent_componentName,
    applicationComponent_componentRemarks,

    -- * ApplicationInfo
    ApplicationInfo (..),
    newApplicationInfo,
    applicationInfo_resourceGroupName,
    applicationInfo_cWEMonitorEnabled,
    applicationInfo_opsItemSNSTopicArn,
    applicationInfo_lifeCycle,
    applicationInfo_opsCenterEnabled,
    applicationInfo_remarks,

    -- * ConfigurationEvent
    ConfigurationEvent (..),
    newConfigurationEvent,
    configurationEvent_monitoredResourceARN,
    configurationEvent_eventStatus,
    configurationEvent_eventResourceName,
    configurationEvent_eventTime,
    configurationEvent_eventDetail,
    configurationEvent_eventResourceType,

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
    observation_codeDeployApplication,
    observation_rdsEventMessage,
    observation_codeDeployDeploymentId,
    observation_startTime,
    observation_sourceType,
    observation_sourceARN,
    observation_xRayRequestAverageLatency,
    observation_statesStatus,
    observation_codeDeployDeploymentGroup,
    observation_healthEventTypeCategory,
    observation_xRayRequestCount,
    observation_s3EventName,
    observation_metricName,
    observation_ec2State,
    observation_logGroup,
    observation_value,
    observation_healthEventDescription,
    observation_cloudWatchEventSource,
    observation_codeDeployState,
    observation_xRayErrorPercent,
    observation_statesArn,
    observation_cloudWatchEventId,
    observation_logText,
    observation_logFilter,
    observation_metricNamespace,
    observation_rdsEventCategories,
    observation_xRayNodeType,
    observation_endTime,
    observation_statesInput,
    observation_xRayNodeName,
    observation_id,
    observation_healthEventArn,
    observation_healthEventTypeCode,
    observation_ebsResult,
    observation_cloudWatchEventDetailType,
    observation_codeDeployInstanceGroupId,
    observation_ebsCause,
    observation_ebsEvent,
    observation_ebsRequestId,
    observation_xRayFaultPercent,
    observation_statesExecutionArn,
    observation_lineTime,
    observation_unit,
    observation_xRayThrottlePercent,
    observation_healthService,

    -- * Problem
    Problem (..),
    newProblem,
    problem_status,
    problem_resourceGroupName,
    problem_startTime,
    problem_insights,
    problem_endTime,
    problem_id,
    problem_severityLevel,
    problem_title,
    problem_affectedResource,
    problem_feedback,

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

import Network.AWS.ApplicationInsights.Types.ApplicationComponent
import Network.AWS.ApplicationInsights.Types.ApplicationInfo
import Network.AWS.ApplicationInsights.Types.CloudWatchEventSource
import Network.AWS.ApplicationInsights.Types.ConfigurationEvent
import Network.AWS.ApplicationInsights.Types.ConfigurationEventResourceType
import Network.AWS.ApplicationInsights.Types.ConfigurationEventStatus
import Network.AWS.ApplicationInsights.Types.FeedbackKey
import Network.AWS.ApplicationInsights.Types.FeedbackValue
import Network.AWS.ApplicationInsights.Types.LogFilter
import Network.AWS.ApplicationInsights.Types.LogPattern
import Network.AWS.ApplicationInsights.Types.Observation
import Network.AWS.ApplicationInsights.Types.OsType
import Network.AWS.ApplicationInsights.Types.Problem
import Network.AWS.ApplicationInsights.Types.RelatedObservations
import Network.AWS.ApplicationInsights.Types.SeverityLevel
import Network.AWS.ApplicationInsights.Types.Status
import Network.AWS.ApplicationInsights.Types.Tag
import Network.AWS.ApplicationInsights.Types.Tier
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-11-25@ of the Amazon CloudWatch Application Insights SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "ApplicationInsights",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "applicationinsights",
      Core._serviceSigningName = "applicationinsights",
      Core._serviceVersion = "2018-11-25",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "ApplicationInsights",
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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The parameter is not valid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | User does not have permissions to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | Tags are already registered for the specified application ARN.
_TagsAlreadyExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagsAlreadyExistException =
  Core._MatchServiceError
    defaultService
    "TagsAlreadyExistException"

-- | The number of the provided tags is beyond the limit, or the number of
-- total tags you are trying to attach to the specified resource exceeds
-- the limit.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The server encountered an internal error and is unable to complete the
-- request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The resource does not exist in the customer account.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The request is not understood by the server.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"

-- | The resource is already created or in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
