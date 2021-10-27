{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeGuruProfiler.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeGuruProfiler.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * ActionGroup
    ActionGroup (..),

    -- * AgentParameterField
    AgentParameterField (..),

    -- * AggregationPeriod
    AggregationPeriod (..),

    -- * ComputePlatform
    ComputePlatform (..),

    -- * EventPublisher
    EventPublisher (..),

    -- * FeedbackType
    FeedbackType (..),

    -- * MetadataField
    MetadataField (..),

    -- * MetricType
    MetricType (..),

    -- * OrderBy
    OrderBy (..),

    -- * AgentConfiguration
    AgentConfiguration (..),
    newAgentConfiguration,
    agentConfiguration_agentParameters,
    agentConfiguration_periodInSeconds,
    agentConfiguration_shouldProfile,

    -- * AgentOrchestrationConfig
    AgentOrchestrationConfig (..),
    newAgentOrchestrationConfig,
    agentOrchestrationConfig_profilingEnabled,

    -- * AggregatedProfileTime
    AggregatedProfileTime (..),
    newAggregatedProfileTime,
    aggregatedProfileTime_period,
    aggregatedProfileTime_start,

    -- * Anomaly
    Anomaly (..),
    newAnomaly,
    anomaly_instances,
    anomaly_metric,
    anomaly_reason,

    -- * AnomalyInstance
    AnomalyInstance (..),
    newAnomalyInstance,
    anomalyInstance_endTime,
    anomalyInstance_userFeedback,
    anomalyInstance_id,
    anomalyInstance_startTime,

    -- * Channel
    Channel (..),
    newChannel,
    channel_id,
    channel_eventPublishers,
    channel_uri,

    -- * FindingsReportSummary
    FindingsReportSummary (..),
    newFindingsReportSummary,
    findingsReportSummary_profileStartTime,
    findingsReportSummary_profileEndTime,
    findingsReportSummary_id,
    findingsReportSummary_profilingGroupName,
    findingsReportSummary_totalNumberOfFindings,

    -- * FrameMetric
    FrameMetric (..),
    newFrameMetric,
    frameMetric_frameName,
    frameMetric_threadStates,
    frameMetric_type,

    -- * FrameMetricDatum
    FrameMetricDatum (..),
    newFrameMetricDatum,
    frameMetricDatum_frameMetric,
    frameMetricDatum_values,

    -- * Match
    Match (..),
    newMatch,
    match_thresholdBreachValue,
    match_frameAddress,
    match_targetFramesIndex,

    -- * Metric
    Metric (..),
    newMetric,
    metric_frameName,
    metric_threadStates,
    metric_type,

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    newNotificationConfiguration,
    notificationConfiguration_channels,

    -- * Pattern
    Pattern (..),
    newPattern,
    pattern_thresholdPercent,
    pattern_targetFrames,
    pattern_countersToAggregate,
    pattern_name,
    pattern_resolutionSteps,
    pattern_id,
    pattern_description,

    -- * ProfileTime
    ProfileTime (..),
    newProfileTime,
    profileTime_start,

    -- * ProfilingGroupDescription
    ProfilingGroupDescription (..),
    newProfilingGroupDescription,
    profilingGroupDescription_computePlatform,
    profilingGroupDescription_arn,
    profilingGroupDescription_createdAt,
    profilingGroupDescription_name,
    profilingGroupDescription_profilingStatus,
    profilingGroupDescription_updatedAt,
    profilingGroupDescription_agentOrchestrationConfig,
    profilingGroupDescription_tags,

    -- * ProfilingStatus
    ProfilingStatus (..),
    newProfilingStatus,
    profilingStatus_latestAgentOrchestratedAt,
    profilingStatus_latestAgentProfileReportedAt,
    profilingStatus_latestAggregatedProfile,

    -- * Recommendation
    Recommendation (..),
    newRecommendation,
    recommendation_allMatchesCount,
    recommendation_allMatchesSum,
    recommendation_endTime,
    recommendation_pattern,
    recommendation_startTime,
    recommendation_topMatches,

    -- * TimestampStructure
    TimestampStructure (..),
    newTimestampStructure,
    timestampStructure_value,

    -- * UserFeedback
    UserFeedback (..),
    newUserFeedback,
    userFeedback_type,
  )
where

import Network.AWS.CodeGuruProfiler.Types.ActionGroup
import Network.AWS.CodeGuruProfiler.Types.AgentConfiguration
import Network.AWS.CodeGuruProfiler.Types.AgentOrchestrationConfig
import Network.AWS.CodeGuruProfiler.Types.AgentParameterField
import Network.AWS.CodeGuruProfiler.Types.AggregatedProfileTime
import Network.AWS.CodeGuruProfiler.Types.AggregationPeriod
import Network.AWS.CodeGuruProfiler.Types.Anomaly
import Network.AWS.CodeGuruProfiler.Types.AnomalyInstance
import Network.AWS.CodeGuruProfiler.Types.Channel
import Network.AWS.CodeGuruProfiler.Types.ComputePlatform
import Network.AWS.CodeGuruProfiler.Types.EventPublisher
import Network.AWS.CodeGuruProfiler.Types.FeedbackType
import Network.AWS.CodeGuruProfiler.Types.FindingsReportSummary
import Network.AWS.CodeGuruProfiler.Types.FrameMetric
import Network.AWS.CodeGuruProfiler.Types.FrameMetricDatum
import Network.AWS.CodeGuruProfiler.Types.Match
import Network.AWS.CodeGuruProfiler.Types.MetadataField
import Network.AWS.CodeGuruProfiler.Types.Metric
import Network.AWS.CodeGuruProfiler.Types.MetricType
import Network.AWS.CodeGuruProfiler.Types.NotificationConfiguration
import Network.AWS.CodeGuruProfiler.Types.OrderBy
import Network.AWS.CodeGuruProfiler.Types.Pattern
import Network.AWS.CodeGuruProfiler.Types.ProfileTime
import Network.AWS.CodeGuruProfiler.Types.ProfilingGroupDescription
import Network.AWS.CodeGuruProfiler.Types.ProfilingStatus
import Network.AWS.CodeGuruProfiler.Types.Recommendation
import Network.AWS.CodeGuruProfiler.Types.TimestampStructure
import Network.AWS.CodeGuruProfiler.Types.UserFeedback
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2019-07-18@ of the Amazon CodeGuru Profiler SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "CodeGuruProfiler",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "codeguru-profiler",
      Core._serviceSigningName = "codeguru-profiler",
      Core._serviceVersion = "2019-07-18",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "CodeGuruProfiler",
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
    Prelude.. Core.hasStatus 400

-- | The requested operation would cause a conflict with the current state of
-- a service resource associated with the request. Resolve the conflict
-- before retrying this request.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | You have exceeded your service quota. To perform the requested action,
-- remove some of the relevant resources, or use
-- <https://docs.aws.amazon.com/servicequotas/latest/userguide/intro.html Service Quotas>
-- to request a service quota increase.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The server encountered an internal error and is unable to complete the
-- request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The resource specified in the request does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
