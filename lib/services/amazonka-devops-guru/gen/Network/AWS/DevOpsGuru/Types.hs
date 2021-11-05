{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DevOpsGuru.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * AnomalySeverity
    AnomalySeverity (..),

    -- * AnomalyStatus
    AnomalyStatus (..),

    -- * CloudWatchMetricsStat
    CloudWatchMetricsStat (..),

    -- * CostEstimationServiceResourceState
    CostEstimationServiceResourceState (..),

    -- * CostEstimationStatus
    CostEstimationStatus (..),

    -- * EventClass
    EventClass (..),

    -- * EventDataSource
    EventDataSource (..),

    -- * InsightFeedbackOption
    InsightFeedbackOption (..),

    -- * InsightSeverity
    InsightSeverity (..),

    -- * InsightStatus
    InsightStatus (..),

    -- * InsightType
    InsightType (..),

    -- * Locale
    Locale (..),

    -- * OptInStatus
    OptInStatus (..),

    -- * ResourceCollectionType
    ResourceCollectionType (..),

    -- * ServiceName
    ServiceName (..),

    -- * UpdateResourceCollectionAction
    UpdateResourceCollectionAction (..),

    -- * AnomalyReportedTimeRange
    AnomalyReportedTimeRange (..),
    newAnomalyReportedTimeRange,
    anomalyReportedTimeRange_closeTime,
    anomalyReportedTimeRange_openTime,

    -- * AnomalySourceDetails
    AnomalySourceDetails (..),
    newAnomalySourceDetails,
    anomalySourceDetails_cloudWatchMetrics,

    -- * AnomalyTimeRange
    AnomalyTimeRange (..),
    newAnomalyTimeRange,
    anomalyTimeRange_endTime,
    anomalyTimeRange_startTime,

    -- * CloudFormationCollection
    CloudFormationCollection (..),
    newCloudFormationCollection,
    cloudFormationCollection_stackNames,

    -- * CloudFormationCollectionFilter
    CloudFormationCollectionFilter (..),
    newCloudFormationCollectionFilter,
    cloudFormationCollectionFilter_stackNames,

    -- * CloudFormationCostEstimationResourceCollectionFilter
    CloudFormationCostEstimationResourceCollectionFilter (..),
    newCloudFormationCostEstimationResourceCollectionFilter,
    cloudFormationCostEstimationResourceCollectionFilter_stackNames,

    -- * CloudFormationHealth
    CloudFormationHealth (..),
    newCloudFormationHealth,
    cloudFormationHealth_insight,
    cloudFormationHealth_stackName,

    -- * CloudWatchMetricsDetail
    CloudWatchMetricsDetail (..),
    newCloudWatchMetricsDetail,
    cloudWatchMetricsDetail_period,
    cloudWatchMetricsDetail_metricName,
    cloudWatchMetricsDetail_namespace,
    cloudWatchMetricsDetail_stat,
    cloudWatchMetricsDetail_dimensions,
    cloudWatchMetricsDetail_unit,

    -- * CloudWatchMetricsDimension
    CloudWatchMetricsDimension (..),
    newCloudWatchMetricsDimension,
    cloudWatchMetricsDimension_value,
    cloudWatchMetricsDimension_name,

    -- * CostEstimationResourceCollectionFilter
    CostEstimationResourceCollectionFilter (..),
    newCostEstimationResourceCollectionFilter,
    costEstimationResourceCollectionFilter_cloudFormation,

    -- * CostEstimationTimeRange
    CostEstimationTimeRange (..),
    newCostEstimationTimeRange,
    costEstimationTimeRange_startTime,
    costEstimationTimeRange_endTime,

    -- * EndTimeRange
    EndTimeRange (..),
    newEndTimeRange,
    endTimeRange_fromTime,
    endTimeRange_toTime,

    -- * Event
    Event (..),
    newEvent,
    event_resourceCollection,
    event_eventClass,
    event_time,
    event_resources,
    event_name,
    event_id,
    event_dataSource,
    event_eventSource,

    -- * EventResource
    EventResource (..),
    newEventResource,
    eventResource_arn,
    eventResource_name,
    eventResource_type,

    -- * EventTimeRange
    EventTimeRange (..),
    newEventTimeRange,
    eventTimeRange_fromTime,
    eventTimeRange_toTime,

    -- * InsightFeedback
    InsightFeedback (..),
    newInsightFeedback,
    insightFeedback_id,
    insightFeedback_feedback,

    -- * InsightHealth
    InsightHealth (..),
    newInsightHealth,
    insightHealth_meanTimeToRecoverInMilliseconds,
    insightHealth_openReactiveInsights,
    insightHealth_openProactiveInsights,

    -- * InsightTimeRange
    InsightTimeRange (..),
    newInsightTimeRange,
    insightTimeRange_endTime,
    insightTimeRange_startTime,

    -- * ListEventsFilters
    ListEventsFilters (..),
    newListEventsFilters,
    listEventsFilters_resourceCollection,
    listEventsFilters_eventClass,
    listEventsFilters_insightId,
    listEventsFilters_dataSource,
    listEventsFilters_eventTimeRange,
    listEventsFilters_eventSource,

    -- * ListInsightsAnyStatusFilter
    ListInsightsAnyStatusFilter (..),
    newListInsightsAnyStatusFilter,
    listInsightsAnyStatusFilter_type,
    listInsightsAnyStatusFilter_startTimeRange,

    -- * ListInsightsClosedStatusFilter
    ListInsightsClosedStatusFilter (..),
    newListInsightsClosedStatusFilter,
    listInsightsClosedStatusFilter_type,
    listInsightsClosedStatusFilter_endTimeRange,

    -- * ListInsightsOngoingStatusFilter
    ListInsightsOngoingStatusFilter (..),
    newListInsightsOngoingStatusFilter,
    listInsightsOngoingStatusFilter_type,

    -- * ListInsightsStatusFilter
    ListInsightsStatusFilter (..),
    newListInsightsStatusFilter,
    listInsightsStatusFilter_closed,
    listInsightsStatusFilter_ongoing,
    listInsightsStatusFilter_any,

    -- * NotificationChannel
    NotificationChannel (..),
    newNotificationChannel,
    notificationChannel_config,
    notificationChannel_id,

    -- * NotificationChannelConfig
    NotificationChannelConfig (..),
    newNotificationChannelConfig,
    notificationChannelConfig_sns,

    -- * OpsCenterIntegration
    OpsCenterIntegration (..),
    newOpsCenterIntegration,
    opsCenterIntegration_optInStatus,

    -- * OpsCenterIntegrationConfig
    OpsCenterIntegrationConfig (..),
    newOpsCenterIntegrationConfig,
    opsCenterIntegrationConfig_optInStatus,

    -- * PredictionTimeRange
    PredictionTimeRange (..),
    newPredictionTimeRange,
    predictionTimeRange_endTime,
    predictionTimeRange_startTime,

    -- * ProactiveAnomaly
    ProactiveAnomaly (..),
    newProactiveAnomaly,
    proactiveAnomaly_anomalyReportedTimeRange,
    proactiveAnomaly_status,
    proactiveAnomaly_resourceCollection,
    proactiveAnomaly_severity,
    proactiveAnomaly_updateTime,
    proactiveAnomaly_sourceDetails,
    proactiveAnomaly_predictionTimeRange,
    proactiveAnomaly_limit,
    proactiveAnomaly_id,
    proactiveAnomaly_associatedInsightId,
    proactiveAnomaly_anomalyTimeRange,

    -- * ProactiveAnomalySummary
    ProactiveAnomalySummary (..),
    newProactiveAnomalySummary,
    proactiveAnomalySummary_anomalyReportedTimeRange,
    proactiveAnomalySummary_status,
    proactiveAnomalySummary_resourceCollection,
    proactiveAnomalySummary_severity,
    proactiveAnomalySummary_updateTime,
    proactiveAnomalySummary_sourceDetails,
    proactiveAnomalySummary_predictionTimeRange,
    proactiveAnomalySummary_limit,
    proactiveAnomalySummary_id,
    proactiveAnomalySummary_associatedInsightId,
    proactiveAnomalySummary_anomalyTimeRange,

    -- * ProactiveInsight
    ProactiveInsight (..),
    newProactiveInsight,
    proactiveInsight_status,
    proactiveInsight_resourceCollection,
    proactiveInsight_severity,
    proactiveInsight_ssmOpsItemId,
    proactiveInsight_insightTimeRange,
    proactiveInsight_name,
    proactiveInsight_predictionTimeRange,
    proactiveInsight_id,

    -- * ProactiveInsightSummary
    ProactiveInsightSummary (..),
    newProactiveInsightSummary,
    proactiveInsightSummary_status,
    proactiveInsightSummary_resourceCollection,
    proactiveInsightSummary_severity,
    proactiveInsightSummary_insightTimeRange,
    proactiveInsightSummary_name,
    proactiveInsightSummary_predictionTimeRange,
    proactiveInsightSummary_id,
    proactiveInsightSummary_serviceCollection,

    -- * ReactiveAnomaly
    ReactiveAnomaly (..),
    newReactiveAnomaly,
    reactiveAnomaly_anomalyReportedTimeRange,
    reactiveAnomaly_status,
    reactiveAnomaly_resourceCollection,
    reactiveAnomaly_severity,
    reactiveAnomaly_sourceDetails,
    reactiveAnomaly_id,
    reactiveAnomaly_associatedInsightId,
    reactiveAnomaly_anomalyTimeRange,

    -- * ReactiveAnomalySummary
    ReactiveAnomalySummary (..),
    newReactiveAnomalySummary,
    reactiveAnomalySummary_anomalyReportedTimeRange,
    reactiveAnomalySummary_status,
    reactiveAnomalySummary_resourceCollection,
    reactiveAnomalySummary_severity,
    reactiveAnomalySummary_sourceDetails,
    reactiveAnomalySummary_id,
    reactiveAnomalySummary_associatedInsightId,
    reactiveAnomalySummary_anomalyTimeRange,

    -- * ReactiveInsight
    ReactiveInsight (..),
    newReactiveInsight,
    reactiveInsight_status,
    reactiveInsight_resourceCollection,
    reactiveInsight_severity,
    reactiveInsight_ssmOpsItemId,
    reactiveInsight_insightTimeRange,
    reactiveInsight_name,
    reactiveInsight_id,

    -- * ReactiveInsightSummary
    ReactiveInsightSummary (..),
    newReactiveInsightSummary,
    reactiveInsightSummary_status,
    reactiveInsightSummary_resourceCollection,
    reactiveInsightSummary_severity,
    reactiveInsightSummary_insightTimeRange,
    reactiveInsightSummary_name,
    reactiveInsightSummary_id,
    reactiveInsightSummary_serviceCollection,

    -- * Recommendation
    Recommendation (..),
    newRecommendation,
    recommendation_link,
    recommendation_relatedAnomalies,
    recommendation_reason,
    recommendation_name,
    recommendation_relatedEvents,
    recommendation_description,

    -- * RecommendationRelatedAnomaly
    RecommendationRelatedAnomaly (..),
    newRecommendationRelatedAnomaly,
    recommendationRelatedAnomaly_resources,
    recommendationRelatedAnomaly_sourceDetails,

    -- * RecommendationRelatedAnomalyResource
    RecommendationRelatedAnomalyResource (..),
    newRecommendationRelatedAnomalyResource,
    recommendationRelatedAnomalyResource_name,
    recommendationRelatedAnomalyResource_type,

    -- * RecommendationRelatedAnomalySourceDetail
    RecommendationRelatedAnomalySourceDetail (..),
    newRecommendationRelatedAnomalySourceDetail,
    recommendationRelatedAnomalySourceDetail_cloudWatchMetrics,

    -- * RecommendationRelatedCloudWatchMetricsSourceDetail
    RecommendationRelatedCloudWatchMetricsSourceDetail (..),
    newRecommendationRelatedCloudWatchMetricsSourceDetail,
    recommendationRelatedCloudWatchMetricsSourceDetail_metricName,
    recommendationRelatedCloudWatchMetricsSourceDetail_namespace,

    -- * RecommendationRelatedEvent
    RecommendationRelatedEvent (..),
    newRecommendationRelatedEvent,
    recommendationRelatedEvent_resources,
    recommendationRelatedEvent_name,

    -- * RecommendationRelatedEventResource
    RecommendationRelatedEventResource (..),
    newRecommendationRelatedEventResource,
    recommendationRelatedEventResource_name,
    recommendationRelatedEventResource_type,

    -- * ResourceCollection
    ResourceCollection (..),
    newResourceCollection,
    resourceCollection_cloudFormation,

    -- * ResourceCollectionFilter
    ResourceCollectionFilter (..),
    newResourceCollectionFilter,
    resourceCollectionFilter_cloudFormation,

    -- * SearchInsightsFilters
    SearchInsightsFilters (..),
    newSearchInsightsFilters,
    searchInsightsFilters_resourceCollection,
    searchInsightsFilters_statuses,
    searchInsightsFilters_severities,
    searchInsightsFilters_serviceCollection,

    -- * ServiceCollection
    ServiceCollection (..),
    newServiceCollection,
    serviceCollection_serviceNames,

    -- * ServiceHealth
    ServiceHealth (..),
    newServiceHealth,
    serviceHealth_insight,
    serviceHealth_serviceName,

    -- * ServiceInsightHealth
    ServiceInsightHealth (..),
    newServiceInsightHealth,
    serviceInsightHealth_openReactiveInsights,
    serviceInsightHealth_openProactiveInsights,

    -- * ServiceIntegrationConfig
    ServiceIntegrationConfig (..),
    newServiceIntegrationConfig,
    serviceIntegrationConfig_opsCenter,

    -- * ServiceResourceCost
    ServiceResourceCost (..),
    newServiceResourceCost,
    serviceResourceCost_state,
    serviceResourceCost_unitCost,
    serviceResourceCost_count,
    serviceResourceCost_cost,
    serviceResourceCost_type,

    -- * SnsChannelConfig
    SnsChannelConfig (..),
    newSnsChannelConfig,
    snsChannelConfig_topicArn,

    -- * StartTimeRange
    StartTimeRange (..),
    newStartTimeRange,
    startTimeRange_fromTime,
    startTimeRange_toTime,

    -- * UpdateCloudFormationCollectionFilter
    UpdateCloudFormationCollectionFilter (..),
    newUpdateCloudFormationCollectionFilter,
    updateCloudFormationCollectionFilter_stackNames,

    -- * UpdateResourceCollectionFilter
    UpdateResourceCollectionFilter (..),
    newUpdateResourceCollectionFilter,
    updateResourceCollectionFilter_cloudFormation,

    -- * UpdateServiceIntegrationConfig
    UpdateServiceIntegrationConfig (..),
    newUpdateServiceIntegrationConfig,
    updateServiceIntegrationConfig_opsCenter,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DevOpsGuru.Types.AnomalyReportedTimeRange
import Amazonka.DevOpsGuru.Types.AnomalySeverity
import Amazonka.DevOpsGuru.Types.AnomalySourceDetails
import Amazonka.DevOpsGuru.Types.AnomalyStatus
import Amazonka.DevOpsGuru.Types.AnomalyTimeRange
import Amazonka.DevOpsGuru.Types.CloudFormationCollection
import Amazonka.DevOpsGuru.Types.CloudFormationCollectionFilter
import Amazonka.DevOpsGuru.Types.CloudFormationCostEstimationResourceCollectionFilter
import Amazonka.DevOpsGuru.Types.CloudFormationHealth
import Amazonka.DevOpsGuru.Types.CloudWatchMetricsDetail
import Amazonka.DevOpsGuru.Types.CloudWatchMetricsDimension
import Amazonka.DevOpsGuru.Types.CloudWatchMetricsStat
import Amazonka.DevOpsGuru.Types.CostEstimationResourceCollectionFilter
import Amazonka.DevOpsGuru.Types.CostEstimationServiceResourceState
import Amazonka.DevOpsGuru.Types.CostEstimationStatus
import Amazonka.DevOpsGuru.Types.CostEstimationTimeRange
import Amazonka.DevOpsGuru.Types.EndTimeRange
import Amazonka.DevOpsGuru.Types.Event
import Amazonka.DevOpsGuru.Types.EventClass
import Amazonka.DevOpsGuru.Types.EventDataSource
import Amazonka.DevOpsGuru.Types.EventResource
import Amazonka.DevOpsGuru.Types.EventTimeRange
import Amazonka.DevOpsGuru.Types.InsightFeedback
import Amazonka.DevOpsGuru.Types.InsightFeedbackOption
import Amazonka.DevOpsGuru.Types.InsightHealth
import Amazonka.DevOpsGuru.Types.InsightSeverity
import Amazonka.DevOpsGuru.Types.InsightStatus
import Amazonka.DevOpsGuru.Types.InsightTimeRange
import Amazonka.DevOpsGuru.Types.InsightType
import Amazonka.DevOpsGuru.Types.ListEventsFilters
import Amazonka.DevOpsGuru.Types.ListInsightsAnyStatusFilter
import Amazonka.DevOpsGuru.Types.ListInsightsClosedStatusFilter
import Amazonka.DevOpsGuru.Types.ListInsightsOngoingStatusFilter
import Amazonka.DevOpsGuru.Types.ListInsightsStatusFilter
import Amazonka.DevOpsGuru.Types.Locale
import Amazonka.DevOpsGuru.Types.NotificationChannel
import Amazonka.DevOpsGuru.Types.NotificationChannelConfig
import Amazonka.DevOpsGuru.Types.OpsCenterIntegration
import Amazonka.DevOpsGuru.Types.OpsCenterIntegrationConfig
import Amazonka.DevOpsGuru.Types.OptInStatus
import Amazonka.DevOpsGuru.Types.PredictionTimeRange
import Amazonka.DevOpsGuru.Types.ProactiveAnomaly
import Amazonka.DevOpsGuru.Types.ProactiveAnomalySummary
import Amazonka.DevOpsGuru.Types.ProactiveInsight
import Amazonka.DevOpsGuru.Types.ProactiveInsightSummary
import Amazonka.DevOpsGuru.Types.ReactiveAnomaly
import Amazonka.DevOpsGuru.Types.ReactiveAnomalySummary
import Amazonka.DevOpsGuru.Types.ReactiveInsight
import Amazonka.DevOpsGuru.Types.ReactiveInsightSummary
import Amazonka.DevOpsGuru.Types.Recommendation
import Amazonka.DevOpsGuru.Types.RecommendationRelatedAnomaly
import Amazonka.DevOpsGuru.Types.RecommendationRelatedAnomalyResource
import Amazonka.DevOpsGuru.Types.RecommendationRelatedAnomalySourceDetail
import Amazonka.DevOpsGuru.Types.RecommendationRelatedCloudWatchMetricsSourceDetail
import Amazonka.DevOpsGuru.Types.RecommendationRelatedEvent
import Amazonka.DevOpsGuru.Types.RecommendationRelatedEventResource
import Amazonka.DevOpsGuru.Types.ResourceCollection
import Amazonka.DevOpsGuru.Types.ResourceCollectionFilter
import Amazonka.DevOpsGuru.Types.ResourceCollectionType
import Amazonka.DevOpsGuru.Types.SearchInsightsFilters
import Amazonka.DevOpsGuru.Types.ServiceCollection
import Amazonka.DevOpsGuru.Types.ServiceHealth
import Amazonka.DevOpsGuru.Types.ServiceInsightHealth
import Amazonka.DevOpsGuru.Types.ServiceIntegrationConfig
import Amazonka.DevOpsGuru.Types.ServiceName
import Amazonka.DevOpsGuru.Types.ServiceResourceCost
import Amazonka.DevOpsGuru.Types.SnsChannelConfig
import Amazonka.DevOpsGuru.Types.StartTimeRange
import Amazonka.DevOpsGuru.Types.UpdateCloudFormationCollectionFilter
import Amazonka.DevOpsGuru.Types.UpdateResourceCollectionAction
import Amazonka.DevOpsGuru.Types.UpdateResourceCollectionFilter
import Amazonka.DevOpsGuru.Types.UpdateServiceIntegrationConfig
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-12-01@ of the Amazon DevOps Guru SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "DevOpsGuru",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "devops-guru",
      Core._serviceSigningName = "devops-guru",
      Core._serviceVersion = "2020-12-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "DevOpsGuru",
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

-- | Contains information about data passed in to a field during a request
-- that is not valid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | You don\'t have permissions to perform the requested operation. The user
-- or role that is making the request must have at least one IAM
-- permissions policy attached that grants the required permissions. For
-- more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access.html Access Management>
-- in the /IAM User Guide/.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | An exception that is thrown when a conflict occurs.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request contains a value that exceeds a maximum quota.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was denied due to a request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | An internal failure in an Amazon service occurred.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | A requested resource could not be found
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
