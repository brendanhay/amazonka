{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DevOpsGuru.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types
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

    -- * AnomalySeverity
    AnomalySeverity (..),

    -- * AnomalyStatus
    AnomalyStatus (..),

    -- * AnomalyType
    AnomalyType (..),

    -- * CloudWatchMetricDataStatusCode
    CloudWatchMetricDataStatusCode (..),

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

    -- * EventSourceOptInStatus
    EventSourceOptInStatus (..),

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

    -- * LogAnomalyType
    LogAnomalyType (..),

    -- * NotificationMessageType
    NotificationMessageType (..),

    -- * OptInStatus
    OptInStatus (..),

    -- * OrganizationResourceCollectionType
    OrganizationResourceCollectionType (..),

    -- * ResourceCollectionType
    ResourceCollectionType (..),

    -- * ResourcePermission
    ResourcePermission (..),

    -- * ResourceTypeFilter
    ResourceTypeFilter (..),

    -- * ServiceName
    ServiceName (..),

    -- * UpdateResourceCollectionAction
    UpdateResourceCollectionAction (..),

    -- * AccountHealth
    AccountHealth (..),
    newAccountHealth,
    accountHealth_accountId,
    accountHealth_insight,

    -- * AccountInsightHealth
    AccountInsightHealth (..),
    newAccountInsightHealth,
    accountInsightHealth_openProactiveInsights,
    accountInsightHealth_openReactiveInsights,

    -- * AmazonCodeGuruProfilerIntegration
    AmazonCodeGuruProfilerIntegration (..),
    newAmazonCodeGuruProfilerIntegration,
    amazonCodeGuruProfilerIntegration_status,

    -- * AnomalousLogGroup
    AnomalousLogGroup (..),
    newAnomalousLogGroup,
    anomalousLogGroup_impactEndTime,
    anomalousLogGroup_impactStartTime,
    anomalousLogGroup_logAnomalyShowcases,
    anomalousLogGroup_logGroupName,
    anomalousLogGroup_numberOfLogLinesScanned,

    -- * AnomalyReportedTimeRange
    AnomalyReportedTimeRange (..),
    newAnomalyReportedTimeRange,
    anomalyReportedTimeRange_closeTime,
    anomalyReportedTimeRange_openTime,

    -- * AnomalyResource
    AnomalyResource (..),
    newAnomalyResource,
    anomalyResource_name,
    anomalyResource_type,

    -- * AnomalySourceDetails
    AnomalySourceDetails (..),
    newAnomalySourceDetails,
    anomalySourceDetails_cloudWatchMetrics,
    anomalySourceDetails_performanceInsightsMetrics,

    -- * AnomalySourceMetadata
    AnomalySourceMetadata (..),
    newAnomalySourceMetadata,
    anomalySourceMetadata_source,
    anomalySourceMetadata_sourceResourceName,
    anomalySourceMetadata_sourceResourceType,

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
    cloudFormationHealth_analyzedResourceCount,
    cloudFormationHealth_insight,
    cloudFormationHealth_stackName,

    -- * CloudWatchMetricsDataSummary
    CloudWatchMetricsDataSummary (..),
    newCloudWatchMetricsDataSummary,
    cloudWatchMetricsDataSummary_statusCode,
    cloudWatchMetricsDataSummary_timestampMetricValuePairList,

    -- * CloudWatchMetricsDetail
    CloudWatchMetricsDetail (..),
    newCloudWatchMetricsDetail,
    cloudWatchMetricsDetail_dimensions,
    cloudWatchMetricsDetail_metricDataSummary,
    cloudWatchMetricsDetail_metricName,
    cloudWatchMetricsDetail_namespace,
    cloudWatchMetricsDetail_period,
    cloudWatchMetricsDetail_stat,
    cloudWatchMetricsDetail_unit,

    -- * CloudWatchMetricsDimension
    CloudWatchMetricsDimension (..),
    newCloudWatchMetricsDimension,
    cloudWatchMetricsDimension_name,
    cloudWatchMetricsDimension_value,

    -- * CostEstimationResourceCollectionFilter
    CostEstimationResourceCollectionFilter (..),
    newCostEstimationResourceCollectionFilter,
    costEstimationResourceCollectionFilter_cloudFormation,
    costEstimationResourceCollectionFilter_tags,

    -- * CostEstimationTimeRange
    CostEstimationTimeRange (..),
    newCostEstimationTimeRange,
    costEstimationTimeRange_endTime,
    costEstimationTimeRange_startTime,

    -- * EndTimeRange
    EndTimeRange (..),
    newEndTimeRange,
    endTimeRange_fromTime,
    endTimeRange_toTime,

    -- * Event
    Event (..),
    newEvent,
    event_dataSource,
    event_eventClass,
    event_eventSource,
    event_id,
    event_name,
    event_resourceCollection,
    event_resources,
    event_time,

    -- * EventResource
    EventResource (..),
    newEventResource,
    eventResource_arn,
    eventResource_name,
    eventResource_type,

    -- * EventSourcesConfig
    EventSourcesConfig (..),
    newEventSourcesConfig,
    eventSourcesConfig_amazonCodeGuruProfiler,

    -- * EventTimeRange
    EventTimeRange (..),
    newEventTimeRange,
    eventTimeRange_fromTime,
    eventTimeRange_toTime,

    -- * InsightFeedback
    InsightFeedback (..),
    newInsightFeedback,
    insightFeedback_feedback,
    insightFeedback_id,

    -- * InsightHealth
    InsightHealth (..),
    newInsightHealth,
    insightHealth_meanTimeToRecoverInMilliseconds,
    insightHealth_openProactiveInsights,
    insightHealth_openReactiveInsights,

    -- * InsightTimeRange
    InsightTimeRange (..),
    newInsightTimeRange,
    insightTimeRange_endTime,
    insightTimeRange_startTime,

    -- * ListEventsFilters
    ListEventsFilters (..),
    newListEventsFilters,
    listEventsFilters_dataSource,
    listEventsFilters_eventClass,
    listEventsFilters_eventSource,
    listEventsFilters_eventTimeRange,
    listEventsFilters_insightId,
    listEventsFilters_resourceCollection,

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
    listInsightsStatusFilter_any,
    listInsightsStatusFilter_closed,
    listInsightsStatusFilter_ongoing,

    -- * ListMonitoredResourcesFilters
    ListMonitoredResourcesFilters (..),
    newListMonitoredResourcesFilters,
    listMonitoredResourcesFilters_resourcePermission,
    listMonitoredResourcesFilters_resourceTypeFilters,

    -- * LogAnomalyClass
    LogAnomalyClass (..),
    newLogAnomalyClass,
    logAnomalyClass_explanation,
    logAnomalyClass_logAnomalyToken,
    logAnomalyClass_logAnomalyType,
    logAnomalyClass_logEventId,
    logAnomalyClass_logEventTimestamp,
    logAnomalyClass_logStreamName,
    logAnomalyClass_numberOfLogLinesOccurrences,

    -- * LogAnomalyShowcase
    LogAnomalyShowcase (..),
    newLogAnomalyShowcase,
    logAnomalyShowcase_logAnomalyClasses,

    -- * LogsAnomalyDetectionIntegration
    LogsAnomalyDetectionIntegration (..),
    newLogsAnomalyDetectionIntegration,
    logsAnomalyDetectionIntegration_optInStatus,

    -- * LogsAnomalyDetectionIntegrationConfig
    LogsAnomalyDetectionIntegrationConfig (..),
    newLogsAnomalyDetectionIntegrationConfig,
    logsAnomalyDetectionIntegrationConfig_optInStatus,

    -- * MonitoredResourceIdentifier
    MonitoredResourceIdentifier (..),
    newMonitoredResourceIdentifier,
    monitoredResourceIdentifier_lastUpdated,
    monitoredResourceIdentifier_monitoredResourceName,
    monitoredResourceIdentifier_resourceCollection,
    monitoredResourceIdentifier_resourcePermission,
    monitoredResourceIdentifier_type,

    -- * NotificationChannel
    NotificationChannel (..),
    newNotificationChannel,
    notificationChannel_config,
    notificationChannel_id,

    -- * NotificationChannelConfig
    NotificationChannelConfig (..),
    newNotificationChannelConfig,
    notificationChannelConfig_filters,
    notificationChannelConfig_sns,

    -- * NotificationFilterConfig
    NotificationFilterConfig (..),
    newNotificationFilterConfig,
    notificationFilterConfig_messageTypes,
    notificationFilterConfig_severities,

    -- * OpsCenterIntegration
    OpsCenterIntegration (..),
    newOpsCenterIntegration,
    opsCenterIntegration_optInStatus,

    -- * OpsCenterIntegrationConfig
    OpsCenterIntegrationConfig (..),
    newOpsCenterIntegrationConfig,
    opsCenterIntegrationConfig_optInStatus,

    -- * PerformanceInsightsMetricDimensionGroup
    PerformanceInsightsMetricDimensionGroup (..),
    newPerformanceInsightsMetricDimensionGroup,
    performanceInsightsMetricDimensionGroup_dimensions,
    performanceInsightsMetricDimensionGroup_group,
    performanceInsightsMetricDimensionGroup_limit,

    -- * PerformanceInsightsMetricQuery
    PerformanceInsightsMetricQuery (..),
    newPerformanceInsightsMetricQuery,
    performanceInsightsMetricQuery_filter,
    performanceInsightsMetricQuery_groupBy,
    performanceInsightsMetricQuery_metric,

    -- * PerformanceInsightsMetricsDetail
    PerformanceInsightsMetricsDetail (..),
    newPerformanceInsightsMetricsDetail,
    performanceInsightsMetricsDetail_metricDisplayName,
    performanceInsightsMetricsDetail_metricQuery,
    performanceInsightsMetricsDetail_referenceData,
    performanceInsightsMetricsDetail_statsAtAnomaly,
    performanceInsightsMetricsDetail_statsAtBaseline,
    performanceInsightsMetricsDetail_unit,

    -- * PerformanceInsightsReferenceComparisonValues
    PerformanceInsightsReferenceComparisonValues (..),
    newPerformanceInsightsReferenceComparisonValues,
    performanceInsightsReferenceComparisonValues_referenceMetric,
    performanceInsightsReferenceComparisonValues_referenceScalar,

    -- * PerformanceInsightsReferenceData
    PerformanceInsightsReferenceData (..),
    newPerformanceInsightsReferenceData,
    performanceInsightsReferenceData_comparisonValues,
    performanceInsightsReferenceData_name,

    -- * PerformanceInsightsReferenceMetric
    PerformanceInsightsReferenceMetric (..),
    newPerformanceInsightsReferenceMetric,
    performanceInsightsReferenceMetric_metricQuery,

    -- * PerformanceInsightsReferenceScalar
    PerformanceInsightsReferenceScalar (..),
    newPerformanceInsightsReferenceScalar,
    performanceInsightsReferenceScalar_value,

    -- * PerformanceInsightsStat
    PerformanceInsightsStat (..),
    newPerformanceInsightsStat,
    performanceInsightsStat_type,
    performanceInsightsStat_value,

    -- * PredictionTimeRange
    PredictionTimeRange (..),
    newPredictionTimeRange,
    predictionTimeRange_endTime,
    predictionTimeRange_startTime,

    -- * ProactiveAnomaly
    ProactiveAnomaly (..),
    newProactiveAnomaly,
    proactiveAnomaly_anomalyReportedTimeRange,
    proactiveAnomaly_anomalyResources,
    proactiveAnomaly_anomalyTimeRange,
    proactiveAnomaly_associatedInsightId,
    proactiveAnomaly_id,
    proactiveAnomaly_limit,
    proactiveAnomaly_predictionTimeRange,
    proactiveAnomaly_resourceCollection,
    proactiveAnomaly_severity,
    proactiveAnomaly_sourceDetails,
    proactiveAnomaly_sourceMetadata,
    proactiveAnomaly_status,
    proactiveAnomaly_updateTime,

    -- * ProactiveAnomalySummary
    ProactiveAnomalySummary (..),
    newProactiveAnomalySummary,
    proactiveAnomalySummary_anomalyReportedTimeRange,
    proactiveAnomalySummary_anomalyResources,
    proactiveAnomalySummary_anomalyTimeRange,
    proactiveAnomalySummary_associatedInsightId,
    proactiveAnomalySummary_id,
    proactiveAnomalySummary_limit,
    proactiveAnomalySummary_predictionTimeRange,
    proactiveAnomalySummary_resourceCollection,
    proactiveAnomalySummary_severity,
    proactiveAnomalySummary_sourceDetails,
    proactiveAnomalySummary_sourceMetadata,
    proactiveAnomalySummary_status,
    proactiveAnomalySummary_updateTime,

    -- * ProactiveInsight
    ProactiveInsight (..),
    newProactiveInsight,
    proactiveInsight_description,
    proactiveInsight_id,
    proactiveInsight_insightTimeRange,
    proactiveInsight_name,
    proactiveInsight_predictionTimeRange,
    proactiveInsight_resourceCollection,
    proactiveInsight_severity,
    proactiveInsight_ssmOpsItemId,
    proactiveInsight_status,

    -- * ProactiveInsightSummary
    ProactiveInsightSummary (..),
    newProactiveInsightSummary,
    proactiveInsightSummary_associatedResourceArns,
    proactiveInsightSummary_id,
    proactiveInsightSummary_insightTimeRange,
    proactiveInsightSummary_name,
    proactiveInsightSummary_predictionTimeRange,
    proactiveInsightSummary_resourceCollection,
    proactiveInsightSummary_serviceCollection,
    proactiveInsightSummary_severity,
    proactiveInsightSummary_status,

    -- * ProactiveOrganizationInsightSummary
    ProactiveOrganizationInsightSummary (..),
    newProactiveOrganizationInsightSummary,
    proactiveOrganizationInsightSummary_accountId,
    proactiveOrganizationInsightSummary_id,
    proactiveOrganizationInsightSummary_insightTimeRange,
    proactiveOrganizationInsightSummary_name,
    proactiveOrganizationInsightSummary_organizationalUnitId,
    proactiveOrganizationInsightSummary_predictionTimeRange,
    proactiveOrganizationInsightSummary_resourceCollection,
    proactiveOrganizationInsightSummary_serviceCollection,
    proactiveOrganizationInsightSummary_severity,
    proactiveOrganizationInsightSummary_status,

    -- * ReactiveAnomaly
    ReactiveAnomaly (..),
    newReactiveAnomaly,
    reactiveAnomaly_anomalyReportedTimeRange,
    reactiveAnomaly_anomalyResources,
    reactiveAnomaly_anomalyTimeRange,
    reactiveAnomaly_associatedInsightId,
    reactiveAnomaly_causalAnomalyId,
    reactiveAnomaly_description,
    reactiveAnomaly_id,
    reactiveAnomaly_name,
    reactiveAnomaly_resourceCollection,
    reactiveAnomaly_severity,
    reactiveAnomaly_sourceDetails,
    reactiveAnomaly_status,
    reactiveAnomaly_type,

    -- * ReactiveAnomalySummary
    ReactiveAnomalySummary (..),
    newReactiveAnomalySummary,
    reactiveAnomalySummary_anomalyReportedTimeRange,
    reactiveAnomalySummary_anomalyResources,
    reactiveAnomalySummary_anomalyTimeRange,
    reactiveAnomalySummary_associatedInsightId,
    reactiveAnomalySummary_causalAnomalyId,
    reactiveAnomalySummary_description,
    reactiveAnomalySummary_id,
    reactiveAnomalySummary_name,
    reactiveAnomalySummary_resourceCollection,
    reactiveAnomalySummary_severity,
    reactiveAnomalySummary_sourceDetails,
    reactiveAnomalySummary_status,
    reactiveAnomalySummary_type,

    -- * ReactiveInsight
    ReactiveInsight (..),
    newReactiveInsight,
    reactiveInsight_description,
    reactiveInsight_id,
    reactiveInsight_insightTimeRange,
    reactiveInsight_name,
    reactiveInsight_resourceCollection,
    reactiveInsight_severity,
    reactiveInsight_ssmOpsItemId,
    reactiveInsight_status,

    -- * ReactiveInsightSummary
    ReactiveInsightSummary (..),
    newReactiveInsightSummary,
    reactiveInsightSummary_associatedResourceArns,
    reactiveInsightSummary_id,
    reactiveInsightSummary_insightTimeRange,
    reactiveInsightSummary_name,
    reactiveInsightSummary_resourceCollection,
    reactiveInsightSummary_serviceCollection,
    reactiveInsightSummary_severity,
    reactiveInsightSummary_status,

    -- * ReactiveOrganizationInsightSummary
    ReactiveOrganizationInsightSummary (..),
    newReactiveOrganizationInsightSummary,
    reactiveOrganizationInsightSummary_accountId,
    reactiveOrganizationInsightSummary_id,
    reactiveOrganizationInsightSummary_insightTimeRange,
    reactiveOrganizationInsightSummary_name,
    reactiveOrganizationInsightSummary_organizationalUnitId,
    reactiveOrganizationInsightSummary_resourceCollection,
    reactiveOrganizationInsightSummary_serviceCollection,
    reactiveOrganizationInsightSummary_severity,
    reactiveOrganizationInsightSummary_status,

    -- * Recommendation
    Recommendation (..),
    newRecommendation,
    recommendation_category,
    recommendation_description,
    recommendation_link,
    recommendation_name,
    recommendation_reason,
    recommendation_relatedAnomalies,
    recommendation_relatedEvents,

    -- * RecommendationRelatedAnomaly
    RecommendationRelatedAnomaly (..),
    newRecommendationRelatedAnomaly,
    recommendationRelatedAnomaly_anomalyId,
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
    recommendationRelatedEvent_name,
    recommendationRelatedEvent_resources,

    -- * RecommendationRelatedEventResource
    RecommendationRelatedEventResource (..),
    newRecommendationRelatedEventResource,
    recommendationRelatedEventResource_name,
    recommendationRelatedEventResource_type,

    -- * ResourceCollection
    ResourceCollection (..),
    newResourceCollection,
    resourceCollection_cloudFormation,
    resourceCollection_tags,

    -- * ResourceCollectionFilter
    ResourceCollectionFilter (..),
    newResourceCollectionFilter,
    resourceCollectionFilter_cloudFormation,
    resourceCollectionFilter_tags,

    -- * SearchInsightsFilters
    SearchInsightsFilters (..),
    newSearchInsightsFilters,
    searchInsightsFilters_resourceCollection,
    searchInsightsFilters_serviceCollection,
    searchInsightsFilters_severities,
    searchInsightsFilters_statuses,

    -- * SearchOrganizationInsightsFilters
    SearchOrganizationInsightsFilters (..),
    newSearchOrganizationInsightsFilters,
    searchOrganizationInsightsFilters_resourceCollection,
    searchOrganizationInsightsFilters_serviceCollection,
    searchOrganizationInsightsFilters_severities,
    searchOrganizationInsightsFilters_statuses,

    -- * ServiceCollection
    ServiceCollection (..),
    newServiceCollection,
    serviceCollection_serviceNames,

    -- * ServiceHealth
    ServiceHealth (..),
    newServiceHealth,
    serviceHealth_analyzedResourceCount,
    serviceHealth_insight,
    serviceHealth_serviceName,

    -- * ServiceInsightHealth
    ServiceInsightHealth (..),
    newServiceInsightHealth,
    serviceInsightHealth_openProactiveInsights,
    serviceInsightHealth_openReactiveInsights,

    -- * ServiceIntegrationConfig
    ServiceIntegrationConfig (..),
    newServiceIntegrationConfig,
    serviceIntegrationConfig_logsAnomalyDetection,
    serviceIntegrationConfig_opsCenter,

    -- * ServiceResourceCost
    ServiceResourceCost (..),
    newServiceResourceCost,
    serviceResourceCost_cost,
    serviceResourceCost_count,
    serviceResourceCost_state,
    serviceResourceCost_type,
    serviceResourceCost_unitCost,

    -- * SnsChannelConfig
    SnsChannelConfig (..),
    newSnsChannelConfig,
    snsChannelConfig_topicArn,

    -- * StartTimeRange
    StartTimeRange (..),
    newStartTimeRange,
    startTimeRange_fromTime,
    startTimeRange_toTime,

    -- * TagCollection
    TagCollection (..),
    newTagCollection,
    tagCollection_appBoundaryKey,
    tagCollection_tagValues,

    -- * TagCollectionFilter
    TagCollectionFilter (..),
    newTagCollectionFilter,
    tagCollectionFilter_appBoundaryKey,
    tagCollectionFilter_tagValues,

    -- * TagCostEstimationResourceCollectionFilter
    TagCostEstimationResourceCollectionFilter (..),
    newTagCostEstimationResourceCollectionFilter,
    tagCostEstimationResourceCollectionFilter_appBoundaryKey,
    tagCostEstimationResourceCollectionFilter_tagValues,

    -- * TagHealth
    TagHealth (..),
    newTagHealth,
    tagHealth_analyzedResourceCount,
    tagHealth_appBoundaryKey,
    tagHealth_insight,
    tagHealth_tagValue,

    -- * TimestampMetricValuePair
    TimestampMetricValuePair (..),
    newTimestampMetricValuePair,
    timestampMetricValuePair_metricValue,
    timestampMetricValuePair_timestamp,

    -- * UpdateCloudFormationCollectionFilter
    UpdateCloudFormationCollectionFilter (..),
    newUpdateCloudFormationCollectionFilter,
    updateCloudFormationCollectionFilter_stackNames,

    -- * UpdateResourceCollectionFilter
    UpdateResourceCollectionFilter (..),
    newUpdateResourceCollectionFilter,
    updateResourceCollectionFilter_cloudFormation,
    updateResourceCollectionFilter_tags,

    -- * UpdateServiceIntegrationConfig
    UpdateServiceIntegrationConfig (..),
    newUpdateServiceIntegrationConfig,
    updateServiceIntegrationConfig_logsAnomalyDetection,
    updateServiceIntegrationConfig_opsCenter,

    -- * UpdateTagCollectionFilter
    UpdateTagCollectionFilter (..),
    newUpdateTagCollectionFilter,
    updateTagCollectionFilter_appBoundaryKey,
    updateTagCollectionFilter_tagValues,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DevOpsGuru.Types.AccountHealth
import Amazonka.DevOpsGuru.Types.AccountInsightHealth
import Amazonka.DevOpsGuru.Types.AmazonCodeGuruProfilerIntegration
import Amazonka.DevOpsGuru.Types.AnomalousLogGroup
import Amazonka.DevOpsGuru.Types.AnomalyReportedTimeRange
import Amazonka.DevOpsGuru.Types.AnomalyResource
import Amazonka.DevOpsGuru.Types.AnomalySeverity
import Amazonka.DevOpsGuru.Types.AnomalySourceDetails
import Amazonka.DevOpsGuru.Types.AnomalySourceMetadata
import Amazonka.DevOpsGuru.Types.AnomalyStatus
import Amazonka.DevOpsGuru.Types.AnomalyTimeRange
import Amazonka.DevOpsGuru.Types.AnomalyType
import Amazonka.DevOpsGuru.Types.CloudFormationCollection
import Amazonka.DevOpsGuru.Types.CloudFormationCollectionFilter
import Amazonka.DevOpsGuru.Types.CloudFormationCostEstimationResourceCollectionFilter
import Amazonka.DevOpsGuru.Types.CloudFormationHealth
import Amazonka.DevOpsGuru.Types.CloudWatchMetricDataStatusCode
import Amazonka.DevOpsGuru.Types.CloudWatchMetricsDataSummary
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
import Amazonka.DevOpsGuru.Types.EventSourceOptInStatus
import Amazonka.DevOpsGuru.Types.EventSourcesConfig
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
import Amazonka.DevOpsGuru.Types.ListMonitoredResourcesFilters
import Amazonka.DevOpsGuru.Types.Locale
import Amazonka.DevOpsGuru.Types.LogAnomalyClass
import Amazonka.DevOpsGuru.Types.LogAnomalyShowcase
import Amazonka.DevOpsGuru.Types.LogAnomalyType
import Amazonka.DevOpsGuru.Types.LogsAnomalyDetectionIntegration
import Amazonka.DevOpsGuru.Types.LogsAnomalyDetectionIntegrationConfig
import Amazonka.DevOpsGuru.Types.MonitoredResourceIdentifier
import Amazonka.DevOpsGuru.Types.NotificationChannel
import Amazonka.DevOpsGuru.Types.NotificationChannelConfig
import Amazonka.DevOpsGuru.Types.NotificationFilterConfig
import Amazonka.DevOpsGuru.Types.NotificationMessageType
import Amazonka.DevOpsGuru.Types.OpsCenterIntegration
import Amazonka.DevOpsGuru.Types.OpsCenterIntegrationConfig
import Amazonka.DevOpsGuru.Types.OptInStatus
import Amazonka.DevOpsGuru.Types.OrganizationResourceCollectionType
import Amazonka.DevOpsGuru.Types.PerformanceInsightsMetricDimensionGroup
import Amazonka.DevOpsGuru.Types.PerformanceInsightsMetricQuery
import Amazonka.DevOpsGuru.Types.PerformanceInsightsMetricsDetail
import Amazonka.DevOpsGuru.Types.PerformanceInsightsReferenceComparisonValues
import Amazonka.DevOpsGuru.Types.PerformanceInsightsReferenceData
import Amazonka.DevOpsGuru.Types.PerformanceInsightsReferenceMetric
import Amazonka.DevOpsGuru.Types.PerformanceInsightsReferenceScalar
import Amazonka.DevOpsGuru.Types.PerformanceInsightsStat
import Amazonka.DevOpsGuru.Types.PredictionTimeRange
import Amazonka.DevOpsGuru.Types.ProactiveAnomaly
import Amazonka.DevOpsGuru.Types.ProactiveAnomalySummary
import Amazonka.DevOpsGuru.Types.ProactiveInsight
import Amazonka.DevOpsGuru.Types.ProactiveInsightSummary
import Amazonka.DevOpsGuru.Types.ProactiveOrganizationInsightSummary
import Amazonka.DevOpsGuru.Types.ReactiveAnomaly
import Amazonka.DevOpsGuru.Types.ReactiveAnomalySummary
import Amazonka.DevOpsGuru.Types.ReactiveInsight
import Amazonka.DevOpsGuru.Types.ReactiveInsightSummary
import Amazonka.DevOpsGuru.Types.ReactiveOrganizationInsightSummary
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
import Amazonka.DevOpsGuru.Types.ResourcePermission
import Amazonka.DevOpsGuru.Types.ResourceTypeFilter
import Amazonka.DevOpsGuru.Types.SearchInsightsFilters
import Amazonka.DevOpsGuru.Types.SearchOrganizationInsightsFilters
import Amazonka.DevOpsGuru.Types.ServiceCollection
import Amazonka.DevOpsGuru.Types.ServiceHealth
import Amazonka.DevOpsGuru.Types.ServiceInsightHealth
import Amazonka.DevOpsGuru.Types.ServiceIntegrationConfig
import Amazonka.DevOpsGuru.Types.ServiceName
import Amazonka.DevOpsGuru.Types.ServiceResourceCost
import Amazonka.DevOpsGuru.Types.SnsChannelConfig
import Amazonka.DevOpsGuru.Types.StartTimeRange
import Amazonka.DevOpsGuru.Types.TagCollection
import Amazonka.DevOpsGuru.Types.TagCollectionFilter
import Amazonka.DevOpsGuru.Types.TagCostEstimationResourceCollectionFilter
import Amazonka.DevOpsGuru.Types.TagHealth
import Amazonka.DevOpsGuru.Types.TimestampMetricValuePair
import Amazonka.DevOpsGuru.Types.UpdateCloudFormationCollectionFilter
import Amazonka.DevOpsGuru.Types.UpdateResourceCollectionAction
import Amazonka.DevOpsGuru.Types.UpdateResourceCollectionFilter
import Amazonka.DevOpsGuru.Types.UpdateServiceIntegrationConfig
import Amazonka.DevOpsGuru.Types.UpdateTagCollectionFilter
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-12-01@ of the Amazon DevOps Guru SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "DevOpsGuru",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "devops-guru",
      Core.signingName = "devops-guru",
      Core.version = "2020-12-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "DevOpsGuru",
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

-- | You don\'t have permissions to perform the requested operation. The user
-- or role that is making the request must have at least one IAM
-- permissions policy attached that grants the required permissions. For
-- more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access.html Access Management>
-- in the /IAM User Guide/.
_AccessDeniedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | An exception that is thrown when a conflict occurs.
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | An internal failure in an Amazon service occurred.
_InternalServerException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | A requested resource could not be found
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request contains a value that exceeds a maximum quota.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was denied due to a request throttling.
_ThrottlingException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | Contains information about data passed in to a field during a request
-- that is not valid.
_ValidationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
