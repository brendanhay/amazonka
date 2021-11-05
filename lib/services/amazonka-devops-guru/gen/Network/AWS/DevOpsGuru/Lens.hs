{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DevOpsGuru.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DevOpsGuru.Lens
  ( -- * Operations

    -- ** DescribeAnomaly
    describeAnomaly_id,
    describeAnomalyResponse_reactiveAnomaly,
    describeAnomalyResponse_proactiveAnomaly,
    describeAnomalyResponse_httpStatus,

    -- ** DescribeFeedback
    describeFeedback_insightId,
    describeFeedbackResponse_insightFeedback,
    describeFeedbackResponse_httpStatus,

    -- ** ListInsights
    listInsights_nextToken,
    listInsights_maxResults,
    listInsights_statusFilter,
    listInsightsResponse_reactiveInsights,
    listInsightsResponse_nextToken,
    listInsightsResponse_proactiveInsights,
    listInsightsResponse_httpStatus,

    -- ** AddNotificationChannel
    addNotificationChannel_config,
    addNotificationChannelResponse_httpStatus,
    addNotificationChannelResponse_id,

    -- ** ListNotificationChannels
    listNotificationChannels_nextToken,
    listNotificationChannelsResponse_channels,
    listNotificationChannelsResponse_nextToken,
    listNotificationChannelsResponse_httpStatus,

    -- ** DescribeAccountOverview
    describeAccountOverview_toTime,
    describeAccountOverview_fromTime,
    describeAccountOverviewResponse_httpStatus,
    describeAccountOverviewResponse_reactiveInsights,
    describeAccountOverviewResponse_proactiveInsights,
    describeAccountOverviewResponse_meanTimeToRecoverInMilliseconds,

    -- ** DescribeResourceCollectionHealth
    describeResourceCollectionHealth_nextToken,
    describeResourceCollectionHealth_resourceCollectionType,
    describeResourceCollectionHealthResponse_service,
    describeResourceCollectionHealthResponse_nextToken,
    describeResourceCollectionHealthResponse_httpStatus,
    describeResourceCollectionHealthResponse_cloudFormation,

    -- ** RemoveNotificationChannel
    removeNotificationChannel_id,
    removeNotificationChannelResponse_httpStatus,

    -- ** ListAnomaliesForInsight
    listAnomaliesForInsight_startTimeRange,
    listAnomaliesForInsight_nextToken,
    listAnomaliesForInsight_maxResults,
    listAnomaliesForInsight_insightId,
    listAnomaliesForInsightResponse_proactiveAnomalies,
    listAnomaliesForInsightResponse_nextToken,
    listAnomaliesForInsightResponse_reactiveAnomalies,
    listAnomaliesForInsightResponse_httpStatus,

    -- ** PutFeedback
    putFeedback_insightFeedback,
    putFeedbackResponse_httpStatus,

    -- ** SearchInsights
    searchInsights_filters,
    searchInsights_nextToken,
    searchInsights_maxResults,
    searchInsights_startTimeRange,
    searchInsights_type,
    searchInsightsResponse_reactiveInsights,
    searchInsightsResponse_nextToken,
    searchInsightsResponse_proactiveInsights,
    searchInsightsResponse_httpStatus,

    -- ** DescribeServiceIntegration
    describeServiceIntegrationResponse_serviceIntegration,
    describeServiceIntegrationResponse_httpStatus,

    -- ** UpdateServiceIntegration
    updateServiceIntegration_serviceIntegration,
    updateServiceIntegrationResponse_httpStatus,

    -- ** GetResourceCollection
    getResourceCollection_nextToken,
    getResourceCollection_resourceCollectionType,
    getResourceCollectionResponse_resourceCollection,
    getResourceCollectionResponse_nextToken,
    getResourceCollectionResponse_httpStatus,

    -- ** ListEvents
    listEvents_nextToken,
    listEvents_maxResults,
    listEvents_filters,
    listEventsResponse_nextToken,
    listEventsResponse_httpStatus,
    listEventsResponse_events,

    -- ** UpdateResourceCollection
    updateResourceCollection_action,
    updateResourceCollection_resourceCollection,
    updateResourceCollectionResponse_httpStatus,

    -- ** StartCostEstimation
    startCostEstimation_clientToken,
    startCostEstimation_resourceCollection,
    startCostEstimationResponse_httpStatus,

    -- ** ListRecommendations
    listRecommendations_locale,
    listRecommendations_nextToken,
    listRecommendations_insightId,
    listRecommendationsResponse_nextToken,
    listRecommendationsResponse_recommendations,
    listRecommendationsResponse_httpStatus,

    -- ** DescribeAccountHealth
    describeAccountHealthResponse_httpStatus,
    describeAccountHealthResponse_openReactiveInsights,
    describeAccountHealthResponse_openProactiveInsights,
    describeAccountHealthResponse_metricsAnalyzed,
    describeAccountHealthResponse_resourceHours,

    -- ** DescribeInsight
    describeInsight_id,
    describeInsightResponse_proactiveInsight,
    describeInsightResponse_reactiveInsight,
    describeInsightResponse_httpStatus,

    -- ** GetCostEstimation
    getCostEstimation_nextToken,
    getCostEstimationResponse_status,
    getCostEstimationResponse_resourceCollection,
    getCostEstimationResponse_timeRange,
    getCostEstimationResponse_costs,
    getCostEstimationResponse_nextToken,
    getCostEstimationResponse_totalCost,
    getCostEstimationResponse_httpStatus,

    -- * Types

    -- ** AnomalyReportedTimeRange
    anomalyReportedTimeRange_closeTime,
    anomalyReportedTimeRange_openTime,

    -- ** AnomalySourceDetails
    anomalySourceDetails_cloudWatchMetrics,

    -- ** AnomalyTimeRange
    anomalyTimeRange_endTime,
    anomalyTimeRange_startTime,

    -- ** CloudFormationCollection
    cloudFormationCollection_stackNames,

    -- ** CloudFormationCollectionFilter
    cloudFormationCollectionFilter_stackNames,

    -- ** CloudFormationCostEstimationResourceCollectionFilter
    cloudFormationCostEstimationResourceCollectionFilter_stackNames,

    -- ** CloudFormationHealth
    cloudFormationHealth_insight,
    cloudFormationHealth_stackName,

    -- ** CloudWatchMetricsDetail
    cloudWatchMetricsDetail_period,
    cloudWatchMetricsDetail_metricName,
    cloudWatchMetricsDetail_namespace,
    cloudWatchMetricsDetail_stat,
    cloudWatchMetricsDetail_dimensions,
    cloudWatchMetricsDetail_unit,

    -- ** CloudWatchMetricsDimension
    cloudWatchMetricsDimension_value,
    cloudWatchMetricsDimension_name,

    -- ** CostEstimationResourceCollectionFilter
    costEstimationResourceCollectionFilter_cloudFormation,

    -- ** CostEstimationTimeRange
    costEstimationTimeRange_startTime,
    costEstimationTimeRange_endTime,

    -- ** EndTimeRange
    endTimeRange_fromTime,
    endTimeRange_toTime,

    -- ** Event
    event_resourceCollection,
    event_eventClass,
    event_time,
    event_resources,
    event_name,
    event_id,
    event_dataSource,
    event_eventSource,

    -- ** EventResource
    eventResource_arn,
    eventResource_name,
    eventResource_type,

    -- ** EventTimeRange
    eventTimeRange_fromTime,
    eventTimeRange_toTime,

    -- ** InsightFeedback
    insightFeedback_id,
    insightFeedback_feedback,

    -- ** InsightHealth
    insightHealth_meanTimeToRecoverInMilliseconds,
    insightHealth_openReactiveInsights,
    insightHealth_openProactiveInsights,

    -- ** InsightTimeRange
    insightTimeRange_endTime,
    insightTimeRange_startTime,

    -- ** ListEventsFilters
    listEventsFilters_resourceCollection,
    listEventsFilters_eventClass,
    listEventsFilters_insightId,
    listEventsFilters_dataSource,
    listEventsFilters_eventTimeRange,
    listEventsFilters_eventSource,

    -- ** ListInsightsAnyStatusFilter
    listInsightsAnyStatusFilter_type,
    listInsightsAnyStatusFilter_startTimeRange,

    -- ** ListInsightsClosedStatusFilter
    listInsightsClosedStatusFilter_type,
    listInsightsClosedStatusFilter_endTimeRange,

    -- ** ListInsightsOngoingStatusFilter
    listInsightsOngoingStatusFilter_type,

    -- ** ListInsightsStatusFilter
    listInsightsStatusFilter_closed,
    listInsightsStatusFilter_ongoing,
    listInsightsStatusFilter_any,

    -- ** NotificationChannel
    notificationChannel_config,
    notificationChannel_id,

    -- ** NotificationChannelConfig
    notificationChannelConfig_sns,

    -- ** OpsCenterIntegration
    opsCenterIntegration_optInStatus,

    -- ** OpsCenterIntegrationConfig
    opsCenterIntegrationConfig_optInStatus,

    -- ** PredictionTimeRange
    predictionTimeRange_endTime,
    predictionTimeRange_startTime,

    -- ** ProactiveAnomaly
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

    -- ** ProactiveAnomalySummary
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

    -- ** ProactiveInsight
    proactiveInsight_status,
    proactiveInsight_resourceCollection,
    proactiveInsight_severity,
    proactiveInsight_ssmOpsItemId,
    proactiveInsight_insightTimeRange,
    proactiveInsight_name,
    proactiveInsight_predictionTimeRange,
    proactiveInsight_id,

    -- ** ProactiveInsightSummary
    proactiveInsightSummary_status,
    proactiveInsightSummary_resourceCollection,
    proactiveInsightSummary_severity,
    proactiveInsightSummary_insightTimeRange,
    proactiveInsightSummary_name,
    proactiveInsightSummary_predictionTimeRange,
    proactiveInsightSummary_id,
    proactiveInsightSummary_serviceCollection,

    -- ** ReactiveAnomaly
    reactiveAnomaly_anomalyReportedTimeRange,
    reactiveAnomaly_status,
    reactiveAnomaly_resourceCollection,
    reactiveAnomaly_severity,
    reactiveAnomaly_sourceDetails,
    reactiveAnomaly_id,
    reactiveAnomaly_associatedInsightId,
    reactiveAnomaly_anomalyTimeRange,

    -- ** ReactiveAnomalySummary
    reactiveAnomalySummary_anomalyReportedTimeRange,
    reactiveAnomalySummary_status,
    reactiveAnomalySummary_resourceCollection,
    reactiveAnomalySummary_severity,
    reactiveAnomalySummary_sourceDetails,
    reactiveAnomalySummary_id,
    reactiveAnomalySummary_associatedInsightId,
    reactiveAnomalySummary_anomalyTimeRange,

    -- ** ReactiveInsight
    reactiveInsight_status,
    reactiveInsight_resourceCollection,
    reactiveInsight_severity,
    reactiveInsight_ssmOpsItemId,
    reactiveInsight_insightTimeRange,
    reactiveInsight_name,
    reactiveInsight_id,

    -- ** ReactiveInsightSummary
    reactiveInsightSummary_status,
    reactiveInsightSummary_resourceCollection,
    reactiveInsightSummary_severity,
    reactiveInsightSummary_insightTimeRange,
    reactiveInsightSummary_name,
    reactiveInsightSummary_id,
    reactiveInsightSummary_serviceCollection,

    -- ** Recommendation
    recommendation_link,
    recommendation_relatedAnomalies,
    recommendation_reason,
    recommendation_name,
    recommendation_relatedEvents,
    recommendation_description,

    -- ** RecommendationRelatedAnomaly
    recommendationRelatedAnomaly_resources,
    recommendationRelatedAnomaly_sourceDetails,

    -- ** RecommendationRelatedAnomalyResource
    recommendationRelatedAnomalyResource_name,
    recommendationRelatedAnomalyResource_type,

    -- ** RecommendationRelatedAnomalySourceDetail
    recommendationRelatedAnomalySourceDetail_cloudWatchMetrics,

    -- ** RecommendationRelatedCloudWatchMetricsSourceDetail
    recommendationRelatedCloudWatchMetricsSourceDetail_metricName,
    recommendationRelatedCloudWatchMetricsSourceDetail_namespace,

    -- ** RecommendationRelatedEvent
    recommendationRelatedEvent_resources,
    recommendationRelatedEvent_name,

    -- ** RecommendationRelatedEventResource
    recommendationRelatedEventResource_name,
    recommendationRelatedEventResource_type,

    -- ** ResourceCollection
    resourceCollection_cloudFormation,

    -- ** ResourceCollectionFilter
    resourceCollectionFilter_cloudFormation,

    -- ** SearchInsightsFilters
    searchInsightsFilters_resourceCollection,
    searchInsightsFilters_statuses,
    searchInsightsFilters_severities,
    searchInsightsFilters_serviceCollection,

    -- ** ServiceCollection
    serviceCollection_serviceNames,

    -- ** ServiceHealth
    serviceHealth_insight,
    serviceHealth_serviceName,

    -- ** ServiceInsightHealth
    serviceInsightHealth_openReactiveInsights,
    serviceInsightHealth_openProactiveInsights,

    -- ** ServiceIntegrationConfig
    serviceIntegrationConfig_opsCenter,

    -- ** ServiceResourceCost
    serviceResourceCost_state,
    serviceResourceCost_unitCost,
    serviceResourceCost_count,
    serviceResourceCost_cost,
    serviceResourceCost_type,

    -- ** SnsChannelConfig
    snsChannelConfig_topicArn,

    -- ** StartTimeRange
    startTimeRange_fromTime,
    startTimeRange_toTime,

    -- ** UpdateCloudFormationCollectionFilter
    updateCloudFormationCollectionFilter_stackNames,

    -- ** UpdateResourceCollectionFilter
    updateResourceCollectionFilter_cloudFormation,

    -- ** UpdateServiceIntegrationConfig
    updateServiceIntegrationConfig_opsCenter,
  )
where

import Network.AWS.DevOpsGuru.AddNotificationChannel
import Network.AWS.DevOpsGuru.DescribeAccountHealth
import Network.AWS.DevOpsGuru.DescribeAccountOverview
import Network.AWS.DevOpsGuru.DescribeAnomaly
import Network.AWS.DevOpsGuru.DescribeFeedback
import Network.AWS.DevOpsGuru.DescribeInsight
import Network.AWS.DevOpsGuru.DescribeResourceCollectionHealth
import Network.AWS.DevOpsGuru.DescribeServiceIntegration
import Network.AWS.DevOpsGuru.GetCostEstimation
import Network.AWS.DevOpsGuru.GetResourceCollection
import Network.AWS.DevOpsGuru.ListAnomaliesForInsight
import Network.AWS.DevOpsGuru.ListEvents
import Network.AWS.DevOpsGuru.ListInsights
import Network.AWS.DevOpsGuru.ListNotificationChannels
import Network.AWS.DevOpsGuru.ListRecommendations
import Network.AWS.DevOpsGuru.PutFeedback
import Network.AWS.DevOpsGuru.RemoveNotificationChannel
import Network.AWS.DevOpsGuru.SearchInsights
import Network.AWS.DevOpsGuru.StartCostEstimation
import Network.AWS.DevOpsGuru.Types.AnomalyReportedTimeRange
import Network.AWS.DevOpsGuru.Types.AnomalySourceDetails
import Network.AWS.DevOpsGuru.Types.AnomalyTimeRange
import Network.AWS.DevOpsGuru.Types.CloudFormationCollection
import Network.AWS.DevOpsGuru.Types.CloudFormationCollectionFilter
import Network.AWS.DevOpsGuru.Types.CloudFormationCostEstimationResourceCollectionFilter
import Network.AWS.DevOpsGuru.Types.CloudFormationHealth
import Network.AWS.DevOpsGuru.Types.CloudWatchMetricsDetail
import Network.AWS.DevOpsGuru.Types.CloudWatchMetricsDimension
import Network.AWS.DevOpsGuru.Types.CostEstimationResourceCollectionFilter
import Network.AWS.DevOpsGuru.Types.CostEstimationTimeRange
import Network.AWS.DevOpsGuru.Types.EndTimeRange
import Network.AWS.DevOpsGuru.Types.Event
import Network.AWS.DevOpsGuru.Types.EventResource
import Network.AWS.DevOpsGuru.Types.EventTimeRange
import Network.AWS.DevOpsGuru.Types.InsightFeedback
import Network.AWS.DevOpsGuru.Types.InsightHealth
import Network.AWS.DevOpsGuru.Types.InsightTimeRange
import Network.AWS.DevOpsGuru.Types.ListEventsFilters
import Network.AWS.DevOpsGuru.Types.ListInsightsAnyStatusFilter
import Network.AWS.DevOpsGuru.Types.ListInsightsClosedStatusFilter
import Network.AWS.DevOpsGuru.Types.ListInsightsOngoingStatusFilter
import Network.AWS.DevOpsGuru.Types.ListInsightsStatusFilter
import Network.AWS.DevOpsGuru.Types.NotificationChannel
import Network.AWS.DevOpsGuru.Types.NotificationChannelConfig
import Network.AWS.DevOpsGuru.Types.OpsCenterIntegration
import Network.AWS.DevOpsGuru.Types.OpsCenterIntegrationConfig
import Network.AWS.DevOpsGuru.Types.PredictionTimeRange
import Network.AWS.DevOpsGuru.Types.ProactiveAnomaly
import Network.AWS.DevOpsGuru.Types.ProactiveAnomalySummary
import Network.AWS.DevOpsGuru.Types.ProactiveInsight
import Network.AWS.DevOpsGuru.Types.ProactiveInsightSummary
import Network.AWS.DevOpsGuru.Types.ReactiveAnomaly
import Network.AWS.DevOpsGuru.Types.ReactiveAnomalySummary
import Network.AWS.DevOpsGuru.Types.ReactiveInsight
import Network.AWS.DevOpsGuru.Types.ReactiveInsightSummary
import Network.AWS.DevOpsGuru.Types.Recommendation
import Network.AWS.DevOpsGuru.Types.RecommendationRelatedAnomaly
import Network.AWS.DevOpsGuru.Types.RecommendationRelatedAnomalyResource
import Network.AWS.DevOpsGuru.Types.RecommendationRelatedAnomalySourceDetail
import Network.AWS.DevOpsGuru.Types.RecommendationRelatedCloudWatchMetricsSourceDetail
import Network.AWS.DevOpsGuru.Types.RecommendationRelatedEvent
import Network.AWS.DevOpsGuru.Types.RecommendationRelatedEventResource
import Network.AWS.DevOpsGuru.Types.ResourceCollection
import Network.AWS.DevOpsGuru.Types.ResourceCollectionFilter
import Network.AWS.DevOpsGuru.Types.SearchInsightsFilters
import Network.AWS.DevOpsGuru.Types.ServiceCollection
import Network.AWS.DevOpsGuru.Types.ServiceHealth
import Network.AWS.DevOpsGuru.Types.ServiceInsightHealth
import Network.AWS.DevOpsGuru.Types.ServiceIntegrationConfig
import Network.AWS.DevOpsGuru.Types.ServiceResourceCost
import Network.AWS.DevOpsGuru.Types.SnsChannelConfig
import Network.AWS.DevOpsGuru.Types.StartTimeRange
import Network.AWS.DevOpsGuru.Types.UpdateCloudFormationCollectionFilter
import Network.AWS.DevOpsGuru.Types.UpdateResourceCollectionFilter
import Network.AWS.DevOpsGuru.Types.UpdateServiceIntegrationConfig
import Network.AWS.DevOpsGuru.UpdateResourceCollection
import Network.AWS.DevOpsGuru.UpdateServiceIntegration
