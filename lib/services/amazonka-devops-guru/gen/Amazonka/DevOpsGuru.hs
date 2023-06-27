{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.DevOpsGuru
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-12-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon DevOps Guru is a fully managed service that helps you identify
-- anomalous behavior in business critical operational applications. You
-- specify the Amazon Web Services resources that you want DevOps Guru to
-- cover, then the Amazon CloudWatch metrics and Amazon Web Services
-- CloudTrail events related to those resources are analyzed. When
-- anomalous behavior is detected, DevOps Guru creates an /insight/ that
-- includes recommendations, related events, and related metrics that can
-- help you improve your operational applications. For more information,
-- see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/welcome.html What is Amazon DevOps Guru>.
--
-- You can specify 1 or 2 Amazon Simple Notification Service topics so you
-- are notified every time a new insight is created. You can also enable
-- DevOps Guru to generate an OpsItem in Amazon Web Services Systems
-- Manager for each insight to help you manage and track your work
-- addressing insights.
--
-- To learn about the DevOps Guru workflow, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/welcome.html#how-it-works How DevOps Guru works>.
-- To learn about DevOps Guru concepts, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/concepts.html Concepts in DevOps Guru>.
module Amazonka.DevOpsGuru
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AddNotificationChannel
    AddNotificationChannel (AddNotificationChannel'),
    newAddNotificationChannel,
    AddNotificationChannelResponse (AddNotificationChannelResponse'),
    newAddNotificationChannelResponse,

    -- ** DeleteInsight
    DeleteInsight (DeleteInsight'),
    newDeleteInsight,
    DeleteInsightResponse (DeleteInsightResponse'),
    newDeleteInsightResponse,

    -- ** DescribeAccountHealth
    DescribeAccountHealth (DescribeAccountHealth'),
    newDescribeAccountHealth,
    DescribeAccountHealthResponse (DescribeAccountHealthResponse'),
    newDescribeAccountHealthResponse,

    -- ** DescribeAccountOverview
    DescribeAccountOverview (DescribeAccountOverview'),
    newDescribeAccountOverview,
    DescribeAccountOverviewResponse (DescribeAccountOverviewResponse'),
    newDescribeAccountOverviewResponse,

    -- ** DescribeAnomaly
    DescribeAnomaly (DescribeAnomaly'),
    newDescribeAnomaly,
    DescribeAnomalyResponse (DescribeAnomalyResponse'),
    newDescribeAnomalyResponse,

    -- ** DescribeEventSourcesConfig
    DescribeEventSourcesConfig (DescribeEventSourcesConfig'),
    newDescribeEventSourcesConfig,
    DescribeEventSourcesConfigResponse (DescribeEventSourcesConfigResponse'),
    newDescribeEventSourcesConfigResponse,

    -- ** DescribeFeedback
    DescribeFeedback (DescribeFeedback'),
    newDescribeFeedback,
    DescribeFeedbackResponse (DescribeFeedbackResponse'),
    newDescribeFeedbackResponse,

    -- ** DescribeInsight
    DescribeInsight (DescribeInsight'),
    newDescribeInsight,
    DescribeInsightResponse (DescribeInsightResponse'),
    newDescribeInsightResponse,

    -- ** DescribeOrganizationHealth
    DescribeOrganizationHealth (DescribeOrganizationHealth'),
    newDescribeOrganizationHealth,
    DescribeOrganizationHealthResponse (DescribeOrganizationHealthResponse'),
    newDescribeOrganizationHealthResponse,

    -- ** DescribeOrganizationOverview
    DescribeOrganizationOverview (DescribeOrganizationOverview'),
    newDescribeOrganizationOverview,
    DescribeOrganizationOverviewResponse (DescribeOrganizationOverviewResponse'),
    newDescribeOrganizationOverviewResponse,

    -- ** DescribeOrganizationResourceCollectionHealth (Paginated)
    DescribeOrganizationResourceCollectionHealth (DescribeOrganizationResourceCollectionHealth'),
    newDescribeOrganizationResourceCollectionHealth,
    DescribeOrganizationResourceCollectionHealthResponse (DescribeOrganizationResourceCollectionHealthResponse'),
    newDescribeOrganizationResourceCollectionHealthResponse,

    -- ** DescribeResourceCollectionHealth (Paginated)
    DescribeResourceCollectionHealth (DescribeResourceCollectionHealth'),
    newDescribeResourceCollectionHealth,
    DescribeResourceCollectionHealthResponse (DescribeResourceCollectionHealthResponse'),
    newDescribeResourceCollectionHealthResponse,

    -- ** DescribeServiceIntegration
    DescribeServiceIntegration (DescribeServiceIntegration'),
    newDescribeServiceIntegration,
    DescribeServiceIntegrationResponse (DescribeServiceIntegrationResponse'),
    newDescribeServiceIntegrationResponse,

    -- ** GetCostEstimation (Paginated)
    GetCostEstimation (GetCostEstimation'),
    newGetCostEstimation,
    GetCostEstimationResponse (GetCostEstimationResponse'),
    newGetCostEstimationResponse,

    -- ** GetResourceCollection (Paginated)
    GetResourceCollection (GetResourceCollection'),
    newGetResourceCollection,
    GetResourceCollectionResponse (GetResourceCollectionResponse'),
    newGetResourceCollectionResponse,

    -- ** ListAnomaliesForInsight (Paginated)
    ListAnomaliesForInsight (ListAnomaliesForInsight'),
    newListAnomaliesForInsight,
    ListAnomaliesForInsightResponse (ListAnomaliesForInsightResponse'),
    newListAnomaliesForInsightResponse,

    -- ** ListAnomalousLogGroups (Paginated)
    ListAnomalousLogGroups (ListAnomalousLogGroups'),
    newListAnomalousLogGroups,
    ListAnomalousLogGroupsResponse (ListAnomalousLogGroupsResponse'),
    newListAnomalousLogGroupsResponse,

    -- ** ListEvents (Paginated)
    ListEvents (ListEvents'),
    newListEvents,
    ListEventsResponse (ListEventsResponse'),
    newListEventsResponse,

    -- ** ListInsights (Paginated)
    ListInsights (ListInsights'),
    newListInsights,
    ListInsightsResponse (ListInsightsResponse'),
    newListInsightsResponse,

    -- ** ListMonitoredResources (Paginated)
    ListMonitoredResources (ListMonitoredResources'),
    newListMonitoredResources,
    ListMonitoredResourcesResponse (ListMonitoredResourcesResponse'),
    newListMonitoredResourcesResponse,

    -- ** ListNotificationChannels (Paginated)
    ListNotificationChannels (ListNotificationChannels'),
    newListNotificationChannels,
    ListNotificationChannelsResponse (ListNotificationChannelsResponse'),
    newListNotificationChannelsResponse,

    -- ** ListOrganizationInsights (Paginated)
    ListOrganizationInsights (ListOrganizationInsights'),
    newListOrganizationInsights,
    ListOrganizationInsightsResponse (ListOrganizationInsightsResponse'),
    newListOrganizationInsightsResponse,

    -- ** ListRecommendations (Paginated)
    ListRecommendations (ListRecommendations'),
    newListRecommendations,
    ListRecommendationsResponse (ListRecommendationsResponse'),
    newListRecommendationsResponse,

    -- ** PutFeedback
    PutFeedback (PutFeedback'),
    newPutFeedback,
    PutFeedbackResponse (PutFeedbackResponse'),
    newPutFeedbackResponse,

    -- ** RemoveNotificationChannel
    RemoveNotificationChannel (RemoveNotificationChannel'),
    newRemoveNotificationChannel,
    RemoveNotificationChannelResponse (RemoveNotificationChannelResponse'),
    newRemoveNotificationChannelResponse,

    -- ** SearchInsights (Paginated)
    SearchInsights (SearchInsights'),
    newSearchInsights,
    SearchInsightsResponse (SearchInsightsResponse'),
    newSearchInsightsResponse,

    -- ** SearchOrganizationInsights (Paginated)
    SearchOrganizationInsights (SearchOrganizationInsights'),
    newSearchOrganizationInsights,
    SearchOrganizationInsightsResponse (SearchOrganizationInsightsResponse'),
    newSearchOrganizationInsightsResponse,

    -- ** StartCostEstimation
    StartCostEstimation (StartCostEstimation'),
    newStartCostEstimation,
    StartCostEstimationResponse (StartCostEstimationResponse'),
    newStartCostEstimationResponse,

    -- ** UpdateEventSourcesConfig
    UpdateEventSourcesConfig (UpdateEventSourcesConfig'),
    newUpdateEventSourcesConfig,
    UpdateEventSourcesConfigResponse (UpdateEventSourcesConfigResponse'),
    newUpdateEventSourcesConfigResponse,

    -- ** UpdateResourceCollection
    UpdateResourceCollection (UpdateResourceCollection'),
    newUpdateResourceCollection,
    UpdateResourceCollectionResponse (UpdateResourceCollectionResponse'),
    newUpdateResourceCollectionResponse,

    -- ** UpdateServiceIntegration
    UpdateServiceIntegration (UpdateServiceIntegration'),
    newUpdateServiceIntegration,
    UpdateServiceIntegrationResponse (UpdateServiceIntegrationResponse'),
    newUpdateServiceIntegrationResponse,

    -- * Types

    -- ** AnomalySeverity
    AnomalySeverity (..),

    -- ** AnomalyStatus
    AnomalyStatus (..),

    -- ** AnomalyType
    AnomalyType (..),

    -- ** CloudWatchMetricDataStatusCode
    CloudWatchMetricDataStatusCode (..),

    -- ** CloudWatchMetricsStat
    CloudWatchMetricsStat (..),

    -- ** CostEstimationServiceResourceState
    CostEstimationServiceResourceState (..),

    -- ** CostEstimationStatus
    CostEstimationStatus (..),

    -- ** EventClass
    EventClass (..),

    -- ** EventDataSource
    EventDataSource (..),

    -- ** EventSourceOptInStatus
    EventSourceOptInStatus (..),

    -- ** InsightFeedbackOption
    InsightFeedbackOption (..),

    -- ** InsightSeverity
    InsightSeverity (..),

    -- ** InsightStatus
    InsightStatus (..),

    -- ** InsightType
    InsightType (..),

    -- ** Locale
    Locale (..),

    -- ** LogAnomalyType
    LogAnomalyType (..),

    -- ** NotificationMessageType
    NotificationMessageType (..),

    -- ** OptInStatus
    OptInStatus (..),

    -- ** OrganizationResourceCollectionType
    OrganizationResourceCollectionType (..),

    -- ** ResourceCollectionType
    ResourceCollectionType (..),

    -- ** ResourcePermission
    ResourcePermission (..),

    -- ** ResourceTypeFilter
    ResourceTypeFilter (..),

    -- ** ServerSideEncryptionType
    ServerSideEncryptionType (..),

    -- ** ServiceName
    ServiceName (..),

    -- ** UpdateResourceCollectionAction
    UpdateResourceCollectionAction (..),

    -- ** AccountHealth
    AccountHealth (AccountHealth'),
    newAccountHealth,

    -- ** AccountInsightHealth
    AccountInsightHealth (AccountInsightHealth'),
    newAccountInsightHealth,

    -- ** AmazonCodeGuruProfilerIntegration
    AmazonCodeGuruProfilerIntegration (AmazonCodeGuruProfilerIntegration'),
    newAmazonCodeGuruProfilerIntegration,

    -- ** AnomalousLogGroup
    AnomalousLogGroup (AnomalousLogGroup'),
    newAnomalousLogGroup,

    -- ** AnomalyReportedTimeRange
    AnomalyReportedTimeRange (AnomalyReportedTimeRange'),
    newAnomalyReportedTimeRange,

    -- ** AnomalyResource
    AnomalyResource (AnomalyResource'),
    newAnomalyResource,

    -- ** AnomalySourceDetails
    AnomalySourceDetails (AnomalySourceDetails'),
    newAnomalySourceDetails,

    -- ** AnomalySourceMetadata
    AnomalySourceMetadata (AnomalySourceMetadata'),
    newAnomalySourceMetadata,

    -- ** AnomalyTimeRange
    AnomalyTimeRange (AnomalyTimeRange'),
    newAnomalyTimeRange,

    -- ** CloudFormationCollection
    CloudFormationCollection (CloudFormationCollection'),
    newCloudFormationCollection,

    -- ** CloudFormationCollectionFilter
    CloudFormationCollectionFilter (CloudFormationCollectionFilter'),
    newCloudFormationCollectionFilter,

    -- ** CloudFormationCostEstimationResourceCollectionFilter
    CloudFormationCostEstimationResourceCollectionFilter (CloudFormationCostEstimationResourceCollectionFilter'),
    newCloudFormationCostEstimationResourceCollectionFilter,

    -- ** CloudFormationHealth
    CloudFormationHealth (CloudFormationHealth'),
    newCloudFormationHealth,

    -- ** CloudWatchMetricsDataSummary
    CloudWatchMetricsDataSummary (CloudWatchMetricsDataSummary'),
    newCloudWatchMetricsDataSummary,

    -- ** CloudWatchMetricsDetail
    CloudWatchMetricsDetail (CloudWatchMetricsDetail'),
    newCloudWatchMetricsDetail,

    -- ** CloudWatchMetricsDimension
    CloudWatchMetricsDimension (CloudWatchMetricsDimension'),
    newCloudWatchMetricsDimension,

    -- ** CostEstimationResourceCollectionFilter
    CostEstimationResourceCollectionFilter (CostEstimationResourceCollectionFilter'),
    newCostEstimationResourceCollectionFilter,

    -- ** CostEstimationTimeRange
    CostEstimationTimeRange (CostEstimationTimeRange'),
    newCostEstimationTimeRange,

    -- ** EndTimeRange
    EndTimeRange (EndTimeRange'),
    newEndTimeRange,

    -- ** Event
    Event (Event'),
    newEvent,

    -- ** EventResource
    EventResource (EventResource'),
    newEventResource,

    -- ** EventSourcesConfig
    EventSourcesConfig (EventSourcesConfig'),
    newEventSourcesConfig,

    -- ** EventTimeRange
    EventTimeRange (EventTimeRange'),
    newEventTimeRange,

    -- ** InsightFeedback
    InsightFeedback (InsightFeedback'),
    newInsightFeedback,

    -- ** InsightHealth
    InsightHealth (InsightHealth'),
    newInsightHealth,

    -- ** InsightTimeRange
    InsightTimeRange (InsightTimeRange'),
    newInsightTimeRange,

    -- ** KMSServerSideEncryptionIntegration
    KMSServerSideEncryptionIntegration (KMSServerSideEncryptionIntegration'),
    newKMSServerSideEncryptionIntegration,

    -- ** KMSServerSideEncryptionIntegrationConfig
    KMSServerSideEncryptionIntegrationConfig (KMSServerSideEncryptionIntegrationConfig'),
    newKMSServerSideEncryptionIntegrationConfig,

    -- ** ListAnomaliesForInsightFilters
    ListAnomaliesForInsightFilters (ListAnomaliesForInsightFilters'),
    newListAnomaliesForInsightFilters,

    -- ** ListEventsFilters
    ListEventsFilters (ListEventsFilters'),
    newListEventsFilters,

    -- ** ListInsightsAnyStatusFilter
    ListInsightsAnyStatusFilter (ListInsightsAnyStatusFilter'),
    newListInsightsAnyStatusFilter,

    -- ** ListInsightsClosedStatusFilter
    ListInsightsClosedStatusFilter (ListInsightsClosedStatusFilter'),
    newListInsightsClosedStatusFilter,

    -- ** ListInsightsOngoingStatusFilter
    ListInsightsOngoingStatusFilter (ListInsightsOngoingStatusFilter'),
    newListInsightsOngoingStatusFilter,

    -- ** ListInsightsStatusFilter
    ListInsightsStatusFilter (ListInsightsStatusFilter'),
    newListInsightsStatusFilter,

    -- ** ListMonitoredResourcesFilters
    ListMonitoredResourcesFilters (ListMonitoredResourcesFilters'),
    newListMonitoredResourcesFilters,

    -- ** LogAnomalyClass
    LogAnomalyClass (LogAnomalyClass'),
    newLogAnomalyClass,

    -- ** LogAnomalyShowcase
    LogAnomalyShowcase (LogAnomalyShowcase'),
    newLogAnomalyShowcase,

    -- ** LogsAnomalyDetectionIntegration
    LogsAnomalyDetectionIntegration (LogsAnomalyDetectionIntegration'),
    newLogsAnomalyDetectionIntegration,

    -- ** LogsAnomalyDetectionIntegrationConfig
    LogsAnomalyDetectionIntegrationConfig (LogsAnomalyDetectionIntegrationConfig'),
    newLogsAnomalyDetectionIntegrationConfig,

    -- ** MonitoredResourceIdentifier
    MonitoredResourceIdentifier (MonitoredResourceIdentifier'),
    newMonitoredResourceIdentifier,

    -- ** NotificationChannel
    NotificationChannel (NotificationChannel'),
    newNotificationChannel,

    -- ** NotificationChannelConfig
    NotificationChannelConfig (NotificationChannelConfig'),
    newNotificationChannelConfig,

    -- ** NotificationFilterConfig
    NotificationFilterConfig (NotificationFilterConfig'),
    newNotificationFilterConfig,

    -- ** OpsCenterIntegration
    OpsCenterIntegration (OpsCenterIntegration'),
    newOpsCenterIntegration,

    -- ** OpsCenterIntegrationConfig
    OpsCenterIntegrationConfig (OpsCenterIntegrationConfig'),
    newOpsCenterIntegrationConfig,

    -- ** PerformanceInsightsMetricDimensionGroup
    PerformanceInsightsMetricDimensionGroup (PerformanceInsightsMetricDimensionGroup'),
    newPerformanceInsightsMetricDimensionGroup,

    -- ** PerformanceInsightsMetricQuery
    PerformanceInsightsMetricQuery (PerformanceInsightsMetricQuery'),
    newPerformanceInsightsMetricQuery,

    -- ** PerformanceInsightsMetricsDetail
    PerformanceInsightsMetricsDetail (PerformanceInsightsMetricsDetail'),
    newPerformanceInsightsMetricsDetail,

    -- ** PerformanceInsightsReferenceComparisonValues
    PerformanceInsightsReferenceComparisonValues (PerformanceInsightsReferenceComparisonValues'),
    newPerformanceInsightsReferenceComparisonValues,

    -- ** PerformanceInsightsReferenceData
    PerformanceInsightsReferenceData (PerformanceInsightsReferenceData'),
    newPerformanceInsightsReferenceData,

    -- ** PerformanceInsightsReferenceMetric
    PerformanceInsightsReferenceMetric (PerformanceInsightsReferenceMetric'),
    newPerformanceInsightsReferenceMetric,

    -- ** PerformanceInsightsReferenceScalar
    PerformanceInsightsReferenceScalar (PerformanceInsightsReferenceScalar'),
    newPerformanceInsightsReferenceScalar,

    -- ** PerformanceInsightsStat
    PerformanceInsightsStat (PerformanceInsightsStat'),
    newPerformanceInsightsStat,

    -- ** PredictionTimeRange
    PredictionTimeRange (PredictionTimeRange'),
    newPredictionTimeRange,

    -- ** ProactiveAnomaly
    ProactiveAnomaly (ProactiveAnomaly'),
    newProactiveAnomaly,

    -- ** ProactiveAnomalySummary
    ProactiveAnomalySummary (ProactiveAnomalySummary'),
    newProactiveAnomalySummary,

    -- ** ProactiveInsight
    ProactiveInsight (ProactiveInsight'),
    newProactiveInsight,

    -- ** ProactiveInsightSummary
    ProactiveInsightSummary (ProactiveInsightSummary'),
    newProactiveInsightSummary,

    -- ** ProactiveOrganizationInsightSummary
    ProactiveOrganizationInsightSummary (ProactiveOrganizationInsightSummary'),
    newProactiveOrganizationInsightSummary,

    -- ** ReactiveAnomaly
    ReactiveAnomaly (ReactiveAnomaly'),
    newReactiveAnomaly,

    -- ** ReactiveAnomalySummary
    ReactiveAnomalySummary (ReactiveAnomalySummary'),
    newReactiveAnomalySummary,

    -- ** ReactiveInsight
    ReactiveInsight (ReactiveInsight'),
    newReactiveInsight,

    -- ** ReactiveInsightSummary
    ReactiveInsightSummary (ReactiveInsightSummary'),
    newReactiveInsightSummary,

    -- ** ReactiveOrganizationInsightSummary
    ReactiveOrganizationInsightSummary (ReactiveOrganizationInsightSummary'),
    newReactiveOrganizationInsightSummary,

    -- ** Recommendation
    Recommendation (Recommendation'),
    newRecommendation,

    -- ** RecommendationRelatedAnomaly
    RecommendationRelatedAnomaly (RecommendationRelatedAnomaly'),
    newRecommendationRelatedAnomaly,

    -- ** RecommendationRelatedAnomalyResource
    RecommendationRelatedAnomalyResource (RecommendationRelatedAnomalyResource'),
    newRecommendationRelatedAnomalyResource,

    -- ** RecommendationRelatedAnomalySourceDetail
    RecommendationRelatedAnomalySourceDetail (RecommendationRelatedAnomalySourceDetail'),
    newRecommendationRelatedAnomalySourceDetail,

    -- ** RecommendationRelatedCloudWatchMetricsSourceDetail
    RecommendationRelatedCloudWatchMetricsSourceDetail (RecommendationRelatedCloudWatchMetricsSourceDetail'),
    newRecommendationRelatedCloudWatchMetricsSourceDetail,

    -- ** RecommendationRelatedEvent
    RecommendationRelatedEvent (RecommendationRelatedEvent'),
    newRecommendationRelatedEvent,

    -- ** RecommendationRelatedEventResource
    RecommendationRelatedEventResource (RecommendationRelatedEventResource'),
    newRecommendationRelatedEventResource,

    -- ** ResourceCollection
    ResourceCollection (ResourceCollection'),
    newResourceCollection,

    -- ** ResourceCollectionFilter
    ResourceCollectionFilter (ResourceCollectionFilter'),
    newResourceCollectionFilter,

    -- ** SearchInsightsFilters
    SearchInsightsFilters (SearchInsightsFilters'),
    newSearchInsightsFilters,

    -- ** SearchOrganizationInsightsFilters
    SearchOrganizationInsightsFilters (SearchOrganizationInsightsFilters'),
    newSearchOrganizationInsightsFilters,

    -- ** ServiceCollection
    ServiceCollection (ServiceCollection'),
    newServiceCollection,

    -- ** ServiceHealth
    ServiceHealth (ServiceHealth'),
    newServiceHealth,

    -- ** ServiceInsightHealth
    ServiceInsightHealth (ServiceInsightHealth'),
    newServiceInsightHealth,

    -- ** ServiceIntegrationConfig
    ServiceIntegrationConfig (ServiceIntegrationConfig'),
    newServiceIntegrationConfig,

    -- ** ServiceResourceCost
    ServiceResourceCost (ServiceResourceCost'),
    newServiceResourceCost,

    -- ** SnsChannelConfig
    SnsChannelConfig (SnsChannelConfig'),
    newSnsChannelConfig,

    -- ** StartTimeRange
    StartTimeRange (StartTimeRange'),
    newStartTimeRange,

    -- ** TagCollection
    TagCollection (TagCollection'),
    newTagCollection,

    -- ** TagCollectionFilter
    TagCollectionFilter (TagCollectionFilter'),
    newTagCollectionFilter,

    -- ** TagCostEstimationResourceCollectionFilter
    TagCostEstimationResourceCollectionFilter (TagCostEstimationResourceCollectionFilter'),
    newTagCostEstimationResourceCollectionFilter,

    -- ** TagHealth
    TagHealth (TagHealth'),
    newTagHealth,

    -- ** TimestampMetricValuePair
    TimestampMetricValuePair (TimestampMetricValuePair'),
    newTimestampMetricValuePair,

    -- ** UpdateCloudFormationCollectionFilter
    UpdateCloudFormationCollectionFilter (UpdateCloudFormationCollectionFilter'),
    newUpdateCloudFormationCollectionFilter,

    -- ** UpdateResourceCollectionFilter
    UpdateResourceCollectionFilter (UpdateResourceCollectionFilter'),
    newUpdateResourceCollectionFilter,

    -- ** UpdateServiceIntegrationConfig
    UpdateServiceIntegrationConfig (UpdateServiceIntegrationConfig'),
    newUpdateServiceIntegrationConfig,

    -- ** UpdateTagCollectionFilter
    UpdateTagCollectionFilter (UpdateTagCollectionFilter'),
    newUpdateTagCollectionFilter,
  )
where

import Amazonka.DevOpsGuru.AddNotificationChannel
import Amazonka.DevOpsGuru.DeleteInsight
import Amazonka.DevOpsGuru.DescribeAccountHealth
import Amazonka.DevOpsGuru.DescribeAccountOverview
import Amazonka.DevOpsGuru.DescribeAnomaly
import Amazonka.DevOpsGuru.DescribeEventSourcesConfig
import Amazonka.DevOpsGuru.DescribeFeedback
import Amazonka.DevOpsGuru.DescribeInsight
import Amazonka.DevOpsGuru.DescribeOrganizationHealth
import Amazonka.DevOpsGuru.DescribeOrganizationOverview
import Amazonka.DevOpsGuru.DescribeOrganizationResourceCollectionHealth
import Amazonka.DevOpsGuru.DescribeResourceCollectionHealth
import Amazonka.DevOpsGuru.DescribeServiceIntegration
import Amazonka.DevOpsGuru.GetCostEstimation
import Amazonka.DevOpsGuru.GetResourceCollection
import Amazonka.DevOpsGuru.Lens
import Amazonka.DevOpsGuru.ListAnomaliesForInsight
import Amazonka.DevOpsGuru.ListAnomalousLogGroups
import Amazonka.DevOpsGuru.ListEvents
import Amazonka.DevOpsGuru.ListInsights
import Amazonka.DevOpsGuru.ListMonitoredResources
import Amazonka.DevOpsGuru.ListNotificationChannels
import Amazonka.DevOpsGuru.ListOrganizationInsights
import Amazonka.DevOpsGuru.ListRecommendations
import Amazonka.DevOpsGuru.PutFeedback
import Amazonka.DevOpsGuru.RemoveNotificationChannel
import Amazonka.DevOpsGuru.SearchInsights
import Amazonka.DevOpsGuru.SearchOrganizationInsights
import Amazonka.DevOpsGuru.StartCostEstimation
import Amazonka.DevOpsGuru.Types
import Amazonka.DevOpsGuru.UpdateEventSourcesConfig
import Amazonka.DevOpsGuru.UpdateResourceCollection
import Amazonka.DevOpsGuru.UpdateServiceIntegration
import Amazonka.DevOpsGuru.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'DevOpsGuru'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
