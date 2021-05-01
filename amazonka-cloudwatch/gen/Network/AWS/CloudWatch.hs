{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon CloudWatch monitors your Amazon Web Services (AWS) resources and
-- the applications you run on AWS in real time. You can use CloudWatch to
-- collect and track metrics, which are the variables you want to measure
-- for your resources and applications.
--
-- CloudWatch alarms send notifications or automatically change the
-- resources you are monitoring based on rules that you define. For
-- example, you can monitor the CPU usage and disk reads and writes of your
-- Amazon EC2 instances. Then, use this data to determine whether you
-- should launch additional instances to handle increased load. You can
-- also use this data to stop under-used instances to save money.
--
-- In addition to monitoring the built-in metrics that come with AWS, you
-- can monitor your own custom metrics. With CloudWatch, you gain
-- system-wide visibility into resource utilization, application
-- performance, and operational health.
module Network.AWS.CloudWatch
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** DashboardNotFoundError
    _DashboardNotFoundError,

    -- ** LimitExceededFault
    _LimitExceededFault,

    -- ** InvalidParameterCombinationException
    _InvalidParameterCombinationException,

    -- ** MissingRequiredParameterException
    _MissingRequiredParameterException,

    -- ** DashboardInvalidInputError
    _DashboardInvalidInputError,

    -- ** InternalServiceFault
    _InternalServiceFault,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceNotFound
    _ResourceNotFound,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InvalidFormatFault
    _InvalidFormatFault,

    -- ** InvalidNextToken
    _InvalidNextToken,

    -- * Waiters
    -- $waiters

    -- ** AlarmExists
    newAlarmExists,

    -- ** CompositeAlarmExists
    newCompositeAlarmExists,

    -- * Operations
    -- $operations

    -- ** EnableAlarmActions
    EnableAlarmActions (EnableAlarmActions'),
    newEnableAlarmActions,
    EnableAlarmActionsResponse (EnableAlarmActionsResponse'),
    newEnableAlarmActionsResponse,

    -- ** GetMetricStatistics
    GetMetricStatistics (GetMetricStatistics'),
    newGetMetricStatistics,
    GetMetricStatisticsResponse (GetMetricStatisticsResponse'),
    newGetMetricStatisticsResponse,

    -- ** PutInsightRule
    PutInsightRule (PutInsightRule'),
    newPutInsightRule,
    PutInsightRuleResponse (PutInsightRuleResponse'),
    newPutInsightRuleResponse,

    -- ** DeleteAlarms
    DeleteAlarms (DeleteAlarms'),
    newDeleteAlarms,
    DeleteAlarmsResponse (DeleteAlarmsResponse'),
    newDeleteAlarmsResponse,

    -- ** GetMetricWidgetImage
    GetMetricWidgetImage (GetMetricWidgetImage'),
    newGetMetricWidgetImage,
    GetMetricWidgetImageResponse (GetMetricWidgetImageResponse'),
    newGetMetricWidgetImageResponse,

    -- ** DescribeInsightRules
    DescribeInsightRules (DescribeInsightRules'),
    newDescribeInsightRules,
    DescribeInsightRulesResponse (DescribeInsightRulesResponse'),
    newDescribeInsightRulesResponse,

    -- ** PutMetricAlarm
    PutMetricAlarm (PutMetricAlarm'),
    newPutMetricAlarm,
    PutMetricAlarmResponse (PutMetricAlarmResponse'),
    newPutMetricAlarmResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** GetInsightRuleReport
    GetInsightRuleReport (GetInsightRuleReport'),
    newGetInsightRuleReport,
    GetInsightRuleReportResponse (GetInsightRuleReportResponse'),
    newGetInsightRuleReportResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DescribeAnomalyDetectors
    DescribeAnomalyDetectors (DescribeAnomalyDetectors'),
    newDescribeAnomalyDetectors,
    DescribeAnomalyDetectorsResponse (DescribeAnomalyDetectorsResponse'),
    newDescribeAnomalyDetectorsResponse,

    -- ** ListMetrics (Paginated)
    ListMetrics (ListMetrics'),
    newListMetrics,
    ListMetricsResponse (ListMetricsResponse'),
    newListMetricsResponse,

    -- ** PutMetricData
    PutMetricData (PutMetricData'),
    newPutMetricData,
    PutMetricDataResponse (PutMetricDataResponse'),
    newPutMetricDataResponse,

    -- ** PutDashboard
    PutDashboard (PutDashboard'),
    newPutDashboard,
    PutDashboardResponse (PutDashboardResponse'),
    newPutDashboardResponse,

    -- ** DescribeAlarmsForMetric
    DescribeAlarmsForMetric (DescribeAlarmsForMetric'),
    newDescribeAlarmsForMetric,
    DescribeAlarmsForMetricResponse (DescribeAlarmsForMetricResponse'),
    newDescribeAlarmsForMetricResponse,

    -- ** PutAnomalyDetector
    PutAnomalyDetector (PutAnomalyDetector'),
    newPutAnomalyDetector,
    PutAnomalyDetectorResponse (PutAnomalyDetectorResponse'),
    newPutAnomalyDetectorResponse,

    -- ** DeleteInsightRules
    DeleteInsightRules (DeleteInsightRules'),
    newDeleteInsightRules,
    DeleteInsightRulesResponse (DeleteInsightRulesResponse'),
    newDeleteInsightRulesResponse,

    -- ** DisableAlarmActions
    DisableAlarmActions (DisableAlarmActions'),
    newDisableAlarmActions,
    DisableAlarmActionsResponse (DisableAlarmActionsResponse'),
    newDisableAlarmActionsResponse,

    -- ** GetDashboard
    GetDashboard (GetDashboard'),
    newGetDashboard,
    GetDashboardResponse (GetDashboardResponse'),
    newGetDashboardResponse,

    -- ** PutCompositeAlarm
    PutCompositeAlarm (PutCompositeAlarm'),
    newPutCompositeAlarm,
    PutCompositeAlarmResponse (PutCompositeAlarmResponse'),
    newPutCompositeAlarmResponse,

    -- ** DisableInsightRules
    DisableInsightRules (DisableInsightRules'),
    newDisableInsightRules,
    DisableInsightRulesResponse (DisableInsightRulesResponse'),
    newDisableInsightRulesResponse,

    -- ** DescribeAlarmHistory (Paginated)
    DescribeAlarmHistory (DescribeAlarmHistory'),
    newDescribeAlarmHistory,
    DescribeAlarmHistoryResponse (DescribeAlarmHistoryResponse'),
    newDescribeAlarmHistoryResponse,

    -- ** DeleteDashboards
    DeleteDashboards (DeleteDashboards'),
    newDeleteDashboards,
    DeleteDashboardsResponse (DeleteDashboardsResponse'),
    newDeleteDashboardsResponse,

    -- ** SetAlarmState
    SetAlarmState (SetAlarmState'),
    newSetAlarmState,
    SetAlarmStateResponse (SetAlarmStateResponse'),
    newSetAlarmStateResponse,

    -- ** ListDashboards (Paginated)
    ListDashboards (ListDashboards'),
    newListDashboards,
    ListDashboardsResponse (ListDashboardsResponse'),
    newListDashboardsResponse,

    -- ** DescribeAlarms (Paginated)
    DescribeAlarms (DescribeAlarms'),
    newDescribeAlarms,
    DescribeAlarmsResponse (DescribeAlarmsResponse'),
    newDescribeAlarmsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DeleteAnomalyDetector
    DeleteAnomalyDetector (DeleteAnomalyDetector'),
    newDeleteAnomalyDetector,
    DeleteAnomalyDetectorResponse (DeleteAnomalyDetectorResponse'),
    newDeleteAnomalyDetectorResponse,

    -- ** EnableInsightRules
    EnableInsightRules (EnableInsightRules'),
    newEnableInsightRules,
    EnableInsightRulesResponse (EnableInsightRulesResponse'),
    newEnableInsightRulesResponse,

    -- ** GetMetricData (Paginated)
    GetMetricData (GetMetricData'),
    newGetMetricData,
    GetMetricDataResponse (GetMetricDataResponse'),
    newGetMetricDataResponse,

    -- * Types

    -- ** AlarmType
    AlarmType (..),

    -- ** AnomalyDetectorStateValue
    AnomalyDetectorStateValue (..),

    -- ** ComparisonOperator
    ComparisonOperator (..),

    -- ** HistoryItemType
    HistoryItemType (..),

    -- ** RecentlyActive
    RecentlyActive (..),

    -- ** ScanBy
    ScanBy (..),

    -- ** StandardUnit
    StandardUnit (..),

    -- ** StateValue
    StateValue (..),

    -- ** Statistic
    Statistic (..),

    -- ** StatusCode
    StatusCode (..),

    -- ** AlarmHistoryItem
    AlarmHistoryItem (AlarmHistoryItem'),
    newAlarmHistoryItem,

    -- ** AnomalyDetector
    AnomalyDetector (AnomalyDetector'),
    newAnomalyDetector,

    -- ** AnomalyDetectorConfiguration
    AnomalyDetectorConfiguration (AnomalyDetectorConfiguration'),
    newAnomalyDetectorConfiguration,

    -- ** CompositeAlarm
    CompositeAlarm (CompositeAlarm'),
    newCompositeAlarm,

    -- ** DashboardEntry
    DashboardEntry (DashboardEntry'),
    newDashboardEntry,

    -- ** DashboardValidationMessage
    DashboardValidationMessage (DashboardValidationMessage'),
    newDashboardValidationMessage,

    -- ** Datapoint
    Datapoint (Datapoint'),
    newDatapoint,

    -- ** Dimension
    Dimension (Dimension'),
    newDimension,

    -- ** DimensionFilter
    DimensionFilter (DimensionFilter'),
    newDimensionFilter,

    -- ** InsightRule
    InsightRule (InsightRule'),
    newInsightRule,

    -- ** InsightRuleContributor
    InsightRuleContributor (InsightRuleContributor'),
    newInsightRuleContributor,

    -- ** InsightRuleContributorDatapoint
    InsightRuleContributorDatapoint (InsightRuleContributorDatapoint'),
    newInsightRuleContributorDatapoint,

    -- ** InsightRuleMetricDatapoint
    InsightRuleMetricDatapoint (InsightRuleMetricDatapoint'),
    newInsightRuleMetricDatapoint,

    -- ** LabelOptions
    LabelOptions (LabelOptions'),
    newLabelOptions,

    -- ** MessageData
    MessageData (MessageData'),
    newMessageData,

    -- ** Metric
    Metric (Metric'),
    newMetric,

    -- ** MetricAlarm
    MetricAlarm (MetricAlarm'),
    newMetricAlarm,

    -- ** MetricDataQuery
    MetricDataQuery (MetricDataQuery'),
    newMetricDataQuery,

    -- ** MetricDataResult
    MetricDataResult (MetricDataResult'),
    newMetricDataResult,

    -- ** MetricDatum
    MetricDatum (MetricDatum'),
    newMetricDatum,

    -- ** MetricStat
    MetricStat (MetricStat'),
    newMetricStat,

    -- ** PartialFailure
    PartialFailure (PartialFailure'),
    newPartialFailure,

    -- ** Range
    Range (Range'),
    newRange,

    -- ** StatisticSet
    StatisticSet (StatisticSet'),
    newStatisticSet,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Network.AWS.CloudWatch.DeleteAlarms
import Network.AWS.CloudWatch.DeleteAnomalyDetector
import Network.AWS.CloudWatch.DeleteDashboards
import Network.AWS.CloudWatch.DeleteInsightRules
import Network.AWS.CloudWatch.DescribeAlarmHistory
import Network.AWS.CloudWatch.DescribeAlarms
import Network.AWS.CloudWatch.DescribeAlarmsForMetric
import Network.AWS.CloudWatch.DescribeAnomalyDetectors
import Network.AWS.CloudWatch.DescribeInsightRules
import Network.AWS.CloudWatch.DisableAlarmActions
import Network.AWS.CloudWatch.DisableInsightRules
import Network.AWS.CloudWatch.EnableAlarmActions
import Network.AWS.CloudWatch.EnableInsightRules
import Network.AWS.CloudWatch.GetDashboard
import Network.AWS.CloudWatch.GetInsightRuleReport
import Network.AWS.CloudWatch.GetMetricData
import Network.AWS.CloudWatch.GetMetricStatistics
import Network.AWS.CloudWatch.GetMetricWidgetImage
import Network.AWS.CloudWatch.Lens
import Network.AWS.CloudWatch.ListDashboards
import Network.AWS.CloudWatch.ListMetrics
import Network.AWS.CloudWatch.ListTagsForResource
import Network.AWS.CloudWatch.PutAnomalyDetector
import Network.AWS.CloudWatch.PutCompositeAlarm
import Network.AWS.CloudWatch.PutDashboard
import Network.AWS.CloudWatch.PutInsightRule
import Network.AWS.CloudWatch.PutMetricAlarm
import Network.AWS.CloudWatch.PutMetricData
import Network.AWS.CloudWatch.SetAlarmState
import Network.AWS.CloudWatch.TagResource
import Network.AWS.CloudWatch.Types
import Network.AWS.CloudWatch.UntagResource
import Network.AWS.CloudWatch.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CloudWatch'.

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
