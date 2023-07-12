{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CloudWatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2010-08-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon CloudWatch monitors your Amazon Web Services (Amazon Web
-- Services) resources and the applications you run on Amazon Web Services
-- in real time. You can use CloudWatch to collect and track metrics, which
-- are the variables you want to measure for your resources and
-- applications.
--
-- CloudWatch alarms send notifications or automatically change the
-- resources you are monitoring based on rules that you define. For
-- example, you can monitor the CPU usage and disk reads and writes of your
-- Amazon EC2 instances. Then, use this data to determine whether you
-- should launch additional instances to handle increased load. You can
-- also use this data to stop under-used instances to save money.
--
-- In addition to monitoring the built-in metrics that come with Amazon Web
-- Services, you can monitor your own custom metrics. With CloudWatch, you
-- gain system-wide visibility into resource utilization, application
-- performance, and operational health.
module Amazonka.CloudWatch
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** DashboardInvalidInputError
    _DashboardInvalidInputError,

    -- ** DashboardNotFoundError
    _DashboardNotFoundError,

    -- ** InternalServiceFault
    _InternalServiceFault,

    -- ** InvalidFormatFault
    _InvalidFormatFault,

    -- ** InvalidNextToken
    _InvalidNextToken,

    -- ** InvalidParameterCombinationException
    _InvalidParameterCombinationException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** LimitExceededFault
    _LimitExceededFault,

    -- ** MissingRequiredParameterException
    _MissingRequiredParameterException,

    -- ** ResourceNotFound
    _ResourceNotFound,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- ** AlarmExists
    newAlarmExists,

    -- ** CompositeAlarmExists
    newCompositeAlarmExists,

    -- * Operations
    -- $operations

    -- ** DeleteAlarms
    DeleteAlarms (DeleteAlarms'),
    newDeleteAlarms,
    DeleteAlarmsResponse (DeleteAlarmsResponse'),
    newDeleteAlarmsResponse,

    -- ** DeleteAnomalyDetector
    DeleteAnomalyDetector (DeleteAnomalyDetector'),
    newDeleteAnomalyDetector,
    DeleteAnomalyDetectorResponse (DeleteAnomalyDetectorResponse'),
    newDeleteAnomalyDetectorResponse,

    -- ** DeleteDashboards
    DeleteDashboards (DeleteDashboards'),
    newDeleteDashboards,
    DeleteDashboardsResponse (DeleteDashboardsResponse'),
    newDeleteDashboardsResponse,

    -- ** DeleteInsightRules
    DeleteInsightRules (DeleteInsightRules'),
    newDeleteInsightRules,
    DeleteInsightRulesResponse (DeleteInsightRulesResponse'),
    newDeleteInsightRulesResponse,

    -- ** DeleteMetricStream
    DeleteMetricStream (DeleteMetricStream'),
    newDeleteMetricStream,
    DeleteMetricStreamResponse (DeleteMetricStreamResponse'),
    newDeleteMetricStreamResponse,

    -- ** DescribeAlarmHistory (Paginated)
    DescribeAlarmHistory (DescribeAlarmHistory'),
    newDescribeAlarmHistory,
    DescribeAlarmHistoryResponse (DescribeAlarmHistoryResponse'),
    newDescribeAlarmHistoryResponse,

    -- ** DescribeAlarms (Paginated)
    DescribeAlarms (DescribeAlarms'),
    newDescribeAlarms,
    DescribeAlarmsResponse (DescribeAlarmsResponse'),
    newDescribeAlarmsResponse,

    -- ** DescribeAlarmsForMetric
    DescribeAlarmsForMetric (DescribeAlarmsForMetric'),
    newDescribeAlarmsForMetric,
    DescribeAlarmsForMetricResponse (DescribeAlarmsForMetricResponse'),
    newDescribeAlarmsForMetricResponse,

    -- ** DescribeAnomalyDetectors (Paginated)
    DescribeAnomalyDetectors (DescribeAnomalyDetectors'),
    newDescribeAnomalyDetectors,
    DescribeAnomalyDetectorsResponse (DescribeAnomalyDetectorsResponse'),
    newDescribeAnomalyDetectorsResponse,

    -- ** DescribeInsightRules
    DescribeInsightRules (DescribeInsightRules'),
    newDescribeInsightRules,
    DescribeInsightRulesResponse (DescribeInsightRulesResponse'),
    newDescribeInsightRulesResponse,

    -- ** DisableAlarmActions
    DisableAlarmActions (DisableAlarmActions'),
    newDisableAlarmActions,
    DisableAlarmActionsResponse (DisableAlarmActionsResponse'),
    newDisableAlarmActionsResponse,

    -- ** DisableInsightRules
    DisableInsightRules (DisableInsightRules'),
    newDisableInsightRules,
    DisableInsightRulesResponse (DisableInsightRulesResponse'),
    newDisableInsightRulesResponse,

    -- ** EnableAlarmActions
    EnableAlarmActions (EnableAlarmActions'),
    newEnableAlarmActions,
    EnableAlarmActionsResponse (EnableAlarmActionsResponse'),
    newEnableAlarmActionsResponse,

    -- ** EnableInsightRules
    EnableInsightRules (EnableInsightRules'),
    newEnableInsightRules,
    EnableInsightRulesResponse (EnableInsightRulesResponse'),
    newEnableInsightRulesResponse,

    -- ** GetDashboard
    GetDashboard (GetDashboard'),
    newGetDashboard,
    GetDashboardResponse (GetDashboardResponse'),
    newGetDashboardResponse,

    -- ** GetInsightRuleReport
    GetInsightRuleReport (GetInsightRuleReport'),
    newGetInsightRuleReport,
    GetInsightRuleReportResponse (GetInsightRuleReportResponse'),
    newGetInsightRuleReportResponse,

    -- ** GetMetricData (Paginated)
    GetMetricData (GetMetricData'),
    newGetMetricData,
    GetMetricDataResponse (GetMetricDataResponse'),
    newGetMetricDataResponse,

    -- ** GetMetricStatistics
    GetMetricStatistics (GetMetricStatistics'),
    newGetMetricStatistics,
    GetMetricStatisticsResponse (GetMetricStatisticsResponse'),
    newGetMetricStatisticsResponse,

    -- ** GetMetricStream
    GetMetricStream (GetMetricStream'),
    newGetMetricStream,
    GetMetricStreamResponse (GetMetricStreamResponse'),
    newGetMetricStreamResponse,

    -- ** GetMetricWidgetImage
    GetMetricWidgetImage (GetMetricWidgetImage'),
    newGetMetricWidgetImage,
    GetMetricWidgetImageResponse (GetMetricWidgetImageResponse'),
    newGetMetricWidgetImageResponse,

    -- ** ListDashboards (Paginated)
    ListDashboards (ListDashboards'),
    newListDashboards,
    ListDashboardsResponse (ListDashboardsResponse'),
    newListDashboardsResponse,

    -- ** ListManagedInsightRules
    ListManagedInsightRules (ListManagedInsightRules'),
    newListManagedInsightRules,
    ListManagedInsightRulesResponse (ListManagedInsightRulesResponse'),
    newListManagedInsightRulesResponse,

    -- ** ListMetricStreams
    ListMetricStreams (ListMetricStreams'),
    newListMetricStreams,
    ListMetricStreamsResponse (ListMetricStreamsResponse'),
    newListMetricStreamsResponse,

    -- ** ListMetrics (Paginated)
    ListMetrics (ListMetrics'),
    newListMetrics,
    ListMetricsResponse (ListMetricsResponse'),
    newListMetricsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutAnomalyDetector
    PutAnomalyDetector (PutAnomalyDetector'),
    newPutAnomalyDetector,
    PutAnomalyDetectorResponse (PutAnomalyDetectorResponse'),
    newPutAnomalyDetectorResponse,

    -- ** PutCompositeAlarm
    PutCompositeAlarm (PutCompositeAlarm'),
    newPutCompositeAlarm,
    PutCompositeAlarmResponse (PutCompositeAlarmResponse'),
    newPutCompositeAlarmResponse,

    -- ** PutDashboard
    PutDashboard (PutDashboard'),
    newPutDashboard,
    PutDashboardResponse (PutDashboardResponse'),
    newPutDashboardResponse,

    -- ** PutInsightRule
    PutInsightRule (PutInsightRule'),
    newPutInsightRule,
    PutInsightRuleResponse (PutInsightRuleResponse'),
    newPutInsightRuleResponse,

    -- ** PutManagedInsightRules
    PutManagedInsightRules (PutManagedInsightRules'),
    newPutManagedInsightRules,
    PutManagedInsightRulesResponse (PutManagedInsightRulesResponse'),
    newPutManagedInsightRulesResponse,

    -- ** PutMetricAlarm
    PutMetricAlarm (PutMetricAlarm'),
    newPutMetricAlarm,
    PutMetricAlarmResponse (PutMetricAlarmResponse'),
    newPutMetricAlarmResponse,

    -- ** PutMetricData
    PutMetricData (PutMetricData'),
    newPutMetricData,
    PutMetricDataResponse (PutMetricDataResponse'),
    newPutMetricDataResponse,

    -- ** PutMetricStream
    PutMetricStream (PutMetricStream'),
    newPutMetricStream,
    PutMetricStreamResponse (PutMetricStreamResponse'),
    newPutMetricStreamResponse,

    -- ** SetAlarmState
    SetAlarmState (SetAlarmState'),
    newSetAlarmState,
    SetAlarmStateResponse (SetAlarmStateResponse'),
    newSetAlarmStateResponse,

    -- ** StartMetricStreams
    StartMetricStreams (StartMetricStreams'),
    newStartMetricStreams,
    StartMetricStreamsResponse (StartMetricStreamsResponse'),
    newStartMetricStreamsResponse,

    -- ** StopMetricStreams
    StopMetricStreams (StopMetricStreams'),
    newStopMetricStreams,
    StopMetricStreamsResponse (StopMetricStreamsResponse'),
    newStopMetricStreamsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- * Types

    -- ** ActionsSuppressedBy
    ActionsSuppressedBy (..),

    -- ** AlarmType
    AlarmType (..),

    -- ** AnomalyDetectorStateValue
    AnomalyDetectorStateValue (..),

    -- ** AnomalyDetectorType
    AnomalyDetectorType (..),

    -- ** ComparisonOperator
    ComparisonOperator (..),

    -- ** EvaluationState
    EvaluationState (..),

    -- ** HistoryItemType
    HistoryItemType (..),

    -- ** MetricStreamOutputFormat
    MetricStreamOutputFormat (..),

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

    -- ** ManagedRule
    ManagedRule (ManagedRule'),
    newManagedRule,

    -- ** ManagedRuleDescription
    ManagedRuleDescription (ManagedRuleDescription'),
    newManagedRuleDescription,

    -- ** ManagedRuleState
    ManagedRuleState (ManagedRuleState'),
    newManagedRuleState,

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

    -- ** MetricMathAnomalyDetector
    MetricMathAnomalyDetector (MetricMathAnomalyDetector'),
    newMetricMathAnomalyDetector,

    -- ** MetricStat
    MetricStat (MetricStat'),
    newMetricStat,

    -- ** MetricStreamEntry
    MetricStreamEntry (MetricStreamEntry'),
    newMetricStreamEntry,

    -- ** MetricStreamFilter
    MetricStreamFilter (MetricStreamFilter'),
    newMetricStreamFilter,

    -- ** MetricStreamStatisticsConfiguration
    MetricStreamStatisticsConfiguration (MetricStreamStatisticsConfiguration'),
    newMetricStreamStatisticsConfiguration,

    -- ** MetricStreamStatisticsMetric
    MetricStreamStatisticsMetric (MetricStreamStatisticsMetric'),
    newMetricStreamStatisticsMetric,

    -- ** PartialFailure
    PartialFailure (PartialFailure'),
    newPartialFailure,

    -- ** Range
    Range (Range'),
    newRange,

    -- ** SingleMetricAnomalyDetector
    SingleMetricAnomalyDetector (SingleMetricAnomalyDetector'),
    newSingleMetricAnomalyDetector,

    -- ** StatisticSet
    StatisticSet (StatisticSet'),
    newStatisticSet,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Amazonka.CloudWatch.DeleteAlarms
import Amazonka.CloudWatch.DeleteAnomalyDetector
import Amazonka.CloudWatch.DeleteDashboards
import Amazonka.CloudWatch.DeleteInsightRules
import Amazonka.CloudWatch.DeleteMetricStream
import Amazonka.CloudWatch.DescribeAlarmHistory
import Amazonka.CloudWatch.DescribeAlarms
import Amazonka.CloudWatch.DescribeAlarmsForMetric
import Amazonka.CloudWatch.DescribeAnomalyDetectors
import Amazonka.CloudWatch.DescribeInsightRules
import Amazonka.CloudWatch.DisableAlarmActions
import Amazonka.CloudWatch.DisableInsightRules
import Amazonka.CloudWatch.EnableAlarmActions
import Amazonka.CloudWatch.EnableInsightRules
import Amazonka.CloudWatch.GetDashboard
import Amazonka.CloudWatch.GetInsightRuleReport
import Amazonka.CloudWatch.GetMetricData
import Amazonka.CloudWatch.GetMetricStatistics
import Amazonka.CloudWatch.GetMetricStream
import Amazonka.CloudWatch.GetMetricWidgetImage
import Amazonka.CloudWatch.Lens
import Amazonka.CloudWatch.ListDashboards
import Amazonka.CloudWatch.ListManagedInsightRules
import Amazonka.CloudWatch.ListMetricStreams
import Amazonka.CloudWatch.ListMetrics
import Amazonka.CloudWatch.ListTagsForResource
import Amazonka.CloudWatch.PutAnomalyDetector
import Amazonka.CloudWatch.PutCompositeAlarm
import Amazonka.CloudWatch.PutDashboard
import Amazonka.CloudWatch.PutInsightRule
import Amazonka.CloudWatch.PutManagedInsightRules
import Amazonka.CloudWatch.PutMetricAlarm
import Amazonka.CloudWatch.PutMetricData
import Amazonka.CloudWatch.PutMetricStream
import Amazonka.CloudWatch.SetAlarmState
import Amazonka.CloudWatch.StartMetricStreams
import Amazonka.CloudWatch.StopMetricStreams
import Amazonka.CloudWatch.TagResource
import Amazonka.CloudWatch.Types
import Amazonka.CloudWatch.UntagResource
import Amazonka.CloudWatch.Waiters

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
