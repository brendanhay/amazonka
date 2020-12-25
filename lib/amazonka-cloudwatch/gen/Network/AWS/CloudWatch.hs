{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon CloudWatch monitors your Amazon Web Services (AWS) resources and the applications you run on AWS in real time. You can use CloudWatch to collect and track metrics, which are the variables you want to measure for your resources and applications.
--
-- CloudWatch alarms send notifications or automatically change the resources you are monitoring based on rules that you define. For example, you can monitor the CPU usage and disk reads and writes of your Amazon EC2 instances. Then, use this data to determine whether you should launch additional instances to handle increased load. You can also use this data to stop under-used instances to save money.
-- In addition to monitoring the built-in metrics that come with AWS, you can monitor your own custom metrics. With CloudWatch, you gain system-wide visibility into resource utilization, application performance, and operational health.
module Network.AWS.CloudWatch
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** LimitExceededFault
    _LimitExceededFault,

    -- ** DashboardNotFoundError
    _DashboardNotFoundError,

    -- ** InvalidNextToken
    _InvalidNextToken,

    -- ** InternalServiceFault
    _InternalServiceFault,

    -- ** DashboardInvalidInputError
    _DashboardInvalidInputError,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** InvalidFormatFault
    _InvalidFormatFault,

    -- ** MissingRequiredParameterException
    _MissingRequiredParameterException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InvalidParameterCombinationException
    _InvalidParameterCombinationException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceNotFound
    _ResourceNotFound,

    -- * Waiters
    -- $waiters

    -- ** CompositeAlarmExists
    mkCompositeAlarmExists,

    -- ** AlarmExists
    mkAlarmExists,

    -- * Operations
    -- $operations

    -- ** EnableAlarmActions
    module Network.AWS.CloudWatch.EnableAlarmActions,

    -- ** DisableInsightRules
    module Network.AWS.CloudWatch.DisableInsightRules,

    -- ** PutCompositeAlarm
    module Network.AWS.CloudWatch.PutCompositeAlarm,

    -- ** DeleteAnomalyDetector
    module Network.AWS.CloudWatch.DeleteAnomalyDetector,

    -- ** DeleteInsightRules
    module Network.AWS.CloudWatch.DeleteInsightRules,

    -- ** GetDashboard
    module Network.AWS.CloudWatch.GetDashboard,

    -- ** PutAnomalyDetector
    module Network.AWS.CloudWatch.PutAnomalyDetector,

    -- ** ListTagsForResource
    module Network.AWS.CloudWatch.ListTagsForResource,

    -- ** GetMetricData (Paginated)
    module Network.AWS.CloudWatch.GetMetricData,

    -- ** PutMetricData
    module Network.AWS.CloudWatch.PutMetricData,

    -- ** ListDashboards (Paginated)
    module Network.AWS.CloudWatch.ListDashboards,

    -- ** DescribeAlarms (Paginated)
    module Network.AWS.CloudWatch.DescribeAlarms,

    -- ** ListMetrics (Paginated)
    module Network.AWS.CloudWatch.ListMetrics,

    -- ** GetInsightRuleReport
    module Network.AWS.CloudWatch.GetInsightRuleReport,

    -- ** DeleteDashboards
    module Network.AWS.CloudWatch.DeleteDashboards,

    -- ** PutInsightRule
    module Network.AWS.CloudWatch.PutInsightRule,

    -- ** GetMetricWidgetImage
    module Network.AWS.CloudWatch.GetMetricWidgetImage,

    -- ** DeleteAlarms
    module Network.AWS.CloudWatch.DeleteAlarms,

    -- ** DescribeAlarmHistory (Paginated)
    module Network.AWS.CloudWatch.DescribeAlarmHistory,

    -- ** GetMetricStatistics
    module Network.AWS.CloudWatch.GetMetricStatistics,

    -- ** DescribeAlarmsForMetric
    module Network.AWS.CloudWatch.DescribeAlarmsForMetric,

    -- ** EnableInsightRules
    module Network.AWS.CloudWatch.EnableInsightRules,

    -- ** DisableAlarmActions
    module Network.AWS.CloudWatch.DisableAlarmActions,

    -- ** DescribeAnomalyDetectors
    module Network.AWS.CloudWatch.DescribeAnomalyDetectors,

    -- ** PutDashboard
    module Network.AWS.CloudWatch.PutDashboard,

    -- ** TagResource
    module Network.AWS.CloudWatch.TagResource,

    -- ** UntagResource
    module Network.AWS.CloudWatch.UntagResource,

    -- ** PutMetricAlarm
    module Network.AWS.CloudWatch.PutMetricAlarm,

    -- ** SetAlarmState
    module Network.AWS.CloudWatch.SetAlarmState,

    -- ** DescribeInsightRules
    module Network.AWS.CloudWatch.DescribeInsightRules,

    -- * Types

    -- ** AlarmName
    AlarmName (..),

    -- ** DashboardValidationMessage
    DashboardValidationMessage (..),
    mkDashboardValidationMessage,
    dvmDataPath,
    dvmMessage,

    -- ** AlarmNamePrefix
    AlarmNamePrefix (..),

    -- ** StatisticSet
    StatisticSet (..),
    mkStatisticSet,
    ssSampleCount,
    ssSum,
    ssMinimum,
    ssMaximum,

    -- ** AnomalyDetectorMetricTimezone
    AnomalyDetectorMetricTimezone (..),

    -- ** MetricAlarm
    MetricAlarm (..),
    mkMetricAlarm,
    maActionsEnabled,
    maAlarmActions,
    maAlarmArn,
    maAlarmConfigurationUpdatedTimestamp,
    maAlarmDescription,
    maAlarmName,
    maComparisonOperator,
    maDatapointsToAlarm,
    maDimensions,
    maEvaluateLowSampleCountPercentile,
    maEvaluationPeriods,
    maExtendedStatistic,
    maInsufficientDataActions,
    maMetricName,
    maMetrics,
    maNamespace,
    maOKActions,
    maPeriod,
    maStateReason,
    maStateReasonData,
    maStateUpdatedTimestamp,
    maStateValue,
    maStatistic,
    maThreshold,
    maThresholdMetricId,
    maTreatMissingData,
    maUnit,

    -- ** HistoryItemType
    HistoryItemType (..),

    -- ** InsightRuleState
    InsightRuleState (..),

    -- ** HistoryData
    HistoryData (..),

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** FailureResource
    FailureResource (..),

    -- ** TreatMissingData
    TreatMissingData (..),

    -- ** InsightRuleOrderBy
    InsightRuleOrderBy (..),

    -- ** ResourceName
    ResourceName (..),

    -- ** InsightRuleDefinition
    InsightRuleDefinition (..),

    -- ** MetricDatum
    MetricDatum (..),
    mkMetricDatum,
    mdMetricName,
    mdCounts,
    mdDimensions,
    mdStatisticValues,
    mdStorageResolution,
    mdTimestamp,
    mdUnit,
    mdValue,
    mdValues,

    -- ** AnomalyDetectorMetricStat
    AnomalyDetectorMetricStat (..),

    -- ** FailureCode
    FailureCode (..),

    -- ** DataPath
    DataPath (..),

    -- ** AlarmDescription
    AlarmDescription (..),

    -- ** StandardUnit
    StandardUnit (..),

    -- ** AlarmRule
    AlarmRule (..),

    -- ** InsightRule
    InsightRule (..),
    mkInsightRule,
    irName,
    irState,
    irSchema,
    irDefinition,

    -- ** InsightRuleMetricDatapoint
    InsightRuleMetricDatapoint (..),
    mkInsightRuleMetricDatapoint,
    irmdTimestamp,
    irmdAverage,
    irmdMaxContributorValue,
    irmdMaximum,
    irmdMinimum,
    irmdSampleCount,
    irmdSum,
    irmdUniqueContributors,

    -- ** MessageDataValue
    MessageDataValue (..),

    -- ** DashboardEntry
    DashboardEntry (..),
    mkDashboardEntry,
    deDashboardArn,
    deDashboardName,
    deLastModified,
    deSize,

    -- ** Dimension
    Dimension (..),
    mkDimension,
    dName,
    dValue,

    -- ** MetricName
    MetricName (..),

    -- ** AnomalyDetectorConfiguration
    AnomalyDetectorConfiguration (..),
    mkAnomalyDetectorConfiguration,
    adcExcludedTimeRanges,
    adcMetricTimezone,

    -- ** Namespace
    Namespace (..),

    -- ** DashboardName
    DashboardName (..),

    -- ** DashboardNamePrefix
    DashboardNamePrefix (..),

    -- ** AnomalyDetector
    AnomalyDetector (..),
    mkAnomalyDetector,
    adConfiguration,
    adDimensions,
    adMetricName,
    adNamespace,
    adStat,
    adStateValue,

    -- ** DimensionValue
    DimensionValue (..),

    -- ** InsightRuleMetricName
    InsightRuleMetricName (..),

    -- ** AlarmType
    AlarmType (..),

    -- ** ActionPrefix
    ActionPrefix (..),

    -- ** CompositeAlarm
    CompositeAlarm (..),
    mkCompositeAlarm,
    caActionsEnabled,
    caAlarmActions,
    caAlarmArn,
    caAlarmConfigurationUpdatedTimestamp,
    caAlarmDescription,
    caAlarmName,
    caAlarmRule,
    caInsufficientDataActions,
    caOKActions,
    caStateReason,
    caStateReasonData,
    caStateUpdatedTimestamp,
    caStateValue,

    -- ** FailureDescription
    FailureDescription (..),

    -- ** ComparisonOperator
    ComparisonOperator (..),

    -- ** MetricId
    MetricId (..),

    -- ** EvaluateLowSampleCountPercentile
    EvaluateLowSampleCountPercentile (..),

    -- ** InsightRuleContributorKey
    InsightRuleContributorKey (..),

    -- ** NextToken
    NextToken (..),

    -- ** ScanBy
    ScanBy (..),

    -- ** AlarmHistoryItem
    AlarmHistoryItem (..),
    mkAlarmHistoryItem,
    ahiAlarmName,
    ahiAlarmType,
    ahiHistoryData,
    ahiHistoryItemType,
    ahiHistorySummary,
    ahiTimestamp,

    -- ** Metric
    Metric (..),
    mkMetric,
    mDimensions,
    mMetricName,
    mNamespace,

    -- ** OutputFormat
    OutputFormat (..),

    -- ** StateValue
    StateValue (..),

    -- ** DashboardBody
    DashboardBody (..),

    -- ** Datapoint
    Datapoint (..),
    mkDatapoint,
    dAverage,
    dExtendedStatistics,
    dMaximum,
    dMinimum,
    dSampleCount,
    dSum,
    dTimestamp,
    dUnit,

    -- ** Range
    Range (..),
    mkRange,
    rStartTime,
    rEndTime,

    -- ** AnomalyDetectorStateValue
    AnomalyDetectorStateValue (..),

    -- ** InsightRuleName
    InsightRuleName (..),

    -- ** InsightRuleContributorDatapoint
    InsightRuleContributorDatapoint (..),
    mkInsightRuleContributorDatapoint,
    ircdTimestamp,
    ircdApproximateValue,

    -- ** ExceptionType
    ExceptionType (..),

    -- ** DimensionName
    DimensionName (..),

    -- ** MetricLabel
    MetricLabel (..),

    -- ** RecentlyActive
    RecentlyActive (..),

    -- ** Stat
    Stat (..),

    -- ** InsightRuleContributorKeyLabel
    InsightRuleContributorKeyLabel (..),

    -- ** MetricDataResult
    MetricDataResult (..),
    mkMetricDataResult,
    mdrId,
    mdrLabel,
    mdrMessages,
    mdrStatusCode,
    mdrTimestamps,
    mdrValues,

    -- ** TagKey
    TagKey (..),

    -- ** MetricWidget
    MetricWidget (..),

    -- ** DimensionFilter
    DimensionFilter (..),
    mkDimensionFilter,
    dfName,
    dfValue,

    -- ** StateReason
    StateReason (..),

    -- ** Message
    Message (..),

    -- ** AmazonResourceName
    AmazonResourceName (..),

    -- ** InsightRuleContributor
    InsightRuleContributor (..),
    mkInsightRuleContributor,
    ircKeys,
    ircApproximateAggregateValue,
    ircDatapoints,

    -- ** StateReasonData
    StateReasonData (..),

    -- ** MessageData
    MessageData (..),
    mkMessageData,
    mCode,
    mValue,

    -- ** PartialFailure
    PartialFailure (..),
    mkPartialFailure,
    pfExceptionType,
    pfFailureCode,
    pfFailureDescription,
    pfFailureResource,

    -- ** MetricDataQuery
    MetricDataQuery (..),
    mkMetricDataQuery,
    mdqId,
    mdqExpression,
    mdqLabel,
    mdqMetricStat,
    mdqPeriod,
    mdqReturnData,

    -- ** AlarmArn
    AlarmArn (..),

    -- ** HistorySummary
    HistorySummary (..),

    -- ** MetricStat
    MetricStat (..),
    mkMetricStat,
    msMetric,
    msPeriod,
    msStat,
    msUnit,

    -- ** Statistic
    Statistic (..),

    -- ** ExtendedStatistic
    ExtendedStatistic (..),

    -- ** DashboardArn
    DashboardArn (..),

    -- ** StatusCode
    StatusCode (..),

    -- ** ThresholdMetricId
    ThresholdMetricId (..),

    -- ** RuleName
    RuleName (..),

    -- ** RuleDefinition
    RuleDefinition (..),

    -- ** RuleState
    RuleState (..),

    -- ** Key
    Key (..),

    -- ** Value
    Value (..),

    -- ** Name
    Name (..),

    -- ** Schema
    Schema (..),

    -- ** Label
    Label (..),

    -- ** ResourceARN
    ResourceARN (..),

    -- ** AggregationStatistic
    AggregationStatistic (..),

    -- ** Code
    Code (..),

    -- ** Expression
    Expression (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
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
import qualified Network.AWS.Prelude as Lude

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
