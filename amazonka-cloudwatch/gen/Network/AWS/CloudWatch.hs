{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon CloudWatch monitors your Amazon Web Services (AWS) resources and the applications you run on AWS in real time. You can use CloudWatch to collect and track metrics, which are the variables you want to measure for your resources and applications.
--
--
-- CloudWatch alarms send notifications or automatically change the resources you are monitoring based on rules that you define. For example, you can monitor the CPU usage and disk reads and writes of your Amazon EC2 instances. Then, use this data to determine whether you should launch additional instances to handle increased load. You can also use this data to stop under-used instances to save money.
--
-- In addition to monitoring the built-in metrics that come with AWS, you can monitor your own custom metrics. With CloudWatch, you gain system-wide visibility into resource utilization, application performance, and operational health.
--
module Network.AWS.CloudWatch
    (
    -- * Service Configuration
      cloudWatch

    -- * Errors
    -- $errors

    -- ** LimitExceededFault
    , _LimitExceededFault

    -- ** DashboardNotFoundError
    , _DashboardNotFoundError

    -- ** InvalidNextToken
    , _InvalidNextToken

    -- ** InternalServiceFault
    , _InternalServiceFault

    -- ** DashboardInvalidInputError
    , _DashboardInvalidInputError

    -- ** InvalidParameterValueException
    , _InvalidParameterValueException

    -- ** InvalidFormatFault
    , _InvalidFormatFault

    -- ** MissingRequiredParameterException
    , _MissingRequiredParameterException

    -- ** InvalidParameterCombinationException
    , _InvalidParameterCombinationException

    -- ** ResourceNotFound
    , _ResourceNotFound

    -- * Waiters
    -- $waiters

    -- ** AlarmExists
    , alarmExists

    -- * Operations
    -- $operations

    -- ** EnableAlarmActions
    , module Network.AWS.CloudWatch.EnableAlarmActions

    -- ** GetDashboard
    , module Network.AWS.CloudWatch.GetDashboard

    -- ** GetMetricData
    , module Network.AWS.CloudWatch.GetMetricData

    -- ** PutMetricData
    , module Network.AWS.CloudWatch.PutMetricData

    -- ** ListDashboards (Paginated)
    , module Network.AWS.CloudWatch.ListDashboards

    -- ** DescribeAlarms (Paginated)
    , module Network.AWS.CloudWatch.DescribeAlarms

    -- ** ListMetrics (Paginated)
    , module Network.AWS.CloudWatch.ListMetrics

    -- ** DeleteDashboards
    , module Network.AWS.CloudWatch.DeleteDashboards

    -- ** DeleteAlarms
    , module Network.AWS.CloudWatch.DeleteAlarms

    -- ** DescribeAlarmHistory (Paginated)
    , module Network.AWS.CloudWatch.DescribeAlarmHistory

    -- ** GetMetricStatistics
    , module Network.AWS.CloudWatch.GetMetricStatistics

    -- ** DescribeAlarmsForMetric
    , module Network.AWS.CloudWatch.DescribeAlarmsForMetric

    -- ** DisableAlarmActions
    , module Network.AWS.CloudWatch.DisableAlarmActions

    -- ** PutDashboard
    , module Network.AWS.CloudWatch.PutDashboard

    -- ** PutMetricAlarm
    , module Network.AWS.CloudWatch.PutMetricAlarm

    -- ** SetAlarmState
    , module Network.AWS.CloudWatch.SetAlarmState

    -- * Types

    -- ** ComparisonOperator
    , ComparisonOperator (..)

    -- ** HistoryItemType
    , HistoryItemType (..)

    -- ** ScanBy
    , ScanBy (..)

    -- ** StandardUnit
    , StandardUnit (..)

    -- ** StateValue
    , StateValue (..)

    -- ** Statistic
    , Statistic (..)

    -- ** StatusCode
    , StatusCode (..)

    -- ** AlarmHistoryItem
    , AlarmHistoryItem
    , alarmHistoryItem
    , ahiAlarmName
    , ahiHistoryItemType
    , ahiHistoryData
    , ahiHistorySummary
    , ahiTimestamp

    -- ** DashboardEntry
    , DashboardEntry
    , dashboardEntry
    , deSize
    , deDashboardName
    , deLastModified
    , deDashboardARN

    -- ** DashboardValidationMessage
    , DashboardValidationMessage
    , dashboardValidationMessage
    , dvmDataPath
    , dvmMessage

    -- ** Datapoint
    , Datapoint
    , datapoint
    , dSampleCount
    , dMaximum
    , dAverage
    , dMinimum
    , dExtendedStatistics
    , dSum
    , dUnit
    , dTimestamp

    -- ** Dimension
    , Dimension
    , dimension
    , dName
    , dValue

    -- ** DimensionFilter
    , DimensionFilter
    , dimensionFilter
    , dfValue
    , dfName

    -- ** MessageData
    , MessageData
    , messageData
    , mValue
    , mCode

    -- ** Metric
    , Metric
    , metric
    , mMetricName
    , mNamespace
    , mDimensions

    -- ** MetricAlarm
    , MetricAlarm
    , metricAlarm
    , maAlarmName
    , maStateUpdatedTimestamp
    , maTreatMissingData
    , maPeriod
    , maAlarmDescription
    , maEvaluationPeriods
    , maMetricName
    , maNamespace
    , maComparisonOperator
    , maOKActions
    , maEvaluateLowSampleCountPercentile
    , maStateValue
    , maDatapointsToAlarm
    , maThreshold
    , maAlarmConfigurationUpdatedTimestamp
    , maActionsEnabled
    , maInsufficientDataActions
    , maStateReason
    , maStateReasonData
    , maDimensions
    , maAlarmARN
    , maAlarmActions
    , maUnit
    , maStatistic
    , maExtendedStatistic

    -- ** MetricDataQuery
    , MetricDataQuery
    , metricDataQuery
    , mdqReturnData
    , mdqExpression
    , mdqLabel
    , mdqMetricStat
    , mdqId

    -- ** MetricDataResult
    , MetricDataResult
    , metricDataResult
    , mdrValues
    , mdrId
    , mdrTimestamps
    , mdrMessages
    , mdrLabel
    , mdrStatusCode

    -- ** MetricDatum
    , MetricDatum
    , metricDatum
    , mdValue
    , mdStorageResolution
    , mdDimensions
    , mdUnit
    , mdTimestamp
    , mdStatisticValues
    , mdMetricName

    -- ** MetricStat
    , MetricStat
    , metricStat
    , msUnit
    , msMetric
    , msPeriod
    , msStat

    -- ** StatisticSet
    , StatisticSet
    , statisticSet
    , ssSampleCount
    , ssSum
    , ssMinimum
    , ssMaximum
    ) where

import Network.AWS.CloudWatch.DeleteAlarms
import Network.AWS.CloudWatch.DeleteDashboards
import Network.AWS.CloudWatch.DescribeAlarmHistory
import Network.AWS.CloudWatch.DescribeAlarms
import Network.AWS.CloudWatch.DescribeAlarmsForMetric
import Network.AWS.CloudWatch.DisableAlarmActions
import Network.AWS.CloudWatch.EnableAlarmActions
import Network.AWS.CloudWatch.GetDashboard
import Network.AWS.CloudWatch.GetMetricData
import Network.AWS.CloudWatch.GetMetricStatistics
import Network.AWS.CloudWatch.ListDashboards
import Network.AWS.CloudWatch.ListMetrics
import Network.AWS.CloudWatch.PutDashboard
import Network.AWS.CloudWatch.PutMetricAlarm
import Network.AWS.CloudWatch.PutMetricData
import Network.AWS.CloudWatch.SetAlarmState
import Network.AWS.CloudWatch.Types
import Network.AWS.CloudWatch.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CloudWatch'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
