{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is the /Amazon CloudWatch API Reference/. This guide provides
-- detailed information about Amazon CloudWatch actions, data types,
-- parameters, and errors. For detailed information about Amazon CloudWatch
-- features and their associated API calls, go to the
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide Amazon CloudWatch Developer Guide>.
--
-- Amazon CloudWatch is a web service that enables you to publish, monitor,
-- and manage various metrics, as well as configure alarm actions based on
-- data from metrics. For more information about this product go to
-- <http://aws.amazon.com/cloudwatch>.
--
-- For information about the namespace, metric names, and dimensions that
-- other Amazon Web Services products use to send metrics to Cloudwatch, go
-- to
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Metrics, Namespaces, and Dimensions Reference>
-- in the /Amazon CloudWatch Developer Guide/.
--
-- Use the following links to get started using the /Amazon CloudWatch API
-- Reference/:
--
-- -   <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_Operations.html Actions>:
--     An alphabetical list of all Amazon CloudWatch actions.
-- -   <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_Types.html Data Types>:
--     An alphabetical list of all Amazon CloudWatch data types.
-- -   <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CommonParameters.html Common Parameters>:
--     Parameters that all Query actions can use.
-- -   <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CommonErrors.html Common Errors>:
--     Client and server errors that all actions can return.
-- -   <http://docs.aws.amazon.com/general/latest/gr/index.html?rande.html Regions and Endpoints>:
--     Itemized regions and endpoints for all AWS products.
-- -   <http://monitoring.amazonaws.com/doc/2010-08-01/CloudWatch.wsdl WSDL Location>:
--     http:\/\/monitoring.amazonaws.com\/doc\/2010-08-01\/CloudWatch.wsdl
--
-- In addition to using the Amazon CloudWatch API, you can also use the
-- following SDKs and third-party libraries to access Amazon CloudWatch
-- programmatically.
--
-- -   <http://aws.amazon.com/documentation/sdkforjava/ AWS SDK for Java Documentation>
-- -   <http://aws.amazon.com/documentation/sdkfornet/ AWS SDK for .NET Documentation>
-- -   <http://aws.amazon.com/documentation/sdkforphp/ AWS SDK for PHP Documentation>
-- -   <http://aws.amazon.com/documentation/sdkforruby/ AWS SDK for Ruby Documentation>
--
-- Developers in the AWS developer community also provide their own
-- libraries, which you can find at the following AWS developer centers:
--
-- -   <http://aws.amazon.com/java/ AWS Java Developer Center>
-- -   <http://aws.amazon.com/php/ AWS PHP Developer Center>
-- -   <http://aws.amazon.com/python/ AWS Python Developer Center>
-- -   <http://aws.amazon.com/ruby/ AWS Ruby Developer Center>
-- -   <http://aws.amazon.com/net/ AWS Windows and .NET Developer Center>
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.CloudWatch
    (
    -- * Service Description
      CloudWatch

    -- * Error Matchers
    -- $errors
    , _LimitExceededFault
    , _InvalidNextToken
    , _InternalServiceFault
    , _InvalidParameterValueException
    , _InvalidFormatFault
    , _MissingRequiredParameterException
    , _InvalidParameterCombinationException
    , _ResourceNotFound

    -- * Operations
    -- $operations

    -- ** EnableAlarmActions
    , module Network.AWS.CloudWatch.EnableAlarmActions

    -- ** PutMetricData
    , module Network.AWS.CloudWatch.PutMetricData

    -- ** DescribeAlarms (Paginated)
    , module Network.AWS.CloudWatch.DescribeAlarms
    -- $pager

    -- ** ListMetrics (Paginated)
    , module Network.AWS.CloudWatch.ListMetrics
    -- $pager

    -- ** DeleteAlarms
    , module Network.AWS.CloudWatch.DeleteAlarms

    -- ** DescribeAlarmHistory (Paginated)
    , module Network.AWS.CloudWatch.DescribeAlarmHistory
    -- $pager

    -- ** GetMetricStatistics
    , module Network.AWS.CloudWatch.GetMetricStatistics

    -- ** DisableAlarmActions
    , module Network.AWS.CloudWatch.DisableAlarmActions

    -- ** DescribeAlarmsForMetric
    , module Network.AWS.CloudWatch.DescribeAlarmsForMetric

    -- ** SetAlarmState
    , module Network.AWS.CloudWatch.SetAlarmState

    -- ** PutMetricAlarm
    , module Network.AWS.CloudWatch.PutMetricAlarm

    -- * Types

    -- ** ComparisonOperator
    , ComparisonOperator (..)

    -- ** HistoryItemType
    , HistoryItemType (..)

    -- ** StandardUnit
    , StandardUnit (..)

    -- ** StateValue
    , StateValue (..)

    -- ** Statistic
    , Statistic (..)

    -- ** AlarmHistoryItem
    , AlarmHistoryItem
    , alarmHistoryItem
    , ahiAlarmName
    , ahiHistoryItemType
    , ahiHistoryData
    , ahiTimestamp
    , ahiHistorySummary

    -- ** Datapoint
    , Datapoint
    , datapoint
    , dSampleCount
    , dMaximum
    , dAverage
    , dMinimum
    , dSum
    , dTimestamp
    , dUnit

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
    , maAlarmDescription
    , maPeriod
    , maEvaluationPeriods
    , maMetricName
    , maNamespace
    , maOKActions
    , maComparisonOperator
    , maStateValue
    , maThreshold
    , maActionsEnabled
    , maAlarmConfigurationUpdatedTimestamp
    , maInsufficientDataActions
    , maDimensions
    , maStateReasonData
    , maStateReason
    , maAlarmARN
    , maAlarmActions
    , maStatistic
    , maUnit

    -- ** MetricDatum
    , MetricDatum
    , metricDatum
    , mdValue
    , mdDimensions
    , mdTimestamp
    , mdStatisticValues
    , mdUnit
    , mdMetricName

    -- ** StatisticSet
    , StatisticSet
    , statisticSet
    , ssSampleCount
    , ssSum
    , ssMinimum
    , ssMaximum
    ) where

import           Network.AWS.CloudWatch.DeleteAlarms
import           Network.AWS.CloudWatch.DescribeAlarmHistory
import           Network.AWS.CloudWatch.DescribeAlarms
import           Network.AWS.CloudWatch.DescribeAlarmsForMetric
import           Network.AWS.CloudWatch.DisableAlarmActions
import           Network.AWS.CloudWatch.EnableAlarmActions
import           Network.AWS.CloudWatch.GetMetricStatistics
import           Network.AWS.CloudWatch.ListMetrics
import           Network.AWS.CloudWatch.PutMetricAlarm
import           Network.AWS.CloudWatch.PutMetricData
import           Network.AWS.CloudWatch.SetAlarmState
import           Network.AWS.CloudWatch.Types
import           Network.AWS.CloudWatch.Waiters

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
Waiters poll by repeatedly send a request until some remote success condition
specified by the 'Wait' configuration is fulfilled. The 'Wait' configuration
specifies how many attempts should be made, in addition to delay and retry strategies.
-}

{- $pager
This operation can return paginated results.
-}
