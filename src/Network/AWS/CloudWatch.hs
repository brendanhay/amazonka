{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudWatch
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon CloudWatch is a web service that enables you to publish, monitor, and
-- manage various metrics, as well as configure alarm actions based on data
-- from metrics.
module Network.AWS.CloudWatch
    (
    -- * Actions
    -- ** DeleteAlarms
      DeleteAlarms                    (..)
    , DeleteAlarmsResponse            (..)

    -- ** DescribeAlarmHistory
    , DescribeAlarmHistory            (..)
    , DescribeAlarmHistoryResponse    (..)

    -- ** DescribeAlarms
    , DescribeAlarms                  (..)
    , DescribeAlarmsResponse          (..)

    -- ** DescribeAlarmsForMetric
    , DescribeAlarmsForMetric         (..)
    , DescribeAlarmsForMetricResponse (..)

    -- ** DisableAlarmActions
    , DisableAlarmActions             (..)
    , DisableAlarmActionsResponse     (..)

    -- ** EnableAlarmActions
    , EnableAlarmActions              (..)
    , EnableAlarmActionsResponse      (..)

    -- ** GetMetricStatistics
    , GetMetricStatistics             (..)
    , GetMetricStatisticsResponse     (..)

    -- ** ListMetrics
    , ListMetrics                     (..)
    , ListMetricsResponse             (..)

    -- ** PutMetricAlarm
    , PutMetricAlarm                  (..)
    , PutMetricAlarmResponse          (..)

    -- ** PutMetricData
    , PutMetricData                   (..)
    , PutMetricDataResponse           (..)

    -- ** SetAlarmState
    , SetAlarmState                   (..)
    , SetAlarmStateResponse           (..)

    -- * Data Types
    , module Network.AWS.CloudWatch.Types

    -- * Common
    , module Network.AWS
    ) where

import Data.ByteString              (ByteString)
import Data.Text                    (Text)
import Data.Time
import Network.AWS
import Network.AWS.CloudWatch.Types
import Network.AWS.Internal
import Network.Http.Client          (Method(..))

query :: IsQuery a => Method -> ByteString -> a -> AWS Signed
query = version4Query cloudWatch

--
-- Actions
--

-- | Deletes all specified alarms. In the event of an error, no alarms are
-- deleted.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DeleteAlarms.html>
data DeleteAlarms = DeleteAlarms
    { daAlarmNames :: Members Text
      -- ^ A list of alarms to be deleted.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteAlarms

instance Rq DeleteAlarms where
    type Er DeleteAlarms = CloudWatchError
    type Rs DeleteAlarms = DeleteAlarmsResponse
    request = query GET "DeleteAlarms"

data DeleteAlarmsResponse = DeleteAlarmsResponse
    { darResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML DeleteAlarmsResponse where
    xmlPickler = withNS cloudWatchNS

-- | Retrieves history for the specified alarm. Filter alarms by date range or
-- item type. If an alarm name is not specified, Amazon CloudWatch returns
-- histories for all of the owner's alarms. Note Amazon CloudWatch retains the
-- history of an alarm for two weeks, whether or not you delete the alarm.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarmHistory.html>
data DescribeAlarmHistory = DescribeAlarmHistory
    { dahAlarmName       :: Maybe Text
      -- ^ The name of the alarm.
    , dahEndDate         :: Maybe UTCTime
      -- ^ The ending date to retrieve alarm history.
    , dahHistoryItemType :: Maybe Text
      -- ^ The type of alarm histories to retrieve.
    , dahMaxRecords      :: Maybe Integer
      -- ^ The maximum number of alarm history records to retrieve.
    , dahNextToken       :: Maybe Text
      -- ^ The token returned by a previous call to indicate that there is
      -- more data available.
    , dahStartDate       :: Maybe UTCTime
      -- ^ The starting date to retrieve alarm history.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeAlarmHistory

instance Rq DescribeAlarmHistory where
    type Er DescribeAlarmHistory = CloudWatchError
    type Rs DescribeAlarmHistory = DescribeAlarmHistoryResponse
    request = query GET "DescribeAlarmHistory"

data DescribeAlarmHistoryResponse = DescribeAlarmHistoryResponse
    { dahrResponseMetadata :: !Text
    , dahrDescribeAlarmHistoryResult :: !DescribeAlarmHistoryResult
    } deriving (Eq, Show, Generic)

instance IsXML DescribeAlarmHistoryResponse where
    xmlPickler = withNS cloudWatchNS

-- | Retrieves alarms with the specified names. If no name is specified, all
-- alarms for the user are returned. Alarms can be retrieved by using only a
-- prefix for the alarm name, the alarm state, or a prefix for any action.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarms.html>
data DescribeAlarms = DescribeAlarms
    { daActionPrefix    :: Maybe Text
      -- ^ The action name prefix.
    , daAlarmNamePrefix :: Maybe Text
      -- ^ The alarm name prefix. AlarmNames cannot be specified if this
      -- parameter is specified.
    , dbAlarmNames      :: Members Text
      -- ^ A list of alarm names to retrieve information for.
    , dbMaxRecords      :: Maybe Integer
      -- ^ The maximum number of alarm descriptions to retrieve.
    , dbNextToken       :: Maybe Text
      -- ^ The token returned by a previous call to indicate that there is
      -- more data available.
    , dbStateValue      :: Maybe Text
      -- ^ The state value to be used in matching alarms.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeAlarms

instance Rq DescribeAlarms where
    type Er DescribeAlarms = CloudWatchError
    type Rs DescribeAlarms = DescribeAlarmsResponse
    request = query GET "DescribeAlarms"

data DescribeAlarmsResponse = DescribeAlarmsResponse
    { dasResponseMetadata :: !Text
    , dasDescribeAlarmsResult :: !DescribeAlarmsResult
    } deriving (Eq, Show, Generic)

instance IsXML DescribeAlarmsResponse where
    xmlPickler = withNS cloudWatchNS

-- | Retrieves all alarms for a single metric. Specify a statistic, period, or
-- unit to filter the set of alarms further.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarmsForMetric.html>
data DescribeAlarmsForMetric = DescribeAlarmsForMetric
    { dafmDimensions :: Members Dimension
      -- ^ The list of dimensions associated with the metric.
    , dafmMetricName :: !Text
      -- ^ The name of the metric.
    , dafmNamespace  :: !Text
      -- ^ The namespace of the metric.
    , dafmPeriod     :: Maybe Integer
      -- ^ The period in seconds over which the statistic is applied.
    , dafmStatistic  :: Maybe Text
      -- ^ The statistic for the metric.
    , dafmUnit       :: Maybe Text
      -- ^ The unit for the metric.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeAlarmsForMetric

instance Rq DescribeAlarmsForMetric where
    type Er DescribeAlarmsForMetric = CloudWatchError
    type Rs DescribeAlarmsForMetric = DescribeAlarmsForMetricResponse
    request = query GET "DescribeAlarmsForMetric"

data DescribeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse
    { dafmrResponseMetadata :: !Text
    , dafmrDescribeAlarmsForMetricResult :: !DescribeAlarmsForMetricResult
    } deriving (Eq, Show, Generic)

instance IsXML DescribeAlarmsForMetricResponse where
    xmlPickler = withNS cloudWatchNS

-- | Disables actions for the specified alarms. When an alarm's actions are
-- disabled the alarm's state may change, but none of the alarm's actions will
-- execute.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DisableAlarmActions.html>
data DisableAlarmActions = DisableAlarmActions
    { daaAlarmNames :: Members Text
      -- ^ The names of the alarms to disable actions for.
    } deriving (Eq, Show, Generic)

instance IsQuery DisableAlarmActions

instance Rq DisableAlarmActions where
    type Er DisableAlarmActions = CloudWatchError
    type Rs DisableAlarmActions = DisableAlarmActionsResponse
    request = query GET "DisableAlarmActions"

data DisableAlarmActionsResponse = DisableAlarmActionsResponse
    { daarResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML DisableAlarmActionsResponse where
    xmlPickler = withNS cloudWatchNS

-- | Enables actions for the specified alarms.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_EnableAlarmActions.html>
data EnableAlarmActions = EnableAlarmActions
    { eaaAlarmNames :: Members Text
      -- ^ The names of the alarms to enable actions for.
    } deriving (Eq, Show, Generic)

instance IsQuery EnableAlarmActions

instance Rq EnableAlarmActions where
    type Er EnableAlarmActions = CloudWatchError
    type Rs EnableAlarmActions = EnableAlarmActionsResponse
    request = query GET "EnableAlarmActions"

data EnableAlarmActionsResponse = EnableAlarmActionsResponse
    { eaarResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML EnableAlarmActionsResponse where
    xmlPickler = withNS cloudWatchNS

-- | Gets statistics for the specified metric. The maximum number of data points
-- returned from a single GetMetricStatistics request is 1,440, whereas the
-- maximum number of data points that can be queried is 50,850. If you make a
-- request that generates more than 1,440 data points, Amazon CloudWatch
-- returns an error. In such a case, you can alter the request by narrowing
-- the specified time range or increasing the specified period. Alternatively,
-- you can make multiple requests across adjacent time ranges.
-- GetMetricStatistics does not return the data in chronological order. Amazon
-- CloudWatch aggregates data points based on the length of the period that
-- you specify. For example, if you request statistics with a one-minute
-- granularity, Amazon CloudWatch aggregates data points with time stamps that
-- fall within the same one-minute period. In such a case, the data points
-- queried can greatly outnumber the data points returned. The following
-- examples show various statistics allowed by the data point query maximum of
-- 50,850 when you call GetMetricStatistics on Amazon EC2 instances with
-- detailed (one-minute) monitoring enabled: For information about the
-- namespace, metric names, and dimensions that other Amazon Web Services
-- products use to send metrics to CloudWatch, go to Amazon CloudWatch
-- Metrics, Namespaces, and Dimensions Reference in the Amazon CloudWatch
-- Developer Guide.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricStatistics.html>
data GetMetricStatistics = GetMetricStatistics
    { gmsDimensions :: Members Dimension
      -- ^ A list of dimensions describing qualities of the metric.
    , gmsEndTime    :: !UTCTime
      -- ^ The time stamp to use for determining the last datapoint to
      -- return. The value specified is exclusive; results will include
      -- datapoints up to the time stamp specified.
    , gmsMetricName :: !Text
      -- ^ The name of the metric, with or without spaces.
    , gmsNamespace  :: !Text
      -- ^ The namespace of the metric, with or without spaces.
    , gmsPeriod     :: !Integer
      -- ^ The granularity, in seconds, of the returned datapoints. Period
      -- must be at least 60 seconds and must be a multiple of 60. The
      -- default value is 60.
    , gmsStartTime  :: !UTCTime
      -- ^ The time stamp to use for determining the first datapoint to
      -- return. The value specified is inclusive; results include
      -- datapoints with the time stamp specified.
    , gmsStatistics :: Members Text
      -- ^ The metric statistics to return. For information about specific
      -- statistics returned by GetMetricStatistics, go to Statistics in
      -- the Amazon CloudWatch Developer Guide.
    , gmsUnit       :: Maybe Text
      -- ^ The unit for the metric.
    } deriving (Eq, Show, Generic)

instance IsQuery GetMetricStatistics

instance Rq GetMetricStatistics where
    type Er GetMetricStatistics = CloudWatchError
    type Rs GetMetricStatistics = GetMetricStatisticsResponse
    request = query GET "GetMetricStatistics"

data GetMetricStatisticsResponse = GetMetricStatisticsResponse
    { gmsrResponseMetadata :: !Text
    , gmsrGetMetricStatisticsResult :: !GetMetricStatisticsResult
    } deriving (Eq, Show, Generic)

instance IsXML GetMetricStatisticsResponse where
    xmlPickler = withNS cloudWatchNS

-- | Returns a list of valid metrics stored for the AWS account owner. Returned
-- metrics can be used with GetMetricStatistics to obtain statistical data for
-- a given metric. Note Up to 500 results are returned for any one call. To
-- retrieve further results, use returned NextToken values with subsequent
-- ListMetrics operations. Note If you create a metric with the PutMetricData
-- action, allow up to fifteen minutes for the metric to appear in calls to
-- the ListMetrics action. Statistics about the metric, however, are available
-- sooner using GetMetricStatistics.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_ListMetrics.html>
data ListMetrics = ListMetrics
    { lmDimensions :: Members DimensionFilter
      -- ^ A list of dimensions to filter against.
    , lmMetricName :: Maybe Text
      -- ^ The name of the metric to filter against.
    , lmNamespace  :: Maybe Text
      -- ^ The namespace to filter against.
    , lmNextToken  :: Maybe Text
      -- ^ The token returned by a previous call to indicate that there is
      -- more data available.
    } deriving (Eq, Show, Generic)

instance IsQuery ListMetrics

instance Rq ListMetrics where
    type Er ListMetrics = CloudWatchError
    type Rs ListMetrics = ListMetricsResponse
    request = query GET "ListMetrics"

data ListMetricsResponse = ListMetricsResponse
    { lmrResponseMetadata :: !Text
    , lmrListMetricsResult :: !ListMetricsResult
    } deriving (Eq, Show, Generic)

instance IsXML ListMetricsResponse where
    xmlPickler = withNS cloudWatchNS

-- | Creates or updates an alarm and associates it with the specified Amazon
-- CloudWatch metric. Optionally, this operation can associate one or more
-- Amazon Simple Notification Service resources with the alarm. When this
-- operation creates an alarm, the alarm state is immediately set to
-- INSUFFICIENT_DATA. The alarm is evaluated and its StateValue is set
-- appropriately. Any actions associated with the StateValue is then executed.
-- Note When updating an existing alarm, its StateValue is left unchanged.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_PutMetricAlarm.html>
data PutMetricAlarm = PutMetricAlarm
    { pmaActionsEnabled          :: Maybe Bool
      -- ^ Indicates whether or not actions should be executed during any
      -- changes to the alarm's state.
    , pmaAlarmActions            :: Members Text
      -- ^ The list of actions to execute when this alarm transitions into
      -- an ALARM state from any other state. Each action is specified as
      -- an Amazon Resource Number (ARN). Currently the only action
      -- supported is publishing to an Amazon SNS topic or an Amazon Auto
      -- Scaling policy.
    , pmaAlarmDescription        :: Maybe Text
      -- ^ The description for the alarm.
    , pmaAlarmName               :: !Text
      -- ^ The descriptive name for the alarm. This name must be unique
      -- within the user's AWS account
    , pmaComparisonOperator      :: !Text
      -- ^ The arithmetic operation to use when comparing the specified
      -- Statistic and Threshold. The specified Statistic value is used as
      -- the first operand.
    , pmaDimensions              :: Members Dimension
      -- ^ The dimensions for the alarm's associated metric.
    , pmaEvaluationPeriods       :: !Integer
      -- ^ The number of periods over which data is compared to the
      -- specified threshold.
    , pmaInsufficientDataActions :: Members Text
      -- ^ The list of actions to execute when this alarm transitions into
      -- an INSUFFICIENT_DATA state from any other state. Each action is
      -- specified as an Amazon Resource Number (ARN). Currently the only
      -- action supported is publishing to an Amazon SNS topic or an
      -- Amazon Auto Scaling policy.
    , pmaMetricName              :: !Text
      -- ^ The name for the alarm's associated metric.
    , pmaNamespace               :: !Text
      -- ^ The namespace for the alarm's associated metric.
    , pmaOKActions               :: Members Text
      -- ^ The list of actions to execute when this alarm transitions into
      -- an OK state from any other state. Each action is specified as an
      -- Amazon Resource Number (ARN). Currently the only action supported
      -- is publishing to an Amazon SNS topic or an Amazon Auto Scaling
      -- policy.
    , pmaPeriod                  :: !Integer
      -- ^ The period in seconds over which the specified statistic is
      -- applied.
    , pmaStatistic               :: !Text
      -- ^ The statistic to apply to the alarm's associated metric.
    , pmaThreshold               :: !Double
      -- ^ The value against which the specified statistic is compared.
    , pmaUnit                    :: Maybe Text
      -- ^ The unit for the alarm's associated metric.
    } deriving (Eq, Show, Generic)

instance IsQuery PutMetricAlarm

instance Rq PutMetricAlarm where
    type Er PutMetricAlarm = CloudWatchError
    type Rs PutMetricAlarm = PutMetricAlarmResponse
    request = query GET "PutMetricAlarm"

data PutMetricAlarmResponse = PutMetricAlarmResponse
    { pmarResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML PutMetricAlarmResponse where
    xmlPickler = withNS cloudWatchNS

-- | Publishes metric data points to Amazon CloudWatch. Amazon CloudWatch
-- associates the data points with the specified metric. If the specified
-- metric does not exist, Amazon CloudWatch creates the metric. When Amazon
-- CloudWatch creates a metric, it can take up to fifteen minutes for the
-- metric to appear in calls to the ListMetrics action. Each PutMetricData
-- request is limited to 8 KB in size for HTTP GET requests and is limited to
-- 40 KB in size for HTTP POST requests. Important Although the Value
-- parameter accepts numbers of type Double, Amazon CloudWatch rejects values
-- that are either too small or too large. Values must be in the range of
-- 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2). In
-- addition, special values (e.g., NaN, +Infinity, -Infinity) are not
-- supported. Data that is timestamped 24 hours or more in the past may take
-- in excess of 48 hours to become available from submission time using
-- GetMetricStatistics.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_PutMetricData.html>
data PutMetricData = PutMetricData
    { pmdMetricData :: Members MetricDatum
      -- ^ A list of data describing the metric.
    , pmdNamespace  :: !Text
      -- ^ The namespace for the metric data.
    } deriving (Eq, Show, Generic)

instance IsQuery PutMetricData

instance Rq PutMetricData where
    type Er PutMetricData = CloudWatchError
    type Rs PutMetricData = PutMetricDataResponse
    request = query GET "PutMetricData"

data PutMetricDataResponse = PutMetricDataResponse
    { pmdrResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML PutMetricDataResponse where
    xmlPickler = withNS cloudWatchNS

-- | Temporarily sets the state of an alarm. When the updated StateValue differs
-- from the previous value, the action configured for the appropriate state is
-- invoked. This is not a permanent change. The next periodic alarm check (in
-- about a minute) will set the alarm to its actual state.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_SetAlarmState.html>
data SetAlarmState = SetAlarmState
    { sasAlarmName       :: !Text
      -- ^ The descriptive name for the alarm. This name must be unique
      -- within the user's AWS account. The maximum length is 255
      -- characters.
    , sasStateReason     :: !Text
      -- ^ The reason that this alarm is set to this specific state (in
      -- human-readable text format)
    , sasStateReasonData :: Maybe Text
      -- ^ The reason that this alarm is set to this specific state (in
      -- machine-readable JSON format)
    , sasStateValue      :: !Text
      -- ^ The value of the state.
    } deriving (Eq, Show, Generic)

instance IsQuery SetAlarmState

instance Rq SetAlarmState where
    type Er SetAlarmState = CloudWatchError
    type Rs SetAlarmState = SetAlarmStateResponse
    request = query GET "SetAlarmState"

data SetAlarmStateResponse = SetAlarmStateResponse
    { sasrResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML SetAlarmStateResponse where
    xmlPickler = withNS cloudWatchNS
