{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudWatch.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CloudWatch.Types where

import Data.ByteString      (ByteString)
import Data.Monoid
import Data.Text            (Text)
import Data.Time
import Network.AWS.Internal

-- | Currently supported version of the CloudWatch service.
cloudWatch :: Service
cloudWatch = Service Regional version4 "cloudwatch" "2010-08-01"

-- | XML namespace to annotate CloudWatch elements with.
cloudWatchNS :: ByteString
cloudWatchNS = "https://cloudwatch.amazonaws.com/doc/" <> svcVersion cloudWatch <> "/"

-- | Helper to define CloudWatch namespaced XML elements.
cloudWatchElem :: ByteString -> NName ByteString
cloudWatchElem = mkNName cloudWatchNS

data CloudWatchError = CloudWatchError
    { cweCode    :: !Text
    , cweMessage :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML CloudWatchError where
    xmlPickler = withNS cloudWatchNS

instance ToError CloudWatchError where
    toError = Err . show

-- | The AlarmHistoryItem data type contains descriptive information about the
-- history of a specific alarm. If you call DescribeAlarmHistory, Amazon
-- CloudWatch returns this data type as part of the DescribeAlarmHistoryResult
-- data type.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_AlarmHistoryItem.html>
data AlarmHistoryItem = AlarmHistoryItem
    { ahiAlarmName       :: Maybe Text
      -- ^ The descriptive name for the alarm.
    , ahiHistoryData     :: Maybe Text
      -- ^ Machine-readable data about the alarm in JSON format.
    , ahiHistoryItemType :: Maybe Text
      -- ^ The type of alarm history item.
    , ahiHistorySummary  :: Maybe Text
      -- ^ A human-readable summary of the alarm history.
    , ahiTimestamp       :: Maybe UTCTime
      -- ^ The time stamp for the alarm history item.
    } deriving (Eq, Show, Generic)

instance IsQuery AlarmHistoryItem

instance IsXML AlarmHistoryItem where
    xmlPickler = withNS cloudWatchNS

-- | The Datapoint data type encapsulates the statistical data that Amazon
-- CloudWatch computes from metric data.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_Datapoint.html>
data Datapoint = Datapoint
    { dAverage     :: Maybe Double
      -- ^ The average of metric values that correspond to the datapoint.
    , dMaximum     :: Maybe Double
      -- ^ The maximum of the metric value used for the datapoint.
    , dMinimum     :: Maybe Double
      -- ^ The minimum metric value used for the datapoint.
    , dSampleCount :: Maybe Double
      -- ^ The number of metric values that contributed to the aggregate
      -- value of this datapoint.
    , dSum         :: Maybe Double
      -- ^ The sum of metric values used for the datapoint.
    , dTimestamp   :: Maybe UTCTime
      -- ^ The time stamp used for the datapoint.
    , dUnit        :: Maybe Text
      -- ^ The standard unit used for the datapoint.
    } deriving (Eq, Show, Generic)

instance IsQuery Datapoint

instance IsXML Datapoint where
    xmlPickler = withNS cloudWatchNS

-- | The output for the DescribeAlarmHistory action.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarmHistoryResult.html>
data DescribeAlarmHistoryResult = DescribeAlarmHistoryResult
    { dahrAlarmHistoryItems :: Maybe AlarmHistoryItem
      -- ^ A list of alarm histories in JSON format.
    , dahrNextToken         :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeAlarmHistoryResult

instance IsXML DescribeAlarmHistoryResult where
    xmlPickler = withNS cloudWatchNS

-- | The output for the DescribeAlarmsForMetric action.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarmsForMetricResult.html>
data DescribeAlarmsForMetricResult = DescribeAlarmsForMetricResult
    { dafmrMetricAlarms :: Maybe MetricAlarm
      -- ^ A list of information for each alarm with the specified metric.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeAlarmsForMetricResult

instance IsXML DescribeAlarmsForMetricResult where
    xmlPickler = withNS cloudWatchNS

-- | The output for the DescribeAlarms action.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarmsResult.html>
data DescribeAlarmsResult = DescribeAlarmsResult
    { darMetricAlarms :: Maybe MetricAlarm
      -- ^ A list of information for the specified alarms.
    , darNextToken    :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Eq, Show, Generic)

instance IsQuery DescribeAlarmsResult

instance IsXML DescribeAlarmsResult where
    xmlPickler = withNS cloudWatchNS

-- | The Dimension data type further expands on the identity of a metric using a
-- Name, Value pair. For examples that use one or more dimensions, see
-- PutMetricData.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_Dimension.html>
data Dimension = Dimension
    { dName  :: !Text
      -- ^ The name of the dimension.
    , dValue :: !Text
      -- ^ The value representing the dimension measurement
    } deriving (Eq, Show, Generic)

instance IsQuery Dimension

instance IsXML Dimension where
    xmlPickler = withNS cloudWatchNS

-- | The DimensionFilter data type is used to filter ListMetrics results.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DimensionFilter.html>
data DimensionFilter = DimensionFilter
    { dfName  :: !Text
      -- ^ The dimension name to be matched.
    , dfValue :: Maybe Text
      -- ^ The value of the dimension to be matched.
    } deriving (Eq, Show, Generic)

instance IsQuery DimensionFilter

instance IsXML DimensionFilter where
    xmlPickler = withNS cloudWatchNS

-- | The output for the GetMetricStatistics action.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricStatisticsResult.html>
data GetMetricStatisticsResult = GetMetricStatisticsResult
    { gmsrDatapoints :: Maybe Datapoint
      -- ^ The datapoints for the specified metric.
    , gmsrLabel      :: Maybe Text
      -- ^ A label describing the specified metric.
    } deriving (Eq, Show, Generic)

instance IsQuery GetMetricStatisticsResult

instance IsXML GetMetricStatisticsResult where
    xmlPickler = withNS cloudWatchNS

-- | The output for the ListMetrics action.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_ListMetricsResult.html>
data ListMetricsResult = ListMetricsResult
    { lmrMetrics   :: Maybe Metric
      -- ^ A list of metrics used to generate statistics for an AWS account.
    , lmrNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Eq, Show, Generic)

instance IsQuery ListMetricsResult

instance IsXML ListMetricsResult where
    xmlPickler = withNS cloudWatchNS

-- | The Metric data type contains information about a specific metric. If you
-- call ListMetrics, Amazon CloudWatch returns information contained by this
-- data type. The example in the Examples section publishes two metrics named
-- buffers and latency. Both metrics are in the examples namespace. Both
-- metrics have two dimensions, InstanceID and InstanceType.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_Metric.html>
data Metric = Metric
    { mDimensions :: Maybe Dimension
      -- ^ A list of dimensions associated with the metric.
    , mMetricName :: Maybe Text
      -- ^ The name of the metric.
    , mNamespace  :: Maybe Text
      -- ^ The namespace of the metric.
    } deriving (Eq, Show, Generic)

instance IsQuery Metric

instance IsXML Metric where
    xmlPickler = withNS cloudWatchNS

-- | The MetricAlarm data type represents an alarm. You can use PutMetricAlarm
-- to create or update an alarm.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricAlarm.html>
data MetricAlarm = MetricAlarm
    { maActionsEnabled                     :: Maybe Bool
      -- ^ Indicates whether actions should be executed during any changes
      -- to the alarm's state.
    , maAlarmActions                       :: Maybe Text
      -- ^ The list of actions to execute when this alarm transitions into
      -- an ALARM state from any other state. Each action is specified as
      -- an Amazon Resource Number (ARN). Currently the only actions
      -- supported are publishing to an Amazon SNS topic and triggering an
      -- Auto Scaling policy.
    , maAlarmArn                           :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the alarm.
    , maAlarmConfigurationUpdatedTimestamp :: Maybe UTCTime
      -- ^ The time stamp of the last update to the alarm configuration.
    , maAlarmDescription                   :: Maybe Text
      -- ^ The description for the alarm.
    , maAlarmName                          :: Maybe Text
      -- ^ The name of the alarm.
    , maComparisonOperator                 :: Maybe Text
      -- ^ The arithmetic operation to use when comparing the specified
      -- Statistic and Threshold. The specified Statistic value is used as
      -- the first operand.
    , maDimensions                         :: Maybe Dimension
      -- ^ The list of dimensions associated with the alarm's associated
      -- metric.
    , maEvaluationPeriods                  :: Maybe Integer
      -- ^ The number of periods over which data is compared to the
      -- specified threshold.
    , maInsufficientDataActions            :: Maybe Text
      -- ^ The list of actions to execute when this alarm transitions into
      -- an INSUFFICIENT_DATA state from any other state. Each action is
      -- specified as an Amazon Resource Number (ARN). Currently the only
      -- actions supported are publishing to an Amazon SNS topic or
      -- triggering an Auto Scaling policy.
    , maMetricName                         :: Maybe Text
      -- ^ The name of the alarm's metric.
    , maNamespace                          :: Maybe Text
      -- ^ The namespace of alarm's associated metric.
    , maOKActions                          :: Maybe Text
      -- ^ The list of actions to execute when this alarm transitions into
      -- an OK state from any other state. Each action is specified as an
      -- Amazon Resource Number (ARN). Currently the only actions
      -- supported are publishing to an Amazon SNS topic and triggering an
      -- Auto Scaling policy.
    , maPeriod                             :: Maybe Integer
      -- ^ The period in seconds over which the statistic is applied.
    , maStateReason                        :: Maybe Text
      -- ^ A human-readable explanation for the alarm's state.
    , maStateReasonData                    :: Maybe Text
      -- ^ An explanation for the alarm's state in machine-readable JSON
      -- format
    , maStateUpdatedTimestamp              :: Maybe UTCTime
      -- ^ The time stamp of the last update to the alarm's state.
    , maStateValue                         :: Maybe Text
      -- ^ The state value for the alarm.
    , maStatistic                          :: Maybe Text
      -- ^ The statistic to apply to the alarm's associated metric.
    , maThreshold                          :: Maybe Double
      -- ^ The value against which the specified statistic is compared.
    , maUnit                               :: Maybe Text
      -- ^ The unit of the alarm's associated metric.
    } deriving (Eq, Show, Generic)

instance IsQuery MetricAlarm

instance IsXML MetricAlarm where
    xmlPickler = withNS cloudWatchNS

-- | The MetricDatum data type encapsulates the information sent with
-- PutMetricData to either create a new metric or add new values to be
-- aggregated into an existing metric.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html>
data MetricDatum = MetricDatum
    { mdDimensions      :: Maybe Dimension
      -- ^ A list of dimensions associated with the metric. Note, when using
      -- the Dimensions value in a query, you need to append .member.N to
      -- it (e.g., Dimensions.member.N).
    , mdMetricName      :: !Text
      -- ^ The name of the metric.
    , mdStatisticValues :: Maybe StatisticSet
      -- ^ A set of statistical values describing the metric.
    , mdTimestamp       :: Maybe UTCTime
      -- ^ The time stamp used for the metric in ISO 8601 Universal
      -- Coordinated Time (UTC) format. If not specified, the default
      -- value is set to the time the metric data was received.
    , mdUnit            :: Maybe Text
      -- ^ The unit of the metric.
    , mdValue           :: Maybe Double
      -- ^ The value for the metric.
    } deriving (Eq, Show, Generic)

instance IsQuery MetricDatum

instance IsXML MetricDatum where
    xmlPickler = withNS cloudWatchNS

-- | The StatisticSet data type describes the StatisticValues component of
-- MetricDatum, and represents a set of statistics that describes a specific
-- metric.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_StatisticSet.html>
data StatisticSet = StatisticSet
    { ssMaximum     :: !Double
      -- ^ The maximum value of the sample set.
    , ssMinimum     :: !Double
      -- ^ The minimum value of the sample set.
    , ssSampleCount :: !Double
      -- ^ The number of samples used for the statistic set.
    , ssSum         :: !Double
      -- ^ The sum of values for the sample set.
    } deriving (Eq, Show, Generic)

instance IsQuery StatisticSet

instance IsXML StatisticSet where
    xmlPickler = withNS cloudWatchNS
