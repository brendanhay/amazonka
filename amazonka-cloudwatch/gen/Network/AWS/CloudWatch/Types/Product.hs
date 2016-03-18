{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatch.Types.Product where

import           Network.AWS.CloudWatch.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | The 'AlarmHistoryItem' data type contains descriptive information about
-- the history of a specific alarm. If you call < DescribeAlarmHistory>,
-- Amazon CloudWatch returns this data type as part of the
-- DescribeAlarmHistoryResult data type.
--
-- /See:/ 'alarmHistoryItem' smart constructor.
data AlarmHistoryItem = AlarmHistoryItem'
    { _ahiAlarmName       :: !(Maybe Text)
    , _ahiHistoryItemType :: !(Maybe HistoryItemType)
    , _ahiHistoryData     :: !(Maybe Text)
    , _ahiHistorySummary  :: !(Maybe Text)
    , _ahiTimestamp       :: !(Maybe ISO8601)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AlarmHistoryItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ahiAlarmName'
--
-- * 'ahiHistoryItemType'
--
-- * 'ahiHistoryData'
--
-- * 'ahiHistorySummary'
--
-- * 'ahiTimestamp'
alarmHistoryItem
    :: AlarmHistoryItem
alarmHistoryItem =
    AlarmHistoryItem'
    { _ahiAlarmName = Nothing
    , _ahiHistoryItemType = Nothing
    , _ahiHistoryData = Nothing
    , _ahiHistorySummary = Nothing
    , _ahiTimestamp = Nothing
    }

-- | The descriptive name for the alarm.
ahiAlarmName :: Lens' AlarmHistoryItem (Maybe Text)
ahiAlarmName = lens _ahiAlarmName (\ s a -> s{_ahiAlarmName = a});

-- | The type of alarm history item.
ahiHistoryItemType :: Lens' AlarmHistoryItem (Maybe HistoryItemType)
ahiHistoryItemType = lens _ahiHistoryItemType (\ s a -> s{_ahiHistoryItemType = a});

-- | Machine-readable data about the alarm in JSON format.
ahiHistoryData :: Lens' AlarmHistoryItem (Maybe Text)
ahiHistoryData = lens _ahiHistoryData (\ s a -> s{_ahiHistoryData = a});

-- | A human-readable summary of the alarm history.
ahiHistorySummary :: Lens' AlarmHistoryItem (Maybe Text)
ahiHistorySummary = lens _ahiHistorySummary (\ s a -> s{_ahiHistorySummary = a});

-- | The time stamp for the alarm history item.
ahiTimestamp :: Lens' AlarmHistoryItem (Maybe UTCTime)
ahiTimestamp = lens _ahiTimestamp (\ s a -> s{_ahiTimestamp = a}) . mapping _Time;

instance FromXML AlarmHistoryItem where
        parseXML x
          = AlarmHistoryItem' <$>
              (x .@? "AlarmName") <*> (x .@? "HistoryItemType") <*>
                (x .@? "HistoryData")
                <*> (x .@? "HistorySummary")
                <*> (x .@? "Timestamp")

-- | The 'Datapoint' data type encapsulates the statistical data that Amazon
-- CloudWatch computes from metric data.
--
-- /See:/ 'datapoint' smart constructor.
data Datapoint = Datapoint'
    { _dSampleCount :: !(Maybe Double)
    , _dMaximum     :: !(Maybe Double)
    , _dAverage     :: !(Maybe Double)
    , _dMinimum     :: !(Maybe Double)
    , _dSum         :: !(Maybe Double)
    , _dUnit        :: !(Maybe StandardUnit)
    , _dTimestamp   :: !(Maybe ISO8601)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Datapoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dSampleCount'
--
-- * 'dMaximum'
--
-- * 'dAverage'
--
-- * 'dMinimum'
--
-- * 'dSum'
--
-- * 'dUnit'
--
-- * 'dTimestamp'
datapoint
    :: Datapoint
datapoint =
    Datapoint'
    { _dSampleCount = Nothing
    , _dMaximum = Nothing
    , _dAverage = Nothing
    , _dMinimum = Nothing
    , _dSum = Nothing
    , _dUnit = Nothing
    , _dTimestamp = Nothing
    }

-- | The number of metric values that contributed to the aggregate value of
-- this datapoint.
dSampleCount :: Lens' Datapoint (Maybe Double)
dSampleCount = lens _dSampleCount (\ s a -> s{_dSampleCount = a});

-- | The maximum of the metric value used for the datapoint.
dMaximum :: Lens' Datapoint (Maybe Double)
dMaximum = lens _dMaximum (\ s a -> s{_dMaximum = a});

-- | The average of metric values that correspond to the datapoint.
dAverage :: Lens' Datapoint (Maybe Double)
dAverage = lens _dAverage (\ s a -> s{_dAverage = a});

-- | The minimum metric value used for the datapoint.
dMinimum :: Lens' Datapoint (Maybe Double)
dMinimum = lens _dMinimum (\ s a -> s{_dMinimum = a});

-- | The sum of metric values used for the datapoint.
dSum :: Lens' Datapoint (Maybe Double)
dSum = lens _dSum (\ s a -> s{_dSum = a});

-- | The standard unit used for the datapoint.
dUnit :: Lens' Datapoint (Maybe StandardUnit)
dUnit = lens _dUnit (\ s a -> s{_dUnit = a});

-- | The time stamp used for the datapoint.
dTimestamp :: Lens' Datapoint (Maybe UTCTime)
dTimestamp = lens _dTimestamp (\ s a -> s{_dTimestamp = a}) . mapping _Time;

instance FromXML Datapoint where
        parseXML x
          = Datapoint' <$>
              (x .@? "SampleCount") <*> (x .@? "Maximum") <*>
                (x .@? "Average")
                <*> (x .@? "Minimum")
                <*> (x .@? "Sum")
                <*> (x .@? "Unit")
                <*> (x .@? "Timestamp")

-- | The 'Dimension' data type further expands on the identity of a metric
-- using a Name, Value pair.
--
-- For examples that use one or more dimensions, see < PutMetricData>.
--
-- /See:/ 'dimension' smart constructor.
data Dimension = Dimension'
    { _dName  :: !Text
    , _dValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Dimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dName'
--
-- * 'dValue'
dimension
    :: Text -- ^ 'dName'
    -> Text -- ^ 'dValue'
    -> Dimension
dimension pName_ pValue_ =
    Dimension'
    { _dName = pName_
    , _dValue = pValue_
    }

-- | The name of the dimension.
dName :: Lens' Dimension Text
dName = lens _dName (\ s a -> s{_dName = a});

-- | The value representing the dimension measurement
dValue :: Lens' Dimension Text
dValue = lens _dValue (\ s a -> s{_dValue = a});

instance FromXML Dimension where
        parseXML x
          = Dimension' <$> (x .@ "Name") <*> (x .@ "Value")

instance ToQuery Dimension where
        toQuery Dimension'{..}
          = mconcat ["Name" =: _dName, "Value" =: _dValue]

-- | The 'DimensionFilter' data type is used to filter < ListMetrics>
-- results.
--
-- /See:/ 'dimensionFilter' smart constructor.
data DimensionFilter = DimensionFilter'
    { _dfValue :: !(Maybe Text)
    , _dfName  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DimensionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfValue'
--
-- * 'dfName'
dimensionFilter
    :: Text -- ^ 'dfName'
    -> DimensionFilter
dimensionFilter pName_ =
    DimensionFilter'
    { _dfValue = Nothing
    , _dfName = pName_
    }

-- | The value of the dimension to be matched.
--
-- Specifying a 'Name' without specifying a 'Value' returns all values
-- associated with that 'Name'.
dfValue :: Lens' DimensionFilter (Maybe Text)
dfValue = lens _dfValue (\ s a -> s{_dfValue = a});

-- | The dimension name to be matched.
dfName :: Lens' DimensionFilter Text
dfName = lens _dfName (\ s a -> s{_dfName = a});

instance ToQuery DimensionFilter where
        toQuery DimensionFilter'{..}
          = mconcat ["Value" =: _dfValue, "Name" =: _dfName]

-- | The 'Metric' data type contains information about a specific metric. If
-- you call < ListMetrics>, Amazon CloudWatch returns information contained
-- by this data type.
--
-- The example in the Examples section publishes two metrics named buffers
-- and latency. Both metrics are in the examples namespace. Both metrics
-- have two dimensions, InstanceID and InstanceType.
--
-- /See:/ 'metric' smart constructor.
data Metric = Metric'
    { _mMetricName :: !(Maybe Text)
    , _mNamespace  :: !(Maybe Text)
    , _mDimensions :: !(Maybe [Dimension])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Metric' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mMetricName'
--
-- * 'mNamespace'
--
-- * 'mDimensions'
metric
    :: Metric
metric =
    Metric'
    { _mMetricName = Nothing
    , _mNamespace = Nothing
    , _mDimensions = Nothing
    }

-- | The name of the metric.
mMetricName :: Lens' Metric (Maybe Text)
mMetricName = lens _mMetricName (\ s a -> s{_mMetricName = a});

-- | The namespace of the metric.
mNamespace :: Lens' Metric (Maybe Text)
mNamespace = lens _mNamespace (\ s a -> s{_mNamespace = a});

-- | A list of dimensions associated with the metric.
mDimensions :: Lens' Metric [Dimension]
mDimensions = lens _mDimensions (\ s a -> s{_mDimensions = a}) . _Default . _Coerce;

instance FromXML Metric where
        parseXML x
          = Metric' <$>
              (x .@? "MetricName") <*> (x .@? "Namespace") <*>
                (x .@? "Dimensions" .!@ mempty >>=
                   may (parseXMLList "member"))

-- | The < MetricAlarm> data type represents an alarm. You can use
-- < PutMetricAlarm> to create or update an alarm.
--
-- /See:/ 'metricAlarm' smart constructor.
data MetricAlarm = MetricAlarm'
    { _maAlarmName                          :: !(Maybe Text)
    , _maStateUpdatedTimestamp              :: !(Maybe ISO8601)
    , _maPeriod                             :: !(Maybe Nat)
    , _maAlarmDescription                   :: !(Maybe Text)
    , _maEvaluationPeriods                  :: !(Maybe Nat)
    , _maMetricName                         :: !(Maybe Text)
    , _maNamespace                          :: !(Maybe Text)
    , _maComparisonOperator                 :: !(Maybe ComparisonOperator)
    , _maOKActions                          :: !(Maybe [Text])
    , _maStateValue                         :: !(Maybe StateValue)
    , _maThreshold                          :: !(Maybe Double)
    , _maAlarmConfigurationUpdatedTimestamp :: !(Maybe ISO8601)
    , _maActionsEnabled                     :: !(Maybe Bool)
    , _maInsufficientDataActions            :: !(Maybe [Text])
    , _maStateReason                        :: !(Maybe Text)
    , _maStateReasonData                    :: !(Maybe Text)
    , _maDimensions                         :: !(Maybe [Dimension])
    , _maAlarmARN                           :: !(Maybe Text)
    , _maAlarmActions                       :: !(Maybe [Text])
    , _maUnit                               :: !(Maybe StandardUnit)
    , _maStatistic                          :: !(Maybe Statistic)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MetricAlarm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'maAlarmName'
--
-- * 'maStateUpdatedTimestamp'
--
-- * 'maPeriod'
--
-- * 'maAlarmDescription'
--
-- * 'maEvaluationPeriods'
--
-- * 'maMetricName'
--
-- * 'maNamespace'
--
-- * 'maComparisonOperator'
--
-- * 'maOKActions'
--
-- * 'maStateValue'
--
-- * 'maThreshold'
--
-- * 'maAlarmConfigurationUpdatedTimestamp'
--
-- * 'maActionsEnabled'
--
-- * 'maInsufficientDataActions'
--
-- * 'maStateReason'
--
-- * 'maStateReasonData'
--
-- * 'maDimensions'
--
-- * 'maAlarmARN'
--
-- * 'maAlarmActions'
--
-- * 'maUnit'
--
-- * 'maStatistic'
metricAlarm
    :: MetricAlarm
metricAlarm =
    MetricAlarm'
    { _maAlarmName = Nothing
    , _maStateUpdatedTimestamp = Nothing
    , _maPeriod = Nothing
    , _maAlarmDescription = Nothing
    , _maEvaluationPeriods = Nothing
    , _maMetricName = Nothing
    , _maNamespace = Nothing
    , _maComparisonOperator = Nothing
    , _maOKActions = Nothing
    , _maStateValue = Nothing
    , _maThreshold = Nothing
    , _maAlarmConfigurationUpdatedTimestamp = Nothing
    , _maActionsEnabled = Nothing
    , _maInsufficientDataActions = Nothing
    , _maStateReason = Nothing
    , _maStateReasonData = Nothing
    , _maDimensions = Nothing
    , _maAlarmARN = Nothing
    , _maAlarmActions = Nothing
    , _maUnit = Nothing
    , _maStatistic = Nothing
    }

-- | The name of the alarm.
maAlarmName :: Lens' MetricAlarm (Maybe Text)
maAlarmName = lens _maAlarmName (\ s a -> s{_maAlarmName = a});

-- | The time stamp of the last update to the alarm\'s state.
maStateUpdatedTimestamp :: Lens' MetricAlarm (Maybe UTCTime)
maStateUpdatedTimestamp = lens _maStateUpdatedTimestamp (\ s a -> s{_maStateUpdatedTimestamp = a}) . mapping _Time;

-- | The period in seconds over which the statistic is applied.
maPeriod :: Lens' MetricAlarm (Maybe Natural)
maPeriod = lens _maPeriod (\ s a -> s{_maPeriod = a}) . mapping _Nat;

-- | The description for the alarm.
maAlarmDescription :: Lens' MetricAlarm (Maybe Text)
maAlarmDescription = lens _maAlarmDescription (\ s a -> s{_maAlarmDescription = a});

-- | The number of periods over which data is compared to the specified
-- threshold.
maEvaluationPeriods :: Lens' MetricAlarm (Maybe Natural)
maEvaluationPeriods = lens _maEvaluationPeriods (\ s a -> s{_maEvaluationPeriods = a}) . mapping _Nat;

-- | The name of the alarm\'s metric.
maMetricName :: Lens' MetricAlarm (Maybe Text)
maMetricName = lens _maMetricName (\ s a -> s{_maMetricName = a});

-- | The namespace of alarm\'s associated metric.
maNamespace :: Lens' MetricAlarm (Maybe Text)
maNamespace = lens _maNamespace (\ s a -> s{_maNamespace = a});

-- | The arithmetic operation to use when comparing the specified 'Statistic'
-- and 'Threshold'. The specified 'Statistic' value is used as the first
-- operand.
maComparisonOperator :: Lens' MetricAlarm (Maybe ComparisonOperator)
maComparisonOperator = lens _maComparisonOperator (\ s a -> s{_maComparisonOperator = a});

-- | The list of actions to execute when this alarm transitions into an 'OK'
-- state from any other state. Each action is specified as an Amazon
-- Resource Name (ARN).
maOKActions :: Lens' MetricAlarm [Text]
maOKActions = lens _maOKActions (\ s a -> s{_maOKActions = a}) . _Default . _Coerce;

-- | The state value for the alarm.
maStateValue :: Lens' MetricAlarm (Maybe StateValue)
maStateValue = lens _maStateValue (\ s a -> s{_maStateValue = a});

-- | The value against which the specified statistic is compared.
maThreshold :: Lens' MetricAlarm (Maybe Double)
maThreshold = lens _maThreshold (\ s a -> s{_maThreshold = a});

-- | The time stamp of the last update to the alarm configuration.
maAlarmConfigurationUpdatedTimestamp :: Lens' MetricAlarm (Maybe UTCTime)
maAlarmConfigurationUpdatedTimestamp = lens _maAlarmConfigurationUpdatedTimestamp (\ s a -> s{_maAlarmConfigurationUpdatedTimestamp = a}) . mapping _Time;

-- | Indicates whether actions should be executed during any changes to the
-- alarm\'s state.
maActionsEnabled :: Lens' MetricAlarm (Maybe Bool)
maActionsEnabled = lens _maActionsEnabled (\ s a -> s{_maActionsEnabled = a});

-- | The list of actions to execute when this alarm transitions into an
-- 'INSUFFICIENT_DATA' state from any other state. Each action is specified
-- as an Amazon Resource Name (ARN).
--
-- The current WSDL lists this attribute as 'UnknownActions'.
maInsufficientDataActions :: Lens' MetricAlarm [Text]
maInsufficientDataActions = lens _maInsufficientDataActions (\ s a -> s{_maInsufficientDataActions = a}) . _Default . _Coerce;

-- | A human-readable explanation for the alarm\'s state.
maStateReason :: Lens' MetricAlarm (Maybe Text)
maStateReason = lens _maStateReason (\ s a -> s{_maStateReason = a});

-- | An explanation for the alarm\'s state in machine-readable JSON format
maStateReasonData :: Lens' MetricAlarm (Maybe Text)
maStateReasonData = lens _maStateReasonData (\ s a -> s{_maStateReasonData = a});

-- | The list of dimensions associated with the alarm\'s associated metric.
maDimensions :: Lens' MetricAlarm [Dimension]
maDimensions = lens _maDimensions (\ s a -> s{_maDimensions = a}) . _Default . _Coerce;

-- | The Amazon Resource Name (ARN) of the alarm.
maAlarmARN :: Lens' MetricAlarm (Maybe Text)
maAlarmARN = lens _maAlarmARN (\ s a -> s{_maAlarmARN = a});

-- | The list of actions to execute when this alarm transitions into an
-- 'ALARM' state from any other state. Each action is specified as an
-- Amazon Resource Name (ARN).
maAlarmActions :: Lens' MetricAlarm [Text]
maAlarmActions = lens _maAlarmActions (\ s a -> s{_maAlarmActions = a}) . _Default . _Coerce;

-- | The unit of the alarm\'s associated metric.
maUnit :: Lens' MetricAlarm (Maybe StandardUnit)
maUnit = lens _maUnit (\ s a -> s{_maUnit = a});

-- | The statistic to apply to the alarm\'s associated metric.
maStatistic :: Lens' MetricAlarm (Maybe Statistic)
maStatistic = lens _maStatistic (\ s a -> s{_maStatistic = a});

instance FromXML MetricAlarm where
        parseXML x
          = MetricAlarm' <$>
              (x .@? "AlarmName") <*>
                (x .@? "StateUpdatedTimestamp")
                <*> (x .@? "Period")
                <*> (x .@? "AlarmDescription")
                <*> (x .@? "EvaluationPeriods")
                <*> (x .@? "MetricName")
                <*> (x .@? "Namespace")
                <*> (x .@? "ComparisonOperator")
                <*>
                (x .@? "OKActions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "StateValue")
                <*> (x .@? "Threshold")
                <*> (x .@? "AlarmConfigurationUpdatedTimestamp")
                <*> (x .@? "ActionsEnabled")
                <*>
                (x .@? "InsufficientDataActions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "StateReason")
                <*> (x .@? "StateReasonData")
                <*>
                (x .@? "Dimensions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "AlarmArn")
                <*>
                (x .@? "AlarmActions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "Unit")
                <*> (x .@? "Statistic")

-- | The 'MetricDatum' data type encapsulates the information sent with
-- < PutMetricData> to either create a new metric or add new values to be
-- aggregated into an existing metric.
--
-- /See:/ 'metricDatum' smart constructor.
data MetricDatum = MetricDatum'
    { _mdValue           :: !(Maybe Double)
    , _mdDimensions      :: !(Maybe [Dimension])
    , _mdUnit            :: !(Maybe StandardUnit)
    , _mdTimestamp       :: !(Maybe ISO8601)
    , _mdStatisticValues :: !(Maybe StatisticSet)
    , _mdMetricName      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MetricDatum' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdValue'
--
-- * 'mdDimensions'
--
-- * 'mdUnit'
--
-- * 'mdTimestamp'
--
-- * 'mdStatisticValues'
--
-- * 'mdMetricName'
metricDatum
    :: Text -- ^ 'mdMetricName'
    -> MetricDatum
metricDatum pMetricName_ =
    MetricDatum'
    { _mdValue = Nothing
    , _mdDimensions = Nothing
    , _mdUnit = Nothing
    , _mdTimestamp = Nothing
    , _mdStatisticValues = Nothing
    , _mdMetricName = pMetricName_
    }

-- | The value for the metric.
--
-- Although the 'Value' parameter accepts numbers of type 'Double', Amazon
-- CloudWatch rejects values that are either too small or too large. Values
-- must be in the range of 8.515920e-109 to 1.174271e+108 (Base 10) or
-- 2e-360 to 2e360 (Base 2). In addition, special values (e.g., NaN,
-- +Infinity, -Infinity) are not supported.
mdValue :: Lens' MetricDatum (Maybe Double)
mdValue = lens _mdValue (\ s a -> s{_mdValue = a});

-- | A list of dimensions associated with the metric. Note, when using the
-- Dimensions value in a query, you need to append .member.N to it (e.g.,
-- Dimensions.member.N).
mdDimensions :: Lens' MetricDatum [Dimension]
mdDimensions = lens _mdDimensions (\ s a -> s{_mdDimensions = a}) . _Default . _Coerce;

-- | The unit of the metric.
mdUnit :: Lens' MetricDatum (Maybe StandardUnit)
mdUnit = lens _mdUnit (\ s a -> s{_mdUnit = a});

-- | The time stamp used for the metric in ISO 8601 Universal Coordinated
-- Time (UTC) format. If not specified, the default value is set to the
-- time the metric data was received.
mdTimestamp :: Lens' MetricDatum (Maybe UTCTime)
mdTimestamp = lens _mdTimestamp (\ s a -> s{_mdTimestamp = a}) . mapping _Time;

-- | A set of statistical values describing the metric.
mdStatisticValues :: Lens' MetricDatum (Maybe StatisticSet)
mdStatisticValues = lens _mdStatisticValues (\ s a -> s{_mdStatisticValues = a});

-- | The name of the metric.
mdMetricName :: Lens' MetricDatum Text
mdMetricName = lens _mdMetricName (\ s a -> s{_mdMetricName = a});

instance ToQuery MetricDatum where
        toQuery MetricDatum'{..}
          = mconcat
              ["Value" =: _mdValue,
               "Dimensions" =:
                 toQuery (toQueryList "member" <$> _mdDimensions),
               "Unit" =: _mdUnit, "Timestamp" =: _mdTimestamp,
               "StatisticValues" =: _mdStatisticValues,
               "MetricName" =: _mdMetricName]

-- | The 'StatisticSet' data type describes the 'StatisticValues' component
-- of < MetricDatum>, and represents a set of statistics that describes a
-- specific metric.
--
-- /See:/ 'statisticSet' smart constructor.
data StatisticSet = StatisticSet'
    { _ssSampleCount :: !Double
    , _ssSum         :: !Double
    , _ssMinimum     :: !Double
    , _ssMaximum     :: !Double
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StatisticSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssSampleCount'
--
-- * 'ssSum'
--
-- * 'ssMinimum'
--
-- * 'ssMaximum'
statisticSet
    :: Double -- ^ 'ssSampleCount'
    -> Double -- ^ 'ssSum'
    -> Double -- ^ 'ssMinimum'
    -> Double -- ^ 'ssMaximum'
    -> StatisticSet
statisticSet pSampleCount_ pSum_ pMinimum_ pMaximum_ =
    StatisticSet'
    { _ssSampleCount = pSampleCount_
    , _ssSum = pSum_
    , _ssMinimum = pMinimum_
    , _ssMaximum = pMaximum_
    }

-- | The number of samples used for the statistic set.
ssSampleCount :: Lens' StatisticSet Double
ssSampleCount = lens _ssSampleCount (\ s a -> s{_ssSampleCount = a});

-- | The sum of values for the sample set.
ssSum :: Lens' StatisticSet Double
ssSum = lens _ssSum (\ s a -> s{_ssSum = a});

-- | The minimum value of the sample set.
ssMinimum :: Lens' StatisticSet Double
ssMinimum = lens _ssMinimum (\ s a -> s{_ssMinimum = a});

-- | The maximum value of the sample set.
ssMaximum :: Lens' StatisticSet Double
ssMaximum = lens _ssMaximum (\ s a -> s{_ssMaximum = a});

instance ToQuery StatisticSet where
        toQuery StatisticSet'{..}
          = mconcat
              ["SampleCount" =: _ssSampleCount, "Sum" =: _ssSum,
               "Minimum" =: _ssMinimum, "Maximum" =: _ssMaximum]
