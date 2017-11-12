{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.Product
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatch.Types.Product where

import Network.AWS.CloudWatch.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the history of a specific alarm.
--
--
--
-- /See:/ 'alarmHistoryItem' smart constructor.
data AlarmHistoryItem = AlarmHistoryItem'
  { _ahiAlarmName       :: !(Maybe Text)
  , _ahiHistoryItemType :: !(Maybe HistoryItemType)
  , _ahiHistoryData     :: !(Maybe Text)
  , _ahiHistorySummary  :: !(Maybe Text)
  , _ahiTimestamp       :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AlarmHistoryItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ahiAlarmName' - The descriptive name for the alarm.
--
-- * 'ahiHistoryItemType' - The type of alarm history item.
--
-- * 'ahiHistoryData' - Data about the alarm, in JSON format.
--
-- * 'ahiHistorySummary' - A summary of the alarm history, in text format.
--
-- * 'ahiTimestamp' - The time stamp for the alarm history item.
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

-- | Data about the alarm, in JSON format.
ahiHistoryData :: Lens' AlarmHistoryItem (Maybe Text)
ahiHistoryData = lens _ahiHistoryData (\ s a -> s{_ahiHistoryData = a});

-- | A summary of the alarm history, in text format.
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

instance Hashable AlarmHistoryItem where

instance NFData AlarmHistoryItem where

-- | Represents a specific dashboard.
--
--
--
-- /See:/ 'dashboardEntry' smart constructor.
data DashboardEntry = DashboardEntry'
  { _deSize          :: !(Maybe Integer)
  , _deDashboardName :: !(Maybe Text)
  , _deLastModified  :: !(Maybe ISO8601)
  , _deDashboardARN  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DashboardEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deSize' - The size of the dashboard, in bytes.
--
-- * 'deDashboardName' - The name of the dashboard.
--
-- * 'deLastModified' - The time stamp of when the dashboard was last modified, either by an API call or through the console. This number is expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC.
--
-- * 'deDashboardARN' - The Amazon Resource Name (ARN) of the dashboard.
dashboardEntry
    :: DashboardEntry
dashboardEntry =
  DashboardEntry'
  { _deSize = Nothing
  , _deDashboardName = Nothing
  , _deLastModified = Nothing
  , _deDashboardARN = Nothing
  }


-- | The size of the dashboard, in bytes.
deSize :: Lens' DashboardEntry (Maybe Integer)
deSize = lens _deSize (\ s a -> s{_deSize = a});

-- | The name of the dashboard.
deDashboardName :: Lens' DashboardEntry (Maybe Text)
deDashboardName = lens _deDashboardName (\ s a -> s{_deDashboardName = a});

-- | The time stamp of when the dashboard was last modified, either by an API call or through the console. This number is expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC.
deLastModified :: Lens' DashboardEntry (Maybe UTCTime)
deLastModified = lens _deLastModified (\ s a -> s{_deLastModified = a}) . mapping _Time;

-- | The Amazon Resource Name (ARN) of the dashboard.
deDashboardARN :: Lens' DashboardEntry (Maybe Text)
deDashboardARN = lens _deDashboardARN (\ s a -> s{_deDashboardARN = a});

instance FromXML DashboardEntry where
        parseXML x
          = DashboardEntry' <$>
              (x .@? "Size") <*> (x .@? "DashboardName") <*>
                (x .@? "LastModified")
                <*> (x .@? "DashboardArn")

instance Hashable DashboardEntry where

instance NFData DashboardEntry where

-- | An error or warning for the operation.
--
--
--
-- /See:/ 'dashboardValidationMessage' smart constructor.
data DashboardValidationMessage = DashboardValidationMessage'
  { _dvmDataPath :: !(Maybe Text)
  , _dvmMessage  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DashboardValidationMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvmDataPath' - The data path related to the message.
--
-- * 'dvmMessage' - A message describing the error or warning.
dashboardValidationMessage
    :: DashboardValidationMessage
dashboardValidationMessage =
  DashboardValidationMessage' {_dvmDataPath = Nothing, _dvmMessage = Nothing}


-- | The data path related to the message.
dvmDataPath :: Lens' DashboardValidationMessage (Maybe Text)
dvmDataPath = lens _dvmDataPath (\ s a -> s{_dvmDataPath = a});

-- | A message describing the error or warning.
dvmMessage :: Lens' DashboardValidationMessage (Maybe Text)
dvmMessage = lens _dvmMessage (\ s a -> s{_dvmMessage = a});

instance FromXML DashboardValidationMessage where
        parseXML x
          = DashboardValidationMessage' <$>
              (x .@? "DataPath") <*> (x .@? "Message")

instance Hashable DashboardValidationMessage where

instance NFData DashboardValidationMessage where

-- | Encapsulates the statistical data that CloudWatch computes from metric data.
--
--
--
-- /See:/ 'datapoint' smart constructor.
data Datapoint = Datapoint'
  { _dSampleCount        :: !(Maybe Double)
  , _dMaximum            :: !(Maybe Double)
  , _dAverage            :: !(Maybe Double)
  , _dMinimum            :: !(Maybe Double)
  , _dExtendedStatistics :: !(Maybe (Map Text Double))
  , _dSum                :: !(Maybe Double)
  , _dUnit               :: !(Maybe StandardUnit)
  , _dTimestamp          :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Datapoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dSampleCount' - The number of metric values that contributed to the aggregate value of this data point.
--
-- * 'dMaximum' - The maximum metric value for the data point.
--
-- * 'dAverage' - The average of the metric values that correspond to the data point.
--
-- * 'dMinimum' - The minimum metric value for the data point.
--
-- * 'dExtendedStatistics' - The percentile statistic for the data point.
--
-- * 'dSum' - The sum of the metric values for the data point.
--
-- * 'dUnit' - The standard unit for the data point.
--
-- * 'dTimestamp' - The time stamp used for the data point.
datapoint
    :: Datapoint
datapoint =
  Datapoint'
  { _dSampleCount = Nothing
  , _dMaximum = Nothing
  , _dAverage = Nothing
  , _dMinimum = Nothing
  , _dExtendedStatistics = Nothing
  , _dSum = Nothing
  , _dUnit = Nothing
  , _dTimestamp = Nothing
  }


-- | The number of metric values that contributed to the aggregate value of this data point.
dSampleCount :: Lens' Datapoint (Maybe Double)
dSampleCount = lens _dSampleCount (\ s a -> s{_dSampleCount = a});

-- | The maximum metric value for the data point.
dMaximum :: Lens' Datapoint (Maybe Double)
dMaximum = lens _dMaximum (\ s a -> s{_dMaximum = a});

-- | The average of the metric values that correspond to the data point.
dAverage :: Lens' Datapoint (Maybe Double)
dAverage = lens _dAverage (\ s a -> s{_dAverage = a});

-- | The minimum metric value for the data point.
dMinimum :: Lens' Datapoint (Maybe Double)
dMinimum = lens _dMinimum (\ s a -> s{_dMinimum = a});

-- | The percentile statistic for the data point.
dExtendedStatistics :: Lens' Datapoint (HashMap Text Double)
dExtendedStatistics = lens _dExtendedStatistics (\ s a -> s{_dExtendedStatistics = a}) . _Default . _Map;

-- | The sum of the metric values for the data point.
dSum :: Lens' Datapoint (Maybe Double)
dSum = lens _dSum (\ s a -> s{_dSum = a});

-- | The standard unit for the data point.
dUnit :: Lens' Datapoint (Maybe StandardUnit)
dUnit = lens _dUnit (\ s a -> s{_dUnit = a});

-- | The time stamp used for the data point.
dTimestamp :: Lens' Datapoint (Maybe UTCTime)
dTimestamp = lens _dTimestamp (\ s a -> s{_dTimestamp = a}) . mapping _Time;

instance FromXML Datapoint where
        parseXML x
          = Datapoint' <$>
              (x .@? "SampleCount") <*> (x .@? "Maximum") <*>
                (x .@? "Average")
                <*> (x .@? "Minimum")
                <*>
                (x .@? "ExtendedStatistics" .!@ mempty >>=
                   may (parseXMLMap "entry" "key" "value"))
                <*> (x .@? "Sum")
                <*> (x .@? "Unit")
                <*> (x .@? "Timestamp")

instance Hashable Datapoint where

instance NFData Datapoint where

-- | Expands the identity of a metric.
--
--
--
-- /See:/ 'dimension' smart constructor.
data Dimension = Dimension'
  { _dName  :: !Text
  , _dValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Dimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dName' - The name of the dimension.
--
-- * 'dValue' - The value representing the dimension measurement.
dimension
    :: Text -- ^ 'dName'
    -> Text -- ^ 'dValue'
    -> Dimension
dimension pName_ pValue_ = Dimension' {_dName = pName_, _dValue = pValue_}


-- | The name of the dimension.
dName :: Lens' Dimension Text
dName = lens _dName (\ s a -> s{_dName = a});

-- | The value representing the dimension measurement.
dValue :: Lens' Dimension Text
dValue = lens _dValue (\ s a -> s{_dValue = a});

instance FromXML Dimension where
        parseXML x
          = Dimension' <$> (x .@ "Name") <*> (x .@ "Value")

instance Hashable Dimension where

instance NFData Dimension where

instance ToQuery Dimension where
        toQuery Dimension'{..}
          = mconcat ["Name" =: _dName, "Value" =: _dValue]

-- | Represents filters for a dimension.
--
--
--
-- /See:/ 'dimensionFilter' smart constructor.
data DimensionFilter = DimensionFilter'
  { _dfValue :: !(Maybe Text)
  , _dfName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DimensionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfValue' - The value of the dimension to be matched.
--
-- * 'dfName' - The dimension name to be matched.
dimensionFilter
    :: Text -- ^ 'dfName'
    -> DimensionFilter
dimensionFilter pName_ = DimensionFilter' {_dfValue = Nothing, _dfName = pName_}


-- | The value of the dimension to be matched.
dfValue :: Lens' DimensionFilter (Maybe Text)
dfValue = lens _dfValue (\ s a -> s{_dfValue = a});

-- | The dimension name to be matched.
dfName :: Lens' DimensionFilter Text
dfName = lens _dfName (\ s a -> s{_dfName = a});

instance Hashable DimensionFilter where

instance NFData DimensionFilter where

instance ToQuery DimensionFilter where
        toQuery DimensionFilter'{..}
          = mconcat ["Value" =: _dfValue, "Name" =: _dfName]

-- | Represents a specific metric.
--
--
--
-- /See:/ 'metric' smart constructor.
data Metric = Metric'
  { _mMetricName :: !(Maybe Text)
  , _mNamespace  :: !(Maybe Text)
  , _mDimensions :: !(Maybe [Dimension])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Metric' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mMetricName' - The name of the metric.
--
-- * 'mNamespace' - The namespace of the metric.
--
-- * 'mDimensions' - The dimensions for the metric.
metric
    :: Metric
metric =
  Metric'
  {_mMetricName = Nothing, _mNamespace = Nothing, _mDimensions = Nothing}


-- | The name of the metric.
mMetricName :: Lens' Metric (Maybe Text)
mMetricName = lens _mMetricName (\ s a -> s{_mMetricName = a});

-- | The namespace of the metric.
mNamespace :: Lens' Metric (Maybe Text)
mNamespace = lens _mNamespace (\ s a -> s{_mNamespace = a});

-- | The dimensions for the metric.
mDimensions :: Lens' Metric [Dimension]
mDimensions = lens _mDimensions (\ s a -> s{_mDimensions = a}) . _Default . _Coerce;

instance FromXML Metric where
        parseXML x
          = Metric' <$>
              (x .@? "MetricName") <*> (x .@? "Namespace") <*>
                (x .@? "Dimensions" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable Metric where

instance NFData Metric where

-- | Represents an alarm.
--
--
--
-- /See:/ 'metricAlarm' smart constructor.
data MetricAlarm = MetricAlarm'
  { _maAlarmName                          :: !(Maybe Text)
  , _maStateUpdatedTimestamp              :: !(Maybe ISO8601)
  , _maTreatMissingData                   :: !(Maybe Text)
  , _maPeriod                             :: !(Maybe Nat)
  , _maAlarmDescription                   :: !(Maybe Text)
  , _maEvaluationPeriods                  :: !(Maybe Nat)
  , _maMetricName                         :: !(Maybe Text)
  , _maNamespace                          :: !(Maybe Text)
  , _maComparisonOperator                 :: !(Maybe ComparisonOperator)
  , _maOKActions                          :: !(Maybe [Text])
  , _maEvaluateLowSampleCountPercentile   :: !(Maybe Text)
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
  , _maExtendedStatistic                  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricAlarm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'maAlarmName' - The name of the alarm.
--
-- * 'maStateUpdatedTimestamp' - The time stamp of the last update to the alarm state.
--
-- * 'maTreatMissingData' - Sets how this alarm is to handle missing data points. If this parameter is omitted, the default behavior of @missing@ is used.
--
-- * 'maPeriod' - The period, in seconds, over which the statistic is applied.
--
-- * 'maAlarmDescription' - The description of the alarm.
--
-- * 'maEvaluationPeriods' - The number of periods over which data is compared to the specified threshold.
--
-- * 'maMetricName' - The name of the metric associated with the alarm.
--
-- * 'maNamespace' - The namespace of the metric associated with the alarm.
--
-- * 'maComparisonOperator' - The arithmetic operation to use when comparing the specified statistic and threshold. The specified statistic value is used as the first operand.
--
-- * 'maOKActions' - The actions to execute when this alarm transitions to the @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- * 'maEvaluateLowSampleCountPercentile' - Used only for alarms based on percentiles. If @ignore@ , the alarm state does not change during periods with too few data points to be statistically significant. If @evaluate@ or this parameter is not used, the alarm is always evaluated and possibly changes state no matter how many data points are available.
--
-- * 'maStateValue' - The state value for the alarm.
--
-- * 'maThreshold' - The value to compare with the specified statistic.
--
-- * 'maAlarmConfigurationUpdatedTimestamp' - The time stamp of the last update to the alarm configuration.
--
-- * 'maActionsEnabled' - Indicates whether actions should be executed during any changes to the alarm state.
--
-- * 'maInsufficientDataActions' - The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- * 'maStateReason' - An explanation for the alarm state, in text format.
--
-- * 'maStateReasonData' - An explanation for the alarm state, in JSON format.
--
-- * 'maDimensions' - The dimensions for the metric associated with the alarm.
--
-- * 'maAlarmARN' - The Amazon Resource Name (ARN) of the alarm.
--
-- * 'maAlarmActions' - The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- * 'maUnit' - The unit of the metric associated with the alarm.
--
-- * 'maStatistic' - The statistic for the metric associated with the alarm, other than percentile. For percentile statistics, use @ExtendedStatistic@ .
--
-- * 'maExtendedStatistic' - The percentile statistic for the metric associated with the alarm. Specify a value between p0.0 and p100.
metricAlarm
    :: MetricAlarm
metricAlarm =
  MetricAlarm'
  { _maAlarmName = Nothing
  , _maStateUpdatedTimestamp = Nothing
  , _maTreatMissingData = Nothing
  , _maPeriod = Nothing
  , _maAlarmDescription = Nothing
  , _maEvaluationPeriods = Nothing
  , _maMetricName = Nothing
  , _maNamespace = Nothing
  , _maComparisonOperator = Nothing
  , _maOKActions = Nothing
  , _maEvaluateLowSampleCountPercentile = Nothing
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
  , _maExtendedStatistic = Nothing
  }


-- | The name of the alarm.
maAlarmName :: Lens' MetricAlarm (Maybe Text)
maAlarmName = lens _maAlarmName (\ s a -> s{_maAlarmName = a});

-- | The time stamp of the last update to the alarm state.
maStateUpdatedTimestamp :: Lens' MetricAlarm (Maybe UTCTime)
maStateUpdatedTimestamp = lens _maStateUpdatedTimestamp (\ s a -> s{_maStateUpdatedTimestamp = a}) . mapping _Time;

-- | Sets how this alarm is to handle missing data points. If this parameter is omitted, the default behavior of @missing@ is used.
maTreatMissingData :: Lens' MetricAlarm (Maybe Text)
maTreatMissingData = lens _maTreatMissingData (\ s a -> s{_maTreatMissingData = a});

-- | The period, in seconds, over which the statistic is applied.
maPeriod :: Lens' MetricAlarm (Maybe Natural)
maPeriod = lens _maPeriod (\ s a -> s{_maPeriod = a}) . mapping _Nat;

-- | The description of the alarm.
maAlarmDescription :: Lens' MetricAlarm (Maybe Text)
maAlarmDescription = lens _maAlarmDescription (\ s a -> s{_maAlarmDescription = a});

-- | The number of periods over which data is compared to the specified threshold.
maEvaluationPeriods :: Lens' MetricAlarm (Maybe Natural)
maEvaluationPeriods = lens _maEvaluationPeriods (\ s a -> s{_maEvaluationPeriods = a}) . mapping _Nat;

-- | The name of the metric associated with the alarm.
maMetricName :: Lens' MetricAlarm (Maybe Text)
maMetricName = lens _maMetricName (\ s a -> s{_maMetricName = a});

-- | The namespace of the metric associated with the alarm.
maNamespace :: Lens' MetricAlarm (Maybe Text)
maNamespace = lens _maNamespace (\ s a -> s{_maNamespace = a});

-- | The arithmetic operation to use when comparing the specified statistic and threshold. The specified statistic value is used as the first operand.
maComparisonOperator :: Lens' MetricAlarm (Maybe ComparisonOperator)
maComparisonOperator = lens _maComparisonOperator (\ s a -> s{_maComparisonOperator = a});

-- | The actions to execute when this alarm transitions to the @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
maOKActions :: Lens' MetricAlarm [Text]
maOKActions = lens _maOKActions (\ s a -> s{_maOKActions = a}) . _Default . _Coerce;

-- | Used only for alarms based on percentiles. If @ignore@ , the alarm state does not change during periods with too few data points to be statistically significant. If @evaluate@ or this parameter is not used, the alarm is always evaluated and possibly changes state no matter how many data points are available.
maEvaluateLowSampleCountPercentile :: Lens' MetricAlarm (Maybe Text)
maEvaluateLowSampleCountPercentile = lens _maEvaluateLowSampleCountPercentile (\ s a -> s{_maEvaluateLowSampleCountPercentile = a});

-- | The state value for the alarm.
maStateValue :: Lens' MetricAlarm (Maybe StateValue)
maStateValue = lens _maStateValue (\ s a -> s{_maStateValue = a});

-- | The value to compare with the specified statistic.
maThreshold :: Lens' MetricAlarm (Maybe Double)
maThreshold = lens _maThreshold (\ s a -> s{_maThreshold = a});

-- | The time stamp of the last update to the alarm configuration.
maAlarmConfigurationUpdatedTimestamp :: Lens' MetricAlarm (Maybe UTCTime)
maAlarmConfigurationUpdatedTimestamp = lens _maAlarmConfigurationUpdatedTimestamp (\ s a -> s{_maAlarmConfigurationUpdatedTimestamp = a}) . mapping _Time;

-- | Indicates whether actions should be executed during any changes to the alarm state.
maActionsEnabled :: Lens' MetricAlarm (Maybe Bool)
maActionsEnabled = lens _maActionsEnabled (\ s a -> s{_maActionsEnabled = a});

-- | The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
maInsufficientDataActions :: Lens' MetricAlarm [Text]
maInsufficientDataActions = lens _maInsufficientDataActions (\ s a -> s{_maInsufficientDataActions = a}) . _Default . _Coerce;

-- | An explanation for the alarm state, in text format.
maStateReason :: Lens' MetricAlarm (Maybe Text)
maStateReason = lens _maStateReason (\ s a -> s{_maStateReason = a});

-- | An explanation for the alarm state, in JSON format.
maStateReasonData :: Lens' MetricAlarm (Maybe Text)
maStateReasonData = lens _maStateReasonData (\ s a -> s{_maStateReasonData = a});

-- | The dimensions for the metric associated with the alarm.
maDimensions :: Lens' MetricAlarm [Dimension]
maDimensions = lens _maDimensions (\ s a -> s{_maDimensions = a}) . _Default . _Coerce;

-- | The Amazon Resource Name (ARN) of the alarm.
maAlarmARN :: Lens' MetricAlarm (Maybe Text)
maAlarmARN = lens _maAlarmARN (\ s a -> s{_maAlarmARN = a});

-- | The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
maAlarmActions :: Lens' MetricAlarm [Text]
maAlarmActions = lens _maAlarmActions (\ s a -> s{_maAlarmActions = a}) . _Default . _Coerce;

-- | The unit of the metric associated with the alarm.
maUnit :: Lens' MetricAlarm (Maybe StandardUnit)
maUnit = lens _maUnit (\ s a -> s{_maUnit = a});

-- | The statistic for the metric associated with the alarm, other than percentile. For percentile statistics, use @ExtendedStatistic@ .
maStatistic :: Lens' MetricAlarm (Maybe Statistic)
maStatistic = lens _maStatistic (\ s a -> s{_maStatistic = a});

-- | The percentile statistic for the metric associated with the alarm. Specify a value between p0.0 and p100.
maExtendedStatistic :: Lens' MetricAlarm (Maybe Text)
maExtendedStatistic = lens _maExtendedStatistic (\ s a -> s{_maExtendedStatistic = a});

instance FromXML MetricAlarm where
        parseXML x
          = MetricAlarm' <$>
              (x .@? "AlarmName") <*>
                (x .@? "StateUpdatedTimestamp")
                <*> (x .@? "TreatMissingData")
                <*> (x .@? "Period")
                <*> (x .@? "AlarmDescription")
                <*> (x .@? "EvaluationPeriods")
                <*> (x .@? "MetricName")
                <*> (x .@? "Namespace")
                <*> (x .@? "ComparisonOperator")
                <*>
                (x .@? "OKActions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "EvaluateLowSampleCountPercentile")
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
                <*> (x .@? "ExtendedStatistic")

instance Hashable MetricAlarm where

instance NFData MetricAlarm where

-- | Encapsulates the information sent to either create a metric or add new values to be aggregated into an existing metric.
--
--
--
-- /See:/ 'metricDatum' smart constructor.
data MetricDatum = MetricDatum'
  { _mdValue             :: !(Maybe Double)
  , _mdStorageResolution :: !(Maybe Nat)
  , _mdDimensions        :: !(Maybe [Dimension])
  , _mdUnit              :: !(Maybe StandardUnit)
  , _mdTimestamp         :: !(Maybe ISO8601)
  , _mdStatisticValues   :: !(Maybe StatisticSet)
  , _mdMetricName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricDatum' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdValue' - The value for the metric. Although the parameter accepts numbers of type Double, CloudWatch rejects values that are either too small or too large. Values must be in the range of 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2). In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.
--
-- * 'mdStorageResolution' - Valid values are 1 and 60. Setting this to 1 specifies this metric as a high-resolution metric, so that CloudWatch stores the metric with sub-minute resolution down to one second. Setting this to 60 specifies this metric as a regular-resolution metric, which CloudWatch stores at 1-minute resolution. Currently, high resolution is available only for custom metrics. For more information about high-resolution metrics, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html#high-resolution-metrics High-Resolution Metrics> in the /Amazon CloudWatch User Guide/ .  This field is optional, if you do not specify it the default of 60 is used.
--
-- * 'mdDimensions' - The dimensions associated with the metric.
--
-- * 'mdUnit' - The unit of the metric.
--
-- * 'mdTimestamp' - The time the metric data was received, expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC.
--
-- * 'mdStatisticValues' - The statistical values for the metric.
--
-- * 'mdMetricName' - The name of the metric.
metricDatum
    :: Text -- ^ 'mdMetricName'
    -> MetricDatum
metricDatum pMetricName_ =
  MetricDatum'
  { _mdValue = Nothing
  , _mdStorageResolution = Nothing
  , _mdDimensions = Nothing
  , _mdUnit = Nothing
  , _mdTimestamp = Nothing
  , _mdStatisticValues = Nothing
  , _mdMetricName = pMetricName_
  }


-- | The value for the metric. Although the parameter accepts numbers of type Double, CloudWatch rejects values that are either too small or too large. Values must be in the range of 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2). In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.
mdValue :: Lens' MetricDatum (Maybe Double)
mdValue = lens _mdValue (\ s a -> s{_mdValue = a});

-- | Valid values are 1 and 60. Setting this to 1 specifies this metric as a high-resolution metric, so that CloudWatch stores the metric with sub-minute resolution down to one second. Setting this to 60 specifies this metric as a regular-resolution metric, which CloudWatch stores at 1-minute resolution. Currently, high resolution is available only for custom metrics. For more information about high-resolution metrics, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html#high-resolution-metrics High-Resolution Metrics> in the /Amazon CloudWatch User Guide/ .  This field is optional, if you do not specify it the default of 60 is used.
mdStorageResolution :: Lens' MetricDatum (Maybe Natural)
mdStorageResolution = lens _mdStorageResolution (\ s a -> s{_mdStorageResolution = a}) . mapping _Nat;

-- | The dimensions associated with the metric.
mdDimensions :: Lens' MetricDatum [Dimension]
mdDimensions = lens _mdDimensions (\ s a -> s{_mdDimensions = a}) . _Default . _Coerce;

-- | The unit of the metric.
mdUnit :: Lens' MetricDatum (Maybe StandardUnit)
mdUnit = lens _mdUnit (\ s a -> s{_mdUnit = a});

-- | The time the metric data was received, expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC.
mdTimestamp :: Lens' MetricDatum (Maybe UTCTime)
mdTimestamp = lens _mdTimestamp (\ s a -> s{_mdTimestamp = a}) . mapping _Time;

-- | The statistical values for the metric.
mdStatisticValues :: Lens' MetricDatum (Maybe StatisticSet)
mdStatisticValues = lens _mdStatisticValues (\ s a -> s{_mdStatisticValues = a});

-- | The name of the metric.
mdMetricName :: Lens' MetricDatum Text
mdMetricName = lens _mdMetricName (\ s a -> s{_mdMetricName = a});

instance Hashable MetricDatum where

instance NFData MetricDatum where

instance ToQuery MetricDatum where
        toQuery MetricDatum'{..}
          = mconcat
              ["Value" =: _mdValue,
               "StorageResolution" =: _mdStorageResolution,
               "Dimensions" =:
                 toQuery (toQueryList "member" <$> _mdDimensions),
               "Unit" =: _mdUnit, "Timestamp" =: _mdTimestamp,
               "StatisticValues" =: _mdStatisticValues,
               "MetricName" =: _mdMetricName]

-- | Represents a set of statistics that describes a specific metric.
--
--
--
-- /See:/ 'statisticSet' smart constructor.
data StatisticSet = StatisticSet'
  { _ssSampleCount :: !Double
  , _ssSum         :: !Double
  , _ssMinimum     :: !Double
  , _ssMaximum     :: !Double
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StatisticSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssSampleCount' - The number of samples used for the statistic set.
--
-- * 'ssSum' - The sum of values for the sample set.
--
-- * 'ssMinimum' - The minimum value of the sample set.
--
-- * 'ssMaximum' - The maximum value of the sample set.
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

instance Hashable StatisticSet where

instance NFData StatisticSet where

instance ToQuery StatisticSet where
        toQuery StatisticSet'{..}
          = mconcat
              ["SampleCount" =: _ssSampleCount, "Sum" =: _ssSum,
               "Minimum" =: _ssMinimum, "Maximum" =: _ssMaximum]
