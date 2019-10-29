{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
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
ahiAlarmName = lens _ahiAlarmName (\ s a -> s{_ahiAlarmName = a})

-- | The type of alarm history item.
ahiHistoryItemType :: Lens' AlarmHistoryItem (Maybe HistoryItemType)
ahiHistoryItemType = lens _ahiHistoryItemType (\ s a -> s{_ahiHistoryItemType = a})

-- | Data about the alarm, in JSON format.
ahiHistoryData :: Lens' AlarmHistoryItem (Maybe Text)
ahiHistoryData = lens _ahiHistoryData (\ s a -> s{_ahiHistoryData = a})

-- | A summary of the alarm history, in text format.
ahiHistorySummary :: Lens' AlarmHistoryItem (Maybe Text)
ahiHistorySummary = lens _ahiHistorySummary (\ s a -> s{_ahiHistorySummary = a})

-- | The time stamp for the alarm history item.
ahiTimestamp :: Lens' AlarmHistoryItem (Maybe UTCTime)
ahiTimestamp = lens _ahiTimestamp (\ s a -> s{_ahiTimestamp = a}) . mapping _Time

instance FromXML AlarmHistoryItem where
        parseXML x
          = AlarmHistoryItem' <$>
              (x .@? "AlarmName") <*> (x .@? "HistoryItemType") <*>
                (x .@? "HistoryData")
                <*> (x .@? "HistorySummary")
                <*> (x .@? "Timestamp")

instance Hashable AlarmHistoryItem where

instance NFData AlarmHistoryItem where

-- | An anomaly detection model associated with a particular CloudWatch metric athresnd statistic. You can use the model to display a band of expected normal values when the metric is graphed.
--
--
--
-- /See:/ 'anomalyDetector' smart constructor.
data AnomalyDetector = AnomalyDetector'
  { _adMetricName    :: !(Maybe Text)
  , _adNamespace     :: !(Maybe Text)
  , _adStat          :: !(Maybe Text)
  , _adConfiguration :: !(Maybe AnomalyDetectorConfiguration)
  , _adDimensions    :: !(Maybe [Dimension])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AnomalyDetector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adMetricName' - The name of the metric associated with the anomaly detection model.
--
-- * 'adNamespace' - The namespace of the metric associated with the anomaly detection model.
--
-- * 'adStat' - The statistic associated with the anomaly detection model.
--
-- * 'adConfiguration' - The configuration specifies details about how the anomaly detection model is to be trained, including time ranges to exclude from use for training the model, and the time zone to use for the metric.
--
-- * 'adDimensions' - The metric dimensions associated with the anomaly detection model.
anomalyDetector
    :: AnomalyDetector
anomalyDetector =
  AnomalyDetector'
    { _adMetricName = Nothing
    , _adNamespace = Nothing
    , _adStat = Nothing
    , _adConfiguration = Nothing
    , _adDimensions = Nothing
    }


-- | The name of the metric associated with the anomaly detection model.
adMetricName :: Lens' AnomalyDetector (Maybe Text)
adMetricName = lens _adMetricName (\ s a -> s{_adMetricName = a})

-- | The namespace of the metric associated with the anomaly detection model.
adNamespace :: Lens' AnomalyDetector (Maybe Text)
adNamespace = lens _adNamespace (\ s a -> s{_adNamespace = a})

-- | The statistic associated with the anomaly detection model.
adStat :: Lens' AnomalyDetector (Maybe Text)
adStat = lens _adStat (\ s a -> s{_adStat = a})

-- | The configuration specifies details about how the anomaly detection model is to be trained, including time ranges to exclude from use for training the model, and the time zone to use for the metric.
adConfiguration :: Lens' AnomalyDetector (Maybe AnomalyDetectorConfiguration)
adConfiguration = lens _adConfiguration (\ s a -> s{_adConfiguration = a})

-- | The metric dimensions associated with the anomaly detection model.
adDimensions :: Lens' AnomalyDetector [Dimension]
adDimensions = lens _adDimensions (\ s a -> s{_adDimensions = a}) . _Default . _Coerce

instance FromXML AnomalyDetector where
        parseXML x
          = AnomalyDetector' <$>
              (x .@? "MetricName") <*> (x .@? "Namespace") <*>
                (x .@? "Stat")
                <*> (x .@? "Configuration")
                <*>
                (x .@? "Dimensions" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable AnomalyDetector where

instance NFData AnomalyDetector where

-- | The configuration specifies details about how the anomaly detection model is to be trained, including time ranges to exclude from use for training the model and the time zone to use for the metric.
--
--
--
-- /See:/ 'anomalyDetectorConfiguration' smart constructor.
data AnomalyDetectorConfiguration = AnomalyDetectorConfiguration'
  { _adcMetricTimezone     :: !(Maybe Text)
  , _adcExcludedTimeRanges :: !(Maybe [Range])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AnomalyDetectorConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adcMetricTimezone' - The time zone to use for the metric. This is useful to enable the model to automatically account for daylight savings time changes if the metric is sensitive to such time changes. To specify a time zone, use the name of the time zone as specified in the standard tz database. For more information, see <https://en.wikipedia.org/wiki/Tz_database tz database> .
--
-- * 'adcExcludedTimeRanges' - An array of time ranges to exclude from use when the anomaly detection model is trained. Use this to make sure that events that could cause unusual values for the metric, such as deployments, aren't used when CloudWatch creates the model.
anomalyDetectorConfiguration
    :: AnomalyDetectorConfiguration
anomalyDetectorConfiguration =
  AnomalyDetectorConfiguration'
    {_adcMetricTimezone = Nothing, _adcExcludedTimeRanges = Nothing}


-- | The time zone to use for the metric. This is useful to enable the model to automatically account for daylight savings time changes if the metric is sensitive to such time changes. To specify a time zone, use the name of the time zone as specified in the standard tz database. For more information, see <https://en.wikipedia.org/wiki/Tz_database tz database> .
adcMetricTimezone :: Lens' AnomalyDetectorConfiguration (Maybe Text)
adcMetricTimezone = lens _adcMetricTimezone (\ s a -> s{_adcMetricTimezone = a})

-- | An array of time ranges to exclude from use when the anomaly detection model is trained. Use this to make sure that events that could cause unusual values for the metric, such as deployments, aren't used when CloudWatch creates the model.
adcExcludedTimeRanges :: Lens' AnomalyDetectorConfiguration [Range]
adcExcludedTimeRanges = lens _adcExcludedTimeRanges (\ s a -> s{_adcExcludedTimeRanges = a}) . _Default . _Coerce

instance FromXML AnomalyDetectorConfiguration where
        parseXML x
          = AnomalyDetectorConfiguration' <$>
              (x .@? "MetricTimezone") <*>
                (x .@? "ExcludedTimeRanges" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable AnomalyDetectorConfiguration where

instance NFData AnomalyDetectorConfiguration where

instance ToQuery AnomalyDetectorConfiguration where
        toQuery AnomalyDetectorConfiguration'{..}
          = mconcat
              ["MetricTimezone" =: _adcMetricTimezone,
               "ExcludedTimeRanges" =:
                 toQuery
                   (toQueryList "member" <$> _adcExcludedTimeRanges)]

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
deSize = lens _deSize (\ s a -> s{_deSize = a})

-- | The name of the dashboard.
deDashboardName :: Lens' DashboardEntry (Maybe Text)
deDashboardName = lens _deDashboardName (\ s a -> s{_deDashboardName = a})

-- | The time stamp of when the dashboard was last modified, either by an API call or through the console. This number is expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC.
deLastModified :: Lens' DashboardEntry (Maybe UTCTime)
deLastModified = lens _deLastModified (\ s a -> s{_deLastModified = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the dashboard.
deDashboardARN :: Lens' DashboardEntry (Maybe Text)
deDashboardARN = lens _deDashboardARN (\ s a -> s{_deDashboardARN = a})

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
dvmDataPath = lens _dvmDataPath (\ s a -> s{_dvmDataPath = a})

-- | A message describing the error or warning.
dvmMessage :: Lens' DashboardValidationMessage (Maybe Text)
dvmMessage = lens _dvmMessage (\ s a -> s{_dvmMessage = a})

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
dSampleCount = lens _dSampleCount (\ s a -> s{_dSampleCount = a})

-- | The maximum metric value for the data point.
dMaximum :: Lens' Datapoint (Maybe Double)
dMaximum = lens _dMaximum (\ s a -> s{_dMaximum = a})

-- | The average of the metric values that correspond to the data point.
dAverage :: Lens' Datapoint (Maybe Double)
dAverage = lens _dAverage (\ s a -> s{_dAverage = a})

-- | The minimum metric value for the data point.
dMinimum :: Lens' Datapoint (Maybe Double)
dMinimum = lens _dMinimum (\ s a -> s{_dMinimum = a})

-- | The percentile statistic for the data point.
dExtendedStatistics :: Lens' Datapoint (HashMap Text Double)
dExtendedStatistics = lens _dExtendedStatistics (\ s a -> s{_dExtendedStatistics = a}) . _Default . _Map

-- | The sum of the metric values for the data point.
dSum :: Lens' Datapoint (Maybe Double)
dSum = lens _dSum (\ s a -> s{_dSum = a})

-- | The standard unit for the data point.
dUnit :: Lens' Datapoint (Maybe StandardUnit)
dUnit = lens _dUnit (\ s a -> s{_dUnit = a})

-- | The time stamp used for the data point.
dTimestamp :: Lens' Datapoint (Maybe UTCTime)
dTimestamp = lens _dTimestamp (\ s a -> s{_dTimestamp = a}) . mapping _Time

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
dName = lens _dName (\ s a -> s{_dName = a})

-- | The value representing the dimension measurement.
dValue :: Lens' Dimension Text
dValue = lens _dValue (\ s a -> s{_dValue = a})

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
dfValue = lens _dfValue (\ s a -> s{_dfValue = a})

-- | The dimension name to be matched.
dfName :: Lens' DimensionFilter Text
dfName = lens _dfName (\ s a -> s{_dfName = a})

instance Hashable DimensionFilter where

instance NFData DimensionFilter where

instance ToQuery DimensionFilter where
        toQuery DimensionFilter'{..}
          = mconcat ["Value" =: _dfValue, "Name" =: _dfName]

-- | A message returned by the @GetMetricData@ API, including a code and a description.
--
--
--
-- /See:/ 'messageData' smart constructor.
data MessageData = MessageData'
  { _mValue :: !(Maybe Text)
  , _mCode  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MessageData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mValue' - The message text.
--
-- * 'mCode' - The error code or status code associated with the message.
messageData
    :: MessageData
messageData = MessageData' {_mValue = Nothing, _mCode = Nothing}


-- | The message text.
mValue :: Lens' MessageData (Maybe Text)
mValue = lens _mValue (\ s a -> s{_mValue = a})

-- | The error code or status code associated with the message.
mCode :: Lens' MessageData (Maybe Text)
mCode = lens _mCode (\ s a -> s{_mCode = a})

instance FromXML MessageData where
        parseXML x
          = MessageData' <$> (x .@? "Value") <*> (x .@? "Code")

instance Hashable MessageData where

instance NFData MessageData where

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
-- * 'mMetricName' - The name of the metric. This is a required field.
--
-- * 'mNamespace' - The namespace of the metric.
--
-- * 'mDimensions' - The dimensions for the metric.
metric
    :: Metric
metric =
  Metric'
    {_mMetricName = Nothing, _mNamespace = Nothing, _mDimensions = Nothing}


-- | The name of the metric. This is a required field.
mMetricName :: Lens' Metric (Maybe Text)
mMetricName = lens _mMetricName (\ s a -> s{_mMetricName = a})

-- | The namespace of the metric.
mNamespace :: Lens' Metric (Maybe Text)
mNamespace = lens _mNamespace (\ s a -> s{_mNamespace = a})

-- | The dimensions for the metric.
mDimensions :: Lens' Metric [Dimension]
mDimensions = lens _mDimensions (\ s a -> s{_mDimensions = a}) . _Default . _Coerce

instance FromXML Metric where
        parseXML x
          = Metric' <$>
              (x .@? "MetricName") <*> (x .@? "Namespace") <*>
                (x .@? "Dimensions" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable Metric where

instance NFData Metric where

instance ToQuery Metric where
        toQuery Metric'{..}
          = mconcat
              ["MetricName" =: _mMetricName,
               "Namespace" =: _mNamespace,
               "Dimensions" =:
                 toQuery (toQueryList "member" <$> _mDimensions)]

-- | Represents an alarm.
--
--
--
-- /See:/ 'metricAlarm' smart constructor.
data MetricAlarm = MetricAlarm'
  { _maAlarmName                          :: !(Maybe Text)
  , _maStateUpdatedTimestamp              :: !(Maybe ISO8601)
  , _maMetrics                            :: !(Maybe [MetricDataQuery])
  , _maTreatMissingData                   :: !(Maybe Text)
  , _maPeriod                             :: !(Maybe Nat)
  , _maAlarmDescription                   :: !(Maybe Text)
  , _maEvaluationPeriods                  :: !(Maybe Nat)
  , _maMetricName                         :: !(Maybe Text)
  , _maNamespace                          :: !(Maybe Text)
  , _maThresholdMetricId                  :: !(Maybe Text)
  , _maComparisonOperator                 :: !(Maybe ComparisonOperator)
  , _maOKActions                          :: !(Maybe [Text])
  , _maEvaluateLowSampleCountPercentile   :: !(Maybe Text)
  , _maStateValue                         :: !(Maybe StateValue)
  , _maDatapointsToAlarm                  :: !(Maybe Nat)
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
-- * 'maMetrics' - An array of MetricDataQuery structures, used in an alarm based on a metric math expression. Each structure either retrieves a metric or performs a math expression. One item in the Metrics array is the math expression that the alarm watches. This expression by designated by having @ReturnValue@ set to true.
--
-- * 'maTreatMissingData' - Sets how this alarm is to handle missing data points. If this parameter is omitted, the default behavior of @missing@ is used.
--
-- * 'maPeriod' - The period, in seconds, over which the statistic is applied.
--
-- * 'maAlarmDescription' - The description of the alarm.
--
-- * 'maEvaluationPeriods' - The number of periods over which data is compared to the specified threshold.
--
-- * 'maMetricName' - The name of the metric associated with the alarm, if this is an alarm based on a single metric.
--
-- * 'maNamespace' - The namespace of the metric associated with the alarm.
--
-- * 'maThresholdMetricId' - In an alarm based on an anomaly detection model, this is the ID of the @ANOMALY_DETECTION_BAND@ function used as the threshold for the alarm.
--
-- * 'maComparisonOperator' - The arithmetic operation to use when comparing the specified statistic and threshold. The specified statistic value is used as the first operand.
--
-- * 'maOKActions' - The actions to execute when this alarm transitions to the @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- * 'maEvaluateLowSampleCountPercentile' - Used only for alarms based on percentiles. If @ignore@ , the alarm state does not change during periods with too few data points to be statistically significant. If @evaluate@ or this parameter is not used, the alarm is always evaluated and possibly changes state no matter how many data points are available.
--
-- * 'maStateValue' - The state value for the alarm.
--
-- * 'maDatapointsToAlarm' - The number of datapoints that must be breaching to trigger the alarm.
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
    , _maMetrics = Nothing
    , _maTreatMissingData = Nothing
    , _maPeriod = Nothing
    , _maAlarmDescription = Nothing
    , _maEvaluationPeriods = Nothing
    , _maMetricName = Nothing
    , _maNamespace = Nothing
    , _maThresholdMetricId = Nothing
    , _maComparisonOperator = Nothing
    , _maOKActions = Nothing
    , _maEvaluateLowSampleCountPercentile = Nothing
    , _maStateValue = Nothing
    , _maDatapointsToAlarm = Nothing
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
maAlarmName = lens _maAlarmName (\ s a -> s{_maAlarmName = a})

-- | The time stamp of the last update to the alarm state.
maStateUpdatedTimestamp :: Lens' MetricAlarm (Maybe UTCTime)
maStateUpdatedTimestamp = lens _maStateUpdatedTimestamp (\ s a -> s{_maStateUpdatedTimestamp = a}) . mapping _Time

-- | An array of MetricDataQuery structures, used in an alarm based on a metric math expression. Each structure either retrieves a metric or performs a math expression. One item in the Metrics array is the math expression that the alarm watches. This expression by designated by having @ReturnValue@ set to true.
maMetrics :: Lens' MetricAlarm [MetricDataQuery]
maMetrics = lens _maMetrics (\ s a -> s{_maMetrics = a}) . _Default . _Coerce

-- | Sets how this alarm is to handle missing data points. If this parameter is omitted, the default behavior of @missing@ is used.
maTreatMissingData :: Lens' MetricAlarm (Maybe Text)
maTreatMissingData = lens _maTreatMissingData (\ s a -> s{_maTreatMissingData = a})

-- | The period, in seconds, over which the statistic is applied.
maPeriod :: Lens' MetricAlarm (Maybe Natural)
maPeriod = lens _maPeriod (\ s a -> s{_maPeriod = a}) . mapping _Nat

-- | The description of the alarm.
maAlarmDescription :: Lens' MetricAlarm (Maybe Text)
maAlarmDescription = lens _maAlarmDescription (\ s a -> s{_maAlarmDescription = a})

-- | The number of periods over which data is compared to the specified threshold.
maEvaluationPeriods :: Lens' MetricAlarm (Maybe Natural)
maEvaluationPeriods = lens _maEvaluationPeriods (\ s a -> s{_maEvaluationPeriods = a}) . mapping _Nat

-- | The name of the metric associated with the alarm, if this is an alarm based on a single metric.
maMetricName :: Lens' MetricAlarm (Maybe Text)
maMetricName = lens _maMetricName (\ s a -> s{_maMetricName = a})

-- | The namespace of the metric associated with the alarm.
maNamespace :: Lens' MetricAlarm (Maybe Text)
maNamespace = lens _maNamespace (\ s a -> s{_maNamespace = a})

-- | In an alarm based on an anomaly detection model, this is the ID of the @ANOMALY_DETECTION_BAND@ function used as the threshold for the alarm.
maThresholdMetricId :: Lens' MetricAlarm (Maybe Text)
maThresholdMetricId = lens _maThresholdMetricId (\ s a -> s{_maThresholdMetricId = a})

-- | The arithmetic operation to use when comparing the specified statistic and threshold. The specified statistic value is used as the first operand.
maComparisonOperator :: Lens' MetricAlarm (Maybe ComparisonOperator)
maComparisonOperator = lens _maComparisonOperator (\ s a -> s{_maComparisonOperator = a})

-- | The actions to execute when this alarm transitions to the @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
maOKActions :: Lens' MetricAlarm [Text]
maOKActions = lens _maOKActions (\ s a -> s{_maOKActions = a}) . _Default . _Coerce

-- | Used only for alarms based on percentiles. If @ignore@ , the alarm state does not change during periods with too few data points to be statistically significant. If @evaluate@ or this parameter is not used, the alarm is always evaluated and possibly changes state no matter how many data points are available.
maEvaluateLowSampleCountPercentile :: Lens' MetricAlarm (Maybe Text)
maEvaluateLowSampleCountPercentile = lens _maEvaluateLowSampleCountPercentile (\ s a -> s{_maEvaluateLowSampleCountPercentile = a})

-- | The state value for the alarm.
maStateValue :: Lens' MetricAlarm (Maybe StateValue)
maStateValue = lens _maStateValue (\ s a -> s{_maStateValue = a})

-- | The number of datapoints that must be breaching to trigger the alarm.
maDatapointsToAlarm :: Lens' MetricAlarm (Maybe Natural)
maDatapointsToAlarm = lens _maDatapointsToAlarm (\ s a -> s{_maDatapointsToAlarm = a}) . mapping _Nat

-- | The value to compare with the specified statistic.
maThreshold :: Lens' MetricAlarm (Maybe Double)
maThreshold = lens _maThreshold (\ s a -> s{_maThreshold = a})

-- | The time stamp of the last update to the alarm configuration.
maAlarmConfigurationUpdatedTimestamp :: Lens' MetricAlarm (Maybe UTCTime)
maAlarmConfigurationUpdatedTimestamp = lens _maAlarmConfigurationUpdatedTimestamp (\ s a -> s{_maAlarmConfigurationUpdatedTimestamp = a}) . mapping _Time

-- | Indicates whether actions should be executed during any changes to the alarm state.
maActionsEnabled :: Lens' MetricAlarm (Maybe Bool)
maActionsEnabled = lens _maActionsEnabled (\ s a -> s{_maActionsEnabled = a})

-- | The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
maInsufficientDataActions :: Lens' MetricAlarm [Text]
maInsufficientDataActions = lens _maInsufficientDataActions (\ s a -> s{_maInsufficientDataActions = a}) . _Default . _Coerce

-- | An explanation for the alarm state, in text format.
maStateReason :: Lens' MetricAlarm (Maybe Text)
maStateReason = lens _maStateReason (\ s a -> s{_maStateReason = a})

-- | An explanation for the alarm state, in JSON format.
maStateReasonData :: Lens' MetricAlarm (Maybe Text)
maStateReasonData = lens _maStateReasonData (\ s a -> s{_maStateReasonData = a})

-- | The dimensions for the metric associated with the alarm.
maDimensions :: Lens' MetricAlarm [Dimension]
maDimensions = lens _maDimensions (\ s a -> s{_maDimensions = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the alarm.
maAlarmARN :: Lens' MetricAlarm (Maybe Text)
maAlarmARN = lens _maAlarmARN (\ s a -> s{_maAlarmARN = a})

-- | The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
maAlarmActions :: Lens' MetricAlarm [Text]
maAlarmActions = lens _maAlarmActions (\ s a -> s{_maAlarmActions = a}) . _Default . _Coerce

-- | The unit of the metric associated with the alarm.
maUnit :: Lens' MetricAlarm (Maybe StandardUnit)
maUnit = lens _maUnit (\ s a -> s{_maUnit = a})

-- | The statistic for the metric associated with the alarm, other than percentile. For percentile statistics, use @ExtendedStatistic@ .
maStatistic :: Lens' MetricAlarm (Maybe Statistic)
maStatistic = lens _maStatistic (\ s a -> s{_maStatistic = a})

-- | The percentile statistic for the metric associated with the alarm. Specify a value between p0.0 and p100.
maExtendedStatistic :: Lens' MetricAlarm (Maybe Text)
maExtendedStatistic = lens _maExtendedStatistic (\ s a -> s{_maExtendedStatistic = a})

instance FromXML MetricAlarm where
        parseXML x
          = MetricAlarm' <$>
              (x .@? "AlarmName") <*>
                (x .@? "StateUpdatedTimestamp")
                <*>
                (x .@? "Metrics" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "TreatMissingData")
                <*> (x .@? "Period")
                <*> (x .@? "AlarmDescription")
                <*> (x .@? "EvaluationPeriods")
                <*> (x .@? "MetricName")
                <*> (x .@? "Namespace")
                <*> (x .@? "ThresholdMetricId")
                <*> (x .@? "ComparisonOperator")
                <*>
                (x .@? "OKActions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "EvaluateLowSampleCountPercentile")
                <*> (x .@? "StateValue")
                <*> (x .@? "DatapointsToAlarm")
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

-- | This structure is used in both @GetMetricData@ and @PutMetricAlarm@ . The supported use of this structure is different for those two operations.
--
--
-- When used in @GetMetricData@ , it indicates the metric data to return, and whether this call is just retrieving a batch set of data for one metric, or is performing a math expression on metric data. A single @GetMetricData@ call can include up to 100 @MetricDataQuery@ structures.
--
-- When used in @PutMetricAlarm@ , it enables you to create an alarm based on a metric math expression. Each @MetricDataQuery@ in the array specifies either a metric to retrieve, or a math expression to be performed on retrieved metrics. A single @PutMetricAlarm@ call can include up to 20 @MetricDataQuery@ structures in the array. The 20 structures can include as many as 10 structures that contain a @MetricStat@ parameter to retrieve a metric, and as many as 10 structures that contain the @Expression@ parameter to perform a math expression. Of those @Expression@ structures, one must have @True@ as the value for @ReturnData@ . The result of this expression is the value the alarm watches.
--
-- Any expression used in a @PutMetricAlarm@ operation must return a single time series. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/using-metric-math.html#metric-math-syntax Metric Math Syntax and Functions> in the /Amazon CloudWatch User Guide/ .
--
-- Some of the parameters of this structure also have different uses whether you are using this structure in a @GetMetricData@ operation or a @PutMetricAlarm@ operation. These differences are explained in the following parameter list.
--
--
-- /See:/ 'metricDataQuery' smart constructor.
data MetricDataQuery = MetricDataQuery'
  { _mdqReturnData :: !(Maybe Bool)
  , _mdqExpression :: !(Maybe Text)
  , _mdqLabel      :: !(Maybe Text)
  , _mdqMetricStat :: !(Maybe MetricStat)
  , _mdqId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricDataQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdqReturnData' - When used in @GetMetricData@ , this option indicates whether to return the timestamps and raw data values of this metric. If you are performing this call just to do math expressions and do not also need the raw data returned, you can specify @False@ . If you omit this, the default of @True@ is used. When used in @PutMetricAlarm@ , specify @True@ for the one expression result to use as the alarm. For all other metrics and expressions in the same @PutMetricAlarm@ operation, specify @ReturnData@ as False.
--
-- * 'mdqExpression' - The math expression to be performed on the returned data, if this object is performing a math expression. This expression can use the @Id@ of the other metrics to refer to those metrics, and can also use the @Id@ of other expressions to use the result of those expressions. For more information about metric math expressions, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/using-metric-math.html#metric-math-syntax Metric Math Syntax and Functions> in the /Amazon CloudWatch User Guide/ . Within each MetricDataQuery object, you must specify either @Expression@ or @MetricStat@ but not both.
--
-- * 'mdqLabel' - A human-readable label for this metric or expression. This is especially useful if this is an expression, so that you know what the value represents. If the metric or expression is shown in a CloudWatch dashboard widget, the label is shown. If Label is omitted, CloudWatch generates a default.
--
-- * 'mdqMetricStat' - The metric to be returned, along with statistics, period, and units. Use this parameter only if this object is retrieving a metric and not performing a math expression on returned data. Within one MetricDataQuery object, you must specify either @Expression@ or @MetricStat@ but not both.
--
-- * 'mdqId' - A short name used to tie this object to the results in the response. This name must be unique within a single call to @GetMetricData@ . If you are performing math expressions on this set of data, this name represents that data and can serve as a variable in the mathematical expression. The valid characters are letters, numbers, and underscore. The first character must be a lowercase letter.
metricDataQuery
    :: Text -- ^ 'mdqId'
    -> MetricDataQuery
metricDataQuery pId_ =
  MetricDataQuery'
    { _mdqReturnData = Nothing
    , _mdqExpression = Nothing
    , _mdqLabel = Nothing
    , _mdqMetricStat = Nothing
    , _mdqId = pId_
    }


-- | When used in @GetMetricData@ , this option indicates whether to return the timestamps and raw data values of this metric. If you are performing this call just to do math expressions and do not also need the raw data returned, you can specify @False@ . If you omit this, the default of @True@ is used. When used in @PutMetricAlarm@ , specify @True@ for the one expression result to use as the alarm. For all other metrics and expressions in the same @PutMetricAlarm@ operation, specify @ReturnData@ as False.
mdqReturnData :: Lens' MetricDataQuery (Maybe Bool)
mdqReturnData = lens _mdqReturnData (\ s a -> s{_mdqReturnData = a})

-- | The math expression to be performed on the returned data, if this object is performing a math expression. This expression can use the @Id@ of the other metrics to refer to those metrics, and can also use the @Id@ of other expressions to use the result of those expressions. For more information about metric math expressions, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/using-metric-math.html#metric-math-syntax Metric Math Syntax and Functions> in the /Amazon CloudWatch User Guide/ . Within each MetricDataQuery object, you must specify either @Expression@ or @MetricStat@ but not both.
mdqExpression :: Lens' MetricDataQuery (Maybe Text)
mdqExpression = lens _mdqExpression (\ s a -> s{_mdqExpression = a})

-- | A human-readable label for this metric or expression. This is especially useful if this is an expression, so that you know what the value represents. If the metric or expression is shown in a CloudWatch dashboard widget, the label is shown. If Label is omitted, CloudWatch generates a default.
mdqLabel :: Lens' MetricDataQuery (Maybe Text)
mdqLabel = lens _mdqLabel (\ s a -> s{_mdqLabel = a})

-- | The metric to be returned, along with statistics, period, and units. Use this parameter only if this object is retrieving a metric and not performing a math expression on returned data. Within one MetricDataQuery object, you must specify either @Expression@ or @MetricStat@ but not both.
mdqMetricStat :: Lens' MetricDataQuery (Maybe MetricStat)
mdqMetricStat = lens _mdqMetricStat (\ s a -> s{_mdqMetricStat = a})

-- | A short name used to tie this object to the results in the response. This name must be unique within a single call to @GetMetricData@ . If you are performing math expressions on this set of data, this name represents that data and can serve as a variable in the mathematical expression. The valid characters are letters, numbers, and underscore. The first character must be a lowercase letter.
mdqId :: Lens' MetricDataQuery Text
mdqId = lens _mdqId (\ s a -> s{_mdqId = a})

instance FromXML MetricDataQuery where
        parseXML x
          = MetricDataQuery' <$>
              (x .@? "ReturnData") <*> (x .@? "Expression") <*>
                (x .@? "Label")
                <*> (x .@? "MetricStat")
                <*> (x .@ "Id")

instance Hashable MetricDataQuery where

instance NFData MetricDataQuery where

instance ToQuery MetricDataQuery where
        toQuery MetricDataQuery'{..}
          = mconcat
              ["ReturnData" =: _mdqReturnData,
               "Expression" =: _mdqExpression, "Label" =: _mdqLabel,
               "MetricStat" =: _mdqMetricStat, "Id" =: _mdqId]

-- | A @GetMetricData@ call returns an array of @MetricDataResult@ structures. Each of these structures includes the data points for that metric, along with the timestamps of those data points and other identifying information.
--
--
--
-- /See:/ 'metricDataResult' smart constructor.
data MetricDataResult = MetricDataResult'
  { _mdrValues     :: !(Maybe [Double])
  , _mdrId         :: !(Maybe Text)
  , _mdrTimestamps :: !(Maybe [ISO8601])
  , _mdrMessages   :: !(Maybe [MessageData])
  , _mdrLabel      :: !(Maybe Text)
  , _mdrStatusCode :: !(Maybe StatusCode)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricDataResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdrValues' - The data points for the metric corresponding to @Timestamps@ . The number of values always matches the number of timestamps and the timestamp for Values[x] is Timestamps[x].
--
-- * 'mdrId' - The short name you specified to represent this metric.
--
-- * 'mdrTimestamps' - The timestamps for the data points, formatted in Unix timestamp format. The number of timestamps always matches the number of values and the value for Timestamps[x] is Values[x].
--
-- * 'mdrMessages' - A list of messages with additional information about the data returned.
--
-- * 'mdrLabel' - The human-readable label associated with the data.
--
-- * 'mdrStatusCode' - The status of the returned data. @Complete@ indicates that all data points in the requested time range were returned. @PartialData@ means that an incomplete set of data points were returned. You can use the @NextToken@ value that was returned and repeat your request to get more data points. @NextToken@ is not returned if you are performing a math expression. @InternalError@ indicates that an error occurred. Retry your request using @NextToken@ , if present.
metricDataResult
    :: MetricDataResult
metricDataResult =
  MetricDataResult'
    { _mdrValues = Nothing
    , _mdrId = Nothing
    , _mdrTimestamps = Nothing
    , _mdrMessages = Nothing
    , _mdrLabel = Nothing
    , _mdrStatusCode = Nothing
    }


-- | The data points for the metric corresponding to @Timestamps@ . The number of values always matches the number of timestamps and the timestamp for Values[x] is Timestamps[x].
mdrValues :: Lens' MetricDataResult [Double]
mdrValues = lens _mdrValues (\ s a -> s{_mdrValues = a}) . _Default . _Coerce

-- | The short name you specified to represent this metric.
mdrId :: Lens' MetricDataResult (Maybe Text)
mdrId = lens _mdrId (\ s a -> s{_mdrId = a})

-- | The timestamps for the data points, formatted in Unix timestamp format. The number of timestamps always matches the number of values and the value for Timestamps[x] is Values[x].
mdrTimestamps :: Lens' MetricDataResult [UTCTime]
mdrTimestamps = lens _mdrTimestamps (\ s a -> s{_mdrTimestamps = a}) . _Default . _Coerce

-- | A list of messages with additional information about the data returned.
mdrMessages :: Lens' MetricDataResult [MessageData]
mdrMessages = lens _mdrMessages (\ s a -> s{_mdrMessages = a}) . _Default . _Coerce

-- | The human-readable label associated with the data.
mdrLabel :: Lens' MetricDataResult (Maybe Text)
mdrLabel = lens _mdrLabel (\ s a -> s{_mdrLabel = a})

-- | The status of the returned data. @Complete@ indicates that all data points in the requested time range were returned. @PartialData@ means that an incomplete set of data points were returned. You can use the @NextToken@ value that was returned and repeat your request to get more data points. @NextToken@ is not returned if you are performing a math expression. @InternalError@ indicates that an error occurred. Retry your request using @NextToken@ , if present.
mdrStatusCode :: Lens' MetricDataResult (Maybe StatusCode)
mdrStatusCode = lens _mdrStatusCode (\ s a -> s{_mdrStatusCode = a})

instance FromXML MetricDataResult where
        parseXML x
          = MetricDataResult' <$>
              (x .@? "Values" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "Id")
                <*>
                (x .@? "Timestamps" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "Messages" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "Label")
                <*> (x .@? "StatusCode")

instance Hashable MetricDataResult where

instance NFData MetricDataResult where

-- | Encapsulates the information sent to either create a metric or add new values to be aggregated into an existing metric.
--
--
--
-- /See:/ 'metricDatum' smart constructor.
data MetricDatum = MetricDatum'
  { _mdValues            :: !(Maybe [Double])
  , _mdCounts            :: !(Maybe [Double])
  , _mdValue             :: !(Maybe Double)
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
-- * 'mdValues' - Array of numbers representing the values for the metric during the period. Each unique value is listed just once in this array, and the corresponding number in the @Counts@ array specifies the number of times that value occurred during the period. You can include up to 150 unique values in each @PutMetricData@ action that specifies a @Values@ array. Although the @Values@ array accepts numbers of type @Double@ , CloudWatch rejects values that are either too small or too large. Values must be in the range of 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2). In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.
--
-- * 'mdCounts' - Array of numbers that is used along with the @Values@ array. Each number in the @Count@ array is the number of times the corresponding value in the @Values@ array occurred during the period.  If you omit the @Counts@ array, the default of 1 is used as the value for each count. If you include a @Counts@ array, it must include the same amount of values as the @Values@ array.
--
-- * 'mdValue' - The value for the metric. Although the parameter accepts numbers of type Double, CloudWatch rejects values that are either too small or too large. Values must be in the range of 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2). In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.
--
-- * 'mdStorageResolution' - Valid values are 1 and 60. Setting this to 1 specifies this metric as a high-resolution metric, so that CloudWatch stores the metric with sub-minute resolution down to one second. Setting this to 60 specifies this metric as a regular-resolution metric, which CloudWatch stores at 1-minute resolution. Currently, high resolution is available only for custom metrics. For more information about high-resolution metrics, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html#high-resolution-metrics High-Resolution Metrics> in the /Amazon CloudWatch User Guide/ .  This field is optional, if you do not specify it the default of 60 is used.
--
-- * 'mdDimensions' - The dimensions associated with the metric.
--
-- * 'mdUnit' - When you are using a @Put@ operation, this defines what unit you want to use when storing the metric. In a @Get@ operation, this displays the unit that is used for the metric.
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
    { _mdValues = Nothing
    , _mdCounts = Nothing
    , _mdValue = Nothing
    , _mdStorageResolution = Nothing
    , _mdDimensions = Nothing
    , _mdUnit = Nothing
    , _mdTimestamp = Nothing
    , _mdStatisticValues = Nothing
    , _mdMetricName = pMetricName_
    }


-- | Array of numbers representing the values for the metric during the period. Each unique value is listed just once in this array, and the corresponding number in the @Counts@ array specifies the number of times that value occurred during the period. You can include up to 150 unique values in each @PutMetricData@ action that specifies a @Values@ array. Although the @Values@ array accepts numbers of type @Double@ , CloudWatch rejects values that are either too small or too large. Values must be in the range of 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2). In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.
mdValues :: Lens' MetricDatum [Double]
mdValues = lens _mdValues (\ s a -> s{_mdValues = a}) . _Default . _Coerce

-- | Array of numbers that is used along with the @Values@ array. Each number in the @Count@ array is the number of times the corresponding value in the @Values@ array occurred during the period.  If you omit the @Counts@ array, the default of 1 is used as the value for each count. If you include a @Counts@ array, it must include the same amount of values as the @Values@ array.
mdCounts :: Lens' MetricDatum [Double]
mdCounts = lens _mdCounts (\ s a -> s{_mdCounts = a}) . _Default . _Coerce

-- | The value for the metric. Although the parameter accepts numbers of type Double, CloudWatch rejects values that are either too small or too large. Values must be in the range of 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2). In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.
mdValue :: Lens' MetricDatum (Maybe Double)
mdValue = lens _mdValue (\ s a -> s{_mdValue = a})

-- | Valid values are 1 and 60. Setting this to 1 specifies this metric as a high-resolution metric, so that CloudWatch stores the metric with sub-minute resolution down to one second. Setting this to 60 specifies this metric as a regular-resolution metric, which CloudWatch stores at 1-minute resolution. Currently, high resolution is available only for custom metrics. For more information about high-resolution metrics, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html#high-resolution-metrics High-Resolution Metrics> in the /Amazon CloudWatch User Guide/ .  This field is optional, if you do not specify it the default of 60 is used.
mdStorageResolution :: Lens' MetricDatum (Maybe Natural)
mdStorageResolution = lens _mdStorageResolution (\ s a -> s{_mdStorageResolution = a}) . mapping _Nat

-- | The dimensions associated with the metric.
mdDimensions :: Lens' MetricDatum [Dimension]
mdDimensions = lens _mdDimensions (\ s a -> s{_mdDimensions = a}) . _Default . _Coerce

-- | When you are using a @Put@ operation, this defines what unit you want to use when storing the metric. In a @Get@ operation, this displays the unit that is used for the metric.
mdUnit :: Lens' MetricDatum (Maybe StandardUnit)
mdUnit = lens _mdUnit (\ s a -> s{_mdUnit = a})

-- | The time the metric data was received, expressed as the number of milliseconds since Jan 1, 1970 00:00:00 UTC.
mdTimestamp :: Lens' MetricDatum (Maybe UTCTime)
mdTimestamp = lens _mdTimestamp (\ s a -> s{_mdTimestamp = a}) . mapping _Time

-- | The statistical values for the metric.
mdStatisticValues :: Lens' MetricDatum (Maybe StatisticSet)
mdStatisticValues = lens _mdStatisticValues (\ s a -> s{_mdStatisticValues = a})

-- | The name of the metric.
mdMetricName :: Lens' MetricDatum Text
mdMetricName = lens _mdMetricName (\ s a -> s{_mdMetricName = a})

instance Hashable MetricDatum where

instance NFData MetricDatum where

instance ToQuery MetricDatum where
        toQuery MetricDatum'{..}
          = mconcat
              ["Values" =:
                 toQuery (toQueryList "member" <$> _mdValues),
               "Counts" =:
                 toQuery (toQueryList "member" <$> _mdCounts),
               "Value" =: _mdValue,
               "StorageResolution" =: _mdStorageResolution,
               "Dimensions" =:
                 toQuery (toQueryList "member" <$> _mdDimensions),
               "Unit" =: _mdUnit, "Timestamp" =: _mdTimestamp,
               "StatisticValues" =: _mdStatisticValues,
               "MetricName" =: _mdMetricName]

-- | This structure defines the metric to be returned, along with the statistics, period, and units.
--
--
--
-- /See:/ 'metricStat' smart constructor.
data MetricStat = MetricStat'
  { _msUnit   :: !(Maybe StandardUnit)
  , _msMetric :: !Metric
  , _msPeriod :: !Nat
  , _msStat   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricStat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msUnit' - When you are using a @Put@ operation, this defines what unit you want to use when storing the metric. In a @Get@ operation, if you omit @Unit@ then all data that was collected with any unit is returned, along with the corresponding units that were specified when the data was reported to CloudWatch. If you specify a unit, the operation returns only data data that was collected with that unit specified. If you specify a unit that does not match the data collected, the results of the operation are null. CloudWatch does not perform unit conversions.
--
-- * 'msMetric' - The metric to return, including the metric name, namespace, and dimensions.
--
-- * 'msPeriod' - The period, in seconds, to use when retrieving the metric.
--
-- * 'msStat' - The statistic to return. It can include any CloudWatch statistic or extended statistic.
metricStat
    :: Metric -- ^ 'msMetric'
    -> Natural -- ^ 'msPeriod'
    -> Text -- ^ 'msStat'
    -> MetricStat
metricStat pMetric_ pPeriod_ pStat_ =
  MetricStat'
    { _msUnit = Nothing
    , _msMetric = pMetric_
    , _msPeriod = _Nat # pPeriod_
    , _msStat = pStat_
    }


-- | When you are using a @Put@ operation, this defines what unit you want to use when storing the metric. In a @Get@ operation, if you omit @Unit@ then all data that was collected with any unit is returned, along with the corresponding units that were specified when the data was reported to CloudWatch. If you specify a unit, the operation returns only data data that was collected with that unit specified. If you specify a unit that does not match the data collected, the results of the operation are null. CloudWatch does not perform unit conversions.
msUnit :: Lens' MetricStat (Maybe StandardUnit)
msUnit = lens _msUnit (\ s a -> s{_msUnit = a})

-- | The metric to return, including the metric name, namespace, and dimensions.
msMetric :: Lens' MetricStat Metric
msMetric = lens _msMetric (\ s a -> s{_msMetric = a})

-- | The period, in seconds, to use when retrieving the metric.
msPeriod :: Lens' MetricStat Natural
msPeriod = lens _msPeriod (\ s a -> s{_msPeriod = a}) . _Nat

-- | The statistic to return. It can include any CloudWatch statistic or extended statistic.
msStat :: Lens' MetricStat Text
msStat = lens _msStat (\ s a -> s{_msStat = a})

instance FromXML MetricStat where
        parseXML x
          = MetricStat' <$>
              (x .@? "Unit") <*> (x .@ "Metric") <*>
                (x .@ "Period")
                <*> (x .@ "Stat")

instance Hashable MetricStat where

instance NFData MetricStat where

instance ToQuery MetricStat where
        toQuery MetricStat'{..}
          = mconcat
              ["Unit" =: _msUnit, "Metric" =: _msMetric,
               "Period" =: _msPeriod, "Stat" =: _msStat]

-- | Specifies one range of days or times to exclude from use for training an anomaly detection model.
--
--
--
-- /See:/ 'range' smart constructor.
data Range = Range'
  { _rStartTime :: !ISO8601
  , _rEndTime   :: !ISO8601
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Range' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rStartTime' - The start time of the range to exclude. The format is @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
--
-- * 'rEndTime' - The end time of the range to exclude. The format is @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
range
    :: UTCTime -- ^ 'rStartTime'
    -> UTCTime -- ^ 'rEndTime'
    -> Range
range pStartTime_ pEndTime_ =
  Range' {_rStartTime = _Time # pStartTime_, _rEndTime = _Time # pEndTime_}


-- | The start time of the range to exclude. The format is @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
rStartTime :: Lens' Range UTCTime
rStartTime = lens _rStartTime (\ s a -> s{_rStartTime = a}) . _Time

-- | The end time of the range to exclude. The format is @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
rEndTime :: Lens' Range UTCTime
rEndTime = lens _rEndTime (\ s a -> s{_rEndTime = a}) . _Time

instance FromXML Range where
        parseXML x
          = Range' <$> (x .@ "StartTime") <*> (x .@ "EndTime")

instance Hashable Range where

instance NFData Range where

instance ToQuery Range where
        toQuery Range'{..}
          = mconcat
              ["StartTime" =: _rStartTime, "EndTime" =: _rEndTime]

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
ssSampleCount = lens _ssSampleCount (\ s a -> s{_ssSampleCount = a})

-- | The sum of values for the sample set.
ssSum :: Lens' StatisticSet Double
ssSum = lens _ssSum (\ s a -> s{_ssSum = a})

-- | The minimum value of the sample set.
ssMinimum :: Lens' StatisticSet Double
ssMinimum = lens _ssMinimum (\ s a -> s{_ssMinimum = a})

-- | The maximum value of the sample set.
ssMaximum :: Lens' StatisticSet Double
ssMaximum = lens _ssMaximum (\ s a -> s{_ssMaximum = a})

instance Hashable StatisticSet where

instance NFData StatisticSet where

instance ToQuery StatisticSet where
        toQuery StatisticSet'{..}
          = mconcat
              ["SampleCount" =: _ssSampleCount, "Sum" =: _ssSum,
               "Minimum" =: _ssMinimum, "Maximum" =: _ssMaximum]

-- | A key-value pair associated with a CloudWatch resource.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagKey   :: !Text
  , _tagValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - A string that you can use to assign a value. The combination of tag keys and values can help you organize and categorize your resources.
--
-- * 'tagValue' - The value for the specified tag key.
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | A string that you can use to assign a value. The combination of tag keys and values can help you organize and categorize your resources.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | The value for the specified tag key.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

instance FromXML Tag where
        parseXML x = Tag' <$> (x .@ "Key") <*> (x .@ "Value")

instance Hashable Tag where

instance NFData Tag where

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Key" =: _tagKey, "Value" =: _tagValue]
