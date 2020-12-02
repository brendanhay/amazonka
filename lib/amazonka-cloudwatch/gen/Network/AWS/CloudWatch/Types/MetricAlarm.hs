{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.MetricAlarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.MetricAlarm where

import Network.AWS.CloudWatch.Types.ComparisonOperator
import Network.AWS.CloudWatch.Types.Dimension
import Network.AWS.CloudWatch.Types.MetricDataQuery
import Network.AWS.CloudWatch.Types.StandardUnit
import Network.AWS.CloudWatch.Types.StateValue
import Network.AWS.CloudWatch.Types.Statistic
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details about a metric alarm.
--
--
--
-- /See:/ 'metricAlarm' smart constructor.
data MetricAlarm = MetricAlarm'
  { _maAlarmName :: !(Maybe Text),
    _maStateUpdatedTimestamp :: !(Maybe ISO8601),
    _maMetrics :: !(Maybe [MetricDataQuery]),
    _maTreatMissingData :: !(Maybe Text),
    _maPeriod :: !(Maybe Nat),
    _maAlarmDescription :: !(Maybe Text),
    _maEvaluationPeriods :: !(Maybe Nat),
    _maMetricName :: !(Maybe Text),
    _maNamespace :: !(Maybe Text),
    _maThresholdMetricId :: !(Maybe Text),
    _maComparisonOperator :: !(Maybe ComparisonOperator),
    _maOKActions :: !(Maybe [Text]),
    _maEvaluateLowSampleCountPercentile :: !(Maybe Text),
    _maStateValue :: !(Maybe StateValue),
    _maDatapointsToAlarm :: !(Maybe Nat),
    _maThreshold :: !(Maybe Double),
    _maAlarmConfigurationUpdatedTimestamp :: !(Maybe ISO8601),
    _maActionsEnabled :: !(Maybe Bool),
    _maInsufficientDataActions :: !(Maybe [Text]),
    _maStateReason :: !(Maybe Text),
    _maStateReasonData :: !(Maybe Text),
    _maDimensions :: !(Maybe [Dimension]),
    _maAlarmARN :: !(Maybe Text),
    _maAlarmActions :: !(Maybe [Text]),
    _maUnit :: !(Maybe StandardUnit),
    _maStatistic :: !(Maybe Statistic),
    _maExtendedStatistic :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricAlarm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'maAlarmName' - The name of the alarm.
--
-- * 'maStateUpdatedTimestamp' - The time stamp of the last update to the alarm state.
--
-- * 'maMetrics' - An array of MetricDataQuery structures, used in an alarm based on a metric math expression. Each structure either retrieves a metric or performs a math expression. One item in the Metrics array is the math expression that the alarm watches. This expression by designated by having @ReturnData@ set to true.
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
-- * 'maDatapointsToAlarm' - The number of data points that must be breaching to trigger the alarm.
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
metricAlarm ::
  MetricAlarm
metricAlarm =
  MetricAlarm'
    { _maAlarmName = Nothing,
      _maStateUpdatedTimestamp = Nothing,
      _maMetrics = Nothing,
      _maTreatMissingData = Nothing,
      _maPeriod = Nothing,
      _maAlarmDescription = Nothing,
      _maEvaluationPeriods = Nothing,
      _maMetricName = Nothing,
      _maNamespace = Nothing,
      _maThresholdMetricId = Nothing,
      _maComparisonOperator = Nothing,
      _maOKActions = Nothing,
      _maEvaluateLowSampleCountPercentile = Nothing,
      _maStateValue = Nothing,
      _maDatapointsToAlarm = Nothing,
      _maThreshold = Nothing,
      _maAlarmConfigurationUpdatedTimestamp = Nothing,
      _maActionsEnabled = Nothing,
      _maInsufficientDataActions = Nothing,
      _maStateReason = Nothing,
      _maStateReasonData = Nothing,
      _maDimensions = Nothing,
      _maAlarmARN = Nothing,
      _maAlarmActions = Nothing,
      _maUnit = Nothing,
      _maStatistic = Nothing,
      _maExtendedStatistic = Nothing
    }

-- | The name of the alarm.
maAlarmName :: Lens' MetricAlarm (Maybe Text)
maAlarmName = lens _maAlarmName (\s a -> s {_maAlarmName = a})

-- | The time stamp of the last update to the alarm state.
maStateUpdatedTimestamp :: Lens' MetricAlarm (Maybe UTCTime)
maStateUpdatedTimestamp = lens _maStateUpdatedTimestamp (\s a -> s {_maStateUpdatedTimestamp = a}) . mapping _Time

-- | An array of MetricDataQuery structures, used in an alarm based on a metric math expression. Each structure either retrieves a metric or performs a math expression. One item in the Metrics array is the math expression that the alarm watches. This expression by designated by having @ReturnData@ set to true.
maMetrics :: Lens' MetricAlarm [MetricDataQuery]
maMetrics = lens _maMetrics (\s a -> s {_maMetrics = a}) . _Default . _Coerce

-- | Sets how this alarm is to handle missing data points. If this parameter is omitted, the default behavior of @missing@ is used.
maTreatMissingData :: Lens' MetricAlarm (Maybe Text)
maTreatMissingData = lens _maTreatMissingData (\s a -> s {_maTreatMissingData = a})

-- | The period, in seconds, over which the statistic is applied.
maPeriod :: Lens' MetricAlarm (Maybe Natural)
maPeriod = lens _maPeriod (\s a -> s {_maPeriod = a}) . mapping _Nat

-- | The description of the alarm.
maAlarmDescription :: Lens' MetricAlarm (Maybe Text)
maAlarmDescription = lens _maAlarmDescription (\s a -> s {_maAlarmDescription = a})

-- | The number of periods over which data is compared to the specified threshold.
maEvaluationPeriods :: Lens' MetricAlarm (Maybe Natural)
maEvaluationPeriods = lens _maEvaluationPeriods (\s a -> s {_maEvaluationPeriods = a}) . mapping _Nat

-- | The name of the metric associated with the alarm, if this is an alarm based on a single metric.
maMetricName :: Lens' MetricAlarm (Maybe Text)
maMetricName = lens _maMetricName (\s a -> s {_maMetricName = a})

-- | The namespace of the metric associated with the alarm.
maNamespace :: Lens' MetricAlarm (Maybe Text)
maNamespace = lens _maNamespace (\s a -> s {_maNamespace = a})

-- | In an alarm based on an anomaly detection model, this is the ID of the @ANOMALY_DETECTION_BAND@ function used as the threshold for the alarm.
maThresholdMetricId :: Lens' MetricAlarm (Maybe Text)
maThresholdMetricId = lens _maThresholdMetricId (\s a -> s {_maThresholdMetricId = a})

-- | The arithmetic operation to use when comparing the specified statistic and threshold. The specified statistic value is used as the first operand.
maComparisonOperator :: Lens' MetricAlarm (Maybe ComparisonOperator)
maComparisonOperator = lens _maComparisonOperator (\s a -> s {_maComparisonOperator = a})

-- | The actions to execute when this alarm transitions to the @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
maOKActions :: Lens' MetricAlarm [Text]
maOKActions = lens _maOKActions (\s a -> s {_maOKActions = a}) . _Default . _Coerce

-- | Used only for alarms based on percentiles. If @ignore@ , the alarm state does not change during periods with too few data points to be statistically significant. If @evaluate@ or this parameter is not used, the alarm is always evaluated and possibly changes state no matter how many data points are available.
maEvaluateLowSampleCountPercentile :: Lens' MetricAlarm (Maybe Text)
maEvaluateLowSampleCountPercentile = lens _maEvaluateLowSampleCountPercentile (\s a -> s {_maEvaluateLowSampleCountPercentile = a})

-- | The state value for the alarm.
maStateValue :: Lens' MetricAlarm (Maybe StateValue)
maStateValue = lens _maStateValue (\s a -> s {_maStateValue = a})

-- | The number of data points that must be breaching to trigger the alarm.
maDatapointsToAlarm :: Lens' MetricAlarm (Maybe Natural)
maDatapointsToAlarm = lens _maDatapointsToAlarm (\s a -> s {_maDatapointsToAlarm = a}) . mapping _Nat

-- | The value to compare with the specified statistic.
maThreshold :: Lens' MetricAlarm (Maybe Double)
maThreshold = lens _maThreshold (\s a -> s {_maThreshold = a})

-- | The time stamp of the last update to the alarm configuration.
maAlarmConfigurationUpdatedTimestamp :: Lens' MetricAlarm (Maybe UTCTime)
maAlarmConfigurationUpdatedTimestamp = lens _maAlarmConfigurationUpdatedTimestamp (\s a -> s {_maAlarmConfigurationUpdatedTimestamp = a}) . mapping _Time

-- | Indicates whether actions should be executed during any changes to the alarm state.
maActionsEnabled :: Lens' MetricAlarm (Maybe Bool)
maActionsEnabled = lens _maActionsEnabled (\s a -> s {_maActionsEnabled = a})

-- | The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
maInsufficientDataActions :: Lens' MetricAlarm [Text]
maInsufficientDataActions = lens _maInsufficientDataActions (\s a -> s {_maInsufficientDataActions = a}) . _Default . _Coerce

-- | An explanation for the alarm state, in text format.
maStateReason :: Lens' MetricAlarm (Maybe Text)
maStateReason = lens _maStateReason (\s a -> s {_maStateReason = a})

-- | An explanation for the alarm state, in JSON format.
maStateReasonData :: Lens' MetricAlarm (Maybe Text)
maStateReasonData = lens _maStateReasonData (\s a -> s {_maStateReasonData = a})

-- | The dimensions for the metric associated with the alarm.
maDimensions :: Lens' MetricAlarm [Dimension]
maDimensions = lens _maDimensions (\s a -> s {_maDimensions = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the alarm.
maAlarmARN :: Lens' MetricAlarm (Maybe Text)
maAlarmARN = lens _maAlarmARN (\s a -> s {_maAlarmARN = a})

-- | The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
maAlarmActions :: Lens' MetricAlarm [Text]
maAlarmActions = lens _maAlarmActions (\s a -> s {_maAlarmActions = a}) . _Default . _Coerce

-- | The unit of the metric associated with the alarm.
maUnit :: Lens' MetricAlarm (Maybe StandardUnit)
maUnit = lens _maUnit (\s a -> s {_maUnit = a})

-- | The statistic for the metric associated with the alarm, other than percentile. For percentile statistics, use @ExtendedStatistic@ .
maStatistic :: Lens' MetricAlarm (Maybe Statistic)
maStatistic = lens _maStatistic (\s a -> s {_maStatistic = a})

-- | The percentile statistic for the metric associated with the alarm. Specify a value between p0.0 and p100.
maExtendedStatistic :: Lens' MetricAlarm (Maybe Text)
maExtendedStatistic = lens _maExtendedStatistic (\s a -> s {_maExtendedStatistic = a})

instance FromXML MetricAlarm where
  parseXML x =
    MetricAlarm'
      <$> (x .@? "AlarmName")
      <*> (x .@? "StateUpdatedTimestamp")
      <*> (x .@? "Metrics" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "TreatMissingData")
      <*> (x .@? "Period")
      <*> (x .@? "AlarmDescription")
      <*> (x .@? "EvaluationPeriods")
      <*> (x .@? "MetricName")
      <*> (x .@? "Namespace")
      <*> (x .@? "ThresholdMetricId")
      <*> (x .@? "ComparisonOperator")
      <*> (x .@? "OKActions" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "EvaluateLowSampleCountPercentile")
      <*> (x .@? "StateValue")
      <*> (x .@? "DatapointsToAlarm")
      <*> (x .@? "Threshold")
      <*> (x .@? "AlarmConfigurationUpdatedTimestamp")
      <*> (x .@? "ActionsEnabled")
      <*> ( x .@? "InsufficientDataActions" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "StateReason")
      <*> (x .@? "StateReasonData")
      <*> (x .@? "Dimensions" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "AlarmArn")
      <*> (x .@? "AlarmActions" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Unit")
      <*> (x .@? "Statistic")
      <*> (x .@? "ExtendedStatistic")

instance Hashable MetricAlarm

instance NFData MetricAlarm
