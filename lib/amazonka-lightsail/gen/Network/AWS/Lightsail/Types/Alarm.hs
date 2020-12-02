{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Alarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Alarm where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.AlarmState
import Network.AWS.Lightsail.Types.ComparisonOperator
import Network.AWS.Lightsail.Types.ContactProtocol
import Network.AWS.Lightsail.Types.MetricName
import Network.AWS.Lightsail.Types.MetricStatistic
import Network.AWS.Lightsail.Types.MetricUnit
import Network.AWS.Lightsail.Types.MonitoredResourceInfo
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.TreatMissingData
import Network.AWS.Prelude

-- | Describes an alarm.
--
--
-- An alarm is a way to monitor your Amazon Lightsail resource metrics. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail> .
--
--
-- /See:/ 'alarm' smart constructor.
data Alarm = Alarm'
  { _aState :: !(Maybe AlarmState),
    _aTreatMissingData :: !(Maybe TreatMissingData),
    _aResourceType :: !(Maybe ResourceType),
    _aArn :: !(Maybe Text),
    _aCreatedAt :: !(Maybe POSIX),
    _aLocation :: !(Maybe ResourceLocation),
    _aContactProtocols :: !(Maybe [ContactProtocol]),
    _aPeriod :: !(Maybe Nat),
    _aEvaluationPeriods :: !(Maybe Int),
    _aMetricName :: !(Maybe MetricName),
    _aComparisonOperator :: !(Maybe ComparisonOperator),
    _aName :: !(Maybe Text),
    _aThreshold :: !(Maybe Double),
    _aDatapointsToAlarm :: !(Maybe Int),
    _aSupportCode :: !(Maybe Text),
    _aNotificationEnabled :: !(Maybe Bool),
    _aNotificationTriggers :: !(Maybe [AlarmState]),
    _aStatistic :: !(Maybe MetricStatistic),
    _aUnit :: !(Maybe MetricUnit),
    _aMonitoredResourceInfo :: !(Maybe MonitoredResourceInfo)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Alarm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aState' - The current state of the alarm. An alarm has the following possible states:     * @ALARM@ - The metric is outside of the defined threshold.     * @INSUFFICIENT_DATA@ - The alarm has just started, the metric is not available, or not enough data is available for the metric to determine the alarm state.     * @OK@ - The metric is within the defined threshold.
--
-- * 'aTreatMissingData' - Specifies how the alarm handles missing data points. An alarm can treat missing data in the following ways:     * @breaching@ - Assume the missing data is not within the threshold. Missing data counts towards the number of times the metric is not within the threshold.     * @notBreaching@ - Assume the missing data is within the threshold. Missing data does not count towards the number of times the metric is not within the threshold.     * @ignore@ - Ignore the missing data. Maintains the current alarm state.     * @missing@ - Missing data is treated as missing.
--
-- * 'aResourceType' - The Lightsail resource type (e.g., @Alarm@ ).
--
-- * 'aArn' - The Amazon Resource Name (ARN) of the alarm.
--
-- * 'aCreatedAt' - The timestamp when the alarm was created.
--
-- * 'aLocation' - An object that lists information about the location of the alarm.
--
-- * 'aContactProtocols' - The contact protocols for the alarm, such as @Email@ , @SMS@ (text messaging), or both.
--
-- * 'aPeriod' - The period, in seconds, over which the statistic is applied.
--
-- * 'aEvaluationPeriods' - The number of periods over which data is compared to the specified threshold.
--
-- * 'aMetricName' - The name of the metric associated with the alarm.
--
-- * 'aComparisonOperator' - The arithmetic operation used when comparing the specified statistic and threshold.
--
-- * 'aName' - The name of the alarm.
--
-- * 'aThreshold' - The value against which the specified statistic is compared.
--
-- * 'aDatapointsToAlarm' - The number of data points that must not within the specified threshold to trigger the alarm.
--
-- * 'aSupportCode' - The support code. Include this code in your email to support when you have questions about your Lightsail alarm. This code enables our support team to look up your Lightsail information more easily.
--
-- * 'aNotificationEnabled' - Indicates whether the alarm is enabled.
--
-- * 'aNotificationTriggers' - The alarm states that trigger a notification.
--
-- * 'aStatistic' - The statistic for the metric associated with the alarm. The following statistics are available:     * @Minimum@ - The lowest value observed during the specified period. Use this value to determine low volumes of activity for your application.     * @Maximum@ - The highest value observed during the specified period. Use this value to determine high volumes of activity for your application.     * @Sum@ - All values submitted for the matching metric added together. You can use this statistic to determine the total volume of a metric.     * @Average@ - The value of Sum / SampleCount during the specified period. By comparing this statistic with the Minimum and Maximum values, you can determine the full scope of a metric and how close the average use is to the Minimum and Maximum values. This comparison helps you to know when to increase or decrease your resources.     * @SampleCount@ - The count, or number, of data points used for the statistical calculation.
--
-- * 'aUnit' - The unit of the metric associated with the alarm.
--
-- * 'aMonitoredResourceInfo' - An object that lists information about the resource monitored by the alarm.
alarm ::
  Alarm
alarm =
  Alarm'
    { _aState = Nothing,
      _aTreatMissingData = Nothing,
      _aResourceType = Nothing,
      _aArn = Nothing,
      _aCreatedAt = Nothing,
      _aLocation = Nothing,
      _aContactProtocols = Nothing,
      _aPeriod = Nothing,
      _aEvaluationPeriods = Nothing,
      _aMetricName = Nothing,
      _aComparisonOperator = Nothing,
      _aName = Nothing,
      _aThreshold = Nothing,
      _aDatapointsToAlarm = Nothing,
      _aSupportCode = Nothing,
      _aNotificationEnabled = Nothing,
      _aNotificationTriggers = Nothing,
      _aStatistic = Nothing,
      _aUnit = Nothing,
      _aMonitoredResourceInfo = Nothing
    }

-- | The current state of the alarm. An alarm has the following possible states:     * @ALARM@ - The metric is outside of the defined threshold.     * @INSUFFICIENT_DATA@ - The alarm has just started, the metric is not available, or not enough data is available for the metric to determine the alarm state.     * @OK@ - The metric is within the defined threshold.
aState :: Lens' Alarm (Maybe AlarmState)
aState = lens _aState (\s a -> s {_aState = a})

-- | Specifies how the alarm handles missing data points. An alarm can treat missing data in the following ways:     * @breaching@ - Assume the missing data is not within the threshold. Missing data counts towards the number of times the metric is not within the threshold.     * @notBreaching@ - Assume the missing data is within the threshold. Missing data does not count towards the number of times the metric is not within the threshold.     * @ignore@ - Ignore the missing data. Maintains the current alarm state.     * @missing@ - Missing data is treated as missing.
aTreatMissingData :: Lens' Alarm (Maybe TreatMissingData)
aTreatMissingData = lens _aTreatMissingData (\s a -> s {_aTreatMissingData = a})

-- | The Lightsail resource type (e.g., @Alarm@ ).
aResourceType :: Lens' Alarm (Maybe ResourceType)
aResourceType = lens _aResourceType (\s a -> s {_aResourceType = a})

-- | The Amazon Resource Name (ARN) of the alarm.
aArn :: Lens' Alarm (Maybe Text)
aArn = lens _aArn (\s a -> s {_aArn = a})

-- | The timestamp when the alarm was created.
aCreatedAt :: Lens' Alarm (Maybe UTCTime)
aCreatedAt = lens _aCreatedAt (\s a -> s {_aCreatedAt = a}) . mapping _Time

-- | An object that lists information about the location of the alarm.
aLocation :: Lens' Alarm (Maybe ResourceLocation)
aLocation = lens _aLocation (\s a -> s {_aLocation = a})

-- | The contact protocols for the alarm, such as @Email@ , @SMS@ (text messaging), or both.
aContactProtocols :: Lens' Alarm [ContactProtocol]
aContactProtocols = lens _aContactProtocols (\s a -> s {_aContactProtocols = a}) . _Default . _Coerce

-- | The period, in seconds, over which the statistic is applied.
aPeriod :: Lens' Alarm (Maybe Natural)
aPeriod = lens _aPeriod (\s a -> s {_aPeriod = a}) . mapping _Nat

-- | The number of periods over which data is compared to the specified threshold.
aEvaluationPeriods :: Lens' Alarm (Maybe Int)
aEvaluationPeriods = lens _aEvaluationPeriods (\s a -> s {_aEvaluationPeriods = a})

-- | The name of the metric associated with the alarm.
aMetricName :: Lens' Alarm (Maybe MetricName)
aMetricName = lens _aMetricName (\s a -> s {_aMetricName = a})

-- | The arithmetic operation used when comparing the specified statistic and threshold.
aComparisonOperator :: Lens' Alarm (Maybe ComparisonOperator)
aComparisonOperator = lens _aComparisonOperator (\s a -> s {_aComparisonOperator = a})

-- | The name of the alarm.
aName :: Lens' Alarm (Maybe Text)
aName = lens _aName (\s a -> s {_aName = a})

-- | The value against which the specified statistic is compared.
aThreshold :: Lens' Alarm (Maybe Double)
aThreshold = lens _aThreshold (\s a -> s {_aThreshold = a})

-- | The number of data points that must not within the specified threshold to trigger the alarm.
aDatapointsToAlarm :: Lens' Alarm (Maybe Int)
aDatapointsToAlarm = lens _aDatapointsToAlarm (\s a -> s {_aDatapointsToAlarm = a})

-- | The support code. Include this code in your email to support when you have questions about your Lightsail alarm. This code enables our support team to look up your Lightsail information more easily.
aSupportCode :: Lens' Alarm (Maybe Text)
aSupportCode = lens _aSupportCode (\s a -> s {_aSupportCode = a})

-- | Indicates whether the alarm is enabled.
aNotificationEnabled :: Lens' Alarm (Maybe Bool)
aNotificationEnabled = lens _aNotificationEnabled (\s a -> s {_aNotificationEnabled = a})

-- | The alarm states that trigger a notification.
aNotificationTriggers :: Lens' Alarm [AlarmState]
aNotificationTriggers = lens _aNotificationTriggers (\s a -> s {_aNotificationTriggers = a}) . _Default . _Coerce

-- | The statistic for the metric associated with the alarm. The following statistics are available:     * @Minimum@ - The lowest value observed during the specified period. Use this value to determine low volumes of activity for your application.     * @Maximum@ - The highest value observed during the specified period. Use this value to determine high volumes of activity for your application.     * @Sum@ - All values submitted for the matching metric added together. You can use this statistic to determine the total volume of a metric.     * @Average@ - The value of Sum / SampleCount during the specified period. By comparing this statistic with the Minimum and Maximum values, you can determine the full scope of a metric and how close the average use is to the Minimum and Maximum values. This comparison helps you to know when to increase or decrease your resources.     * @SampleCount@ - The count, or number, of data points used for the statistical calculation.
aStatistic :: Lens' Alarm (Maybe MetricStatistic)
aStatistic = lens _aStatistic (\s a -> s {_aStatistic = a})

-- | The unit of the metric associated with the alarm.
aUnit :: Lens' Alarm (Maybe MetricUnit)
aUnit = lens _aUnit (\s a -> s {_aUnit = a})

-- | An object that lists information about the resource monitored by the alarm.
aMonitoredResourceInfo :: Lens' Alarm (Maybe MonitoredResourceInfo)
aMonitoredResourceInfo = lens _aMonitoredResourceInfo (\s a -> s {_aMonitoredResourceInfo = a})

instance FromJSON Alarm where
  parseJSON =
    withObject
      "Alarm"
      ( \x ->
          Alarm'
            <$> (x .:? "state")
            <*> (x .:? "treatMissingData")
            <*> (x .:? "resourceType")
            <*> (x .:? "arn")
            <*> (x .:? "createdAt")
            <*> (x .:? "location")
            <*> (x .:? "contactProtocols" .!= mempty)
            <*> (x .:? "period")
            <*> (x .:? "evaluationPeriods")
            <*> (x .:? "metricName")
            <*> (x .:? "comparisonOperator")
            <*> (x .:? "name")
            <*> (x .:? "threshold")
            <*> (x .:? "datapointsToAlarm")
            <*> (x .:? "supportCode")
            <*> (x .:? "notificationEnabled")
            <*> (x .:? "notificationTriggers" .!= mempty)
            <*> (x .:? "statistic")
            <*> (x .:? "unit")
            <*> (x .:? "monitoredResourceInfo")
      )

instance Hashable Alarm

instance NFData Alarm
