{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.PutAlarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an alarm, and associates it with the specified metric.
--
--
-- An alarm is used to monitor a single metric for one of your resources. When a metric condition is met, the alarm can notify you by email, SMS text message, and a banner displayed on the Amazon Lightsail console. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail> .
--
-- When this action creates an alarm, the alarm state is immediately set to @INSUFFICIENT_DATA@ . The alarm is then evaluated and its state is set appropriately. Any actions associated with the new state are then executed.
--
-- When you update an existing alarm, its state is left unchanged, but the update completely overwrites the previous configuration of the alarm. The alarm is then evaluated with the updated configuration.
module Network.AWS.Lightsail.PutAlarm
  ( -- * Creating a Request
    putAlarm,
    PutAlarm,

    -- * Request Lenses
    paTreatMissingData,
    paContactProtocols,
    paDatapointsToAlarm,
    paNotificationEnabled,
    paNotificationTriggers,
    paAlarmName,
    paMetricName,
    paMonitoredResourceName,
    paComparisonOperator,
    paThreshold,
    paEvaluationPeriods,

    -- * Destructuring the Response
    putAlarmResponse,
    PutAlarmResponse,

    -- * Response Lenses
    parsOperations,
    parsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putAlarm' smart constructor.
data PutAlarm = PutAlarm'
  { _paTreatMissingData ::
      !(Maybe TreatMissingData),
    _paContactProtocols :: !(Maybe [ContactProtocol]),
    _paDatapointsToAlarm :: !(Maybe Int),
    _paNotificationEnabled :: !(Maybe Bool),
    _paNotificationTriggers :: !(Maybe [AlarmState]),
    _paAlarmName :: !Text,
    _paMetricName :: !MetricName,
    _paMonitoredResourceName :: !Text,
    _paComparisonOperator :: !ComparisonOperator,
    _paThreshold :: !Double,
    _paEvaluationPeriods :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutAlarm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paTreatMissingData' - Sets how this alarm will handle missing data points. An alarm can treat missing data in the following ways:     * @breaching@ - Assume the missing data is not within the threshold. Missing data counts towards the number of times the metric is not within the threshold.     * @notBreaching@ - Assume the missing data is within the threshold. Missing data does not count towards the number of times the metric is not within the threshold.     * @ignore@ - Ignore the missing data. Maintains the current alarm state.     * @missing@ - Missing data is treated as missing. If @treatMissingData@ is not specified, the default behavior of @missing@ is used.
--
-- * 'paContactProtocols' - The contact protocols to use for the alarm, such as @Email@ , @SMS@ (text messaging), or both. A notification is sent via the specified contact protocol if notifications are enabled for the alarm, and when the alarm is triggered. A notification is not sent if a contact protocol is not specified, if the specified contact protocol is not configured in the AWS Region, or if notifications are not enabled for the alarm using the @notificationEnabled@ paramater. Use the @CreateContactMethod@ action to configure a contact protocol in an AWS Region.
--
-- * 'paDatapointsToAlarm' - The number of data points that must be not within the specified threshold to trigger the alarm. If you are setting an "M out of N" alarm, this value (@datapointsToAlarm@ ) is the M.
--
-- * 'paNotificationEnabled' - Indicates whether the alarm is enabled. Notifications are enabled by default if you don't specify this parameter.
--
-- * 'paNotificationTriggers' - The alarm states that trigger a notification. An alarm has the following possible states:     * @ALARM@ - The metric is outside of the defined threshold.     * @INSUFFICIENT_DATA@ - The alarm has just started, the metric is not available, or not enough data is available for the metric to determine the alarm state.     * @OK@ - The metric is within the defined threshold. When you specify a notification trigger, the @ALARM@ state must be specified. The @INSUFFICIENT_DATA@ and @OK@ states can be specified in addition to the @ALARM@ state.     * If you specify @OK@ as an alarm trigger, a notification is sent when the alarm switches from an @ALARM@ or @INSUFFICIENT_DATA@ alarm state to an @OK@ state. This can be thought of as an /all clear/ alarm notification.     * If you specify @INSUFFICIENT_DATA@ as the alarm trigger, a notification is sent when the alarm switches from an @OK@ or @ALARM@ alarm state to an @INSUFFICIENT_DATA@ state. The notification trigger defaults to @ALARM@ if you don't specify this parameter.
--
-- * 'paAlarmName' - The name for the alarm. Specify the name of an existing alarm to update, and overwrite the previous configuration of the alarm.
--
-- * 'paMetricName' - The name of the metric to associate with the alarm. You can configure up to two alarms per metric. The following metrics are available for each resource type:     * __Instances__ : @BurstCapacityPercentage@ , @BurstCapacityTime@ , @CPUUtilization@ , @NetworkIn@ , @NetworkOut@ , @StatusCheckFailed@ , @StatusCheckFailed_Instance@ , and @StatusCheckFailed_System@ .     * __Load balancers__ : @ClientTLSNegotiationErrorCount@ , @HealthyHostCount@ , @UnhealthyHostCount@ , @HTTPCode_LB_4XX_Count@ , @HTTPCode_LB_5XX_Count@ , @HTTPCode_Instance_2XX_Count@ , @HTTPCode_Instance_3XX_Count@ , @HTTPCode_Instance_4XX_Count@ , @HTTPCode_Instance_5XX_Count@ , @InstanceResponseTime@ , @RejectedConnectionCount@ , and @RequestCount@ .     * __Relational databases__ : @CPUUtilization@ , @DatabaseConnections@ , @DiskQueueDepth@ , @FreeStorageSpace@ , @NetworkReceiveThroughput@ , and @NetworkTransmitThroughput@ . For more information about these metrics, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-resource-health-metrics#available-metrics Metrics available in Lightsail> .
--
-- * 'paMonitoredResourceName' - The name of the Lightsail resource that will be monitored. Instances, load balancers, and relational databases are the only Lightsail resources that can currently be monitored by alarms.
--
-- * 'paComparisonOperator' - The arithmetic operation to use when comparing the specified statistic to the threshold. The specified statistic value is used as the first operand.
--
-- * 'paThreshold' - The value against which the specified statistic is compared.
--
-- * 'paEvaluationPeriods' - The number of most recent periods over which data is compared to the specified threshold. If you are setting an "M out of N" alarm, this value (@evaluationPeriods@ ) is the N. If you are setting an alarm that requires that a number of consecutive data points be breaching to trigger the alarm, this value specifies the rolling period of time in which data points are evaluated. Each evaluation period is five minutes long. For example, specify an evaluation period of 24 to evaluate a metric over a rolling period of two hours. You can specify a minimum valuation period of 1 (5 minutes), and a maximum evaluation period of 288 (24 hours).
putAlarm ::
  -- | 'paAlarmName'
  Text ->
  -- | 'paMetricName'
  MetricName ->
  -- | 'paMonitoredResourceName'
  Text ->
  -- | 'paComparisonOperator'
  ComparisonOperator ->
  -- | 'paThreshold'
  Double ->
  -- | 'paEvaluationPeriods'
  Int ->
  PutAlarm
putAlarm
  pAlarmName_
  pMetricName_
  pMonitoredResourceName_
  pComparisonOperator_
  pThreshold_
  pEvaluationPeriods_ =
    PutAlarm'
      { _paTreatMissingData = Nothing,
        _paContactProtocols = Nothing,
        _paDatapointsToAlarm = Nothing,
        _paNotificationEnabled = Nothing,
        _paNotificationTriggers = Nothing,
        _paAlarmName = pAlarmName_,
        _paMetricName = pMetricName_,
        _paMonitoredResourceName = pMonitoredResourceName_,
        _paComparisonOperator = pComparisonOperator_,
        _paThreshold = pThreshold_,
        _paEvaluationPeriods = pEvaluationPeriods_
      }

-- | Sets how this alarm will handle missing data points. An alarm can treat missing data in the following ways:     * @breaching@ - Assume the missing data is not within the threshold. Missing data counts towards the number of times the metric is not within the threshold.     * @notBreaching@ - Assume the missing data is within the threshold. Missing data does not count towards the number of times the metric is not within the threshold.     * @ignore@ - Ignore the missing data. Maintains the current alarm state.     * @missing@ - Missing data is treated as missing. If @treatMissingData@ is not specified, the default behavior of @missing@ is used.
paTreatMissingData :: Lens' PutAlarm (Maybe TreatMissingData)
paTreatMissingData = lens _paTreatMissingData (\s a -> s {_paTreatMissingData = a})

-- | The contact protocols to use for the alarm, such as @Email@ , @SMS@ (text messaging), or both. A notification is sent via the specified contact protocol if notifications are enabled for the alarm, and when the alarm is triggered. A notification is not sent if a contact protocol is not specified, if the specified contact protocol is not configured in the AWS Region, or if notifications are not enabled for the alarm using the @notificationEnabled@ paramater. Use the @CreateContactMethod@ action to configure a contact protocol in an AWS Region.
paContactProtocols :: Lens' PutAlarm [ContactProtocol]
paContactProtocols = lens _paContactProtocols (\s a -> s {_paContactProtocols = a}) . _Default . _Coerce

-- | The number of data points that must be not within the specified threshold to trigger the alarm. If you are setting an "M out of N" alarm, this value (@datapointsToAlarm@ ) is the M.
paDatapointsToAlarm :: Lens' PutAlarm (Maybe Int)
paDatapointsToAlarm = lens _paDatapointsToAlarm (\s a -> s {_paDatapointsToAlarm = a})

-- | Indicates whether the alarm is enabled. Notifications are enabled by default if you don't specify this parameter.
paNotificationEnabled :: Lens' PutAlarm (Maybe Bool)
paNotificationEnabled = lens _paNotificationEnabled (\s a -> s {_paNotificationEnabled = a})

-- | The alarm states that trigger a notification. An alarm has the following possible states:     * @ALARM@ - The metric is outside of the defined threshold.     * @INSUFFICIENT_DATA@ - The alarm has just started, the metric is not available, or not enough data is available for the metric to determine the alarm state.     * @OK@ - The metric is within the defined threshold. When you specify a notification trigger, the @ALARM@ state must be specified. The @INSUFFICIENT_DATA@ and @OK@ states can be specified in addition to the @ALARM@ state.     * If you specify @OK@ as an alarm trigger, a notification is sent when the alarm switches from an @ALARM@ or @INSUFFICIENT_DATA@ alarm state to an @OK@ state. This can be thought of as an /all clear/ alarm notification.     * If you specify @INSUFFICIENT_DATA@ as the alarm trigger, a notification is sent when the alarm switches from an @OK@ or @ALARM@ alarm state to an @INSUFFICIENT_DATA@ state. The notification trigger defaults to @ALARM@ if you don't specify this parameter.
paNotificationTriggers :: Lens' PutAlarm [AlarmState]
paNotificationTriggers = lens _paNotificationTriggers (\s a -> s {_paNotificationTriggers = a}) . _Default . _Coerce

-- | The name for the alarm. Specify the name of an existing alarm to update, and overwrite the previous configuration of the alarm.
paAlarmName :: Lens' PutAlarm Text
paAlarmName = lens _paAlarmName (\s a -> s {_paAlarmName = a})

-- | The name of the metric to associate with the alarm. You can configure up to two alarms per metric. The following metrics are available for each resource type:     * __Instances__ : @BurstCapacityPercentage@ , @BurstCapacityTime@ , @CPUUtilization@ , @NetworkIn@ , @NetworkOut@ , @StatusCheckFailed@ , @StatusCheckFailed_Instance@ , and @StatusCheckFailed_System@ .     * __Load balancers__ : @ClientTLSNegotiationErrorCount@ , @HealthyHostCount@ , @UnhealthyHostCount@ , @HTTPCode_LB_4XX_Count@ , @HTTPCode_LB_5XX_Count@ , @HTTPCode_Instance_2XX_Count@ , @HTTPCode_Instance_3XX_Count@ , @HTTPCode_Instance_4XX_Count@ , @HTTPCode_Instance_5XX_Count@ , @InstanceResponseTime@ , @RejectedConnectionCount@ , and @RequestCount@ .     * __Relational databases__ : @CPUUtilization@ , @DatabaseConnections@ , @DiskQueueDepth@ , @FreeStorageSpace@ , @NetworkReceiveThroughput@ , and @NetworkTransmitThroughput@ . For more information about these metrics, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-resource-health-metrics#available-metrics Metrics available in Lightsail> .
paMetricName :: Lens' PutAlarm MetricName
paMetricName = lens _paMetricName (\s a -> s {_paMetricName = a})

-- | The name of the Lightsail resource that will be monitored. Instances, load balancers, and relational databases are the only Lightsail resources that can currently be monitored by alarms.
paMonitoredResourceName :: Lens' PutAlarm Text
paMonitoredResourceName = lens _paMonitoredResourceName (\s a -> s {_paMonitoredResourceName = a})

-- | The arithmetic operation to use when comparing the specified statistic to the threshold. The specified statistic value is used as the first operand.
paComparisonOperator :: Lens' PutAlarm ComparisonOperator
paComparisonOperator = lens _paComparisonOperator (\s a -> s {_paComparisonOperator = a})

-- | The value against which the specified statistic is compared.
paThreshold :: Lens' PutAlarm Double
paThreshold = lens _paThreshold (\s a -> s {_paThreshold = a})

-- | The number of most recent periods over which data is compared to the specified threshold. If you are setting an "M out of N" alarm, this value (@evaluationPeriods@ ) is the N. If you are setting an alarm that requires that a number of consecutive data points be breaching to trigger the alarm, this value specifies the rolling period of time in which data points are evaluated. Each evaluation period is five minutes long. For example, specify an evaluation period of 24 to evaluate a metric over a rolling period of two hours. You can specify a minimum valuation period of 1 (5 minutes), and a maximum evaluation period of 288 (24 hours).
paEvaluationPeriods :: Lens' PutAlarm Int
paEvaluationPeriods = lens _paEvaluationPeriods (\s a -> s {_paEvaluationPeriods = a})

instance AWSRequest PutAlarm where
  type Rs PutAlarm = PutAlarmResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          PutAlarmResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable PutAlarm

instance NFData PutAlarm

instance ToHeaders PutAlarm where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("Lightsail_20161128.PutAlarm" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutAlarm where
  toJSON PutAlarm' {..} =
    object
      ( catMaybes
          [ ("treatMissingData" .=) <$> _paTreatMissingData,
            ("contactProtocols" .=) <$> _paContactProtocols,
            ("datapointsToAlarm" .=) <$> _paDatapointsToAlarm,
            ("notificationEnabled" .=) <$> _paNotificationEnabled,
            ("notificationTriggers" .=) <$> _paNotificationTriggers,
            Just ("alarmName" .= _paAlarmName),
            Just ("metricName" .= _paMetricName),
            Just ("monitoredResourceName" .= _paMonitoredResourceName),
            Just ("comparisonOperator" .= _paComparisonOperator),
            Just ("threshold" .= _paThreshold),
            Just ("evaluationPeriods" .= _paEvaluationPeriods)
          ]
      )

instance ToPath PutAlarm where
  toPath = const "/"

instance ToQuery PutAlarm where
  toQuery = const mempty

-- | /See:/ 'putAlarmResponse' smart constructor.
data PutAlarmResponse = PutAlarmResponse'
  { _parsOperations ::
      !(Maybe [Operation]),
    _parsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutAlarmResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'parsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'parsResponseStatus' - -- | The response status code.
putAlarmResponse ::
  -- | 'parsResponseStatus'
  Int ->
  PutAlarmResponse
putAlarmResponse pResponseStatus_ =
  PutAlarmResponse'
    { _parsOperations = Nothing,
      _parsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
parsOperations :: Lens' PutAlarmResponse [Operation]
parsOperations = lens _parsOperations (\s a -> s {_parsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
parsResponseStatus :: Lens' PutAlarmResponse Int
parsResponseStatus = lens _parsResponseStatus (\s a -> s {_parsResponseStatus = a})

instance NFData PutAlarmResponse
