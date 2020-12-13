{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- An alarm is used to monitor a single metric for one of your resources. When a metric condition is met, the alarm can notify you by email, SMS text message, and a banner displayed on the Amazon Lightsail console. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail> .
-- When this action creates an alarm, the alarm state is immediately set to @INSUFFICIENT_DATA@ . The alarm is then evaluated and its state is set appropriately. Any actions associated with the new state are then executed.
-- When you update an existing alarm, its state is left unchanged, but the update completely overwrites the previous configuration of the alarm. The alarm is then evaluated with the updated configuration.
module Network.AWS.Lightsail.PutAlarm
  ( -- * Creating a request
    PutAlarm (..),
    mkPutAlarm,

    -- ** Request lenses
    paAlarmName,
    paTreatMissingData,
    paContactProtocols,
    paEvaluationPeriods,
    paMonitoredResourceName,
    paMetricName,
    paComparisonOperator,
    paThreshold,
    paDatapointsToAlarm,
    paNotificationEnabled,
    paNotificationTriggers,

    -- * Destructuring the response
    PutAlarmResponse (..),
    mkPutAlarmResponse,

    -- ** Response lenses
    parsOperations,
    parsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutAlarm' smart constructor.
data PutAlarm = PutAlarm'
  { -- | The name for the alarm. Specify the name of an existing alarm to update, and overwrite the previous configuration of the alarm.
    alarmName :: Lude.Text,
    -- | Sets how this alarm will handle missing data points.
    --
    -- An alarm can treat missing data in the following ways:
    --
    --     * @breaching@ - Assume the missing data is not within the threshold. Missing data counts towards the number of times the metric is not within the threshold.
    --
    --
    --     * @notBreaching@ - Assume the missing data is within the threshold. Missing data does not count towards the number of times the metric is not within the threshold.
    --
    --
    --     * @ignore@ - Ignore the missing data. Maintains the current alarm state.
    --
    --
    --     * @missing@ - Missing data is treated as missing.
    --
    --
    -- If @treatMissingData@ is not specified, the default behavior of @missing@ is used.
    treatMissingData :: Lude.Maybe TreatMissingData,
    -- | The contact protocols to use for the alarm, such as @Email@ , @SMS@ (text messaging), or both.
    --
    -- A notification is sent via the specified contact protocol if notifications are enabled for the alarm, and when the alarm is triggered.
    -- A notification is not sent if a contact protocol is not specified, if the specified contact protocol is not configured in the AWS Region, or if notifications are not enabled for the alarm using the @notificationEnabled@ paramater.
    -- Use the @CreateContactMethod@ action to configure a contact protocol in an AWS Region.
    contactProtocols :: Lude.Maybe [ContactProtocol],
    -- | The number of most recent periods over which data is compared to the specified threshold. If you are setting an "M out of N" alarm, this value (@evaluationPeriods@ ) is the N.
    --
    -- If you are setting an alarm that requires that a number of consecutive data points be breaching to trigger the alarm, this value specifies the rolling period of time in which data points are evaluated.
    -- Each evaluation period is five minutes long. For example, specify an evaluation period of 24 to evaluate a metric over a rolling period of two hours.
    -- You can specify a minimum valuation period of 1 (5 minutes), and a maximum evaluation period of 288 (24 hours).
    evaluationPeriods :: Lude.Int,
    -- | The name of the Lightsail resource that will be monitored.
    --
    -- Instances, load balancers, and relational databases are the only Lightsail resources that can currently be monitored by alarms.
    monitoredResourceName :: Lude.Text,
    -- | The name of the metric to associate with the alarm.
    --
    -- You can configure up to two alarms per metric.
    -- The following metrics are available for each resource type:
    --
    --     * __Instances__ : @BurstCapacityPercentage@ , @BurstCapacityTime@ , @CPUUtilization@ , @NetworkIn@ , @NetworkOut@ , @StatusCheckFailed@ , @StatusCheckFailed_Instance@ , and @StatusCheckFailed_System@ .
    --
    --
    --     * __Load balancers__ : @ClientTLSNegotiationErrorCount@ , @HealthyHostCount@ , @UnhealthyHostCount@ , @HTTPCode_LB_4XX_Count@ , @HTTPCode_LB_5XX_Count@ , @HTTPCode_Instance_2XX_Count@ , @HTTPCode_Instance_3XX_Count@ , @HTTPCode_Instance_4XX_Count@ , @HTTPCode_Instance_5XX_Count@ , @InstanceResponseTime@ , @RejectedConnectionCount@ , and @RequestCount@ .
    --
    --
    --     * __Relational databases__ : @CPUUtilization@ , @DatabaseConnections@ , @DiskQueueDepth@ , @FreeStorageSpace@ , @NetworkReceiveThroughput@ , and @NetworkTransmitThroughput@ .
    --
    --
    -- For more information about these metrics, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-resource-health-metrics#available-metrics Metrics available in Lightsail> .
    metricName :: MetricName,
    -- | The arithmetic operation to use when comparing the specified statistic to the threshold. The specified statistic value is used as the first operand.
    comparisonOperator :: ComparisonOperator,
    -- | The value against which the specified statistic is compared.
    threshold :: Lude.Double,
    -- | The number of data points that must be not within the specified threshold to trigger the alarm. If you are setting an "M out of N" alarm, this value (@datapointsToAlarm@ ) is the M.
    datapointsToAlarm :: Lude.Maybe Lude.Int,
    -- | Indicates whether the alarm is enabled.
    --
    -- Notifications are enabled by default if you don't specify this parameter.
    notificationEnabled :: Lude.Maybe Lude.Bool,
    -- | The alarm states that trigger a notification.
    --
    -- An alarm has the following possible states:
    --
    --     * @ALARM@ - The metric is outside of the defined threshold.
    --
    --
    --     * @INSUFFICIENT_DATA@ - The alarm has just started, the metric is not available, or not enough data is available for the metric to determine the alarm state.
    --
    --
    --     * @OK@ - The metric is within the defined threshold.
    --
    --
    -- When you specify a notification trigger, the @ALARM@ state must be specified. The @INSUFFICIENT_DATA@ and @OK@ states can be specified in addition to the @ALARM@ state.
    --
    --     * If you specify @OK@ as an alarm trigger, a notification is sent when the alarm switches from an @ALARM@ or @INSUFFICIENT_DATA@ alarm state to an @OK@ state. This can be thought of as an /all clear/ alarm notification.
    --
    --
    --     * If you specify @INSUFFICIENT_DATA@ as the alarm trigger, a notification is sent when the alarm switches from an @OK@ or @ALARM@ alarm state to an @INSUFFICIENT_DATA@ state.
    --
    --
    -- The notification trigger defaults to @ALARM@ if you don't specify this parameter.
    notificationTriggers :: Lude.Maybe [AlarmState]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAlarm' with the minimum fields required to make a request.
--
-- * 'alarmName' - The name for the alarm. Specify the name of an existing alarm to update, and overwrite the previous configuration of the alarm.
-- * 'treatMissingData' - Sets how this alarm will handle missing data points.
--
-- An alarm can treat missing data in the following ways:
--
--     * @breaching@ - Assume the missing data is not within the threshold. Missing data counts towards the number of times the metric is not within the threshold.
--
--
--     * @notBreaching@ - Assume the missing data is within the threshold. Missing data does not count towards the number of times the metric is not within the threshold.
--
--
--     * @ignore@ - Ignore the missing data. Maintains the current alarm state.
--
--
--     * @missing@ - Missing data is treated as missing.
--
--
-- If @treatMissingData@ is not specified, the default behavior of @missing@ is used.
-- * 'contactProtocols' - The contact protocols to use for the alarm, such as @Email@ , @SMS@ (text messaging), or both.
--
-- A notification is sent via the specified contact protocol if notifications are enabled for the alarm, and when the alarm is triggered.
-- A notification is not sent if a contact protocol is not specified, if the specified contact protocol is not configured in the AWS Region, or if notifications are not enabled for the alarm using the @notificationEnabled@ paramater.
-- Use the @CreateContactMethod@ action to configure a contact protocol in an AWS Region.
-- * 'evaluationPeriods' - The number of most recent periods over which data is compared to the specified threshold. If you are setting an "M out of N" alarm, this value (@evaluationPeriods@ ) is the N.
--
-- If you are setting an alarm that requires that a number of consecutive data points be breaching to trigger the alarm, this value specifies the rolling period of time in which data points are evaluated.
-- Each evaluation period is five minutes long. For example, specify an evaluation period of 24 to evaluate a metric over a rolling period of two hours.
-- You can specify a minimum valuation period of 1 (5 minutes), and a maximum evaluation period of 288 (24 hours).
-- * 'monitoredResourceName' - The name of the Lightsail resource that will be monitored.
--
-- Instances, load balancers, and relational databases are the only Lightsail resources that can currently be monitored by alarms.
-- * 'metricName' - The name of the metric to associate with the alarm.
--
-- You can configure up to two alarms per metric.
-- The following metrics are available for each resource type:
--
--     * __Instances__ : @BurstCapacityPercentage@ , @BurstCapacityTime@ , @CPUUtilization@ , @NetworkIn@ , @NetworkOut@ , @StatusCheckFailed@ , @StatusCheckFailed_Instance@ , and @StatusCheckFailed_System@ .
--
--
--     * __Load balancers__ : @ClientTLSNegotiationErrorCount@ , @HealthyHostCount@ , @UnhealthyHostCount@ , @HTTPCode_LB_4XX_Count@ , @HTTPCode_LB_5XX_Count@ , @HTTPCode_Instance_2XX_Count@ , @HTTPCode_Instance_3XX_Count@ , @HTTPCode_Instance_4XX_Count@ , @HTTPCode_Instance_5XX_Count@ , @InstanceResponseTime@ , @RejectedConnectionCount@ , and @RequestCount@ .
--
--
--     * __Relational databases__ : @CPUUtilization@ , @DatabaseConnections@ , @DiskQueueDepth@ , @FreeStorageSpace@ , @NetworkReceiveThroughput@ , and @NetworkTransmitThroughput@ .
--
--
-- For more information about these metrics, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-resource-health-metrics#available-metrics Metrics available in Lightsail> .
-- * 'comparisonOperator' - The arithmetic operation to use when comparing the specified statistic to the threshold. The specified statistic value is used as the first operand.
-- * 'threshold' - The value against which the specified statistic is compared.
-- * 'datapointsToAlarm' - The number of data points that must be not within the specified threshold to trigger the alarm. If you are setting an "M out of N" alarm, this value (@datapointsToAlarm@ ) is the M.
-- * 'notificationEnabled' - Indicates whether the alarm is enabled.
--
-- Notifications are enabled by default if you don't specify this parameter.
-- * 'notificationTriggers' - The alarm states that trigger a notification.
--
-- An alarm has the following possible states:
--
--     * @ALARM@ - The metric is outside of the defined threshold.
--
--
--     * @INSUFFICIENT_DATA@ - The alarm has just started, the metric is not available, or not enough data is available for the metric to determine the alarm state.
--
--
--     * @OK@ - The metric is within the defined threshold.
--
--
-- When you specify a notification trigger, the @ALARM@ state must be specified. The @INSUFFICIENT_DATA@ and @OK@ states can be specified in addition to the @ALARM@ state.
--
--     * If you specify @OK@ as an alarm trigger, a notification is sent when the alarm switches from an @ALARM@ or @INSUFFICIENT_DATA@ alarm state to an @OK@ state. This can be thought of as an /all clear/ alarm notification.
--
--
--     * If you specify @INSUFFICIENT_DATA@ as the alarm trigger, a notification is sent when the alarm switches from an @OK@ or @ALARM@ alarm state to an @INSUFFICIENT_DATA@ state.
--
--
-- The notification trigger defaults to @ALARM@ if you don't specify this parameter.
mkPutAlarm ::
  -- | 'alarmName'
  Lude.Text ->
  -- | 'evaluationPeriods'
  Lude.Int ->
  -- | 'monitoredResourceName'
  Lude.Text ->
  -- | 'metricName'
  MetricName ->
  -- | 'comparisonOperator'
  ComparisonOperator ->
  -- | 'threshold'
  Lude.Double ->
  PutAlarm
mkPutAlarm
  pAlarmName_
  pEvaluationPeriods_
  pMonitoredResourceName_
  pMetricName_
  pComparisonOperator_
  pThreshold_ =
    PutAlarm'
      { alarmName = pAlarmName_,
        treatMissingData = Lude.Nothing,
        contactProtocols = Lude.Nothing,
        evaluationPeriods = pEvaluationPeriods_,
        monitoredResourceName = pMonitoredResourceName_,
        metricName = pMetricName_,
        comparisonOperator = pComparisonOperator_,
        threshold = pThreshold_,
        datapointsToAlarm = Lude.Nothing,
        notificationEnabled = Lude.Nothing,
        notificationTriggers = Lude.Nothing
      }

-- | The name for the alarm. Specify the name of an existing alarm to update, and overwrite the previous configuration of the alarm.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paAlarmName :: Lens.Lens' PutAlarm Lude.Text
paAlarmName = Lens.lens (alarmName :: PutAlarm -> Lude.Text) (\s a -> s {alarmName = a} :: PutAlarm)
{-# DEPRECATED paAlarmName "Use generic-lens or generic-optics with 'alarmName' instead." #-}

-- | Sets how this alarm will handle missing data points.
--
-- An alarm can treat missing data in the following ways:
--
--     * @breaching@ - Assume the missing data is not within the threshold. Missing data counts towards the number of times the metric is not within the threshold.
--
--
--     * @notBreaching@ - Assume the missing data is within the threshold. Missing data does not count towards the number of times the metric is not within the threshold.
--
--
--     * @ignore@ - Ignore the missing data. Maintains the current alarm state.
--
--
--     * @missing@ - Missing data is treated as missing.
--
--
-- If @treatMissingData@ is not specified, the default behavior of @missing@ is used.
--
-- /Note:/ Consider using 'treatMissingData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paTreatMissingData :: Lens.Lens' PutAlarm (Lude.Maybe TreatMissingData)
paTreatMissingData = Lens.lens (treatMissingData :: PutAlarm -> Lude.Maybe TreatMissingData) (\s a -> s {treatMissingData = a} :: PutAlarm)
{-# DEPRECATED paTreatMissingData "Use generic-lens or generic-optics with 'treatMissingData' instead." #-}

-- | The contact protocols to use for the alarm, such as @Email@ , @SMS@ (text messaging), or both.
--
-- A notification is sent via the specified contact protocol if notifications are enabled for the alarm, and when the alarm is triggered.
-- A notification is not sent if a contact protocol is not specified, if the specified contact protocol is not configured in the AWS Region, or if notifications are not enabled for the alarm using the @notificationEnabled@ paramater.
-- Use the @CreateContactMethod@ action to configure a contact protocol in an AWS Region.
--
-- /Note:/ Consider using 'contactProtocols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paContactProtocols :: Lens.Lens' PutAlarm (Lude.Maybe [ContactProtocol])
paContactProtocols = Lens.lens (contactProtocols :: PutAlarm -> Lude.Maybe [ContactProtocol]) (\s a -> s {contactProtocols = a} :: PutAlarm)
{-# DEPRECATED paContactProtocols "Use generic-lens or generic-optics with 'contactProtocols' instead." #-}

-- | The number of most recent periods over which data is compared to the specified threshold. If you are setting an "M out of N" alarm, this value (@evaluationPeriods@ ) is the N.
--
-- If you are setting an alarm that requires that a number of consecutive data points be breaching to trigger the alarm, this value specifies the rolling period of time in which data points are evaluated.
-- Each evaluation period is five minutes long. For example, specify an evaluation period of 24 to evaluate a metric over a rolling period of two hours.
-- You can specify a minimum valuation period of 1 (5 minutes), and a maximum evaluation period of 288 (24 hours).
--
-- /Note:/ Consider using 'evaluationPeriods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paEvaluationPeriods :: Lens.Lens' PutAlarm Lude.Int
paEvaluationPeriods = Lens.lens (evaluationPeriods :: PutAlarm -> Lude.Int) (\s a -> s {evaluationPeriods = a} :: PutAlarm)
{-# DEPRECATED paEvaluationPeriods "Use generic-lens or generic-optics with 'evaluationPeriods' instead." #-}

-- | The name of the Lightsail resource that will be monitored.
--
-- Instances, load balancers, and relational databases are the only Lightsail resources that can currently be monitored by alarms.
--
-- /Note:/ Consider using 'monitoredResourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paMonitoredResourceName :: Lens.Lens' PutAlarm Lude.Text
paMonitoredResourceName = Lens.lens (monitoredResourceName :: PutAlarm -> Lude.Text) (\s a -> s {monitoredResourceName = a} :: PutAlarm)
{-# DEPRECATED paMonitoredResourceName "Use generic-lens or generic-optics with 'monitoredResourceName' instead." #-}

-- | The name of the metric to associate with the alarm.
--
-- You can configure up to two alarms per metric.
-- The following metrics are available for each resource type:
--
--     * __Instances__ : @BurstCapacityPercentage@ , @BurstCapacityTime@ , @CPUUtilization@ , @NetworkIn@ , @NetworkOut@ , @StatusCheckFailed@ , @StatusCheckFailed_Instance@ , and @StatusCheckFailed_System@ .
--
--
--     * __Load balancers__ : @ClientTLSNegotiationErrorCount@ , @HealthyHostCount@ , @UnhealthyHostCount@ , @HTTPCode_LB_4XX_Count@ , @HTTPCode_LB_5XX_Count@ , @HTTPCode_Instance_2XX_Count@ , @HTTPCode_Instance_3XX_Count@ , @HTTPCode_Instance_4XX_Count@ , @HTTPCode_Instance_5XX_Count@ , @InstanceResponseTime@ , @RejectedConnectionCount@ , and @RequestCount@ .
--
--
--     * __Relational databases__ : @CPUUtilization@ , @DatabaseConnections@ , @DiskQueueDepth@ , @FreeStorageSpace@ , @NetworkReceiveThroughput@ , and @NetworkTransmitThroughput@ .
--
--
-- For more information about these metrics, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-resource-health-metrics#available-metrics Metrics available in Lightsail> .
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paMetricName :: Lens.Lens' PutAlarm MetricName
paMetricName = Lens.lens (metricName :: PutAlarm -> MetricName) (\s a -> s {metricName = a} :: PutAlarm)
{-# DEPRECATED paMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The arithmetic operation to use when comparing the specified statistic to the threshold. The specified statistic value is used as the first operand.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paComparisonOperator :: Lens.Lens' PutAlarm ComparisonOperator
paComparisonOperator = Lens.lens (comparisonOperator :: PutAlarm -> ComparisonOperator) (\s a -> s {comparisonOperator = a} :: PutAlarm)
{-# DEPRECATED paComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | The value against which the specified statistic is compared.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paThreshold :: Lens.Lens' PutAlarm Lude.Double
paThreshold = Lens.lens (threshold :: PutAlarm -> Lude.Double) (\s a -> s {threshold = a} :: PutAlarm)
{-# DEPRECATED paThreshold "Use generic-lens or generic-optics with 'threshold' instead." #-}

-- | The number of data points that must be not within the specified threshold to trigger the alarm. If you are setting an "M out of N" alarm, this value (@datapointsToAlarm@ ) is the M.
--
-- /Note:/ Consider using 'datapointsToAlarm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paDatapointsToAlarm :: Lens.Lens' PutAlarm (Lude.Maybe Lude.Int)
paDatapointsToAlarm = Lens.lens (datapointsToAlarm :: PutAlarm -> Lude.Maybe Lude.Int) (\s a -> s {datapointsToAlarm = a} :: PutAlarm)
{-# DEPRECATED paDatapointsToAlarm "Use generic-lens or generic-optics with 'datapointsToAlarm' instead." #-}

-- | Indicates whether the alarm is enabled.
--
-- Notifications are enabled by default if you don't specify this parameter.
--
-- /Note:/ Consider using 'notificationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paNotificationEnabled :: Lens.Lens' PutAlarm (Lude.Maybe Lude.Bool)
paNotificationEnabled = Lens.lens (notificationEnabled :: PutAlarm -> Lude.Maybe Lude.Bool) (\s a -> s {notificationEnabled = a} :: PutAlarm)
{-# DEPRECATED paNotificationEnabled "Use generic-lens or generic-optics with 'notificationEnabled' instead." #-}

-- | The alarm states that trigger a notification.
--
-- An alarm has the following possible states:
--
--     * @ALARM@ - The metric is outside of the defined threshold.
--
--
--     * @INSUFFICIENT_DATA@ - The alarm has just started, the metric is not available, or not enough data is available for the metric to determine the alarm state.
--
--
--     * @OK@ - The metric is within the defined threshold.
--
--
-- When you specify a notification trigger, the @ALARM@ state must be specified. The @INSUFFICIENT_DATA@ and @OK@ states can be specified in addition to the @ALARM@ state.
--
--     * If you specify @OK@ as an alarm trigger, a notification is sent when the alarm switches from an @ALARM@ or @INSUFFICIENT_DATA@ alarm state to an @OK@ state. This can be thought of as an /all clear/ alarm notification.
--
--
--     * If you specify @INSUFFICIENT_DATA@ as the alarm trigger, a notification is sent when the alarm switches from an @OK@ or @ALARM@ alarm state to an @INSUFFICIENT_DATA@ state.
--
--
-- The notification trigger defaults to @ALARM@ if you don't specify this parameter.
--
-- /Note:/ Consider using 'notificationTriggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paNotificationTriggers :: Lens.Lens' PutAlarm (Lude.Maybe [AlarmState])
paNotificationTriggers = Lens.lens (notificationTriggers :: PutAlarm -> Lude.Maybe [AlarmState]) (\s a -> s {notificationTriggers = a} :: PutAlarm)
{-# DEPRECATED paNotificationTriggers "Use generic-lens or generic-optics with 'notificationTriggers' instead." #-}

instance Lude.AWSRequest PutAlarm where
  type Rs PutAlarm = PutAlarmResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutAlarmResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutAlarm where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.PutAlarm" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutAlarm where
  toJSON PutAlarm' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("alarmName" Lude..= alarmName),
            ("treatMissingData" Lude..=) Lude.<$> treatMissingData,
            ("contactProtocols" Lude..=) Lude.<$> contactProtocols,
            Lude.Just ("evaluationPeriods" Lude..= evaluationPeriods),
            Lude.Just ("monitoredResourceName" Lude..= monitoredResourceName),
            Lude.Just ("metricName" Lude..= metricName),
            Lude.Just ("comparisonOperator" Lude..= comparisonOperator),
            Lude.Just ("threshold" Lude..= threshold),
            ("datapointsToAlarm" Lude..=) Lude.<$> datapointsToAlarm,
            ("notificationEnabled" Lude..=) Lude.<$> notificationEnabled,
            ("notificationTriggers" Lude..=) Lude.<$> notificationTriggers
          ]
      )

instance Lude.ToPath PutAlarm where
  toPath = Lude.const "/"

instance Lude.ToQuery PutAlarm where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutAlarmResponse' smart constructor.
data PutAlarmResponse = PutAlarmResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAlarmResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkPutAlarmResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutAlarmResponse
mkPutAlarmResponse pResponseStatus_ =
  PutAlarmResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parsOperations :: Lens.Lens' PutAlarmResponse (Lude.Maybe [Operation])
parsOperations = Lens.lens (operations :: PutAlarmResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: PutAlarmResponse)
{-# DEPRECATED parsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parsResponseStatus :: Lens.Lens' PutAlarmResponse Lude.Int
parsResponseStatus = Lens.lens (responseStatus :: PutAlarmResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutAlarmResponse)
{-# DEPRECATED parsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
