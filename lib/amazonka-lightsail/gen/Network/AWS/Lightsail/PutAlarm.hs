{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      PutAlarm (..)
    , mkPutAlarm
    -- ** Request lenses
    , paAlarmName
    , paMetricName
    , paMonitoredResourceName
    , paComparisonOperator
    , paThreshold
    , paEvaluationPeriods
    , paContactProtocols
    , paDatapointsToAlarm
    , paNotificationEnabled
    , paNotificationTriggers
    , paTreatMissingData

    -- * Destructuring the response
    , PutAlarmResponse (..)
    , mkPutAlarmResponse
    -- ** Response lenses
    , parrsOperations
    , parrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutAlarm' smart constructor.
data PutAlarm = PutAlarm'
  { alarmName :: Types.ResourceName
    -- ^ The name for the alarm. Specify the name of an existing alarm to update, and overwrite the previous configuration of the alarm.
  , metricName :: Types.MetricName
    -- ^ The name of the metric to associate with the alarm.
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
  , monitoredResourceName :: Types.ResourceName
    -- ^ The name of the Lightsail resource that will be monitored.
--
-- Instances, load balancers, and relational databases are the only Lightsail resources that can currently be monitored by alarms.
  , comparisonOperator :: Types.ComparisonOperator
    -- ^ The arithmetic operation to use when comparing the specified statistic to the threshold. The specified statistic value is used as the first operand.
  , threshold :: Core.Double
    -- ^ The value against which the specified statistic is compared.
  , evaluationPeriods :: Core.Int
    -- ^ The number of most recent periods over which data is compared to the specified threshold. If you are setting an "M out of N" alarm, this value (@evaluationPeriods@ ) is the N.
--
-- If you are setting an alarm that requires that a number of consecutive data points be breaching to trigger the alarm, this value specifies the rolling period of time in which data points are evaluated.
-- Each evaluation period is five minutes long. For example, specify an evaluation period of 24 to evaluate a metric over a rolling period of two hours.
-- You can specify a minimum valuation period of 1 (5 minutes), and a maximum evaluation period of 288 (24 hours).
  , contactProtocols :: Core.Maybe [Types.ContactProtocol]
    -- ^ The contact protocols to use for the alarm, such as @Email@ , @SMS@ (text messaging), or both.
--
-- A notification is sent via the specified contact protocol if notifications are enabled for the alarm, and when the alarm is triggered.
-- A notification is not sent if a contact protocol is not specified, if the specified contact protocol is not configured in the AWS Region, or if notifications are not enabled for the alarm using the @notificationEnabled@ paramater.
-- Use the @CreateContactMethod@ action to configure a contact protocol in an AWS Region.
  , datapointsToAlarm :: Core.Maybe Core.Int
    -- ^ The number of data points that must be not within the specified threshold to trigger the alarm. If you are setting an "M out of N" alarm, this value (@datapointsToAlarm@ ) is the M.
  , notificationEnabled :: Core.Maybe Core.Bool
    -- ^ Indicates whether the alarm is enabled.
--
-- Notifications are enabled by default if you don't specify this parameter.
  , notificationTriggers :: Core.Maybe [Types.AlarmState]
    -- ^ The alarm states that trigger a notification.
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
  , treatMissingData :: Core.Maybe Types.TreatMissingData
    -- ^ Sets how this alarm will handle missing data points.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutAlarm' value with any optional fields omitted.
mkPutAlarm
    :: Types.ResourceName -- ^ 'alarmName'
    -> Types.MetricName -- ^ 'metricName'
    -> Types.ResourceName -- ^ 'monitoredResourceName'
    -> Types.ComparisonOperator -- ^ 'comparisonOperator'
    -> Core.Double -- ^ 'threshold'
    -> Core.Int -- ^ 'evaluationPeriods'
    -> PutAlarm
mkPutAlarm alarmName metricName monitoredResourceName
  comparisonOperator threshold evaluationPeriods
  = PutAlarm'{alarmName, metricName, monitoredResourceName,
              comparisonOperator, threshold, evaluationPeriods,
              contactProtocols = Core.Nothing, datapointsToAlarm = Core.Nothing,
              notificationEnabled = Core.Nothing,
              notificationTriggers = Core.Nothing,
              treatMissingData = Core.Nothing}

-- | The name for the alarm. Specify the name of an existing alarm to update, and overwrite the previous configuration of the alarm.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paAlarmName :: Lens.Lens' PutAlarm Types.ResourceName
paAlarmName = Lens.field @"alarmName"
{-# INLINEABLE paAlarmName #-}
{-# DEPRECATED alarmName "Use generic-lens or generic-optics with 'alarmName' instead"  #-}

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
paMetricName :: Lens.Lens' PutAlarm Types.MetricName
paMetricName = Lens.field @"metricName"
{-# INLINEABLE paMetricName #-}
{-# DEPRECATED metricName "Use generic-lens or generic-optics with 'metricName' instead"  #-}

-- | The name of the Lightsail resource that will be monitored.
--
-- Instances, load balancers, and relational databases are the only Lightsail resources that can currently be monitored by alarms.
--
-- /Note:/ Consider using 'monitoredResourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paMonitoredResourceName :: Lens.Lens' PutAlarm Types.ResourceName
paMonitoredResourceName = Lens.field @"monitoredResourceName"
{-# INLINEABLE paMonitoredResourceName #-}
{-# DEPRECATED monitoredResourceName "Use generic-lens or generic-optics with 'monitoredResourceName' instead"  #-}

-- | The arithmetic operation to use when comparing the specified statistic to the threshold. The specified statistic value is used as the first operand.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paComparisonOperator :: Lens.Lens' PutAlarm Types.ComparisonOperator
paComparisonOperator = Lens.field @"comparisonOperator"
{-# INLINEABLE paComparisonOperator #-}
{-# DEPRECATED comparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead"  #-}

-- | The value against which the specified statistic is compared.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paThreshold :: Lens.Lens' PutAlarm Core.Double
paThreshold = Lens.field @"threshold"
{-# INLINEABLE paThreshold #-}
{-# DEPRECATED threshold "Use generic-lens or generic-optics with 'threshold' instead"  #-}

-- | The number of most recent periods over which data is compared to the specified threshold. If you are setting an "M out of N" alarm, this value (@evaluationPeriods@ ) is the N.
--
-- If you are setting an alarm that requires that a number of consecutive data points be breaching to trigger the alarm, this value specifies the rolling period of time in which data points are evaluated.
-- Each evaluation period is five minutes long. For example, specify an evaluation period of 24 to evaluate a metric over a rolling period of two hours.
-- You can specify a minimum valuation period of 1 (5 minutes), and a maximum evaluation period of 288 (24 hours).
--
-- /Note:/ Consider using 'evaluationPeriods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paEvaluationPeriods :: Lens.Lens' PutAlarm Core.Int
paEvaluationPeriods = Lens.field @"evaluationPeriods"
{-# INLINEABLE paEvaluationPeriods #-}
{-# DEPRECATED evaluationPeriods "Use generic-lens or generic-optics with 'evaluationPeriods' instead"  #-}

-- | The contact protocols to use for the alarm, such as @Email@ , @SMS@ (text messaging), or both.
--
-- A notification is sent via the specified contact protocol if notifications are enabled for the alarm, and when the alarm is triggered.
-- A notification is not sent if a contact protocol is not specified, if the specified contact protocol is not configured in the AWS Region, or if notifications are not enabled for the alarm using the @notificationEnabled@ paramater.
-- Use the @CreateContactMethod@ action to configure a contact protocol in an AWS Region.
--
-- /Note:/ Consider using 'contactProtocols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paContactProtocols :: Lens.Lens' PutAlarm (Core.Maybe [Types.ContactProtocol])
paContactProtocols = Lens.field @"contactProtocols"
{-# INLINEABLE paContactProtocols #-}
{-# DEPRECATED contactProtocols "Use generic-lens or generic-optics with 'contactProtocols' instead"  #-}

-- | The number of data points that must be not within the specified threshold to trigger the alarm. If you are setting an "M out of N" alarm, this value (@datapointsToAlarm@ ) is the M.
--
-- /Note:/ Consider using 'datapointsToAlarm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paDatapointsToAlarm :: Lens.Lens' PutAlarm (Core.Maybe Core.Int)
paDatapointsToAlarm = Lens.field @"datapointsToAlarm"
{-# INLINEABLE paDatapointsToAlarm #-}
{-# DEPRECATED datapointsToAlarm "Use generic-lens or generic-optics with 'datapointsToAlarm' instead"  #-}

-- | Indicates whether the alarm is enabled.
--
-- Notifications are enabled by default if you don't specify this parameter.
--
-- /Note:/ Consider using 'notificationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paNotificationEnabled :: Lens.Lens' PutAlarm (Core.Maybe Core.Bool)
paNotificationEnabled = Lens.field @"notificationEnabled"
{-# INLINEABLE paNotificationEnabled #-}
{-# DEPRECATED notificationEnabled "Use generic-lens or generic-optics with 'notificationEnabled' instead"  #-}

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
paNotificationTriggers :: Lens.Lens' PutAlarm (Core.Maybe [Types.AlarmState])
paNotificationTriggers = Lens.field @"notificationTriggers"
{-# INLINEABLE paNotificationTriggers #-}
{-# DEPRECATED notificationTriggers "Use generic-lens or generic-optics with 'notificationTriggers' instead"  #-}

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
paTreatMissingData :: Lens.Lens' PutAlarm (Core.Maybe Types.TreatMissingData)
paTreatMissingData = Lens.field @"treatMissingData"
{-# INLINEABLE paTreatMissingData #-}
{-# DEPRECATED treatMissingData "Use generic-lens or generic-optics with 'treatMissingData' instead"  #-}

instance Core.ToQuery PutAlarm where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutAlarm where
        toHeaders PutAlarm{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.PutAlarm") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutAlarm where
        toJSON PutAlarm{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("alarmName" Core..= alarmName),
                  Core.Just ("metricName" Core..= metricName),
                  Core.Just ("monitoredResourceName" Core..= monitoredResourceName),
                  Core.Just ("comparisonOperator" Core..= comparisonOperator),
                  Core.Just ("threshold" Core..= threshold),
                  Core.Just ("evaluationPeriods" Core..= evaluationPeriods),
                  ("contactProtocols" Core..=) Core.<$> contactProtocols,
                  ("datapointsToAlarm" Core..=) Core.<$> datapointsToAlarm,
                  ("notificationEnabled" Core..=) Core.<$> notificationEnabled,
                  ("notificationTriggers" Core..=) Core.<$> notificationTriggers,
                  ("treatMissingData" Core..=) Core.<$> treatMissingData])

instance Core.AWSRequest PutAlarm where
        type Rs PutAlarm = PutAlarmResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutAlarmResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutAlarmResponse' smart constructor.
data PutAlarmResponse = PutAlarmResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PutAlarmResponse' value with any optional fields omitted.
mkPutAlarmResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutAlarmResponse
mkPutAlarmResponse responseStatus
  = PutAlarmResponse'{operations = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parrsOperations :: Lens.Lens' PutAlarmResponse (Core.Maybe [Types.Operation])
parrsOperations = Lens.field @"operations"
{-# INLINEABLE parrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parrsResponseStatus :: Lens.Lens' PutAlarmResponse Core.Int
parrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE parrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
