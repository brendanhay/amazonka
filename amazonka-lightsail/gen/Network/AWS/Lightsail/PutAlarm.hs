{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.PutAlarm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an alarm, and associates it with the specified
-- metric.
--
-- An alarm is used to monitor a single metric for one of your resources.
-- When a metric condition is met, the alarm can notify you by email, SMS
-- text message, and a banner displayed on the Amazon Lightsail console.
-- For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail>.
--
-- When this action creates an alarm, the alarm state is immediately set to
-- @INSUFFICIENT_DATA@. The alarm is then evaluated and its state is set
-- appropriately. Any actions associated with the new state are then
-- executed.
--
-- When you update an existing alarm, its state is left unchanged, but the
-- update completely overwrites the previous configuration of the alarm.
-- The alarm is then evaluated with the updated configuration.
module Network.AWS.Lightsail.PutAlarm
  ( -- * Creating a Request
    PutAlarm (..),
    newPutAlarm,

    -- * Request Lenses
    putAlarm_datapointsToAlarm,
    putAlarm_notificationTriggers,
    putAlarm_notificationEnabled,
    putAlarm_treatMissingData,
    putAlarm_contactProtocols,
    putAlarm_alarmName,
    putAlarm_metricName,
    putAlarm_monitoredResourceName,
    putAlarm_comparisonOperator,
    putAlarm_threshold,
    putAlarm_evaluationPeriods,

    -- * Destructuring the Response
    PutAlarmResponse (..),
    newPutAlarmResponse,

    -- * Response Lenses
    putAlarmResponse_operations,
    putAlarmResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutAlarm' smart constructor.
data PutAlarm = PutAlarm'
  { -- | The number of data points that must be not within the specified
    -- threshold to trigger the alarm. If you are setting an \"M out of N\"
    -- alarm, this value (@datapointsToAlarm@) is the M.
    datapointsToAlarm :: Core.Maybe Core.Int,
    -- | The alarm states that trigger a notification.
    --
    -- An alarm has the following possible states:
    --
    -- -   @ALARM@ - The metric is outside of the defined threshold.
    --
    -- -   @INSUFFICIENT_DATA@ - The alarm has just started, the metric is not
    --     available, or not enough data is available for the metric to
    --     determine the alarm state.
    --
    -- -   @OK@ - The metric is within the defined threshold.
    --
    -- When you specify a notification trigger, the @ALARM@ state must be
    -- specified. The @INSUFFICIENT_DATA@ and @OK@ states can be specified in
    -- addition to the @ALARM@ state.
    --
    -- -   If you specify @OK@ as an alarm trigger, a notification is sent when
    --     the alarm switches from an @ALARM@ or @INSUFFICIENT_DATA@ alarm
    --     state to an @OK@ state. This can be thought of as an /all clear/
    --     alarm notification.
    --
    -- -   If you specify @INSUFFICIENT_DATA@ as the alarm trigger, a
    --     notification is sent when the alarm switches from an @OK@ or @ALARM@
    --     alarm state to an @INSUFFICIENT_DATA@ state.
    --
    -- The notification trigger defaults to @ALARM@ if you don\'t specify this
    -- parameter.
    notificationTriggers :: Core.Maybe [AlarmState],
    -- | Indicates whether the alarm is enabled.
    --
    -- Notifications are enabled by default if you don\'t specify this
    -- parameter.
    notificationEnabled :: Core.Maybe Core.Bool,
    -- | Sets how this alarm will handle missing data points.
    --
    -- An alarm can treat missing data in the following ways:
    --
    -- -   @breaching@ - Assume the missing data is not within the threshold.
    --     Missing data counts towards the number of times the metric is not
    --     within the threshold.
    --
    -- -   @notBreaching@ - Assume the missing data is within the threshold.
    --     Missing data does not count towards the number of times the metric
    --     is not within the threshold.
    --
    -- -   @ignore@ - Ignore the missing data. Maintains the current alarm
    --     state.
    --
    -- -   @missing@ - Missing data is treated as missing.
    --
    -- If @treatMissingData@ is not specified, the default behavior of
    -- @missing@ is used.
    treatMissingData :: Core.Maybe TreatMissingData,
    -- | The contact protocols to use for the alarm, such as @Email@, @SMS@ (text
    -- messaging), or both.
    --
    -- A notification is sent via the specified contact protocol if
    -- notifications are enabled for the alarm, and when the alarm is
    -- triggered.
    --
    -- A notification is not sent if a contact protocol is not specified, if
    -- the specified contact protocol is not configured in the AWS Region, or
    -- if notifications are not enabled for the alarm using the
    -- @notificationEnabled@ paramater.
    --
    -- Use the @CreateContactMethod@ action to configure a contact protocol in
    -- an AWS Region.
    contactProtocols :: Core.Maybe [ContactProtocol],
    -- | The name for the alarm. Specify the name of an existing alarm to update,
    -- and overwrite the previous configuration of the alarm.
    alarmName :: Core.Text,
    -- | The name of the metric to associate with the alarm.
    --
    -- You can configure up to two alarms per metric.
    --
    -- The following metrics are available for each resource type:
    --
    -- -   __Instances__: @BurstCapacityPercentage@, @BurstCapacityTime@,
    --     @CPUUtilization@, @NetworkIn@, @NetworkOut@, @StatusCheckFailed@,
    --     @StatusCheckFailed_Instance@, and @StatusCheckFailed_System@.
    --
    -- -   __Load balancers__: @ClientTLSNegotiationErrorCount@,
    --     @HealthyHostCount@, @UnhealthyHostCount@, @HTTPCode_LB_4XX_Count@,
    --     @HTTPCode_LB_5XX_Count@, @HTTPCode_Instance_2XX_Count@,
    --     @HTTPCode_Instance_3XX_Count@, @HTTPCode_Instance_4XX_Count@,
    --     @HTTPCode_Instance_5XX_Count@, @InstanceResponseTime@,
    --     @RejectedConnectionCount@, and @RequestCount@.
    --
    -- -   __Relational databases__: @CPUUtilization@, @DatabaseConnections@,
    --     @DiskQueueDepth@, @FreeStorageSpace@, @NetworkReceiveThroughput@,
    --     and @NetworkTransmitThroughput@.
    --
    -- For more information about these metrics, see
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-resource-health-metrics#available-metrics Metrics available in Lightsail>.
    metricName :: MetricName,
    -- | The name of the Lightsail resource that will be monitored.
    --
    -- Instances, load balancers, and relational databases are the only
    -- Lightsail resources that can currently be monitored by alarms.
    monitoredResourceName :: Core.Text,
    -- | The arithmetic operation to use when comparing the specified statistic
    -- to the threshold. The specified statistic value is used as the first
    -- operand.
    comparisonOperator :: ComparisonOperator,
    -- | The value against which the specified statistic is compared.
    threshold :: Core.Double,
    -- | The number of most recent periods over which data is compared to the
    -- specified threshold. If you are setting an \"M out of N\" alarm, this
    -- value (@evaluationPeriods@) is the N.
    --
    -- If you are setting an alarm that requires that a number of consecutive
    -- data points be breaching to trigger the alarm, this value specifies the
    -- rolling period of time in which data points are evaluated.
    --
    -- Each evaluation period is five minutes long. For example, specify an
    -- evaluation period of 24 to evaluate a metric over a rolling period of
    -- two hours.
    --
    -- You can specify a minimum valuation period of 1 (5 minutes), and a
    -- maximum evaluation period of 288 (24 hours).
    evaluationPeriods :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutAlarm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datapointsToAlarm', 'putAlarm_datapointsToAlarm' - The number of data points that must be not within the specified
-- threshold to trigger the alarm. If you are setting an \"M out of N\"
-- alarm, this value (@datapointsToAlarm@) is the M.
--
-- 'notificationTriggers', 'putAlarm_notificationTriggers' - The alarm states that trigger a notification.
--
-- An alarm has the following possible states:
--
-- -   @ALARM@ - The metric is outside of the defined threshold.
--
-- -   @INSUFFICIENT_DATA@ - The alarm has just started, the metric is not
--     available, or not enough data is available for the metric to
--     determine the alarm state.
--
-- -   @OK@ - The metric is within the defined threshold.
--
-- When you specify a notification trigger, the @ALARM@ state must be
-- specified. The @INSUFFICIENT_DATA@ and @OK@ states can be specified in
-- addition to the @ALARM@ state.
--
-- -   If you specify @OK@ as an alarm trigger, a notification is sent when
--     the alarm switches from an @ALARM@ or @INSUFFICIENT_DATA@ alarm
--     state to an @OK@ state. This can be thought of as an /all clear/
--     alarm notification.
--
-- -   If you specify @INSUFFICIENT_DATA@ as the alarm trigger, a
--     notification is sent when the alarm switches from an @OK@ or @ALARM@
--     alarm state to an @INSUFFICIENT_DATA@ state.
--
-- The notification trigger defaults to @ALARM@ if you don\'t specify this
-- parameter.
--
-- 'notificationEnabled', 'putAlarm_notificationEnabled' - Indicates whether the alarm is enabled.
--
-- Notifications are enabled by default if you don\'t specify this
-- parameter.
--
-- 'treatMissingData', 'putAlarm_treatMissingData' - Sets how this alarm will handle missing data points.
--
-- An alarm can treat missing data in the following ways:
--
-- -   @breaching@ - Assume the missing data is not within the threshold.
--     Missing data counts towards the number of times the metric is not
--     within the threshold.
--
-- -   @notBreaching@ - Assume the missing data is within the threshold.
--     Missing data does not count towards the number of times the metric
--     is not within the threshold.
--
-- -   @ignore@ - Ignore the missing data. Maintains the current alarm
--     state.
--
-- -   @missing@ - Missing data is treated as missing.
--
-- If @treatMissingData@ is not specified, the default behavior of
-- @missing@ is used.
--
-- 'contactProtocols', 'putAlarm_contactProtocols' - The contact protocols to use for the alarm, such as @Email@, @SMS@ (text
-- messaging), or both.
--
-- A notification is sent via the specified contact protocol if
-- notifications are enabled for the alarm, and when the alarm is
-- triggered.
--
-- A notification is not sent if a contact protocol is not specified, if
-- the specified contact protocol is not configured in the AWS Region, or
-- if notifications are not enabled for the alarm using the
-- @notificationEnabled@ paramater.
--
-- Use the @CreateContactMethod@ action to configure a contact protocol in
-- an AWS Region.
--
-- 'alarmName', 'putAlarm_alarmName' - The name for the alarm. Specify the name of an existing alarm to update,
-- and overwrite the previous configuration of the alarm.
--
-- 'metricName', 'putAlarm_metricName' - The name of the metric to associate with the alarm.
--
-- You can configure up to two alarms per metric.
--
-- The following metrics are available for each resource type:
--
-- -   __Instances__: @BurstCapacityPercentage@, @BurstCapacityTime@,
--     @CPUUtilization@, @NetworkIn@, @NetworkOut@, @StatusCheckFailed@,
--     @StatusCheckFailed_Instance@, and @StatusCheckFailed_System@.
--
-- -   __Load balancers__: @ClientTLSNegotiationErrorCount@,
--     @HealthyHostCount@, @UnhealthyHostCount@, @HTTPCode_LB_4XX_Count@,
--     @HTTPCode_LB_5XX_Count@, @HTTPCode_Instance_2XX_Count@,
--     @HTTPCode_Instance_3XX_Count@, @HTTPCode_Instance_4XX_Count@,
--     @HTTPCode_Instance_5XX_Count@, @InstanceResponseTime@,
--     @RejectedConnectionCount@, and @RequestCount@.
--
-- -   __Relational databases__: @CPUUtilization@, @DatabaseConnections@,
--     @DiskQueueDepth@, @FreeStorageSpace@, @NetworkReceiveThroughput@,
--     and @NetworkTransmitThroughput@.
--
-- For more information about these metrics, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-resource-health-metrics#available-metrics Metrics available in Lightsail>.
--
-- 'monitoredResourceName', 'putAlarm_monitoredResourceName' - The name of the Lightsail resource that will be monitored.
--
-- Instances, load balancers, and relational databases are the only
-- Lightsail resources that can currently be monitored by alarms.
--
-- 'comparisonOperator', 'putAlarm_comparisonOperator' - The arithmetic operation to use when comparing the specified statistic
-- to the threshold. The specified statistic value is used as the first
-- operand.
--
-- 'threshold', 'putAlarm_threshold' - The value against which the specified statistic is compared.
--
-- 'evaluationPeriods', 'putAlarm_evaluationPeriods' - The number of most recent periods over which data is compared to the
-- specified threshold. If you are setting an \"M out of N\" alarm, this
-- value (@evaluationPeriods@) is the N.
--
-- If you are setting an alarm that requires that a number of consecutive
-- data points be breaching to trigger the alarm, this value specifies the
-- rolling period of time in which data points are evaluated.
--
-- Each evaluation period is five minutes long. For example, specify an
-- evaluation period of 24 to evaluate a metric over a rolling period of
-- two hours.
--
-- You can specify a minimum valuation period of 1 (5 minutes), and a
-- maximum evaluation period of 288 (24 hours).
newPutAlarm ::
  -- | 'alarmName'
  Core.Text ->
  -- | 'metricName'
  MetricName ->
  -- | 'monitoredResourceName'
  Core.Text ->
  -- | 'comparisonOperator'
  ComparisonOperator ->
  -- | 'threshold'
  Core.Double ->
  -- | 'evaluationPeriods'
  Core.Int ->
  PutAlarm
newPutAlarm
  pAlarmName_
  pMetricName_
  pMonitoredResourceName_
  pComparisonOperator_
  pThreshold_
  pEvaluationPeriods_ =
    PutAlarm'
      { datapointsToAlarm = Core.Nothing,
        notificationTriggers = Core.Nothing,
        notificationEnabled = Core.Nothing,
        treatMissingData = Core.Nothing,
        contactProtocols = Core.Nothing,
        alarmName = pAlarmName_,
        metricName = pMetricName_,
        monitoredResourceName = pMonitoredResourceName_,
        comparisonOperator = pComparisonOperator_,
        threshold = pThreshold_,
        evaluationPeriods = pEvaluationPeriods_
      }

-- | The number of data points that must be not within the specified
-- threshold to trigger the alarm. If you are setting an \"M out of N\"
-- alarm, this value (@datapointsToAlarm@) is the M.
putAlarm_datapointsToAlarm :: Lens.Lens' PutAlarm (Core.Maybe Core.Int)
putAlarm_datapointsToAlarm = Lens.lens (\PutAlarm' {datapointsToAlarm} -> datapointsToAlarm) (\s@PutAlarm' {} a -> s {datapointsToAlarm = a} :: PutAlarm)

-- | The alarm states that trigger a notification.
--
-- An alarm has the following possible states:
--
-- -   @ALARM@ - The metric is outside of the defined threshold.
--
-- -   @INSUFFICIENT_DATA@ - The alarm has just started, the metric is not
--     available, or not enough data is available for the metric to
--     determine the alarm state.
--
-- -   @OK@ - The metric is within the defined threshold.
--
-- When you specify a notification trigger, the @ALARM@ state must be
-- specified. The @INSUFFICIENT_DATA@ and @OK@ states can be specified in
-- addition to the @ALARM@ state.
--
-- -   If you specify @OK@ as an alarm trigger, a notification is sent when
--     the alarm switches from an @ALARM@ or @INSUFFICIENT_DATA@ alarm
--     state to an @OK@ state. This can be thought of as an /all clear/
--     alarm notification.
--
-- -   If you specify @INSUFFICIENT_DATA@ as the alarm trigger, a
--     notification is sent when the alarm switches from an @OK@ or @ALARM@
--     alarm state to an @INSUFFICIENT_DATA@ state.
--
-- The notification trigger defaults to @ALARM@ if you don\'t specify this
-- parameter.
putAlarm_notificationTriggers :: Lens.Lens' PutAlarm (Core.Maybe [AlarmState])
putAlarm_notificationTriggers = Lens.lens (\PutAlarm' {notificationTriggers} -> notificationTriggers) (\s@PutAlarm' {} a -> s {notificationTriggers = a} :: PutAlarm) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether the alarm is enabled.
--
-- Notifications are enabled by default if you don\'t specify this
-- parameter.
putAlarm_notificationEnabled :: Lens.Lens' PutAlarm (Core.Maybe Core.Bool)
putAlarm_notificationEnabled = Lens.lens (\PutAlarm' {notificationEnabled} -> notificationEnabled) (\s@PutAlarm' {} a -> s {notificationEnabled = a} :: PutAlarm)

-- | Sets how this alarm will handle missing data points.
--
-- An alarm can treat missing data in the following ways:
--
-- -   @breaching@ - Assume the missing data is not within the threshold.
--     Missing data counts towards the number of times the metric is not
--     within the threshold.
--
-- -   @notBreaching@ - Assume the missing data is within the threshold.
--     Missing data does not count towards the number of times the metric
--     is not within the threshold.
--
-- -   @ignore@ - Ignore the missing data. Maintains the current alarm
--     state.
--
-- -   @missing@ - Missing data is treated as missing.
--
-- If @treatMissingData@ is not specified, the default behavior of
-- @missing@ is used.
putAlarm_treatMissingData :: Lens.Lens' PutAlarm (Core.Maybe TreatMissingData)
putAlarm_treatMissingData = Lens.lens (\PutAlarm' {treatMissingData} -> treatMissingData) (\s@PutAlarm' {} a -> s {treatMissingData = a} :: PutAlarm)

-- | The contact protocols to use for the alarm, such as @Email@, @SMS@ (text
-- messaging), or both.
--
-- A notification is sent via the specified contact protocol if
-- notifications are enabled for the alarm, and when the alarm is
-- triggered.
--
-- A notification is not sent if a contact protocol is not specified, if
-- the specified contact protocol is not configured in the AWS Region, or
-- if notifications are not enabled for the alarm using the
-- @notificationEnabled@ paramater.
--
-- Use the @CreateContactMethod@ action to configure a contact protocol in
-- an AWS Region.
putAlarm_contactProtocols :: Lens.Lens' PutAlarm (Core.Maybe [ContactProtocol])
putAlarm_contactProtocols = Lens.lens (\PutAlarm' {contactProtocols} -> contactProtocols) (\s@PutAlarm' {} a -> s {contactProtocols = a} :: PutAlarm) Core.. Lens.mapping Lens._Coerce

-- | The name for the alarm. Specify the name of an existing alarm to update,
-- and overwrite the previous configuration of the alarm.
putAlarm_alarmName :: Lens.Lens' PutAlarm Core.Text
putAlarm_alarmName = Lens.lens (\PutAlarm' {alarmName} -> alarmName) (\s@PutAlarm' {} a -> s {alarmName = a} :: PutAlarm)

-- | The name of the metric to associate with the alarm.
--
-- You can configure up to two alarms per metric.
--
-- The following metrics are available for each resource type:
--
-- -   __Instances__: @BurstCapacityPercentage@, @BurstCapacityTime@,
--     @CPUUtilization@, @NetworkIn@, @NetworkOut@, @StatusCheckFailed@,
--     @StatusCheckFailed_Instance@, and @StatusCheckFailed_System@.
--
-- -   __Load balancers__: @ClientTLSNegotiationErrorCount@,
--     @HealthyHostCount@, @UnhealthyHostCount@, @HTTPCode_LB_4XX_Count@,
--     @HTTPCode_LB_5XX_Count@, @HTTPCode_Instance_2XX_Count@,
--     @HTTPCode_Instance_3XX_Count@, @HTTPCode_Instance_4XX_Count@,
--     @HTTPCode_Instance_5XX_Count@, @InstanceResponseTime@,
--     @RejectedConnectionCount@, and @RequestCount@.
--
-- -   __Relational databases__: @CPUUtilization@, @DatabaseConnections@,
--     @DiskQueueDepth@, @FreeStorageSpace@, @NetworkReceiveThroughput@,
--     and @NetworkTransmitThroughput@.
--
-- For more information about these metrics, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-resource-health-metrics#available-metrics Metrics available in Lightsail>.
putAlarm_metricName :: Lens.Lens' PutAlarm MetricName
putAlarm_metricName = Lens.lens (\PutAlarm' {metricName} -> metricName) (\s@PutAlarm' {} a -> s {metricName = a} :: PutAlarm)

-- | The name of the Lightsail resource that will be monitored.
--
-- Instances, load balancers, and relational databases are the only
-- Lightsail resources that can currently be monitored by alarms.
putAlarm_monitoredResourceName :: Lens.Lens' PutAlarm Core.Text
putAlarm_monitoredResourceName = Lens.lens (\PutAlarm' {monitoredResourceName} -> monitoredResourceName) (\s@PutAlarm' {} a -> s {monitoredResourceName = a} :: PutAlarm)

-- | The arithmetic operation to use when comparing the specified statistic
-- to the threshold. The specified statistic value is used as the first
-- operand.
putAlarm_comparisonOperator :: Lens.Lens' PutAlarm ComparisonOperator
putAlarm_comparisonOperator = Lens.lens (\PutAlarm' {comparisonOperator} -> comparisonOperator) (\s@PutAlarm' {} a -> s {comparisonOperator = a} :: PutAlarm)

-- | The value against which the specified statistic is compared.
putAlarm_threshold :: Lens.Lens' PutAlarm Core.Double
putAlarm_threshold = Lens.lens (\PutAlarm' {threshold} -> threshold) (\s@PutAlarm' {} a -> s {threshold = a} :: PutAlarm)

-- | The number of most recent periods over which data is compared to the
-- specified threshold. If you are setting an \"M out of N\" alarm, this
-- value (@evaluationPeriods@) is the N.
--
-- If you are setting an alarm that requires that a number of consecutive
-- data points be breaching to trigger the alarm, this value specifies the
-- rolling period of time in which data points are evaluated.
--
-- Each evaluation period is five minutes long. For example, specify an
-- evaluation period of 24 to evaluate a metric over a rolling period of
-- two hours.
--
-- You can specify a minimum valuation period of 1 (5 minutes), and a
-- maximum evaluation period of 288 (24 hours).
putAlarm_evaluationPeriods :: Lens.Lens' PutAlarm Core.Int
putAlarm_evaluationPeriods = Lens.lens (\PutAlarm' {evaluationPeriods} -> evaluationPeriods) (\s@PutAlarm' {} a -> s {evaluationPeriods = a} :: PutAlarm)

instance Core.AWSRequest PutAlarm where
  type AWSResponse PutAlarm = PutAlarmResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAlarmResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutAlarm

instance Core.NFData PutAlarm

instance Core.ToHeaders PutAlarm where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Lightsail_20161128.PutAlarm" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutAlarm where
  toJSON PutAlarm' {..} =
    Core.object
      ( Core.catMaybes
          [ ("datapointsToAlarm" Core..=)
              Core.<$> datapointsToAlarm,
            ("notificationTriggers" Core..=)
              Core.<$> notificationTriggers,
            ("notificationEnabled" Core..=)
              Core.<$> notificationEnabled,
            ("treatMissingData" Core..=)
              Core.<$> treatMissingData,
            ("contactProtocols" Core..=)
              Core.<$> contactProtocols,
            Core.Just ("alarmName" Core..= alarmName),
            Core.Just ("metricName" Core..= metricName),
            Core.Just
              ( "monitoredResourceName"
                  Core..= monitoredResourceName
              ),
            Core.Just
              ("comparisonOperator" Core..= comparisonOperator),
            Core.Just ("threshold" Core..= threshold),
            Core.Just
              ("evaluationPeriods" Core..= evaluationPeriods)
          ]
      )

instance Core.ToPath PutAlarm where
  toPath = Core.const "/"

instance Core.ToQuery PutAlarm where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutAlarmResponse' smart constructor.
data PutAlarmResponse = PutAlarmResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutAlarmResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'putAlarmResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'putAlarmResponse_httpStatus' - The response's http status code.
newPutAlarmResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutAlarmResponse
newPutAlarmResponse pHttpStatus_ =
  PutAlarmResponse'
    { operations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
putAlarmResponse_operations :: Lens.Lens' PutAlarmResponse (Core.Maybe [Operation])
putAlarmResponse_operations = Lens.lens (\PutAlarmResponse' {operations} -> operations) (\s@PutAlarmResponse' {} a -> s {operations = a} :: PutAlarmResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
putAlarmResponse_httpStatus :: Lens.Lens' PutAlarmResponse Core.Int
putAlarmResponse_httpStatus = Lens.lens (\PutAlarmResponse' {httpStatus} -> httpStatus) (\s@PutAlarmResponse' {} a -> s {httpStatus = a} :: PutAlarmResponse)

instance Core.NFData PutAlarmResponse
