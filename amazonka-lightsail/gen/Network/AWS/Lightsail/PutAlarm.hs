{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutAlarm' smart constructor.
data PutAlarm = PutAlarm'
  { -- | The number of data points that must be not within the specified
    -- threshold to trigger the alarm. If you are setting an \"M out of N\"
    -- alarm, this value (@datapointsToAlarm@) is the M.
    datapointsToAlarm :: Prelude.Maybe Prelude.Int,
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
    notificationTriggers :: Prelude.Maybe [AlarmState],
    -- | Indicates whether the alarm is enabled.
    --
    -- Notifications are enabled by default if you don\'t specify this
    -- parameter.
    notificationEnabled :: Prelude.Maybe Prelude.Bool,
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
    treatMissingData :: Prelude.Maybe TreatMissingData,
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
    contactProtocols :: Prelude.Maybe [ContactProtocol],
    -- | The name for the alarm. Specify the name of an existing alarm to update,
    -- and overwrite the previous configuration of the alarm.
    alarmName :: Prelude.Text,
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
    monitoredResourceName :: Prelude.Text,
    -- | The arithmetic operation to use when comparing the specified statistic
    -- to the threshold. The specified statistic value is used as the first
    -- operand.
    comparisonOperator :: ComparisonOperator,
    -- | The value against which the specified statistic is compared.
    threshold :: Prelude.Double,
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
    evaluationPeriods :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'metricName'
  MetricName ->
  -- | 'monitoredResourceName'
  Prelude.Text ->
  -- | 'comparisonOperator'
  ComparisonOperator ->
  -- | 'threshold'
  Prelude.Double ->
  -- | 'evaluationPeriods'
  Prelude.Int ->
  PutAlarm
newPutAlarm
  pAlarmName_
  pMetricName_
  pMonitoredResourceName_
  pComparisonOperator_
  pThreshold_
  pEvaluationPeriods_ =
    PutAlarm'
      { datapointsToAlarm = Prelude.Nothing,
        notificationTriggers = Prelude.Nothing,
        notificationEnabled = Prelude.Nothing,
        treatMissingData = Prelude.Nothing,
        contactProtocols = Prelude.Nothing,
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
putAlarm_datapointsToAlarm :: Lens.Lens' PutAlarm (Prelude.Maybe Prelude.Int)
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
putAlarm_notificationTriggers :: Lens.Lens' PutAlarm (Prelude.Maybe [AlarmState])
putAlarm_notificationTriggers = Lens.lens (\PutAlarm' {notificationTriggers} -> notificationTriggers) (\s@PutAlarm' {} a -> s {notificationTriggers = a} :: PutAlarm) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates whether the alarm is enabled.
--
-- Notifications are enabled by default if you don\'t specify this
-- parameter.
putAlarm_notificationEnabled :: Lens.Lens' PutAlarm (Prelude.Maybe Prelude.Bool)
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
putAlarm_treatMissingData :: Lens.Lens' PutAlarm (Prelude.Maybe TreatMissingData)
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
putAlarm_contactProtocols :: Lens.Lens' PutAlarm (Prelude.Maybe [ContactProtocol])
putAlarm_contactProtocols = Lens.lens (\PutAlarm' {contactProtocols} -> contactProtocols) (\s@PutAlarm' {} a -> s {contactProtocols = a} :: PutAlarm) Prelude.. Lens.mapping Prelude._Coerce

-- | The name for the alarm. Specify the name of an existing alarm to update,
-- and overwrite the previous configuration of the alarm.
putAlarm_alarmName :: Lens.Lens' PutAlarm Prelude.Text
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
putAlarm_monitoredResourceName :: Lens.Lens' PutAlarm Prelude.Text
putAlarm_monitoredResourceName = Lens.lens (\PutAlarm' {monitoredResourceName} -> monitoredResourceName) (\s@PutAlarm' {} a -> s {monitoredResourceName = a} :: PutAlarm)

-- | The arithmetic operation to use when comparing the specified statistic
-- to the threshold. The specified statistic value is used as the first
-- operand.
putAlarm_comparisonOperator :: Lens.Lens' PutAlarm ComparisonOperator
putAlarm_comparisonOperator = Lens.lens (\PutAlarm' {comparisonOperator} -> comparisonOperator) (\s@PutAlarm' {} a -> s {comparisonOperator = a} :: PutAlarm)

-- | The value against which the specified statistic is compared.
putAlarm_threshold :: Lens.Lens' PutAlarm Prelude.Double
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
putAlarm_evaluationPeriods :: Lens.Lens' PutAlarm Prelude.Int
putAlarm_evaluationPeriods = Lens.lens (\PutAlarm' {evaluationPeriods} -> evaluationPeriods) (\s@PutAlarm' {} a -> s {evaluationPeriods = a} :: PutAlarm)

instance Prelude.AWSRequest PutAlarm where
  type Rs PutAlarm = PutAlarmResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAlarmResponse'
            Prelude.<$> ( x Prelude..?> "operations"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAlarm

instance Prelude.NFData PutAlarm

instance Prelude.ToHeaders PutAlarm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Lightsail_20161128.PutAlarm" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutAlarm where
  toJSON PutAlarm' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("datapointsToAlarm" Prelude..=)
              Prelude.<$> datapointsToAlarm,
            ("notificationTriggers" Prelude..=)
              Prelude.<$> notificationTriggers,
            ("notificationEnabled" Prelude..=)
              Prelude.<$> notificationEnabled,
            ("treatMissingData" Prelude..=)
              Prelude.<$> treatMissingData,
            ("contactProtocols" Prelude..=)
              Prelude.<$> contactProtocols,
            Prelude.Just ("alarmName" Prelude..= alarmName),
            Prelude.Just ("metricName" Prelude..= metricName),
            Prelude.Just
              ( "monitoredResourceName"
                  Prelude..= monitoredResourceName
              ),
            Prelude.Just
              ("comparisonOperator" Prelude..= comparisonOperator),
            Prelude.Just ("threshold" Prelude..= threshold),
            Prelude.Just
              ("evaluationPeriods" Prelude..= evaluationPeriods)
          ]
      )

instance Prelude.ToPath PutAlarm where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutAlarm where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAlarmResponse' smart constructor.
data PutAlarmResponse = PutAlarmResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  PutAlarmResponse
newPutAlarmResponse pHttpStatus_ =
  PutAlarmResponse'
    { operations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
putAlarmResponse_operations :: Lens.Lens' PutAlarmResponse (Prelude.Maybe [Operation])
putAlarmResponse_operations = Lens.lens (\PutAlarmResponse' {operations} -> operations) (\s@PutAlarmResponse' {} a -> s {operations = a} :: PutAlarmResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
putAlarmResponse_httpStatus :: Lens.Lens' PutAlarmResponse Prelude.Int
putAlarmResponse_httpStatus = Lens.lens (\PutAlarmResponse' {httpStatus} -> httpStatus) (\s@PutAlarmResponse' {} a -> s {httpStatus = a} :: PutAlarmResponse)

instance Prelude.NFData PutAlarmResponse
