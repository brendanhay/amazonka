{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Alarm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Alarm where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
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

-- | Describes an alarm.
--
-- An alarm is a way to monitor your Amazon Lightsail resource metrics. For
-- more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail>.
--
-- /See:/ 'newAlarm' smart constructor.
data Alarm = Alarm'
  { -- | The number of data points that must not within the specified threshold
    -- to trigger the alarm.
    datapointsToAlarm :: Core.Maybe Core.Int,
    -- | The value against which the specified statistic is compared.
    threshold :: Core.Maybe Core.Double,
    -- | The arithmetic operation used when comparing the specified statistic and
    -- threshold.
    comparisonOperator :: Core.Maybe ComparisonOperator,
    -- | An object that lists information about the resource monitored by the
    -- alarm.
    monitoredResourceInfo :: Core.Maybe MonitoredResourceInfo,
    -- | The alarm states that trigger a notification.
    notificationTriggers :: Core.Maybe [AlarmState],
    -- | The unit of the metric associated with the alarm.
    unit :: Core.Maybe MetricUnit,
    -- | The name of the metric associated with the alarm.
    metricName :: Core.Maybe MetricName,
    -- | Indicates whether the alarm is enabled.
    notificationEnabled :: Core.Maybe Core.Bool,
    -- | The timestamp when the alarm was created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the alarm.
    arn :: Core.Maybe Core.Text,
    -- | Specifies how the alarm handles missing data points.
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
    treatMissingData :: Core.Maybe TreatMissingData,
    -- | The Lightsail resource type (e.g., @Alarm@).
    resourceType :: Core.Maybe ResourceType,
    -- | The support code. Include this code in your email to support when you
    -- have questions about your Lightsail alarm. This code enables our support
    -- team to look up your Lightsail information more easily.
    supportCode :: Core.Maybe Core.Text,
    -- | The current state of the alarm.
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
    state :: Core.Maybe AlarmState,
    -- | The name of the alarm.
    name :: Core.Maybe Core.Text,
    -- | The statistic for the metric associated with the alarm.
    --
    -- The following statistics are available:
    --
    -- -   @Minimum@ - The lowest value observed during the specified period.
    --     Use this value to determine low volumes of activity for your
    --     application.
    --
    -- -   @Maximum@ - The highest value observed during the specified period.
    --     Use this value to determine high volumes of activity for your
    --     application.
    --
    -- -   @Sum@ - All values submitted for the matching metric added together.
    --     You can use this statistic to determine the total volume of a
    --     metric.
    --
    -- -   @Average@ - The value of Sum \/ SampleCount during the specified
    --     period. By comparing this statistic with the Minimum and Maximum
    --     values, you can determine the full scope of a metric and how close
    --     the average use is to the Minimum and Maximum values. This
    --     comparison helps you to know when to increase or decrease your
    --     resources.
    --
    -- -   @SampleCount@ - The count, or number, of data points used for the
    --     statistical calculation.
    statistic :: Core.Maybe MetricStatistic,
    -- | The number of periods over which data is compared to the specified
    -- threshold.
    evaluationPeriods :: Core.Maybe Core.Int,
    -- | The period, in seconds, over which the statistic is applied.
    period :: Core.Maybe Core.Natural,
    -- | An object that lists information about the location of the alarm.
    location :: Core.Maybe ResourceLocation,
    -- | The contact protocols for the alarm, such as @Email@, @SMS@ (text
    -- messaging), or both.
    contactProtocols :: Core.Maybe [ContactProtocol]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Alarm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datapointsToAlarm', 'alarm_datapointsToAlarm' - The number of data points that must not within the specified threshold
-- to trigger the alarm.
--
-- 'threshold', 'alarm_threshold' - The value against which the specified statistic is compared.
--
-- 'comparisonOperator', 'alarm_comparisonOperator' - The arithmetic operation used when comparing the specified statistic and
-- threshold.
--
-- 'monitoredResourceInfo', 'alarm_monitoredResourceInfo' - An object that lists information about the resource monitored by the
-- alarm.
--
-- 'notificationTriggers', 'alarm_notificationTriggers' - The alarm states that trigger a notification.
--
-- 'unit', 'alarm_unit' - The unit of the metric associated with the alarm.
--
-- 'metricName', 'alarm_metricName' - The name of the metric associated with the alarm.
--
-- 'notificationEnabled', 'alarm_notificationEnabled' - Indicates whether the alarm is enabled.
--
-- 'createdAt', 'alarm_createdAt' - The timestamp when the alarm was created.
--
-- 'arn', 'alarm_arn' - The Amazon Resource Name (ARN) of the alarm.
--
-- 'treatMissingData', 'alarm_treatMissingData' - Specifies how the alarm handles missing data points.
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
-- 'resourceType', 'alarm_resourceType' - The Lightsail resource type (e.g., @Alarm@).
--
-- 'supportCode', 'alarm_supportCode' - The support code. Include this code in your email to support when you
-- have questions about your Lightsail alarm. This code enables our support
-- team to look up your Lightsail information more easily.
--
-- 'state', 'alarm_state' - The current state of the alarm.
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
-- 'name', 'alarm_name' - The name of the alarm.
--
-- 'statistic', 'alarm_statistic' - The statistic for the metric associated with the alarm.
--
-- The following statistics are available:
--
-- -   @Minimum@ - The lowest value observed during the specified period.
--     Use this value to determine low volumes of activity for your
--     application.
--
-- -   @Maximum@ - The highest value observed during the specified period.
--     Use this value to determine high volumes of activity for your
--     application.
--
-- -   @Sum@ - All values submitted for the matching metric added together.
--     You can use this statistic to determine the total volume of a
--     metric.
--
-- -   @Average@ - The value of Sum \/ SampleCount during the specified
--     period. By comparing this statistic with the Minimum and Maximum
--     values, you can determine the full scope of a metric and how close
--     the average use is to the Minimum and Maximum values. This
--     comparison helps you to know when to increase or decrease your
--     resources.
--
-- -   @SampleCount@ - The count, or number, of data points used for the
--     statistical calculation.
--
-- 'evaluationPeriods', 'alarm_evaluationPeriods' - The number of periods over which data is compared to the specified
-- threshold.
--
-- 'period', 'alarm_period' - The period, in seconds, over which the statistic is applied.
--
-- 'location', 'alarm_location' - An object that lists information about the location of the alarm.
--
-- 'contactProtocols', 'alarm_contactProtocols' - The contact protocols for the alarm, such as @Email@, @SMS@ (text
-- messaging), or both.
newAlarm ::
  Alarm
newAlarm =
  Alarm'
    { datapointsToAlarm = Core.Nothing,
      threshold = Core.Nothing,
      comparisonOperator = Core.Nothing,
      monitoredResourceInfo = Core.Nothing,
      notificationTriggers = Core.Nothing,
      unit = Core.Nothing,
      metricName = Core.Nothing,
      notificationEnabled = Core.Nothing,
      createdAt = Core.Nothing,
      arn = Core.Nothing,
      treatMissingData = Core.Nothing,
      resourceType = Core.Nothing,
      supportCode = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      statistic = Core.Nothing,
      evaluationPeriods = Core.Nothing,
      period = Core.Nothing,
      location = Core.Nothing,
      contactProtocols = Core.Nothing
    }

-- | The number of data points that must not within the specified threshold
-- to trigger the alarm.
alarm_datapointsToAlarm :: Lens.Lens' Alarm (Core.Maybe Core.Int)
alarm_datapointsToAlarm = Lens.lens (\Alarm' {datapointsToAlarm} -> datapointsToAlarm) (\s@Alarm' {} a -> s {datapointsToAlarm = a} :: Alarm)

-- | The value against which the specified statistic is compared.
alarm_threshold :: Lens.Lens' Alarm (Core.Maybe Core.Double)
alarm_threshold = Lens.lens (\Alarm' {threshold} -> threshold) (\s@Alarm' {} a -> s {threshold = a} :: Alarm)

-- | The arithmetic operation used when comparing the specified statistic and
-- threshold.
alarm_comparisonOperator :: Lens.Lens' Alarm (Core.Maybe ComparisonOperator)
alarm_comparisonOperator = Lens.lens (\Alarm' {comparisonOperator} -> comparisonOperator) (\s@Alarm' {} a -> s {comparisonOperator = a} :: Alarm)

-- | An object that lists information about the resource monitored by the
-- alarm.
alarm_monitoredResourceInfo :: Lens.Lens' Alarm (Core.Maybe MonitoredResourceInfo)
alarm_monitoredResourceInfo = Lens.lens (\Alarm' {monitoredResourceInfo} -> monitoredResourceInfo) (\s@Alarm' {} a -> s {monitoredResourceInfo = a} :: Alarm)

-- | The alarm states that trigger a notification.
alarm_notificationTriggers :: Lens.Lens' Alarm (Core.Maybe [AlarmState])
alarm_notificationTriggers = Lens.lens (\Alarm' {notificationTriggers} -> notificationTriggers) (\s@Alarm' {} a -> s {notificationTriggers = a} :: Alarm) Core.. Lens.mapping Lens._Coerce

-- | The unit of the metric associated with the alarm.
alarm_unit :: Lens.Lens' Alarm (Core.Maybe MetricUnit)
alarm_unit = Lens.lens (\Alarm' {unit} -> unit) (\s@Alarm' {} a -> s {unit = a} :: Alarm)

-- | The name of the metric associated with the alarm.
alarm_metricName :: Lens.Lens' Alarm (Core.Maybe MetricName)
alarm_metricName = Lens.lens (\Alarm' {metricName} -> metricName) (\s@Alarm' {} a -> s {metricName = a} :: Alarm)

-- | Indicates whether the alarm is enabled.
alarm_notificationEnabled :: Lens.Lens' Alarm (Core.Maybe Core.Bool)
alarm_notificationEnabled = Lens.lens (\Alarm' {notificationEnabled} -> notificationEnabled) (\s@Alarm' {} a -> s {notificationEnabled = a} :: Alarm)

-- | The timestamp when the alarm was created.
alarm_createdAt :: Lens.Lens' Alarm (Core.Maybe Core.UTCTime)
alarm_createdAt = Lens.lens (\Alarm' {createdAt} -> createdAt) (\s@Alarm' {} a -> s {createdAt = a} :: Alarm) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the alarm.
alarm_arn :: Lens.Lens' Alarm (Core.Maybe Core.Text)
alarm_arn = Lens.lens (\Alarm' {arn} -> arn) (\s@Alarm' {} a -> s {arn = a} :: Alarm)

-- | Specifies how the alarm handles missing data points.
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
alarm_treatMissingData :: Lens.Lens' Alarm (Core.Maybe TreatMissingData)
alarm_treatMissingData = Lens.lens (\Alarm' {treatMissingData} -> treatMissingData) (\s@Alarm' {} a -> s {treatMissingData = a} :: Alarm)

-- | The Lightsail resource type (e.g., @Alarm@).
alarm_resourceType :: Lens.Lens' Alarm (Core.Maybe ResourceType)
alarm_resourceType = Lens.lens (\Alarm' {resourceType} -> resourceType) (\s@Alarm' {} a -> s {resourceType = a} :: Alarm)

-- | The support code. Include this code in your email to support when you
-- have questions about your Lightsail alarm. This code enables our support
-- team to look up your Lightsail information more easily.
alarm_supportCode :: Lens.Lens' Alarm (Core.Maybe Core.Text)
alarm_supportCode = Lens.lens (\Alarm' {supportCode} -> supportCode) (\s@Alarm' {} a -> s {supportCode = a} :: Alarm)

-- | The current state of the alarm.
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
alarm_state :: Lens.Lens' Alarm (Core.Maybe AlarmState)
alarm_state = Lens.lens (\Alarm' {state} -> state) (\s@Alarm' {} a -> s {state = a} :: Alarm)

-- | The name of the alarm.
alarm_name :: Lens.Lens' Alarm (Core.Maybe Core.Text)
alarm_name = Lens.lens (\Alarm' {name} -> name) (\s@Alarm' {} a -> s {name = a} :: Alarm)

-- | The statistic for the metric associated with the alarm.
--
-- The following statistics are available:
--
-- -   @Minimum@ - The lowest value observed during the specified period.
--     Use this value to determine low volumes of activity for your
--     application.
--
-- -   @Maximum@ - The highest value observed during the specified period.
--     Use this value to determine high volumes of activity for your
--     application.
--
-- -   @Sum@ - All values submitted for the matching metric added together.
--     You can use this statistic to determine the total volume of a
--     metric.
--
-- -   @Average@ - The value of Sum \/ SampleCount during the specified
--     period. By comparing this statistic with the Minimum and Maximum
--     values, you can determine the full scope of a metric and how close
--     the average use is to the Minimum and Maximum values. This
--     comparison helps you to know when to increase or decrease your
--     resources.
--
-- -   @SampleCount@ - The count, or number, of data points used for the
--     statistical calculation.
alarm_statistic :: Lens.Lens' Alarm (Core.Maybe MetricStatistic)
alarm_statistic = Lens.lens (\Alarm' {statistic} -> statistic) (\s@Alarm' {} a -> s {statistic = a} :: Alarm)

-- | The number of periods over which data is compared to the specified
-- threshold.
alarm_evaluationPeriods :: Lens.Lens' Alarm (Core.Maybe Core.Int)
alarm_evaluationPeriods = Lens.lens (\Alarm' {evaluationPeriods} -> evaluationPeriods) (\s@Alarm' {} a -> s {evaluationPeriods = a} :: Alarm)

-- | The period, in seconds, over which the statistic is applied.
alarm_period :: Lens.Lens' Alarm (Core.Maybe Core.Natural)
alarm_period = Lens.lens (\Alarm' {period} -> period) (\s@Alarm' {} a -> s {period = a} :: Alarm)

-- | An object that lists information about the location of the alarm.
alarm_location :: Lens.Lens' Alarm (Core.Maybe ResourceLocation)
alarm_location = Lens.lens (\Alarm' {location} -> location) (\s@Alarm' {} a -> s {location = a} :: Alarm)

-- | The contact protocols for the alarm, such as @Email@, @SMS@ (text
-- messaging), or both.
alarm_contactProtocols :: Lens.Lens' Alarm (Core.Maybe [ContactProtocol])
alarm_contactProtocols = Lens.lens (\Alarm' {contactProtocols} -> contactProtocols) (\s@Alarm' {} a -> s {contactProtocols = a} :: Alarm) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON Alarm where
  parseJSON =
    Core.withObject
      "Alarm"
      ( \x ->
          Alarm'
            Core.<$> (x Core..:? "datapointsToAlarm")
            Core.<*> (x Core..:? "threshold")
            Core.<*> (x Core..:? "comparisonOperator")
            Core.<*> (x Core..:? "monitoredResourceInfo")
            Core.<*> ( x Core..:? "notificationTriggers"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "unit")
            Core.<*> (x Core..:? "metricName")
            Core.<*> (x Core..:? "notificationEnabled")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "treatMissingData")
            Core.<*> (x Core..:? "resourceType")
            Core.<*> (x Core..:? "supportCode")
            Core.<*> (x Core..:? "state")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "statistic")
            Core.<*> (x Core..:? "evaluationPeriods")
            Core.<*> (x Core..:? "period")
            Core.<*> (x Core..:? "location")
            Core.<*> (x Core..:? "contactProtocols" Core..!= Core.mempty)
      )

instance Core.Hashable Alarm

instance Core.NFData Alarm
