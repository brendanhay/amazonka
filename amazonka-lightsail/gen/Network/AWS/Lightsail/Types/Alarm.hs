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
import qualified Network.AWS.Prelude as Prelude

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
    datapointsToAlarm :: Prelude.Maybe Prelude.Int,
    -- | The value against which the specified statistic is compared.
    threshold :: Prelude.Maybe Prelude.Double,
    -- | The arithmetic operation used when comparing the specified statistic and
    -- threshold.
    comparisonOperator :: Prelude.Maybe ComparisonOperator,
    -- | An object that lists information about the resource monitored by the
    -- alarm.
    monitoredResourceInfo :: Prelude.Maybe MonitoredResourceInfo,
    -- | The alarm states that trigger a notification.
    notificationTriggers :: Prelude.Maybe [AlarmState],
    -- | The unit of the metric associated with the alarm.
    unit :: Prelude.Maybe MetricUnit,
    -- | The name of the metric associated with the alarm.
    metricName :: Prelude.Maybe MetricName,
    -- | Indicates whether the alarm is enabled.
    notificationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The timestamp when the alarm was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the alarm.
    arn :: Prelude.Maybe Prelude.Text,
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
    treatMissingData :: Prelude.Maybe TreatMissingData,
    -- | The Lightsail resource type (e.g., @Alarm@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The support code. Include this code in your email to support when you
    -- have questions about your Lightsail alarm. This code enables our support
    -- team to look up your Lightsail information more easily.
    supportCode :: Prelude.Maybe Prelude.Text,
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
    state :: Prelude.Maybe AlarmState,
    -- | The name of the alarm.
    name :: Prelude.Maybe Prelude.Text,
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
    statistic :: Prelude.Maybe MetricStatistic,
    -- | The number of periods over which data is compared to the specified
    -- threshold.
    evaluationPeriods :: Prelude.Maybe Prelude.Int,
    -- | The period, in seconds, over which the statistic is applied.
    period :: Prelude.Maybe Prelude.Natural,
    -- | An object that lists information about the location of the alarm.
    location :: Prelude.Maybe ResourceLocation,
    -- | The contact protocols for the alarm, such as @Email@, @SMS@ (text
    -- messaging), or both.
    contactProtocols :: Prelude.Maybe [ContactProtocol]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { datapointsToAlarm = Prelude.Nothing,
      threshold = Prelude.Nothing,
      comparisonOperator = Prelude.Nothing,
      monitoredResourceInfo = Prelude.Nothing,
      notificationTriggers = Prelude.Nothing,
      unit = Prelude.Nothing,
      metricName = Prelude.Nothing,
      notificationEnabled = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      treatMissingData = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      statistic = Prelude.Nothing,
      evaluationPeriods = Prelude.Nothing,
      period = Prelude.Nothing,
      location = Prelude.Nothing,
      contactProtocols = Prelude.Nothing
    }

-- | The number of data points that must not within the specified threshold
-- to trigger the alarm.
alarm_datapointsToAlarm :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Int)
alarm_datapointsToAlarm = Lens.lens (\Alarm' {datapointsToAlarm} -> datapointsToAlarm) (\s@Alarm' {} a -> s {datapointsToAlarm = a} :: Alarm)

-- | The value against which the specified statistic is compared.
alarm_threshold :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Double)
alarm_threshold = Lens.lens (\Alarm' {threshold} -> threshold) (\s@Alarm' {} a -> s {threshold = a} :: Alarm)

-- | The arithmetic operation used when comparing the specified statistic and
-- threshold.
alarm_comparisonOperator :: Lens.Lens' Alarm (Prelude.Maybe ComparisonOperator)
alarm_comparisonOperator = Lens.lens (\Alarm' {comparisonOperator} -> comparisonOperator) (\s@Alarm' {} a -> s {comparisonOperator = a} :: Alarm)

-- | An object that lists information about the resource monitored by the
-- alarm.
alarm_monitoredResourceInfo :: Lens.Lens' Alarm (Prelude.Maybe MonitoredResourceInfo)
alarm_monitoredResourceInfo = Lens.lens (\Alarm' {monitoredResourceInfo} -> monitoredResourceInfo) (\s@Alarm' {} a -> s {monitoredResourceInfo = a} :: Alarm)

-- | The alarm states that trigger a notification.
alarm_notificationTriggers :: Lens.Lens' Alarm (Prelude.Maybe [AlarmState])
alarm_notificationTriggers = Lens.lens (\Alarm' {notificationTriggers} -> notificationTriggers) (\s@Alarm' {} a -> s {notificationTriggers = a} :: Alarm) Prelude.. Lens.mapping Lens._Coerce

-- | The unit of the metric associated with the alarm.
alarm_unit :: Lens.Lens' Alarm (Prelude.Maybe MetricUnit)
alarm_unit = Lens.lens (\Alarm' {unit} -> unit) (\s@Alarm' {} a -> s {unit = a} :: Alarm)

-- | The name of the metric associated with the alarm.
alarm_metricName :: Lens.Lens' Alarm (Prelude.Maybe MetricName)
alarm_metricName = Lens.lens (\Alarm' {metricName} -> metricName) (\s@Alarm' {} a -> s {metricName = a} :: Alarm)

-- | Indicates whether the alarm is enabled.
alarm_notificationEnabled :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Bool)
alarm_notificationEnabled = Lens.lens (\Alarm' {notificationEnabled} -> notificationEnabled) (\s@Alarm' {} a -> s {notificationEnabled = a} :: Alarm)

-- | The timestamp when the alarm was created.
alarm_createdAt :: Lens.Lens' Alarm (Prelude.Maybe Prelude.UTCTime)
alarm_createdAt = Lens.lens (\Alarm' {createdAt} -> createdAt) (\s@Alarm' {} a -> s {createdAt = a} :: Alarm) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the alarm.
alarm_arn :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Text)
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
alarm_treatMissingData :: Lens.Lens' Alarm (Prelude.Maybe TreatMissingData)
alarm_treatMissingData = Lens.lens (\Alarm' {treatMissingData} -> treatMissingData) (\s@Alarm' {} a -> s {treatMissingData = a} :: Alarm)

-- | The Lightsail resource type (e.g., @Alarm@).
alarm_resourceType :: Lens.Lens' Alarm (Prelude.Maybe ResourceType)
alarm_resourceType = Lens.lens (\Alarm' {resourceType} -> resourceType) (\s@Alarm' {} a -> s {resourceType = a} :: Alarm)

-- | The support code. Include this code in your email to support when you
-- have questions about your Lightsail alarm. This code enables our support
-- team to look up your Lightsail information more easily.
alarm_supportCode :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Text)
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
alarm_state :: Lens.Lens' Alarm (Prelude.Maybe AlarmState)
alarm_state = Lens.lens (\Alarm' {state} -> state) (\s@Alarm' {} a -> s {state = a} :: Alarm)

-- | The name of the alarm.
alarm_name :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Text)
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
alarm_statistic :: Lens.Lens' Alarm (Prelude.Maybe MetricStatistic)
alarm_statistic = Lens.lens (\Alarm' {statistic} -> statistic) (\s@Alarm' {} a -> s {statistic = a} :: Alarm)

-- | The number of periods over which data is compared to the specified
-- threshold.
alarm_evaluationPeriods :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Int)
alarm_evaluationPeriods = Lens.lens (\Alarm' {evaluationPeriods} -> evaluationPeriods) (\s@Alarm' {} a -> s {evaluationPeriods = a} :: Alarm)

-- | The period, in seconds, over which the statistic is applied.
alarm_period :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Natural)
alarm_period = Lens.lens (\Alarm' {period} -> period) (\s@Alarm' {} a -> s {period = a} :: Alarm)

-- | An object that lists information about the location of the alarm.
alarm_location :: Lens.Lens' Alarm (Prelude.Maybe ResourceLocation)
alarm_location = Lens.lens (\Alarm' {location} -> location) (\s@Alarm' {} a -> s {location = a} :: Alarm)

-- | The contact protocols for the alarm, such as @Email@, @SMS@ (text
-- messaging), or both.
alarm_contactProtocols :: Lens.Lens' Alarm (Prelude.Maybe [ContactProtocol])
alarm_contactProtocols = Lens.lens (\Alarm' {contactProtocols} -> contactProtocols) (\s@Alarm' {} a -> s {contactProtocols = a} :: Alarm) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON Alarm where
  parseJSON =
    Core.withObject
      "Alarm"
      ( \x ->
          Alarm'
            Prelude.<$> (x Core..:? "datapointsToAlarm")
            Prelude.<*> (x Core..:? "threshold")
            Prelude.<*> (x Core..:? "comparisonOperator")
            Prelude.<*> (x Core..:? "monitoredResourceInfo")
            Prelude.<*> ( x Core..:? "notificationTriggers"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "unit")
            Prelude.<*> (x Core..:? "metricName")
            Prelude.<*> (x Core..:? "notificationEnabled")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "treatMissingData")
            Prelude.<*> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "supportCode")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "statistic")
            Prelude.<*> (x Core..:? "evaluationPeriods")
            Prelude.<*> (x Core..:? "period")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> ( x Core..:? "contactProtocols"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Alarm

instance Prelude.NFData Alarm
