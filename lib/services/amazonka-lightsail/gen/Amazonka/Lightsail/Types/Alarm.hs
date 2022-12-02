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
-- Module      : Amazonka.Lightsail.Types.Alarm
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.Alarm where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.AlarmState
import Amazonka.Lightsail.Types.ComparisonOperator
import Amazonka.Lightsail.Types.ContactProtocol
import Amazonka.Lightsail.Types.MetricName
import Amazonka.Lightsail.Types.MetricStatistic
import Amazonka.Lightsail.Types.MetricUnit
import Amazonka.Lightsail.Types.MonitoredResourceInfo
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import Amazonka.Lightsail.Types.TreatMissingData
import qualified Amazonka.Prelude as Prelude

-- | Describes an alarm.
--
-- An alarm is a way to monitor your Lightsail resource metrics. For more
-- information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail>.
--
-- /See:/ 'newAlarm' smart constructor.
data Alarm = Alarm'
  { -- | The Lightsail resource type (e.g., @Alarm@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The name of the alarm.
    name :: Prelude.Maybe Prelude.Text,
    -- | The period, in seconds, over which the statistic is applied.
    period :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the alarm.
    arn :: Prelude.Maybe Prelude.Text,
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
    -- | The number of periods over which data is compared to the specified
    -- threshold.
    evaluationPeriods :: Prelude.Maybe Prelude.Int,
    -- | The number of data points that must not within the specified threshold
    -- to trigger the alarm.
    datapointsToAlarm :: Prelude.Maybe Prelude.Int,
    -- | An object that lists information about the location of the alarm.
    location :: Prelude.Maybe ResourceLocation,
    -- | Indicates whether the alarm is enabled.
    notificationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the metric associated with the alarm.
    metricName :: Prelude.Maybe MetricName,
    -- | An object that lists information about the resource monitored by the
    -- alarm.
    monitoredResourceInfo :: Prelude.Maybe MonitoredResourceInfo,
    -- | The value against which the specified statistic is compared.
    threshold :: Prelude.Maybe Prelude.Double,
    -- | The contact protocols for the alarm, such as @Email@, @SMS@ (text
    -- messaging), or both.
    contactProtocols :: Prelude.Maybe [ContactProtocol],
    -- | The arithmetic operation used when comparing the specified statistic and
    -- threshold.
    comparisonOperator :: Prelude.Maybe ComparisonOperator,
    -- | The support code. Include this code in your email to support when you
    -- have questions about your Lightsail alarm. This code enables our support
    -- team to look up your Lightsail information more easily.
    supportCode :: Prelude.Maybe Prelude.Text,
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
    -- | The unit of the metric associated with the alarm.
    unit :: Prelude.Maybe MetricUnit,
    -- | The alarm states that trigger a notification.
    notificationTriggers :: Prelude.Maybe [AlarmState],
    -- | The timestamp when the alarm was created.
    createdAt :: Prelude.Maybe Data.POSIX
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
-- 'resourceType', 'alarm_resourceType' - The Lightsail resource type (e.g., @Alarm@).
--
-- 'name', 'alarm_name' - The name of the alarm.
--
-- 'period', 'alarm_period' - The period, in seconds, over which the statistic is applied.
--
-- 'arn', 'alarm_arn' - The Amazon Resource Name (ARN) of the alarm.
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
-- 'evaluationPeriods', 'alarm_evaluationPeriods' - The number of periods over which data is compared to the specified
-- threshold.
--
-- 'datapointsToAlarm', 'alarm_datapointsToAlarm' - The number of data points that must not within the specified threshold
-- to trigger the alarm.
--
-- 'location', 'alarm_location' - An object that lists information about the location of the alarm.
--
-- 'notificationEnabled', 'alarm_notificationEnabled' - Indicates whether the alarm is enabled.
--
-- 'metricName', 'alarm_metricName' - The name of the metric associated with the alarm.
--
-- 'monitoredResourceInfo', 'alarm_monitoredResourceInfo' - An object that lists information about the resource monitored by the
-- alarm.
--
-- 'threshold', 'alarm_threshold' - The value against which the specified statistic is compared.
--
-- 'contactProtocols', 'alarm_contactProtocols' - The contact protocols for the alarm, such as @Email@, @SMS@ (text
-- messaging), or both.
--
-- 'comparisonOperator', 'alarm_comparisonOperator' - The arithmetic operation used when comparing the specified statistic and
-- threshold.
--
-- 'supportCode', 'alarm_supportCode' - The support code. Include this code in your email to support when you
-- have questions about your Lightsail alarm. This code enables our support
-- team to look up your Lightsail information more easily.
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
-- 'unit', 'alarm_unit' - The unit of the metric associated with the alarm.
--
-- 'notificationTriggers', 'alarm_notificationTriggers' - The alarm states that trigger a notification.
--
-- 'createdAt', 'alarm_createdAt' - The timestamp when the alarm was created.
newAlarm ::
  Alarm
newAlarm =
  Alarm'
    { resourceType = Prelude.Nothing,
      name = Prelude.Nothing,
      period = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      treatMissingData = Prelude.Nothing,
      evaluationPeriods = Prelude.Nothing,
      datapointsToAlarm = Prelude.Nothing,
      location = Prelude.Nothing,
      notificationEnabled = Prelude.Nothing,
      metricName = Prelude.Nothing,
      monitoredResourceInfo = Prelude.Nothing,
      threshold = Prelude.Nothing,
      contactProtocols = Prelude.Nothing,
      comparisonOperator = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      statistic = Prelude.Nothing,
      unit = Prelude.Nothing,
      notificationTriggers = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The Lightsail resource type (e.g., @Alarm@).
alarm_resourceType :: Lens.Lens' Alarm (Prelude.Maybe ResourceType)
alarm_resourceType = Lens.lens (\Alarm' {resourceType} -> resourceType) (\s@Alarm' {} a -> s {resourceType = a} :: Alarm)

-- | The name of the alarm.
alarm_name :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Text)
alarm_name = Lens.lens (\Alarm' {name} -> name) (\s@Alarm' {} a -> s {name = a} :: Alarm)

-- | The period, in seconds, over which the statistic is applied.
alarm_period :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Natural)
alarm_period = Lens.lens (\Alarm' {period} -> period) (\s@Alarm' {} a -> s {period = a} :: Alarm)

-- | The Amazon Resource Name (ARN) of the alarm.
alarm_arn :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Text)
alarm_arn = Lens.lens (\Alarm' {arn} -> arn) (\s@Alarm' {} a -> s {arn = a} :: Alarm)

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

-- | The number of periods over which data is compared to the specified
-- threshold.
alarm_evaluationPeriods :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Int)
alarm_evaluationPeriods = Lens.lens (\Alarm' {evaluationPeriods} -> evaluationPeriods) (\s@Alarm' {} a -> s {evaluationPeriods = a} :: Alarm)

-- | The number of data points that must not within the specified threshold
-- to trigger the alarm.
alarm_datapointsToAlarm :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Int)
alarm_datapointsToAlarm = Lens.lens (\Alarm' {datapointsToAlarm} -> datapointsToAlarm) (\s@Alarm' {} a -> s {datapointsToAlarm = a} :: Alarm)

-- | An object that lists information about the location of the alarm.
alarm_location :: Lens.Lens' Alarm (Prelude.Maybe ResourceLocation)
alarm_location = Lens.lens (\Alarm' {location} -> location) (\s@Alarm' {} a -> s {location = a} :: Alarm)

-- | Indicates whether the alarm is enabled.
alarm_notificationEnabled :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Bool)
alarm_notificationEnabled = Lens.lens (\Alarm' {notificationEnabled} -> notificationEnabled) (\s@Alarm' {} a -> s {notificationEnabled = a} :: Alarm)

-- | The name of the metric associated with the alarm.
alarm_metricName :: Lens.Lens' Alarm (Prelude.Maybe MetricName)
alarm_metricName = Lens.lens (\Alarm' {metricName} -> metricName) (\s@Alarm' {} a -> s {metricName = a} :: Alarm)

-- | An object that lists information about the resource monitored by the
-- alarm.
alarm_monitoredResourceInfo :: Lens.Lens' Alarm (Prelude.Maybe MonitoredResourceInfo)
alarm_monitoredResourceInfo = Lens.lens (\Alarm' {monitoredResourceInfo} -> monitoredResourceInfo) (\s@Alarm' {} a -> s {monitoredResourceInfo = a} :: Alarm)

-- | The value against which the specified statistic is compared.
alarm_threshold :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Double)
alarm_threshold = Lens.lens (\Alarm' {threshold} -> threshold) (\s@Alarm' {} a -> s {threshold = a} :: Alarm)

-- | The contact protocols for the alarm, such as @Email@, @SMS@ (text
-- messaging), or both.
alarm_contactProtocols :: Lens.Lens' Alarm (Prelude.Maybe [ContactProtocol])
alarm_contactProtocols = Lens.lens (\Alarm' {contactProtocols} -> contactProtocols) (\s@Alarm' {} a -> s {contactProtocols = a} :: Alarm) Prelude.. Lens.mapping Lens.coerced

-- | The arithmetic operation used when comparing the specified statistic and
-- threshold.
alarm_comparisonOperator :: Lens.Lens' Alarm (Prelude.Maybe ComparisonOperator)
alarm_comparisonOperator = Lens.lens (\Alarm' {comparisonOperator} -> comparisonOperator) (\s@Alarm' {} a -> s {comparisonOperator = a} :: Alarm)

-- | The support code. Include this code in your email to support when you
-- have questions about your Lightsail alarm. This code enables our support
-- team to look up your Lightsail information more easily.
alarm_supportCode :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Text)
alarm_supportCode = Lens.lens (\Alarm' {supportCode} -> supportCode) (\s@Alarm' {} a -> s {supportCode = a} :: Alarm)

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

-- | The unit of the metric associated with the alarm.
alarm_unit :: Lens.Lens' Alarm (Prelude.Maybe MetricUnit)
alarm_unit = Lens.lens (\Alarm' {unit} -> unit) (\s@Alarm' {} a -> s {unit = a} :: Alarm)

-- | The alarm states that trigger a notification.
alarm_notificationTriggers :: Lens.Lens' Alarm (Prelude.Maybe [AlarmState])
alarm_notificationTriggers = Lens.lens (\Alarm' {notificationTriggers} -> notificationTriggers) (\s@Alarm' {} a -> s {notificationTriggers = a} :: Alarm) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp when the alarm was created.
alarm_createdAt :: Lens.Lens' Alarm (Prelude.Maybe Prelude.UTCTime)
alarm_createdAt = Lens.lens (\Alarm' {createdAt} -> createdAt) (\s@Alarm' {} a -> s {createdAt = a} :: Alarm) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Alarm where
  parseJSON =
    Data.withObject
      "Alarm"
      ( \x ->
          Alarm'
            Prelude.<$> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "period")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "treatMissingData")
            Prelude.<*> (x Data..:? "evaluationPeriods")
            Prelude.<*> (x Data..:? "datapointsToAlarm")
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..:? "notificationEnabled")
            Prelude.<*> (x Data..:? "metricName")
            Prelude.<*> (x Data..:? "monitoredResourceInfo")
            Prelude.<*> (x Data..:? "threshold")
            Prelude.<*> ( x Data..:? "contactProtocols"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "comparisonOperator")
            Prelude.<*> (x Data..:? "supportCode")
            Prelude.<*> (x Data..:? "statistic")
            Prelude.<*> (x Data..:? "unit")
            Prelude.<*> ( x Data..:? "notificationTriggers"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "createdAt")
      )

instance Prelude.Hashable Alarm where
  hashWithSalt _salt Alarm' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` treatMissingData
      `Prelude.hashWithSalt` evaluationPeriods
      `Prelude.hashWithSalt` datapointsToAlarm
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` notificationEnabled
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` monitoredResourceInfo
      `Prelude.hashWithSalt` threshold
      `Prelude.hashWithSalt` contactProtocols
      `Prelude.hashWithSalt` comparisonOperator
      `Prelude.hashWithSalt` supportCode
      `Prelude.hashWithSalt` statistic
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` notificationTriggers
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData Alarm where
  rnf Alarm' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf treatMissingData
      `Prelude.seq` Prelude.rnf evaluationPeriods
      `Prelude.seq` Prelude.rnf datapointsToAlarm
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf notificationEnabled
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf monitoredResourceInfo
      `Prelude.seq` Prelude.rnf threshold
      `Prelude.seq` Prelude.rnf contactProtocols
      `Prelude.seq` Prelude.rnf comparisonOperator
      `Prelude.seq` Prelude.rnf supportCode
      `Prelude.seq` Prelude.rnf statistic
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf notificationTriggers
      `Prelude.seq` Prelude.rnf createdAt
