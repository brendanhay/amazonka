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
-- Module      : Amazonka.SecurityHub.Types.AwsCloudWatchAlarmDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudWatchAlarmDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsCloudWatchAlarmDimensionsDetails

-- | Specifies an alarm and associates it with the specified metric or metric
-- math expression.
--
-- /See:/ 'newAwsCloudWatchAlarmDetails' smart constructor.
data AwsCloudWatchAlarmDetails = AwsCloudWatchAlarmDetails'
  { -- | The list of actions, specified as Amazon Resource Names (ARNs) to
    -- execute when this alarm transitions into an @ALARM@ state from any other
    -- state.
    alarmActions :: Prelude.Maybe [Prelude.Text],
    -- | The description of the alarm.
    alarmDescription :: Prelude.Maybe Prelude.Text,
    -- | The percentile statistic for the metric associated with the alarm.
    extendedStatistic :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether actions should be executed during any changes to the
    -- alarm state.
    actionsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The period, in seconds, over which the statistic is applied. This is
    -- required for an alarm based on a metric.
    period :: Prelude.Maybe Prelude.Int,
    -- | Used only for alarms based on percentiles. If @ignore@, the alarm state
    -- does not change during periods with too few data points to be
    -- statistically significant. If @evaluate@ or this parameter is not used,
    -- the alarm is always evaluated and possibly changes state no matter how
    -- many data points are available.
    evaluateLowSampleCountPercentile :: Prelude.Maybe Prelude.Text,
    -- | The dimensions for the metric associated with the alarm.
    dimensions :: Prelude.Maybe [AwsCloudWatchAlarmDimensionsDetails],
    -- | n an alarm based on an anomaly detection model, this is the ID of the
    -- @ANOMALY_DETECTION_BAND@ function used as the threshold for the alarm.
    thresholdMetricId :: Prelude.Maybe Prelude.Text,
    -- | Sets how this alarm is to handle missing data points.
    treatMissingData :: Prelude.Maybe Prelude.Text,
    -- | The number of periods over which data is compared to the specified
    -- threshold.
    evaluationPeriods :: Prelude.Maybe Prelude.Int,
    -- | The number of datapoints that must be breaching to trigger the alarm.
    datapointsToAlarm :: Prelude.Maybe Prelude.Int,
    -- | The actions to execute when this alarm transitions to the
    -- @INSUFFICIENT_DATA@ state from any other state. Each action is specified
    -- as an ARN.
    insufficientDataActions :: Prelude.Maybe [Prelude.Text],
    -- | The ARN of the alarm.
    alarmArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the metric associated with the alarm. This is required for
    -- an alarm based on a metric. For an alarm based on a math expression, you
    -- use @Metrics@ instead and you can\'t specify @MetricName@.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The time stamp of the last update to the alarm configuration.
    alarmConfigurationUpdatedTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The value to compare with the specified statistic.
    threshold :: Prelude.Maybe Prelude.Double,
    -- | The actions to execute when this alarm transitions to the @OK@ state
    -- from any other state. Each action is specified as an ARN.
    okActions :: Prelude.Maybe [Prelude.Text],
    -- | The name of the alarm. If you don\'t specify a name, CloudFront
    -- generates a unique physical ID and uses that ID for the alarm name.
    alarmName :: Prelude.Maybe Prelude.Text,
    -- | The arithmetic operation to use when comparing the specified statistic
    -- and threshold. The specified statistic value is used as the first
    -- operand.
    comparisonOperator :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the metric associated with the alarm. This is required
    -- for an alarm based on a metric. For an alarm based on a math expression,
    -- you can\'t specify @Namespace@ and you use @Metrics@ instead.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The statistic for the metric associated with the alarm, other than
    -- percentile. For percentile statistics, use @ExtendedStatistic@.
    --
    -- For an alarm based on a metric, you must specify either @Statistic@ or
    -- @ExtendedStatistic@ but not both.
    --
    -- For an alarm based on a math expression, you can\'t specify @Statistic@.
    -- Instead, you use @Metrics@.
    statistic :: Prelude.Maybe Prelude.Text,
    -- | The unit of the metric associated with the alarm.
    unit :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudWatchAlarmDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmActions', 'awsCloudWatchAlarmDetails_alarmActions' - The list of actions, specified as Amazon Resource Names (ARNs) to
-- execute when this alarm transitions into an @ALARM@ state from any other
-- state.
--
-- 'alarmDescription', 'awsCloudWatchAlarmDetails_alarmDescription' - The description of the alarm.
--
-- 'extendedStatistic', 'awsCloudWatchAlarmDetails_extendedStatistic' - The percentile statistic for the metric associated with the alarm.
--
-- 'actionsEnabled', 'awsCloudWatchAlarmDetails_actionsEnabled' - Indicates whether actions should be executed during any changes to the
-- alarm state.
--
-- 'period', 'awsCloudWatchAlarmDetails_period' - The period, in seconds, over which the statistic is applied. This is
-- required for an alarm based on a metric.
--
-- 'evaluateLowSampleCountPercentile', 'awsCloudWatchAlarmDetails_evaluateLowSampleCountPercentile' - Used only for alarms based on percentiles. If @ignore@, the alarm state
-- does not change during periods with too few data points to be
-- statistically significant. If @evaluate@ or this parameter is not used,
-- the alarm is always evaluated and possibly changes state no matter how
-- many data points are available.
--
-- 'dimensions', 'awsCloudWatchAlarmDetails_dimensions' - The dimensions for the metric associated with the alarm.
--
-- 'thresholdMetricId', 'awsCloudWatchAlarmDetails_thresholdMetricId' - n an alarm based on an anomaly detection model, this is the ID of the
-- @ANOMALY_DETECTION_BAND@ function used as the threshold for the alarm.
--
-- 'treatMissingData', 'awsCloudWatchAlarmDetails_treatMissingData' - Sets how this alarm is to handle missing data points.
--
-- 'evaluationPeriods', 'awsCloudWatchAlarmDetails_evaluationPeriods' - The number of periods over which data is compared to the specified
-- threshold.
--
-- 'datapointsToAlarm', 'awsCloudWatchAlarmDetails_datapointsToAlarm' - The number of datapoints that must be breaching to trigger the alarm.
--
-- 'insufficientDataActions', 'awsCloudWatchAlarmDetails_insufficientDataActions' - The actions to execute when this alarm transitions to the
-- @INSUFFICIENT_DATA@ state from any other state. Each action is specified
-- as an ARN.
--
-- 'alarmArn', 'awsCloudWatchAlarmDetails_alarmArn' - The ARN of the alarm.
--
-- 'metricName', 'awsCloudWatchAlarmDetails_metricName' - The name of the metric associated with the alarm. This is required for
-- an alarm based on a metric. For an alarm based on a math expression, you
-- use @Metrics@ instead and you can\'t specify @MetricName@.
--
-- 'alarmConfigurationUpdatedTimestamp', 'awsCloudWatchAlarmDetails_alarmConfigurationUpdatedTimestamp' - The time stamp of the last update to the alarm configuration.
--
-- 'threshold', 'awsCloudWatchAlarmDetails_threshold' - The value to compare with the specified statistic.
--
-- 'okActions', 'awsCloudWatchAlarmDetails_okActions' - The actions to execute when this alarm transitions to the @OK@ state
-- from any other state. Each action is specified as an ARN.
--
-- 'alarmName', 'awsCloudWatchAlarmDetails_alarmName' - The name of the alarm. If you don\'t specify a name, CloudFront
-- generates a unique physical ID and uses that ID for the alarm name.
--
-- 'comparisonOperator', 'awsCloudWatchAlarmDetails_comparisonOperator' - The arithmetic operation to use when comparing the specified statistic
-- and threshold. The specified statistic value is used as the first
-- operand.
--
-- 'namespace', 'awsCloudWatchAlarmDetails_namespace' - The namespace of the metric associated with the alarm. This is required
-- for an alarm based on a metric. For an alarm based on a math expression,
-- you can\'t specify @Namespace@ and you use @Metrics@ instead.
--
-- 'statistic', 'awsCloudWatchAlarmDetails_statistic' - The statistic for the metric associated with the alarm, other than
-- percentile. For percentile statistics, use @ExtendedStatistic@.
--
-- For an alarm based on a metric, you must specify either @Statistic@ or
-- @ExtendedStatistic@ but not both.
--
-- For an alarm based on a math expression, you can\'t specify @Statistic@.
-- Instead, you use @Metrics@.
--
-- 'unit', 'awsCloudWatchAlarmDetails_unit' - The unit of the metric associated with the alarm.
newAwsCloudWatchAlarmDetails ::
  AwsCloudWatchAlarmDetails
newAwsCloudWatchAlarmDetails =
  AwsCloudWatchAlarmDetails'
    { alarmActions =
        Prelude.Nothing,
      alarmDescription = Prelude.Nothing,
      extendedStatistic = Prelude.Nothing,
      actionsEnabled = Prelude.Nothing,
      period = Prelude.Nothing,
      evaluateLowSampleCountPercentile =
        Prelude.Nothing,
      dimensions = Prelude.Nothing,
      thresholdMetricId = Prelude.Nothing,
      treatMissingData = Prelude.Nothing,
      evaluationPeriods = Prelude.Nothing,
      datapointsToAlarm = Prelude.Nothing,
      insufficientDataActions = Prelude.Nothing,
      alarmArn = Prelude.Nothing,
      metricName = Prelude.Nothing,
      alarmConfigurationUpdatedTimestamp =
        Prelude.Nothing,
      threshold = Prelude.Nothing,
      okActions = Prelude.Nothing,
      alarmName = Prelude.Nothing,
      comparisonOperator = Prelude.Nothing,
      namespace = Prelude.Nothing,
      statistic = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | The list of actions, specified as Amazon Resource Names (ARNs) to
-- execute when this alarm transitions into an @ALARM@ state from any other
-- state.
awsCloudWatchAlarmDetails_alarmActions :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe [Prelude.Text])
awsCloudWatchAlarmDetails_alarmActions = Lens.lens (\AwsCloudWatchAlarmDetails' {alarmActions} -> alarmActions) (\s@AwsCloudWatchAlarmDetails' {} a -> s {alarmActions = a} :: AwsCloudWatchAlarmDetails) Prelude.. Lens.mapping Lens.coerced

-- | The description of the alarm.
awsCloudWatchAlarmDetails_alarmDescription :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe Prelude.Text)
awsCloudWatchAlarmDetails_alarmDescription = Lens.lens (\AwsCloudWatchAlarmDetails' {alarmDescription} -> alarmDescription) (\s@AwsCloudWatchAlarmDetails' {} a -> s {alarmDescription = a} :: AwsCloudWatchAlarmDetails)

-- | The percentile statistic for the metric associated with the alarm.
awsCloudWatchAlarmDetails_extendedStatistic :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe Prelude.Text)
awsCloudWatchAlarmDetails_extendedStatistic = Lens.lens (\AwsCloudWatchAlarmDetails' {extendedStatistic} -> extendedStatistic) (\s@AwsCloudWatchAlarmDetails' {} a -> s {extendedStatistic = a} :: AwsCloudWatchAlarmDetails)

-- | Indicates whether actions should be executed during any changes to the
-- alarm state.
awsCloudWatchAlarmDetails_actionsEnabled :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe Prelude.Bool)
awsCloudWatchAlarmDetails_actionsEnabled = Lens.lens (\AwsCloudWatchAlarmDetails' {actionsEnabled} -> actionsEnabled) (\s@AwsCloudWatchAlarmDetails' {} a -> s {actionsEnabled = a} :: AwsCloudWatchAlarmDetails)

-- | The period, in seconds, over which the statistic is applied. This is
-- required for an alarm based on a metric.
awsCloudWatchAlarmDetails_period :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe Prelude.Int)
awsCloudWatchAlarmDetails_period = Lens.lens (\AwsCloudWatchAlarmDetails' {period} -> period) (\s@AwsCloudWatchAlarmDetails' {} a -> s {period = a} :: AwsCloudWatchAlarmDetails)

-- | Used only for alarms based on percentiles. If @ignore@, the alarm state
-- does not change during periods with too few data points to be
-- statistically significant. If @evaluate@ or this parameter is not used,
-- the alarm is always evaluated and possibly changes state no matter how
-- many data points are available.
awsCloudWatchAlarmDetails_evaluateLowSampleCountPercentile :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe Prelude.Text)
awsCloudWatchAlarmDetails_evaluateLowSampleCountPercentile = Lens.lens (\AwsCloudWatchAlarmDetails' {evaluateLowSampleCountPercentile} -> evaluateLowSampleCountPercentile) (\s@AwsCloudWatchAlarmDetails' {} a -> s {evaluateLowSampleCountPercentile = a} :: AwsCloudWatchAlarmDetails)

-- | The dimensions for the metric associated with the alarm.
awsCloudWatchAlarmDetails_dimensions :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe [AwsCloudWatchAlarmDimensionsDetails])
awsCloudWatchAlarmDetails_dimensions = Lens.lens (\AwsCloudWatchAlarmDetails' {dimensions} -> dimensions) (\s@AwsCloudWatchAlarmDetails' {} a -> s {dimensions = a} :: AwsCloudWatchAlarmDetails) Prelude.. Lens.mapping Lens.coerced

-- | n an alarm based on an anomaly detection model, this is the ID of the
-- @ANOMALY_DETECTION_BAND@ function used as the threshold for the alarm.
awsCloudWatchAlarmDetails_thresholdMetricId :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe Prelude.Text)
awsCloudWatchAlarmDetails_thresholdMetricId = Lens.lens (\AwsCloudWatchAlarmDetails' {thresholdMetricId} -> thresholdMetricId) (\s@AwsCloudWatchAlarmDetails' {} a -> s {thresholdMetricId = a} :: AwsCloudWatchAlarmDetails)

-- | Sets how this alarm is to handle missing data points.
awsCloudWatchAlarmDetails_treatMissingData :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe Prelude.Text)
awsCloudWatchAlarmDetails_treatMissingData = Lens.lens (\AwsCloudWatchAlarmDetails' {treatMissingData} -> treatMissingData) (\s@AwsCloudWatchAlarmDetails' {} a -> s {treatMissingData = a} :: AwsCloudWatchAlarmDetails)

-- | The number of periods over which data is compared to the specified
-- threshold.
awsCloudWatchAlarmDetails_evaluationPeriods :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe Prelude.Int)
awsCloudWatchAlarmDetails_evaluationPeriods = Lens.lens (\AwsCloudWatchAlarmDetails' {evaluationPeriods} -> evaluationPeriods) (\s@AwsCloudWatchAlarmDetails' {} a -> s {evaluationPeriods = a} :: AwsCloudWatchAlarmDetails)

-- | The number of datapoints that must be breaching to trigger the alarm.
awsCloudWatchAlarmDetails_datapointsToAlarm :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe Prelude.Int)
awsCloudWatchAlarmDetails_datapointsToAlarm = Lens.lens (\AwsCloudWatchAlarmDetails' {datapointsToAlarm} -> datapointsToAlarm) (\s@AwsCloudWatchAlarmDetails' {} a -> s {datapointsToAlarm = a} :: AwsCloudWatchAlarmDetails)

-- | The actions to execute when this alarm transitions to the
-- @INSUFFICIENT_DATA@ state from any other state. Each action is specified
-- as an ARN.
awsCloudWatchAlarmDetails_insufficientDataActions :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe [Prelude.Text])
awsCloudWatchAlarmDetails_insufficientDataActions = Lens.lens (\AwsCloudWatchAlarmDetails' {insufficientDataActions} -> insufficientDataActions) (\s@AwsCloudWatchAlarmDetails' {} a -> s {insufficientDataActions = a} :: AwsCloudWatchAlarmDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the alarm.
awsCloudWatchAlarmDetails_alarmArn :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe Prelude.Text)
awsCloudWatchAlarmDetails_alarmArn = Lens.lens (\AwsCloudWatchAlarmDetails' {alarmArn} -> alarmArn) (\s@AwsCloudWatchAlarmDetails' {} a -> s {alarmArn = a} :: AwsCloudWatchAlarmDetails)

-- | The name of the metric associated with the alarm. This is required for
-- an alarm based on a metric. For an alarm based on a math expression, you
-- use @Metrics@ instead and you can\'t specify @MetricName@.
awsCloudWatchAlarmDetails_metricName :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe Prelude.Text)
awsCloudWatchAlarmDetails_metricName = Lens.lens (\AwsCloudWatchAlarmDetails' {metricName} -> metricName) (\s@AwsCloudWatchAlarmDetails' {} a -> s {metricName = a} :: AwsCloudWatchAlarmDetails)

-- | The time stamp of the last update to the alarm configuration.
awsCloudWatchAlarmDetails_alarmConfigurationUpdatedTimestamp :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe Prelude.Text)
awsCloudWatchAlarmDetails_alarmConfigurationUpdatedTimestamp = Lens.lens (\AwsCloudWatchAlarmDetails' {alarmConfigurationUpdatedTimestamp} -> alarmConfigurationUpdatedTimestamp) (\s@AwsCloudWatchAlarmDetails' {} a -> s {alarmConfigurationUpdatedTimestamp = a} :: AwsCloudWatchAlarmDetails)

-- | The value to compare with the specified statistic.
awsCloudWatchAlarmDetails_threshold :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe Prelude.Double)
awsCloudWatchAlarmDetails_threshold = Lens.lens (\AwsCloudWatchAlarmDetails' {threshold} -> threshold) (\s@AwsCloudWatchAlarmDetails' {} a -> s {threshold = a} :: AwsCloudWatchAlarmDetails)

-- | The actions to execute when this alarm transitions to the @OK@ state
-- from any other state. Each action is specified as an ARN.
awsCloudWatchAlarmDetails_okActions :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe [Prelude.Text])
awsCloudWatchAlarmDetails_okActions = Lens.lens (\AwsCloudWatchAlarmDetails' {okActions} -> okActions) (\s@AwsCloudWatchAlarmDetails' {} a -> s {okActions = a} :: AwsCloudWatchAlarmDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of the alarm. If you don\'t specify a name, CloudFront
-- generates a unique physical ID and uses that ID for the alarm name.
awsCloudWatchAlarmDetails_alarmName :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe Prelude.Text)
awsCloudWatchAlarmDetails_alarmName = Lens.lens (\AwsCloudWatchAlarmDetails' {alarmName} -> alarmName) (\s@AwsCloudWatchAlarmDetails' {} a -> s {alarmName = a} :: AwsCloudWatchAlarmDetails)

-- | The arithmetic operation to use when comparing the specified statistic
-- and threshold. The specified statistic value is used as the first
-- operand.
awsCloudWatchAlarmDetails_comparisonOperator :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe Prelude.Text)
awsCloudWatchAlarmDetails_comparisonOperator = Lens.lens (\AwsCloudWatchAlarmDetails' {comparisonOperator} -> comparisonOperator) (\s@AwsCloudWatchAlarmDetails' {} a -> s {comparisonOperator = a} :: AwsCloudWatchAlarmDetails)

-- | The namespace of the metric associated with the alarm. This is required
-- for an alarm based on a metric. For an alarm based on a math expression,
-- you can\'t specify @Namespace@ and you use @Metrics@ instead.
awsCloudWatchAlarmDetails_namespace :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe Prelude.Text)
awsCloudWatchAlarmDetails_namespace = Lens.lens (\AwsCloudWatchAlarmDetails' {namespace} -> namespace) (\s@AwsCloudWatchAlarmDetails' {} a -> s {namespace = a} :: AwsCloudWatchAlarmDetails)

-- | The statistic for the metric associated with the alarm, other than
-- percentile. For percentile statistics, use @ExtendedStatistic@.
--
-- For an alarm based on a metric, you must specify either @Statistic@ or
-- @ExtendedStatistic@ but not both.
--
-- For an alarm based on a math expression, you can\'t specify @Statistic@.
-- Instead, you use @Metrics@.
awsCloudWatchAlarmDetails_statistic :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe Prelude.Text)
awsCloudWatchAlarmDetails_statistic = Lens.lens (\AwsCloudWatchAlarmDetails' {statistic} -> statistic) (\s@AwsCloudWatchAlarmDetails' {} a -> s {statistic = a} :: AwsCloudWatchAlarmDetails)

-- | The unit of the metric associated with the alarm.
awsCloudWatchAlarmDetails_unit :: Lens.Lens' AwsCloudWatchAlarmDetails (Prelude.Maybe Prelude.Text)
awsCloudWatchAlarmDetails_unit = Lens.lens (\AwsCloudWatchAlarmDetails' {unit} -> unit) (\s@AwsCloudWatchAlarmDetails' {} a -> s {unit = a} :: AwsCloudWatchAlarmDetails)

instance Data.FromJSON AwsCloudWatchAlarmDetails where
  parseJSON =
    Data.withObject
      "AwsCloudWatchAlarmDetails"
      ( \x ->
          AwsCloudWatchAlarmDetails'
            Prelude.<$> (x Data..:? "AlarmActions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "AlarmDescription")
            Prelude.<*> (x Data..:? "ExtendedStatistic")
            Prelude.<*> (x Data..:? "ActionsEnabled")
            Prelude.<*> (x Data..:? "Period")
            Prelude.<*> (x Data..:? "EvaluateLowSampleCountPercentile")
            Prelude.<*> (x Data..:? "Dimensions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ThresholdMetricId")
            Prelude.<*> (x Data..:? "TreatMissingData")
            Prelude.<*> (x Data..:? "EvaluationPeriods")
            Prelude.<*> (x Data..:? "DatapointsToAlarm")
            Prelude.<*> ( x Data..:? "InsufficientDataActions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "AlarmArn")
            Prelude.<*> (x Data..:? "MetricName")
            Prelude.<*> (x Data..:? "AlarmConfigurationUpdatedTimestamp")
            Prelude.<*> (x Data..:? "Threshold")
            Prelude.<*> (x Data..:? "OkActions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "AlarmName")
            Prelude.<*> (x Data..:? "ComparisonOperator")
            Prelude.<*> (x Data..:? "Namespace")
            Prelude.<*> (x Data..:? "Statistic")
            Prelude.<*> (x Data..:? "Unit")
      )

instance Prelude.Hashable AwsCloudWatchAlarmDetails where
  hashWithSalt _salt AwsCloudWatchAlarmDetails' {..} =
    _salt `Prelude.hashWithSalt` alarmActions
      `Prelude.hashWithSalt` alarmDescription
      `Prelude.hashWithSalt` extendedStatistic
      `Prelude.hashWithSalt` actionsEnabled
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` evaluateLowSampleCountPercentile
      `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` thresholdMetricId
      `Prelude.hashWithSalt` treatMissingData
      `Prelude.hashWithSalt` evaluationPeriods
      `Prelude.hashWithSalt` datapointsToAlarm
      `Prelude.hashWithSalt` insufficientDataActions
      `Prelude.hashWithSalt` alarmArn
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` alarmConfigurationUpdatedTimestamp
      `Prelude.hashWithSalt` threshold
      `Prelude.hashWithSalt` okActions
      `Prelude.hashWithSalt` alarmName
      `Prelude.hashWithSalt` comparisonOperator
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` statistic
      `Prelude.hashWithSalt` unit

instance Prelude.NFData AwsCloudWatchAlarmDetails where
  rnf AwsCloudWatchAlarmDetails' {..} =
    Prelude.rnf alarmActions
      `Prelude.seq` Prelude.rnf alarmDescription
      `Prelude.seq` Prelude.rnf extendedStatistic
      `Prelude.seq` Prelude.rnf actionsEnabled
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf evaluateLowSampleCountPercentile
      `Prelude.seq` Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf thresholdMetricId
      `Prelude.seq` Prelude.rnf treatMissingData
      `Prelude.seq` Prelude.rnf evaluationPeriods
      `Prelude.seq` Prelude.rnf datapointsToAlarm
      `Prelude.seq` Prelude.rnf insufficientDataActions
      `Prelude.seq` Prelude.rnf alarmArn
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf
        alarmConfigurationUpdatedTimestamp
      `Prelude.seq` Prelude.rnf threshold
      `Prelude.seq` Prelude.rnf okActions
      `Prelude.seq` Prelude.rnf alarmName
      `Prelude.seq` Prelude.rnf comparisonOperator
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf statistic
      `Prelude.seq` Prelude.rnf unit

instance Data.ToJSON AwsCloudWatchAlarmDetails where
  toJSON AwsCloudWatchAlarmDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AlarmActions" Data..=) Prelude.<$> alarmActions,
            ("AlarmDescription" Data..=)
              Prelude.<$> alarmDescription,
            ("ExtendedStatistic" Data..=)
              Prelude.<$> extendedStatistic,
            ("ActionsEnabled" Data..=)
              Prelude.<$> actionsEnabled,
            ("Period" Data..=) Prelude.<$> period,
            ("EvaluateLowSampleCountPercentile" Data..=)
              Prelude.<$> evaluateLowSampleCountPercentile,
            ("Dimensions" Data..=) Prelude.<$> dimensions,
            ("ThresholdMetricId" Data..=)
              Prelude.<$> thresholdMetricId,
            ("TreatMissingData" Data..=)
              Prelude.<$> treatMissingData,
            ("EvaluationPeriods" Data..=)
              Prelude.<$> evaluationPeriods,
            ("DatapointsToAlarm" Data..=)
              Prelude.<$> datapointsToAlarm,
            ("InsufficientDataActions" Data..=)
              Prelude.<$> insufficientDataActions,
            ("AlarmArn" Data..=) Prelude.<$> alarmArn,
            ("MetricName" Data..=) Prelude.<$> metricName,
            ("AlarmConfigurationUpdatedTimestamp" Data..=)
              Prelude.<$> alarmConfigurationUpdatedTimestamp,
            ("Threshold" Data..=) Prelude.<$> threshold,
            ("OkActions" Data..=) Prelude.<$> okActions,
            ("AlarmName" Data..=) Prelude.<$> alarmName,
            ("ComparisonOperator" Data..=)
              Prelude.<$> comparisonOperator,
            ("Namespace" Data..=) Prelude.<$> namespace,
            ("Statistic" Data..=) Prelude.<$> statistic,
            ("Unit" Data..=) Prelude.<$> unit
          ]
      )
