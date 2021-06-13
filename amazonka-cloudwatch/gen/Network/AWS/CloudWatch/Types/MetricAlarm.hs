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
-- Module      : Network.AWS.CloudWatch.Types.MetricAlarm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.MetricAlarm where

import Network.AWS.CloudWatch.Types.ComparisonOperator
import Network.AWS.CloudWatch.Types.Dimension
import Network.AWS.CloudWatch.Types.MetricDataQuery
import Network.AWS.CloudWatch.Types.StandardUnit
import Network.AWS.CloudWatch.Types.StateValue
import Network.AWS.CloudWatch.Types.Statistic
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details about a metric alarm.
--
-- /See:/ 'newMetricAlarm' smart constructor.
data MetricAlarm = MetricAlarm'
  { -- | The value to compare with the specified statistic.
    threshold :: Prelude.Maybe Prelude.Double,
    -- | The number of data points that must be breaching to trigger the alarm.
    datapointsToAlarm :: Prelude.Maybe Prelude.Natural,
    -- | Used only for alarms based on percentiles. If @ignore@, the alarm state
    -- does not change during periods with too few data points to be
    -- statistically significant. If @evaluate@ or this parameter is not used,
    -- the alarm is always evaluated and possibly changes state no matter how
    -- many data points are available.
    evaluateLowSampleCountPercentile :: Prelude.Maybe Prelude.Text,
    -- | The arithmetic operation to use when comparing the specified statistic
    -- and threshold. The specified statistic value is used as the first
    -- operand.
    comparisonOperator :: Prelude.Maybe ComparisonOperator,
    -- | The percentile statistic for the metric associated with the alarm.
    -- Specify a value between p0.0 and p100.
    extendedStatistic :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the alarm.
    alarmArn :: Prelude.Maybe Prelude.Text,
    -- | The actions to execute when this alarm transitions to the @ALARM@ state
    -- from any other state. Each action is specified as an Amazon Resource
    -- Name (ARN).
    alarmActions :: Prelude.Maybe [Prelude.Text],
    -- | The unit of the metric associated with the alarm.
    unit :: Prelude.Maybe StandardUnit,
    -- | In an alarm based on an anomaly detection model, this is the ID of the
    -- @ANOMALY_DETECTION_BAND@ function used as the threshold for the alarm.
    thresholdMetricId :: Prelude.Maybe Prelude.Text,
    -- | An explanation for the alarm state, in text format.
    stateReason :: Prelude.Maybe Prelude.Text,
    -- | An explanation for the alarm state, in JSON format.
    stateReasonData :: Prelude.Maybe Prelude.Text,
    -- | The name of the metric associated with the alarm, if this is an alarm
    -- based on a single metric.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The actions to execute when this alarm transitions to the
    -- @INSUFFICIENT_DATA@ state from any other state. Each action is specified
    -- as an Amazon Resource Name (ARN).
    insufficientDataActions :: Prelude.Maybe [Prelude.Text],
    -- | Sets how this alarm is to handle missing data points. If this parameter
    -- is omitted, the default behavior of @missing@ is used.
    treatMissingData :: Prelude.Maybe Prelude.Text,
    -- | An array of MetricDataQuery structures, used in an alarm based on a
    -- metric math expression. Each structure either retrieves a metric or
    -- performs a math expression. One item in the Metrics array is the math
    -- expression that the alarm watches. This expression by designated by
    -- having @ReturnData@ set to true.
    metrics :: Prelude.Maybe [MetricDataQuery],
    -- | The time stamp of the last update to the alarm state.
    stateUpdatedTimestamp :: Prelude.Maybe Core.ISO8601,
    -- | The state value for the alarm.
    stateValue :: Prelude.Maybe StateValue,
    -- | The name of the alarm.
    alarmName :: Prelude.Maybe Prelude.Text,
    -- | The actions to execute when this alarm transitions to the @OK@ state
    -- from any other state. Each action is specified as an Amazon Resource
    -- Name (ARN).
    oKActions :: Prelude.Maybe [Prelude.Text],
    -- | The statistic for the metric associated with the alarm, other than
    -- percentile. For percentile statistics, use @ExtendedStatistic@.
    statistic :: Prelude.Maybe Statistic,
    -- | The dimensions for the metric associated with the alarm.
    dimensions :: Prelude.Maybe [Dimension],
    -- | The namespace of the metric associated with the alarm.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The number of periods over which data is compared to the specified
    -- threshold.
    evaluationPeriods :: Prelude.Maybe Prelude.Natural,
    -- | Indicates whether actions should be executed during any changes to the
    -- alarm state.
    actionsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The time stamp of the last update to the alarm configuration.
    alarmConfigurationUpdatedTimestamp :: Prelude.Maybe Core.ISO8601,
    -- | The description of the alarm.
    alarmDescription :: Prelude.Maybe Prelude.Text,
    -- | The period, in seconds, over which the statistic is applied.
    period :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricAlarm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'threshold', 'metricAlarm_threshold' - The value to compare with the specified statistic.
--
-- 'datapointsToAlarm', 'metricAlarm_datapointsToAlarm' - The number of data points that must be breaching to trigger the alarm.
--
-- 'evaluateLowSampleCountPercentile', 'metricAlarm_evaluateLowSampleCountPercentile' - Used only for alarms based on percentiles. If @ignore@, the alarm state
-- does not change during periods with too few data points to be
-- statistically significant. If @evaluate@ or this parameter is not used,
-- the alarm is always evaluated and possibly changes state no matter how
-- many data points are available.
--
-- 'comparisonOperator', 'metricAlarm_comparisonOperator' - The arithmetic operation to use when comparing the specified statistic
-- and threshold. The specified statistic value is used as the first
-- operand.
--
-- 'extendedStatistic', 'metricAlarm_extendedStatistic' - The percentile statistic for the metric associated with the alarm.
-- Specify a value between p0.0 and p100.
--
-- 'alarmArn', 'metricAlarm_alarmArn' - The Amazon Resource Name (ARN) of the alarm.
--
-- 'alarmActions', 'metricAlarm_alarmActions' - The actions to execute when this alarm transitions to the @ALARM@ state
-- from any other state. Each action is specified as an Amazon Resource
-- Name (ARN).
--
-- 'unit', 'metricAlarm_unit' - The unit of the metric associated with the alarm.
--
-- 'thresholdMetricId', 'metricAlarm_thresholdMetricId' - In an alarm based on an anomaly detection model, this is the ID of the
-- @ANOMALY_DETECTION_BAND@ function used as the threshold for the alarm.
--
-- 'stateReason', 'metricAlarm_stateReason' - An explanation for the alarm state, in text format.
--
-- 'stateReasonData', 'metricAlarm_stateReasonData' - An explanation for the alarm state, in JSON format.
--
-- 'metricName', 'metricAlarm_metricName' - The name of the metric associated with the alarm, if this is an alarm
-- based on a single metric.
--
-- 'insufficientDataActions', 'metricAlarm_insufficientDataActions' - The actions to execute when this alarm transitions to the
-- @INSUFFICIENT_DATA@ state from any other state. Each action is specified
-- as an Amazon Resource Name (ARN).
--
-- 'treatMissingData', 'metricAlarm_treatMissingData' - Sets how this alarm is to handle missing data points. If this parameter
-- is omitted, the default behavior of @missing@ is used.
--
-- 'metrics', 'metricAlarm_metrics' - An array of MetricDataQuery structures, used in an alarm based on a
-- metric math expression. Each structure either retrieves a metric or
-- performs a math expression. One item in the Metrics array is the math
-- expression that the alarm watches. This expression by designated by
-- having @ReturnData@ set to true.
--
-- 'stateUpdatedTimestamp', 'metricAlarm_stateUpdatedTimestamp' - The time stamp of the last update to the alarm state.
--
-- 'stateValue', 'metricAlarm_stateValue' - The state value for the alarm.
--
-- 'alarmName', 'metricAlarm_alarmName' - The name of the alarm.
--
-- 'oKActions', 'metricAlarm_oKActions' - The actions to execute when this alarm transitions to the @OK@ state
-- from any other state. Each action is specified as an Amazon Resource
-- Name (ARN).
--
-- 'statistic', 'metricAlarm_statistic' - The statistic for the metric associated with the alarm, other than
-- percentile. For percentile statistics, use @ExtendedStatistic@.
--
-- 'dimensions', 'metricAlarm_dimensions' - The dimensions for the metric associated with the alarm.
--
-- 'namespace', 'metricAlarm_namespace' - The namespace of the metric associated with the alarm.
--
-- 'evaluationPeriods', 'metricAlarm_evaluationPeriods' - The number of periods over which data is compared to the specified
-- threshold.
--
-- 'actionsEnabled', 'metricAlarm_actionsEnabled' - Indicates whether actions should be executed during any changes to the
-- alarm state.
--
-- 'alarmConfigurationUpdatedTimestamp', 'metricAlarm_alarmConfigurationUpdatedTimestamp' - The time stamp of the last update to the alarm configuration.
--
-- 'alarmDescription', 'metricAlarm_alarmDescription' - The description of the alarm.
--
-- 'period', 'metricAlarm_period' - The period, in seconds, over which the statistic is applied.
newMetricAlarm ::
  MetricAlarm
newMetricAlarm =
  MetricAlarm'
    { threshold = Prelude.Nothing,
      datapointsToAlarm = Prelude.Nothing,
      evaluateLowSampleCountPercentile = Prelude.Nothing,
      comparisonOperator = Prelude.Nothing,
      extendedStatistic = Prelude.Nothing,
      alarmArn = Prelude.Nothing,
      alarmActions = Prelude.Nothing,
      unit = Prelude.Nothing,
      thresholdMetricId = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      stateReasonData = Prelude.Nothing,
      metricName = Prelude.Nothing,
      insufficientDataActions = Prelude.Nothing,
      treatMissingData = Prelude.Nothing,
      metrics = Prelude.Nothing,
      stateUpdatedTimestamp = Prelude.Nothing,
      stateValue = Prelude.Nothing,
      alarmName = Prelude.Nothing,
      oKActions = Prelude.Nothing,
      statistic = Prelude.Nothing,
      dimensions = Prelude.Nothing,
      namespace = Prelude.Nothing,
      evaluationPeriods = Prelude.Nothing,
      actionsEnabled = Prelude.Nothing,
      alarmConfigurationUpdatedTimestamp = Prelude.Nothing,
      alarmDescription = Prelude.Nothing,
      period = Prelude.Nothing
    }

-- | The value to compare with the specified statistic.
metricAlarm_threshold :: Lens.Lens' MetricAlarm (Prelude.Maybe Prelude.Double)
metricAlarm_threshold = Lens.lens (\MetricAlarm' {threshold} -> threshold) (\s@MetricAlarm' {} a -> s {threshold = a} :: MetricAlarm)

-- | The number of data points that must be breaching to trigger the alarm.
metricAlarm_datapointsToAlarm :: Lens.Lens' MetricAlarm (Prelude.Maybe Prelude.Natural)
metricAlarm_datapointsToAlarm = Lens.lens (\MetricAlarm' {datapointsToAlarm} -> datapointsToAlarm) (\s@MetricAlarm' {} a -> s {datapointsToAlarm = a} :: MetricAlarm)

-- | Used only for alarms based on percentiles. If @ignore@, the alarm state
-- does not change during periods with too few data points to be
-- statistically significant. If @evaluate@ or this parameter is not used,
-- the alarm is always evaluated and possibly changes state no matter how
-- many data points are available.
metricAlarm_evaluateLowSampleCountPercentile :: Lens.Lens' MetricAlarm (Prelude.Maybe Prelude.Text)
metricAlarm_evaluateLowSampleCountPercentile = Lens.lens (\MetricAlarm' {evaluateLowSampleCountPercentile} -> evaluateLowSampleCountPercentile) (\s@MetricAlarm' {} a -> s {evaluateLowSampleCountPercentile = a} :: MetricAlarm)

-- | The arithmetic operation to use when comparing the specified statistic
-- and threshold. The specified statistic value is used as the first
-- operand.
metricAlarm_comparisonOperator :: Lens.Lens' MetricAlarm (Prelude.Maybe ComparisonOperator)
metricAlarm_comparisonOperator = Lens.lens (\MetricAlarm' {comparisonOperator} -> comparisonOperator) (\s@MetricAlarm' {} a -> s {comparisonOperator = a} :: MetricAlarm)

-- | The percentile statistic for the metric associated with the alarm.
-- Specify a value between p0.0 and p100.
metricAlarm_extendedStatistic :: Lens.Lens' MetricAlarm (Prelude.Maybe Prelude.Text)
metricAlarm_extendedStatistic = Lens.lens (\MetricAlarm' {extendedStatistic} -> extendedStatistic) (\s@MetricAlarm' {} a -> s {extendedStatistic = a} :: MetricAlarm)

-- | The Amazon Resource Name (ARN) of the alarm.
metricAlarm_alarmArn :: Lens.Lens' MetricAlarm (Prelude.Maybe Prelude.Text)
metricAlarm_alarmArn = Lens.lens (\MetricAlarm' {alarmArn} -> alarmArn) (\s@MetricAlarm' {} a -> s {alarmArn = a} :: MetricAlarm)

-- | The actions to execute when this alarm transitions to the @ALARM@ state
-- from any other state. Each action is specified as an Amazon Resource
-- Name (ARN).
metricAlarm_alarmActions :: Lens.Lens' MetricAlarm (Prelude.Maybe [Prelude.Text])
metricAlarm_alarmActions = Lens.lens (\MetricAlarm' {alarmActions} -> alarmActions) (\s@MetricAlarm' {} a -> s {alarmActions = a} :: MetricAlarm) Prelude.. Lens.mapping Lens._Coerce

-- | The unit of the metric associated with the alarm.
metricAlarm_unit :: Lens.Lens' MetricAlarm (Prelude.Maybe StandardUnit)
metricAlarm_unit = Lens.lens (\MetricAlarm' {unit} -> unit) (\s@MetricAlarm' {} a -> s {unit = a} :: MetricAlarm)

-- | In an alarm based on an anomaly detection model, this is the ID of the
-- @ANOMALY_DETECTION_BAND@ function used as the threshold for the alarm.
metricAlarm_thresholdMetricId :: Lens.Lens' MetricAlarm (Prelude.Maybe Prelude.Text)
metricAlarm_thresholdMetricId = Lens.lens (\MetricAlarm' {thresholdMetricId} -> thresholdMetricId) (\s@MetricAlarm' {} a -> s {thresholdMetricId = a} :: MetricAlarm)

-- | An explanation for the alarm state, in text format.
metricAlarm_stateReason :: Lens.Lens' MetricAlarm (Prelude.Maybe Prelude.Text)
metricAlarm_stateReason = Lens.lens (\MetricAlarm' {stateReason} -> stateReason) (\s@MetricAlarm' {} a -> s {stateReason = a} :: MetricAlarm)

-- | An explanation for the alarm state, in JSON format.
metricAlarm_stateReasonData :: Lens.Lens' MetricAlarm (Prelude.Maybe Prelude.Text)
metricAlarm_stateReasonData = Lens.lens (\MetricAlarm' {stateReasonData} -> stateReasonData) (\s@MetricAlarm' {} a -> s {stateReasonData = a} :: MetricAlarm)

-- | The name of the metric associated with the alarm, if this is an alarm
-- based on a single metric.
metricAlarm_metricName :: Lens.Lens' MetricAlarm (Prelude.Maybe Prelude.Text)
metricAlarm_metricName = Lens.lens (\MetricAlarm' {metricName} -> metricName) (\s@MetricAlarm' {} a -> s {metricName = a} :: MetricAlarm)

-- | The actions to execute when this alarm transitions to the
-- @INSUFFICIENT_DATA@ state from any other state. Each action is specified
-- as an Amazon Resource Name (ARN).
metricAlarm_insufficientDataActions :: Lens.Lens' MetricAlarm (Prelude.Maybe [Prelude.Text])
metricAlarm_insufficientDataActions = Lens.lens (\MetricAlarm' {insufficientDataActions} -> insufficientDataActions) (\s@MetricAlarm' {} a -> s {insufficientDataActions = a} :: MetricAlarm) Prelude.. Lens.mapping Lens._Coerce

-- | Sets how this alarm is to handle missing data points. If this parameter
-- is omitted, the default behavior of @missing@ is used.
metricAlarm_treatMissingData :: Lens.Lens' MetricAlarm (Prelude.Maybe Prelude.Text)
metricAlarm_treatMissingData = Lens.lens (\MetricAlarm' {treatMissingData} -> treatMissingData) (\s@MetricAlarm' {} a -> s {treatMissingData = a} :: MetricAlarm)

-- | An array of MetricDataQuery structures, used in an alarm based on a
-- metric math expression. Each structure either retrieves a metric or
-- performs a math expression. One item in the Metrics array is the math
-- expression that the alarm watches. This expression by designated by
-- having @ReturnData@ set to true.
metricAlarm_metrics :: Lens.Lens' MetricAlarm (Prelude.Maybe [MetricDataQuery])
metricAlarm_metrics = Lens.lens (\MetricAlarm' {metrics} -> metrics) (\s@MetricAlarm' {} a -> s {metrics = a} :: MetricAlarm) Prelude.. Lens.mapping Lens._Coerce

-- | The time stamp of the last update to the alarm state.
metricAlarm_stateUpdatedTimestamp :: Lens.Lens' MetricAlarm (Prelude.Maybe Prelude.UTCTime)
metricAlarm_stateUpdatedTimestamp = Lens.lens (\MetricAlarm' {stateUpdatedTimestamp} -> stateUpdatedTimestamp) (\s@MetricAlarm' {} a -> s {stateUpdatedTimestamp = a} :: MetricAlarm) Prelude.. Lens.mapping Core._Time

-- | The state value for the alarm.
metricAlarm_stateValue :: Lens.Lens' MetricAlarm (Prelude.Maybe StateValue)
metricAlarm_stateValue = Lens.lens (\MetricAlarm' {stateValue} -> stateValue) (\s@MetricAlarm' {} a -> s {stateValue = a} :: MetricAlarm)

-- | The name of the alarm.
metricAlarm_alarmName :: Lens.Lens' MetricAlarm (Prelude.Maybe Prelude.Text)
metricAlarm_alarmName = Lens.lens (\MetricAlarm' {alarmName} -> alarmName) (\s@MetricAlarm' {} a -> s {alarmName = a} :: MetricAlarm)

-- | The actions to execute when this alarm transitions to the @OK@ state
-- from any other state. Each action is specified as an Amazon Resource
-- Name (ARN).
metricAlarm_oKActions :: Lens.Lens' MetricAlarm (Prelude.Maybe [Prelude.Text])
metricAlarm_oKActions = Lens.lens (\MetricAlarm' {oKActions} -> oKActions) (\s@MetricAlarm' {} a -> s {oKActions = a} :: MetricAlarm) Prelude.. Lens.mapping Lens._Coerce

-- | The statistic for the metric associated with the alarm, other than
-- percentile. For percentile statistics, use @ExtendedStatistic@.
metricAlarm_statistic :: Lens.Lens' MetricAlarm (Prelude.Maybe Statistic)
metricAlarm_statistic = Lens.lens (\MetricAlarm' {statistic} -> statistic) (\s@MetricAlarm' {} a -> s {statistic = a} :: MetricAlarm)

-- | The dimensions for the metric associated with the alarm.
metricAlarm_dimensions :: Lens.Lens' MetricAlarm (Prelude.Maybe [Dimension])
metricAlarm_dimensions = Lens.lens (\MetricAlarm' {dimensions} -> dimensions) (\s@MetricAlarm' {} a -> s {dimensions = a} :: MetricAlarm) Prelude.. Lens.mapping Lens._Coerce

-- | The namespace of the metric associated with the alarm.
metricAlarm_namespace :: Lens.Lens' MetricAlarm (Prelude.Maybe Prelude.Text)
metricAlarm_namespace = Lens.lens (\MetricAlarm' {namespace} -> namespace) (\s@MetricAlarm' {} a -> s {namespace = a} :: MetricAlarm)

-- | The number of periods over which data is compared to the specified
-- threshold.
metricAlarm_evaluationPeriods :: Lens.Lens' MetricAlarm (Prelude.Maybe Prelude.Natural)
metricAlarm_evaluationPeriods = Lens.lens (\MetricAlarm' {evaluationPeriods} -> evaluationPeriods) (\s@MetricAlarm' {} a -> s {evaluationPeriods = a} :: MetricAlarm)

-- | Indicates whether actions should be executed during any changes to the
-- alarm state.
metricAlarm_actionsEnabled :: Lens.Lens' MetricAlarm (Prelude.Maybe Prelude.Bool)
metricAlarm_actionsEnabled = Lens.lens (\MetricAlarm' {actionsEnabled} -> actionsEnabled) (\s@MetricAlarm' {} a -> s {actionsEnabled = a} :: MetricAlarm)

-- | The time stamp of the last update to the alarm configuration.
metricAlarm_alarmConfigurationUpdatedTimestamp :: Lens.Lens' MetricAlarm (Prelude.Maybe Prelude.UTCTime)
metricAlarm_alarmConfigurationUpdatedTimestamp = Lens.lens (\MetricAlarm' {alarmConfigurationUpdatedTimestamp} -> alarmConfigurationUpdatedTimestamp) (\s@MetricAlarm' {} a -> s {alarmConfigurationUpdatedTimestamp = a} :: MetricAlarm) Prelude.. Lens.mapping Core._Time

-- | The description of the alarm.
metricAlarm_alarmDescription :: Lens.Lens' MetricAlarm (Prelude.Maybe Prelude.Text)
metricAlarm_alarmDescription = Lens.lens (\MetricAlarm' {alarmDescription} -> alarmDescription) (\s@MetricAlarm' {} a -> s {alarmDescription = a} :: MetricAlarm)

-- | The period, in seconds, over which the statistic is applied.
metricAlarm_period :: Lens.Lens' MetricAlarm (Prelude.Maybe Prelude.Natural)
metricAlarm_period = Lens.lens (\MetricAlarm' {period} -> period) (\s@MetricAlarm' {} a -> s {period = a} :: MetricAlarm)

instance Core.FromXML MetricAlarm where
  parseXML x =
    MetricAlarm'
      Prelude.<$> (x Core..@? "Threshold")
      Prelude.<*> (x Core..@? "DatapointsToAlarm")
      Prelude.<*> (x Core..@? "EvaluateLowSampleCountPercentile")
      Prelude.<*> (x Core..@? "ComparisonOperator")
      Prelude.<*> (x Core..@? "ExtendedStatistic")
      Prelude.<*> (x Core..@? "AlarmArn")
      Prelude.<*> ( x Core..@? "AlarmActions" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "Unit")
      Prelude.<*> (x Core..@? "ThresholdMetricId")
      Prelude.<*> (x Core..@? "StateReason")
      Prelude.<*> (x Core..@? "StateReasonData")
      Prelude.<*> (x Core..@? "MetricName")
      Prelude.<*> ( x Core..@? "InsufficientDataActions"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "TreatMissingData")
      Prelude.<*> ( x Core..@? "Metrics" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "StateUpdatedTimestamp")
      Prelude.<*> (x Core..@? "StateValue")
      Prelude.<*> (x Core..@? "AlarmName")
      Prelude.<*> ( x Core..@? "OKActions" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "Statistic")
      Prelude.<*> ( x Core..@? "Dimensions" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "Namespace")
      Prelude.<*> (x Core..@? "EvaluationPeriods")
      Prelude.<*> (x Core..@? "ActionsEnabled")
      Prelude.<*> (x Core..@? "AlarmConfigurationUpdatedTimestamp")
      Prelude.<*> (x Core..@? "AlarmDescription")
      Prelude.<*> (x Core..@? "Period")

instance Prelude.Hashable MetricAlarm

instance Prelude.NFData MetricAlarm
