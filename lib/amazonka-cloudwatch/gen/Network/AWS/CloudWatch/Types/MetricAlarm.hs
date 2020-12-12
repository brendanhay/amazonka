{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.MetricAlarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.MetricAlarm
  ( MetricAlarm (..),

    -- * Smart constructor
    mkMetricAlarm,

    -- * Lenses
    maAlarmName,
    maStateUpdatedTimestamp,
    maMetrics,
    maTreatMissingData,
    maPeriod,
    maAlarmDescription,
    maEvaluationPeriods,
    maMetricName,
    maNamespace,
    maThresholdMetricId,
    maComparisonOperator,
    maOKActions,
    maEvaluateLowSampleCountPercentile,
    maStateValue,
    maDatapointsToAlarm,
    maThreshold,
    maAlarmConfigurationUpdatedTimestamp,
    maActionsEnabled,
    maInsufficientDataActions,
    maStateReason,
    maStateReasonData,
    maDimensions,
    maAlarmARN,
    maAlarmActions,
    maUnit,
    maStatistic,
    maExtendedStatistic,
  )
where

import Network.AWS.CloudWatch.Types.ComparisonOperator
import Network.AWS.CloudWatch.Types.Dimension
import Network.AWS.CloudWatch.Types.MetricDataQuery
import Network.AWS.CloudWatch.Types.StandardUnit
import Network.AWS.CloudWatch.Types.StateValue
import Network.AWS.CloudWatch.Types.Statistic
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details about a metric alarm.
--
-- /See:/ 'mkMetricAlarm' smart constructor.
data MetricAlarm = MetricAlarm'
  { alarmName :: Lude.Maybe Lude.Text,
    stateUpdatedTimestamp :: Lude.Maybe Lude.DateTime,
    metrics :: Lude.Maybe [MetricDataQuery],
    treatMissingData :: Lude.Maybe Lude.Text,
    period :: Lude.Maybe Lude.Natural,
    alarmDescription :: Lude.Maybe Lude.Text,
    evaluationPeriods :: Lude.Maybe Lude.Natural,
    metricName :: Lude.Maybe Lude.Text,
    namespace :: Lude.Maybe Lude.Text,
    thresholdMetricId :: Lude.Maybe Lude.Text,
    comparisonOperator :: Lude.Maybe ComparisonOperator,
    okActions :: Lude.Maybe [Lude.Text],
    evaluateLowSampleCountPercentile :: Lude.Maybe Lude.Text,
    stateValue :: Lude.Maybe StateValue,
    datapointsToAlarm :: Lude.Maybe Lude.Natural,
    threshold :: Lude.Maybe Lude.Double,
    alarmConfigurationUpdatedTimestamp :: Lude.Maybe Lude.DateTime,
    actionsEnabled :: Lude.Maybe Lude.Bool,
    insufficientDataActions :: Lude.Maybe [Lude.Text],
    stateReason :: Lude.Maybe Lude.Text,
    stateReasonData :: Lude.Maybe Lude.Text,
    dimensions :: Lude.Maybe [Dimension],
    alarmARN :: Lude.Maybe Lude.Text,
    alarmActions :: Lude.Maybe [Lude.Text],
    unit :: Lude.Maybe StandardUnit,
    statistic :: Lude.Maybe Statistic,
    extendedStatistic :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricAlarm' with the minimum fields required to make a request.
--
-- * 'actionsEnabled' - Indicates whether actions should be executed during any changes to the alarm state.
-- * 'alarmARN' - The Amazon Resource Name (ARN) of the alarm.
-- * 'alarmActions' - The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
-- * 'alarmConfigurationUpdatedTimestamp' - The time stamp of the last update to the alarm configuration.
-- * 'alarmDescription' - The description of the alarm.
-- * 'alarmName' - The name of the alarm.
-- * 'comparisonOperator' - The arithmetic operation to use when comparing the specified statistic and threshold. The specified statistic value is used as the first operand.
-- * 'datapointsToAlarm' - The number of data points that must be breaching to trigger the alarm.
-- * 'dimensions' - The dimensions for the metric associated with the alarm.
-- * 'evaluateLowSampleCountPercentile' - Used only for alarms based on percentiles. If @ignore@ , the alarm state does not change during periods with too few data points to be statistically significant. If @evaluate@ or this parameter is not used, the alarm is always evaluated and possibly changes state no matter how many data points are available.
-- * 'evaluationPeriods' - The number of periods over which data is compared to the specified threshold.
-- * 'extendedStatistic' - The percentile statistic for the metric associated with the alarm. Specify a value between p0.0 and p100.
-- * 'insufficientDataActions' - The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
-- * 'metricName' - The name of the metric associated with the alarm, if this is an alarm based on a single metric.
-- * 'metrics' - An array of MetricDataQuery structures, used in an alarm based on a metric math expression. Each structure either retrieves a metric or performs a math expression. One item in the Metrics array is the math expression that the alarm watches. This expression by designated by having @ReturnData@ set to true.
-- * 'namespace' - The namespace of the metric associated with the alarm.
-- * 'okActions' - The actions to execute when this alarm transitions to the @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
-- * 'period' - The period, in seconds, over which the statistic is applied.
-- * 'stateReason' - An explanation for the alarm state, in text format.
-- * 'stateReasonData' - An explanation for the alarm state, in JSON format.
-- * 'stateUpdatedTimestamp' - The time stamp of the last update to the alarm state.
-- * 'stateValue' - The state value for the alarm.
-- * 'statistic' - The statistic for the metric associated with the alarm, other than percentile. For percentile statistics, use @ExtendedStatistic@ .
-- * 'threshold' - The value to compare with the specified statistic.
-- * 'thresholdMetricId' - In an alarm based on an anomaly detection model, this is the ID of the @ANOMALY_DETECTION_BAND@ function used as the threshold for the alarm.
-- * 'treatMissingData' - Sets how this alarm is to handle missing data points. If this parameter is omitted, the default behavior of @missing@ is used.
-- * 'unit' - The unit of the metric associated with the alarm.
mkMetricAlarm ::
  MetricAlarm
mkMetricAlarm =
  MetricAlarm'
    { alarmName = Lude.Nothing,
      stateUpdatedTimestamp = Lude.Nothing,
      metrics = Lude.Nothing,
      treatMissingData = Lude.Nothing,
      period = Lude.Nothing,
      alarmDescription = Lude.Nothing,
      evaluationPeriods = Lude.Nothing,
      metricName = Lude.Nothing,
      namespace = Lude.Nothing,
      thresholdMetricId = Lude.Nothing,
      comparisonOperator = Lude.Nothing,
      okActions = Lude.Nothing,
      evaluateLowSampleCountPercentile = Lude.Nothing,
      stateValue = Lude.Nothing,
      datapointsToAlarm = Lude.Nothing,
      threshold = Lude.Nothing,
      alarmConfigurationUpdatedTimestamp = Lude.Nothing,
      actionsEnabled = Lude.Nothing,
      insufficientDataActions = Lude.Nothing,
      stateReason = Lude.Nothing,
      stateReasonData = Lude.Nothing,
      dimensions = Lude.Nothing,
      alarmARN = Lude.Nothing,
      alarmActions = Lude.Nothing,
      unit = Lude.Nothing,
      statistic = Lude.Nothing,
      extendedStatistic = Lude.Nothing
    }

-- | The name of the alarm.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maAlarmName :: Lens.Lens' MetricAlarm (Lude.Maybe Lude.Text)
maAlarmName = Lens.lens (alarmName :: MetricAlarm -> Lude.Maybe Lude.Text) (\s a -> s {alarmName = a} :: MetricAlarm)
{-# DEPRECATED maAlarmName "Use generic-lens or generic-optics with 'alarmName' instead." #-}

-- | The time stamp of the last update to the alarm state.
--
-- /Note:/ Consider using 'stateUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maStateUpdatedTimestamp :: Lens.Lens' MetricAlarm (Lude.Maybe Lude.DateTime)
maStateUpdatedTimestamp = Lens.lens (stateUpdatedTimestamp :: MetricAlarm -> Lude.Maybe Lude.DateTime) (\s a -> s {stateUpdatedTimestamp = a} :: MetricAlarm)
{-# DEPRECATED maStateUpdatedTimestamp "Use generic-lens or generic-optics with 'stateUpdatedTimestamp' instead." #-}

-- | An array of MetricDataQuery structures, used in an alarm based on a metric math expression. Each structure either retrieves a metric or performs a math expression. One item in the Metrics array is the math expression that the alarm watches. This expression by designated by having @ReturnData@ set to true.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maMetrics :: Lens.Lens' MetricAlarm (Lude.Maybe [MetricDataQuery])
maMetrics = Lens.lens (metrics :: MetricAlarm -> Lude.Maybe [MetricDataQuery]) (\s a -> s {metrics = a} :: MetricAlarm)
{-# DEPRECATED maMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | Sets how this alarm is to handle missing data points. If this parameter is omitted, the default behavior of @missing@ is used.
--
-- /Note:/ Consider using 'treatMissingData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maTreatMissingData :: Lens.Lens' MetricAlarm (Lude.Maybe Lude.Text)
maTreatMissingData = Lens.lens (treatMissingData :: MetricAlarm -> Lude.Maybe Lude.Text) (\s a -> s {treatMissingData = a} :: MetricAlarm)
{-# DEPRECATED maTreatMissingData "Use generic-lens or generic-optics with 'treatMissingData' instead." #-}

-- | The period, in seconds, over which the statistic is applied.
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maPeriod :: Lens.Lens' MetricAlarm (Lude.Maybe Lude.Natural)
maPeriod = Lens.lens (period :: MetricAlarm -> Lude.Maybe Lude.Natural) (\s a -> s {period = a} :: MetricAlarm)
{-# DEPRECATED maPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

-- | The description of the alarm.
--
-- /Note:/ Consider using 'alarmDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maAlarmDescription :: Lens.Lens' MetricAlarm (Lude.Maybe Lude.Text)
maAlarmDescription = Lens.lens (alarmDescription :: MetricAlarm -> Lude.Maybe Lude.Text) (\s a -> s {alarmDescription = a} :: MetricAlarm)
{-# DEPRECATED maAlarmDescription "Use generic-lens or generic-optics with 'alarmDescription' instead." #-}

-- | The number of periods over which data is compared to the specified threshold.
--
-- /Note:/ Consider using 'evaluationPeriods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maEvaluationPeriods :: Lens.Lens' MetricAlarm (Lude.Maybe Lude.Natural)
maEvaluationPeriods = Lens.lens (evaluationPeriods :: MetricAlarm -> Lude.Maybe Lude.Natural) (\s a -> s {evaluationPeriods = a} :: MetricAlarm)
{-# DEPRECATED maEvaluationPeriods "Use generic-lens or generic-optics with 'evaluationPeriods' instead." #-}

-- | The name of the metric associated with the alarm, if this is an alarm based on a single metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maMetricName :: Lens.Lens' MetricAlarm (Lude.Maybe Lude.Text)
maMetricName = Lens.lens (metricName :: MetricAlarm -> Lude.Maybe Lude.Text) (\s a -> s {metricName = a} :: MetricAlarm)
{-# DEPRECATED maMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The namespace of the metric associated with the alarm.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maNamespace :: Lens.Lens' MetricAlarm (Lude.Maybe Lude.Text)
maNamespace = Lens.lens (namespace :: MetricAlarm -> Lude.Maybe Lude.Text) (\s a -> s {namespace = a} :: MetricAlarm)
{-# DEPRECATED maNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | In an alarm based on an anomaly detection model, this is the ID of the @ANOMALY_DETECTION_BAND@ function used as the threshold for the alarm.
--
-- /Note:/ Consider using 'thresholdMetricId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maThresholdMetricId :: Lens.Lens' MetricAlarm (Lude.Maybe Lude.Text)
maThresholdMetricId = Lens.lens (thresholdMetricId :: MetricAlarm -> Lude.Maybe Lude.Text) (\s a -> s {thresholdMetricId = a} :: MetricAlarm)
{-# DEPRECATED maThresholdMetricId "Use generic-lens or generic-optics with 'thresholdMetricId' instead." #-}

-- | The arithmetic operation to use when comparing the specified statistic and threshold. The specified statistic value is used as the first operand.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maComparisonOperator :: Lens.Lens' MetricAlarm (Lude.Maybe ComparisonOperator)
maComparisonOperator = Lens.lens (comparisonOperator :: MetricAlarm -> Lude.Maybe ComparisonOperator) (\s a -> s {comparisonOperator = a} :: MetricAlarm)
{-# DEPRECATED maComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | The actions to execute when this alarm transitions to the @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'okActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maOKActions :: Lens.Lens' MetricAlarm (Lude.Maybe [Lude.Text])
maOKActions = Lens.lens (okActions :: MetricAlarm -> Lude.Maybe [Lude.Text]) (\s a -> s {okActions = a} :: MetricAlarm)
{-# DEPRECATED maOKActions "Use generic-lens or generic-optics with 'okActions' instead." #-}

-- | Used only for alarms based on percentiles. If @ignore@ , the alarm state does not change during periods with too few data points to be statistically significant. If @evaluate@ or this parameter is not used, the alarm is always evaluated and possibly changes state no matter how many data points are available.
--
-- /Note:/ Consider using 'evaluateLowSampleCountPercentile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maEvaluateLowSampleCountPercentile :: Lens.Lens' MetricAlarm (Lude.Maybe Lude.Text)
maEvaluateLowSampleCountPercentile = Lens.lens (evaluateLowSampleCountPercentile :: MetricAlarm -> Lude.Maybe Lude.Text) (\s a -> s {evaluateLowSampleCountPercentile = a} :: MetricAlarm)
{-# DEPRECATED maEvaluateLowSampleCountPercentile "Use generic-lens or generic-optics with 'evaluateLowSampleCountPercentile' instead." #-}

-- | The state value for the alarm.
--
-- /Note:/ Consider using 'stateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maStateValue :: Lens.Lens' MetricAlarm (Lude.Maybe StateValue)
maStateValue = Lens.lens (stateValue :: MetricAlarm -> Lude.Maybe StateValue) (\s a -> s {stateValue = a} :: MetricAlarm)
{-# DEPRECATED maStateValue "Use generic-lens or generic-optics with 'stateValue' instead." #-}

-- | The number of data points that must be breaching to trigger the alarm.
--
-- /Note:/ Consider using 'datapointsToAlarm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maDatapointsToAlarm :: Lens.Lens' MetricAlarm (Lude.Maybe Lude.Natural)
maDatapointsToAlarm = Lens.lens (datapointsToAlarm :: MetricAlarm -> Lude.Maybe Lude.Natural) (\s a -> s {datapointsToAlarm = a} :: MetricAlarm)
{-# DEPRECATED maDatapointsToAlarm "Use generic-lens or generic-optics with 'datapointsToAlarm' instead." #-}

-- | The value to compare with the specified statistic.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maThreshold :: Lens.Lens' MetricAlarm (Lude.Maybe Lude.Double)
maThreshold = Lens.lens (threshold :: MetricAlarm -> Lude.Maybe Lude.Double) (\s a -> s {threshold = a} :: MetricAlarm)
{-# DEPRECATED maThreshold "Use generic-lens or generic-optics with 'threshold' instead." #-}

-- | The time stamp of the last update to the alarm configuration.
--
-- /Note:/ Consider using 'alarmConfigurationUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maAlarmConfigurationUpdatedTimestamp :: Lens.Lens' MetricAlarm (Lude.Maybe Lude.DateTime)
maAlarmConfigurationUpdatedTimestamp = Lens.lens (alarmConfigurationUpdatedTimestamp :: MetricAlarm -> Lude.Maybe Lude.DateTime) (\s a -> s {alarmConfigurationUpdatedTimestamp = a} :: MetricAlarm)
{-# DEPRECATED maAlarmConfigurationUpdatedTimestamp "Use generic-lens or generic-optics with 'alarmConfigurationUpdatedTimestamp' instead." #-}

-- | Indicates whether actions should be executed during any changes to the alarm state.
--
-- /Note:/ Consider using 'actionsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maActionsEnabled :: Lens.Lens' MetricAlarm (Lude.Maybe Lude.Bool)
maActionsEnabled = Lens.lens (actionsEnabled :: MetricAlarm -> Lude.Maybe Lude.Bool) (\s a -> s {actionsEnabled = a} :: MetricAlarm)
{-# DEPRECATED maActionsEnabled "Use generic-lens or generic-optics with 'actionsEnabled' instead." #-}

-- | The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'insufficientDataActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maInsufficientDataActions :: Lens.Lens' MetricAlarm (Lude.Maybe [Lude.Text])
maInsufficientDataActions = Lens.lens (insufficientDataActions :: MetricAlarm -> Lude.Maybe [Lude.Text]) (\s a -> s {insufficientDataActions = a} :: MetricAlarm)
{-# DEPRECATED maInsufficientDataActions "Use generic-lens or generic-optics with 'insufficientDataActions' instead." #-}

-- | An explanation for the alarm state, in text format.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maStateReason :: Lens.Lens' MetricAlarm (Lude.Maybe Lude.Text)
maStateReason = Lens.lens (stateReason :: MetricAlarm -> Lude.Maybe Lude.Text) (\s a -> s {stateReason = a} :: MetricAlarm)
{-# DEPRECATED maStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | An explanation for the alarm state, in JSON format.
--
-- /Note:/ Consider using 'stateReasonData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maStateReasonData :: Lens.Lens' MetricAlarm (Lude.Maybe Lude.Text)
maStateReasonData = Lens.lens (stateReasonData :: MetricAlarm -> Lude.Maybe Lude.Text) (\s a -> s {stateReasonData = a} :: MetricAlarm)
{-# DEPRECATED maStateReasonData "Use generic-lens or generic-optics with 'stateReasonData' instead." #-}

-- | The dimensions for the metric associated with the alarm.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maDimensions :: Lens.Lens' MetricAlarm (Lude.Maybe [Dimension])
maDimensions = Lens.lens (dimensions :: MetricAlarm -> Lude.Maybe [Dimension]) (\s a -> s {dimensions = a} :: MetricAlarm)
{-# DEPRECATED maDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The Amazon Resource Name (ARN) of the alarm.
--
-- /Note:/ Consider using 'alarmARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maAlarmARN :: Lens.Lens' MetricAlarm (Lude.Maybe Lude.Text)
maAlarmARN = Lens.lens (alarmARN :: MetricAlarm -> Lude.Maybe Lude.Text) (\s a -> s {alarmARN = a} :: MetricAlarm)
{-# DEPRECATED maAlarmARN "Use generic-lens or generic-optics with 'alarmARN' instead." #-}

-- | The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'alarmActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maAlarmActions :: Lens.Lens' MetricAlarm (Lude.Maybe [Lude.Text])
maAlarmActions = Lens.lens (alarmActions :: MetricAlarm -> Lude.Maybe [Lude.Text]) (\s a -> s {alarmActions = a} :: MetricAlarm)
{-# DEPRECATED maAlarmActions "Use generic-lens or generic-optics with 'alarmActions' instead." #-}

-- | The unit of the metric associated with the alarm.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maUnit :: Lens.Lens' MetricAlarm (Lude.Maybe StandardUnit)
maUnit = Lens.lens (unit :: MetricAlarm -> Lude.Maybe StandardUnit) (\s a -> s {unit = a} :: MetricAlarm)
{-# DEPRECATED maUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

-- | The statistic for the metric associated with the alarm, other than percentile. For percentile statistics, use @ExtendedStatistic@ .
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maStatistic :: Lens.Lens' MetricAlarm (Lude.Maybe Statistic)
maStatistic = Lens.lens (statistic :: MetricAlarm -> Lude.Maybe Statistic) (\s a -> s {statistic = a} :: MetricAlarm)
{-# DEPRECATED maStatistic "Use generic-lens or generic-optics with 'statistic' instead." #-}

-- | The percentile statistic for the metric associated with the alarm. Specify a value between p0.0 and p100.
--
-- /Note:/ Consider using 'extendedStatistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maExtendedStatistic :: Lens.Lens' MetricAlarm (Lude.Maybe Lude.Text)
maExtendedStatistic = Lens.lens (extendedStatistic :: MetricAlarm -> Lude.Maybe Lude.Text) (\s a -> s {extendedStatistic = a} :: MetricAlarm)
{-# DEPRECATED maExtendedStatistic "Use generic-lens or generic-optics with 'extendedStatistic' instead." #-}

instance Lude.FromXML MetricAlarm where
  parseXML x =
    MetricAlarm'
      Lude.<$> (x Lude..@? "AlarmName")
      Lude.<*> (x Lude..@? "StateUpdatedTimestamp")
      Lude.<*> ( x Lude..@? "Metrics" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "TreatMissingData")
      Lude.<*> (x Lude..@? "Period")
      Lude.<*> (x Lude..@? "AlarmDescription")
      Lude.<*> (x Lude..@? "EvaluationPeriods")
      Lude.<*> (x Lude..@? "MetricName")
      Lude.<*> (x Lude..@? "Namespace")
      Lude.<*> (x Lude..@? "ThresholdMetricId")
      Lude.<*> (x Lude..@? "ComparisonOperator")
      Lude.<*> ( x Lude..@? "OKActions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "EvaluateLowSampleCountPercentile")
      Lude.<*> (x Lude..@? "StateValue")
      Lude.<*> (x Lude..@? "DatapointsToAlarm")
      Lude.<*> (x Lude..@? "Threshold")
      Lude.<*> (x Lude..@? "AlarmConfigurationUpdatedTimestamp")
      Lude.<*> (x Lude..@? "ActionsEnabled")
      Lude.<*> ( x Lude..@? "InsufficientDataActions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "StateReason")
      Lude.<*> (x Lude..@? "StateReasonData")
      Lude.<*> ( x Lude..@? "Dimensions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "AlarmArn")
      Lude.<*> ( x Lude..@? "AlarmActions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "Unit")
      Lude.<*> (x Lude..@? "Statistic")
      Lude.<*> (x Lude..@? "ExtendedStatistic")
