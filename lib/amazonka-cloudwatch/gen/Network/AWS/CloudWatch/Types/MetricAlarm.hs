{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.MetricAlarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatch.Types.MetricAlarm
  ( MetricAlarm (..)
  -- * Smart constructor
  , mkMetricAlarm
  -- * Lenses
  , maActionsEnabled
  , maAlarmActions
  , maAlarmArn
  , maAlarmConfigurationUpdatedTimestamp
  , maAlarmDescription
  , maAlarmName
  , maComparisonOperator
  , maDatapointsToAlarm
  , maDimensions
  , maEvaluateLowSampleCountPercentile
  , maEvaluationPeriods
  , maExtendedStatistic
  , maInsufficientDataActions
  , maMetricName
  , maMetrics
  , maNamespace
  , maOKActions
  , maPeriod
  , maStateReason
  , maStateReasonData
  , maStateUpdatedTimestamp
  , maStateValue
  , maStatistic
  , maThreshold
  , maThresholdMetricId
  , maTreatMissingData
  , maUnit
  ) where

import qualified Network.AWS.CloudWatch.Types.AlarmArn as Types
import qualified Network.AWS.CloudWatch.Types.AlarmDescription as Types
import qualified Network.AWS.CloudWatch.Types.AlarmName as Types
import qualified Network.AWS.CloudWatch.Types.ComparisonOperator as Types
import qualified Network.AWS.CloudWatch.Types.Dimension as Types
import qualified Network.AWS.CloudWatch.Types.EvaluateLowSampleCountPercentile as Types
import qualified Network.AWS.CloudWatch.Types.ExtendedStatistic as Types
import qualified Network.AWS.CloudWatch.Types.MetricDataQuery as Types
import qualified Network.AWS.CloudWatch.Types.MetricName as Types
import qualified Network.AWS.CloudWatch.Types.Namespace as Types
import qualified Network.AWS.CloudWatch.Types.ResourceName as Types
import qualified Network.AWS.CloudWatch.Types.StandardUnit as Types
import qualified Network.AWS.CloudWatch.Types.StateReason as Types
import qualified Network.AWS.CloudWatch.Types.StateReasonData as Types
import qualified Network.AWS.CloudWatch.Types.StateValue as Types
import qualified Network.AWS.CloudWatch.Types.Statistic as Types
import qualified Network.AWS.CloudWatch.Types.ThresholdMetricId as Types
import qualified Network.AWS.CloudWatch.Types.TreatMissingData as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details about a metric alarm.
--
-- /See:/ 'mkMetricAlarm' smart constructor.
data MetricAlarm = MetricAlarm'
  { actionsEnabled :: Core.Maybe Core.Bool
    -- ^ Indicates whether actions should be executed during any changes to the alarm state.
  , alarmActions :: Core.Maybe [Types.ResourceName]
    -- ^ The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
  , alarmArn :: Core.Maybe Types.AlarmArn
    -- ^ The Amazon Resource Name (ARN) of the alarm.
  , alarmConfigurationUpdatedTimestamp :: Core.Maybe Core.UTCTime
    -- ^ The time stamp of the last update to the alarm configuration.
  , alarmDescription :: Core.Maybe Types.AlarmDescription
    -- ^ The description of the alarm.
  , alarmName :: Core.Maybe Types.AlarmName
    -- ^ The name of the alarm.
  , comparisonOperator :: Core.Maybe Types.ComparisonOperator
    -- ^ The arithmetic operation to use when comparing the specified statistic and threshold. The specified statistic value is used as the first operand.
  , datapointsToAlarm :: Core.Maybe Core.Natural
    -- ^ The number of data points that must be breaching to trigger the alarm.
  , dimensions :: Core.Maybe [Types.Dimension]
    -- ^ The dimensions for the metric associated with the alarm.
  , evaluateLowSampleCountPercentile :: Core.Maybe Types.EvaluateLowSampleCountPercentile
    -- ^ Used only for alarms based on percentiles. If @ignore@ , the alarm state does not change during periods with too few data points to be statistically significant. If @evaluate@ or this parameter is not used, the alarm is always evaluated and possibly changes state no matter how many data points are available.
  , evaluationPeriods :: Core.Maybe Core.Natural
    -- ^ The number of periods over which data is compared to the specified threshold.
  , extendedStatistic :: Core.Maybe Types.ExtendedStatistic
    -- ^ The percentile statistic for the metric associated with the alarm. Specify a value between p0.0 and p100.
  , insufficientDataActions :: Core.Maybe [Types.ResourceName]
    -- ^ The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
  , metricName :: Core.Maybe Types.MetricName
    -- ^ The name of the metric associated with the alarm, if this is an alarm based on a single metric.
  , metrics :: Core.Maybe [Types.MetricDataQuery]
    -- ^ An array of MetricDataQuery structures, used in an alarm based on a metric math expression. Each structure either retrieves a metric or performs a math expression. One item in the Metrics array is the math expression that the alarm watches. This expression by designated by having @ReturnData@ set to true.
  , namespace :: Core.Maybe Types.Namespace
    -- ^ The namespace of the metric associated with the alarm.
  , oKActions :: Core.Maybe [Types.ResourceName]
    -- ^ The actions to execute when this alarm transitions to the @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
  , period :: Core.Maybe Core.Natural
    -- ^ The period, in seconds, over which the statistic is applied.
  , stateReason :: Core.Maybe Types.StateReason
    -- ^ An explanation for the alarm state, in text format.
  , stateReasonData :: Core.Maybe Types.StateReasonData
    -- ^ An explanation for the alarm state, in JSON format.
  , stateUpdatedTimestamp :: Core.Maybe Core.UTCTime
    -- ^ The time stamp of the last update to the alarm state.
  , stateValue :: Core.Maybe Types.StateValue
    -- ^ The state value for the alarm.
  , statistic :: Core.Maybe Types.Statistic
    -- ^ The statistic for the metric associated with the alarm, other than percentile. For percentile statistics, use @ExtendedStatistic@ .
  , threshold :: Core.Maybe Core.Double
    -- ^ The value to compare with the specified statistic.
  , thresholdMetricId :: Core.Maybe Types.ThresholdMetricId
    -- ^ In an alarm based on an anomaly detection model, this is the ID of the @ANOMALY_DETECTION_BAND@ function used as the threshold for the alarm.
  , treatMissingData :: Core.Maybe Types.TreatMissingData
    -- ^ Sets how this alarm is to handle missing data points. If this parameter is omitted, the default behavior of @missing@ is used.
  , unit :: Core.Maybe Types.StandardUnit
    -- ^ The unit of the metric associated with the alarm.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'MetricAlarm' value with any optional fields omitted.
mkMetricAlarm
    :: MetricAlarm
mkMetricAlarm
  = MetricAlarm'{actionsEnabled = Core.Nothing,
                 alarmActions = Core.Nothing, alarmArn = Core.Nothing,
                 alarmConfigurationUpdatedTimestamp = Core.Nothing,
                 alarmDescription = Core.Nothing, alarmName = Core.Nothing,
                 comparisonOperator = Core.Nothing,
                 datapointsToAlarm = Core.Nothing, dimensions = Core.Nothing,
                 evaluateLowSampleCountPercentile = Core.Nothing,
                 evaluationPeriods = Core.Nothing, extendedStatistic = Core.Nothing,
                 insufficientDataActions = Core.Nothing, metricName = Core.Nothing,
                 metrics = Core.Nothing, namespace = Core.Nothing,
                 oKActions = Core.Nothing, period = Core.Nothing,
                 stateReason = Core.Nothing, stateReasonData = Core.Nothing,
                 stateUpdatedTimestamp = Core.Nothing, stateValue = Core.Nothing,
                 statistic = Core.Nothing, threshold = Core.Nothing,
                 thresholdMetricId = Core.Nothing, treatMissingData = Core.Nothing,
                 unit = Core.Nothing}

-- | Indicates whether actions should be executed during any changes to the alarm state.
--
-- /Note:/ Consider using 'actionsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maActionsEnabled :: Lens.Lens' MetricAlarm (Core.Maybe Core.Bool)
maActionsEnabled = Lens.field @"actionsEnabled"
{-# INLINEABLE maActionsEnabled #-}
{-# DEPRECATED actionsEnabled "Use generic-lens or generic-optics with 'actionsEnabled' instead"  #-}

-- | The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'alarmActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maAlarmActions :: Lens.Lens' MetricAlarm (Core.Maybe [Types.ResourceName])
maAlarmActions = Lens.field @"alarmActions"
{-# INLINEABLE maAlarmActions #-}
{-# DEPRECATED alarmActions "Use generic-lens or generic-optics with 'alarmActions' instead"  #-}

-- | The Amazon Resource Name (ARN) of the alarm.
--
-- /Note:/ Consider using 'alarmArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maAlarmArn :: Lens.Lens' MetricAlarm (Core.Maybe Types.AlarmArn)
maAlarmArn = Lens.field @"alarmArn"
{-# INLINEABLE maAlarmArn #-}
{-# DEPRECATED alarmArn "Use generic-lens or generic-optics with 'alarmArn' instead"  #-}

-- | The time stamp of the last update to the alarm configuration.
--
-- /Note:/ Consider using 'alarmConfigurationUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maAlarmConfigurationUpdatedTimestamp :: Lens.Lens' MetricAlarm (Core.Maybe Core.UTCTime)
maAlarmConfigurationUpdatedTimestamp = Lens.field @"alarmConfigurationUpdatedTimestamp"
{-# INLINEABLE maAlarmConfigurationUpdatedTimestamp #-}
{-# DEPRECATED alarmConfigurationUpdatedTimestamp "Use generic-lens or generic-optics with 'alarmConfigurationUpdatedTimestamp' instead"  #-}

-- | The description of the alarm.
--
-- /Note:/ Consider using 'alarmDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maAlarmDescription :: Lens.Lens' MetricAlarm (Core.Maybe Types.AlarmDescription)
maAlarmDescription = Lens.field @"alarmDescription"
{-# INLINEABLE maAlarmDescription #-}
{-# DEPRECATED alarmDescription "Use generic-lens or generic-optics with 'alarmDescription' instead"  #-}

-- | The name of the alarm.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maAlarmName :: Lens.Lens' MetricAlarm (Core.Maybe Types.AlarmName)
maAlarmName = Lens.field @"alarmName"
{-# INLINEABLE maAlarmName #-}
{-# DEPRECATED alarmName "Use generic-lens or generic-optics with 'alarmName' instead"  #-}

-- | The arithmetic operation to use when comparing the specified statistic and threshold. The specified statistic value is used as the first operand.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maComparisonOperator :: Lens.Lens' MetricAlarm (Core.Maybe Types.ComparisonOperator)
maComparisonOperator = Lens.field @"comparisonOperator"
{-# INLINEABLE maComparisonOperator #-}
{-# DEPRECATED comparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead"  #-}

-- | The number of data points that must be breaching to trigger the alarm.
--
-- /Note:/ Consider using 'datapointsToAlarm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maDatapointsToAlarm :: Lens.Lens' MetricAlarm (Core.Maybe Core.Natural)
maDatapointsToAlarm = Lens.field @"datapointsToAlarm"
{-# INLINEABLE maDatapointsToAlarm #-}
{-# DEPRECATED datapointsToAlarm "Use generic-lens or generic-optics with 'datapointsToAlarm' instead"  #-}

-- | The dimensions for the metric associated with the alarm.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maDimensions :: Lens.Lens' MetricAlarm (Core.Maybe [Types.Dimension])
maDimensions = Lens.field @"dimensions"
{-# INLINEABLE maDimensions #-}
{-# DEPRECATED dimensions "Use generic-lens or generic-optics with 'dimensions' instead"  #-}

-- | Used only for alarms based on percentiles. If @ignore@ , the alarm state does not change during periods with too few data points to be statistically significant. If @evaluate@ or this parameter is not used, the alarm is always evaluated and possibly changes state no matter how many data points are available.
--
-- /Note:/ Consider using 'evaluateLowSampleCountPercentile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maEvaluateLowSampleCountPercentile :: Lens.Lens' MetricAlarm (Core.Maybe Types.EvaluateLowSampleCountPercentile)
maEvaluateLowSampleCountPercentile = Lens.field @"evaluateLowSampleCountPercentile"
{-# INLINEABLE maEvaluateLowSampleCountPercentile #-}
{-# DEPRECATED evaluateLowSampleCountPercentile "Use generic-lens or generic-optics with 'evaluateLowSampleCountPercentile' instead"  #-}

-- | The number of periods over which data is compared to the specified threshold.
--
-- /Note:/ Consider using 'evaluationPeriods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maEvaluationPeriods :: Lens.Lens' MetricAlarm (Core.Maybe Core.Natural)
maEvaluationPeriods = Lens.field @"evaluationPeriods"
{-# INLINEABLE maEvaluationPeriods #-}
{-# DEPRECATED evaluationPeriods "Use generic-lens or generic-optics with 'evaluationPeriods' instead"  #-}

-- | The percentile statistic for the metric associated with the alarm. Specify a value between p0.0 and p100.
--
-- /Note:/ Consider using 'extendedStatistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maExtendedStatistic :: Lens.Lens' MetricAlarm (Core.Maybe Types.ExtendedStatistic)
maExtendedStatistic = Lens.field @"extendedStatistic"
{-# INLINEABLE maExtendedStatistic #-}
{-# DEPRECATED extendedStatistic "Use generic-lens or generic-optics with 'extendedStatistic' instead"  #-}

-- | The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'insufficientDataActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maInsufficientDataActions :: Lens.Lens' MetricAlarm (Core.Maybe [Types.ResourceName])
maInsufficientDataActions = Lens.field @"insufficientDataActions"
{-# INLINEABLE maInsufficientDataActions #-}
{-# DEPRECATED insufficientDataActions "Use generic-lens or generic-optics with 'insufficientDataActions' instead"  #-}

-- | The name of the metric associated with the alarm, if this is an alarm based on a single metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maMetricName :: Lens.Lens' MetricAlarm (Core.Maybe Types.MetricName)
maMetricName = Lens.field @"metricName"
{-# INLINEABLE maMetricName #-}
{-# DEPRECATED metricName "Use generic-lens or generic-optics with 'metricName' instead"  #-}

-- | An array of MetricDataQuery structures, used in an alarm based on a metric math expression. Each structure either retrieves a metric or performs a math expression. One item in the Metrics array is the math expression that the alarm watches. This expression by designated by having @ReturnData@ set to true.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maMetrics :: Lens.Lens' MetricAlarm (Core.Maybe [Types.MetricDataQuery])
maMetrics = Lens.field @"metrics"
{-# INLINEABLE maMetrics #-}
{-# DEPRECATED metrics "Use generic-lens or generic-optics with 'metrics' instead"  #-}

-- | The namespace of the metric associated with the alarm.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maNamespace :: Lens.Lens' MetricAlarm (Core.Maybe Types.Namespace)
maNamespace = Lens.field @"namespace"
{-# INLINEABLE maNamespace #-}
{-# DEPRECATED namespace "Use generic-lens or generic-optics with 'namespace' instead"  #-}

-- | The actions to execute when this alarm transitions to the @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'oKActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maOKActions :: Lens.Lens' MetricAlarm (Core.Maybe [Types.ResourceName])
maOKActions = Lens.field @"oKActions"
{-# INLINEABLE maOKActions #-}
{-# DEPRECATED oKActions "Use generic-lens or generic-optics with 'oKActions' instead"  #-}

-- | The period, in seconds, over which the statistic is applied.
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maPeriod :: Lens.Lens' MetricAlarm (Core.Maybe Core.Natural)
maPeriod = Lens.field @"period"
{-# INLINEABLE maPeriod #-}
{-# DEPRECATED period "Use generic-lens or generic-optics with 'period' instead"  #-}

-- | An explanation for the alarm state, in text format.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maStateReason :: Lens.Lens' MetricAlarm (Core.Maybe Types.StateReason)
maStateReason = Lens.field @"stateReason"
{-# INLINEABLE maStateReason #-}
{-# DEPRECATED stateReason "Use generic-lens or generic-optics with 'stateReason' instead"  #-}

-- | An explanation for the alarm state, in JSON format.
--
-- /Note:/ Consider using 'stateReasonData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maStateReasonData :: Lens.Lens' MetricAlarm (Core.Maybe Types.StateReasonData)
maStateReasonData = Lens.field @"stateReasonData"
{-# INLINEABLE maStateReasonData #-}
{-# DEPRECATED stateReasonData "Use generic-lens or generic-optics with 'stateReasonData' instead"  #-}

-- | The time stamp of the last update to the alarm state.
--
-- /Note:/ Consider using 'stateUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maStateUpdatedTimestamp :: Lens.Lens' MetricAlarm (Core.Maybe Core.UTCTime)
maStateUpdatedTimestamp = Lens.field @"stateUpdatedTimestamp"
{-# INLINEABLE maStateUpdatedTimestamp #-}
{-# DEPRECATED stateUpdatedTimestamp "Use generic-lens or generic-optics with 'stateUpdatedTimestamp' instead"  #-}

-- | The state value for the alarm.
--
-- /Note:/ Consider using 'stateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maStateValue :: Lens.Lens' MetricAlarm (Core.Maybe Types.StateValue)
maStateValue = Lens.field @"stateValue"
{-# INLINEABLE maStateValue #-}
{-# DEPRECATED stateValue "Use generic-lens or generic-optics with 'stateValue' instead"  #-}

-- | The statistic for the metric associated with the alarm, other than percentile. For percentile statistics, use @ExtendedStatistic@ .
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maStatistic :: Lens.Lens' MetricAlarm (Core.Maybe Types.Statistic)
maStatistic = Lens.field @"statistic"
{-# INLINEABLE maStatistic #-}
{-# DEPRECATED statistic "Use generic-lens or generic-optics with 'statistic' instead"  #-}

-- | The value to compare with the specified statistic.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maThreshold :: Lens.Lens' MetricAlarm (Core.Maybe Core.Double)
maThreshold = Lens.field @"threshold"
{-# INLINEABLE maThreshold #-}
{-# DEPRECATED threshold "Use generic-lens or generic-optics with 'threshold' instead"  #-}

-- | In an alarm based on an anomaly detection model, this is the ID of the @ANOMALY_DETECTION_BAND@ function used as the threshold for the alarm.
--
-- /Note:/ Consider using 'thresholdMetricId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maThresholdMetricId :: Lens.Lens' MetricAlarm (Core.Maybe Types.ThresholdMetricId)
maThresholdMetricId = Lens.field @"thresholdMetricId"
{-# INLINEABLE maThresholdMetricId #-}
{-# DEPRECATED thresholdMetricId "Use generic-lens or generic-optics with 'thresholdMetricId' instead"  #-}

-- | Sets how this alarm is to handle missing data points. If this parameter is omitted, the default behavior of @missing@ is used.
--
-- /Note:/ Consider using 'treatMissingData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maTreatMissingData :: Lens.Lens' MetricAlarm (Core.Maybe Types.TreatMissingData)
maTreatMissingData = Lens.field @"treatMissingData"
{-# INLINEABLE maTreatMissingData #-}
{-# DEPRECATED treatMissingData "Use generic-lens or generic-optics with 'treatMissingData' instead"  #-}

-- | The unit of the metric associated with the alarm.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maUnit :: Lens.Lens' MetricAlarm (Core.Maybe Types.StandardUnit)
maUnit = Lens.field @"unit"
{-# INLINEABLE maUnit #-}
{-# DEPRECATED unit "Use generic-lens or generic-optics with 'unit' instead"  #-}

instance Core.FromXML MetricAlarm where
        parseXML x
          = MetricAlarm' Core.<$>
              (x Core..@? "ActionsEnabled") Core.<*>
                x Core..@? "AlarmActions" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "AlarmArn"
                Core.<*> x Core..@? "AlarmConfigurationUpdatedTimestamp"
                Core.<*> x Core..@? "AlarmDescription"
                Core.<*> x Core..@? "AlarmName"
                Core.<*> x Core..@? "ComparisonOperator"
                Core.<*> x Core..@? "DatapointsToAlarm"
                Core.<*>
                x Core..@? "Dimensions" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "EvaluateLowSampleCountPercentile"
                Core.<*> x Core..@? "EvaluationPeriods"
                Core.<*> x Core..@? "ExtendedStatistic"
                Core.<*>
                x Core..@? "InsufficientDataActions" Core..<@>
                  Core.parseXMLList "member"
                Core.<*> x Core..@? "MetricName"
                Core.<*> x Core..@? "Metrics" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "Namespace"
                Core.<*>
                x Core..@? "OKActions" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "Period"
                Core.<*> x Core..@? "StateReason"
                Core.<*> x Core..@? "StateReasonData"
                Core.<*> x Core..@? "StateUpdatedTimestamp"
                Core.<*> x Core..@? "StateValue"
                Core.<*> x Core..@? "Statistic"
                Core.<*> x Core..@? "Threshold"
                Core.<*> x Core..@? "ThresholdMetricId"
                Core.<*> x Core..@? "TreatMissingData"
                Core.<*> x Core..@? "Unit"
