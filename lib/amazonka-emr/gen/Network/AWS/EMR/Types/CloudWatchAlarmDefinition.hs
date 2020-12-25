{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.CloudWatchAlarmDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.CloudWatchAlarmDefinition
  ( CloudWatchAlarmDefinition (..),

    -- * Smart constructor
    mkCloudWatchAlarmDefinition,

    -- * Lenses
    cwadComparisonOperator,
    cwadMetricName,
    cwadPeriod,
    cwadThreshold,
    cwadDimensions,
    cwadEvaluationPeriods,
    cwadNamespace,
    cwadStatistic,
    cwadUnit,
  )
where

import qualified Network.AWS.EMR.Types.ComparisonOperator as Types
import qualified Network.AWS.EMR.Types.MetricDimension as Types
import qualified Network.AWS.EMR.Types.Statistic as Types
import qualified Network.AWS.EMR.Types.String as Types
import qualified Network.AWS.EMR.Types.Unit as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The definition of a CloudWatch metric alarm, which determines when an automatic scaling activity is triggered. When the defined alarm conditions are satisfied, scaling activity begins.
--
-- /See:/ 'mkCloudWatchAlarmDefinition' smart constructor.
data CloudWatchAlarmDefinition = CloudWatchAlarmDefinition'
  { -- | Determines how the metric specified by @MetricName@ is compared to the value specified by @Threshold@ .
    comparisonOperator :: Types.ComparisonOperator,
    -- | The name of the CloudWatch metric that is watched to determine an alarm condition.
    metricName :: Types.String,
    -- | The period, in seconds, over which the statistic is applied. EMR CloudWatch metrics are emitted every five minutes (300 seconds), so if an EMR CloudWatch metric is specified, specify @300@ .
    period :: Core.Int,
    -- | The value against which the specified statistic is compared.
    threshold :: Core.Double,
    -- | A CloudWatch metric dimension.
    dimensions :: Core.Maybe [Types.MetricDimension],
    -- | The number of periods, in five-minute increments, during which the alarm condition must exist before the alarm triggers automatic scaling activity. The default value is @1@ .
    evaluationPeriods :: Core.Maybe Core.Int,
    -- | The namespace for the CloudWatch metric. The default is @AWS/ElasticMapReduce@ .
    namespace :: Core.Maybe Types.String,
    -- | The statistic to apply to the metric associated with the alarm. The default is @AVERAGE@ .
    statistic :: Core.Maybe Types.Statistic,
    -- | The unit of measure associated with the CloudWatch metric being watched. The value specified for @Unit@ must correspond to the units specified in the CloudWatch metric.
    unit :: Core.Maybe Types.Unit
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudWatchAlarmDefinition' value with any optional fields omitted.
mkCloudWatchAlarmDefinition ::
  -- | 'comparisonOperator'
  Types.ComparisonOperator ->
  -- | 'metricName'
  Types.String ->
  -- | 'period'
  Core.Int ->
  -- | 'threshold'
  Core.Double ->
  CloudWatchAlarmDefinition
mkCloudWatchAlarmDefinition
  comparisonOperator
  metricName
  period
  threshold =
    CloudWatchAlarmDefinition'
      { comparisonOperator,
        metricName,
        period,
        threshold,
        dimensions = Core.Nothing,
        evaluationPeriods = Core.Nothing,
        namespace = Core.Nothing,
        statistic = Core.Nothing,
        unit = Core.Nothing
      }

-- | Determines how the metric specified by @MetricName@ is compared to the value specified by @Threshold@ .
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwadComparisonOperator :: Lens.Lens' CloudWatchAlarmDefinition Types.ComparisonOperator
cwadComparisonOperator = Lens.field @"comparisonOperator"
{-# DEPRECATED cwadComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | The name of the CloudWatch metric that is watched to determine an alarm condition.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwadMetricName :: Lens.Lens' CloudWatchAlarmDefinition Types.String
cwadMetricName = Lens.field @"metricName"
{-# DEPRECATED cwadMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The period, in seconds, over which the statistic is applied. EMR CloudWatch metrics are emitted every five minutes (300 seconds), so if an EMR CloudWatch metric is specified, specify @300@ .
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwadPeriod :: Lens.Lens' CloudWatchAlarmDefinition Core.Int
cwadPeriod = Lens.field @"period"
{-# DEPRECATED cwadPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

-- | The value against which the specified statistic is compared.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwadThreshold :: Lens.Lens' CloudWatchAlarmDefinition Core.Double
cwadThreshold = Lens.field @"threshold"
{-# DEPRECATED cwadThreshold "Use generic-lens or generic-optics with 'threshold' instead." #-}

-- | A CloudWatch metric dimension.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwadDimensions :: Lens.Lens' CloudWatchAlarmDefinition (Core.Maybe [Types.MetricDimension])
cwadDimensions = Lens.field @"dimensions"
{-# DEPRECATED cwadDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The number of periods, in five-minute increments, during which the alarm condition must exist before the alarm triggers automatic scaling activity. The default value is @1@ .
--
-- /Note:/ Consider using 'evaluationPeriods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwadEvaluationPeriods :: Lens.Lens' CloudWatchAlarmDefinition (Core.Maybe Core.Int)
cwadEvaluationPeriods = Lens.field @"evaluationPeriods"
{-# DEPRECATED cwadEvaluationPeriods "Use generic-lens or generic-optics with 'evaluationPeriods' instead." #-}

-- | The namespace for the CloudWatch metric. The default is @AWS/ElasticMapReduce@ .
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwadNamespace :: Lens.Lens' CloudWatchAlarmDefinition (Core.Maybe Types.String)
cwadNamespace = Lens.field @"namespace"
{-# DEPRECATED cwadNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The statistic to apply to the metric associated with the alarm. The default is @AVERAGE@ .
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwadStatistic :: Lens.Lens' CloudWatchAlarmDefinition (Core.Maybe Types.Statistic)
cwadStatistic = Lens.field @"statistic"
{-# DEPRECATED cwadStatistic "Use generic-lens or generic-optics with 'statistic' instead." #-}

-- | The unit of measure associated with the CloudWatch metric being watched. The value specified for @Unit@ must correspond to the units specified in the CloudWatch metric.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwadUnit :: Lens.Lens' CloudWatchAlarmDefinition (Core.Maybe Types.Unit)
cwadUnit = Lens.field @"unit"
{-# DEPRECATED cwadUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

instance Core.FromJSON CloudWatchAlarmDefinition where
  toJSON CloudWatchAlarmDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ComparisonOperator" Core..= comparisonOperator),
            Core.Just ("MetricName" Core..= metricName),
            Core.Just ("Period" Core..= period),
            Core.Just ("Threshold" Core..= threshold),
            ("Dimensions" Core..=) Core.<$> dimensions,
            ("EvaluationPeriods" Core..=) Core.<$> evaluationPeriods,
            ("Namespace" Core..=) Core.<$> namespace,
            ("Statistic" Core..=) Core.<$> statistic,
            ("Unit" Core..=) Core.<$> unit
          ]
      )

instance Core.FromJSON CloudWatchAlarmDefinition where
  parseJSON =
    Core.withObject "CloudWatchAlarmDefinition" Core.$
      \x ->
        CloudWatchAlarmDefinition'
          Core.<$> (x Core..: "ComparisonOperator")
          Core.<*> (x Core..: "MetricName")
          Core.<*> (x Core..: "Period")
          Core.<*> (x Core..: "Threshold")
          Core.<*> (x Core..:? "Dimensions")
          Core.<*> (x Core..:? "EvaluationPeriods")
          Core.<*> (x Core..:? "Namespace")
          Core.<*> (x Core..:? "Statistic")
          Core.<*> (x Core..:? "Unit")
