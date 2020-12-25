{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.CloudWatchAlarmConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.CloudWatchAlarmConfiguration
  ( CloudWatchAlarmConfiguration (..),

    -- * Smart constructor
    mkCloudWatchAlarmConfiguration,

    -- * Lenses
    cwacEvaluationPeriods,
    cwacThreshold,
    cwacComparisonOperator,
    cwacPeriod,
    cwacMetricName,
    cwacNamespace,
    cwacStatistic,
    cwacDimensions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.ComparisonOperator as Types
import qualified Network.AWS.Route53.Types.Dimension as Types
import qualified Network.AWS.Route53.Types.MetricName as Types
import qualified Network.AWS.Route53.Types.Namespace as Types
import qualified Network.AWS.Route53.Types.Statistic as Types

-- | A complex type that contains information about the CloudWatch alarm that Amazon Route 53 is monitoring for this health check.
--
-- /See:/ 'mkCloudWatchAlarmConfiguration' smart constructor.
data CloudWatchAlarmConfiguration = CloudWatchAlarmConfiguration'
  { -- | For the metric that the CloudWatch alarm is associated with, the number of periods that the metric is compared to the threshold.
    evaluationPeriods :: Core.Natural,
    -- | For the metric that the CloudWatch alarm is associated with, the value the metric is compared with.
    threshold :: Core.Double,
    -- | For the metric that the CloudWatch alarm is associated with, the arithmetic operation that is used for the comparison.
    comparisonOperator :: Types.ComparisonOperator,
    -- | For the metric that the CloudWatch alarm is associated with, the duration of one evaluation period in seconds.
    period :: Core.Natural,
    -- | The name of the CloudWatch metric that the alarm is associated with.
    metricName :: Types.MetricName,
    -- | The namespace of the metric that the alarm is associated with. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference> in the /Amazon CloudWatch User Guide/ .
    namespace :: Types.Namespace,
    -- | For the metric that the CloudWatch alarm is associated with, the statistic that is applied to the metric.
    statistic :: Types.Statistic,
    -- | For the metric that the CloudWatch alarm is associated with, a complex type that contains information about the dimensions for the metric. For information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference> in the /Amazon CloudWatch User Guide/ .
    dimensions :: Core.Maybe [Types.Dimension]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudWatchAlarmConfiguration' value with any optional fields omitted.
mkCloudWatchAlarmConfiguration ::
  -- | 'evaluationPeriods'
  Core.Natural ->
  -- | 'threshold'
  Core.Double ->
  -- | 'comparisonOperator'
  Types.ComparisonOperator ->
  -- | 'period'
  Core.Natural ->
  -- | 'metricName'
  Types.MetricName ->
  -- | 'namespace'
  Types.Namespace ->
  -- | 'statistic'
  Types.Statistic ->
  CloudWatchAlarmConfiguration
mkCloudWatchAlarmConfiguration
  evaluationPeriods
  threshold
  comparisonOperator
  period
  metricName
  namespace
  statistic =
    CloudWatchAlarmConfiguration'
      { evaluationPeriods,
        threshold,
        comparisonOperator,
        period,
        metricName,
        namespace,
        statistic,
        dimensions = Core.Nothing
      }

-- | For the metric that the CloudWatch alarm is associated with, the number of periods that the metric is compared to the threshold.
--
-- /Note:/ Consider using 'evaluationPeriods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwacEvaluationPeriods :: Lens.Lens' CloudWatchAlarmConfiguration Core.Natural
cwacEvaluationPeriods = Lens.field @"evaluationPeriods"
{-# DEPRECATED cwacEvaluationPeriods "Use generic-lens or generic-optics with 'evaluationPeriods' instead." #-}

-- | For the metric that the CloudWatch alarm is associated with, the value the metric is compared with.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwacThreshold :: Lens.Lens' CloudWatchAlarmConfiguration Core.Double
cwacThreshold = Lens.field @"threshold"
{-# DEPRECATED cwacThreshold "Use generic-lens or generic-optics with 'threshold' instead." #-}

-- | For the metric that the CloudWatch alarm is associated with, the arithmetic operation that is used for the comparison.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwacComparisonOperator :: Lens.Lens' CloudWatchAlarmConfiguration Types.ComparisonOperator
cwacComparisonOperator = Lens.field @"comparisonOperator"
{-# DEPRECATED cwacComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | For the metric that the CloudWatch alarm is associated with, the duration of one evaluation period in seconds.
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwacPeriod :: Lens.Lens' CloudWatchAlarmConfiguration Core.Natural
cwacPeriod = Lens.field @"period"
{-# DEPRECATED cwacPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

-- | The name of the CloudWatch metric that the alarm is associated with.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwacMetricName :: Lens.Lens' CloudWatchAlarmConfiguration Types.MetricName
cwacMetricName = Lens.field @"metricName"
{-# DEPRECATED cwacMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The namespace of the metric that the alarm is associated with. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference> in the /Amazon CloudWatch User Guide/ .
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwacNamespace :: Lens.Lens' CloudWatchAlarmConfiguration Types.Namespace
cwacNamespace = Lens.field @"namespace"
{-# DEPRECATED cwacNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | For the metric that the CloudWatch alarm is associated with, the statistic that is applied to the metric.
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwacStatistic :: Lens.Lens' CloudWatchAlarmConfiguration Types.Statistic
cwacStatistic = Lens.field @"statistic"
{-# DEPRECATED cwacStatistic "Use generic-lens or generic-optics with 'statistic' instead." #-}

-- | For the metric that the CloudWatch alarm is associated with, a complex type that contains information about the dimensions for the metric. For information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference> in the /Amazon CloudWatch User Guide/ .
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwacDimensions :: Lens.Lens' CloudWatchAlarmConfiguration (Core.Maybe [Types.Dimension])
cwacDimensions = Lens.field @"dimensions"
{-# DEPRECATED cwacDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

instance Core.FromXML CloudWatchAlarmConfiguration where
  parseXML x =
    CloudWatchAlarmConfiguration'
      Core.<$> (x Core..@ "EvaluationPeriods")
      Core.<*> (x Core..@ "Threshold")
      Core.<*> (x Core..@ "ComparisonOperator")
      Core.<*> (x Core..@ "Period")
      Core.<*> (x Core..@ "MetricName")
      Core.<*> (x Core..@ "Namespace")
      Core.<*> (x Core..@ "Statistic")
      Core.<*> (x Core..@? "Dimensions" Core..<@> Core.parseXMLList "Dimension")
