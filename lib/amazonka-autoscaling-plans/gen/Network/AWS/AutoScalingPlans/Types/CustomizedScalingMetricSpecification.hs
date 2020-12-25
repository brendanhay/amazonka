{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.CustomizedScalingMetricSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.CustomizedScalingMetricSpecification
  ( CustomizedScalingMetricSpecification (..),

    -- * Smart constructor
    mkCustomizedScalingMetricSpecification,

    -- * Lenses
    csmsMetricName,
    csmsNamespace,
    csmsStatistic,
    csmsDimensions,
    csmsUnit,
  )
where

import qualified Network.AWS.AutoScalingPlans.Types.MetricDimension as Types
import qualified Network.AWS.AutoScalingPlans.Types.MetricName as Types
import qualified Network.AWS.AutoScalingPlans.Types.MetricNamespace as Types
import qualified Network.AWS.AutoScalingPlans.Types.MetricStatistic as Types
import qualified Network.AWS.AutoScalingPlans.Types.MetricUnit as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a CloudWatch metric of your choosing that can be used for dynamic scaling as part of a target tracking scaling policy.
--
-- To create your customized scaling metric specification:
--
--     * Add values for each required parameter from CloudWatch. You can use an existing metric, or a new metric that you create. To use your own metric, you must first publish the metric to CloudWatch. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html Publish Custom Metrics> in the /Amazon CloudWatch User Guide/ .
--
--
--     * Choose a metric that changes proportionally with capacity. The value of the metric should increase or decrease in inverse proportion to the number of capacity units. That is, the value of the metric should decrease when capacity increases.
--
--
-- For more information about CloudWatch, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html Amazon CloudWatch Concepts> .
--
-- /See:/ 'mkCustomizedScalingMetricSpecification' smart constructor.
data CustomizedScalingMetricSpecification = CustomizedScalingMetricSpecification'
  { -- | The name of the metric.
    metricName :: Types.MetricName,
    -- | The namespace of the metric.
    namespace :: Types.MetricNamespace,
    -- | The statistic of the metric.
    statistic :: Types.MetricStatistic,
    -- | The dimensions of the metric.
    --
    -- Conditional: If you published your metric with dimensions, you must specify the same dimensions in your customized scaling metric specification.
    dimensions :: Core.Maybe [Types.MetricDimension],
    -- | The unit of the metric.
    unit :: Core.Maybe Types.MetricUnit
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomizedScalingMetricSpecification' value with any optional fields omitted.
mkCustomizedScalingMetricSpecification ::
  -- | 'metricName'
  Types.MetricName ->
  -- | 'namespace'
  Types.MetricNamespace ->
  -- | 'statistic'
  Types.MetricStatistic ->
  CustomizedScalingMetricSpecification
mkCustomizedScalingMetricSpecification
  metricName
  namespace
  statistic =
    CustomizedScalingMetricSpecification'
      { metricName,
        namespace,
        statistic,
        dimensions = Core.Nothing,
        unit = Core.Nothing
      }

-- | The name of the metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmsMetricName :: Lens.Lens' CustomizedScalingMetricSpecification Types.MetricName
csmsMetricName = Lens.field @"metricName"
{-# DEPRECATED csmsMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The namespace of the metric.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmsNamespace :: Lens.Lens' CustomizedScalingMetricSpecification Types.MetricNamespace
csmsNamespace = Lens.field @"namespace"
{-# DEPRECATED csmsNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The statistic of the metric.
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmsStatistic :: Lens.Lens' CustomizedScalingMetricSpecification Types.MetricStatistic
csmsStatistic = Lens.field @"statistic"
{-# DEPRECATED csmsStatistic "Use generic-lens or generic-optics with 'statistic' instead." #-}

-- | The dimensions of the metric.
--
-- Conditional: If you published your metric with dimensions, you must specify the same dimensions in your customized scaling metric specification.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmsDimensions :: Lens.Lens' CustomizedScalingMetricSpecification (Core.Maybe [Types.MetricDimension])
csmsDimensions = Lens.field @"dimensions"
{-# DEPRECATED csmsDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The unit of the metric.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmsUnit :: Lens.Lens' CustomizedScalingMetricSpecification (Core.Maybe Types.MetricUnit)
csmsUnit = Lens.field @"unit"
{-# DEPRECATED csmsUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

instance Core.FromJSON CustomizedScalingMetricSpecification where
  toJSON CustomizedScalingMetricSpecification {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("MetricName" Core..= metricName),
            Core.Just ("Namespace" Core..= namespace),
            Core.Just ("Statistic" Core..= statistic),
            ("Dimensions" Core..=) Core.<$> dimensions,
            ("Unit" Core..=) Core.<$> unit
          ]
      )

instance Core.FromJSON CustomizedScalingMetricSpecification where
  parseJSON =
    Core.withObject "CustomizedScalingMetricSpecification" Core.$
      \x ->
        CustomizedScalingMetricSpecification'
          Core.<$> (x Core..: "MetricName")
          Core.<*> (x Core..: "Namespace")
          Core.<*> (x Core..: "Statistic")
          Core.<*> (x Core..:? "Dimensions")
          Core.<*> (x Core..:? "Unit")
