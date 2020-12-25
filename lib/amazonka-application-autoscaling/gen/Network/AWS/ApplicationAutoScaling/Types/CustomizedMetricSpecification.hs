{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.CustomizedMetricSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.CustomizedMetricSpecification
  ( CustomizedMetricSpecification (..),

    -- * Smart constructor
    mkCustomizedMetricSpecification,

    -- * Lenses
    cmsMetricName,
    cmsNamespace,
    cmsStatistic,
    cmsDimensions,
    cmsUnit,
  )
where

import qualified Network.AWS.ApplicationAutoScaling.Types.MetricDimension as Types
import qualified Network.AWS.ApplicationAutoScaling.Types.MetricName as Types
import qualified Network.AWS.ApplicationAutoScaling.Types.MetricStatistic as Types
import qualified Network.AWS.ApplicationAutoScaling.Types.Namespace as Types
import qualified Network.AWS.ApplicationAutoScaling.Types.Unit as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a CloudWatch metric of your choosing for a target tracking scaling policy to use with Application Auto Scaling.
--
-- For information about the available metrics for a service, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/aws-services-cloudwatch-metrics.html AWS Services That Publish CloudWatch Metrics> in the /Amazon CloudWatch User Guide/ .
-- To create your customized metric specification:
--
--     * Add values for each required parameter from CloudWatch. You can use an existing metric, or a new metric that you create. To use your own metric, you must first publish the metric to CloudWatch. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html Publish Custom Metrics> in the /Amazon CloudWatch User Guide/ .
--
--
--     * Choose a metric that changes proportionally with capacity. The value of the metric should increase or decrease in inverse proportion to the number of capacity units. That is, the value of the metric should decrease when capacity increases, and increase when capacity decreases.
--
--
-- For more information about CloudWatch, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html Amazon CloudWatch Concepts> .
--
-- /See:/ 'mkCustomizedMetricSpecification' smart constructor.
data CustomizedMetricSpecification = CustomizedMetricSpecification'
  { -- | The name of the metric.
    metricName :: Types.MetricName,
    -- | The namespace of the metric.
    namespace :: Types.Namespace,
    -- | The statistic of the metric.
    statistic :: Types.MetricStatistic,
    -- | The dimensions of the metric.
    --
    -- Conditional: If you published your metric with dimensions, you must specify the same dimensions in your scaling policy.
    dimensions :: Core.Maybe [Types.MetricDimension],
    -- | The unit of the metric.
    unit :: Core.Maybe Types.Unit
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomizedMetricSpecification' value with any optional fields omitted.
mkCustomizedMetricSpecification ::
  -- | 'metricName'
  Types.MetricName ->
  -- | 'namespace'
  Types.Namespace ->
  -- | 'statistic'
  Types.MetricStatistic ->
  CustomizedMetricSpecification
mkCustomizedMetricSpecification metricName namespace statistic =
  CustomizedMetricSpecification'
    { metricName,
      namespace,
      statistic,
      dimensions = Core.Nothing,
      unit = Core.Nothing
    }

-- | The name of the metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsMetricName :: Lens.Lens' CustomizedMetricSpecification Types.MetricName
cmsMetricName = Lens.field @"metricName"
{-# DEPRECATED cmsMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The namespace of the metric.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsNamespace :: Lens.Lens' CustomizedMetricSpecification Types.Namespace
cmsNamespace = Lens.field @"namespace"
{-# DEPRECATED cmsNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The statistic of the metric.
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsStatistic :: Lens.Lens' CustomizedMetricSpecification Types.MetricStatistic
cmsStatistic = Lens.field @"statistic"
{-# DEPRECATED cmsStatistic "Use generic-lens or generic-optics with 'statistic' instead." #-}

-- | The dimensions of the metric.
--
-- Conditional: If you published your metric with dimensions, you must specify the same dimensions in your scaling policy.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsDimensions :: Lens.Lens' CustomizedMetricSpecification (Core.Maybe [Types.MetricDimension])
cmsDimensions = Lens.field @"dimensions"
{-# DEPRECATED cmsDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The unit of the metric.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsUnit :: Lens.Lens' CustomizedMetricSpecification (Core.Maybe Types.Unit)
cmsUnit = Lens.field @"unit"
{-# DEPRECATED cmsUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

instance Core.FromJSON CustomizedMetricSpecification where
  toJSON CustomizedMetricSpecification {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("MetricName" Core..= metricName),
            Core.Just ("Namespace" Core..= namespace),
            Core.Just ("Statistic" Core..= statistic),
            ("Dimensions" Core..=) Core.<$> dimensions,
            ("Unit" Core..=) Core.<$> unit
          ]
      )

instance Core.FromJSON CustomizedMetricSpecification where
  parseJSON =
    Core.withObject "CustomizedMetricSpecification" Core.$
      \x ->
        CustomizedMetricSpecification'
          Core.<$> (x Core..: "MetricName")
          Core.<*> (x Core..: "Namespace")
          Core.<*> (x Core..: "Statistic")
          Core.<*> (x Core..:? "Dimensions")
          Core.<*> (x Core..:? "Unit")
