{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.CustomizedMetricSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.CustomizedMetricSpecification
  ( CustomizedMetricSpecification (..)
  -- * Smart constructor
  , mkCustomizedMetricSpecification
  -- * Lenses
  , cmsMetricName
  , cmsNamespace
  , cmsStatistic
  , cmsDimensions
  , cmsUnit
  ) where

import qualified Network.AWS.AutoScaling.Types.MetricDimension as Types
import qualified Network.AWS.AutoScaling.Types.MetricName as Types
import qualified Network.AWS.AutoScaling.Types.MetricStatistic as Types
import qualified Network.AWS.AutoScaling.Types.Namespace as Types
import qualified Network.AWS.AutoScaling.Types.Unit as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a CloudWatch metric of your choosing for a target tracking scaling policy to use with Amazon EC2 Auto Scaling.
--
-- To create your customized metric specification:
--
--     * Add values for each required parameter from CloudWatch. You can use an existing metric, or a new metric that you create. To use your own metric, you must first publish the metric to CloudWatch. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html Publish Custom Metrics> in the /Amazon CloudWatch User Guide/ .
--
--
--     * Choose a metric that changes proportionally with capacity. The value of the metric should increase or decrease in inverse proportion to the number of capacity units. That is, the value of the metric should decrease when capacity increases.
--
--
-- For more information about CloudWatch, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html Amazon CloudWatch Concepts> .
--
-- /See:/ 'mkCustomizedMetricSpecification' smart constructor.
data CustomizedMetricSpecification = CustomizedMetricSpecification'
  { metricName :: Types.MetricName
    -- ^ The name of the metric.
  , namespace :: Types.Namespace
    -- ^ The namespace of the metric.
  , statistic :: Types.MetricStatistic
    -- ^ The statistic of the metric.
  , dimensions :: Core.Maybe [Types.MetricDimension]
    -- ^ The dimensions of the metric.
--
-- Conditional: If you published your metric with dimensions, you must specify the same dimensions in your scaling policy.
  , unit :: Core.Maybe Types.Unit
    -- ^ The unit of the metric.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomizedMetricSpecification' value with any optional fields omitted.
mkCustomizedMetricSpecification
    :: Types.MetricName -- ^ 'metricName'
    -> Types.Namespace -- ^ 'namespace'
    -> Types.MetricStatistic -- ^ 'statistic'
    -> CustomizedMetricSpecification
mkCustomizedMetricSpecification metricName namespace statistic
  = CustomizedMetricSpecification'{metricName, namespace, statistic,
                                   dimensions = Core.Nothing, unit = Core.Nothing}

-- | The name of the metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsMetricName :: Lens.Lens' CustomizedMetricSpecification Types.MetricName
cmsMetricName = Lens.field @"metricName"
{-# INLINEABLE cmsMetricName #-}
{-# DEPRECATED metricName "Use generic-lens or generic-optics with 'metricName' instead"  #-}

-- | The namespace of the metric.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsNamespace :: Lens.Lens' CustomizedMetricSpecification Types.Namespace
cmsNamespace = Lens.field @"namespace"
{-# INLINEABLE cmsNamespace #-}
{-# DEPRECATED namespace "Use generic-lens or generic-optics with 'namespace' instead"  #-}

-- | The statistic of the metric.
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsStatistic :: Lens.Lens' CustomizedMetricSpecification Types.MetricStatistic
cmsStatistic = Lens.field @"statistic"
{-# INLINEABLE cmsStatistic #-}
{-# DEPRECATED statistic "Use generic-lens or generic-optics with 'statistic' instead"  #-}

-- | The dimensions of the metric.
--
-- Conditional: If you published your metric with dimensions, you must specify the same dimensions in your scaling policy.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsDimensions :: Lens.Lens' CustomizedMetricSpecification (Core.Maybe [Types.MetricDimension])
cmsDimensions = Lens.field @"dimensions"
{-# INLINEABLE cmsDimensions #-}
{-# DEPRECATED dimensions "Use generic-lens or generic-optics with 'dimensions' instead"  #-}

-- | The unit of the metric.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsUnit :: Lens.Lens' CustomizedMetricSpecification (Core.Maybe Types.Unit)
cmsUnit = Lens.field @"unit"
{-# INLINEABLE cmsUnit #-}
{-# DEPRECATED unit "Use generic-lens or generic-optics with 'unit' instead"  #-}

instance Core.ToQuery CustomizedMetricSpecification where
        toQuery CustomizedMetricSpecification{..}
          = Core.toQueryPair "MetricName" metricName Core.<>
              Core.toQueryPair "Namespace" namespace
              Core.<> Core.toQueryPair "Statistic" statistic
              Core.<>
              Core.toQueryPair "Dimensions"
                (Core.maybe Core.mempty (Core.toQueryList "member") dimensions)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Unit") unit

instance Core.FromXML CustomizedMetricSpecification where
        parseXML x
          = CustomizedMetricSpecification' Core.<$>
              (x Core..@ "MetricName") Core.<*> x Core..@ "Namespace" Core.<*>
                x Core..@ "Statistic"
                Core.<*>
                x Core..@? "Dimensions" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "Unit"
