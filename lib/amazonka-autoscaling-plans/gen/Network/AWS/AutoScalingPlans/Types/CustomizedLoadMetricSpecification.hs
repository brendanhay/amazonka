{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.CustomizedLoadMetricSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScalingPlans.Types.CustomizedLoadMetricSpecification
  ( CustomizedLoadMetricSpecification (..)
  -- * Smart constructor
  , mkCustomizedLoadMetricSpecification
  -- * Lenses
  , clmsMetricName
  , clmsNamespace
  , clmsStatistic
  , clmsDimensions
  , clmsUnit
  ) where

import qualified Network.AWS.AutoScalingPlans.Types.MetricDimension as Types
import qualified Network.AWS.AutoScalingPlans.Types.MetricName as Types
import qualified Network.AWS.AutoScalingPlans.Types.MetricStatistic as Types
import qualified Network.AWS.AutoScalingPlans.Types.Namespace as Types
import qualified Network.AWS.AutoScalingPlans.Types.Unit as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a CloudWatch metric of your choosing that can be used for predictive scaling. 
--
-- For predictive scaling to work with a customized load metric specification, AWS Auto Scaling needs access to the @Sum@ and @Average@ statistics that CloudWatch computes from metric data. Statistics are calculations used to aggregate data over specified time periods.
-- When you choose a load metric, make sure that the required @Sum@ and @Average@ statistics for your metric are available in CloudWatch and that they provide relevant data for predictive scaling. The @Sum@ statistic must represent the total load on the resource, and the @Average@ statistic must represent the average load per capacity unit of the resource. For example, there is a metric that counts the number of requests processed by your Auto Scaling group. If the @Sum@ statistic represents the total request count processed by the group, then the @Average@ statistic for the specified metric must represent the average request count processed by each instance of the group.
-- For information about terminology, available metrics, or how to publish new metrics, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html Amazon CloudWatch Concepts> in the /Amazon CloudWatch User Guide/ . 
--
-- /See:/ 'mkCustomizedLoadMetricSpecification' smart constructor.
data CustomizedLoadMetricSpecification = CustomizedLoadMetricSpecification'
  { metricName :: Types.MetricName
    -- ^ The name of the metric.
  , namespace :: Types.Namespace
    -- ^ The namespace of the metric.
  , statistic :: Types.MetricStatistic
    -- ^ The statistic of the metric. Currently, the value must always be @Sum@ . 
  , dimensions :: Core.Maybe [Types.MetricDimension]
    -- ^ The dimensions of the metric.
--
-- Conditional: If you published your metric with dimensions, you must specify the same dimensions in your customized load metric specification.
  , unit :: Core.Maybe Types.Unit
    -- ^ The unit of the metric.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomizedLoadMetricSpecification' value with any optional fields omitted.
mkCustomizedLoadMetricSpecification
    :: Types.MetricName -- ^ 'metricName'
    -> Types.Namespace -- ^ 'namespace'
    -> Types.MetricStatistic -- ^ 'statistic'
    -> CustomizedLoadMetricSpecification
mkCustomizedLoadMetricSpecification metricName namespace statistic
  = CustomizedLoadMetricSpecification'{metricName, namespace,
                                       statistic, dimensions = Core.Nothing, unit = Core.Nothing}

-- | The name of the metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmsMetricName :: Lens.Lens' CustomizedLoadMetricSpecification Types.MetricName
clmsMetricName = Lens.field @"metricName"
{-# INLINEABLE clmsMetricName #-}
{-# DEPRECATED metricName "Use generic-lens or generic-optics with 'metricName' instead"  #-}

-- | The namespace of the metric.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmsNamespace :: Lens.Lens' CustomizedLoadMetricSpecification Types.Namespace
clmsNamespace = Lens.field @"namespace"
{-# INLINEABLE clmsNamespace #-}
{-# DEPRECATED namespace "Use generic-lens or generic-optics with 'namespace' instead"  #-}

-- | The statistic of the metric. Currently, the value must always be @Sum@ . 
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmsStatistic :: Lens.Lens' CustomizedLoadMetricSpecification Types.MetricStatistic
clmsStatistic = Lens.field @"statistic"
{-# INLINEABLE clmsStatistic #-}
{-# DEPRECATED statistic "Use generic-lens or generic-optics with 'statistic' instead"  #-}

-- | The dimensions of the metric.
--
-- Conditional: If you published your metric with dimensions, you must specify the same dimensions in your customized load metric specification.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmsDimensions :: Lens.Lens' CustomizedLoadMetricSpecification (Core.Maybe [Types.MetricDimension])
clmsDimensions = Lens.field @"dimensions"
{-# INLINEABLE clmsDimensions #-}
{-# DEPRECATED dimensions "Use generic-lens or generic-optics with 'dimensions' instead"  #-}

-- | The unit of the metric.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmsUnit :: Lens.Lens' CustomizedLoadMetricSpecification (Core.Maybe Types.Unit)
clmsUnit = Lens.field @"unit"
{-# INLINEABLE clmsUnit #-}
{-# DEPRECATED unit "Use generic-lens or generic-optics with 'unit' instead"  #-}

instance Core.FromJSON CustomizedLoadMetricSpecification where
        toJSON CustomizedLoadMetricSpecification{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("MetricName" Core..= metricName),
                  Core.Just ("Namespace" Core..= namespace),
                  Core.Just ("Statistic" Core..= statistic),
                  ("Dimensions" Core..=) Core.<$> dimensions,
                  ("Unit" Core..=) Core.<$> unit])

instance Core.FromJSON CustomizedLoadMetricSpecification where
        parseJSON
          = Core.withObject "CustomizedLoadMetricSpecification" Core.$
              \ x ->
                CustomizedLoadMetricSpecification' Core.<$>
                  (x Core..: "MetricName") Core.<*> x Core..: "Namespace" Core.<*>
                    x Core..: "Statistic"
                    Core.<*> x Core..:? "Dimensions"
                    Core.<*> x Core..:? "Unit"
