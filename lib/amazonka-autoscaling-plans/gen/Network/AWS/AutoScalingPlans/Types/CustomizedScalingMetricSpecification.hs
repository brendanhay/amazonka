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
    csmsDimensions,
    csmsUnit,
    csmsMetricName,
    csmsNamespace,
    csmsStatistic,
  )
where

import Network.AWS.AutoScalingPlans.Types.MetricDimension
import Network.AWS.AutoScalingPlans.Types.MetricStatistic
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { dimensions ::
      Lude.Maybe
        [MetricDimension],
    unit ::
      Lude.Maybe
        Lude.Text,
    metricName ::
      Lude.Text,
    namespace ::
      Lude.Text,
    statistic ::
      MetricStatistic
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomizedScalingMetricSpecification' with the minimum fields required to make a request.
--
-- * 'dimensions' - The dimensions of the metric.
--
-- Conditional: If you published your metric with dimensions, you must specify the same dimensions in your customized scaling metric specification.
-- * 'metricName' - The name of the metric.
-- * 'namespace' - The namespace of the metric.
-- * 'statistic' - The statistic of the metric.
-- * 'unit' - The unit of the metric.
mkCustomizedScalingMetricSpecification ::
  -- | 'metricName'
  Lude.Text ->
  -- | 'namespace'
  Lude.Text ->
  -- | 'statistic'
  MetricStatistic ->
  CustomizedScalingMetricSpecification
mkCustomizedScalingMetricSpecification
  pMetricName_
  pNamespace_
  pStatistic_ =
    CustomizedScalingMetricSpecification'
      { dimensions = Lude.Nothing,
        unit = Lude.Nothing,
        metricName = pMetricName_,
        namespace = pNamespace_,
        statistic = pStatistic_
      }

-- | The dimensions of the metric.
--
-- Conditional: If you published your metric with dimensions, you must specify the same dimensions in your customized scaling metric specification.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmsDimensions :: Lens.Lens' CustomizedScalingMetricSpecification (Lude.Maybe [MetricDimension])
csmsDimensions = Lens.lens (dimensions :: CustomizedScalingMetricSpecification -> Lude.Maybe [MetricDimension]) (\s a -> s {dimensions = a} :: CustomizedScalingMetricSpecification)
{-# DEPRECATED csmsDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The unit of the metric.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmsUnit :: Lens.Lens' CustomizedScalingMetricSpecification (Lude.Maybe Lude.Text)
csmsUnit = Lens.lens (unit :: CustomizedScalingMetricSpecification -> Lude.Maybe Lude.Text) (\s a -> s {unit = a} :: CustomizedScalingMetricSpecification)
{-# DEPRECATED csmsUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

-- | The name of the metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmsMetricName :: Lens.Lens' CustomizedScalingMetricSpecification Lude.Text
csmsMetricName = Lens.lens (metricName :: CustomizedScalingMetricSpecification -> Lude.Text) (\s a -> s {metricName = a} :: CustomizedScalingMetricSpecification)
{-# DEPRECATED csmsMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The namespace of the metric.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmsNamespace :: Lens.Lens' CustomizedScalingMetricSpecification Lude.Text
csmsNamespace = Lens.lens (namespace :: CustomizedScalingMetricSpecification -> Lude.Text) (\s a -> s {namespace = a} :: CustomizedScalingMetricSpecification)
{-# DEPRECATED csmsNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The statistic of the metric.
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmsStatistic :: Lens.Lens' CustomizedScalingMetricSpecification MetricStatistic
csmsStatistic = Lens.lens (statistic :: CustomizedScalingMetricSpecification -> MetricStatistic) (\s a -> s {statistic = a} :: CustomizedScalingMetricSpecification)
{-# DEPRECATED csmsStatistic "Use generic-lens or generic-optics with 'statistic' instead." #-}

instance Lude.FromJSON CustomizedScalingMetricSpecification where
  parseJSON =
    Lude.withObject
      "CustomizedScalingMetricSpecification"
      ( \x ->
          CustomizedScalingMetricSpecification'
            Lude.<$> (x Lude..:? "Dimensions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Unit")
            Lude.<*> (x Lude..: "MetricName")
            Lude.<*> (x Lude..: "Namespace")
            Lude.<*> (x Lude..: "Statistic")
      )

instance Lude.ToJSON CustomizedScalingMetricSpecification where
  toJSON CustomizedScalingMetricSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Dimensions" Lude..=) Lude.<$> dimensions,
            ("Unit" Lude..=) Lude.<$> unit,
            Lude.Just ("MetricName" Lude..= metricName),
            Lude.Just ("Namespace" Lude..= namespace),
            Lude.Just ("Statistic" Lude..= statistic)
          ]
      )
