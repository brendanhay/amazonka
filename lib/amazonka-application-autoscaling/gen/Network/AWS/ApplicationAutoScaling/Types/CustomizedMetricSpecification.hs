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
    cmsDimensions,
    cmsUnit,
    cmsMetricName,
    cmsNamespace,
    cmsStatistic,
  )
where

import Network.AWS.ApplicationAutoScaling.Types.MetricDimension
import Network.AWS.ApplicationAutoScaling.Types.MetricStatistic
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { dimensions ::
      Lude.Maybe [MetricDimension],
    unit :: Lude.Maybe Lude.Text,
    metricName :: Lude.Text,
    namespace :: Lude.Text,
    statistic :: MetricStatistic
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomizedMetricSpecification' with the minimum fields required to make a request.
--
-- * 'dimensions' - The dimensions of the metric.
--
-- Conditional: If you published your metric with dimensions, you must specify the same dimensions in your scaling policy.
-- * 'metricName' - The name of the metric.
-- * 'namespace' - The namespace of the metric.
-- * 'statistic' - The statistic of the metric.
-- * 'unit' - The unit of the metric.
mkCustomizedMetricSpecification ::
  -- | 'metricName'
  Lude.Text ->
  -- | 'namespace'
  Lude.Text ->
  -- | 'statistic'
  MetricStatistic ->
  CustomizedMetricSpecification
mkCustomizedMetricSpecification
  pMetricName_
  pNamespace_
  pStatistic_ =
    CustomizedMetricSpecification'
      { dimensions = Lude.Nothing,
        unit = Lude.Nothing,
        metricName = pMetricName_,
        namespace = pNamespace_,
        statistic = pStatistic_
      }

-- | The dimensions of the metric.
--
-- Conditional: If you published your metric with dimensions, you must specify the same dimensions in your scaling policy.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsDimensions :: Lens.Lens' CustomizedMetricSpecification (Lude.Maybe [MetricDimension])
cmsDimensions = Lens.lens (dimensions :: CustomizedMetricSpecification -> Lude.Maybe [MetricDimension]) (\s a -> s {dimensions = a} :: CustomizedMetricSpecification)
{-# DEPRECATED cmsDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The unit of the metric.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsUnit :: Lens.Lens' CustomizedMetricSpecification (Lude.Maybe Lude.Text)
cmsUnit = Lens.lens (unit :: CustomizedMetricSpecification -> Lude.Maybe Lude.Text) (\s a -> s {unit = a} :: CustomizedMetricSpecification)
{-# DEPRECATED cmsUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

-- | The name of the metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsMetricName :: Lens.Lens' CustomizedMetricSpecification Lude.Text
cmsMetricName = Lens.lens (metricName :: CustomizedMetricSpecification -> Lude.Text) (\s a -> s {metricName = a} :: CustomizedMetricSpecification)
{-# DEPRECATED cmsMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The namespace of the metric.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsNamespace :: Lens.Lens' CustomizedMetricSpecification Lude.Text
cmsNamespace = Lens.lens (namespace :: CustomizedMetricSpecification -> Lude.Text) (\s a -> s {namespace = a} :: CustomizedMetricSpecification)
{-# DEPRECATED cmsNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The statistic of the metric.
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsStatistic :: Lens.Lens' CustomizedMetricSpecification MetricStatistic
cmsStatistic = Lens.lens (statistic :: CustomizedMetricSpecification -> MetricStatistic) (\s a -> s {statistic = a} :: CustomizedMetricSpecification)
{-# DEPRECATED cmsStatistic "Use generic-lens or generic-optics with 'statistic' instead." #-}

instance Lude.FromJSON CustomizedMetricSpecification where
  parseJSON =
    Lude.withObject
      "CustomizedMetricSpecification"
      ( \x ->
          CustomizedMetricSpecification'
            Lude.<$> (x Lude..:? "Dimensions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Unit")
            Lude.<*> (x Lude..: "MetricName")
            Lude.<*> (x Lude..: "Namespace")
            Lude.<*> (x Lude..: "Statistic")
      )

instance Lude.ToJSON CustomizedMetricSpecification where
  toJSON CustomizedMetricSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Dimensions" Lude..=) Lude.<$> dimensions,
            ("Unit" Lude..=) Lude.<$> unit,
            Lude.Just ("MetricName" Lude..= metricName),
            Lude.Just ("Namespace" Lude..= namespace),
            Lude.Just ("Statistic" Lude..= statistic)
          ]
      )
