{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.CustomizedLoadMetricSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.CustomizedLoadMetricSpecification
  ( CustomizedLoadMetricSpecification (..),

    -- * Smart constructor
    mkCustomizedLoadMetricSpecification,

    -- * Lenses
    clmsMetricName,
    clmsNamespace,
    clmsDimensions,
    clmsUnit,
    clmsStatistic,
  )
where

import Network.AWS.AutoScalingPlans.Types.MetricDimension
import Network.AWS.AutoScalingPlans.Types.MetricStatistic
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a CloudWatch metric of your choosing that can be used for predictive scaling.
--
-- For predictive scaling to work with a customized load metric specification, AWS Auto Scaling needs access to the @Sum@ and @Average@ statistics that CloudWatch computes from metric data. Statistics are calculations used to aggregate data over specified time periods.
-- When you choose a load metric, make sure that the required @Sum@ and @Average@ statistics for your metric are available in CloudWatch and that they provide relevant data for predictive scaling. The @Sum@ statistic must represent the total load on the resource, and the @Average@ statistic must represent the average load per capacity unit of the resource. For example, there is a metric that counts the number of requests processed by your Auto Scaling group. If the @Sum@ statistic represents the total request count processed by the group, then the @Average@ statistic for the specified metric must represent the average request count processed by each instance of the group.
-- For information about terminology, available metrics, or how to publish new metrics, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html Amazon CloudWatch Concepts> in the /Amazon CloudWatch User Guide/ .
--
-- /See:/ 'mkCustomizedLoadMetricSpecification' smart constructor.
data CustomizedLoadMetricSpecification = CustomizedLoadMetricSpecification'
  { -- | The name of the metric.
    metricName :: Lude.Text,
    -- | The namespace of the metric.
    namespace :: Lude.Text,
    -- | The dimensions of the metric.
    --
    -- Conditional: If you published your metric with dimensions, you must specify the same dimensions in your customized load metric specification.
    dimensions :: Lude.Maybe [MetricDimension],
    -- | The unit of the metric.
    unit :: Lude.Maybe Lude.Text,
    -- | The statistic of the metric. Currently, the value must always be @Sum@ .
    statistic :: MetricStatistic
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomizedLoadMetricSpecification' with the minimum fields required to make a request.
--
-- * 'metricName' - The name of the metric.
-- * 'namespace' - The namespace of the metric.
-- * 'dimensions' - The dimensions of the metric.
--
-- Conditional: If you published your metric with dimensions, you must specify the same dimensions in your customized load metric specification.
-- * 'unit' - The unit of the metric.
-- * 'statistic' - The statistic of the metric. Currently, the value must always be @Sum@ .
mkCustomizedLoadMetricSpecification ::
  -- | 'metricName'
  Lude.Text ->
  -- | 'namespace'
  Lude.Text ->
  -- | 'statistic'
  MetricStatistic ->
  CustomizedLoadMetricSpecification
mkCustomizedLoadMetricSpecification
  pMetricName_
  pNamespace_
  pStatistic_ =
    CustomizedLoadMetricSpecification'
      { metricName = pMetricName_,
        namespace = pNamespace_,
        dimensions = Lude.Nothing,
        unit = Lude.Nothing,
        statistic = pStatistic_
      }

-- | The name of the metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmsMetricName :: Lens.Lens' CustomizedLoadMetricSpecification Lude.Text
clmsMetricName = Lens.lens (metricName :: CustomizedLoadMetricSpecification -> Lude.Text) (\s a -> s {metricName = a} :: CustomizedLoadMetricSpecification)
{-# DEPRECATED clmsMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The namespace of the metric.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmsNamespace :: Lens.Lens' CustomizedLoadMetricSpecification Lude.Text
clmsNamespace = Lens.lens (namespace :: CustomizedLoadMetricSpecification -> Lude.Text) (\s a -> s {namespace = a} :: CustomizedLoadMetricSpecification)
{-# DEPRECATED clmsNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The dimensions of the metric.
--
-- Conditional: If you published your metric with dimensions, you must specify the same dimensions in your customized load metric specification.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmsDimensions :: Lens.Lens' CustomizedLoadMetricSpecification (Lude.Maybe [MetricDimension])
clmsDimensions = Lens.lens (dimensions :: CustomizedLoadMetricSpecification -> Lude.Maybe [MetricDimension]) (\s a -> s {dimensions = a} :: CustomizedLoadMetricSpecification)
{-# DEPRECATED clmsDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The unit of the metric.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmsUnit :: Lens.Lens' CustomizedLoadMetricSpecification (Lude.Maybe Lude.Text)
clmsUnit = Lens.lens (unit :: CustomizedLoadMetricSpecification -> Lude.Maybe Lude.Text) (\s a -> s {unit = a} :: CustomizedLoadMetricSpecification)
{-# DEPRECATED clmsUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

-- | The statistic of the metric. Currently, the value must always be @Sum@ .
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmsStatistic :: Lens.Lens' CustomizedLoadMetricSpecification MetricStatistic
clmsStatistic = Lens.lens (statistic :: CustomizedLoadMetricSpecification -> MetricStatistic) (\s a -> s {statistic = a} :: CustomizedLoadMetricSpecification)
{-# DEPRECATED clmsStatistic "Use generic-lens or generic-optics with 'statistic' instead." #-}

instance Lude.FromJSON CustomizedLoadMetricSpecification where
  parseJSON =
    Lude.withObject
      "CustomizedLoadMetricSpecification"
      ( \x ->
          CustomizedLoadMetricSpecification'
            Lude.<$> (x Lude..: "MetricName")
            Lude.<*> (x Lude..: "Namespace")
            Lude.<*> (x Lude..:? "Dimensions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Unit")
            Lude.<*> (x Lude..: "Statistic")
      )

instance Lude.ToJSON CustomizedLoadMetricSpecification where
  toJSON CustomizedLoadMetricSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("MetricName" Lude..= metricName),
            Lude.Just ("Namespace" Lude..= namespace),
            ("Dimensions" Lude..=) Lude.<$> dimensions,
            ("Unit" Lude..=) Lude.<$> unit,
            Lude.Just ("Statistic" Lude..= statistic)
          ]
      )
