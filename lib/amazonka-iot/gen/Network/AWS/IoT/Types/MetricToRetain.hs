-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MetricToRetain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MetricToRetain
  ( MetricToRetain (..),

    -- * Smart constructor
    mkMetricToRetain,

    -- * Lenses
    mtrMetricDimension,
    mtrMetric,
  )
where

import Network.AWS.IoT.Types.MetricDimension
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The metric you want to retain. Dimensions are optional.
--
-- /See:/ 'mkMetricToRetain' smart constructor.
data MetricToRetain = MetricToRetain'
  { metricDimension ::
      Lude.Maybe MetricDimension,
    metric :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricToRetain' with the minimum fields required to make a request.
--
-- * 'metric' - What is measured by the behavior.
-- * 'metricDimension' - The dimension of a metric.
mkMetricToRetain ::
  -- | 'metric'
  Lude.Text ->
  MetricToRetain
mkMetricToRetain pMetric_ =
  MetricToRetain'
    { metricDimension = Lude.Nothing,
      metric = pMetric_
    }

-- | The dimension of a metric.
--
-- /Note:/ Consider using 'metricDimension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtrMetricDimension :: Lens.Lens' MetricToRetain (Lude.Maybe MetricDimension)
mtrMetricDimension = Lens.lens (metricDimension :: MetricToRetain -> Lude.Maybe MetricDimension) (\s a -> s {metricDimension = a} :: MetricToRetain)
{-# DEPRECATED mtrMetricDimension "Use generic-lens or generic-optics with 'metricDimension' instead." #-}

-- | What is measured by the behavior.
--
-- /Note:/ Consider using 'metric' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtrMetric :: Lens.Lens' MetricToRetain Lude.Text
mtrMetric = Lens.lens (metric :: MetricToRetain -> Lude.Text) (\s a -> s {metric = a} :: MetricToRetain)
{-# DEPRECATED mtrMetric "Use generic-lens or generic-optics with 'metric' instead." #-}

instance Lude.FromJSON MetricToRetain where
  parseJSON =
    Lude.withObject
      "MetricToRetain"
      ( \x ->
          MetricToRetain'
            Lude.<$> (x Lude..:? "metricDimension") Lude.<*> (x Lude..: "metric")
      )

instance Lude.ToJSON MetricToRetain where
  toJSON MetricToRetain' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("metricDimension" Lude..=) Lude.<$> metricDimension,
            Lude.Just ("metric" Lude..= metric)
          ]
      )
