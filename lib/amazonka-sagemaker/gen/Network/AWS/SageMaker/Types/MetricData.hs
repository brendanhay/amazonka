{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MetricData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MetricData
  ( MetricData (..),

    -- * Smart constructor
    mkMetricData,

    -- * Lenses
    mdMetricName,
    mdValue,
    mdTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The name, value, and date and time of a metric that was emitted to Amazon CloudWatch.
--
-- /See:/ 'mkMetricData' smart constructor.
data MetricData = MetricData'
  { metricName :: Lude.Maybe Lude.Text,
    value :: Lude.Maybe Lude.Double,
    timestamp :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricData' with the minimum fields required to make a request.
--
-- * 'metricName' - The name of the metric.
-- * 'timestamp' - The date and time that the algorithm emitted the metric.
-- * 'value' - The value of the metric.
mkMetricData ::
  MetricData
mkMetricData =
  MetricData'
    { metricName = Lude.Nothing,
      value = Lude.Nothing,
      timestamp = Lude.Nothing
    }

-- | The name of the metric.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdMetricName :: Lens.Lens' MetricData (Lude.Maybe Lude.Text)
mdMetricName = Lens.lens (metricName :: MetricData -> Lude.Maybe Lude.Text) (\s a -> s {metricName = a} :: MetricData)
{-# DEPRECATED mdMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The value of the metric.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdValue :: Lens.Lens' MetricData (Lude.Maybe Lude.Double)
mdValue = Lens.lens (value :: MetricData -> Lude.Maybe Lude.Double) (\s a -> s {value = a} :: MetricData)
{-# DEPRECATED mdValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The date and time that the algorithm emitted the metric.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdTimestamp :: Lens.Lens' MetricData (Lude.Maybe Lude.Timestamp)
mdTimestamp = Lens.lens (timestamp :: MetricData -> Lude.Maybe Lude.Timestamp) (\s a -> s {timestamp = a} :: MetricData)
{-# DEPRECATED mdTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromJSON MetricData where
  parseJSON =
    Lude.withObject
      "MetricData"
      ( \x ->
          MetricData'
            Lude.<$> (x Lude..:? "MetricName")
            Lude.<*> (x Lude..:? "Value")
            Lude.<*> (x Lude..:? "Timestamp")
      )
