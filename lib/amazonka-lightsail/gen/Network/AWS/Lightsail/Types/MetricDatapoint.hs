{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.MetricDatapoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.MetricDatapoint
  ( MetricDatapoint (..),

    -- * Smart constructor
    mkMetricDatapoint,

    -- * Lenses
    mdSampleCount,
    mdMaximum,
    mdAverage,
    mdMinimum,
    mdSum,
    mdTimestamp,
    mdUnit,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.MetricUnit
import qualified Network.AWS.Prelude as Lude

-- | Describes the metric data point.
--
-- /See:/ 'mkMetricDatapoint' smart constructor.
data MetricDatapoint = MetricDatapoint'
  { -- | The sample count.
    sampleCount :: Lude.Maybe Lude.Double,
    -- | The maximum.
    maximum :: Lude.Maybe Lude.Double,
    -- | The average.
    average :: Lude.Maybe Lude.Double,
    -- | The minimum.
    minimum :: Lude.Maybe Lude.Double,
    -- | The sum.
    sum :: Lude.Maybe Lude.Double,
    -- | The timestamp (e.g., @1479816991.349@ ).
    timestamp :: Lude.Maybe Lude.Timestamp,
    -- | The unit.
    unit :: Lude.Maybe MetricUnit
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricDatapoint' with the minimum fields required to make a request.
--
-- * 'sampleCount' - The sample count.
-- * 'maximum' - The maximum.
-- * 'average' - The average.
-- * 'minimum' - The minimum.
-- * 'sum' - The sum.
-- * 'timestamp' - The timestamp (e.g., @1479816991.349@ ).
-- * 'unit' - The unit.
mkMetricDatapoint ::
  MetricDatapoint
mkMetricDatapoint =
  MetricDatapoint'
    { sampleCount = Lude.Nothing,
      maximum = Lude.Nothing,
      average = Lude.Nothing,
      minimum = Lude.Nothing,
      sum = Lude.Nothing,
      timestamp = Lude.Nothing,
      unit = Lude.Nothing
    }

-- | The sample count.
--
-- /Note:/ Consider using 'sampleCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdSampleCount :: Lens.Lens' MetricDatapoint (Lude.Maybe Lude.Double)
mdSampleCount = Lens.lens (sampleCount :: MetricDatapoint -> Lude.Maybe Lude.Double) (\s a -> s {sampleCount = a} :: MetricDatapoint)
{-# DEPRECATED mdSampleCount "Use generic-lens or generic-optics with 'sampleCount' instead." #-}

-- | The maximum.
--
-- /Note:/ Consider using 'maximum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdMaximum :: Lens.Lens' MetricDatapoint (Lude.Maybe Lude.Double)
mdMaximum = Lens.lens (maximum :: MetricDatapoint -> Lude.Maybe Lude.Double) (\s a -> s {maximum = a} :: MetricDatapoint)
{-# DEPRECATED mdMaximum "Use generic-lens or generic-optics with 'maximum' instead." #-}

-- | The average.
--
-- /Note:/ Consider using 'average' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdAverage :: Lens.Lens' MetricDatapoint (Lude.Maybe Lude.Double)
mdAverage = Lens.lens (average :: MetricDatapoint -> Lude.Maybe Lude.Double) (\s a -> s {average = a} :: MetricDatapoint)
{-# DEPRECATED mdAverage "Use generic-lens or generic-optics with 'average' instead." #-}

-- | The minimum.
--
-- /Note:/ Consider using 'minimum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdMinimum :: Lens.Lens' MetricDatapoint (Lude.Maybe Lude.Double)
mdMinimum = Lens.lens (minimum :: MetricDatapoint -> Lude.Maybe Lude.Double) (\s a -> s {minimum = a} :: MetricDatapoint)
{-# DEPRECATED mdMinimum "Use generic-lens or generic-optics with 'minimum' instead." #-}

-- | The sum.
--
-- /Note:/ Consider using 'sum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdSum :: Lens.Lens' MetricDatapoint (Lude.Maybe Lude.Double)
mdSum = Lens.lens (sum :: MetricDatapoint -> Lude.Maybe Lude.Double) (\s a -> s {sum = a} :: MetricDatapoint)
{-# DEPRECATED mdSum "Use generic-lens or generic-optics with 'sum' instead." #-}

-- | The timestamp (e.g., @1479816991.349@ ).
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdTimestamp :: Lens.Lens' MetricDatapoint (Lude.Maybe Lude.Timestamp)
mdTimestamp = Lens.lens (timestamp :: MetricDatapoint -> Lude.Maybe Lude.Timestamp) (\s a -> s {timestamp = a} :: MetricDatapoint)
{-# DEPRECATED mdTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The unit.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdUnit :: Lens.Lens' MetricDatapoint (Lude.Maybe MetricUnit)
mdUnit = Lens.lens (unit :: MetricDatapoint -> Lude.Maybe MetricUnit) (\s a -> s {unit = a} :: MetricDatapoint)
{-# DEPRECATED mdUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

instance Lude.FromJSON MetricDatapoint where
  parseJSON =
    Lude.withObject
      "MetricDatapoint"
      ( \x ->
          MetricDatapoint'
            Lude.<$> (x Lude..:? "sampleCount")
            Lude.<*> (x Lude..:? "maximum")
            Lude.<*> (x Lude..:? "average")
            Lude.<*> (x Lude..:? "minimum")
            Lude.<*> (x Lude..:? "sum")
            Lude.<*> (x Lude..:? "timestamp")
            Lude.<*> (x Lude..:? "unit")
      )
