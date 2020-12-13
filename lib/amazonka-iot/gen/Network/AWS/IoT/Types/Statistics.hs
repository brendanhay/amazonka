{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Statistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Statistics
  ( Statistics (..),

    -- * Smart constructor
    mkStatistics,

    -- * Lenses
    sStdDeviation,
    sMaximum,
    sAverage,
    sCount,
    sMinimum,
    sVariance,
    sSumOfSquares,
    sSum,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A map of key-value pairs for all supported statistics. Currently, only count is supported.
--
-- /See:/ 'mkStatistics' smart constructor.
data Statistics = Statistics'
  { -- | The standard deviation of the aggregated field values.
    stdDeviation :: Lude.Maybe Lude.Double,
    -- | The maximum aggregated field value.
    maximum :: Lude.Maybe Lude.Double,
    -- | The average of the aggregated field values.
    average :: Lude.Maybe Lude.Double,
    -- | The count of things that match the query.
    count :: Lude.Maybe Lude.Int,
    -- | The minimum aggregated field value.
    minimum :: Lude.Maybe Lude.Double,
    -- | The variance of the aggregated field values.
    variance :: Lude.Maybe Lude.Double,
    -- | The sum of the squares of the aggregated field values.
    sumOfSquares :: Lude.Maybe Lude.Double,
    -- | The sum of the aggregated field values.
    sum :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Statistics' with the minimum fields required to make a request.
--
-- * 'stdDeviation' - The standard deviation of the aggregated field values.
-- * 'maximum' - The maximum aggregated field value.
-- * 'average' - The average of the aggregated field values.
-- * 'count' - The count of things that match the query.
-- * 'minimum' - The minimum aggregated field value.
-- * 'variance' - The variance of the aggregated field values.
-- * 'sumOfSquares' - The sum of the squares of the aggregated field values.
-- * 'sum' - The sum of the aggregated field values.
mkStatistics ::
  Statistics
mkStatistics =
  Statistics'
    { stdDeviation = Lude.Nothing,
      maximum = Lude.Nothing,
      average = Lude.Nothing,
      count = Lude.Nothing,
      minimum = Lude.Nothing,
      variance = Lude.Nothing,
      sumOfSquares = Lude.Nothing,
      sum = Lude.Nothing
    }

-- | The standard deviation of the aggregated field values.
--
-- /Note:/ Consider using 'stdDeviation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStdDeviation :: Lens.Lens' Statistics (Lude.Maybe Lude.Double)
sStdDeviation = Lens.lens (stdDeviation :: Statistics -> Lude.Maybe Lude.Double) (\s a -> s {stdDeviation = a} :: Statistics)
{-# DEPRECATED sStdDeviation "Use generic-lens or generic-optics with 'stdDeviation' instead." #-}

-- | The maximum aggregated field value.
--
-- /Note:/ Consider using 'maximum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMaximum :: Lens.Lens' Statistics (Lude.Maybe Lude.Double)
sMaximum = Lens.lens (maximum :: Statistics -> Lude.Maybe Lude.Double) (\s a -> s {maximum = a} :: Statistics)
{-# DEPRECATED sMaximum "Use generic-lens or generic-optics with 'maximum' instead." #-}

-- | The average of the aggregated field values.
--
-- /Note:/ Consider using 'average' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAverage :: Lens.Lens' Statistics (Lude.Maybe Lude.Double)
sAverage = Lens.lens (average :: Statistics -> Lude.Maybe Lude.Double) (\s a -> s {average = a} :: Statistics)
{-# DEPRECATED sAverage "Use generic-lens or generic-optics with 'average' instead." #-}

-- | The count of things that match the query.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCount :: Lens.Lens' Statistics (Lude.Maybe Lude.Int)
sCount = Lens.lens (count :: Statistics -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: Statistics)
{-# DEPRECATED sCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The minimum aggregated field value.
--
-- /Note:/ Consider using 'minimum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMinimum :: Lens.Lens' Statistics (Lude.Maybe Lude.Double)
sMinimum = Lens.lens (minimum :: Statistics -> Lude.Maybe Lude.Double) (\s a -> s {minimum = a} :: Statistics)
{-# DEPRECATED sMinimum "Use generic-lens or generic-optics with 'minimum' instead." #-}

-- | The variance of the aggregated field values.
--
-- /Note:/ Consider using 'variance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVariance :: Lens.Lens' Statistics (Lude.Maybe Lude.Double)
sVariance = Lens.lens (variance :: Statistics -> Lude.Maybe Lude.Double) (\s a -> s {variance = a} :: Statistics)
{-# DEPRECATED sVariance "Use generic-lens or generic-optics with 'variance' instead." #-}

-- | The sum of the squares of the aggregated field values.
--
-- /Note:/ Consider using 'sumOfSquares' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSumOfSquares :: Lens.Lens' Statistics (Lude.Maybe Lude.Double)
sSumOfSquares = Lens.lens (sumOfSquares :: Statistics -> Lude.Maybe Lude.Double) (\s a -> s {sumOfSquares = a} :: Statistics)
{-# DEPRECATED sSumOfSquares "Use generic-lens or generic-optics with 'sumOfSquares' instead." #-}

-- | The sum of the aggregated field values.
--
-- /Note:/ Consider using 'sum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSum :: Lens.Lens' Statistics (Lude.Maybe Lude.Double)
sSum = Lens.lens (sum :: Statistics -> Lude.Maybe Lude.Double) (\s a -> s {sum = a} :: Statistics)
{-# DEPRECATED sSum "Use generic-lens or generic-optics with 'sum' instead." #-}

instance Lude.FromJSON Statistics where
  parseJSON =
    Lude.withObject
      "Statistics"
      ( \x ->
          Statistics'
            Lude.<$> (x Lude..:? "stdDeviation")
            Lude.<*> (x Lude..:? "maximum")
            Lude.<*> (x Lude..:? "average")
            Lude.<*> (x Lude..:? "count")
            Lude.<*> (x Lude..:? "minimum")
            Lude.<*> (x Lude..:? "variance")
            Lude.<*> (x Lude..:? "sumOfSquares")
            Lude.<*> (x Lude..:? "sum")
      )
