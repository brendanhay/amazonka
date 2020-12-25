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
    sAverage,
    sCount,
    sMaximum,
    sMinimum,
    sStdDeviation,
    sSum,
    sSumOfSquares,
    sVariance,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A map of key-value pairs for all supported statistics. Currently, only count is supported.
--
-- /See:/ 'mkStatistics' smart constructor.
data Statistics = Statistics'
  { -- | The average of the aggregated field values.
    average :: Core.Maybe Core.Double,
    -- | The count of things that match the query.
    count :: Core.Maybe Core.Int,
    -- | The maximum aggregated field value.
    maximum :: Core.Maybe Core.Double,
    -- | The minimum aggregated field value.
    minimum :: Core.Maybe Core.Double,
    -- | The standard deviation of the aggregated field values.
    stdDeviation :: Core.Maybe Core.Double,
    -- | The sum of the aggregated field values.
    sum :: Core.Maybe Core.Double,
    -- | The sum of the squares of the aggregated field values.
    sumOfSquares :: Core.Maybe Core.Double,
    -- | The variance of the aggregated field values.
    variance :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Statistics' value with any optional fields omitted.
mkStatistics ::
  Statistics
mkStatistics =
  Statistics'
    { average = Core.Nothing,
      count = Core.Nothing,
      maximum = Core.Nothing,
      minimum = Core.Nothing,
      stdDeviation = Core.Nothing,
      sum = Core.Nothing,
      sumOfSquares = Core.Nothing,
      variance = Core.Nothing
    }

-- | The average of the aggregated field values.
--
-- /Note:/ Consider using 'average' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAverage :: Lens.Lens' Statistics (Core.Maybe Core.Double)
sAverage = Lens.field @"average"
{-# DEPRECATED sAverage "Use generic-lens or generic-optics with 'average' instead." #-}

-- | The count of things that match the query.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCount :: Lens.Lens' Statistics (Core.Maybe Core.Int)
sCount = Lens.field @"count"
{-# DEPRECATED sCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The maximum aggregated field value.
--
-- /Note:/ Consider using 'maximum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMaximum :: Lens.Lens' Statistics (Core.Maybe Core.Double)
sMaximum = Lens.field @"maximum"
{-# DEPRECATED sMaximum "Use generic-lens or generic-optics with 'maximum' instead." #-}

-- | The minimum aggregated field value.
--
-- /Note:/ Consider using 'minimum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMinimum :: Lens.Lens' Statistics (Core.Maybe Core.Double)
sMinimum = Lens.field @"minimum"
{-# DEPRECATED sMinimum "Use generic-lens or generic-optics with 'minimum' instead." #-}

-- | The standard deviation of the aggregated field values.
--
-- /Note:/ Consider using 'stdDeviation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStdDeviation :: Lens.Lens' Statistics (Core.Maybe Core.Double)
sStdDeviation = Lens.field @"stdDeviation"
{-# DEPRECATED sStdDeviation "Use generic-lens or generic-optics with 'stdDeviation' instead." #-}

-- | The sum of the aggregated field values.
--
-- /Note:/ Consider using 'sum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSum :: Lens.Lens' Statistics (Core.Maybe Core.Double)
sSum = Lens.field @"sum"
{-# DEPRECATED sSum "Use generic-lens or generic-optics with 'sum' instead." #-}

-- | The sum of the squares of the aggregated field values.
--
-- /Note:/ Consider using 'sumOfSquares' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSumOfSquares :: Lens.Lens' Statistics (Core.Maybe Core.Double)
sSumOfSquares = Lens.field @"sumOfSquares"
{-# DEPRECATED sSumOfSquares "Use generic-lens or generic-optics with 'sumOfSquares' instead." #-}

-- | The variance of the aggregated field values.
--
-- /Note:/ Consider using 'variance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVariance :: Lens.Lens' Statistics (Core.Maybe Core.Double)
sVariance = Lens.field @"variance"
{-# DEPRECATED sVariance "Use generic-lens or generic-optics with 'variance' instead." #-}

instance Core.FromJSON Statistics where
  parseJSON =
    Core.withObject "Statistics" Core.$
      \x ->
        Statistics'
          Core.<$> (x Core..:? "average")
          Core.<*> (x Core..:? "count")
          Core.<*> (x Core..:? "maximum")
          Core.<*> (x Core..:? "minimum")
          Core.<*> (x Core..:? "stdDeviation")
          Core.<*> (x Core..:? "sum")
          Core.<*> (x Core..:? "sumOfSquares")
          Core.<*> (x Core..:? "variance")
