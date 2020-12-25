{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.SummarizedCounter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.SummarizedCounter
  ( SummarizedCounter (..),

    -- * Smart constructor
    mkSummarizedCounter,

    -- * Lenses
    scAverage,
    scMax,
    scN,
    scName,
    scSum,
    scUnit,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Shield.Types.String as Types

-- | The counter that describes a DDoS attack.
--
-- /See:/ 'mkSummarizedCounter' smart constructor.
data SummarizedCounter = SummarizedCounter'
  { -- | The average value of the counter for a specified time period.
    average :: Core.Maybe Core.Double,
    -- | The maximum value of the counter for a specified time period.
    max :: Core.Maybe Core.Double,
    -- | The number of counters for a specified time period.
    n :: Core.Maybe Core.Int,
    -- | The counter name.
    name :: Core.Maybe Types.String,
    -- | The total of counter values for a specified time period.
    sum :: Core.Maybe Core.Double,
    -- | The unit of the counters.
    unit :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SummarizedCounter' value with any optional fields omitted.
mkSummarizedCounter ::
  SummarizedCounter
mkSummarizedCounter =
  SummarizedCounter'
    { average = Core.Nothing,
      max = Core.Nothing,
      n = Core.Nothing,
      name = Core.Nothing,
      sum = Core.Nothing,
      unit = Core.Nothing
    }

-- | The average value of the counter for a specified time period.
--
-- /Note:/ Consider using 'average' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scAverage :: Lens.Lens' SummarizedCounter (Core.Maybe Core.Double)
scAverage = Lens.field @"average"
{-# DEPRECATED scAverage "Use generic-lens or generic-optics with 'average' instead." #-}

-- | The maximum value of the counter for a specified time period.
--
-- /Note:/ Consider using 'max' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scMax :: Lens.Lens' SummarizedCounter (Core.Maybe Core.Double)
scMax = Lens.field @"max"
{-# DEPRECATED scMax "Use generic-lens or generic-optics with 'max' instead." #-}

-- | The number of counters for a specified time period.
--
-- /Note:/ Consider using 'n' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scN :: Lens.Lens' SummarizedCounter (Core.Maybe Core.Int)
scN = Lens.field @"n"
{-# DEPRECATED scN "Use generic-lens or generic-optics with 'n' instead." #-}

-- | The counter name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scName :: Lens.Lens' SummarizedCounter (Core.Maybe Types.String)
scName = Lens.field @"name"
{-# DEPRECATED scName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The total of counter values for a specified time period.
--
-- /Note:/ Consider using 'sum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scSum :: Lens.Lens' SummarizedCounter (Core.Maybe Core.Double)
scSum = Lens.field @"sum"
{-# DEPRECATED scSum "Use generic-lens or generic-optics with 'sum' instead." #-}

-- | The unit of the counters.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scUnit :: Lens.Lens' SummarizedCounter (Core.Maybe Types.String)
scUnit = Lens.field @"unit"
{-# DEPRECATED scUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

instance Core.FromJSON SummarizedCounter where
  parseJSON =
    Core.withObject "SummarizedCounter" Core.$
      \x ->
        SummarizedCounter'
          Core.<$> (x Core..:? "Average")
          Core.<*> (x Core..:? "Max")
          Core.<*> (x Core..:? "N")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Sum")
          Core.<*> (x Core..:? "Unit")
