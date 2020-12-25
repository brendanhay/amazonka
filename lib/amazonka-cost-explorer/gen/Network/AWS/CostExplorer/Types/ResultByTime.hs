{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ResultByTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ResultByTime
  ( ResultByTime (..),

    -- * Smart constructor
    mkResultByTime,

    -- * Lenses
    rbtEstimated,
    rbtGroups,
    rbtTimePeriod,
    rbtTotal,
  )
where

import qualified Network.AWS.CostExplorer.Types.DateInterval as Types
import qualified Network.AWS.CostExplorer.Types.Group as Types
import qualified Network.AWS.CostExplorer.Types.MetricName as Types
import qualified Network.AWS.CostExplorer.Types.MetricValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The result that is associated with a time period.
--
-- /See:/ 'mkResultByTime' smart constructor.
data ResultByTime = ResultByTime'
  { -- | Whether the result is estimated.
    estimated :: Core.Maybe Core.Bool,
    -- | The groups that this time period includes.
    groups :: Core.Maybe [Types.Group],
    -- | The time period that the result covers.
    timePeriod :: Core.Maybe Types.DateInterval,
    -- | The total amount of cost or usage accrued during the time period.
    total :: Core.Maybe (Core.HashMap Types.MetricName Types.MetricValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResultByTime' value with any optional fields omitted.
mkResultByTime ::
  ResultByTime
mkResultByTime =
  ResultByTime'
    { estimated = Core.Nothing,
      groups = Core.Nothing,
      timePeriod = Core.Nothing,
      total = Core.Nothing
    }

-- | Whether the result is estimated.
--
-- /Note:/ Consider using 'estimated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbtEstimated :: Lens.Lens' ResultByTime (Core.Maybe Core.Bool)
rbtEstimated = Lens.field @"estimated"
{-# DEPRECATED rbtEstimated "Use generic-lens or generic-optics with 'estimated' instead." #-}

-- | The groups that this time period includes.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbtGroups :: Lens.Lens' ResultByTime (Core.Maybe [Types.Group])
rbtGroups = Lens.field @"groups"
{-# DEPRECATED rbtGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The time period that the result covers.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbtTimePeriod :: Lens.Lens' ResultByTime (Core.Maybe Types.DateInterval)
rbtTimePeriod = Lens.field @"timePeriod"
{-# DEPRECATED rbtTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | The total amount of cost or usage accrued during the time period.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbtTotal :: Lens.Lens' ResultByTime (Core.Maybe (Core.HashMap Types.MetricName Types.MetricValue))
rbtTotal = Lens.field @"total"
{-# DEPRECATED rbtTotal "Use generic-lens or generic-optics with 'total' instead." #-}

instance Core.FromJSON ResultByTime where
  parseJSON =
    Core.withObject "ResultByTime" Core.$
      \x ->
        ResultByTime'
          Core.<$> (x Core..:? "Estimated")
          Core.<*> (x Core..:? "Groups")
          Core.<*> (x Core..:? "TimePeriod")
          Core.<*> (x Core..:? "Total")
