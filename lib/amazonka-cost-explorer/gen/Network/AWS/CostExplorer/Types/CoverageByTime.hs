{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CoverageByTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CoverageByTime
  ( CoverageByTime (..),

    -- * Smart constructor
    mkCoverageByTime,

    -- * Lenses
    cbtGroups,
    cbtTimePeriod,
    cbtTotal,
  )
where

import qualified Network.AWS.CostExplorer.Types.Coverage as Types
import qualified Network.AWS.CostExplorer.Types.DateInterval as Types
import qualified Network.AWS.CostExplorer.Types.ReservationCoverageGroup as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Reservation coverage for a specified period, in hours.
--
-- /See:/ 'mkCoverageByTime' smart constructor.
data CoverageByTime = CoverageByTime'
  { -- | The groups of instances that the reservation covered.
    groups :: Core.Maybe [Types.ReservationCoverageGroup],
    -- | The period that this coverage was used over.
    timePeriod :: Core.Maybe Types.DateInterval,
    -- | The total reservation coverage, in hours.
    total :: Core.Maybe Types.Coverage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CoverageByTime' value with any optional fields omitted.
mkCoverageByTime ::
  CoverageByTime
mkCoverageByTime =
  CoverageByTime'
    { groups = Core.Nothing,
      timePeriod = Core.Nothing,
      total = Core.Nothing
    }

-- | The groups of instances that the reservation covered.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtGroups :: Lens.Lens' CoverageByTime (Core.Maybe [Types.ReservationCoverageGroup])
cbtGroups = Lens.field @"groups"
{-# DEPRECATED cbtGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The period that this coverage was used over.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtTimePeriod :: Lens.Lens' CoverageByTime (Core.Maybe Types.DateInterval)
cbtTimePeriod = Lens.field @"timePeriod"
{-# DEPRECATED cbtTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | The total reservation coverage, in hours.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtTotal :: Lens.Lens' CoverageByTime (Core.Maybe Types.Coverage)
cbtTotal = Lens.field @"total"
{-# DEPRECATED cbtTotal "Use generic-lens or generic-optics with 'total' instead." #-}

instance Core.FromJSON CoverageByTime where
  parseJSON =
    Core.withObject "CoverageByTime" Core.$
      \x ->
        CoverageByTime'
          Core.<$> (x Core..:? "Groups")
          Core.<*> (x Core..:? "TimePeriod")
          Core.<*> (x Core..:? "Total")
