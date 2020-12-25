{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.UtilizationByTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.UtilizationByTime
  ( UtilizationByTime (..),

    -- * Smart constructor
    mkUtilizationByTime,

    -- * Lenses
    ubtGroups,
    ubtTimePeriod,
    ubtTotal,
  )
where

import qualified Network.AWS.CostExplorer.Types.DateInterval as Types
import qualified Network.AWS.CostExplorer.Types.ReservationAggregates as Types
import qualified Network.AWS.CostExplorer.Types.ReservationUtilizationGroup as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The amount of utilization, in hours.
--
-- /See:/ 'mkUtilizationByTime' smart constructor.
data UtilizationByTime = UtilizationByTime'
  { -- | The groups that this utilization result uses.
    groups :: Core.Maybe [Types.ReservationUtilizationGroup],
    -- | The period of time that this utilization was used for.
    timePeriod :: Core.Maybe Types.DateInterval,
    -- | The total number of reservation hours that were used.
    total :: Core.Maybe Types.ReservationAggregates
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UtilizationByTime' value with any optional fields omitted.
mkUtilizationByTime ::
  UtilizationByTime
mkUtilizationByTime =
  UtilizationByTime'
    { groups = Core.Nothing,
      timePeriod = Core.Nothing,
      total = Core.Nothing
    }

-- | The groups that this utilization result uses.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubtGroups :: Lens.Lens' UtilizationByTime (Core.Maybe [Types.ReservationUtilizationGroup])
ubtGroups = Lens.field @"groups"
{-# DEPRECATED ubtGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The period of time that this utilization was used for.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubtTimePeriod :: Lens.Lens' UtilizationByTime (Core.Maybe Types.DateInterval)
ubtTimePeriod = Lens.field @"timePeriod"
{-# DEPRECATED ubtTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | The total number of reservation hours that were used.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubtTotal :: Lens.Lens' UtilizationByTime (Core.Maybe Types.ReservationAggregates)
ubtTotal = Lens.field @"total"
{-# DEPRECATED ubtTotal "Use generic-lens or generic-optics with 'total' instead." #-}

instance Core.FromJSON UtilizationByTime where
  parseJSON =
    Core.withObject "UtilizationByTime" Core.$
      \x ->
        UtilizationByTime'
          Core.<$> (x Core..:? "Groups")
          Core.<*> (x Core..:? "TimePeriod")
          Core.<*> (x Core..:? "Total")
