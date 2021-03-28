{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CoverageHours
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.CoverageHours
  ( CoverageHours (..)
  -- * Smart constructor
  , mkCoverageHours
  -- * Lenses
  , chCoverageHoursPercentage
  , chOnDemandHours
  , chReservedHours
  , chTotalRunningHours
  ) where

import qualified Network.AWS.CostExplorer.Types.CoverageHoursPercentage as Types
import qualified Network.AWS.CostExplorer.Types.OnDemandHours as Types
import qualified Network.AWS.CostExplorer.Types.ReservedHours as Types
import qualified Network.AWS.CostExplorer.Types.TotalRunningHours as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | How long a running instance either used a reservation or was On-Demand.
--
-- /See:/ 'mkCoverageHours' smart constructor.
data CoverageHours = CoverageHours'
  { coverageHoursPercentage :: Core.Maybe Types.CoverageHoursPercentage
    -- ^ The percentage of instance hours that a reservation covered.
  , onDemandHours :: Core.Maybe Types.OnDemandHours
    -- ^ The number of instance running hours that On-Demand Instances covered.
  , reservedHours :: Core.Maybe Types.ReservedHours
    -- ^ The number of instance running hours that reservations covered.
  , totalRunningHours :: Core.Maybe Types.TotalRunningHours
    -- ^ The total instance usage, in hours.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CoverageHours' value with any optional fields omitted.
mkCoverageHours
    :: CoverageHours
mkCoverageHours
  = CoverageHours'{coverageHoursPercentage = Core.Nothing,
                   onDemandHours = Core.Nothing, reservedHours = Core.Nothing,
                   totalRunningHours = Core.Nothing}

-- | The percentage of instance hours that a reservation covered.
--
-- /Note:/ Consider using 'coverageHoursPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chCoverageHoursPercentage :: Lens.Lens' CoverageHours (Core.Maybe Types.CoverageHoursPercentage)
chCoverageHoursPercentage = Lens.field @"coverageHoursPercentage"
{-# INLINEABLE chCoverageHoursPercentage #-}
{-# DEPRECATED coverageHoursPercentage "Use generic-lens or generic-optics with 'coverageHoursPercentage' instead"  #-}

-- | The number of instance running hours that On-Demand Instances covered.
--
-- /Note:/ Consider using 'onDemandHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chOnDemandHours :: Lens.Lens' CoverageHours (Core.Maybe Types.OnDemandHours)
chOnDemandHours = Lens.field @"onDemandHours"
{-# INLINEABLE chOnDemandHours #-}
{-# DEPRECATED onDemandHours "Use generic-lens or generic-optics with 'onDemandHours' instead"  #-}

-- | The number of instance running hours that reservations covered.
--
-- /Note:/ Consider using 'reservedHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chReservedHours :: Lens.Lens' CoverageHours (Core.Maybe Types.ReservedHours)
chReservedHours = Lens.field @"reservedHours"
{-# INLINEABLE chReservedHours #-}
{-# DEPRECATED reservedHours "Use generic-lens or generic-optics with 'reservedHours' instead"  #-}

-- | The total instance usage, in hours.
--
-- /Note:/ Consider using 'totalRunningHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chTotalRunningHours :: Lens.Lens' CoverageHours (Core.Maybe Types.TotalRunningHours)
chTotalRunningHours = Lens.field @"totalRunningHours"
{-# INLINEABLE chTotalRunningHours #-}
{-# DEPRECATED totalRunningHours "Use generic-lens or generic-optics with 'totalRunningHours' instead"  #-}

instance Core.FromJSON CoverageHours where
        parseJSON
          = Core.withObject "CoverageHours" Core.$
              \ x ->
                CoverageHours' Core.<$>
                  (x Core..:? "CoverageHoursPercentage") Core.<*>
                    x Core..:? "OnDemandHours"
                    Core.<*> x Core..:? "ReservedHours"
                    Core.<*> x Core..:? "TotalRunningHours"
