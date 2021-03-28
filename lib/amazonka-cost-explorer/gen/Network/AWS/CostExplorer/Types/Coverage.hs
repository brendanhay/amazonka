{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Coverage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.Coverage
  ( Coverage (..)
  -- * Smart constructor
  , mkCoverage
  -- * Lenses
  , cCoverageCost
  , cCoverageHours
  , cCoverageNormalizedUnits
  ) where

import qualified Network.AWS.CostExplorer.Types.CoverageCost as Types
import qualified Network.AWS.CostExplorer.Types.CoverageHours as Types
import qualified Network.AWS.CostExplorer.Types.CoverageNormalizedUnits as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The amount of instance usage that a reservation covered.
--
-- /See:/ 'mkCoverage' smart constructor.
data Coverage = Coverage'
  { coverageCost :: Core.Maybe Types.CoverageCost
    -- ^ The amount of cost that the reservation covered.
  , coverageHours :: Core.Maybe Types.CoverageHours
    -- ^ The amount of instance usage that the reservation covered, in hours.
  , coverageNormalizedUnits :: Core.Maybe Types.CoverageNormalizedUnits
    -- ^ The amount of instance usage that the reservation covered, in normalized units.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Coverage' value with any optional fields omitted.
mkCoverage
    :: Coverage
mkCoverage
  = Coverage'{coverageCost = Core.Nothing,
              coverageHours = Core.Nothing,
              coverageNormalizedUnits = Core.Nothing}

-- | The amount of cost that the reservation covered.
--
-- /Note:/ Consider using 'coverageCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCoverageCost :: Lens.Lens' Coverage (Core.Maybe Types.CoverageCost)
cCoverageCost = Lens.field @"coverageCost"
{-# INLINEABLE cCoverageCost #-}
{-# DEPRECATED coverageCost "Use generic-lens or generic-optics with 'coverageCost' instead"  #-}

-- | The amount of instance usage that the reservation covered, in hours.
--
-- /Note:/ Consider using 'coverageHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCoverageHours :: Lens.Lens' Coverage (Core.Maybe Types.CoverageHours)
cCoverageHours = Lens.field @"coverageHours"
{-# INLINEABLE cCoverageHours #-}
{-# DEPRECATED coverageHours "Use generic-lens or generic-optics with 'coverageHours' instead"  #-}

-- | The amount of instance usage that the reservation covered, in normalized units.
--
-- /Note:/ Consider using 'coverageNormalizedUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCoverageNormalizedUnits :: Lens.Lens' Coverage (Core.Maybe Types.CoverageNormalizedUnits)
cCoverageNormalizedUnits = Lens.field @"coverageNormalizedUnits"
{-# INLINEABLE cCoverageNormalizedUnits #-}
{-# DEPRECATED coverageNormalizedUnits "Use generic-lens or generic-optics with 'coverageNormalizedUnits' instead"  #-}

instance Core.FromJSON Coverage where
        parseJSON
          = Core.withObject "Coverage" Core.$
              \ x ->
                Coverage' Core.<$>
                  (x Core..:? "CoverageCost") Core.<*> x Core..:? "CoverageHours"
                    Core.<*> x Core..:? "CoverageNormalizedUnits"
