{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CoverageNormalizedUnits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CoverageNormalizedUnits
  ( CoverageNormalizedUnits (..),

    -- * Smart constructor
    mkCoverageNormalizedUnits,

    -- * Lenses
    cnuCoverageNormalizedUnitsPercentage,
    cnuOnDemandNormalizedUnits,
    cnuReservedNormalizedUnits,
    cnuTotalRunningNormalizedUnits,
  )
where

import qualified Network.AWS.CostExplorer.Types.CoverageNormalizedUnitsPercentage as Types
import qualified Network.AWS.CostExplorer.Types.OnDemandNormalizedUnits as Types
import qualified Network.AWS.CostExplorer.Types.ReservedNormalizedUnits as Types
import qualified Network.AWS.CostExplorer.Types.TotalRunningNormalizedUnits as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The amount of instance usage, in normalized units. Normalized units enable you to see your EC2 usage for multiple sizes of instances in a uniform way. For example, suppose you run an xlarge instance and a 2xlarge instance. If you run both instances for the same amount of time, the 2xlarge instance uses twice as much of your reservation as the xlarge instance, even though both instances show only one instance-hour. Using normalized units instead of instance-hours, the xlarge instance used 8 normalized units, and the 2xlarge instance used 16 normalized units.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-modifying.html Modifying Reserved Instances> in the /Amazon Elastic Compute Cloud User Guide for Linux Instances/ .
--
-- /See:/ 'mkCoverageNormalizedUnits' smart constructor.
data CoverageNormalizedUnits = CoverageNormalizedUnits'
  { -- | The percentage of your used instance normalized units that a reservation covers.
    coverageNormalizedUnitsPercentage :: Core.Maybe Types.CoverageNormalizedUnitsPercentage,
    -- | The number of normalized units that are covered by On-Demand Instances instead of a reservation.
    onDemandNormalizedUnits :: Core.Maybe Types.OnDemandNormalizedUnits,
    -- | The number of normalized units that a reservation covers.
    reservedNormalizedUnits :: Core.Maybe Types.ReservedNormalizedUnits,
    -- | The total number of normalized units that you used.
    totalRunningNormalizedUnits :: Core.Maybe Types.TotalRunningNormalizedUnits
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CoverageNormalizedUnits' value with any optional fields omitted.
mkCoverageNormalizedUnits ::
  CoverageNormalizedUnits
mkCoverageNormalizedUnits =
  CoverageNormalizedUnits'
    { coverageNormalizedUnitsPercentage =
        Core.Nothing,
      onDemandNormalizedUnits = Core.Nothing,
      reservedNormalizedUnits = Core.Nothing,
      totalRunningNormalizedUnits = Core.Nothing
    }

-- | The percentage of your used instance normalized units that a reservation covers.
--
-- /Note:/ Consider using 'coverageNormalizedUnitsPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnuCoverageNormalizedUnitsPercentage :: Lens.Lens' CoverageNormalizedUnits (Core.Maybe Types.CoverageNormalizedUnitsPercentage)
cnuCoverageNormalizedUnitsPercentage = Lens.field @"coverageNormalizedUnitsPercentage"
{-# DEPRECATED cnuCoverageNormalizedUnitsPercentage "Use generic-lens or generic-optics with 'coverageNormalizedUnitsPercentage' instead." #-}

-- | The number of normalized units that are covered by On-Demand Instances instead of a reservation.
--
-- /Note:/ Consider using 'onDemandNormalizedUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnuOnDemandNormalizedUnits :: Lens.Lens' CoverageNormalizedUnits (Core.Maybe Types.OnDemandNormalizedUnits)
cnuOnDemandNormalizedUnits = Lens.field @"onDemandNormalizedUnits"
{-# DEPRECATED cnuOnDemandNormalizedUnits "Use generic-lens or generic-optics with 'onDemandNormalizedUnits' instead." #-}

-- | The number of normalized units that a reservation covers.
--
-- /Note:/ Consider using 'reservedNormalizedUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnuReservedNormalizedUnits :: Lens.Lens' CoverageNormalizedUnits (Core.Maybe Types.ReservedNormalizedUnits)
cnuReservedNormalizedUnits = Lens.field @"reservedNormalizedUnits"
{-# DEPRECATED cnuReservedNormalizedUnits "Use generic-lens or generic-optics with 'reservedNormalizedUnits' instead." #-}

-- | The total number of normalized units that you used.
--
-- /Note:/ Consider using 'totalRunningNormalizedUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnuTotalRunningNormalizedUnits :: Lens.Lens' CoverageNormalizedUnits (Core.Maybe Types.TotalRunningNormalizedUnits)
cnuTotalRunningNormalizedUnits = Lens.field @"totalRunningNormalizedUnits"
{-# DEPRECATED cnuTotalRunningNormalizedUnits "Use generic-lens or generic-optics with 'totalRunningNormalizedUnits' instead." #-}

instance Core.FromJSON CoverageNormalizedUnits where
  parseJSON =
    Core.withObject "CoverageNormalizedUnits" Core.$
      \x ->
        CoverageNormalizedUnits'
          Core.<$> (x Core..:? "CoverageNormalizedUnitsPercentage")
          Core.<*> (x Core..:? "OnDemandNormalizedUnits")
          Core.<*> (x Core..:? "ReservedNormalizedUnits")
          Core.<*> (x Core..:? "TotalRunningNormalizedUnits")
