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
    cnuReservedNormalizedUnits,
    cnuTotalRunningNormalizedUnits,
    cnuCoverageNormalizedUnitsPercentage,
    cnuOnDemandNormalizedUnits,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The amount of instance usage, in normalized units. Normalized units enable you to see your EC2 usage for multiple sizes of instances in a uniform way. For example, suppose you run an xlarge instance and a 2xlarge instance. If you run both instances for the same amount of time, the 2xlarge instance uses twice as much of your reservation as the xlarge instance, even though both instances show only one instance-hour. Using normalized units instead of instance-hours, the xlarge instance used 8 normalized units, and the 2xlarge instance used 16 normalized units.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-modifying.html Modifying Reserved Instances> in the /Amazon Elastic Compute Cloud User Guide for Linux Instances/ .
--
-- /See:/ 'mkCoverageNormalizedUnits' smart constructor.
data CoverageNormalizedUnits = CoverageNormalizedUnits'
  { -- | The number of normalized units that a reservation covers.
    reservedNormalizedUnits :: Lude.Maybe Lude.Text,
    -- | The total number of normalized units that you used.
    totalRunningNormalizedUnits :: Lude.Maybe Lude.Text,
    -- | The percentage of your used instance normalized units that a reservation covers.
    coverageNormalizedUnitsPercentage :: Lude.Maybe Lude.Text,
    -- | The number of normalized units that are covered by On-Demand Instances instead of a reservation.
    onDemandNormalizedUnits :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CoverageNormalizedUnits' with the minimum fields required to make a request.
--
-- * 'reservedNormalizedUnits' - The number of normalized units that a reservation covers.
-- * 'totalRunningNormalizedUnits' - The total number of normalized units that you used.
-- * 'coverageNormalizedUnitsPercentage' - The percentage of your used instance normalized units that a reservation covers.
-- * 'onDemandNormalizedUnits' - The number of normalized units that are covered by On-Demand Instances instead of a reservation.
mkCoverageNormalizedUnits ::
  CoverageNormalizedUnits
mkCoverageNormalizedUnits =
  CoverageNormalizedUnits'
    { reservedNormalizedUnits = Lude.Nothing,
      totalRunningNormalizedUnits = Lude.Nothing,
      coverageNormalizedUnitsPercentage = Lude.Nothing,
      onDemandNormalizedUnits = Lude.Nothing
    }

-- | The number of normalized units that a reservation covers.
--
-- /Note:/ Consider using 'reservedNormalizedUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnuReservedNormalizedUnits :: Lens.Lens' CoverageNormalizedUnits (Lude.Maybe Lude.Text)
cnuReservedNormalizedUnits = Lens.lens (reservedNormalizedUnits :: CoverageNormalizedUnits -> Lude.Maybe Lude.Text) (\s a -> s {reservedNormalizedUnits = a} :: CoverageNormalizedUnits)
{-# DEPRECATED cnuReservedNormalizedUnits "Use generic-lens or generic-optics with 'reservedNormalizedUnits' instead." #-}

-- | The total number of normalized units that you used.
--
-- /Note:/ Consider using 'totalRunningNormalizedUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnuTotalRunningNormalizedUnits :: Lens.Lens' CoverageNormalizedUnits (Lude.Maybe Lude.Text)
cnuTotalRunningNormalizedUnits = Lens.lens (totalRunningNormalizedUnits :: CoverageNormalizedUnits -> Lude.Maybe Lude.Text) (\s a -> s {totalRunningNormalizedUnits = a} :: CoverageNormalizedUnits)
{-# DEPRECATED cnuTotalRunningNormalizedUnits "Use generic-lens or generic-optics with 'totalRunningNormalizedUnits' instead." #-}

-- | The percentage of your used instance normalized units that a reservation covers.
--
-- /Note:/ Consider using 'coverageNormalizedUnitsPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnuCoverageNormalizedUnitsPercentage :: Lens.Lens' CoverageNormalizedUnits (Lude.Maybe Lude.Text)
cnuCoverageNormalizedUnitsPercentage = Lens.lens (coverageNormalizedUnitsPercentage :: CoverageNormalizedUnits -> Lude.Maybe Lude.Text) (\s a -> s {coverageNormalizedUnitsPercentage = a} :: CoverageNormalizedUnits)
{-# DEPRECATED cnuCoverageNormalizedUnitsPercentage "Use generic-lens or generic-optics with 'coverageNormalizedUnitsPercentage' instead." #-}

-- | The number of normalized units that are covered by On-Demand Instances instead of a reservation.
--
-- /Note:/ Consider using 'onDemandNormalizedUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnuOnDemandNormalizedUnits :: Lens.Lens' CoverageNormalizedUnits (Lude.Maybe Lude.Text)
cnuOnDemandNormalizedUnits = Lens.lens (onDemandNormalizedUnits :: CoverageNormalizedUnits -> Lude.Maybe Lude.Text) (\s a -> s {onDemandNormalizedUnits = a} :: CoverageNormalizedUnits)
{-# DEPRECATED cnuOnDemandNormalizedUnits "Use generic-lens or generic-optics with 'onDemandNormalizedUnits' instead." #-}

instance Lude.FromJSON CoverageNormalizedUnits where
  parseJSON =
    Lude.withObject
      "CoverageNormalizedUnits"
      ( \x ->
          CoverageNormalizedUnits'
            Lude.<$> (x Lude..:? "ReservedNormalizedUnits")
            Lude.<*> (x Lude..:? "TotalRunningNormalizedUnits")
            Lude.<*> (x Lude..:? "CoverageNormalizedUnitsPercentage")
            Lude.<*> (x Lude..:? "OnDemandNormalizedUnits")
      )
