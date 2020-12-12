{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Coverage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Coverage
  ( Coverage (..),

    -- * Smart constructor
    mkCoverage,

    -- * Lenses
    cCoverageNormalizedUnits,
    cCoverageHours,
    cCoverageCost,
  )
where

import Network.AWS.CostExplorer.Types.CoverageCost
import Network.AWS.CostExplorer.Types.CoverageHours
import Network.AWS.CostExplorer.Types.CoverageNormalizedUnits
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The amount of instance usage that a reservation covered.
--
-- /See:/ 'mkCoverage' smart constructor.
data Coverage = Coverage'
  { coverageNormalizedUnits ::
      Lude.Maybe CoverageNormalizedUnits,
    coverageHours :: Lude.Maybe CoverageHours,
    coverageCost :: Lude.Maybe CoverageCost
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Coverage' with the minimum fields required to make a request.
--
-- * 'coverageCost' - The amount of cost that the reservation covered.
-- * 'coverageHours' - The amount of instance usage that the reservation covered, in hours.
-- * 'coverageNormalizedUnits' - The amount of instance usage that the reservation covered, in normalized units.
mkCoverage ::
  Coverage
mkCoverage =
  Coverage'
    { coverageNormalizedUnits = Lude.Nothing,
      coverageHours = Lude.Nothing,
      coverageCost = Lude.Nothing
    }

-- | The amount of instance usage that the reservation covered, in normalized units.
--
-- /Note:/ Consider using 'coverageNormalizedUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCoverageNormalizedUnits :: Lens.Lens' Coverage (Lude.Maybe CoverageNormalizedUnits)
cCoverageNormalizedUnits = Lens.lens (coverageNormalizedUnits :: Coverage -> Lude.Maybe CoverageNormalizedUnits) (\s a -> s {coverageNormalizedUnits = a} :: Coverage)
{-# DEPRECATED cCoverageNormalizedUnits "Use generic-lens or generic-optics with 'coverageNormalizedUnits' instead." #-}

-- | The amount of instance usage that the reservation covered, in hours.
--
-- /Note:/ Consider using 'coverageHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCoverageHours :: Lens.Lens' Coverage (Lude.Maybe CoverageHours)
cCoverageHours = Lens.lens (coverageHours :: Coverage -> Lude.Maybe CoverageHours) (\s a -> s {coverageHours = a} :: Coverage)
{-# DEPRECATED cCoverageHours "Use generic-lens or generic-optics with 'coverageHours' instead." #-}

-- | The amount of cost that the reservation covered.
--
-- /Note:/ Consider using 'coverageCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCoverageCost :: Lens.Lens' Coverage (Lude.Maybe CoverageCost)
cCoverageCost = Lens.lens (coverageCost :: Coverage -> Lude.Maybe CoverageCost) (\s a -> s {coverageCost = a} :: Coverage)
{-# DEPRECATED cCoverageCost "Use generic-lens or generic-optics with 'coverageCost' instead." #-}

instance Lude.FromJSON Coverage where
  parseJSON =
    Lude.withObject
      "Coverage"
      ( \x ->
          Coverage'
            Lude.<$> (x Lude..:? "CoverageNormalizedUnits")
            Lude.<*> (x Lude..:? "CoverageHours")
            Lude.<*> (x Lude..:? "CoverageCost")
      )
