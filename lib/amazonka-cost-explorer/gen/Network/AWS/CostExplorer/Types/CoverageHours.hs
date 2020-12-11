-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CoverageHours
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CoverageHours
  ( CoverageHours (..),

    -- * Smart constructor
    mkCoverageHours,

    -- * Lenses
    chCoverageHoursPercentage,
    chOnDemandHours,
    chTotalRunningHours,
    chReservedHours,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | How long a running instance either used a reservation or was On-Demand.
--
-- /See:/ 'mkCoverageHours' smart constructor.
data CoverageHours = CoverageHours'
  { coverageHoursPercentage ::
      Lude.Maybe Lude.Text,
    onDemandHours :: Lude.Maybe Lude.Text,
    totalRunningHours :: Lude.Maybe Lude.Text,
    reservedHours :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CoverageHours' with the minimum fields required to make a request.
--
-- * 'coverageHoursPercentage' - The percentage of instance hours that a reservation covered.
-- * 'onDemandHours' - The number of instance running hours that On-Demand Instances covered.
-- * 'reservedHours' - The number of instance running hours that reservations covered.
-- * 'totalRunningHours' - The total instance usage, in hours.
mkCoverageHours ::
  CoverageHours
mkCoverageHours =
  CoverageHours'
    { coverageHoursPercentage = Lude.Nothing,
      onDemandHours = Lude.Nothing,
      totalRunningHours = Lude.Nothing,
      reservedHours = Lude.Nothing
    }

-- | The percentage of instance hours that a reservation covered.
--
-- /Note:/ Consider using 'coverageHoursPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chCoverageHoursPercentage :: Lens.Lens' CoverageHours (Lude.Maybe Lude.Text)
chCoverageHoursPercentage = Lens.lens (coverageHoursPercentage :: CoverageHours -> Lude.Maybe Lude.Text) (\s a -> s {coverageHoursPercentage = a} :: CoverageHours)
{-# DEPRECATED chCoverageHoursPercentage "Use generic-lens or generic-optics with 'coverageHoursPercentage' instead." #-}

-- | The number of instance running hours that On-Demand Instances covered.
--
-- /Note:/ Consider using 'onDemandHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chOnDemandHours :: Lens.Lens' CoverageHours (Lude.Maybe Lude.Text)
chOnDemandHours = Lens.lens (onDemandHours :: CoverageHours -> Lude.Maybe Lude.Text) (\s a -> s {onDemandHours = a} :: CoverageHours)
{-# DEPRECATED chOnDemandHours "Use generic-lens or generic-optics with 'onDemandHours' instead." #-}

-- | The total instance usage, in hours.
--
-- /Note:/ Consider using 'totalRunningHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chTotalRunningHours :: Lens.Lens' CoverageHours (Lude.Maybe Lude.Text)
chTotalRunningHours = Lens.lens (totalRunningHours :: CoverageHours -> Lude.Maybe Lude.Text) (\s a -> s {totalRunningHours = a} :: CoverageHours)
{-# DEPRECATED chTotalRunningHours "Use generic-lens or generic-optics with 'totalRunningHours' instead." #-}

-- | The number of instance running hours that reservations covered.
--
-- /Note:/ Consider using 'reservedHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chReservedHours :: Lens.Lens' CoverageHours (Lude.Maybe Lude.Text)
chReservedHours = Lens.lens (reservedHours :: CoverageHours -> Lude.Maybe Lude.Text) (\s a -> s {reservedHours = a} :: CoverageHours)
{-# DEPRECATED chReservedHours "Use generic-lens or generic-optics with 'reservedHours' instead." #-}

instance Lude.FromJSON CoverageHours where
  parseJSON =
    Lude.withObject
      "CoverageHours"
      ( \x ->
          CoverageHours'
            Lude.<$> (x Lude..:? "CoverageHoursPercentage")
            Lude.<*> (x Lude..:? "OnDemandHours")
            Lude.<*> (x Lude..:? "TotalRunningHours")
            Lude.<*> (x Lude..:? "ReservedHours")
      )
