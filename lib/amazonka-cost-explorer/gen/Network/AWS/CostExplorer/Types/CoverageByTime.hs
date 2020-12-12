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

import Network.AWS.CostExplorer.Types.Coverage
import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.CostExplorer.Types.ReservationCoverageGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Reservation coverage for a specified period, in hours.
--
-- /See:/ 'mkCoverageByTime' smart constructor.
data CoverageByTime = CoverageByTime'
  { groups ::
      Lude.Maybe [ReservationCoverageGroup],
    timePeriod :: Lude.Maybe DateInterval,
    total :: Lude.Maybe Coverage
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CoverageByTime' with the minimum fields required to make a request.
--
-- * 'groups' - The groups of instances that the reservation covered.
-- * 'timePeriod' - The period that this coverage was used over.
-- * 'total' - The total reservation coverage, in hours.
mkCoverageByTime ::
  CoverageByTime
mkCoverageByTime =
  CoverageByTime'
    { groups = Lude.Nothing,
      timePeriod = Lude.Nothing,
      total = Lude.Nothing
    }

-- | The groups of instances that the reservation covered.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtGroups :: Lens.Lens' CoverageByTime (Lude.Maybe [ReservationCoverageGroup])
cbtGroups = Lens.lens (groups :: CoverageByTime -> Lude.Maybe [ReservationCoverageGroup]) (\s a -> s {groups = a} :: CoverageByTime)
{-# DEPRECATED cbtGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The period that this coverage was used over.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtTimePeriod :: Lens.Lens' CoverageByTime (Lude.Maybe DateInterval)
cbtTimePeriod = Lens.lens (timePeriod :: CoverageByTime -> Lude.Maybe DateInterval) (\s a -> s {timePeriod = a} :: CoverageByTime)
{-# DEPRECATED cbtTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | The total reservation coverage, in hours.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbtTotal :: Lens.Lens' CoverageByTime (Lude.Maybe Coverage)
cbtTotal = Lens.lens (total :: CoverageByTime -> Lude.Maybe Coverage) (\s a -> s {total = a} :: CoverageByTime)
{-# DEPRECATED cbtTotal "Use generic-lens or generic-optics with 'total' instead." #-}

instance Lude.FromJSON CoverageByTime where
  parseJSON =
    Lude.withObject
      "CoverageByTime"
      ( \x ->
          CoverageByTime'
            Lude.<$> (x Lude..:? "Groups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "TimePeriod")
            Lude.<*> (x Lude..:? "Total")
      )
