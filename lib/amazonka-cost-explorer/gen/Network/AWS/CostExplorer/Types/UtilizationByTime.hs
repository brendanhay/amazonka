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

import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.CostExplorer.Types.ReservationAggregates
import Network.AWS.CostExplorer.Types.ReservationUtilizationGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The amount of utilization, in hours.
--
-- /See:/ 'mkUtilizationByTime' smart constructor.
data UtilizationByTime = UtilizationByTime'
  { groups ::
      Lude.Maybe [ReservationUtilizationGroup],
    timePeriod :: Lude.Maybe DateInterval,
    total :: Lude.Maybe ReservationAggregates
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UtilizationByTime' with the minimum fields required to make a request.
--
-- * 'groups' - The groups that this utilization result uses.
-- * 'timePeriod' - The period of time that this utilization was used for.
-- * 'total' - The total number of reservation hours that were used.
mkUtilizationByTime ::
  UtilizationByTime
mkUtilizationByTime =
  UtilizationByTime'
    { groups = Lude.Nothing,
      timePeriod = Lude.Nothing,
      total = Lude.Nothing
    }

-- | The groups that this utilization result uses.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubtGroups :: Lens.Lens' UtilizationByTime (Lude.Maybe [ReservationUtilizationGroup])
ubtGroups = Lens.lens (groups :: UtilizationByTime -> Lude.Maybe [ReservationUtilizationGroup]) (\s a -> s {groups = a} :: UtilizationByTime)
{-# DEPRECATED ubtGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The period of time that this utilization was used for.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubtTimePeriod :: Lens.Lens' UtilizationByTime (Lude.Maybe DateInterval)
ubtTimePeriod = Lens.lens (timePeriod :: UtilizationByTime -> Lude.Maybe DateInterval) (\s a -> s {timePeriod = a} :: UtilizationByTime)
{-# DEPRECATED ubtTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | The total number of reservation hours that were used.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubtTotal :: Lens.Lens' UtilizationByTime (Lude.Maybe ReservationAggregates)
ubtTotal = Lens.lens (total :: UtilizationByTime -> Lude.Maybe ReservationAggregates) (\s a -> s {total = a} :: UtilizationByTime)
{-# DEPRECATED ubtTotal "Use generic-lens or generic-optics with 'total' instead." #-}

instance Lude.FromJSON UtilizationByTime where
  parseJSON =
    Lude.withObject
      "UtilizationByTime"
      ( \x ->
          UtilizationByTime'
            Lude.<$> (x Lude..:? "Groups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "TimePeriod")
            Lude.<*> (x Lude..:? "Total")
      )
