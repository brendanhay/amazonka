{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ResultByTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ResultByTime
  ( ResultByTime (..),

    -- * Smart constructor
    mkResultByTime,

    -- * Lenses
    rbtGroups,
    rbtTimePeriod,
    rbtTotal,
    rbtEstimated,
  )
where

import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.CostExplorer.Types.Group
import Network.AWS.CostExplorer.Types.MetricValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The result that is associated with a time period.
--
-- /See:/ 'mkResultByTime' smart constructor.
data ResultByTime = ResultByTime'
  { groups :: Lude.Maybe [Group],
    timePeriod :: Lude.Maybe DateInterval,
    total :: Lude.Maybe (Lude.HashMap Lude.Text (MetricValue)),
    estimated :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResultByTime' with the minimum fields required to make a request.
--
-- * 'estimated' - Whether the result is estimated.
-- * 'groups' - The groups that this time period includes.
-- * 'timePeriod' - The time period that the result covers.
-- * 'total' - The total amount of cost or usage accrued during the time period.
mkResultByTime ::
  ResultByTime
mkResultByTime =
  ResultByTime'
    { groups = Lude.Nothing,
      timePeriod = Lude.Nothing,
      total = Lude.Nothing,
      estimated = Lude.Nothing
    }

-- | The groups that this time period includes.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbtGroups :: Lens.Lens' ResultByTime (Lude.Maybe [Group])
rbtGroups = Lens.lens (groups :: ResultByTime -> Lude.Maybe [Group]) (\s a -> s {groups = a} :: ResultByTime)
{-# DEPRECATED rbtGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The time period that the result covers.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbtTimePeriod :: Lens.Lens' ResultByTime (Lude.Maybe DateInterval)
rbtTimePeriod = Lens.lens (timePeriod :: ResultByTime -> Lude.Maybe DateInterval) (\s a -> s {timePeriod = a} :: ResultByTime)
{-# DEPRECATED rbtTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | The total amount of cost or usage accrued during the time period.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbtTotal :: Lens.Lens' ResultByTime (Lude.Maybe (Lude.HashMap Lude.Text (MetricValue)))
rbtTotal = Lens.lens (total :: ResultByTime -> Lude.Maybe (Lude.HashMap Lude.Text (MetricValue))) (\s a -> s {total = a} :: ResultByTime)
{-# DEPRECATED rbtTotal "Use generic-lens or generic-optics with 'total' instead." #-}

-- | Whether the result is estimated.
--
-- /Note:/ Consider using 'estimated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbtEstimated :: Lens.Lens' ResultByTime (Lude.Maybe Lude.Bool)
rbtEstimated = Lens.lens (estimated :: ResultByTime -> Lude.Maybe Lude.Bool) (\s a -> s {estimated = a} :: ResultByTime)
{-# DEPRECATED rbtEstimated "Use generic-lens or generic-optics with 'estimated' instead." #-}

instance Lude.FromJSON ResultByTime where
  parseJSON =
    Lude.withObject
      "ResultByTime"
      ( \x ->
          ResultByTime'
            Lude.<$> (x Lude..:? "Groups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "TimePeriod")
            Lude.<*> (x Lude..:? "Total" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Estimated")
      )
