-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.WeeklyAutoScalingSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.WeeklyAutoScalingSchedule
  ( WeeklyAutoScalingSchedule (..),

    -- * Smart constructor
    mkWeeklyAutoScalingSchedule,

    -- * Lenses
    wassThursday,
    wassWednesday,
    wassSaturday,
    wassMonday,
    wassFriday,
    wassSunday,
    wassTuesday,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a time-based instance's auto scaling schedule. The schedule consists of a set of key-value pairs.
--
--
--     * The key is the time period (a UTC hour) and must be an integer from 0 - 23.
--
--
--     * The value indicates whether the instance should be online or offline for the specified period, and must be set to "on" or "off"
--
--
-- The default setting for all time periods is off, so you use the following parameters primarily to specify the online periods. You don't have to explicitly specify offline periods unless you want to change an online period to an offline period.
-- The following example specifies that the instance should be online for four hours, from UTC 1200 - 1600. It will be off for the remainder of the day.
-- @{ "12":"on", "13":"on", "14":"on", "15":"on" } @
--
-- /See:/ 'mkWeeklyAutoScalingSchedule' smart constructor.
data WeeklyAutoScalingSchedule = WeeklyAutoScalingSchedule'
  { thursday ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    wednesday ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    saturday ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    monday ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    friday ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    sunday ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    tuesday ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WeeklyAutoScalingSchedule' with the minimum fields required to make a request.
--
-- * 'friday' - The schedule for Friday.
-- * 'monday' - The schedule for Monday.
-- * 'saturday' - The schedule for Saturday.
-- * 'sunday' - The schedule for Sunday.
-- * 'thursday' - The schedule for Thursday.
-- * 'tuesday' - The schedule for Tuesday.
-- * 'wednesday' - The schedule for Wednesday.
mkWeeklyAutoScalingSchedule ::
  WeeklyAutoScalingSchedule
mkWeeklyAutoScalingSchedule =
  WeeklyAutoScalingSchedule'
    { thursday = Lude.Nothing,
      wednesday = Lude.Nothing,
      saturday = Lude.Nothing,
      monday = Lude.Nothing,
      friday = Lude.Nothing,
      sunday = Lude.Nothing,
      tuesday = Lude.Nothing
    }

-- | The schedule for Thursday.
--
-- /Note:/ Consider using 'thursday' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wassThursday :: Lens.Lens' WeeklyAutoScalingSchedule (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
wassThursday = Lens.lens (thursday :: WeeklyAutoScalingSchedule -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {thursday = a} :: WeeklyAutoScalingSchedule)
{-# DEPRECATED wassThursday "Use generic-lens or generic-optics with 'thursday' instead." #-}

-- | The schedule for Wednesday.
--
-- /Note:/ Consider using 'wednesday' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wassWednesday :: Lens.Lens' WeeklyAutoScalingSchedule (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
wassWednesday = Lens.lens (wednesday :: WeeklyAutoScalingSchedule -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {wednesday = a} :: WeeklyAutoScalingSchedule)
{-# DEPRECATED wassWednesday "Use generic-lens or generic-optics with 'wednesday' instead." #-}

-- | The schedule for Saturday.
--
-- /Note:/ Consider using 'saturday' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wassSaturday :: Lens.Lens' WeeklyAutoScalingSchedule (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
wassSaturday = Lens.lens (saturday :: WeeklyAutoScalingSchedule -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {saturday = a} :: WeeklyAutoScalingSchedule)
{-# DEPRECATED wassSaturday "Use generic-lens or generic-optics with 'saturday' instead." #-}

-- | The schedule for Monday.
--
-- /Note:/ Consider using 'monday' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wassMonday :: Lens.Lens' WeeklyAutoScalingSchedule (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
wassMonday = Lens.lens (monday :: WeeklyAutoScalingSchedule -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {monday = a} :: WeeklyAutoScalingSchedule)
{-# DEPRECATED wassMonday "Use generic-lens or generic-optics with 'monday' instead." #-}

-- | The schedule for Friday.
--
-- /Note:/ Consider using 'friday' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wassFriday :: Lens.Lens' WeeklyAutoScalingSchedule (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
wassFriday = Lens.lens (friday :: WeeklyAutoScalingSchedule -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {friday = a} :: WeeklyAutoScalingSchedule)
{-# DEPRECATED wassFriday "Use generic-lens or generic-optics with 'friday' instead." #-}

-- | The schedule for Sunday.
--
-- /Note:/ Consider using 'sunday' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wassSunday :: Lens.Lens' WeeklyAutoScalingSchedule (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
wassSunday = Lens.lens (sunday :: WeeklyAutoScalingSchedule -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {sunday = a} :: WeeklyAutoScalingSchedule)
{-# DEPRECATED wassSunday "Use generic-lens or generic-optics with 'sunday' instead." #-}

-- | The schedule for Tuesday.
--
-- /Note:/ Consider using 'tuesday' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wassTuesday :: Lens.Lens' WeeklyAutoScalingSchedule (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
wassTuesday = Lens.lens (tuesday :: WeeklyAutoScalingSchedule -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tuesday = a} :: WeeklyAutoScalingSchedule)
{-# DEPRECATED wassTuesday "Use generic-lens or generic-optics with 'tuesday' instead." #-}

instance Lude.FromJSON WeeklyAutoScalingSchedule where
  parseJSON =
    Lude.withObject
      "WeeklyAutoScalingSchedule"
      ( \x ->
          WeeklyAutoScalingSchedule'
            Lude.<$> (x Lude..:? "Thursday" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Wednesday" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Saturday" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Monday" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Friday" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Sunday" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Tuesday" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON WeeklyAutoScalingSchedule where
  toJSON WeeklyAutoScalingSchedule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Thursday" Lude..=) Lude.<$> thursday,
            ("Wednesday" Lude..=) Lude.<$> wednesday,
            ("Saturday" Lude..=) Lude.<$> saturday,
            ("Monday" Lude..=) Lude.<$> monday,
            ("Friday" Lude..=) Lude.<$> friday,
            ("Sunday" Lude..=) Lude.<$> sunday,
            ("Tuesday" Lude..=) Lude.<$> tuesday
          ]
      )
