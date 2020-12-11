-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.WeeklyStartTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.WeeklyStartTime
  ( WeeklyStartTime (..),

    -- * Smart constructor
    mkWeeklyStartTime,

    -- * Lenses
    wstTimeOfDay,
    wstTimeZone,
    wstDayOfWeek,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.DayOfWeek
import qualified Network.AWS.Prelude as Lude

-- | The scheduled time period relative to UTC during which Amazon MQ begins to apply pending updates or patches to the broker.
--
-- /See:/ 'mkWeeklyStartTime' smart constructor.
data WeeklyStartTime = WeeklyStartTime'
  { timeOfDay ::
      Lude.Maybe Lude.Text,
    timeZone :: Lude.Maybe Lude.Text,
    dayOfWeek :: Lude.Maybe DayOfWeek
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WeeklyStartTime' with the minimum fields required to make a request.
--
-- * 'dayOfWeek' - Required. The day of the week.
-- * 'timeOfDay' - Required. The time, in 24-hour format.
-- * 'timeZone' - The time zone, UTC by default, in either the Country/City format, or the UTC offset format.
mkWeeklyStartTime ::
  WeeklyStartTime
mkWeeklyStartTime =
  WeeklyStartTime'
    { timeOfDay = Lude.Nothing,
      timeZone = Lude.Nothing,
      dayOfWeek = Lude.Nothing
    }

-- | Required. The time, in 24-hour format.
--
-- /Note:/ Consider using 'timeOfDay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wstTimeOfDay :: Lens.Lens' WeeklyStartTime (Lude.Maybe Lude.Text)
wstTimeOfDay = Lens.lens (timeOfDay :: WeeklyStartTime -> Lude.Maybe Lude.Text) (\s a -> s {timeOfDay = a} :: WeeklyStartTime)
{-# DEPRECATED wstTimeOfDay "Use generic-lens or generic-optics with 'timeOfDay' instead." #-}

-- | The time zone, UTC by default, in either the Country/City format, or the UTC offset format.
--
-- /Note:/ Consider using 'timeZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wstTimeZone :: Lens.Lens' WeeklyStartTime (Lude.Maybe Lude.Text)
wstTimeZone = Lens.lens (timeZone :: WeeklyStartTime -> Lude.Maybe Lude.Text) (\s a -> s {timeZone = a} :: WeeklyStartTime)
{-# DEPRECATED wstTimeZone "Use generic-lens or generic-optics with 'timeZone' instead." #-}

-- | Required. The day of the week.
--
-- /Note:/ Consider using 'dayOfWeek' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wstDayOfWeek :: Lens.Lens' WeeklyStartTime (Lude.Maybe DayOfWeek)
wstDayOfWeek = Lens.lens (dayOfWeek :: WeeklyStartTime -> Lude.Maybe DayOfWeek) (\s a -> s {dayOfWeek = a} :: WeeklyStartTime)
{-# DEPRECATED wstDayOfWeek "Use generic-lens or generic-optics with 'dayOfWeek' instead." #-}

instance Lude.FromJSON WeeklyStartTime where
  parseJSON =
    Lude.withObject
      "WeeklyStartTime"
      ( \x ->
          WeeklyStartTime'
            Lude.<$> (x Lude..:? "timeOfDay")
            Lude.<*> (x Lude..:? "timeZone")
            Lude.<*> (x Lude..:? "dayOfWeek")
      )

instance Lude.ToJSON WeeklyStartTime where
  toJSON WeeklyStartTime' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("timeOfDay" Lude..=) Lude.<$> timeOfDay,
            ("timeZone" Lude..=) Lude.<$> timeZone,
            ("dayOfWeek" Lude..=) Lude.<$> dayOfWeek
          ]
      )
