{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    wstDayOfWeek,
    wstTimeOfDay,
    wstTimeZone,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types.DayOfWeek as Types
import qualified Network.AWS.Prelude as Core

-- | The scheduled time period relative to UTC during which Amazon MQ begins to apply pending updates or patches to the broker.
--
-- /See:/ 'mkWeeklyStartTime' smart constructor.
data WeeklyStartTime = WeeklyStartTime'
  { -- | Required. The day of the week.
    dayOfWeek :: Core.Maybe Types.DayOfWeek,
    -- | Required. The time, in 24-hour format.
    timeOfDay :: Core.Maybe Core.Text,
    -- | The time zone, UTC by default, in either the Country/City format, or the UTC offset format.
    timeZone :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WeeklyStartTime' value with any optional fields omitted.
mkWeeklyStartTime ::
  WeeklyStartTime
mkWeeklyStartTime =
  WeeklyStartTime'
    { dayOfWeek = Core.Nothing,
      timeOfDay = Core.Nothing,
      timeZone = Core.Nothing
    }

-- | Required. The day of the week.
--
-- /Note:/ Consider using 'dayOfWeek' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wstDayOfWeek :: Lens.Lens' WeeklyStartTime (Core.Maybe Types.DayOfWeek)
wstDayOfWeek = Lens.field @"dayOfWeek"
{-# DEPRECATED wstDayOfWeek "Use generic-lens or generic-optics with 'dayOfWeek' instead." #-}

-- | Required. The time, in 24-hour format.
--
-- /Note:/ Consider using 'timeOfDay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wstTimeOfDay :: Lens.Lens' WeeklyStartTime (Core.Maybe Core.Text)
wstTimeOfDay = Lens.field @"timeOfDay"
{-# DEPRECATED wstTimeOfDay "Use generic-lens or generic-optics with 'timeOfDay' instead." #-}

-- | The time zone, UTC by default, in either the Country/City format, or the UTC offset format.
--
-- /Note:/ Consider using 'timeZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wstTimeZone :: Lens.Lens' WeeklyStartTime (Core.Maybe Core.Text)
wstTimeZone = Lens.field @"timeZone"
{-# DEPRECATED wstTimeZone "Use generic-lens or generic-optics with 'timeZone' instead." #-}

instance Core.FromJSON WeeklyStartTime where
  toJSON WeeklyStartTime {..} =
    Core.object
      ( Core.catMaybes
          [ ("dayOfWeek" Core..=) Core.<$> dayOfWeek,
            ("timeOfDay" Core..=) Core.<$> timeOfDay,
            ("timeZone" Core..=) Core.<$> timeZone
          ]
      )

instance Core.FromJSON WeeklyStartTime where
  parseJSON =
    Core.withObject "WeeklyStartTime" Core.$
      \x ->
        WeeklyStartTime'
          Core.<$> (x Core..:? "dayOfWeek")
          Core.<*> (x Core..:? "timeOfDay")
          Core.<*> (x Core..:? "timeZone")
