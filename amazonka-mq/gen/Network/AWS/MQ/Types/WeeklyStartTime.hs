{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.WeeklyStartTime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.WeeklyStartTime where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.DayOfWeek

-- | The scheduled time period relative to UTC during which Amazon MQ begins
-- to apply pending updates or patches to the broker.
--
-- /See:/ 'newWeeklyStartTime' smart constructor.
data WeeklyStartTime = WeeklyStartTime'
  { -- | Required. The day of the week.
    dayOfWeek :: Core.Maybe DayOfWeek,
    -- | Required. The time, in 24-hour format.
    timeOfDay :: Core.Maybe Core.Text,
    -- | The time zone, UTC by default, in either the Country\/City format, or
    -- the UTC offset format.
    timeZone :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'WeeklyStartTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dayOfWeek', 'weeklyStartTime_dayOfWeek' - Required. The day of the week.
--
-- 'timeOfDay', 'weeklyStartTime_timeOfDay' - Required. The time, in 24-hour format.
--
-- 'timeZone', 'weeklyStartTime_timeZone' - The time zone, UTC by default, in either the Country\/City format, or
-- the UTC offset format.
newWeeklyStartTime ::
  WeeklyStartTime
newWeeklyStartTime =
  WeeklyStartTime'
    { dayOfWeek = Core.Nothing,
      timeOfDay = Core.Nothing,
      timeZone = Core.Nothing
    }

-- | Required. The day of the week.
weeklyStartTime_dayOfWeek :: Lens.Lens' WeeklyStartTime (Core.Maybe DayOfWeek)
weeklyStartTime_dayOfWeek = Lens.lens (\WeeklyStartTime' {dayOfWeek} -> dayOfWeek) (\s@WeeklyStartTime' {} a -> s {dayOfWeek = a} :: WeeklyStartTime)

-- | Required. The time, in 24-hour format.
weeklyStartTime_timeOfDay :: Lens.Lens' WeeklyStartTime (Core.Maybe Core.Text)
weeklyStartTime_timeOfDay = Lens.lens (\WeeklyStartTime' {timeOfDay} -> timeOfDay) (\s@WeeklyStartTime' {} a -> s {timeOfDay = a} :: WeeklyStartTime)

-- | The time zone, UTC by default, in either the Country\/City format, or
-- the UTC offset format.
weeklyStartTime_timeZone :: Lens.Lens' WeeklyStartTime (Core.Maybe Core.Text)
weeklyStartTime_timeZone = Lens.lens (\WeeklyStartTime' {timeZone} -> timeZone) (\s@WeeklyStartTime' {} a -> s {timeZone = a} :: WeeklyStartTime)

instance Core.FromJSON WeeklyStartTime where
  parseJSON =
    Core.withObject
      "WeeklyStartTime"
      ( \x ->
          WeeklyStartTime'
            Core.<$> (x Core..:? "dayOfWeek")
            Core.<*> (x Core..:? "timeOfDay")
            Core.<*> (x Core..:? "timeZone")
      )

instance Core.Hashable WeeklyStartTime

instance Core.NFData WeeklyStartTime

instance Core.ToJSON WeeklyStartTime where
  toJSON WeeklyStartTime' {..} =
    Core.object
      ( Core.catMaybes
          [ ("dayOfWeek" Core..=) Core.<$> dayOfWeek,
            ("timeOfDay" Core..=) Core.<$> timeOfDay,
            ("timeZone" Core..=) Core.<$> timeZone
          ]
      )
