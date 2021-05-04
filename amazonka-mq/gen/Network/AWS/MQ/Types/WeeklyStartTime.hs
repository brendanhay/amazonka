{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.DayOfWeek
import qualified Network.AWS.Prelude as Prelude

-- | The scheduled time period relative to UTC during which Amazon MQ begins
-- to apply pending updates or patches to the broker.
--
-- /See:/ 'newWeeklyStartTime' smart constructor.
data WeeklyStartTime = WeeklyStartTime'
  { -- | Required. The day of the week.
    dayOfWeek :: Prelude.Maybe DayOfWeek,
    -- | Required. The time, in 24-hour format.
    timeOfDay :: Prelude.Maybe Prelude.Text,
    -- | The time zone, UTC by default, in either the Country\/City format, or
    -- the UTC offset format.
    timeZone :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { dayOfWeek = Prelude.Nothing,
      timeOfDay = Prelude.Nothing,
      timeZone = Prelude.Nothing
    }

-- | Required. The day of the week.
weeklyStartTime_dayOfWeek :: Lens.Lens' WeeklyStartTime (Prelude.Maybe DayOfWeek)
weeklyStartTime_dayOfWeek = Lens.lens (\WeeklyStartTime' {dayOfWeek} -> dayOfWeek) (\s@WeeklyStartTime' {} a -> s {dayOfWeek = a} :: WeeklyStartTime)

-- | Required. The time, in 24-hour format.
weeklyStartTime_timeOfDay :: Lens.Lens' WeeklyStartTime (Prelude.Maybe Prelude.Text)
weeklyStartTime_timeOfDay = Lens.lens (\WeeklyStartTime' {timeOfDay} -> timeOfDay) (\s@WeeklyStartTime' {} a -> s {timeOfDay = a} :: WeeklyStartTime)

-- | The time zone, UTC by default, in either the Country\/City format, or
-- the UTC offset format.
weeklyStartTime_timeZone :: Lens.Lens' WeeklyStartTime (Prelude.Maybe Prelude.Text)
weeklyStartTime_timeZone = Lens.lens (\WeeklyStartTime' {timeZone} -> timeZone) (\s@WeeklyStartTime' {} a -> s {timeZone = a} :: WeeklyStartTime)

instance Prelude.FromJSON WeeklyStartTime where
  parseJSON =
    Prelude.withObject
      "WeeklyStartTime"
      ( \x ->
          WeeklyStartTime'
            Prelude.<$> (x Prelude..:? "dayOfWeek")
            Prelude.<*> (x Prelude..:? "timeOfDay")
            Prelude.<*> (x Prelude..:? "timeZone")
      )

instance Prelude.Hashable WeeklyStartTime

instance Prelude.NFData WeeklyStartTime

instance Prelude.ToJSON WeeklyStartTime where
  toJSON WeeklyStartTime' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("dayOfWeek" Prelude..=) Prelude.<$> dayOfWeek,
            ("timeOfDay" Prelude..=) Prelude.<$> timeOfDay,
            ("timeZone" Prelude..=) Prelude.<$> timeZone
          ]
      )
