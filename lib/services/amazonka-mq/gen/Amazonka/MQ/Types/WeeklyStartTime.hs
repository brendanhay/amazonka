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
-- Module      : Amazonka.MQ.Types.WeeklyStartTime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.WeeklyStartTime where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MQ.Types.DayOfWeek
import qualified Amazonka.Prelude as Prelude

-- | The scheduled time period relative to UTC during which Amazon MQ begins
-- to apply pending updates or patches to the broker.
--
-- /See:/ 'newWeeklyStartTime' smart constructor.
data WeeklyStartTime = WeeklyStartTime'
  { -- | The time zone, UTC by default, in either the Country\/City format, or
    -- the UTC offset format.
    timeZone :: Prelude.Maybe Prelude.Text,
    -- | Required. The time, in 24-hour format.
    timeOfDay :: Prelude.Text,
    -- | Required. The day of the week.
    dayOfWeek :: DayOfWeek
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WeeklyStartTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeZone', 'weeklyStartTime_timeZone' - The time zone, UTC by default, in either the Country\/City format, or
-- the UTC offset format.
--
-- 'timeOfDay', 'weeklyStartTime_timeOfDay' - Required. The time, in 24-hour format.
--
-- 'dayOfWeek', 'weeklyStartTime_dayOfWeek' - Required. The day of the week.
newWeeklyStartTime ::
  -- | 'timeOfDay'
  Prelude.Text ->
  -- | 'dayOfWeek'
  DayOfWeek ->
  WeeklyStartTime
newWeeklyStartTime pTimeOfDay_ pDayOfWeek_ =
  WeeklyStartTime'
    { timeZone = Prelude.Nothing,
      timeOfDay = pTimeOfDay_,
      dayOfWeek = pDayOfWeek_
    }

-- | The time zone, UTC by default, in either the Country\/City format, or
-- the UTC offset format.
weeklyStartTime_timeZone :: Lens.Lens' WeeklyStartTime (Prelude.Maybe Prelude.Text)
weeklyStartTime_timeZone = Lens.lens (\WeeklyStartTime' {timeZone} -> timeZone) (\s@WeeklyStartTime' {} a -> s {timeZone = a} :: WeeklyStartTime)

-- | Required. The time, in 24-hour format.
weeklyStartTime_timeOfDay :: Lens.Lens' WeeklyStartTime Prelude.Text
weeklyStartTime_timeOfDay = Lens.lens (\WeeklyStartTime' {timeOfDay} -> timeOfDay) (\s@WeeklyStartTime' {} a -> s {timeOfDay = a} :: WeeklyStartTime)

-- | Required. The day of the week.
weeklyStartTime_dayOfWeek :: Lens.Lens' WeeklyStartTime DayOfWeek
weeklyStartTime_dayOfWeek = Lens.lens (\WeeklyStartTime' {dayOfWeek} -> dayOfWeek) (\s@WeeklyStartTime' {} a -> s {dayOfWeek = a} :: WeeklyStartTime)

instance Data.FromJSON WeeklyStartTime where
  parseJSON =
    Data.withObject
      "WeeklyStartTime"
      ( \x ->
          WeeklyStartTime'
            Prelude.<$> (x Data..:? "timeZone")
            Prelude.<*> (x Data..: "timeOfDay")
            Prelude.<*> (x Data..: "dayOfWeek")
      )

instance Prelude.Hashable WeeklyStartTime where
  hashWithSalt _salt WeeklyStartTime' {..} =
    _salt `Prelude.hashWithSalt` timeZone
      `Prelude.hashWithSalt` timeOfDay
      `Prelude.hashWithSalt` dayOfWeek

instance Prelude.NFData WeeklyStartTime where
  rnf WeeklyStartTime' {..} =
    Prelude.rnf timeZone
      `Prelude.seq` Prelude.rnf timeOfDay
      `Prelude.seq` Prelude.rnf dayOfWeek

instance Data.ToJSON WeeklyStartTime where
  toJSON WeeklyStartTime' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("timeZone" Data..=) Prelude.<$> timeZone,
            Prelude.Just ("timeOfDay" Data..= timeOfDay),
            Prelude.Just ("dayOfWeek" Data..= dayOfWeek)
          ]
      )
