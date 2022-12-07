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
-- Module      : Amazonka.BackupGateway.Types.MaintenanceStartTime
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BackupGateway.Types.MaintenanceStartTime where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This is your gateway\'s weekly maintenance start time including the day
-- and time of the week. Note that values are in terms of the gateway\'s
-- time zone. Can be weekly or monthly.
--
-- /See:/ 'newMaintenanceStartTime' smart constructor.
data MaintenanceStartTime = MaintenanceStartTime'
  { -- | An ordinal number between 0 and 6 that represents the day of the week,
    -- where 0 represents Sunday and 6 represents Saturday. The day of week is
    -- in the time zone of the gateway.
    dayOfWeek :: Prelude.Maybe Prelude.Natural,
    -- | The day of the month component of the maintenance start time represented
    -- as an ordinal number from 1 to 28, where 1 represents the first day of
    -- the month and 28 represents the last day of the month.
    dayOfMonth :: Prelude.Maybe Prelude.Natural,
    -- | The hour component of the maintenance start time represented as /hh/,
    -- where /hh/ is the hour (0 to 23). The hour of the day is in the time
    -- zone of the gateway.
    hourOfDay :: Prelude.Natural,
    -- | The minute component of the maintenance start time represented as /mm/,
    -- where /mm/ is the minute (0 to 59). The minute of the hour is in the
    -- time zone of the gateway.
    minuteOfHour :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceStartTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dayOfWeek', 'maintenanceStartTime_dayOfWeek' - An ordinal number between 0 and 6 that represents the day of the week,
-- where 0 represents Sunday and 6 represents Saturday. The day of week is
-- in the time zone of the gateway.
--
-- 'dayOfMonth', 'maintenanceStartTime_dayOfMonth' - The day of the month component of the maintenance start time represented
-- as an ordinal number from 1 to 28, where 1 represents the first day of
-- the month and 28 represents the last day of the month.
--
-- 'hourOfDay', 'maintenanceStartTime_hourOfDay' - The hour component of the maintenance start time represented as /hh/,
-- where /hh/ is the hour (0 to 23). The hour of the day is in the time
-- zone of the gateway.
--
-- 'minuteOfHour', 'maintenanceStartTime_minuteOfHour' - The minute component of the maintenance start time represented as /mm/,
-- where /mm/ is the minute (0 to 59). The minute of the hour is in the
-- time zone of the gateway.
newMaintenanceStartTime ::
  -- | 'hourOfDay'
  Prelude.Natural ->
  -- | 'minuteOfHour'
  Prelude.Natural ->
  MaintenanceStartTime
newMaintenanceStartTime pHourOfDay_ pMinuteOfHour_ =
  MaintenanceStartTime'
    { dayOfWeek = Prelude.Nothing,
      dayOfMonth = Prelude.Nothing,
      hourOfDay = pHourOfDay_,
      minuteOfHour = pMinuteOfHour_
    }

-- | An ordinal number between 0 and 6 that represents the day of the week,
-- where 0 represents Sunday and 6 represents Saturday. The day of week is
-- in the time zone of the gateway.
maintenanceStartTime_dayOfWeek :: Lens.Lens' MaintenanceStartTime (Prelude.Maybe Prelude.Natural)
maintenanceStartTime_dayOfWeek = Lens.lens (\MaintenanceStartTime' {dayOfWeek} -> dayOfWeek) (\s@MaintenanceStartTime' {} a -> s {dayOfWeek = a} :: MaintenanceStartTime)

-- | The day of the month component of the maintenance start time represented
-- as an ordinal number from 1 to 28, where 1 represents the first day of
-- the month and 28 represents the last day of the month.
maintenanceStartTime_dayOfMonth :: Lens.Lens' MaintenanceStartTime (Prelude.Maybe Prelude.Natural)
maintenanceStartTime_dayOfMonth = Lens.lens (\MaintenanceStartTime' {dayOfMonth} -> dayOfMonth) (\s@MaintenanceStartTime' {} a -> s {dayOfMonth = a} :: MaintenanceStartTime)

-- | The hour component of the maintenance start time represented as /hh/,
-- where /hh/ is the hour (0 to 23). The hour of the day is in the time
-- zone of the gateway.
maintenanceStartTime_hourOfDay :: Lens.Lens' MaintenanceStartTime Prelude.Natural
maintenanceStartTime_hourOfDay = Lens.lens (\MaintenanceStartTime' {hourOfDay} -> hourOfDay) (\s@MaintenanceStartTime' {} a -> s {hourOfDay = a} :: MaintenanceStartTime)

-- | The minute component of the maintenance start time represented as /mm/,
-- where /mm/ is the minute (0 to 59). The minute of the hour is in the
-- time zone of the gateway.
maintenanceStartTime_minuteOfHour :: Lens.Lens' MaintenanceStartTime Prelude.Natural
maintenanceStartTime_minuteOfHour = Lens.lens (\MaintenanceStartTime' {minuteOfHour} -> minuteOfHour) (\s@MaintenanceStartTime' {} a -> s {minuteOfHour = a} :: MaintenanceStartTime)

instance Data.FromJSON MaintenanceStartTime where
  parseJSON =
    Data.withObject
      "MaintenanceStartTime"
      ( \x ->
          MaintenanceStartTime'
            Prelude.<$> (x Data..:? "DayOfWeek")
            Prelude.<*> (x Data..:? "DayOfMonth")
            Prelude.<*> (x Data..: "HourOfDay")
            Prelude.<*> (x Data..: "MinuteOfHour")
      )

instance Prelude.Hashable MaintenanceStartTime where
  hashWithSalt _salt MaintenanceStartTime' {..} =
    _salt `Prelude.hashWithSalt` dayOfWeek
      `Prelude.hashWithSalt` dayOfMonth
      `Prelude.hashWithSalt` hourOfDay
      `Prelude.hashWithSalt` minuteOfHour

instance Prelude.NFData MaintenanceStartTime where
  rnf MaintenanceStartTime' {..} =
    Prelude.rnf dayOfWeek
      `Prelude.seq` Prelude.rnf dayOfMonth
      `Prelude.seq` Prelude.rnf hourOfDay
      `Prelude.seq` Prelude.rnf minuteOfHour
