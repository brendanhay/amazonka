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
-- Module      : Amazonka.Pinpoint.Types.JourneySchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.JourneySchedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the schedule settings for a journey.
--
-- /See:/ 'newJourneySchedule' smart constructor.
data JourneySchedule = JourneySchedule'
  { -- | The scheduled time, in ISO 8601 format, when the journey ended or will
    -- end.
    endTime :: Prelude.Maybe Data.ISO8601,
    -- | The scheduled time, in ISO 8601 format, when the journey began or will
    -- begin.
    startTime :: Prelude.Maybe Data.ISO8601,
    -- | The starting UTC offset for the journey schedule, if the value of the
    -- journey\'s LocalTime property is true. Valid values are: UTC, UTC+01,
    -- UTC+02, UTC+03, UTC+03:30, UTC+04, UTC+04:30, UTC+05, UTC+05:30,
    -- UTC+05:45, UTC+06, UTC+06:30, UTC+07, UTC+08, UTC+08:45, UTC+09,
    -- UTC+09:30, UTC+10, UTC+10:30, UTC+11, UTC+12, UTC+12:45, UTC+13,
    -- UTC+13:45, UTC-02, UTC-02:30, UTC-03, UTC-03:30, UTC-04, UTC-05, UTC-06,
    -- UTC-07, UTC-08, UTC-09, UTC-09:30, UTC-10, and UTC-11.
    timezone :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JourneySchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'journeySchedule_endTime' - The scheduled time, in ISO 8601 format, when the journey ended or will
-- end.
--
-- 'startTime', 'journeySchedule_startTime' - The scheduled time, in ISO 8601 format, when the journey began or will
-- begin.
--
-- 'timezone', 'journeySchedule_timezone' - The starting UTC offset for the journey schedule, if the value of the
-- journey\'s LocalTime property is true. Valid values are: UTC, UTC+01,
-- UTC+02, UTC+03, UTC+03:30, UTC+04, UTC+04:30, UTC+05, UTC+05:30,
-- UTC+05:45, UTC+06, UTC+06:30, UTC+07, UTC+08, UTC+08:45, UTC+09,
-- UTC+09:30, UTC+10, UTC+10:30, UTC+11, UTC+12, UTC+12:45, UTC+13,
-- UTC+13:45, UTC-02, UTC-02:30, UTC-03, UTC-03:30, UTC-04, UTC-05, UTC-06,
-- UTC-07, UTC-08, UTC-09, UTC-09:30, UTC-10, and UTC-11.
newJourneySchedule ::
  JourneySchedule
newJourneySchedule =
  JourneySchedule'
    { endTime = Prelude.Nothing,
      startTime = Prelude.Nothing,
      timezone = Prelude.Nothing
    }

-- | The scheduled time, in ISO 8601 format, when the journey ended or will
-- end.
journeySchedule_endTime :: Lens.Lens' JourneySchedule (Prelude.Maybe Prelude.UTCTime)
journeySchedule_endTime = Lens.lens (\JourneySchedule' {endTime} -> endTime) (\s@JourneySchedule' {} a -> s {endTime = a} :: JourneySchedule) Prelude.. Lens.mapping Data._Time

-- | The scheduled time, in ISO 8601 format, when the journey began or will
-- begin.
journeySchedule_startTime :: Lens.Lens' JourneySchedule (Prelude.Maybe Prelude.UTCTime)
journeySchedule_startTime = Lens.lens (\JourneySchedule' {startTime} -> startTime) (\s@JourneySchedule' {} a -> s {startTime = a} :: JourneySchedule) Prelude.. Lens.mapping Data._Time

-- | The starting UTC offset for the journey schedule, if the value of the
-- journey\'s LocalTime property is true. Valid values are: UTC, UTC+01,
-- UTC+02, UTC+03, UTC+03:30, UTC+04, UTC+04:30, UTC+05, UTC+05:30,
-- UTC+05:45, UTC+06, UTC+06:30, UTC+07, UTC+08, UTC+08:45, UTC+09,
-- UTC+09:30, UTC+10, UTC+10:30, UTC+11, UTC+12, UTC+12:45, UTC+13,
-- UTC+13:45, UTC-02, UTC-02:30, UTC-03, UTC-03:30, UTC-04, UTC-05, UTC-06,
-- UTC-07, UTC-08, UTC-09, UTC-09:30, UTC-10, and UTC-11.
journeySchedule_timezone :: Lens.Lens' JourneySchedule (Prelude.Maybe Prelude.Text)
journeySchedule_timezone = Lens.lens (\JourneySchedule' {timezone} -> timezone) (\s@JourneySchedule' {} a -> s {timezone = a} :: JourneySchedule)

instance Data.FromJSON JourneySchedule where
  parseJSON =
    Data.withObject
      "JourneySchedule"
      ( \x ->
          JourneySchedule'
            Prelude.<$> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "Timezone")
      )

instance Prelude.Hashable JourneySchedule where
  hashWithSalt _salt JourneySchedule' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` timezone

instance Prelude.NFData JourneySchedule where
  rnf JourneySchedule' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf timezone

instance Data.ToJSON JourneySchedule where
  toJSON JourneySchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndTime" Data..=) Prelude.<$> endTime,
            ("StartTime" Data..=) Prelude.<$> startTime,
            ("Timezone" Data..=) Prelude.<$> timezone
          ]
      )
