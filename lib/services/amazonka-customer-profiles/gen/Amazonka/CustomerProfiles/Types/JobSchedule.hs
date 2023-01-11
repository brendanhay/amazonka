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
-- Module      : Amazonka.CustomerProfiles.Types.JobSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.JobSchedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.JobScheduleDayOfTheWeek
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The day and time when do you want to start the Identity Resolution Job
-- every week.
--
-- /See:/ 'newJobSchedule' smart constructor.
data JobSchedule = JobSchedule'
  { -- | The day when the Identity Resolution Job should run every week.
    dayOfTheWeek :: JobScheduleDayOfTheWeek,
    -- | The time when the Identity Resolution Job should run every week.
    time :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dayOfTheWeek', 'jobSchedule_dayOfTheWeek' - The day when the Identity Resolution Job should run every week.
--
-- 'time', 'jobSchedule_time' - The time when the Identity Resolution Job should run every week.
newJobSchedule ::
  -- | 'dayOfTheWeek'
  JobScheduleDayOfTheWeek ->
  -- | 'time'
  Prelude.Text ->
  JobSchedule
newJobSchedule pDayOfTheWeek_ pTime_ =
  JobSchedule'
    { dayOfTheWeek = pDayOfTheWeek_,
      time = pTime_
    }

-- | The day when the Identity Resolution Job should run every week.
jobSchedule_dayOfTheWeek :: Lens.Lens' JobSchedule JobScheduleDayOfTheWeek
jobSchedule_dayOfTheWeek = Lens.lens (\JobSchedule' {dayOfTheWeek} -> dayOfTheWeek) (\s@JobSchedule' {} a -> s {dayOfTheWeek = a} :: JobSchedule)

-- | The time when the Identity Resolution Job should run every week.
jobSchedule_time :: Lens.Lens' JobSchedule Prelude.Text
jobSchedule_time = Lens.lens (\JobSchedule' {time} -> time) (\s@JobSchedule' {} a -> s {time = a} :: JobSchedule)

instance Data.FromJSON JobSchedule where
  parseJSON =
    Data.withObject
      "JobSchedule"
      ( \x ->
          JobSchedule'
            Prelude.<$> (x Data..: "DayOfTheWeek")
            Prelude.<*> (x Data..: "Time")
      )

instance Prelude.Hashable JobSchedule where
  hashWithSalt _salt JobSchedule' {..} =
    _salt `Prelude.hashWithSalt` dayOfTheWeek
      `Prelude.hashWithSalt` time

instance Prelude.NFData JobSchedule where
  rnf JobSchedule' {..} =
    Prelude.rnf dayOfTheWeek
      `Prelude.seq` Prelude.rnf time

instance Data.ToJSON JobSchedule where
  toJSON JobSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DayOfTheWeek" Data..= dayOfTheWeek),
            Prelude.Just ("Time" Data..= time)
          ]
      )
