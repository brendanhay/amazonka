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
-- Module      : Amazonka.MacieV2.Types.JobScheduleFrequency
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.JobScheduleFrequency where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.DailySchedule
import Amazonka.MacieV2.Types.MonthlySchedule
import Amazonka.MacieV2.Types.WeeklySchedule
import qualified Amazonka.Prelude as Prelude

-- | Specifies the recurrence pattern for running a classification job.
--
-- /See:/ 'newJobScheduleFrequency' smart constructor.
data JobScheduleFrequency = JobScheduleFrequency'
  { -- | Specifies a daily recurrence pattern for running the job.
    dailySchedule :: Prelude.Maybe DailySchedule,
    -- | Specifies a monthly recurrence pattern for running the job.
    monthlySchedule :: Prelude.Maybe MonthlySchedule,
    -- | Specifies a weekly recurrence pattern for running the job.
    weeklySchedule :: Prelude.Maybe WeeklySchedule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobScheduleFrequency' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dailySchedule', 'jobScheduleFrequency_dailySchedule' - Specifies a daily recurrence pattern for running the job.
--
-- 'monthlySchedule', 'jobScheduleFrequency_monthlySchedule' - Specifies a monthly recurrence pattern for running the job.
--
-- 'weeklySchedule', 'jobScheduleFrequency_weeklySchedule' - Specifies a weekly recurrence pattern for running the job.
newJobScheduleFrequency ::
  JobScheduleFrequency
newJobScheduleFrequency =
  JobScheduleFrequency'
    { dailySchedule =
        Prelude.Nothing,
      monthlySchedule = Prelude.Nothing,
      weeklySchedule = Prelude.Nothing
    }

-- | Specifies a daily recurrence pattern for running the job.
jobScheduleFrequency_dailySchedule :: Lens.Lens' JobScheduleFrequency (Prelude.Maybe DailySchedule)
jobScheduleFrequency_dailySchedule = Lens.lens (\JobScheduleFrequency' {dailySchedule} -> dailySchedule) (\s@JobScheduleFrequency' {} a -> s {dailySchedule = a} :: JobScheduleFrequency)

-- | Specifies a monthly recurrence pattern for running the job.
jobScheduleFrequency_monthlySchedule :: Lens.Lens' JobScheduleFrequency (Prelude.Maybe MonthlySchedule)
jobScheduleFrequency_monthlySchedule = Lens.lens (\JobScheduleFrequency' {monthlySchedule} -> monthlySchedule) (\s@JobScheduleFrequency' {} a -> s {monthlySchedule = a} :: JobScheduleFrequency)

-- | Specifies a weekly recurrence pattern for running the job.
jobScheduleFrequency_weeklySchedule :: Lens.Lens' JobScheduleFrequency (Prelude.Maybe WeeklySchedule)
jobScheduleFrequency_weeklySchedule = Lens.lens (\JobScheduleFrequency' {weeklySchedule} -> weeklySchedule) (\s@JobScheduleFrequency' {} a -> s {weeklySchedule = a} :: JobScheduleFrequency)

instance Data.FromJSON JobScheduleFrequency where
  parseJSON =
    Data.withObject
      "JobScheduleFrequency"
      ( \x ->
          JobScheduleFrequency'
            Prelude.<$> (x Data..:? "dailySchedule")
            Prelude.<*> (x Data..:? "monthlySchedule")
            Prelude.<*> (x Data..:? "weeklySchedule")
      )

instance Prelude.Hashable JobScheduleFrequency where
  hashWithSalt _salt JobScheduleFrequency' {..} =
    _salt `Prelude.hashWithSalt` dailySchedule
      `Prelude.hashWithSalt` monthlySchedule
      `Prelude.hashWithSalt` weeklySchedule

instance Prelude.NFData JobScheduleFrequency where
  rnf JobScheduleFrequency' {..} =
    Prelude.rnf dailySchedule
      `Prelude.seq` Prelude.rnf monthlySchedule
      `Prelude.seq` Prelude.rnf weeklySchedule

instance Data.ToJSON JobScheduleFrequency where
  toJSON JobScheduleFrequency' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dailySchedule" Data..=) Prelude.<$> dailySchedule,
            ("monthlySchedule" Data..=)
              Prelude.<$> monthlySchedule,
            ("weeklySchedule" Data..=)
              Prelude.<$> weeklySchedule
          ]
      )
