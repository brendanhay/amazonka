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
-- Module      : Amazonka.QuickSight.Types.ScheduleRefreshOnEntity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ScheduleRefreshOnEntity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DayOfWeek

-- | The refresh on entity for weekly or monthly schedules.
--
-- /See:/ 'newScheduleRefreshOnEntity' smart constructor.
data ScheduleRefreshOnEntity = ScheduleRefreshOnEntity'
  { -- | The day of the month that you want to schedule refresh on.
    dayOfMonth :: Prelude.Maybe Prelude.Text,
    -- | The day of the week that you want to schedule a refresh on.
    dayOfWeek :: Prelude.Maybe DayOfWeek
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduleRefreshOnEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dayOfMonth', 'scheduleRefreshOnEntity_dayOfMonth' - The day of the month that you want to schedule refresh on.
--
-- 'dayOfWeek', 'scheduleRefreshOnEntity_dayOfWeek' - The day of the week that you want to schedule a refresh on.
newScheduleRefreshOnEntity ::
  ScheduleRefreshOnEntity
newScheduleRefreshOnEntity =
  ScheduleRefreshOnEntity'
    { dayOfMonth =
        Prelude.Nothing,
      dayOfWeek = Prelude.Nothing
    }

-- | The day of the month that you want to schedule refresh on.
scheduleRefreshOnEntity_dayOfMonth :: Lens.Lens' ScheduleRefreshOnEntity (Prelude.Maybe Prelude.Text)
scheduleRefreshOnEntity_dayOfMonth = Lens.lens (\ScheduleRefreshOnEntity' {dayOfMonth} -> dayOfMonth) (\s@ScheduleRefreshOnEntity' {} a -> s {dayOfMonth = a} :: ScheduleRefreshOnEntity)

-- | The day of the week that you want to schedule a refresh on.
scheduleRefreshOnEntity_dayOfWeek :: Lens.Lens' ScheduleRefreshOnEntity (Prelude.Maybe DayOfWeek)
scheduleRefreshOnEntity_dayOfWeek = Lens.lens (\ScheduleRefreshOnEntity' {dayOfWeek} -> dayOfWeek) (\s@ScheduleRefreshOnEntity' {} a -> s {dayOfWeek = a} :: ScheduleRefreshOnEntity)

instance Data.FromJSON ScheduleRefreshOnEntity where
  parseJSON =
    Data.withObject
      "ScheduleRefreshOnEntity"
      ( \x ->
          ScheduleRefreshOnEntity'
            Prelude.<$> (x Data..:? "DayOfMonth")
            Prelude.<*> (x Data..:? "DayOfWeek")
      )

instance Prelude.Hashable ScheduleRefreshOnEntity where
  hashWithSalt _salt ScheduleRefreshOnEntity' {..} =
    _salt
      `Prelude.hashWithSalt` dayOfMonth
      `Prelude.hashWithSalt` dayOfWeek

instance Prelude.NFData ScheduleRefreshOnEntity where
  rnf ScheduleRefreshOnEntity' {..} =
    Prelude.rnf dayOfMonth
      `Prelude.seq` Prelude.rnf dayOfWeek

instance Data.ToJSON ScheduleRefreshOnEntity where
  toJSON ScheduleRefreshOnEntity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DayOfMonth" Data..=) Prelude.<$> dayOfMonth,
            ("DayOfWeek" Data..=) Prelude.<$> dayOfWeek
          ]
      )
