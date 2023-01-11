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
-- Module      : Amazonka.OpsWorks.Types.WeeklyAutoScalingSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.WeeklyAutoScalingSchedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a time-based instance\'s auto scaling schedule. The schedule
-- consists of a set of key-value pairs.
--
-- -   The key is the time period (a UTC hour) and must be an integer from
--     0 - 23.
--
-- -   The value indicates whether the instance should be online or offline
--     for the specified period, and must be set to \"on\" or \"off\"
--
-- The default setting for all time periods is off, so you use the
-- following parameters primarily to specify the online periods. You don\'t
-- have to explicitly specify offline periods unless you want to change an
-- online period to an offline period.
--
-- The following example specifies that the instance should be online for
-- four hours, from UTC 1200 - 1600. It will be off for the remainder of
-- the day.
--
-- @ { \"12\":\"on\", \"13\":\"on\", \"14\":\"on\", \"15\":\"on\" } @
--
-- /See:/ 'newWeeklyAutoScalingSchedule' smart constructor.
data WeeklyAutoScalingSchedule = WeeklyAutoScalingSchedule'
  { -- | The schedule for Friday.
    friday :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The schedule for Monday.
    monday :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The schedule for Saturday.
    saturday :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The schedule for Sunday.
    sunday :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The schedule for Thursday.
    thursday :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The schedule for Tuesday.
    tuesday :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The schedule for Wednesday.
    wednesday :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WeeklyAutoScalingSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'friday', 'weeklyAutoScalingSchedule_friday' - The schedule for Friday.
--
-- 'monday', 'weeklyAutoScalingSchedule_monday' - The schedule for Monday.
--
-- 'saturday', 'weeklyAutoScalingSchedule_saturday' - The schedule for Saturday.
--
-- 'sunday', 'weeklyAutoScalingSchedule_sunday' - The schedule for Sunday.
--
-- 'thursday', 'weeklyAutoScalingSchedule_thursday' - The schedule for Thursday.
--
-- 'tuesday', 'weeklyAutoScalingSchedule_tuesday' - The schedule for Tuesday.
--
-- 'wednesday', 'weeklyAutoScalingSchedule_wednesday' - The schedule for Wednesday.
newWeeklyAutoScalingSchedule ::
  WeeklyAutoScalingSchedule
newWeeklyAutoScalingSchedule =
  WeeklyAutoScalingSchedule'
    { friday =
        Prelude.Nothing,
      monday = Prelude.Nothing,
      saturday = Prelude.Nothing,
      sunday = Prelude.Nothing,
      thursday = Prelude.Nothing,
      tuesday = Prelude.Nothing,
      wednesday = Prelude.Nothing
    }

-- | The schedule for Friday.
weeklyAutoScalingSchedule_friday :: Lens.Lens' WeeklyAutoScalingSchedule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
weeklyAutoScalingSchedule_friday = Lens.lens (\WeeklyAutoScalingSchedule' {friday} -> friday) (\s@WeeklyAutoScalingSchedule' {} a -> s {friday = a} :: WeeklyAutoScalingSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The schedule for Monday.
weeklyAutoScalingSchedule_monday :: Lens.Lens' WeeklyAutoScalingSchedule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
weeklyAutoScalingSchedule_monday = Lens.lens (\WeeklyAutoScalingSchedule' {monday} -> monday) (\s@WeeklyAutoScalingSchedule' {} a -> s {monday = a} :: WeeklyAutoScalingSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The schedule for Saturday.
weeklyAutoScalingSchedule_saturday :: Lens.Lens' WeeklyAutoScalingSchedule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
weeklyAutoScalingSchedule_saturday = Lens.lens (\WeeklyAutoScalingSchedule' {saturday} -> saturday) (\s@WeeklyAutoScalingSchedule' {} a -> s {saturday = a} :: WeeklyAutoScalingSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The schedule for Sunday.
weeklyAutoScalingSchedule_sunday :: Lens.Lens' WeeklyAutoScalingSchedule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
weeklyAutoScalingSchedule_sunday = Lens.lens (\WeeklyAutoScalingSchedule' {sunday} -> sunday) (\s@WeeklyAutoScalingSchedule' {} a -> s {sunday = a} :: WeeklyAutoScalingSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The schedule for Thursday.
weeklyAutoScalingSchedule_thursday :: Lens.Lens' WeeklyAutoScalingSchedule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
weeklyAutoScalingSchedule_thursday = Lens.lens (\WeeklyAutoScalingSchedule' {thursday} -> thursday) (\s@WeeklyAutoScalingSchedule' {} a -> s {thursday = a} :: WeeklyAutoScalingSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The schedule for Tuesday.
weeklyAutoScalingSchedule_tuesday :: Lens.Lens' WeeklyAutoScalingSchedule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
weeklyAutoScalingSchedule_tuesday = Lens.lens (\WeeklyAutoScalingSchedule' {tuesday} -> tuesday) (\s@WeeklyAutoScalingSchedule' {} a -> s {tuesday = a} :: WeeklyAutoScalingSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The schedule for Wednesday.
weeklyAutoScalingSchedule_wednesday :: Lens.Lens' WeeklyAutoScalingSchedule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
weeklyAutoScalingSchedule_wednesday = Lens.lens (\WeeklyAutoScalingSchedule' {wednesday} -> wednesday) (\s@WeeklyAutoScalingSchedule' {} a -> s {wednesday = a} :: WeeklyAutoScalingSchedule) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON WeeklyAutoScalingSchedule where
  parseJSON =
    Data.withObject
      "WeeklyAutoScalingSchedule"
      ( \x ->
          WeeklyAutoScalingSchedule'
            Prelude.<$> (x Data..:? "Friday" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Monday" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Saturday" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Sunday" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Thursday" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Tuesday" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Wednesday" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable WeeklyAutoScalingSchedule where
  hashWithSalt _salt WeeklyAutoScalingSchedule' {..} =
    _salt `Prelude.hashWithSalt` friday
      `Prelude.hashWithSalt` monday
      `Prelude.hashWithSalt` saturday
      `Prelude.hashWithSalt` sunday
      `Prelude.hashWithSalt` thursday
      `Prelude.hashWithSalt` tuesday
      `Prelude.hashWithSalt` wednesday

instance Prelude.NFData WeeklyAutoScalingSchedule where
  rnf WeeklyAutoScalingSchedule' {..} =
    Prelude.rnf friday
      `Prelude.seq` Prelude.rnf monday
      `Prelude.seq` Prelude.rnf saturday
      `Prelude.seq` Prelude.rnf sunday
      `Prelude.seq` Prelude.rnf thursday
      `Prelude.seq` Prelude.rnf tuesday
      `Prelude.seq` Prelude.rnf wednesday

instance Data.ToJSON WeeklyAutoScalingSchedule where
  toJSON WeeklyAutoScalingSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Friday" Data..=) Prelude.<$> friday,
            ("Monday" Data..=) Prelude.<$> monday,
            ("Saturday" Data..=) Prelude.<$> saturday,
            ("Sunday" Data..=) Prelude.<$> sunday,
            ("Thursday" Data..=) Prelude.<$> thursday,
            ("Tuesday" Data..=) Prelude.<$> tuesday,
            ("Wednesday" Data..=) Prelude.<$> wednesday
          ]
      )
