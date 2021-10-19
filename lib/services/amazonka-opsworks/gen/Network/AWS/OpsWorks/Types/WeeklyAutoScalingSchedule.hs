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
-- Module      : Network.AWS.OpsWorks.Types.WeeklyAutoScalingSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.WeeklyAutoScalingSchedule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  { -- | The schedule for Thursday.
    thursday :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The schedule for Wednesday.
    wednesday :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The schedule for Saturday.
    saturday :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The schedule for Monday.
    monday :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The schedule for Friday.
    friday :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The schedule for Sunday.
    sunday :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The schedule for Tuesday.
    tuesday :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'thursday', 'weeklyAutoScalingSchedule_thursday' - The schedule for Thursday.
--
-- 'wednesday', 'weeklyAutoScalingSchedule_wednesday' - The schedule for Wednesday.
--
-- 'saturday', 'weeklyAutoScalingSchedule_saturday' - The schedule for Saturday.
--
-- 'monday', 'weeklyAutoScalingSchedule_monday' - The schedule for Monday.
--
-- 'friday', 'weeklyAutoScalingSchedule_friday' - The schedule for Friday.
--
-- 'sunday', 'weeklyAutoScalingSchedule_sunday' - The schedule for Sunday.
--
-- 'tuesday', 'weeklyAutoScalingSchedule_tuesday' - The schedule for Tuesday.
newWeeklyAutoScalingSchedule ::
  WeeklyAutoScalingSchedule
newWeeklyAutoScalingSchedule =
  WeeklyAutoScalingSchedule'
    { thursday =
        Prelude.Nothing,
      wednesday = Prelude.Nothing,
      saturday = Prelude.Nothing,
      monday = Prelude.Nothing,
      friday = Prelude.Nothing,
      sunday = Prelude.Nothing,
      tuesday = Prelude.Nothing
    }

-- | The schedule for Thursday.
weeklyAutoScalingSchedule_thursday :: Lens.Lens' WeeklyAutoScalingSchedule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
weeklyAutoScalingSchedule_thursday = Lens.lens (\WeeklyAutoScalingSchedule' {thursday} -> thursday) (\s@WeeklyAutoScalingSchedule' {} a -> s {thursday = a} :: WeeklyAutoScalingSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The schedule for Wednesday.
weeklyAutoScalingSchedule_wednesday :: Lens.Lens' WeeklyAutoScalingSchedule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
weeklyAutoScalingSchedule_wednesday = Lens.lens (\WeeklyAutoScalingSchedule' {wednesday} -> wednesday) (\s@WeeklyAutoScalingSchedule' {} a -> s {wednesday = a} :: WeeklyAutoScalingSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The schedule for Saturday.
weeklyAutoScalingSchedule_saturday :: Lens.Lens' WeeklyAutoScalingSchedule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
weeklyAutoScalingSchedule_saturday = Lens.lens (\WeeklyAutoScalingSchedule' {saturday} -> saturday) (\s@WeeklyAutoScalingSchedule' {} a -> s {saturday = a} :: WeeklyAutoScalingSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The schedule for Monday.
weeklyAutoScalingSchedule_monday :: Lens.Lens' WeeklyAutoScalingSchedule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
weeklyAutoScalingSchedule_monday = Lens.lens (\WeeklyAutoScalingSchedule' {monday} -> monday) (\s@WeeklyAutoScalingSchedule' {} a -> s {monday = a} :: WeeklyAutoScalingSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The schedule for Friday.
weeklyAutoScalingSchedule_friday :: Lens.Lens' WeeklyAutoScalingSchedule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
weeklyAutoScalingSchedule_friday = Lens.lens (\WeeklyAutoScalingSchedule' {friday} -> friday) (\s@WeeklyAutoScalingSchedule' {} a -> s {friday = a} :: WeeklyAutoScalingSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The schedule for Sunday.
weeklyAutoScalingSchedule_sunday :: Lens.Lens' WeeklyAutoScalingSchedule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
weeklyAutoScalingSchedule_sunday = Lens.lens (\WeeklyAutoScalingSchedule' {sunday} -> sunday) (\s@WeeklyAutoScalingSchedule' {} a -> s {sunday = a} :: WeeklyAutoScalingSchedule) Prelude.. Lens.mapping Lens.coerced

-- | The schedule for Tuesday.
weeklyAutoScalingSchedule_tuesday :: Lens.Lens' WeeklyAutoScalingSchedule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
weeklyAutoScalingSchedule_tuesday = Lens.lens (\WeeklyAutoScalingSchedule' {tuesday} -> tuesday) (\s@WeeklyAutoScalingSchedule' {} a -> s {tuesday = a} :: WeeklyAutoScalingSchedule) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON WeeklyAutoScalingSchedule where
  parseJSON =
    Core.withObject
      "WeeklyAutoScalingSchedule"
      ( \x ->
          WeeklyAutoScalingSchedule'
            Prelude.<$> (x Core..:? "Thursday" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Wednesday" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Saturday" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Monday" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Friday" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Sunday" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Tuesday" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable WeeklyAutoScalingSchedule

instance Prelude.NFData WeeklyAutoScalingSchedule

instance Core.ToJSON WeeklyAutoScalingSchedule where
  toJSON WeeklyAutoScalingSchedule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Thursday" Core..=) Prelude.<$> thursday,
            ("Wednesday" Core..=) Prelude.<$> wednesday,
            ("Saturday" Core..=) Prelude.<$> saturday,
            ("Monday" Core..=) Prelude.<$> monday,
            ("Friday" Core..=) Prelude.<$> friday,
            ("Sunday" Core..=) Prelude.<$> sunday,
            ("Tuesday" Core..=) Prelude.<$> tuesday
          ]
      )
