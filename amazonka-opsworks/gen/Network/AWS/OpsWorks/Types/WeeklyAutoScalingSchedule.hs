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
    thursday :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The schedule for Friday.
    friday :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The schedule for Tuesday.
    tuesday :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The schedule for Monday.
    monday :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The schedule for Sunday.
    sunday :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The schedule for Saturday.
    saturday :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The schedule for Wednesday.
    wednesday :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'friday', 'weeklyAutoScalingSchedule_friday' - The schedule for Friday.
--
-- 'tuesday', 'weeklyAutoScalingSchedule_tuesday' - The schedule for Tuesday.
--
-- 'monday', 'weeklyAutoScalingSchedule_monday' - The schedule for Monday.
--
-- 'sunday', 'weeklyAutoScalingSchedule_sunday' - The schedule for Sunday.
--
-- 'saturday', 'weeklyAutoScalingSchedule_saturday' - The schedule for Saturday.
--
-- 'wednesday', 'weeklyAutoScalingSchedule_wednesday' - The schedule for Wednesday.
newWeeklyAutoScalingSchedule ::
  WeeklyAutoScalingSchedule
newWeeklyAutoScalingSchedule =
  WeeklyAutoScalingSchedule'
    { thursday = Core.Nothing,
      friday = Core.Nothing,
      tuesday = Core.Nothing,
      monday = Core.Nothing,
      sunday = Core.Nothing,
      saturday = Core.Nothing,
      wednesday = Core.Nothing
    }

-- | The schedule for Thursday.
weeklyAutoScalingSchedule_thursday :: Lens.Lens' WeeklyAutoScalingSchedule (Core.Maybe (Core.HashMap Core.Text Core.Text))
weeklyAutoScalingSchedule_thursday = Lens.lens (\WeeklyAutoScalingSchedule' {thursday} -> thursday) (\s@WeeklyAutoScalingSchedule' {} a -> s {thursday = a} :: WeeklyAutoScalingSchedule) Core.. Lens.mapping Lens._Coerce

-- | The schedule for Friday.
weeklyAutoScalingSchedule_friday :: Lens.Lens' WeeklyAutoScalingSchedule (Core.Maybe (Core.HashMap Core.Text Core.Text))
weeklyAutoScalingSchedule_friday = Lens.lens (\WeeklyAutoScalingSchedule' {friday} -> friday) (\s@WeeklyAutoScalingSchedule' {} a -> s {friday = a} :: WeeklyAutoScalingSchedule) Core.. Lens.mapping Lens._Coerce

-- | The schedule for Tuesday.
weeklyAutoScalingSchedule_tuesday :: Lens.Lens' WeeklyAutoScalingSchedule (Core.Maybe (Core.HashMap Core.Text Core.Text))
weeklyAutoScalingSchedule_tuesday = Lens.lens (\WeeklyAutoScalingSchedule' {tuesday} -> tuesday) (\s@WeeklyAutoScalingSchedule' {} a -> s {tuesday = a} :: WeeklyAutoScalingSchedule) Core.. Lens.mapping Lens._Coerce

-- | The schedule for Monday.
weeklyAutoScalingSchedule_monday :: Lens.Lens' WeeklyAutoScalingSchedule (Core.Maybe (Core.HashMap Core.Text Core.Text))
weeklyAutoScalingSchedule_monday = Lens.lens (\WeeklyAutoScalingSchedule' {monday} -> monday) (\s@WeeklyAutoScalingSchedule' {} a -> s {monday = a} :: WeeklyAutoScalingSchedule) Core.. Lens.mapping Lens._Coerce

-- | The schedule for Sunday.
weeklyAutoScalingSchedule_sunday :: Lens.Lens' WeeklyAutoScalingSchedule (Core.Maybe (Core.HashMap Core.Text Core.Text))
weeklyAutoScalingSchedule_sunday = Lens.lens (\WeeklyAutoScalingSchedule' {sunday} -> sunday) (\s@WeeklyAutoScalingSchedule' {} a -> s {sunday = a} :: WeeklyAutoScalingSchedule) Core.. Lens.mapping Lens._Coerce

-- | The schedule for Saturday.
weeklyAutoScalingSchedule_saturday :: Lens.Lens' WeeklyAutoScalingSchedule (Core.Maybe (Core.HashMap Core.Text Core.Text))
weeklyAutoScalingSchedule_saturday = Lens.lens (\WeeklyAutoScalingSchedule' {saturday} -> saturday) (\s@WeeklyAutoScalingSchedule' {} a -> s {saturday = a} :: WeeklyAutoScalingSchedule) Core.. Lens.mapping Lens._Coerce

-- | The schedule for Wednesday.
weeklyAutoScalingSchedule_wednesday :: Lens.Lens' WeeklyAutoScalingSchedule (Core.Maybe (Core.HashMap Core.Text Core.Text))
weeklyAutoScalingSchedule_wednesday = Lens.lens (\WeeklyAutoScalingSchedule' {wednesday} -> wednesday) (\s@WeeklyAutoScalingSchedule' {} a -> s {wednesday = a} :: WeeklyAutoScalingSchedule) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON WeeklyAutoScalingSchedule where
  parseJSON =
    Core.withObject
      "WeeklyAutoScalingSchedule"
      ( \x ->
          WeeklyAutoScalingSchedule'
            Core.<$> (x Core..:? "Thursday" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Friday" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Tuesday" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Monday" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Sunday" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Saturday" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Wednesday" Core..!= Core.mempty)
      )

instance Core.Hashable WeeklyAutoScalingSchedule

instance Core.NFData WeeklyAutoScalingSchedule

instance Core.ToJSON WeeklyAutoScalingSchedule where
  toJSON WeeklyAutoScalingSchedule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Thursday" Core..=) Core.<$> thursday,
            ("Friday" Core..=) Core.<$> friday,
            ("Tuesday" Core..=) Core.<$> tuesday,
            ("Monday" Core..=) Core.<$> monday,
            ("Sunday" Core..=) Core.<$> sunday,
            ("Saturday" Core..=) Core.<$> saturday,
            ("Wednesday" Core..=) Core.<$> wednesday
          ]
      )
