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
-- Module      : Network.AWS.Glue.Types.Schedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Schedule where

import Network.AWS.Glue.Types.ScheduleState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A scheduling object using a @cron@ statement to schedule an event.
--
-- /See:/ 'newSchedule' smart constructor.
data Schedule = Schedule'
  { -- | The state of the schedule.
    state :: Prelude.Maybe ScheduleState,
    -- | A @cron@ expression used to specify the schedule (see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
    -- For example, to run something every day at 12:15 UTC, you would specify:
    -- @cron(15 12 * * ? *)@.
    scheduleExpression :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Schedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'schedule_state' - The state of the schedule.
--
-- 'scheduleExpression', 'schedule_scheduleExpression' - A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
newSchedule ::
  Schedule
newSchedule =
  Schedule'
    { state = Prelude.Nothing,
      scheduleExpression = Prelude.Nothing
    }

-- | The state of the schedule.
schedule_state :: Lens.Lens' Schedule (Prelude.Maybe ScheduleState)
schedule_state = Lens.lens (\Schedule' {state} -> state) (\s@Schedule' {} a -> s {state = a} :: Schedule)

-- | A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
schedule_scheduleExpression :: Lens.Lens' Schedule (Prelude.Maybe Prelude.Text)
schedule_scheduleExpression = Lens.lens (\Schedule' {scheduleExpression} -> scheduleExpression) (\s@Schedule' {} a -> s {scheduleExpression = a} :: Schedule)

instance Prelude.FromJSON Schedule where
  parseJSON =
    Prelude.withObject
      "Schedule"
      ( \x ->
          Schedule'
            Prelude.<$> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "ScheduleExpression")
      )

instance Prelude.Hashable Schedule

instance Prelude.NFData Schedule
