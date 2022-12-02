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
-- Module      : Amazonka.DataSync.Types.TaskSchedule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.TaskSchedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the schedule you want your task to use for repeated
-- executions. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html Schedule Expressions for Rules>.
--
-- /See:/ 'newTaskSchedule' smart constructor.
data TaskSchedule = TaskSchedule'
  { -- | A cron expression that specifies when DataSync initiates a scheduled
    -- transfer from a source to a destination location.
    scheduleExpression :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduleExpression', 'taskSchedule_scheduleExpression' - A cron expression that specifies when DataSync initiates a scheduled
-- transfer from a source to a destination location.
newTaskSchedule ::
  -- | 'scheduleExpression'
  Prelude.Text ->
  TaskSchedule
newTaskSchedule pScheduleExpression_ =
  TaskSchedule'
    { scheduleExpression =
        pScheduleExpression_
    }

-- | A cron expression that specifies when DataSync initiates a scheduled
-- transfer from a source to a destination location.
taskSchedule_scheduleExpression :: Lens.Lens' TaskSchedule Prelude.Text
taskSchedule_scheduleExpression = Lens.lens (\TaskSchedule' {scheduleExpression} -> scheduleExpression) (\s@TaskSchedule' {} a -> s {scheduleExpression = a} :: TaskSchedule)

instance Data.FromJSON TaskSchedule where
  parseJSON =
    Data.withObject
      "TaskSchedule"
      ( \x ->
          TaskSchedule'
            Prelude.<$> (x Data..: "ScheduleExpression")
      )

instance Prelude.Hashable TaskSchedule where
  hashWithSalt _salt TaskSchedule' {..} =
    _salt `Prelude.hashWithSalt` scheduleExpression

instance Prelude.NFData TaskSchedule where
  rnf TaskSchedule' {..} =
    Prelude.rnf scheduleExpression

instance Data.ToJSON TaskSchedule where
  toJSON TaskSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ScheduleExpression" Data..= scheduleExpression)
          ]
      )
