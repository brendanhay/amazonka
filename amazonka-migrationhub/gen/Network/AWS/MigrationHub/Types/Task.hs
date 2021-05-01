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
-- Module      : Network.AWS.MigrationHub.Types.Task
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.Task where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types.MigrationStatus
import qualified Network.AWS.Prelude as Prelude

-- | Task object encapsulating task information.
--
-- /See:/ 'newTask' smart constructor.
data Task = Task'
  { -- | Indication of the percentage completion of the task.
    progressPercent :: Prelude.Maybe Prelude.Natural,
    -- | Details of task status as notified by a migration tool. A tool might use
    -- this field to provide clarifying information about the status that is
    -- unique to that tool or that explains an error state.
    statusDetail :: Prelude.Maybe Prelude.Text,
    -- | Status of the task - Not Started, In-Progress, Complete.
    status :: MigrationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Task' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'progressPercent', 'task_progressPercent' - Indication of the percentage completion of the task.
--
-- 'statusDetail', 'task_statusDetail' - Details of task status as notified by a migration tool. A tool might use
-- this field to provide clarifying information about the status that is
-- unique to that tool or that explains an error state.
--
-- 'status', 'task_status' - Status of the task - Not Started, In-Progress, Complete.
newTask ::
  -- | 'status'
  MigrationStatus ->
  Task
newTask pStatus_ =
  Task'
    { progressPercent = Prelude.Nothing,
      statusDetail = Prelude.Nothing,
      status = pStatus_
    }

-- | Indication of the percentage completion of the task.
task_progressPercent :: Lens.Lens' Task (Prelude.Maybe Prelude.Natural)
task_progressPercent = Lens.lens (\Task' {progressPercent} -> progressPercent) (\s@Task' {} a -> s {progressPercent = a} :: Task)

-- | Details of task status as notified by a migration tool. A tool might use
-- this field to provide clarifying information about the status that is
-- unique to that tool or that explains an error state.
task_statusDetail :: Lens.Lens' Task (Prelude.Maybe Prelude.Text)
task_statusDetail = Lens.lens (\Task' {statusDetail} -> statusDetail) (\s@Task' {} a -> s {statusDetail = a} :: Task)

-- | Status of the task - Not Started, In-Progress, Complete.
task_status :: Lens.Lens' Task MigrationStatus
task_status = Lens.lens (\Task' {status} -> status) (\s@Task' {} a -> s {status = a} :: Task)

instance Prelude.FromJSON Task where
  parseJSON =
    Prelude.withObject
      "Task"
      ( \x ->
          Task'
            Prelude.<$> (x Prelude..:? "ProgressPercent")
            Prelude.<*> (x Prelude..:? "StatusDetail")
            Prelude.<*> (x Prelude..: "Status")
      )

instance Prelude.Hashable Task

instance Prelude.NFData Task

instance Prelude.ToJSON Task where
  toJSON Task' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ProgressPercent" Prelude..=)
              Prelude.<$> progressPercent,
            ("StatusDetail" Prelude..=) Prelude.<$> statusDetail,
            Prelude.Just ("Status" Prelude..= status)
          ]
      )
