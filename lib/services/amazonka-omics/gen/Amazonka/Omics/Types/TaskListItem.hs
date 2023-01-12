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
-- Module      : Amazonka.Omics.Types.TaskListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.TaskListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.TaskStatus
import qualified Amazonka.Prelude as Prelude

-- | A workflow run task.
--
-- /See:/ 'newTaskListItem' smart constructor.
data TaskListItem = TaskListItem'
  { -- | The task\'s CPU count.
    cpus :: Prelude.Maybe Prelude.Natural,
    -- | When the task was created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The task\'s memory.
    memory :: Prelude.Maybe Prelude.Natural,
    -- | The task\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | When the task started.
    startTime :: Prelude.Maybe Data.ISO8601,
    -- | The task\'s status.
    status :: Prelude.Maybe TaskStatus,
    -- | When the task stopped.
    stopTime :: Prelude.Maybe Data.ISO8601,
    -- | The task\'s ID.
    taskId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpus', 'taskListItem_cpus' - The task\'s CPU count.
--
-- 'creationTime', 'taskListItem_creationTime' - When the task was created.
--
-- 'memory', 'taskListItem_memory' - The task\'s memory.
--
-- 'name', 'taskListItem_name' - The task\'s name.
--
-- 'startTime', 'taskListItem_startTime' - When the task started.
--
-- 'status', 'taskListItem_status' - The task\'s status.
--
-- 'stopTime', 'taskListItem_stopTime' - When the task stopped.
--
-- 'taskId', 'taskListItem_taskId' - The task\'s ID.
newTaskListItem ::
  TaskListItem
newTaskListItem =
  TaskListItem'
    { cpus = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      memory = Prelude.Nothing,
      name = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      stopTime = Prelude.Nothing,
      taskId = Prelude.Nothing
    }

-- | The task\'s CPU count.
taskListItem_cpus :: Lens.Lens' TaskListItem (Prelude.Maybe Prelude.Natural)
taskListItem_cpus = Lens.lens (\TaskListItem' {cpus} -> cpus) (\s@TaskListItem' {} a -> s {cpus = a} :: TaskListItem)

-- | When the task was created.
taskListItem_creationTime :: Lens.Lens' TaskListItem (Prelude.Maybe Prelude.UTCTime)
taskListItem_creationTime = Lens.lens (\TaskListItem' {creationTime} -> creationTime) (\s@TaskListItem' {} a -> s {creationTime = a} :: TaskListItem) Prelude.. Lens.mapping Data._Time

-- | The task\'s memory.
taskListItem_memory :: Lens.Lens' TaskListItem (Prelude.Maybe Prelude.Natural)
taskListItem_memory = Lens.lens (\TaskListItem' {memory} -> memory) (\s@TaskListItem' {} a -> s {memory = a} :: TaskListItem)

-- | The task\'s name.
taskListItem_name :: Lens.Lens' TaskListItem (Prelude.Maybe Prelude.Text)
taskListItem_name = Lens.lens (\TaskListItem' {name} -> name) (\s@TaskListItem' {} a -> s {name = a} :: TaskListItem)

-- | When the task started.
taskListItem_startTime :: Lens.Lens' TaskListItem (Prelude.Maybe Prelude.UTCTime)
taskListItem_startTime = Lens.lens (\TaskListItem' {startTime} -> startTime) (\s@TaskListItem' {} a -> s {startTime = a} :: TaskListItem) Prelude.. Lens.mapping Data._Time

-- | The task\'s status.
taskListItem_status :: Lens.Lens' TaskListItem (Prelude.Maybe TaskStatus)
taskListItem_status = Lens.lens (\TaskListItem' {status} -> status) (\s@TaskListItem' {} a -> s {status = a} :: TaskListItem)

-- | When the task stopped.
taskListItem_stopTime :: Lens.Lens' TaskListItem (Prelude.Maybe Prelude.UTCTime)
taskListItem_stopTime = Lens.lens (\TaskListItem' {stopTime} -> stopTime) (\s@TaskListItem' {} a -> s {stopTime = a} :: TaskListItem) Prelude.. Lens.mapping Data._Time

-- | The task\'s ID.
taskListItem_taskId :: Lens.Lens' TaskListItem (Prelude.Maybe Prelude.Text)
taskListItem_taskId = Lens.lens (\TaskListItem' {taskId} -> taskId) (\s@TaskListItem' {} a -> s {taskId = a} :: TaskListItem)

instance Data.FromJSON TaskListItem where
  parseJSON =
    Data.withObject
      "TaskListItem"
      ( \x ->
          TaskListItem'
            Prelude.<$> (x Data..:? "cpus")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "memory")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "stopTime")
            Prelude.<*> (x Data..:? "taskId")
      )

instance Prelude.Hashable TaskListItem where
  hashWithSalt _salt TaskListItem' {..} =
    _salt `Prelude.hashWithSalt` cpus
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` memory
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` stopTime
      `Prelude.hashWithSalt` taskId

instance Prelude.NFData TaskListItem where
  rnf TaskListItem' {..} =
    Prelude.rnf cpus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf memory
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf stopTime
      `Prelude.seq` Prelude.rnf taskId
