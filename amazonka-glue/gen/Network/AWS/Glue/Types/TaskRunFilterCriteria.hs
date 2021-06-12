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
-- Module      : Network.AWS.Glue.Types.TaskRunFilterCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TaskRunFilterCriteria where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.TaskStatusType
import Network.AWS.Glue.Types.TaskType
import qualified Network.AWS.Lens as Lens

-- | The criteria that are used to filter the task runs for the machine
-- learning transform.
--
-- /See:/ 'newTaskRunFilterCriteria' smart constructor.
data TaskRunFilterCriteria = TaskRunFilterCriteria'
  { -- | The current status of the task run.
    status :: Core.Maybe TaskStatusType,
    -- | The type of task run.
    taskRunType :: Core.Maybe TaskType,
    -- | Filter on task runs started before this date.
    startedBefore :: Core.Maybe Core.POSIX,
    -- | Filter on task runs started after this date.
    startedAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TaskRunFilterCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'taskRunFilterCriteria_status' - The current status of the task run.
--
-- 'taskRunType', 'taskRunFilterCriteria_taskRunType' - The type of task run.
--
-- 'startedBefore', 'taskRunFilterCriteria_startedBefore' - Filter on task runs started before this date.
--
-- 'startedAfter', 'taskRunFilterCriteria_startedAfter' - Filter on task runs started after this date.
newTaskRunFilterCriteria ::
  TaskRunFilterCriteria
newTaskRunFilterCriteria =
  TaskRunFilterCriteria'
    { status = Core.Nothing,
      taskRunType = Core.Nothing,
      startedBefore = Core.Nothing,
      startedAfter = Core.Nothing
    }

-- | The current status of the task run.
taskRunFilterCriteria_status :: Lens.Lens' TaskRunFilterCriteria (Core.Maybe TaskStatusType)
taskRunFilterCriteria_status = Lens.lens (\TaskRunFilterCriteria' {status} -> status) (\s@TaskRunFilterCriteria' {} a -> s {status = a} :: TaskRunFilterCriteria)

-- | The type of task run.
taskRunFilterCriteria_taskRunType :: Lens.Lens' TaskRunFilterCriteria (Core.Maybe TaskType)
taskRunFilterCriteria_taskRunType = Lens.lens (\TaskRunFilterCriteria' {taskRunType} -> taskRunType) (\s@TaskRunFilterCriteria' {} a -> s {taskRunType = a} :: TaskRunFilterCriteria)

-- | Filter on task runs started before this date.
taskRunFilterCriteria_startedBefore :: Lens.Lens' TaskRunFilterCriteria (Core.Maybe Core.UTCTime)
taskRunFilterCriteria_startedBefore = Lens.lens (\TaskRunFilterCriteria' {startedBefore} -> startedBefore) (\s@TaskRunFilterCriteria' {} a -> s {startedBefore = a} :: TaskRunFilterCriteria) Core.. Lens.mapping Core._Time

-- | Filter on task runs started after this date.
taskRunFilterCriteria_startedAfter :: Lens.Lens' TaskRunFilterCriteria (Core.Maybe Core.UTCTime)
taskRunFilterCriteria_startedAfter = Lens.lens (\TaskRunFilterCriteria' {startedAfter} -> startedAfter) (\s@TaskRunFilterCriteria' {} a -> s {startedAfter = a} :: TaskRunFilterCriteria) Core.. Lens.mapping Core._Time

instance Core.Hashable TaskRunFilterCriteria

instance Core.NFData TaskRunFilterCriteria

instance Core.ToJSON TaskRunFilterCriteria where
  toJSON TaskRunFilterCriteria' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Status" Core..=) Core.<$> status,
            ("TaskRunType" Core..=) Core.<$> taskRunType,
            ("StartedBefore" Core..=) Core.<$> startedBefore,
            ("StartedAfter" Core..=) Core.<$> startedAfter
          ]
      )
