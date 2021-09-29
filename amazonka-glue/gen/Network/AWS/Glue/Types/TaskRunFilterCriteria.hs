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
import qualified Network.AWS.Prelude as Prelude

-- | The criteria that are used to filter the task runs for the machine
-- learning transform.
--
-- /See:/ 'newTaskRunFilterCriteria' smart constructor.
data TaskRunFilterCriteria = TaskRunFilterCriteria'
  { -- | The type of task run.
    taskRunType :: Prelude.Maybe TaskType,
    -- | The current status of the task run.
    status :: Prelude.Maybe TaskStatusType,
    -- | Filter on task runs started before this date.
    startedBefore :: Prelude.Maybe Core.POSIX,
    -- | Filter on task runs started after this date.
    startedAfter :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskRunFilterCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskRunType', 'taskRunFilterCriteria_taskRunType' - The type of task run.
--
-- 'status', 'taskRunFilterCriteria_status' - The current status of the task run.
--
-- 'startedBefore', 'taskRunFilterCriteria_startedBefore' - Filter on task runs started before this date.
--
-- 'startedAfter', 'taskRunFilterCriteria_startedAfter' - Filter on task runs started after this date.
newTaskRunFilterCriteria ::
  TaskRunFilterCriteria
newTaskRunFilterCriteria =
  TaskRunFilterCriteria'
    { taskRunType =
        Prelude.Nothing,
      status = Prelude.Nothing,
      startedBefore = Prelude.Nothing,
      startedAfter = Prelude.Nothing
    }

-- | The type of task run.
taskRunFilterCriteria_taskRunType :: Lens.Lens' TaskRunFilterCriteria (Prelude.Maybe TaskType)
taskRunFilterCriteria_taskRunType = Lens.lens (\TaskRunFilterCriteria' {taskRunType} -> taskRunType) (\s@TaskRunFilterCriteria' {} a -> s {taskRunType = a} :: TaskRunFilterCriteria)

-- | The current status of the task run.
taskRunFilterCriteria_status :: Lens.Lens' TaskRunFilterCriteria (Prelude.Maybe TaskStatusType)
taskRunFilterCriteria_status = Lens.lens (\TaskRunFilterCriteria' {status} -> status) (\s@TaskRunFilterCriteria' {} a -> s {status = a} :: TaskRunFilterCriteria)

-- | Filter on task runs started before this date.
taskRunFilterCriteria_startedBefore :: Lens.Lens' TaskRunFilterCriteria (Prelude.Maybe Prelude.UTCTime)
taskRunFilterCriteria_startedBefore = Lens.lens (\TaskRunFilterCriteria' {startedBefore} -> startedBefore) (\s@TaskRunFilterCriteria' {} a -> s {startedBefore = a} :: TaskRunFilterCriteria) Prelude.. Lens.mapping Core._Time

-- | Filter on task runs started after this date.
taskRunFilterCriteria_startedAfter :: Lens.Lens' TaskRunFilterCriteria (Prelude.Maybe Prelude.UTCTime)
taskRunFilterCriteria_startedAfter = Lens.lens (\TaskRunFilterCriteria' {startedAfter} -> startedAfter) (\s@TaskRunFilterCriteria' {} a -> s {startedAfter = a} :: TaskRunFilterCriteria) Prelude.. Lens.mapping Core._Time

instance Prelude.Hashable TaskRunFilterCriteria

instance Prelude.NFData TaskRunFilterCriteria

instance Core.ToJSON TaskRunFilterCriteria where
  toJSON TaskRunFilterCriteria' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TaskRunType" Core..=) Prelude.<$> taskRunType,
            ("Status" Core..=) Prelude.<$> status,
            ("StartedBefore" Core..=) Prelude.<$> startedBefore,
            ("StartedAfter" Core..=) Prelude.<$> startedAfter
          ]
      )
