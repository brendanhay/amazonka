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
-- Module      : Amazonka.Glue.Types.TaskRunFilterCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.TaskRunFilterCriteria where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.TaskStatusType
import Amazonka.Glue.Types.TaskType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The criteria that are used to filter the task runs for the machine
-- learning transform.
--
-- /See:/ 'newTaskRunFilterCriteria' smart constructor.
data TaskRunFilterCriteria = TaskRunFilterCriteria'
  { -- | The current status of the task run.
    status :: Prelude.Maybe TaskStatusType,
    -- | Filter on task runs started after this date.
    startedAfter :: Prelude.Maybe Core.POSIX,
    -- | Filter on task runs started before this date.
    startedBefore :: Prelude.Maybe Core.POSIX,
    -- | The type of task run.
    taskRunType :: Prelude.Maybe TaskType
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
-- 'status', 'taskRunFilterCriteria_status' - The current status of the task run.
--
-- 'startedAfter', 'taskRunFilterCriteria_startedAfter' - Filter on task runs started after this date.
--
-- 'startedBefore', 'taskRunFilterCriteria_startedBefore' - Filter on task runs started before this date.
--
-- 'taskRunType', 'taskRunFilterCriteria_taskRunType' - The type of task run.
newTaskRunFilterCriteria ::
  TaskRunFilterCriteria
newTaskRunFilterCriteria =
  TaskRunFilterCriteria'
    { status = Prelude.Nothing,
      startedAfter = Prelude.Nothing,
      startedBefore = Prelude.Nothing,
      taskRunType = Prelude.Nothing
    }

-- | The current status of the task run.
taskRunFilterCriteria_status :: Lens.Lens' TaskRunFilterCriteria (Prelude.Maybe TaskStatusType)
taskRunFilterCriteria_status = Lens.lens (\TaskRunFilterCriteria' {status} -> status) (\s@TaskRunFilterCriteria' {} a -> s {status = a} :: TaskRunFilterCriteria)

-- | Filter on task runs started after this date.
taskRunFilterCriteria_startedAfter :: Lens.Lens' TaskRunFilterCriteria (Prelude.Maybe Prelude.UTCTime)
taskRunFilterCriteria_startedAfter = Lens.lens (\TaskRunFilterCriteria' {startedAfter} -> startedAfter) (\s@TaskRunFilterCriteria' {} a -> s {startedAfter = a} :: TaskRunFilterCriteria) Prelude.. Lens.mapping Core._Time

-- | Filter on task runs started before this date.
taskRunFilterCriteria_startedBefore :: Lens.Lens' TaskRunFilterCriteria (Prelude.Maybe Prelude.UTCTime)
taskRunFilterCriteria_startedBefore = Lens.lens (\TaskRunFilterCriteria' {startedBefore} -> startedBefore) (\s@TaskRunFilterCriteria' {} a -> s {startedBefore = a} :: TaskRunFilterCriteria) Prelude.. Lens.mapping Core._Time

-- | The type of task run.
taskRunFilterCriteria_taskRunType :: Lens.Lens' TaskRunFilterCriteria (Prelude.Maybe TaskType)
taskRunFilterCriteria_taskRunType = Lens.lens (\TaskRunFilterCriteria' {taskRunType} -> taskRunType) (\s@TaskRunFilterCriteria' {} a -> s {taskRunType = a} :: TaskRunFilterCriteria)

instance Prelude.Hashable TaskRunFilterCriteria where
  hashWithSalt salt' TaskRunFilterCriteria' {..} =
    salt' `Prelude.hashWithSalt` taskRunType
      `Prelude.hashWithSalt` startedBefore
      `Prelude.hashWithSalt` startedAfter
      `Prelude.hashWithSalt` status

instance Prelude.NFData TaskRunFilterCriteria where
  rnf TaskRunFilterCriteria' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf taskRunType
      `Prelude.seq` Prelude.rnf startedBefore
      `Prelude.seq` Prelude.rnf startedAfter

instance Core.ToJSON TaskRunFilterCriteria where
  toJSON TaskRunFilterCriteria' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("StartedAfter" Core..=) Prelude.<$> startedAfter,
            ("StartedBefore" Core..=) Prelude.<$> startedBefore,
            ("TaskRunType" Core..=) Prelude.<$> taskRunType
          ]
      )
