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
-- Module      : Network.AWS.Glue.Types.TaskRunFilterCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TaskRunFilterCriteria where

import Network.AWS.Glue.Types.TaskStatusType
import Network.AWS.Glue.Types.TaskType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The criteria that are used to filter the task runs for the machine
-- learning transform.
--
-- /See:/ 'newTaskRunFilterCriteria' smart constructor.
data TaskRunFilterCriteria = TaskRunFilterCriteria'
  { -- | The current status of the task run.
    status :: Prelude.Maybe TaskStatusType,
    -- | The type of task run.
    taskRunType :: Prelude.Maybe TaskType,
    -- | Filter on task runs started before this date.
    startedBefore :: Prelude.Maybe Prelude.POSIX,
    -- | Filter on task runs started after this date.
    startedAfter :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { status = Prelude.Nothing,
      taskRunType = Prelude.Nothing,
      startedBefore = Prelude.Nothing,
      startedAfter = Prelude.Nothing
    }

-- | The current status of the task run.
taskRunFilterCriteria_status :: Lens.Lens' TaskRunFilterCriteria (Prelude.Maybe TaskStatusType)
taskRunFilterCriteria_status = Lens.lens (\TaskRunFilterCriteria' {status} -> status) (\s@TaskRunFilterCriteria' {} a -> s {status = a} :: TaskRunFilterCriteria)

-- | The type of task run.
taskRunFilterCriteria_taskRunType :: Lens.Lens' TaskRunFilterCriteria (Prelude.Maybe TaskType)
taskRunFilterCriteria_taskRunType = Lens.lens (\TaskRunFilterCriteria' {taskRunType} -> taskRunType) (\s@TaskRunFilterCriteria' {} a -> s {taskRunType = a} :: TaskRunFilterCriteria)

-- | Filter on task runs started before this date.
taskRunFilterCriteria_startedBefore :: Lens.Lens' TaskRunFilterCriteria (Prelude.Maybe Prelude.UTCTime)
taskRunFilterCriteria_startedBefore = Lens.lens (\TaskRunFilterCriteria' {startedBefore} -> startedBefore) (\s@TaskRunFilterCriteria' {} a -> s {startedBefore = a} :: TaskRunFilterCriteria) Prelude.. Lens.mapping Prelude._Time

-- | Filter on task runs started after this date.
taskRunFilterCriteria_startedAfter :: Lens.Lens' TaskRunFilterCriteria (Prelude.Maybe Prelude.UTCTime)
taskRunFilterCriteria_startedAfter = Lens.lens (\TaskRunFilterCriteria' {startedAfter} -> startedAfter) (\s@TaskRunFilterCriteria' {} a -> s {startedAfter = a} :: TaskRunFilterCriteria) Prelude.. Lens.mapping Prelude._Time

instance Prelude.Hashable TaskRunFilterCriteria

instance Prelude.NFData TaskRunFilterCriteria

instance Prelude.ToJSON TaskRunFilterCriteria where
  toJSON TaskRunFilterCriteria' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Status" Prelude..=) Prelude.<$> status,
            ("TaskRunType" Prelude..=) Prelude.<$> taskRunType,
            ("StartedBefore" Prelude..=)
              Prelude.<$> startedBefore,
            ("StartedAfter" Prelude..=)
              Prelude.<$> startedAfter
          ]
      )
