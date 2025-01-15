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
-- Module      : Amazonka.DataSync.Types.TaskExecutionListEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.TaskExecutionListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types.TaskExecutionStatus
import qualified Amazonka.Prelude as Prelude

-- | Represents a single entry in a list of task executions.
-- @TaskExecutionListEntry@ returns an array that contains a list of
-- specific invocations of a task when the
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_ListTaskExecutions.html ListTaskExecutions>
-- operation is called.
--
-- /See:/ 'newTaskExecutionListEntry' smart constructor.
data TaskExecutionListEntry = TaskExecutionListEntry'
  { -- | The status of a task execution.
    status :: Prelude.Maybe TaskExecutionStatus,
    -- | The Amazon Resource Name (ARN) of the task that was executed.
    taskExecutionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskExecutionListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'taskExecutionListEntry_status' - The status of a task execution.
--
-- 'taskExecutionArn', 'taskExecutionListEntry_taskExecutionArn' - The Amazon Resource Name (ARN) of the task that was executed.
newTaskExecutionListEntry ::
  TaskExecutionListEntry
newTaskExecutionListEntry =
  TaskExecutionListEntry'
    { status = Prelude.Nothing,
      taskExecutionArn = Prelude.Nothing
    }

-- | The status of a task execution.
taskExecutionListEntry_status :: Lens.Lens' TaskExecutionListEntry (Prelude.Maybe TaskExecutionStatus)
taskExecutionListEntry_status = Lens.lens (\TaskExecutionListEntry' {status} -> status) (\s@TaskExecutionListEntry' {} a -> s {status = a} :: TaskExecutionListEntry)

-- | The Amazon Resource Name (ARN) of the task that was executed.
taskExecutionListEntry_taskExecutionArn :: Lens.Lens' TaskExecutionListEntry (Prelude.Maybe Prelude.Text)
taskExecutionListEntry_taskExecutionArn = Lens.lens (\TaskExecutionListEntry' {taskExecutionArn} -> taskExecutionArn) (\s@TaskExecutionListEntry' {} a -> s {taskExecutionArn = a} :: TaskExecutionListEntry)

instance Data.FromJSON TaskExecutionListEntry where
  parseJSON =
    Data.withObject
      "TaskExecutionListEntry"
      ( \x ->
          TaskExecutionListEntry'
            Prelude.<$> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "TaskExecutionArn")
      )

instance Prelude.Hashable TaskExecutionListEntry where
  hashWithSalt _salt TaskExecutionListEntry' {..} =
    _salt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` taskExecutionArn

instance Prelude.NFData TaskExecutionListEntry where
  rnf TaskExecutionListEntry' {..} =
    Prelude.rnf status `Prelude.seq`
      Prelude.rnf taskExecutionArn
