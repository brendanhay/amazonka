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
-- Module      : Amazonka.SnowDeviceManagement.Types.TaskSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SnowDeviceManagement.Types.TaskSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SnowDeviceManagement.Types.TaskState

-- | Information about the task assigned to one or many devices.
--
-- /See:/ 'newTaskSummary' smart constructor.
data TaskSummary = TaskSummary'
  { -- | Optional metadata that you assign to a resource. You can use tags to
    -- categorize a resource in different ways, such as by purpose, owner, or
    -- environment.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the task.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the task assigned to one or many devices.
    state :: Prelude.Maybe TaskState,
    -- | The task ID.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'taskSummary_tags' - Optional metadata that you assign to a resource. You can use tags to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment.
--
-- 'taskArn', 'taskSummary_taskArn' - The Amazon Resource Name (ARN) of the task.
--
-- 'state', 'taskSummary_state' - The state of the task assigned to one or many devices.
--
-- 'taskId', 'taskSummary_taskId' - The task ID.
newTaskSummary ::
  -- | 'taskId'
  Prelude.Text ->
  TaskSummary
newTaskSummary pTaskId_ =
  TaskSummary'
    { tags = Prelude.Nothing,
      taskArn = Prelude.Nothing,
      state = Prelude.Nothing,
      taskId = pTaskId_
    }

-- | Optional metadata that you assign to a resource. You can use tags to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment.
taskSummary_tags :: Lens.Lens' TaskSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
taskSummary_tags = Lens.lens (\TaskSummary' {tags} -> tags) (\s@TaskSummary' {} a -> s {tags = a} :: TaskSummary) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the task.
taskSummary_taskArn :: Lens.Lens' TaskSummary (Prelude.Maybe Prelude.Text)
taskSummary_taskArn = Lens.lens (\TaskSummary' {taskArn} -> taskArn) (\s@TaskSummary' {} a -> s {taskArn = a} :: TaskSummary)

-- | The state of the task assigned to one or many devices.
taskSummary_state :: Lens.Lens' TaskSummary (Prelude.Maybe TaskState)
taskSummary_state = Lens.lens (\TaskSummary' {state} -> state) (\s@TaskSummary' {} a -> s {state = a} :: TaskSummary)

-- | The task ID.
taskSummary_taskId :: Lens.Lens' TaskSummary Prelude.Text
taskSummary_taskId = Lens.lens (\TaskSummary' {taskId} -> taskId) (\s@TaskSummary' {} a -> s {taskId = a} :: TaskSummary)

instance Data.FromJSON TaskSummary where
  parseJSON =
    Data.withObject
      "TaskSummary"
      ( \x ->
          TaskSummary'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "taskArn")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..: "taskId")
      )

instance Prelude.Hashable TaskSummary where
  hashWithSalt _salt TaskSummary' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` taskArn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` taskId

instance Prelude.NFData TaskSummary where
  rnf TaskSummary' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf taskArn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf taskId
