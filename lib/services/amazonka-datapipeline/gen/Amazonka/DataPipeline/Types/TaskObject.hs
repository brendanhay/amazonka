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
-- Module      : Amazonka.DataPipeline.Types.TaskObject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataPipeline.Types.TaskObject where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataPipeline.Types.PipelineObject
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a pipeline task that is assigned to a task
-- runner.
--
-- /See:/ 'newTaskObject' smart constructor.
data TaskObject = TaskObject'
  { -- | The ID of the pipeline task attempt object. AWS Data Pipeline uses this
    -- value to track how many times a task is attempted.
    attemptId :: Prelude.Maybe Prelude.Text,
    -- | An internal identifier for the task. This ID is passed to the
    -- SetTaskStatus and ReportTaskProgress actions.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the pipeline that provided the task.
    pipelineId :: Prelude.Maybe Prelude.Text,
    -- | Connection information for the location where the task runner will
    -- publish the output of the task.
    objects :: Prelude.Maybe (Prelude.HashMap Prelude.Text PipelineObject)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attemptId', 'taskObject_attemptId' - The ID of the pipeline task attempt object. AWS Data Pipeline uses this
-- value to track how many times a task is attempted.
--
-- 'taskId', 'taskObject_taskId' - An internal identifier for the task. This ID is passed to the
-- SetTaskStatus and ReportTaskProgress actions.
--
-- 'pipelineId', 'taskObject_pipelineId' - The ID of the pipeline that provided the task.
--
-- 'objects', 'taskObject_objects' - Connection information for the location where the task runner will
-- publish the output of the task.
newTaskObject ::
  TaskObject
newTaskObject =
  TaskObject'
    { attemptId = Prelude.Nothing,
      taskId = Prelude.Nothing,
      pipelineId = Prelude.Nothing,
      objects = Prelude.Nothing
    }

-- | The ID of the pipeline task attempt object. AWS Data Pipeline uses this
-- value to track how many times a task is attempted.
taskObject_attemptId :: Lens.Lens' TaskObject (Prelude.Maybe Prelude.Text)
taskObject_attemptId = Lens.lens (\TaskObject' {attemptId} -> attemptId) (\s@TaskObject' {} a -> s {attemptId = a} :: TaskObject)

-- | An internal identifier for the task. This ID is passed to the
-- SetTaskStatus and ReportTaskProgress actions.
taskObject_taskId :: Lens.Lens' TaskObject (Prelude.Maybe Prelude.Text)
taskObject_taskId = Lens.lens (\TaskObject' {taskId} -> taskId) (\s@TaskObject' {} a -> s {taskId = a} :: TaskObject)

-- | The ID of the pipeline that provided the task.
taskObject_pipelineId :: Lens.Lens' TaskObject (Prelude.Maybe Prelude.Text)
taskObject_pipelineId = Lens.lens (\TaskObject' {pipelineId} -> pipelineId) (\s@TaskObject' {} a -> s {pipelineId = a} :: TaskObject)

-- | Connection information for the location where the task runner will
-- publish the output of the task.
taskObject_objects :: Lens.Lens' TaskObject (Prelude.Maybe (Prelude.HashMap Prelude.Text PipelineObject))
taskObject_objects = Lens.lens (\TaskObject' {objects} -> objects) (\s@TaskObject' {} a -> s {objects = a} :: TaskObject) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON TaskObject where
  parseJSON =
    Core.withObject
      "TaskObject"
      ( \x ->
          TaskObject'
            Prelude.<$> (x Core..:? "attemptId")
            Prelude.<*> (x Core..:? "taskId")
            Prelude.<*> (x Core..:? "pipelineId")
            Prelude.<*> (x Core..:? "objects" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable TaskObject where
  hashWithSalt _salt TaskObject' {..} =
    _salt `Prelude.hashWithSalt` attemptId
      `Prelude.hashWithSalt` taskId
      `Prelude.hashWithSalt` pipelineId
      `Prelude.hashWithSalt` objects

instance Prelude.NFData TaskObject where
  rnf TaskObject' {..} =
    Prelude.rnf attemptId
      `Prelude.seq` Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf pipelineId
      `Prelude.seq` Prelude.rnf objects
