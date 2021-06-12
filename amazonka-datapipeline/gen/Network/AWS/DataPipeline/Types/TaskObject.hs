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
-- Module      : Network.AWS.DataPipeline.Types.TaskObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.TaskObject where

import qualified Network.AWS.Core as Core
import Network.AWS.DataPipeline.Types.PipelineObject
import qualified Network.AWS.Lens as Lens

-- | Contains information about a pipeline task that is assigned to a task
-- runner.
--
-- /See:/ 'newTaskObject' smart constructor.
data TaskObject = TaskObject'
  { -- | The ID of the pipeline that provided the task.
    pipelineId :: Core.Maybe Core.Text,
    -- | Connection information for the location where the task runner will
    -- publish the output of the task.
    objects :: Core.Maybe (Core.HashMap Core.Text PipelineObject),
    -- | An internal identifier for the task. This ID is passed to the
    -- SetTaskStatus and ReportTaskProgress actions.
    taskId :: Core.Maybe Core.Text,
    -- | The ID of the pipeline task attempt object. AWS Data Pipeline uses this
    -- value to track how many times a task is attempted.
    attemptId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TaskObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineId', 'taskObject_pipelineId' - The ID of the pipeline that provided the task.
--
-- 'objects', 'taskObject_objects' - Connection information for the location where the task runner will
-- publish the output of the task.
--
-- 'taskId', 'taskObject_taskId' - An internal identifier for the task. This ID is passed to the
-- SetTaskStatus and ReportTaskProgress actions.
--
-- 'attemptId', 'taskObject_attemptId' - The ID of the pipeline task attempt object. AWS Data Pipeline uses this
-- value to track how many times a task is attempted.
newTaskObject ::
  TaskObject
newTaskObject =
  TaskObject'
    { pipelineId = Core.Nothing,
      objects = Core.Nothing,
      taskId = Core.Nothing,
      attemptId = Core.Nothing
    }

-- | The ID of the pipeline that provided the task.
taskObject_pipelineId :: Lens.Lens' TaskObject (Core.Maybe Core.Text)
taskObject_pipelineId = Lens.lens (\TaskObject' {pipelineId} -> pipelineId) (\s@TaskObject' {} a -> s {pipelineId = a} :: TaskObject)

-- | Connection information for the location where the task runner will
-- publish the output of the task.
taskObject_objects :: Lens.Lens' TaskObject (Core.Maybe (Core.HashMap Core.Text PipelineObject))
taskObject_objects = Lens.lens (\TaskObject' {objects} -> objects) (\s@TaskObject' {} a -> s {objects = a} :: TaskObject) Core.. Lens.mapping Lens._Coerce

-- | An internal identifier for the task. This ID is passed to the
-- SetTaskStatus and ReportTaskProgress actions.
taskObject_taskId :: Lens.Lens' TaskObject (Core.Maybe Core.Text)
taskObject_taskId = Lens.lens (\TaskObject' {taskId} -> taskId) (\s@TaskObject' {} a -> s {taskId = a} :: TaskObject)

-- | The ID of the pipeline task attempt object. AWS Data Pipeline uses this
-- value to track how many times a task is attempted.
taskObject_attemptId :: Lens.Lens' TaskObject (Core.Maybe Core.Text)
taskObject_attemptId = Lens.lens (\TaskObject' {attemptId} -> attemptId) (\s@TaskObject' {} a -> s {attemptId = a} :: TaskObject)

instance Core.FromJSON TaskObject where
  parseJSON =
    Core.withObject
      "TaskObject"
      ( \x ->
          TaskObject'
            Core.<$> (x Core..:? "pipelineId")
            Core.<*> (x Core..:? "objects" Core..!= Core.mempty)
            Core.<*> (x Core..:? "taskId")
            Core.<*> (x Core..:? "attemptId")
      )

instance Core.Hashable TaskObject

instance Core.NFData TaskObject
