{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.TaskObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.TaskObject
  ( TaskObject (..),

    -- * Smart constructor
    mkTaskObject,

    -- * Lenses
    toAttemptId,
    toObjects,
    toPipelineId,
    toTaskId,
  )
where

import qualified Network.AWS.DataPipeline.Types.AttemptId as Types
import qualified Network.AWS.DataPipeline.Types.Id as Types
import qualified Network.AWS.DataPipeline.Types.PipelineId as Types
import qualified Network.AWS.DataPipeline.Types.PipelineObject as Types
import qualified Network.AWS.DataPipeline.Types.TaskId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a pipeline task that is assigned to a task runner.
--
-- /See:/ 'mkTaskObject' smart constructor.
data TaskObject = TaskObject'
  { -- | The ID of the pipeline task attempt object. AWS Data Pipeline uses this value to track how many times a task is attempted.
    attemptId :: Core.Maybe Types.AttemptId,
    -- | Connection information for the location where the task runner will publish the output of the task.
    objects :: Core.Maybe (Core.HashMap Types.Id Types.PipelineObject),
    -- | The ID of the pipeline that provided the task.
    pipelineId :: Core.Maybe Types.PipelineId,
    -- | An internal identifier for the task. This ID is passed to the 'SetTaskStatus' and 'ReportTaskProgress' actions.
    taskId :: Core.Maybe Types.TaskId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TaskObject' value with any optional fields omitted.
mkTaskObject ::
  TaskObject
mkTaskObject =
  TaskObject'
    { attemptId = Core.Nothing,
      objects = Core.Nothing,
      pipelineId = Core.Nothing,
      taskId = Core.Nothing
    }

-- | The ID of the pipeline task attempt object. AWS Data Pipeline uses this value to track how many times a task is attempted.
--
-- /Note:/ Consider using 'attemptId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toAttemptId :: Lens.Lens' TaskObject (Core.Maybe Types.AttemptId)
toAttemptId = Lens.field @"attemptId"
{-# DEPRECATED toAttemptId "Use generic-lens or generic-optics with 'attemptId' instead." #-}

-- | Connection information for the location where the task runner will publish the output of the task.
--
-- /Note:/ Consider using 'objects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toObjects :: Lens.Lens' TaskObject (Core.Maybe (Core.HashMap Types.Id Types.PipelineObject))
toObjects = Lens.field @"objects"
{-# DEPRECATED toObjects "Use generic-lens or generic-optics with 'objects' instead." #-}

-- | The ID of the pipeline that provided the task.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPipelineId :: Lens.Lens' TaskObject (Core.Maybe Types.PipelineId)
toPipelineId = Lens.field @"pipelineId"
{-# DEPRECATED toPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | An internal identifier for the task. This ID is passed to the 'SetTaskStatus' and 'ReportTaskProgress' actions.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toTaskId :: Lens.Lens' TaskObject (Core.Maybe Types.TaskId)
toTaskId = Lens.field @"taskId"
{-# DEPRECATED toTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

instance Core.FromJSON TaskObject where
  parseJSON =
    Core.withObject "TaskObject" Core.$
      \x ->
        TaskObject'
          Core.<$> (x Core..:? "attemptId")
          Core.<*> (x Core..:? "objects")
          Core.<*> (x Core..:? "pipelineId")
          Core.<*> (x Core..:? "taskId")
