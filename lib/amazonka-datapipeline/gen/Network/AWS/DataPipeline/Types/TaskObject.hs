{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.TaskObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DataPipeline.Types.TaskObject
  ( TaskObject (..)
  -- * Smart constructor
  , mkTaskObject
  -- * Lenses
  , toAttemptId
  , toObjects
  , toPipelineId
  , toTaskId
  ) where

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
  { attemptId :: Core.Maybe Types.AttemptId
    -- ^ The ID of the pipeline task attempt object. AWS Data Pipeline uses this value to track how many times a task is attempted.
  , objects :: Core.Maybe (Core.HashMap Types.Id Types.PipelineObject)
    -- ^ Connection information for the location where the task runner will publish the output of the task.
  , pipelineId :: Core.Maybe Types.PipelineId
    -- ^ The ID of the pipeline that provided the task.
  , taskId :: Core.Maybe Types.TaskId
    -- ^ An internal identifier for the task. This ID is passed to the 'SetTaskStatus' and 'ReportTaskProgress' actions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TaskObject' value with any optional fields omitted.
mkTaskObject
    :: TaskObject
mkTaskObject
  = TaskObject'{attemptId = Core.Nothing, objects = Core.Nothing,
                pipelineId = Core.Nothing, taskId = Core.Nothing}

-- | The ID of the pipeline task attempt object. AWS Data Pipeline uses this value to track how many times a task is attempted.
--
-- /Note:/ Consider using 'attemptId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toAttemptId :: Lens.Lens' TaskObject (Core.Maybe Types.AttemptId)
toAttemptId = Lens.field @"attemptId"
{-# INLINEABLE toAttemptId #-}
{-# DEPRECATED attemptId "Use generic-lens or generic-optics with 'attemptId' instead"  #-}

-- | Connection information for the location where the task runner will publish the output of the task.
--
-- /Note:/ Consider using 'objects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toObjects :: Lens.Lens' TaskObject (Core.Maybe (Core.HashMap Types.Id Types.PipelineObject))
toObjects = Lens.field @"objects"
{-# INLINEABLE toObjects #-}
{-# DEPRECATED objects "Use generic-lens or generic-optics with 'objects' instead"  #-}

-- | The ID of the pipeline that provided the task.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPipelineId :: Lens.Lens' TaskObject (Core.Maybe Types.PipelineId)
toPipelineId = Lens.field @"pipelineId"
{-# INLINEABLE toPipelineId #-}
{-# DEPRECATED pipelineId "Use generic-lens or generic-optics with 'pipelineId' instead"  #-}

-- | An internal identifier for the task. This ID is passed to the 'SetTaskStatus' and 'ReportTaskProgress' actions.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toTaskId :: Lens.Lens' TaskObject (Core.Maybe Types.TaskId)
toTaskId = Lens.field @"taskId"
{-# INLINEABLE toTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

instance Core.FromJSON TaskObject where
        parseJSON
          = Core.withObject "TaskObject" Core.$
              \ x ->
                TaskObject' Core.<$>
                  (x Core..:? "attemptId") Core.<*> x Core..:? "objects" Core.<*>
                    x Core..:? "pipelineId"
                    Core.<*> x Core..:? "taskId"
