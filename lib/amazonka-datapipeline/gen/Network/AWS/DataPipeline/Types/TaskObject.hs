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
    toPipelineId,
    toAttemptId,
    toTaskId,
    toObjects,
  )
where

import Network.AWS.DataPipeline.Types.PipelineObject
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a pipeline task that is assigned to a task runner.
--
-- /See:/ 'mkTaskObject' smart constructor.
data TaskObject = TaskObject'
  { -- | The ID of the pipeline that provided the task.
    pipelineId :: Lude.Maybe Lude.Text,
    -- | The ID of the pipeline task attempt object. AWS Data Pipeline uses this value to track how many times a task is attempted.
    attemptId :: Lude.Maybe Lude.Text,
    -- | An internal identifier for the task. This ID is passed to the 'SetTaskStatus' and 'ReportTaskProgress' actions.
    taskId :: Lude.Maybe Lude.Text,
    -- | Connection information for the location where the task runner will publish the output of the task.
    objects :: Lude.Maybe (Lude.HashMap Lude.Text (PipelineObject))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TaskObject' with the minimum fields required to make a request.
--
-- * 'pipelineId' - The ID of the pipeline that provided the task.
-- * 'attemptId' - The ID of the pipeline task attempt object. AWS Data Pipeline uses this value to track how many times a task is attempted.
-- * 'taskId' - An internal identifier for the task. This ID is passed to the 'SetTaskStatus' and 'ReportTaskProgress' actions.
-- * 'objects' - Connection information for the location where the task runner will publish the output of the task.
mkTaskObject ::
  TaskObject
mkTaskObject =
  TaskObject'
    { pipelineId = Lude.Nothing,
      attemptId = Lude.Nothing,
      taskId = Lude.Nothing,
      objects = Lude.Nothing
    }

-- | The ID of the pipeline that provided the task.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toPipelineId :: Lens.Lens' TaskObject (Lude.Maybe Lude.Text)
toPipelineId = Lens.lens (pipelineId :: TaskObject -> Lude.Maybe Lude.Text) (\s a -> s {pipelineId = a} :: TaskObject)
{-# DEPRECATED toPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | The ID of the pipeline task attempt object. AWS Data Pipeline uses this value to track how many times a task is attempted.
--
-- /Note:/ Consider using 'attemptId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toAttemptId :: Lens.Lens' TaskObject (Lude.Maybe Lude.Text)
toAttemptId = Lens.lens (attemptId :: TaskObject -> Lude.Maybe Lude.Text) (\s a -> s {attemptId = a} :: TaskObject)
{-# DEPRECATED toAttemptId "Use generic-lens or generic-optics with 'attemptId' instead." #-}

-- | An internal identifier for the task. This ID is passed to the 'SetTaskStatus' and 'ReportTaskProgress' actions.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toTaskId :: Lens.Lens' TaskObject (Lude.Maybe Lude.Text)
toTaskId = Lens.lens (taskId :: TaskObject -> Lude.Maybe Lude.Text) (\s a -> s {taskId = a} :: TaskObject)
{-# DEPRECATED toTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | Connection information for the location where the task runner will publish the output of the task.
--
-- /Note:/ Consider using 'objects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toObjects :: Lens.Lens' TaskObject (Lude.Maybe (Lude.HashMap Lude.Text (PipelineObject)))
toObjects = Lens.lens (objects :: TaskObject -> Lude.Maybe (Lude.HashMap Lude.Text (PipelineObject))) (\s a -> s {objects = a} :: TaskObject)
{-# DEPRECATED toObjects "Use generic-lens or generic-optics with 'objects' instead." #-}

instance Lude.FromJSON TaskObject where
  parseJSON =
    Lude.withObject
      "TaskObject"
      ( \x ->
          TaskObject'
            Lude.<$> (x Lude..:? "pipelineId")
            Lude.<*> (x Lude..:? "attemptId")
            Lude.<*> (x Lude..:? "taskId")
            Lude.<*> (x Lude..:? "objects" Lude..!= Lude.mempty)
      )
