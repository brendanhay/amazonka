-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TaskRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TaskRun
  ( TaskRun (..),

    -- * Smart constructor
    mkTaskRun,

    -- * Lenses
    trCompletedOn,
    trStatus,
    trLastModifiedOn,
    trErrorString,
    trStartedOn,
    trLogGroupName,
    trExecutionTime,
    trProperties,
    trTransformId,
    trTaskRunId,
  )
where

import Network.AWS.Glue.Types.TaskRunProperties
import Network.AWS.Glue.Types.TaskStatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The sampling parameters that are associated with the machine learning transform.
--
-- /See:/ 'mkTaskRun' smart constructor.
data TaskRun = TaskRun'
  { completedOn :: Lude.Maybe Lude.Timestamp,
    status :: Lude.Maybe TaskStatusType,
    lastModifiedOn :: Lude.Maybe Lude.Timestamp,
    errorString :: Lude.Maybe Lude.Text,
    startedOn :: Lude.Maybe Lude.Timestamp,
    logGroupName :: Lude.Maybe Lude.Text,
    executionTime :: Lude.Maybe Lude.Int,
    properties :: Lude.Maybe TaskRunProperties,
    transformId :: Lude.Maybe Lude.Text,
    taskRunId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TaskRun' with the minimum fields required to make a request.
--
-- * 'completedOn' - The last point in time that the requested task run was completed.
-- * 'errorString' - The list of error strings associated with this task run.
-- * 'executionTime' - The amount of time (in seconds) that the task run consumed resources.
-- * 'lastModifiedOn' - The last point in time that the requested task run was updated.
-- * 'logGroupName' - The names of the log group for secure logging, associated with this task run.
-- * 'properties' - Specifies configuration properties associated with this task run.
-- * 'startedOn' - The date and time that this task run started.
-- * 'status' - The current status of the requested task run.
-- * 'taskRunId' - The unique identifier for this task run.
-- * 'transformId' - The unique identifier for the transform.
mkTaskRun ::
  TaskRun
mkTaskRun =
  TaskRun'
    { completedOn = Lude.Nothing,
      status = Lude.Nothing,
      lastModifiedOn = Lude.Nothing,
      errorString = Lude.Nothing,
      startedOn = Lude.Nothing,
      logGroupName = Lude.Nothing,
      executionTime = Lude.Nothing,
      properties = Lude.Nothing,
      transformId = Lude.Nothing,
      taskRunId = Lude.Nothing
    }

-- | The last point in time that the requested task run was completed.
--
-- /Note:/ Consider using 'completedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trCompletedOn :: Lens.Lens' TaskRun (Lude.Maybe Lude.Timestamp)
trCompletedOn = Lens.lens (completedOn :: TaskRun -> Lude.Maybe Lude.Timestamp) (\s a -> s {completedOn = a} :: TaskRun)
{-# DEPRECATED trCompletedOn "Use generic-lens or generic-optics with 'completedOn' instead." #-}

-- | The current status of the requested task run.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trStatus :: Lens.Lens' TaskRun (Lude.Maybe TaskStatusType)
trStatus = Lens.lens (status :: TaskRun -> Lude.Maybe TaskStatusType) (\s a -> s {status = a} :: TaskRun)
{-# DEPRECATED trStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The last point in time that the requested task run was updated.
--
-- /Note:/ Consider using 'lastModifiedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trLastModifiedOn :: Lens.Lens' TaskRun (Lude.Maybe Lude.Timestamp)
trLastModifiedOn = Lens.lens (lastModifiedOn :: TaskRun -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedOn = a} :: TaskRun)
{-# DEPRECATED trLastModifiedOn "Use generic-lens or generic-optics with 'lastModifiedOn' instead." #-}

-- | The list of error strings associated with this task run.
--
-- /Note:/ Consider using 'errorString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trErrorString :: Lens.Lens' TaskRun (Lude.Maybe Lude.Text)
trErrorString = Lens.lens (errorString :: TaskRun -> Lude.Maybe Lude.Text) (\s a -> s {errorString = a} :: TaskRun)
{-# DEPRECATED trErrorString "Use generic-lens or generic-optics with 'errorString' instead." #-}

-- | The date and time that this task run started.
--
-- /Note:/ Consider using 'startedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trStartedOn :: Lens.Lens' TaskRun (Lude.Maybe Lude.Timestamp)
trStartedOn = Lens.lens (startedOn :: TaskRun -> Lude.Maybe Lude.Timestamp) (\s a -> s {startedOn = a} :: TaskRun)
{-# DEPRECATED trStartedOn "Use generic-lens or generic-optics with 'startedOn' instead." #-}

-- | The names of the log group for secure logging, associated with this task run.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trLogGroupName :: Lens.Lens' TaskRun (Lude.Maybe Lude.Text)
trLogGroupName = Lens.lens (logGroupName :: TaskRun -> Lude.Maybe Lude.Text) (\s a -> s {logGroupName = a} :: TaskRun)
{-# DEPRECATED trLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The amount of time (in seconds) that the task run consumed resources.
--
-- /Note:/ Consider using 'executionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trExecutionTime :: Lens.Lens' TaskRun (Lude.Maybe Lude.Int)
trExecutionTime = Lens.lens (executionTime :: TaskRun -> Lude.Maybe Lude.Int) (\s a -> s {executionTime = a} :: TaskRun)
{-# DEPRECATED trExecutionTime "Use generic-lens or generic-optics with 'executionTime' instead." #-}

-- | Specifies configuration properties associated with this task run.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trProperties :: Lens.Lens' TaskRun (Lude.Maybe TaskRunProperties)
trProperties = Lens.lens (properties :: TaskRun -> Lude.Maybe TaskRunProperties) (\s a -> s {properties = a} :: TaskRun)
{-# DEPRECATED trProperties "Use generic-lens or generic-optics with 'properties' instead." #-}

-- | The unique identifier for the transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTransformId :: Lens.Lens' TaskRun (Lude.Maybe Lude.Text)
trTransformId = Lens.lens (transformId :: TaskRun -> Lude.Maybe Lude.Text) (\s a -> s {transformId = a} :: TaskRun)
{-# DEPRECATED trTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

-- | The unique identifier for this task run.
--
-- /Note:/ Consider using 'taskRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTaskRunId :: Lens.Lens' TaskRun (Lude.Maybe Lude.Text)
trTaskRunId = Lens.lens (taskRunId :: TaskRun -> Lude.Maybe Lude.Text) (\s a -> s {taskRunId = a} :: TaskRun)
{-# DEPRECATED trTaskRunId "Use generic-lens or generic-optics with 'taskRunId' instead." #-}

instance Lude.FromJSON TaskRun where
  parseJSON =
    Lude.withObject
      "TaskRun"
      ( \x ->
          TaskRun'
            Lude.<$> (x Lude..:? "CompletedOn")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "LastModifiedOn")
            Lude.<*> (x Lude..:? "ErrorString")
            Lude.<*> (x Lude..:? "StartedOn")
            Lude.<*> (x Lude..:? "LogGroupName")
            Lude.<*> (x Lude..:? "ExecutionTime")
            Lude.<*> (x Lude..:? "Properties")
            Lude.<*> (x Lude..:? "TransformId")
            Lude.<*> (x Lude..:? "TaskRunId")
      )
