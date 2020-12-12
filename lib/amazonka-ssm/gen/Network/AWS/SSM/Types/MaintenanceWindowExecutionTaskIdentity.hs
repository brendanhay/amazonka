{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskIdentity
  ( MaintenanceWindowExecutionTaskIdentity (..),

    -- * Smart constructor
    mkMaintenanceWindowExecutionTaskIdentity,

    -- * Lenses
    mwetiStatus,
    mwetiTaskExecutionId,
    mwetiStartTime,
    mwetiTaskType,
    mwetiTaskARN,
    mwetiWindowExecutionId,
    mwetiStatusDetails,
    mwetiEndTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus
import Network.AWS.SSM.Types.MaintenanceWindowTaskType

-- | Information about a task execution performed as part of a maintenance window execution.
--
-- /See:/ 'mkMaintenanceWindowExecutionTaskIdentity' smart constructor.
data MaintenanceWindowExecutionTaskIdentity = MaintenanceWindowExecutionTaskIdentity'
  { status ::
      Lude.Maybe
        MaintenanceWindowExecutionStatus,
    taskExecutionId ::
      Lude.Maybe
        Lude.Text,
    startTime ::
      Lude.Maybe
        Lude.Timestamp,
    taskType ::
      Lude.Maybe
        MaintenanceWindowTaskType,
    taskARN ::
      Lude.Maybe
        Lude.Text,
    windowExecutionId ::
      Lude.Maybe
        Lude.Text,
    statusDetails ::
      Lude.Maybe
        Lude.Text,
    endTime ::
      Lude.Maybe
        Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MaintenanceWindowExecutionTaskIdentity' with the minimum fields required to make a request.
--
-- * 'endTime' - The time the task execution finished.
-- * 'startTime' - The time the task execution started.
-- * 'status' - The status of the task execution.
-- * 'statusDetails' - The details explaining the status of the task execution. Only available for certain status values.
-- * 'taskARN' - The ARN of the task that ran.
-- * 'taskExecutionId' - The ID of the specific task execution in the maintenance window execution.
-- * 'taskType' - The type of task that ran.
-- * 'windowExecutionId' - The ID of the maintenance window execution that ran the task.
mkMaintenanceWindowExecutionTaskIdentity ::
  MaintenanceWindowExecutionTaskIdentity
mkMaintenanceWindowExecutionTaskIdentity =
  MaintenanceWindowExecutionTaskIdentity'
    { status = Lude.Nothing,
      taskExecutionId = Lude.Nothing,
      startTime = Lude.Nothing,
      taskType = Lude.Nothing,
      taskARN = Lude.Nothing,
      windowExecutionId = Lude.Nothing,
      statusDetails = Lude.Nothing,
      endTime = Lude.Nothing
    }

-- | The status of the task execution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiStatus :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Lude.Maybe MaintenanceWindowExecutionStatus)
mwetiStatus = Lens.lens (status :: MaintenanceWindowExecutionTaskIdentity -> Lude.Maybe MaintenanceWindowExecutionStatus) (\s a -> s {status = a} :: MaintenanceWindowExecutionTaskIdentity)
{-# DEPRECATED mwetiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID of the specific task execution in the maintenance window execution.
--
-- /Note:/ Consider using 'taskExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiTaskExecutionId :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Lude.Maybe Lude.Text)
mwetiTaskExecutionId = Lens.lens (taskExecutionId :: MaintenanceWindowExecutionTaskIdentity -> Lude.Maybe Lude.Text) (\s a -> s {taskExecutionId = a} :: MaintenanceWindowExecutionTaskIdentity)
{-# DEPRECATED mwetiTaskExecutionId "Use generic-lens or generic-optics with 'taskExecutionId' instead." #-}

-- | The time the task execution started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiStartTime :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Lude.Maybe Lude.Timestamp)
mwetiStartTime = Lens.lens (startTime :: MaintenanceWindowExecutionTaskIdentity -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: MaintenanceWindowExecutionTaskIdentity)
{-# DEPRECATED mwetiStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The type of task that ran.
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiTaskType :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Lude.Maybe MaintenanceWindowTaskType)
mwetiTaskType = Lens.lens (taskType :: MaintenanceWindowExecutionTaskIdentity -> Lude.Maybe MaintenanceWindowTaskType) (\s a -> s {taskType = a} :: MaintenanceWindowExecutionTaskIdentity)
{-# DEPRECATED mwetiTaskType "Use generic-lens or generic-optics with 'taskType' instead." #-}

-- | The ARN of the task that ran.
--
-- /Note:/ Consider using 'taskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiTaskARN :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Lude.Maybe Lude.Text)
mwetiTaskARN = Lens.lens (taskARN :: MaintenanceWindowExecutionTaskIdentity -> Lude.Maybe Lude.Text) (\s a -> s {taskARN = a} :: MaintenanceWindowExecutionTaskIdentity)
{-# DEPRECATED mwetiTaskARN "Use generic-lens or generic-optics with 'taskARN' instead." #-}

-- | The ID of the maintenance window execution that ran the task.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiWindowExecutionId :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Lude.Maybe Lude.Text)
mwetiWindowExecutionId = Lens.lens (windowExecutionId :: MaintenanceWindowExecutionTaskIdentity -> Lude.Maybe Lude.Text) (\s a -> s {windowExecutionId = a} :: MaintenanceWindowExecutionTaskIdentity)
{-# DEPRECATED mwetiWindowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead." #-}

-- | The details explaining the status of the task execution. Only available for certain status values.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiStatusDetails :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Lude.Maybe Lude.Text)
mwetiStatusDetails = Lens.lens (statusDetails :: MaintenanceWindowExecutionTaskIdentity -> Lude.Maybe Lude.Text) (\s a -> s {statusDetails = a} :: MaintenanceWindowExecutionTaskIdentity)
{-# DEPRECATED mwetiStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | The time the task execution finished.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiEndTime :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Lude.Maybe Lude.Timestamp)
mwetiEndTime = Lens.lens (endTime :: MaintenanceWindowExecutionTaskIdentity -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: MaintenanceWindowExecutionTaskIdentity)
{-# DEPRECATED mwetiEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Lude.FromJSON MaintenanceWindowExecutionTaskIdentity where
  parseJSON =
    Lude.withObject
      "MaintenanceWindowExecutionTaskIdentity"
      ( \x ->
          MaintenanceWindowExecutionTaskIdentity'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "TaskExecutionId")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "TaskType")
            Lude.<*> (x Lude..:? "TaskArn")
            Lude.<*> (x Lude..:? "WindowExecutionId")
            Lude.<*> (x Lude..:? "StatusDetails")
            Lude.<*> (x Lude..:? "EndTime")
      )
