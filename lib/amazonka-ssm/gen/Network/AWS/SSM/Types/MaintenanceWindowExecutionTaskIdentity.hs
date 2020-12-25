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
    mwetiEndTime,
    mwetiStartTime,
    mwetiStatus,
    mwetiStatusDetails,
    mwetiTaskArn,
    mwetiTaskExecutionId,
    mwetiTaskType,
    mwetiWindowExecutionId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.MaintenanceWindowExecutionId as Types
import qualified Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus as Types
import qualified Network.AWS.SSM.Types.MaintenanceWindowExecutionStatusDetails as Types
import qualified Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskId as Types
import qualified Network.AWS.SSM.Types.MaintenanceWindowTaskType as Types
import qualified Network.AWS.SSM.Types.TaskArn as Types

-- | Information about a task execution performed as part of a maintenance window execution.
--
-- /See:/ 'mkMaintenanceWindowExecutionTaskIdentity' smart constructor.
data MaintenanceWindowExecutionTaskIdentity = MaintenanceWindowExecutionTaskIdentity'
  { -- | The time the task execution finished.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The time the task execution started.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | The status of the task execution.
    status :: Core.Maybe Types.MaintenanceWindowExecutionStatus,
    -- | The details explaining the status of the task execution. Only available for certain status values.
    statusDetails :: Core.Maybe Types.MaintenanceWindowExecutionStatusDetails,
    -- | The ARN of the task that ran.
    taskArn :: Core.Maybe Types.TaskArn,
    -- | The ID of the specific task execution in the maintenance window execution.
    taskExecutionId :: Core.Maybe Types.MaintenanceWindowExecutionTaskId,
    -- | The type of task that ran.
    taskType :: Core.Maybe Types.MaintenanceWindowTaskType,
    -- | The ID of the maintenance window execution that ran the task.
    windowExecutionId :: Core.Maybe Types.MaintenanceWindowExecutionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'MaintenanceWindowExecutionTaskIdentity' value with any optional fields omitted.
mkMaintenanceWindowExecutionTaskIdentity ::
  MaintenanceWindowExecutionTaskIdentity
mkMaintenanceWindowExecutionTaskIdentity =
  MaintenanceWindowExecutionTaskIdentity'
    { endTime = Core.Nothing,
      startTime = Core.Nothing,
      status = Core.Nothing,
      statusDetails = Core.Nothing,
      taskArn = Core.Nothing,
      taskExecutionId = Core.Nothing,
      taskType = Core.Nothing,
      windowExecutionId = Core.Nothing
    }

-- | The time the task execution finished.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiEndTime :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Core.NominalDiffTime)
mwetiEndTime = Lens.field @"endTime"
{-# DEPRECATED mwetiEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The time the task execution started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiStartTime :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Core.NominalDiffTime)
mwetiStartTime = Lens.field @"startTime"
{-# DEPRECATED mwetiStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The status of the task execution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiStatus :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Types.MaintenanceWindowExecutionStatus)
mwetiStatus = Lens.field @"status"
{-# DEPRECATED mwetiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The details explaining the status of the task execution. Only available for certain status values.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiStatusDetails :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Types.MaintenanceWindowExecutionStatusDetails)
mwetiStatusDetails = Lens.field @"statusDetails"
{-# DEPRECATED mwetiStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | The ARN of the task that ran.
--
-- /Note:/ Consider using 'taskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiTaskArn :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Types.TaskArn)
mwetiTaskArn = Lens.field @"taskArn"
{-# DEPRECATED mwetiTaskArn "Use generic-lens or generic-optics with 'taskArn' instead." #-}

-- | The ID of the specific task execution in the maintenance window execution.
--
-- /Note:/ Consider using 'taskExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiTaskExecutionId :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Types.MaintenanceWindowExecutionTaskId)
mwetiTaskExecutionId = Lens.field @"taskExecutionId"
{-# DEPRECATED mwetiTaskExecutionId "Use generic-lens or generic-optics with 'taskExecutionId' instead." #-}

-- | The type of task that ran.
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiTaskType :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Types.MaintenanceWindowTaskType)
mwetiTaskType = Lens.field @"taskType"
{-# DEPRECATED mwetiTaskType "Use generic-lens or generic-optics with 'taskType' instead." #-}

-- | The ID of the maintenance window execution that ran the task.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiWindowExecutionId :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Types.MaintenanceWindowExecutionId)
mwetiWindowExecutionId = Lens.field @"windowExecutionId"
{-# DEPRECATED mwetiWindowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead." #-}

instance Core.FromJSON MaintenanceWindowExecutionTaskIdentity where
  parseJSON =
    Core.withObject "MaintenanceWindowExecutionTaskIdentity" Core.$
      \x ->
        MaintenanceWindowExecutionTaskIdentity'
          Core.<$> (x Core..:? "EndTime")
          Core.<*> (x Core..:? "StartTime")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "StatusDetails")
          Core.<*> (x Core..:? "TaskArn")
          Core.<*> (x Core..:? "TaskExecutionId")
          Core.<*> (x Core..:? "TaskType")
          Core.<*> (x Core..:? "WindowExecutionId")
