{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskIdentity
  ( MaintenanceWindowExecutionTaskIdentity (..)
  -- * Smart constructor
  , mkMaintenanceWindowExecutionTaskIdentity
  -- * Lenses
  , mwetiEndTime
  , mwetiStartTime
  , mwetiStatus
  , mwetiStatusDetails
  , mwetiTaskArn
  , mwetiTaskExecutionId
  , mwetiTaskType
  , mwetiWindowExecutionId
  ) where

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
  { endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the task execution finished.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the task execution started.
  , status :: Core.Maybe Types.MaintenanceWindowExecutionStatus
    -- ^ The status of the task execution.
  , statusDetails :: Core.Maybe Types.MaintenanceWindowExecutionStatusDetails
    -- ^ The details explaining the status of the task execution. Only available for certain status values.
  , taskArn :: Core.Maybe Types.TaskArn
    -- ^ The ARN of the task that ran.
  , taskExecutionId :: Core.Maybe Types.MaintenanceWindowExecutionTaskId
    -- ^ The ID of the specific task execution in the maintenance window execution.
  , taskType :: Core.Maybe Types.MaintenanceWindowTaskType
    -- ^ The type of task that ran.
  , windowExecutionId :: Core.Maybe Types.MaintenanceWindowExecutionId
    -- ^ The ID of the maintenance window execution that ran the task.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'MaintenanceWindowExecutionTaskIdentity' value with any optional fields omitted.
mkMaintenanceWindowExecutionTaskIdentity
    :: MaintenanceWindowExecutionTaskIdentity
mkMaintenanceWindowExecutionTaskIdentity
  = MaintenanceWindowExecutionTaskIdentity'{endTime = Core.Nothing,
                                            startTime = Core.Nothing, status = Core.Nothing,
                                            statusDetails = Core.Nothing, taskArn = Core.Nothing,
                                            taskExecutionId = Core.Nothing, taskType = Core.Nothing,
                                            windowExecutionId = Core.Nothing}

-- | The time the task execution finished.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiEndTime :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Core.NominalDiffTime)
mwetiEndTime = Lens.field @"endTime"
{-# INLINEABLE mwetiEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The time the task execution started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiStartTime :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Core.NominalDiffTime)
mwetiStartTime = Lens.field @"startTime"
{-# INLINEABLE mwetiStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The status of the task execution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiStatus :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Types.MaintenanceWindowExecutionStatus)
mwetiStatus = Lens.field @"status"
{-# INLINEABLE mwetiStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The details explaining the status of the task execution. Only available for certain status values.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiStatusDetails :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Types.MaintenanceWindowExecutionStatusDetails)
mwetiStatusDetails = Lens.field @"statusDetails"
{-# INLINEABLE mwetiStatusDetails #-}
{-# DEPRECATED statusDetails "Use generic-lens or generic-optics with 'statusDetails' instead"  #-}

-- | The ARN of the task that ran.
--
-- /Note:/ Consider using 'taskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiTaskArn :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Types.TaskArn)
mwetiTaskArn = Lens.field @"taskArn"
{-# INLINEABLE mwetiTaskArn #-}
{-# DEPRECATED taskArn "Use generic-lens or generic-optics with 'taskArn' instead"  #-}

-- | The ID of the specific task execution in the maintenance window execution.
--
-- /Note:/ Consider using 'taskExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiTaskExecutionId :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Types.MaintenanceWindowExecutionTaskId)
mwetiTaskExecutionId = Lens.field @"taskExecutionId"
{-# INLINEABLE mwetiTaskExecutionId #-}
{-# DEPRECATED taskExecutionId "Use generic-lens or generic-optics with 'taskExecutionId' instead"  #-}

-- | The type of task that ran.
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiTaskType :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Types.MaintenanceWindowTaskType)
mwetiTaskType = Lens.field @"taskType"
{-# INLINEABLE mwetiTaskType #-}
{-# DEPRECATED taskType "Use generic-lens or generic-optics with 'taskType' instead"  #-}

-- | The ID of the maintenance window execution that ran the task.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiWindowExecutionId :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Types.MaintenanceWindowExecutionId)
mwetiWindowExecutionId = Lens.field @"windowExecutionId"
{-# INLINEABLE mwetiWindowExecutionId #-}
{-# DEPRECATED windowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead"  #-}

instance Core.FromJSON MaintenanceWindowExecutionTaskIdentity where
        parseJSON
          = Core.withObject "MaintenanceWindowExecutionTaskIdentity" Core.$
              \ x ->
                MaintenanceWindowExecutionTaskIdentity' Core.<$>
                  (x Core..:? "EndTime") Core.<*> x Core..:? "StartTime" Core.<*>
                    x Core..:? "Status"
                    Core.<*> x Core..:? "StatusDetails"
                    Core.<*> x Core..:? "TaskArn"
                    Core.<*> x Core..:? "TaskExecutionId"
                    Core.<*> x Core..:? "TaskType"
                    Core.<*> x Core..:? "WindowExecutionId"
