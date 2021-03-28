{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskInvocationIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskInvocationIdentity
  ( MaintenanceWindowExecutionTaskInvocationIdentity (..)
  -- * Smart constructor
  , mkMaintenanceWindowExecutionTaskInvocationIdentity
  -- * Lenses
  , mwetiiEndTime
  , mwetiiExecutionId
  , mwetiiInvocationId
  , mwetiiOwnerInformation
  , mwetiiParameters
  , mwetiiStartTime
  , mwetiiStatus
  , mwetiiStatusDetails
  , mwetiiTaskExecutionId
  , mwetiiTaskType
  , mwetiiWindowExecutionId
  , mwetiiWindowTargetId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.ExecutionId as Types
import qualified Network.AWS.SSM.Types.InvocationId as Types
import qualified Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus as Types
import qualified Network.AWS.SSM.Types.MaintenanceWindowExecutionStatusDetails as Types
import qualified Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskInvocationParameters as Types
import qualified Network.AWS.SSM.Types.MaintenanceWindowTaskType as Types
import qualified Network.AWS.SSM.Types.OwnerInformation as Types
import qualified Network.AWS.SSM.Types.TaskExecutionId as Types
import qualified Network.AWS.SSM.Types.WindowExecutionId as Types
import qualified Network.AWS.SSM.Types.WindowTargetId as Types

-- | Describes the information about a task invocation for a particular target as part of a task execution performed as part of a maintenance window execution.
--
-- /See:/ 'mkMaintenanceWindowExecutionTaskInvocationIdentity' smart constructor.
data MaintenanceWindowExecutionTaskInvocationIdentity = MaintenanceWindowExecutionTaskInvocationIdentity'
  { endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the invocation finished.
  , executionId :: Core.Maybe Types.ExecutionId
    -- ^ The ID of the action performed in the service that actually handled the task invocation. If the task type is RUN_COMMAND, this value is the command ID.
  , invocationId :: Core.Maybe Types.InvocationId
    -- ^ The ID of the task invocation.
  , ownerInformation :: Core.Maybe Types.OwnerInformation
    -- ^ User-provided value that was specified when the target was registered with the maintenance window. This was also included in any CloudWatch events raised during the task invocation.
  , parameters :: Core.Maybe Types.MaintenanceWindowExecutionTaskInvocationParameters
    -- ^ The parameters that were provided for the invocation when it was run.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the invocation started.
  , status :: Core.Maybe Types.MaintenanceWindowExecutionStatus
    -- ^ The status of the task invocation.
  , statusDetails :: Core.Maybe Types.MaintenanceWindowExecutionStatusDetails
    -- ^ The details explaining the status of the task invocation. Only available for certain Status values. 
  , taskExecutionId :: Core.Maybe Types.TaskExecutionId
    -- ^ The ID of the specific task execution in the maintenance window execution.
  , taskType :: Core.Maybe Types.MaintenanceWindowTaskType
    -- ^ The task type.
  , windowExecutionId :: Core.Maybe Types.WindowExecutionId
    -- ^ The ID of the maintenance window execution that ran the task.
  , windowTargetId :: Core.Maybe Types.WindowTargetId
    -- ^ The ID of the target definition in this maintenance window the invocation was performed for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'MaintenanceWindowExecutionTaskInvocationIdentity' value with any optional fields omitted.
mkMaintenanceWindowExecutionTaskInvocationIdentity
    :: MaintenanceWindowExecutionTaskInvocationIdentity
mkMaintenanceWindowExecutionTaskInvocationIdentity
  = MaintenanceWindowExecutionTaskInvocationIdentity'{endTime =
                                                        Core.Nothing,
                                                      executionId = Core.Nothing,
                                                      invocationId = Core.Nothing,
                                                      ownerInformation = Core.Nothing,
                                                      parameters = Core.Nothing,
                                                      startTime = Core.Nothing,
                                                      status = Core.Nothing,
                                                      statusDetails = Core.Nothing,
                                                      taskExecutionId = Core.Nothing,
                                                      taskType = Core.Nothing,
                                                      windowExecutionId = Core.Nothing,
                                                      windowTargetId = Core.Nothing}

-- | The time the invocation finished.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiEndTime :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Core.Maybe Core.NominalDiffTime)
mwetiiEndTime = Lens.field @"endTime"
{-# INLINEABLE mwetiiEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The ID of the action performed in the service that actually handled the task invocation. If the task type is RUN_COMMAND, this value is the command ID.
--
-- /Note:/ Consider using 'executionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiExecutionId :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Core.Maybe Types.ExecutionId)
mwetiiExecutionId = Lens.field @"executionId"
{-# INLINEABLE mwetiiExecutionId #-}
{-# DEPRECATED executionId "Use generic-lens or generic-optics with 'executionId' instead"  #-}

-- | The ID of the task invocation.
--
-- /Note:/ Consider using 'invocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiInvocationId :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Core.Maybe Types.InvocationId)
mwetiiInvocationId = Lens.field @"invocationId"
{-# INLINEABLE mwetiiInvocationId #-}
{-# DEPRECATED invocationId "Use generic-lens or generic-optics with 'invocationId' instead"  #-}

-- | User-provided value that was specified when the target was registered with the maintenance window. This was also included in any CloudWatch events raised during the task invocation.
--
-- /Note:/ Consider using 'ownerInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiOwnerInformation :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Core.Maybe Types.OwnerInformation)
mwetiiOwnerInformation = Lens.field @"ownerInformation"
{-# INLINEABLE mwetiiOwnerInformation #-}
{-# DEPRECATED ownerInformation "Use generic-lens or generic-optics with 'ownerInformation' instead"  #-}

-- | The parameters that were provided for the invocation when it was run.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiParameters :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Core.Maybe Types.MaintenanceWindowExecutionTaskInvocationParameters)
mwetiiParameters = Lens.field @"parameters"
{-# INLINEABLE mwetiiParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The time the invocation started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiStartTime :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Core.Maybe Core.NominalDiffTime)
mwetiiStartTime = Lens.field @"startTime"
{-# INLINEABLE mwetiiStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The status of the task invocation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiStatus :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Core.Maybe Types.MaintenanceWindowExecutionStatus)
mwetiiStatus = Lens.field @"status"
{-# INLINEABLE mwetiiStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The details explaining the status of the task invocation. Only available for certain Status values. 
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiStatusDetails :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Core.Maybe Types.MaintenanceWindowExecutionStatusDetails)
mwetiiStatusDetails = Lens.field @"statusDetails"
{-# INLINEABLE mwetiiStatusDetails #-}
{-# DEPRECATED statusDetails "Use generic-lens or generic-optics with 'statusDetails' instead"  #-}

-- | The ID of the specific task execution in the maintenance window execution.
--
-- /Note:/ Consider using 'taskExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiTaskExecutionId :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Core.Maybe Types.TaskExecutionId)
mwetiiTaskExecutionId = Lens.field @"taskExecutionId"
{-# INLINEABLE mwetiiTaskExecutionId #-}
{-# DEPRECATED taskExecutionId "Use generic-lens or generic-optics with 'taskExecutionId' instead"  #-}

-- | The task type.
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiTaskType :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Core.Maybe Types.MaintenanceWindowTaskType)
mwetiiTaskType = Lens.field @"taskType"
{-# INLINEABLE mwetiiTaskType #-}
{-# DEPRECATED taskType "Use generic-lens or generic-optics with 'taskType' instead"  #-}

-- | The ID of the maintenance window execution that ran the task.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiWindowExecutionId :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Core.Maybe Types.WindowExecutionId)
mwetiiWindowExecutionId = Lens.field @"windowExecutionId"
{-# INLINEABLE mwetiiWindowExecutionId #-}
{-# DEPRECATED windowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead"  #-}

-- | The ID of the target definition in this maintenance window the invocation was performed for.
--
-- /Note:/ Consider using 'windowTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwetiiWindowTargetId :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Core.Maybe Types.WindowTargetId)
mwetiiWindowTargetId = Lens.field @"windowTargetId"
{-# INLINEABLE mwetiiWindowTargetId #-}
{-# DEPRECATED windowTargetId "Use generic-lens or generic-optics with 'windowTargetId' instead"  #-}

instance Core.FromJSON
           MaintenanceWindowExecutionTaskInvocationIdentity
         where
        parseJSON
          = Core.withObject
              "MaintenanceWindowExecutionTaskInvocationIdentity"
              Core.$
              \ x ->
                MaintenanceWindowExecutionTaskInvocationIdentity' Core.<$>
                  (x Core..:? "EndTime") Core.<*> x Core..:? "ExecutionId" Core.<*>
                    x Core..:? "InvocationId"
                    Core.<*> x Core..:? "OwnerInformation"
                    Core.<*> x Core..:? "Parameters"
                    Core.<*> x Core..:? "StartTime"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "StatusDetails"
                    Core.<*> x Core..:? "TaskExecutionId"
                    Core.<*> x Core..:? "TaskType"
                    Core.<*> x Core..:? "WindowExecutionId"
                    Core.<*> x Core..:? "WindowTargetId"
