{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditMitigationActionExecutionMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditMitigationActionExecutionMetadata
  ( AuditMitigationActionExecutionMetadata (..),

    -- * Smart constructor
    mkAuditMitigationActionExecutionMetadata,

    -- * Lenses
    amaemActionId,
    amaemActionName,
    amaemEndTime,
    amaemErrorCode,
    amaemFindingId,
    amaemMessage,
    amaemStartTime,
    amaemStatus,
    amaemTaskId,
  )
where

import qualified Network.AWS.IoT.Types.AuditMitigationActionsExecutionStatus as Types
import qualified Network.AWS.IoT.Types.AuditMitigationActionsTaskId as Types
import qualified Network.AWS.IoT.Types.ErrorCode as Types
import qualified Network.AWS.IoT.Types.ErrorMessage as Types
import qualified Network.AWS.IoT.Types.FindingId as Types
import qualified Network.AWS.IoT.Types.MitigationActionId as Types
import qualified Network.AWS.IoT.Types.MitigationActionName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returned by ListAuditMitigationActionsTask, this object contains information that describes a mitigation action that has been started.
--
-- /See:/ 'mkAuditMitigationActionExecutionMetadata' smart constructor.
data AuditMitigationActionExecutionMetadata = AuditMitigationActionExecutionMetadata'
  { -- | The unique identifier for the mitigation action being applied by the task.
    actionId :: Core.Maybe Types.MitigationActionId,
    -- | The friendly name of the mitigation action being applied by the task.
    actionName :: Core.Maybe Types.MitigationActionName,
    -- | The date and time when the task was completed or canceled. Blank if the task is still running.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | If an error occurred, the code that indicates which type of error occurred.
    errorCode :: Core.Maybe Types.ErrorCode,
    -- | The unique identifier for the findings to which the task and associated mitigation action are applied.
    findingId :: Core.Maybe Types.FindingId,
    -- | If an error occurred, a message that describes the error.
    message :: Core.Maybe Types.ErrorMessage,
    -- | The date and time when the task was started.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | The current status of the task being executed.
    status :: Core.Maybe Types.AuditMitigationActionsExecutionStatus,
    -- | The unique identifier for the task that applies the mitigation action.
    taskId :: Core.Maybe Types.AuditMitigationActionsTaskId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AuditMitigationActionExecutionMetadata' value with any optional fields omitted.
mkAuditMitigationActionExecutionMetadata ::
  AuditMitigationActionExecutionMetadata
mkAuditMitigationActionExecutionMetadata =
  AuditMitigationActionExecutionMetadata'
    { actionId = Core.Nothing,
      actionName = Core.Nothing,
      endTime = Core.Nothing,
      errorCode = Core.Nothing,
      findingId = Core.Nothing,
      message = Core.Nothing,
      startTime = Core.Nothing,
      status = Core.Nothing,
      taskId = Core.Nothing
    }

-- | The unique identifier for the mitigation action being applied by the task.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amaemActionId :: Lens.Lens' AuditMitigationActionExecutionMetadata (Core.Maybe Types.MitigationActionId)
amaemActionId = Lens.field @"actionId"
{-# DEPRECATED amaemActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | The friendly name of the mitigation action being applied by the task.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amaemActionName :: Lens.Lens' AuditMitigationActionExecutionMetadata (Core.Maybe Types.MitigationActionName)
amaemActionName = Lens.field @"actionName"
{-# DEPRECATED amaemActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | The date and time when the task was completed or canceled. Blank if the task is still running.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amaemEndTime :: Lens.Lens' AuditMitigationActionExecutionMetadata (Core.Maybe Core.NominalDiffTime)
amaemEndTime = Lens.field @"endTime"
{-# DEPRECATED amaemEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | If an error occurred, the code that indicates which type of error occurred.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amaemErrorCode :: Lens.Lens' AuditMitigationActionExecutionMetadata (Core.Maybe Types.ErrorCode)
amaemErrorCode = Lens.field @"errorCode"
{-# DEPRECATED amaemErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The unique identifier for the findings to which the task and associated mitigation action are applied.
--
-- /Note:/ Consider using 'findingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amaemFindingId :: Lens.Lens' AuditMitigationActionExecutionMetadata (Core.Maybe Types.FindingId)
amaemFindingId = Lens.field @"findingId"
{-# DEPRECATED amaemFindingId "Use generic-lens or generic-optics with 'findingId' instead." #-}

-- | If an error occurred, a message that describes the error.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amaemMessage :: Lens.Lens' AuditMitigationActionExecutionMetadata (Core.Maybe Types.ErrorMessage)
amaemMessage = Lens.field @"message"
{-# DEPRECATED amaemMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The date and time when the task was started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amaemStartTime :: Lens.Lens' AuditMitigationActionExecutionMetadata (Core.Maybe Core.NominalDiffTime)
amaemStartTime = Lens.field @"startTime"
{-# DEPRECATED amaemStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The current status of the task being executed.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amaemStatus :: Lens.Lens' AuditMitigationActionExecutionMetadata (Core.Maybe Types.AuditMitigationActionsExecutionStatus)
amaemStatus = Lens.field @"status"
{-# DEPRECATED amaemStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique identifier for the task that applies the mitigation action.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amaemTaskId :: Lens.Lens' AuditMitigationActionExecutionMetadata (Core.Maybe Types.AuditMitigationActionsTaskId)
amaemTaskId = Lens.field @"taskId"
{-# DEPRECATED amaemTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

instance Core.FromJSON AuditMitigationActionExecutionMetadata where
  parseJSON =
    Core.withObject "AuditMitigationActionExecutionMetadata" Core.$
      \x ->
        AuditMitigationActionExecutionMetadata'
          Core.<$> (x Core..:? "actionId")
          Core.<*> (x Core..:? "actionName")
          Core.<*> (x Core..:? "endTime")
          Core.<*> (x Core..:? "errorCode")
          Core.<*> (x Core..:? "findingId")
          Core.<*> (x Core..:? "message")
          Core.<*> (x Core..:? "startTime")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "taskId")
