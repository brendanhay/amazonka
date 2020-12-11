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
    amaemStatus,
    amaemStartTime,
    amaemTaskId,
    amaemActionId,
    amaemActionName,
    amaemEndTime,
    amaemErrorCode,
    amaemFindingId,
    amaemMessage,
  )
where

import Network.AWS.IoT.Types.AuditMitigationActionsExecutionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returned by ListAuditMitigationActionsTask, this object contains information that describes a mitigation action that has been started.
--
-- /See:/ 'mkAuditMitigationActionExecutionMetadata' smart constructor.
data AuditMitigationActionExecutionMetadata = AuditMitigationActionExecutionMetadata'
  { status ::
      Lude.Maybe
        AuditMitigationActionsExecutionStatus,
    startTime ::
      Lude.Maybe
        Lude.Timestamp,
    taskId ::
      Lude.Maybe
        Lude.Text,
    actionId ::
      Lude.Maybe
        Lude.Text,
    actionName ::
      Lude.Maybe
        Lude.Text,
    endTime ::
      Lude.Maybe
        Lude.Timestamp,
    errorCode ::
      Lude.Maybe
        Lude.Text,
    findingId ::
      Lude.Maybe
        Lude.Text,
    message ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuditMitigationActionExecutionMetadata' with the minimum fields required to make a request.
--
-- * 'actionId' - The unique identifier for the mitigation action being applied by the task.
-- * 'actionName' - The friendly name of the mitigation action being applied by the task.
-- * 'endTime' - The date and time when the task was completed or canceled. Blank if the task is still running.
-- * 'errorCode' - If an error occurred, the code that indicates which type of error occurred.
-- * 'findingId' - The unique identifier for the findings to which the task and associated mitigation action are applied.
-- * 'message' - If an error occurred, a message that describes the error.
-- * 'startTime' - The date and time when the task was started.
-- * 'status' - The current status of the task being executed.
-- * 'taskId' - The unique identifier for the task that applies the mitigation action.
mkAuditMitigationActionExecutionMetadata ::
  AuditMitigationActionExecutionMetadata
mkAuditMitigationActionExecutionMetadata =
  AuditMitigationActionExecutionMetadata'
    { status = Lude.Nothing,
      startTime = Lude.Nothing,
      taskId = Lude.Nothing,
      actionId = Lude.Nothing,
      actionName = Lude.Nothing,
      endTime = Lude.Nothing,
      errorCode = Lude.Nothing,
      findingId = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The current status of the task being executed.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amaemStatus :: Lens.Lens' AuditMitigationActionExecutionMetadata (Lude.Maybe AuditMitigationActionsExecutionStatus)
amaemStatus = Lens.lens (status :: AuditMitigationActionExecutionMetadata -> Lude.Maybe AuditMitigationActionsExecutionStatus) (\s a -> s {status = a} :: AuditMitigationActionExecutionMetadata)
{-# DEPRECATED amaemStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time when the task was started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amaemStartTime :: Lens.Lens' AuditMitigationActionExecutionMetadata (Lude.Maybe Lude.Timestamp)
amaemStartTime = Lens.lens (startTime :: AuditMitigationActionExecutionMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: AuditMitigationActionExecutionMetadata)
{-# DEPRECATED amaemStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The unique identifier for the task that applies the mitigation action.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amaemTaskId :: Lens.Lens' AuditMitigationActionExecutionMetadata (Lude.Maybe Lude.Text)
amaemTaskId = Lens.lens (taskId :: AuditMitigationActionExecutionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {taskId = a} :: AuditMitigationActionExecutionMetadata)
{-# DEPRECATED amaemTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | The unique identifier for the mitigation action being applied by the task.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amaemActionId :: Lens.Lens' AuditMitigationActionExecutionMetadata (Lude.Maybe Lude.Text)
amaemActionId = Lens.lens (actionId :: AuditMitigationActionExecutionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {actionId = a} :: AuditMitigationActionExecutionMetadata)
{-# DEPRECATED amaemActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | The friendly name of the mitigation action being applied by the task.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amaemActionName :: Lens.Lens' AuditMitigationActionExecutionMetadata (Lude.Maybe Lude.Text)
amaemActionName = Lens.lens (actionName :: AuditMitigationActionExecutionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {actionName = a} :: AuditMitigationActionExecutionMetadata)
{-# DEPRECATED amaemActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | The date and time when the task was completed or canceled. Blank if the task is still running.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amaemEndTime :: Lens.Lens' AuditMitigationActionExecutionMetadata (Lude.Maybe Lude.Timestamp)
amaemEndTime = Lens.lens (endTime :: AuditMitigationActionExecutionMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: AuditMitigationActionExecutionMetadata)
{-# DEPRECATED amaemEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | If an error occurred, the code that indicates which type of error occurred.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amaemErrorCode :: Lens.Lens' AuditMitigationActionExecutionMetadata (Lude.Maybe Lude.Text)
amaemErrorCode = Lens.lens (errorCode :: AuditMitigationActionExecutionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: AuditMitigationActionExecutionMetadata)
{-# DEPRECATED amaemErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The unique identifier for the findings to which the task and associated mitigation action are applied.
--
-- /Note:/ Consider using 'findingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amaemFindingId :: Lens.Lens' AuditMitigationActionExecutionMetadata (Lude.Maybe Lude.Text)
amaemFindingId = Lens.lens (findingId :: AuditMitigationActionExecutionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {findingId = a} :: AuditMitigationActionExecutionMetadata)
{-# DEPRECATED amaemFindingId "Use generic-lens or generic-optics with 'findingId' instead." #-}

-- | If an error occurred, a message that describes the error.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amaemMessage :: Lens.Lens' AuditMitigationActionExecutionMetadata (Lude.Maybe Lude.Text)
amaemMessage = Lens.lens (message :: AuditMitigationActionExecutionMetadata -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: AuditMitigationActionExecutionMetadata)
{-# DEPRECATED amaemMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON AuditMitigationActionExecutionMetadata where
  parseJSON =
    Lude.withObject
      "AuditMitigationActionExecutionMetadata"
      ( \x ->
          AuditMitigationActionExecutionMetadata'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "startTime")
            Lude.<*> (x Lude..:? "taskId")
            Lude.<*> (x Lude..:? "actionId")
            Lude.<*> (x Lude..:? "actionName")
            Lude.<*> (x Lude..:? "endTime")
            Lude.<*> (x Lude..:? "errorCode")
            Lude.<*> (x Lude..:? "findingId")
            Lude.<*> (x Lude..:? "message")
      )
