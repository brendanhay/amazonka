{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.StartAuditMitigationActionsTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a task that applies a set of mitigation actions to the specified target.
module Network.AWS.IoT.StartAuditMitigationActionsTask
  ( -- * Creating a request
    StartAuditMitigationActionsTask (..),
    mkStartAuditMitigationActionsTask,

    -- ** Request lenses
    samatTaskId,
    samatTarget,
    samatAuditCheckToActionsMapping,
    samatClientRequestToken,

    -- * Destructuring the response
    StartAuditMitigationActionsTaskResponse (..),
    mkStartAuditMitigationActionsTaskResponse,

    -- ** Response lenses
    samatrsTaskId,
    samatrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartAuditMitigationActionsTask' smart constructor.
data StartAuditMitigationActionsTask = StartAuditMitigationActionsTask'
  { taskId ::
      Lude.Text,
    target ::
      AuditMitigationActionsTaskTarget,
    auditCheckToActionsMapping ::
      Lude.HashMap
        Lude.Text
        (Lude.NonEmpty Lude.Text),
    clientRequestToken ::
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

-- | Creates a value of 'StartAuditMitigationActionsTask' with the minimum fields required to make a request.
--
-- * 'auditCheckToActionsMapping' - For an audit check, specifies which mitigation actions to apply. Those actions must be defined in your AWS account.
-- * 'clientRequestToken' - Each audit mitigation task must have a unique client request token. If you try to start a new task with the same token as a task that already exists, an exception occurs. If you omit this value, a unique client request token is generated automatically.
-- * 'target' - Specifies the audit findings to which the mitigation actions are applied. You can apply them to a type of audit check, to all findings from an audit, or to a speecific set of findings.
-- * 'taskId' - A unique identifier for the task. You can use this identifier to check the status of the task or to cancel it.
mkStartAuditMitigationActionsTask ::
  -- | 'taskId'
  Lude.Text ->
  -- | 'target'
  AuditMitigationActionsTaskTarget ->
  -- | 'clientRequestToken'
  Lude.Text ->
  StartAuditMitigationActionsTask
mkStartAuditMitigationActionsTask
  pTaskId_
  pTarget_
  pClientRequestToken_ =
    StartAuditMitigationActionsTask'
      { taskId = pTaskId_,
        target = pTarget_,
        auditCheckToActionsMapping = Lude.mempty,
        clientRequestToken = pClientRequestToken_
      }

-- | A unique identifier for the task. You can use this identifier to check the status of the task or to cancel it.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samatTaskId :: Lens.Lens' StartAuditMitigationActionsTask Lude.Text
samatTaskId = Lens.lens (taskId :: StartAuditMitigationActionsTask -> Lude.Text) (\s a -> s {taskId = a} :: StartAuditMitigationActionsTask)
{-# DEPRECATED samatTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | Specifies the audit findings to which the mitigation actions are applied. You can apply them to a type of audit check, to all findings from an audit, or to a speecific set of findings.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samatTarget :: Lens.Lens' StartAuditMitigationActionsTask AuditMitigationActionsTaskTarget
samatTarget = Lens.lens (target :: StartAuditMitigationActionsTask -> AuditMitigationActionsTaskTarget) (\s a -> s {target = a} :: StartAuditMitigationActionsTask)
{-# DEPRECATED samatTarget "Use generic-lens or generic-optics with 'target' instead." #-}

-- | For an audit check, specifies which mitigation actions to apply. Those actions must be defined in your AWS account.
--
-- /Note:/ Consider using 'auditCheckToActionsMapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samatAuditCheckToActionsMapping :: Lens.Lens' StartAuditMitigationActionsTask (Lude.HashMap Lude.Text (Lude.NonEmpty Lude.Text))
samatAuditCheckToActionsMapping = Lens.lens (auditCheckToActionsMapping :: StartAuditMitigationActionsTask -> Lude.HashMap Lude.Text (Lude.NonEmpty Lude.Text)) (\s a -> s {auditCheckToActionsMapping = a} :: StartAuditMitigationActionsTask)
{-# DEPRECATED samatAuditCheckToActionsMapping "Use generic-lens or generic-optics with 'auditCheckToActionsMapping' instead." #-}

-- | Each audit mitigation task must have a unique client request token. If you try to start a new task with the same token as a task that already exists, an exception occurs. If you omit this value, a unique client request token is generated automatically.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samatClientRequestToken :: Lens.Lens' StartAuditMitigationActionsTask Lude.Text
samatClientRequestToken = Lens.lens (clientRequestToken :: StartAuditMitigationActionsTask -> Lude.Text) (\s a -> s {clientRequestToken = a} :: StartAuditMitigationActionsTask)
{-# DEPRECATED samatClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

instance Lude.AWSRequest StartAuditMitigationActionsTask where
  type
    Rs StartAuditMitigationActionsTask =
      StartAuditMitigationActionsTaskResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartAuditMitigationActionsTaskResponse'
            Lude.<$> (x Lude..?> "taskId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartAuditMitigationActionsTask where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON StartAuditMitigationActionsTask where
  toJSON StartAuditMitigationActionsTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("target" Lude..= target),
            Lude.Just
              ("auditCheckToActionsMapping" Lude..= auditCheckToActionsMapping),
            Lude.Just ("clientRequestToken" Lude..= clientRequestToken)
          ]
      )

instance Lude.ToPath StartAuditMitigationActionsTask where
  toPath StartAuditMitigationActionsTask' {..} =
    Lude.mconcat
      ["/audit/mitigationactions/tasks/", Lude.toBS taskId]

instance Lude.ToQuery StartAuditMitigationActionsTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartAuditMitigationActionsTaskResponse' smart constructor.
data StartAuditMitigationActionsTaskResponse = StartAuditMitigationActionsTaskResponse'
  { taskId ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartAuditMitigationActionsTaskResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'taskId' - The unique identifier for the audit mitigation task. This matches the @taskId@ that you specified in the request.
mkStartAuditMitigationActionsTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartAuditMitigationActionsTaskResponse
mkStartAuditMitigationActionsTaskResponse pResponseStatus_ =
  StartAuditMitigationActionsTaskResponse'
    { taskId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier for the audit mitigation task. This matches the @taskId@ that you specified in the request.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samatrsTaskId :: Lens.Lens' StartAuditMitigationActionsTaskResponse (Lude.Maybe Lude.Text)
samatrsTaskId = Lens.lens (taskId :: StartAuditMitigationActionsTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {taskId = a} :: StartAuditMitigationActionsTaskResponse)
{-# DEPRECATED samatrsTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samatrsResponseStatus :: Lens.Lens' StartAuditMitigationActionsTaskResponse Lude.Int
samatrsResponseStatus = Lens.lens (responseStatus :: StartAuditMitigationActionsTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartAuditMitigationActionsTaskResponse)
{-# DEPRECATED samatrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
