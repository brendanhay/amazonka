{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CancelAuditMitigationActionsTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a mitigation action task that is in progress. If the task is not in progress, an InvalidRequestException occurs.
module Network.AWS.IoT.CancelAuditMitigationActionsTask
  ( -- * Creating a request
    CancelAuditMitigationActionsTask (..),
    mkCancelAuditMitigationActionsTask,

    -- ** Request lenses
    camatTaskId,

    -- * Destructuring the response
    CancelAuditMitigationActionsTaskResponse (..),
    mkCancelAuditMitigationActionsTaskResponse,

    -- ** Response lenses
    camatrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCancelAuditMitigationActionsTask' smart constructor.
newtype CancelAuditMitigationActionsTask = CancelAuditMitigationActionsTask'
  { -- | The unique identifier for the task that you want to cancel.
    taskId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelAuditMitigationActionsTask' with the minimum fields required to make a request.
--
-- * 'taskId' - The unique identifier for the task that you want to cancel.
mkCancelAuditMitigationActionsTask ::
  -- | 'taskId'
  Lude.Text ->
  CancelAuditMitigationActionsTask
mkCancelAuditMitigationActionsTask pTaskId_ =
  CancelAuditMitigationActionsTask' {taskId = pTaskId_}

-- | The unique identifier for the task that you want to cancel.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camatTaskId :: Lens.Lens' CancelAuditMitigationActionsTask Lude.Text
camatTaskId = Lens.lens (taskId :: CancelAuditMitigationActionsTask -> Lude.Text) (\s a -> s {taskId = a} :: CancelAuditMitigationActionsTask)
{-# DEPRECATED camatTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

instance Lude.AWSRequest CancelAuditMitigationActionsTask where
  type
    Rs CancelAuditMitigationActionsTask =
      CancelAuditMitigationActionsTaskResponse
  request = Req.putJSON ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CancelAuditMitigationActionsTaskResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelAuditMitigationActionsTask where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CancelAuditMitigationActionsTask where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath CancelAuditMitigationActionsTask where
  toPath CancelAuditMitigationActionsTask' {..} =
    Lude.mconcat
      ["/audit/mitigationactions/tasks/", Lude.toBS taskId, "/cancel"]

instance Lude.ToQuery CancelAuditMitigationActionsTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCancelAuditMitigationActionsTaskResponse' smart constructor.
newtype CancelAuditMitigationActionsTaskResponse = CancelAuditMitigationActionsTaskResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelAuditMitigationActionsTaskResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCancelAuditMitigationActionsTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelAuditMitigationActionsTaskResponse
mkCancelAuditMitigationActionsTaskResponse pResponseStatus_ =
  CancelAuditMitigationActionsTaskResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camatrsResponseStatus :: Lens.Lens' CancelAuditMitigationActionsTaskResponse Lude.Int
camatrsResponseStatus = Lens.lens (responseStatus :: CancelAuditMitigationActionsTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelAuditMitigationActionsTaskResponse)
{-# DEPRECATED camatrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
