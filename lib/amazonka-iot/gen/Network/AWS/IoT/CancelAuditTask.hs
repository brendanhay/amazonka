{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CancelAuditTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an audit that is in progress. The audit can be either scheduled or on-demand. If the audit is not in progress, an "InvalidRequestException" occurs.
module Network.AWS.IoT.CancelAuditTask
  ( -- * Creating a request
    CancelAuditTask (..),
    mkCancelAuditTask,

    -- ** Request lenses
    catTaskId,

    -- * Destructuring the response
    CancelAuditTaskResponse (..),
    mkCancelAuditTaskResponse,

    -- ** Response lenses
    catrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCancelAuditTask' smart constructor.
newtype CancelAuditTask = CancelAuditTask'
  { -- | The ID of the audit you want to cancel. You can only cancel an audit that is "IN_PROGRESS".
    taskId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelAuditTask' with the minimum fields required to make a request.
--
-- * 'taskId' - The ID of the audit you want to cancel. You can only cancel an audit that is "IN_PROGRESS".
mkCancelAuditTask ::
  -- | 'taskId'
  Lude.Text ->
  CancelAuditTask
mkCancelAuditTask pTaskId_ = CancelAuditTask' {taskId = pTaskId_}

-- | The ID of the audit you want to cancel. You can only cancel an audit that is "IN_PROGRESS".
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
catTaskId :: Lens.Lens' CancelAuditTask Lude.Text
catTaskId = Lens.lens (taskId :: CancelAuditTask -> Lude.Text) (\s a -> s {taskId = a} :: CancelAuditTask)
{-# DEPRECATED catTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

instance Lude.AWSRequest CancelAuditTask where
  type Rs CancelAuditTask = CancelAuditTaskResponse
  request = Req.putJSON ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CancelAuditTaskResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelAuditTask where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CancelAuditTask where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath CancelAuditTask where
  toPath CancelAuditTask' {..} =
    Lude.mconcat ["/audit/tasks/", Lude.toBS taskId, "/cancel"]

instance Lude.ToQuery CancelAuditTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCancelAuditTaskResponse' smart constructor.
newtype CancelAuditTaskResponse = CancelAuditTaskResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelAuditTaskResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCancelAuditTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelAuditTaskResponse
mkCancelAuditTaskResponse pResponseStatus_ =
  CancelAuditTaskResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
catrsResponseStatus :: Lens.Lens' CancelAuditTaskResponse Lude.Int
catrsResponseStatus = Lens.lens (responseStatus :: CancelAuditTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelAuditTaskResponse)
{-# DEPRECATED catrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
