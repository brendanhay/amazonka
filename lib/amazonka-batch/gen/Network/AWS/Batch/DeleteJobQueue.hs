{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.DeleteJobQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified job queue. You must first disable submissions for a queue with the 'UpdateJobQueue' operation. All jobs in the queue are terminated when you delete a job queue.
--
-- It is not necessary to disassociate compute environments from a queue before submitting a @DeleteJobQueue@ request.
module Network.AWS.Batch.DeleteJobQueue
  ( -- * Creating a request
    DeleteJobQueue (..),
    mkDeleteJobQueue,

    -- ** Request lenses
    djqJobQueue,

    -- * Destructuring the response
    DeleteJobQueueResponse (..),
    mkDeleteJobQueueResponse,

    -- ** Response lenses
    djqrsResponseStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteJobQueue' smart constructor.
newtype DeleteJobQueue = DeleteJobQueue'
  { -- | The short name or full Amazon Resource Name (ARN) of the queue to delete.
    jobQueue :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteJobQueue' with the minimum fields required to make a request.
--
-- * 'jobQueue' - The short name or full Amazon Resource Name (ARN) of the queue to delete.
mkDeleteJobQueue ::
  -- | 'jobQueue'
  Lude.Text ->
  DeleteJobQueue
mkDeleteJobQueue pJobQueue_ =
  DeleteJobQueue' {jobQueue = pJobQueue_}

-- | The short name or full Amazon Resource Name (ARN) of the queue to delete.
--
-- /Note:/ Consider using 'jobQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djqJobQueue :: Lens.Lens' DeleteJobQueue Lude.Text
djqJobQueue = Lens.lens (jobQueue :: DeleteJobQueue -> Lude.Text) (\s a -> s {jobQueue = a} :: DeleteJobQueue)
{-# DEPRECATED djqJobQueue "Use generic-lens or generic-optics with 'jobQueue' instead." #-}

instance Lude.AWSRequest DeleteJobQueue where
  type Rs DeleteJobQueue = DeleteJobQueueResponse
  request = Req.postJSON batchService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteJobQueueResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteJobQueue where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteJobQueue where
  toJSON DeleteJobQueue' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("jobQueue" Lude..= jobQueue)])

instance Lude.ToPath DeleteJobQueue where
  toPath = Lude.const "/v1/deletejobqueue"

instance Lude.ToQuery DeleteJobQueue where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteJobQueueResponse' smart constructor.
newtype DeleteJobQueueResponse = DeleteJobQueueResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteJobQueueResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteJobQueueResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteJobQueueResponse
mkDeleteJobQueueResponse pResponseStatus_ =
  DeleteJobQueueResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djqrsResponseStatus :: Lens.Lens' DeleteJobQueueResponse Lude.Int
djqrsResponseStatus = Lens.lens (responseStatus :: DeleteJobQueueResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteJobQueueResponse)
{-# DEPRECATED djqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
