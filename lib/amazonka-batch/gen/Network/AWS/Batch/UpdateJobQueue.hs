{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.UpdateJobQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a job queue.
module Network.AWS.Batch.UpdateJobQueue
  ( -- * Creating a request
    UpdateJobQueue (..),
    mkUpdateJobQueue,

    -- ** Request lenses
    ujqState,
    ujqPriority,
    ujqComputeEnvironmentOrder,
    ujqJobQueue,

    -- * Destructuring the response
    UpdateJobQueueResponse (..),
    mkUpdateJobQueueResponse,

    -- ** Response lenses
    ujqrsJobQueueARN,
    ujqrsJobQueueName,
    ujqrsResponseStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateJobQueue' smart constructor.
data UpdateJobQueue = UpdateJobQueue'
  { state :: Lude.Maybe JQState,
    priority :: Lude.Maybe Lude.Int,
    computeEnvironmentOrder ::
      Lude.Maybe [ComputeEnvironmentOrder],
    jobQueue :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateJobQueue' with the minimum fields required to make a request.
--
-- * 'computeEnvironmentOrder' - Details the set of compute environments mapped to a job queue and their order relative to each other. This is one of the parameters used by the job scheduler to determine which compute environment should execute a given job.
-- * 'jobQueue' - The name or the Amazon Resource Name (ARN) of the job queue.
-- * 'priority' - The priority of the job queue. Job queues with a higher priority (or a higher integer value for the @priority@ parameter) are evaluated first when associated with the same compute environment. Priority is determined in descending order, for example, a job queue with a priority value of @10@ is given scheduling preference over a job queue with a priority value of @1@ .
-- * 'state' - Describes the queue's ability to accept new jobs. If the job queue state is @ENABLED@ , it is able to accept jobs. If the job queue state is @DISABLED@ , new jobs cannot be added to the queue, but jobs already in the queue can finish.
mkUpdateJobQueue ::
  -- | 'jobQueue'
  Lude.Text ->
  UpdateJobQueue
mkUpdateJobQueue pJobQueue_ =
  UpdateJobQueue'
    { state = Lude.Nothing,
      priority = Lude.Nothing,
      computeEnvironmentOrder = Lude.Nothing,
      jobQueue = pJobQueue_
    }

-- | Describes the queue's ability to accept new jobs. If the job queue state is @ENABLED@ , it is able to accept jobs. If the job queue state is @DISABLED@ , new jobs cannot be added to the queue, but jobs already in the queue can finish.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqState :: Lens.Lens' UpdateJobQueue (Lude.Maybe JQState)
ujqState = Lens.lens (state :: UpdateJobQueue -> Lude.Maybe JQState) (\s a -> s {state = a} :: UpdateJobQueue)
{-# DEPRECATED ujqState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The priority of the job queue. Job queues with a higher priority (or a higher integer value for the @priority@ parameter) are evaluated first when associated with the same compute environment. Priority is determined in descending order, for example, a job queue with a priority value of @10@ is given scheduling preference over a job queue with a priority value of @1@ .
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqPriority :: Lens.Lens' UpdateJobQueue (Lude.Maybe Lude.Int)
ujqPriority = Lens.lens (priority :: UpdateJobQueue -> Lude.Maybe Lude.Int) (\s a -> s {priority = a} :: UpdateJobQueue)
{-# DEPRECATED ujqPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | Details the set of compute environments mapped to a job queue and their order relative to each other. This is one of the parameters used by the job scheduler to determine which compute environment should execute a given job.
--
-- /Note:/ Consider using 'computeEnvironmentOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqComputeEnvironmentOrder :: Lens.Lens' UpdateJobQueue (Lude.Maybe [ComputeEnvironmentOrder])
ujqComputeEnvironmentOrder = Lens.lens (computeEnvironmentOrder :: UpdateJobQueue -> Lude.Maybe [ComputeEnvironmentOrder]) (\s a -> s {computeEnvironmentOrder = a} :: UpdateJobQueue)
{-# DEPRECATED ujqComputeEnvironmentOrder "Use generic-lens or generic-optics with 'computeEnvironmentOrder' instead." #-}

-- | The name or the Amazon Resource Name (ARN) of the job queue.
--
-- /Note:/ Consider using 'jobQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqJobQueue :: Lens.Lens' UpdateJobQueue Lude.Text
ujqJobQueue = Lens.lens (jobQueue :: UpdateJobQueue -> Lude.Text) (\s a -> s {jobQueue = a} :: UpdateJobQueue)
{-# DEPRECATED ujqJobQueue "Use generic-lens or generic-optics with 'jobQueue' instead." #-}

instance Lude.AWSRequest UpdateJobQueue where
  type Rs UpdateJobQueue = UpdateJobQueueResponse
  request = Req.postJSON batchService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateJobQueueResponse'
            Lude.<$> (x Lude..?> "jobQueueArn")
            Lude.<*> (x Lude..?> "jobQueueName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateJobQueue where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateJobQueue where
  toJSON UpdateJobQueue' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("state" Lude..=) Lude.<$> state,
            ("priority" Lude..=) Lude.<$> priority,
            ("computeEnvironmentOrder" Lude..=)
              Lude.<$> computeEnvironmentOrder,
            Lude.Just ("jobQueue" Lude..= jobQueue)
          ]
      )

instance Lude.ToPath UpdateJobQueue where
  toPath = Lude.const "/v1/updatejobqueue"

instance Lude.ToQuery UpdateJobQueue where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateJobQueueResponse' smart constructor.
data UpdateJobQueueResponse = UpdateJobQueueResponse'
  { jobQueueARN ::
      Lude.Maybe Lude.Text,
    jobQueueName :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateJobQueueResponse' with the minimum fields required to make a request.
--
-- * 'jobQueueARN' - The Amazon Resource Name (ARN) of the job queue.
-- * 'jobQueueName' - The name of the job queue.
-- * 'responseStatus' - The response status code.
mkUpdateJobQueueResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateJobQueueResponse
mkUpdateJobQueueResponse pResponseStatus_ =
  UpdateJobQueueResponse'
    { jobQueueARN = Lude.Nothing,
      jobQueueName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the job queue.
--
-- /Note:/ Consider using 'jobQueueARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqrsJobQueueARN :: Lens.Lens' UpdateJobQueueResponse (Lude.Maybe Lude.Text)
ujqrsJobQueueARN = Lens.lens (jobQueueARN :: UpdateJobQueueResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobQueueARN = a} :: UpdateJobQueueResponse)
{-# DEPRECATED ujqrsJobQueueARN "Use generic-lens or generic-optics with 'jobQueueARN' instead." #-}

-- | The name of the job queue.
--
-- /Note:/ Consider using 'jobQueueName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqrsJobQueueName :: Lens.Lens' UpdateJobQueueResponse (Lude.Maybe Lude.Text)
ujqrsJobQueueName = Lens.lens (jobQueueName :: UpdateJobQueueResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobQueueName = a} :: UpdateJobQueueResponse)
{-# DEPRECATED ujqrsJobQueueName "Use generic-lens or generic-optics with 'jobQueueName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqrsResponseStatus :: Lens.Lens' UpdateJobQueueResponse Lude.Int
ujqrsResponseStatus = Lens.lens (responseStatus :: UpdateJobQueueResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateJobQueueResponse)
{-# DEPRECATED ujqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
