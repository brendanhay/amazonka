{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    ujqJobQueue,
    ujqComputeEnvironmentOrder,
    ujqPriority,
    ujqState,

    -- * Destructuring the response
    UpdateJobQueueResponse (..),
    mkUpdateJobQueueResponse,

    -- ** Response lenses
    ujqrrsJobQueueArn,
    ujqrrsJobQueueName,
    ujqrrsResponseStatus,
  )
where

import qualified Network.AWS.Batch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateJobQueue' smart constructor.
data UpdateJobQueue = UpdateJobQueue'
  { -- | The name or the Amazon Resource Name (ARN) of the job queue.
    jobQueue :: Types.String,
    -- | Details the set of compute environments mapped to a job queue and their order relative to each other. This is one of the parameters used by the job scheduler to determine which compute environment should execute a given job.
    computeEnvironmentOrder :: Core.Maybe [Types.ComputeEnvironmentOrder],
    -- | The priority of the job queue. Job queues with a higher priority (or a higher integer value for the @priority@ parameter) are evaluated first when associated with the same compute environment. Priority is determined in descending order, for example, a job queue with a priority value of @10@ is given scheduling preference over a job queue with a priority value of @1@ .
    priority :: Core.Maybe Core.Int,
    -- | Describes the queue's ability to accept new jobs. If the job queue state is @ENABLED@ , it is able to accept jobs. If the job queue state is @DISABLED@ , new jobs cannot be added to the queue, but jobs already in the queue can finish.
    state :: Core.Maybe Types.JQState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJobQueue' value with any optional fields omitted.
mkUpdateJobQueue ::
  -- | 'jobQueue'
  Types.String ->
  UpdateJobQueue
mkUpdateJobQueue jobQueue =
  UpdateJobQueue'
    { jobQueue,
      computeEnvironmentOrder = Core.Nothing,
      priority = Core.Nothing,
      state = Core.Nothing
    }

-- | The name or the Amazon Resource Name (ARN) of the job queue.
--
-- /Note:/ Consider using 'jobQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqJobQueue :: Lens.Lens' UpdateJobQueue Types.String
ujqJobQueue = Lens.field @"jobQueue"
{-# DEPRECATED ujqJobQueue "Use generic-lens or generic-optics with 'jobQueue' instead." #-}

-- | Details the set of compute environments mapped to a job queue and their order relative to each other. This is one of the parameters used by the job scheduler to determine which compute environment should execute a given job.
--
-- /Note:/ Consider using 'computeEnvironmentOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqComputeEnvironmentOrder :: Lens.Lens' UpdateJobQueue (Core.Maybe [Types.ComputeEnvironmentOrder])
ujqComputeEnvironmentOrder = Lens.field @"computeEnvironmentOrder"
{-# DEPRECATED ujqComputeEnvironmentOrder "Use generic-lens or generic-optics with 'computeEnvironmentOrder' instead." #-}

-- | The priority of the job queue. Job queues with a higher priority (or a higher integer value for the @priority@ parameter) are evaluated first when associated with the same compute environment. Priority is determined in descending order, for example, a job queue with a priority value of @10@ is given scheduling preference over a job queue with a priority value of @1@ .
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqPriority :: Lens.Lens' UpdateJobQueue (Core.Maybe Core.Int)
ujqPriority = Lens.field @"priority"
{-# DEPRECATED ujqPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | Describes the queue's ability to accept new jobs. If the job queue state is @ENABLED@ , it is able to accept jobs. If the job queue state is @DISABLED@ , new jobs cannot be added to the queue, but jobs already in the queue can finish.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqState :: Lens.Lens' UpdateJobQueue (Core.Maybe Types.JQState)
ujqState = Lens.field @"state"
{-# DEPRECATED ujqState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromJSON UpdateJobQueue where
  toJSON UpdateJobQueue {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("jobQueue" Core..= jobQueue),
            ("computeEnvironmentOrder" Core..=)
              Core.<$> computeEnvironmentOrder,
            ("priority" Core..=) Core.<$> priority,
            ("state" Core..=) Core.<$> state
          ]
      )

instance Core.AWSRequest UpdateJobQueue where
  type Rs UpdateJobQueue = UpdateJobQueueResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/v1/updatejobqueue",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateJobQueueResponse'
            Core.<$> (x Core..:? "jobQueueArn")
            Core.<*> (x Core..:? "jobQueueName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateJobQueueResponse' smart constructor.
data UpdateJobQueueResponse = UpdateJobQueueResponse'
  { -- | The Amazon Resource Name (ARN) of the job queue.
    jobQueueArn :: Core.Maybe Types.String,
    -- | The name of the job queue.
    jobQueueName :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJobQueueResponse' value with any optional fields omitted.
mkUpdateJobQueueResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateJobQueueResponse
mkUpdateJobQueueResponse responseStatus =
  UpdateJobQueueResponse'
    { jobQueueArn = Core.Nothing,
      jobQueueName = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the job queue.
--
-- /Note:/ Consider using 'jobQueueArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqrrsJobQueueArn :: Lens.Lens' UpdateJobQueueResponse (Core.Maybe Types.String)
ujqrrsJobQueueArn = Lens.field @"jobQueueArn"
{-# DEPRECATED ujqrrsJobQueueArn "Use generic-lens or generic-optics with 'jobQueueArn' instead." #-}

-- | The name of the job queue.
--
-- /Note:/ Consider using 'jobQueueName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqrrsJobQueueName :: Lens.Lens' UpdateJobQueueResponse (Core.Maybe Types.String)
ujqrrsJobQueueName = Lens.field @"jobQueueName"
{-# DEPRECATED ujqrrsJobQueueName "Use generic-lens or generic-optics with 'jobQueueName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqrrsResponseStatus :: Lens.Lens' UpdateJobQueueResponse Core.Int
ujqrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ujqrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
