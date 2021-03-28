{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateJobQueue (..)
    , mkUpdateJobQueue
    -- ** Request lenses
    , ujqJobQueue
    , ujqComputeEnvironmentOrder
    , ujqPriority
    , ujqState

    -- * Destructuring the response
    , UpdateJobQueueResponse (..)
    , mkUpdateJobQueueResponse
    -- ** Response lenses
    , ujqrrsJobQueueArn
    , ujqrrsJobQueueName
    , ujqrrsResponseStatus
    ) where

import qualified Network.AWS.Batch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateJobQueue' smart constructor.
data UpdateJobQueue = UpdateJobQueue'
  { jobQueue :: Core.Text
    -- ^ The name or the Amazon Resource Name (ARN) of the job queue.
  , computeEnvironmentOrder :: Core.Maybe [Types.ComputeEnvironmentOrder]
    -- ^ Details the set of compute environments mapped to a job queue and their order relative to each other. This is one of the parameters used by the job scheduler to determine which compute environment should execute a given job.
  , priority :: Core.Maybe Core.Int
    -- ^ The priority of the job queue. Job queues with a higher priority (or a higher integer value for the @priority@ parameter) are evaluated first when associated with the same compute environment. Priority is determined in descending order, for example, a job queue with a priority value of @10@ is given scheduling preference over a job queue with a priority value of @1@ .
  , state :: Core.Maybe Types.JQState
    -- ^ Describes the queue's ability to accept new jobs. If the job queue state is @ENABLED@ , it is able to accept jobs. If the job queue state is @DISABLED@ , new jobs cannot be added to the queue, but jobs already in the queue can finish.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJobQueue' value with any optional fields omitted.
mkUpdateJobQueue
    :: Core.Text -- ^ 'jobQueue'
    -> UpdateJobQueue
mkUpdateJobQueue jobQueue
  = UpdateJobQueue'{jobQueue, computeEnvironmentOrder = Core.Nothing,
                    priority = Core.Nothing, state = Core.Nothing}

-- | The name or the Amazon Resource Name (ARN) of the job queue.
--
-- /Note:/ Consider using 'jobQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqJobQueue :: Lens.Lens' UpdateJobQueue Core.Text
ujqJobQueue = Lens.field @"jobQueue"
{-# INLINEABLE ujqJobQueue #-}
{-# DEPRECATED jobQueue "Use generic-lens or generic-optics with 'jobQueue' instead"  #-}

-- | Details the set of compute environments mapped to a job queue and their order relative to each other. This is one of the parameters used by the job scheduler to determine which compute environment should execute a given job.
--
-- /Note:/ Consider using 'computeEnvironmentOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqComputeEnvironmentOrder :: Lens.Lens' UpdateJobQueue (Core.Maybe [Types.ComputeEnvironmentOrder])
ujqComputeEnvironmentOrder = Lens.field @"computeEnvironmentOrder"
{-# INLINEABLE ujqComputeEnvironmentOrder #-}
{-# DEPRECATED computeEnvironmentOrder "Use generic-lens or generic-optics with 'computeEnvironmentOrder' instead"  #-}

-- | The priority of the job queue. Job queues with a higher priority (or a higher integer value for the @priority@ parameter) are evaluated first when associated with the same compute environment. Priority is determined in descending order, for example, a job queue with a priority value of @10@ is given scheduling preference over a job queue with a priority value of @1@ .
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqPriority :: Lens.Lens' UpdateJobQueue (Core.Maybe Core.Int)
ujqPriority = Lens.field @"priority"
{-# INLINEABLE ujqPriority #-}
{-# DEPRECATED priority "Use generic-lens or generic-optics with 'priority' instead"  #-}

-- | Describes the queue's ability to accept new jobs. If the job queue state is @ENABLED@ , it is able to accept jobs. If the job queue state is @DISABLED@ , new jobs cannot be added to the queue, but jobs already in the queue can finish.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqState :: Lens.Lens' UpdateJobQueue (Core.Maybe Types.JQState)
ujqState = Lens.field @"state"
{-# INLINEABLE ujqState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.ToQuery UpdateJobQueue where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateJobQueue where
        toHeaders UpdateJobQueue{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateJobQueue where
        toJSON UpdateJobQueue{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("jobQueue" Core..= jobQueue),
                  ("computeEnvironmentOrder" Core..=) Core.<$>
                    computeEnvironmentOrder,
                  ("priority" Core..=) Core.<$> priority,
                  ("state" Core..=) Core.<$> state])

instance Core.AWSRequest UpdateJobQueue where
        type Rs UpdateJobQueue = UpdateJobQueueResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/v1/updatejobqueue",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateJobQueueResponse' Core.<$>
                   (x Core..:? "jobQueueArn") Core.<*> x Core..:? "jobQueueName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateJobQueueResponse' smart constructor.
data UpdateJobQueueResponse = UpdateJobQueueResponse'
  { jobQueueArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the job queue.
  , jobQueueName :: Core.Maybe Core.Text
    -- ^ The name of the job queue.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJobQueueResponse' value with any optional fields omitted.
mkUpdateJobQueueResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateJobQueueResponse
mkUpdateJobQueueResponse responseStatus
  = UpdateJobQueueResponse'{jobQueueArn = Core.Nothing,
                            jobQueueName = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the job queue.
--
-- /Note:/ Consider using 'jobQueueArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqrrsJobQueueArn :: Lens.Lens' UpdateJobQueueResponse (Core.Maybe Core.Text)
ujqrrsJobQueueArn = Lens.field @"jobQueueArn"
{-# INLINEABLE ujqrrsJobQueueArn #-}
{-# DEPRECATED jobQueueArn "Use generic-lens or generic-optics with 'jobQueueArn' instead"  #-}

-- | The name of the job queue.
--
-- /Note:/ Consider using 'jobQueueName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqrrsJobQueueName :: Lens.Lens' UpdateJobQueueResponse (Core.Maybe Core.Text)
ujqrrsJobQueueName = Lens.field @"jobQueueName"
{-# INLINEABLE ujqrrsJobQueueName #-}
{-# DEPRECATED jobQueueName "Use generic-lens or generic-optics with 'jobQueueName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujqrrsResponseStatus :: Lens.Lens' UpdateJobQueueResponse Core.Int
ujqrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ujqrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
