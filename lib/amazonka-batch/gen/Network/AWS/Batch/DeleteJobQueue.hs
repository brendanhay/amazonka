{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteJobQueue (..)
    , mkDeleteJobQueue
    -- ** Request lenses
    , djqJobQueue

    -- * Destructuring the response
    , DeleteJobQueueResponse (..)
    , mkDeleteJobQueueResponse
    -- ** Response lenses
    , djqrrsResponseStatus
    ) where

import qualified Network.AWS.Batch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteJobQueue' smart constructor.
newtype DeleteJobQueue = DeleteJobQueue'
  { jobQueue :: Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the queue to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteJobQueue' value with any optional fields omitted.
mkDeleteJobQueue
    :: Core.Text -- ^ 'jobQueue'
    -> DeleteJobQueue
mkDeleteJobQueue jobQueue = DeleteJobQueue'{jobQueue}

-- | The short name or full Amazon Resource Name (ARN) of the queue to delete.
--
-- /Note:/ Consider using 'jobQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djqJobQueue :: Lens.Lens' DeleteJobQueue Core.Text
djqJobQueue = Lens.field @"jobQueue"
{-# INLINEABLE djqJobQueue #-}
{-# DEPRECATED jobQueue "Use generic-lens or generic-optics with 'jobQueue' instead"  #-}

instance Core.ToQuery DeleteJobQueue where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteJobQueue where
        toHeaders DeleteJobQueue{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteJobQueue where
        toJSON DeleteJobQueue{..}
          = Core.object
              (Core.catMaybes [Core.Just ("jobQueue" Core..= jobQueue)])

instance Core.AWSRequest DeleteJobQueue where
        type Rs DeleteJobQueue = DeleteJobQueueResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/v1/deletejobqueue",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteJobQueueResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteJobQueueResponse' smart constructor.
newtype DeleteJobQueueResponse = DeleteJobQueueResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteJobQueueResponse' value with any optional fields omitted.
mkDeleteJobQueueResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteJobQueueResponse
mkDeleteJobQueueResponse responseStatus
  = DeleteJobQueueResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djqrrsResponseStatus :: Lens.Lens' DeleteJobQueueResponse Core.Int
djqrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE djqrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
