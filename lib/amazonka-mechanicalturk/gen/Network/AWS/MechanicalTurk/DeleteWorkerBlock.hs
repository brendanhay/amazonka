{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.DeleteWorkerBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteWorkerBlock@ operation allows you to reinstate a blocked Worker to work on your HITs. This operation reverses the effects of the CreateWorkerBlock operation. You need the Worker ID to use this operation. If the Worker ID is missing or invalid, this operation fails and returns the message “WorkerId is invalid.” If the specified Worker is not blocked, this operation returns successfully.
module Network.AWS.MechanicalTurk.DeleteWorkerBlock
    (
    -- * Creating a request
      DeleteWorkerBlock (..)
    , mkDeleteWorkerBlock
    -- ** Request lenses
    , dwbWorkerId
    , dwbReason

    -- * Destructuring the response
    , DeleteWorkerBlockResponse (..)
    , mkDeleteWorkerBlockResponse
    -- ** Response lenses
    , dwbrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteWorkerBlock' smart constructor.
data DeleteWorkerBlock = DeleteWorkerBlock'
  { workerId :: Types.CustomerId
    -- ^ The ID of the Worker to unblock.
  , reason :: Core.Maybe Core.Text
    -- ^ A message that explains the reason for unblocking the Worker. The Worker does not see this message.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWorkerBlock' value with any optional fields omitted.
mkDeleteWorkerBlock
    :: Types.CustomerId -- ^ 'workerId'
    -> DeleteWorkerBlock
mkDeleteWorkerBlock workerId
  = DeleteWorkerBlock'{workerId, reason = Core.Nothing}

-- | The ID of the Worker to unblock.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbWorkerId :: Lens.Lens' DeleteWorkerBlock Types.CustomerId
dwbWorkerId = Lens.field @"workerId"
{-# INLINEABLE dwbWorkerId #-}
{-# DEPRECATED workerId "Use generic-lens or generic-optics with 'workerId' instead"  #-}

-- | A message that explains the reason for unblocking the Worker. The Worker does not see this message.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbReason :: Lens.Lens' DeleteWorkerBlock (Core.Maybe Core.Text)
dwbReason = Lens.field @"reason"
{-# INLINEABLE dwbReason #-}
{-# DEPRECATED reason "Use generic-lens or generic-optics with 'reason' instead"  #-}

instance Core.ToQuery DeleteWorkerBlock where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteWorkerBlock where
        toHeaders DeleteWorkerBlock{..}
          = Core.pure
              ("X-Amz-Target",
               "MTurkRequesterServiceV20170117.DeleteWorkerBlock")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteWorkerBlock where
        toJSON DeleteWorkerBlock{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WorkerId" Core..= workerId),
                  ("Reason" Core..=) Core.<$> reason])

instance Core.AWSRequest DeleteWorkerBlock where
        type Rs DeleteWorkerBlock = DeleteWorkerBlockResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteWorkerBlockResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteWorkerBlockResponse' smart constructor.
newtype DeleteWorkerBlockResponse = DeleteWorkerBlockResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWorkerBlockResponse' value with any optional fields omitted.
mkDeleteWorkerBlockResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteWorkerBlockResponse
mkDeleteWorkerBlockResponse responseStatus
  = DeleteWorkerBlockResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbrrsResponseStatus :: Lens.Lens' DeleteWorkerBlockResponse Core.Int
dwbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dwbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
