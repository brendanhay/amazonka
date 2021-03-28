{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.CreateWorkerBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateWorkerBlock@ operation allows you to prevent a Worker from working on your HITs. For example, you can block a Worker who is producing poor quality work. You can block up to 100,000 Workers.
module Network.AWS.MechanicalTurk.CreateWorkerBlock
    (
    -- * Creating a request
      CreateWorkerBlock (..)
    , mkCreateWorkerBlock
    -- ** Request lenses
    , cwbWorkerId
    , cwbReason

    -- * Destructuring the response
    , CreateWorkerBlockResponse (..)
    , mkCreateWorkerBlockResponse
    -- ** Response lenses
    , cwbrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateWorkerBlock' smart constructor.
data CreateWorkerBlock = CreateWorkerBlock'
  { workerId :: Types.CustomerId
    -- ^ The ID of the Worker to block.
  , reason :: Core.Text
    -- ^ A message explaining the reason for blocking the Worker. This parameter enables you to keep track of your Workers. The Worker does not see this message.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateWorkerBlock' value with any optional fields omitted.
mkCreateWorkerBlock
    :: Types.CustomerId -- ^ 'workerId'
    -> Core.Text -- ^ 'reason'
    -> CreateWorkerBlock
mkCreateWorkerBlock workerId reason
  = CreateWorkerBlock'{workerId, reason}

-- | The ID of the Worker to block.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwbWorkerId :: Lens.Lens' CreateWorkerBlock Types.CustomerId
cwbWorkerId = Lens.field @"workerId"
{-# INLINEABLE cwbWorkerId #-}
{-# DEPRECATED workerId "Use generic-lens or generic-optics with 'workerId' instead"  #-}

-- | A message explaining the reason for blocking the Worker. This parameter enables you to keep track of your Workers. The Worker does not see this message.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwbReason :: Lens.Lens' CreateWorkerBlock Core.Text
cwbReason = Lens.field @"reason"
{-# INLINEABLE cwbReason #-}
{-# DEPRECATED reason "Use generic-lens or generic-optics with 'reason' instead"  #-}

instance Core.ToQuery CreateWorkerBlock where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateWorkerBlock where
        toHeaders CreateWorkerBlock{..}
          = Core.pure
              ("X-Amz-Target",
               "MTurkRequesterServiceV20170117.CreateWorkerBlock")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateWorkerBlock where
        toJSON CreateWorkerBlock{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WorkerId" Core..= workerId),
                  Core.Just ("Reason" Core..= reason)])

instance Core.AWSRequest CreateWorkerBlock where
        type Rs CreateWorkerBlock = CreateWorkerBlockResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CreateWorkerBlockResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateWorkerBlockResponse' smart constructor.
newtype CreateWorkerBlockResponse = CreateWorkerBlockResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateWorkerBlockResponse' value with any optional fields omitted.
mkCreateWorkerBlockResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateWorkerBlockResponse
mkCreateWorkerBlockResponse responseStatus
  = CreateWorkerBlockResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwbrrsResponseStatus :: Lens.Lens' CreateWorkerBlockResponse Core.Int
cwbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cwbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
