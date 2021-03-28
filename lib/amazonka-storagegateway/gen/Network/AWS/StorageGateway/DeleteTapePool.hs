{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteTapePool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a custom tape pool. A custom tape pool can only be deleted if there are no tapes in the pool and if there are no automatic tape creation policies that reference the custom tape pool.
module Network.AWS.StorageGateway.DeleteTapePool
    (
    -- * Creating a request
      DeleteTapePool (..)
    , mkDeleteTapePool
    -- ** Request lenses
    , dtpPoolARN

    -- * Destructuring the response
    , DeleteTapePoolResponse (..)
    , mkDeleteTapePoolResponse
    -- ** Response lenses
    , dtprrsPoolARN
    , dtprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkDeleteTapePool' smart constructor.
newtype DeleteTapePool = DeleteTapePool'
  { poolARN :: Types.PoolARN
    -- ^ The Amazon Resource Name (ARN) of the custom tape pool to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTapePool' value with any optional fields omitted.
mkDeleteTapePool
    :: Types.PoolARN -- ^ 'poolARN'
    -> DeleteTapePool
mkDeleteTapePool poolARN = DeleteTapePool'{poolARN}

-- | The Amazon Resource Name (ARN) of the custom tape pool to delete.
--
-- /Note:/ Consider using 'poolARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtpPoolARN :: Lens.Lens' DeleteTapePool Types.PoolARN
dtpPoolARN = Lens.field @"poolARN"
{-# INLINEABLE dtpPoolARN #-}
{-# DEPRECATED poolARN "Use generic-lens or generic-optics with 'poolARN' instead"  #-}

instance Core.ToQuery DeleteTapePool where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteTapePool where
        toHeaders DeleteTapePool{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.DeleteTapePool")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteTapePool where
        toJSON DeleteTapePool{..}
          = Core.object
              (Core.catMaybes [Core.Just ("PoolARN" Core..= poolARN)])

instance Core.AWSRequest DeleteTapePool where
        type Rs DeleteTapePool = DeleteTapePoolResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteTapePoolResponse' Core.<$>
                   (x Core..:? "PoolARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteTapePoolResponse' smart constructor.
data DeleteTapePoolResponse = DeleteTapePoolResponse'
  { poolARN :: Core.Maybe Types.PoolARN
    -- ^ The Amazon Resource Name (ARN) of the custom tape pool being deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTapePoolResponse' value with any optional fields omitted.
mkDeleteTapePoolResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteTapePoolResponse
mkDeleteTapePoolResponse responseStatus
  = DeleteTapePoolResponse'{poolARN = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the custom tape pool being deleted.
--
-- /Note:/ Consider using 'poolARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtprrsPoolARN :: Lens.Lens' DeleteTapePoolResponse (Core.Maybe Types.PoolARN)
dtprrsPoolARN = Lens.field @"poolARN"
{-# INLINEABLE dtprrsPoolARN #-}
{-# DEPRECATED poolARN "Use generic-lens or generic-optics with 'poolARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtprrsResponseStatus :: Lens.Lens' DeleteTapePoolResponse Core.Int
dtprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
