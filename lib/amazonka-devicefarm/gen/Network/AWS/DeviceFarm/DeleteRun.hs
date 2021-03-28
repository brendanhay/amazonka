{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the run, given the run ARN.
--
-- Deleting this resource does not stop an in-progress run.
module Network.AWS.DeviceFarm.DeleteRun
    (
    -- * Creating a request
      DeleteRun (..)
    , mkDeleteRun
    -- ** Request lenses
    , drArn

    -- * Destructuring the response
    , DeleteRunResponse (..)
    , mkDeleteRunResponse
    -- ** Response lenses
    , drrrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the delete run operation.
--
-- /See:/ 'mkDeleteRun' smart constructor.
newtype DeleteRun = DeleteRun'
  { arn :: Types.AmazonResourceName
    -- ^ The Amazon Resource Name (ARN) for the run to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRun' value with any optional fields omitted.
mkDeleteRun
    :: Types.AmazonResourceName -- ^ 'arn'
    -> DeleteRun
mkDeleteRun arn = DeleteRun'{arn}

-- | The Amazon Resource Name (ARN) for the run to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drArn :: Lens.Lens' DeleteRun Types.AmazonResourceName
drArn = Lens.field @"arn"
{-# INLINEABLE drArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery DeleteRun where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteRun where
        toHeaders DeleteRun{..}
          = Core.pure ("X-Amz-Target", "DeviceFarm_20150623.DeleteRun")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteRun where
        toJSON DeleteRun{..}
          = Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest DeleteRun where
        type Rs DeleteRun = DeleteRunResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteRunResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Represents the result of a delete run request.
--
-- /See:/ 'mkDeleteRunResponse' smart constructor.
newtype DeleteRunResponse = DeleteRunResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRunResponse' value with any optional fields omitted.
mkDeleteRunResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteRunResponse
mkDeleteRunResponse responseStatus
  = DeleteRunResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResponseStatus :: Lens.Lens' DeleteRunResponse Core.Int
drrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
