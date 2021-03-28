{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DeleteLogSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified log subscription.
module Network.AWS.DirectoryService.DeleteLogSubscription
    (
    -- * Creating a request
      DeleteLogSubscription (..)
    , mkDeleteLogSubscription
    -- ** Request lenses
    , dlsDirectoryId

    -- * Destructuring the response
    , DeleteLogSubscriptionResponse (..)
    , mkDeleteLogSubscriptionResponse
    -- ** Response lenses
    , dlsrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLogSubscription' smart constructor.
newtype DeleteLogSubscription = DeleteLogSubscription'
  { directoryId :: Types.DirectoryId
    -- ^ Identifier of the directory whose log subscription you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLogSubscription' value with any optional fields omitted.
mkDeleteLogSubscription
    :: Types.DirectoryId -- ^ 'directoryId'
    -> DeleteLogSubscription
mkDeleteLogSubscription directoryId
  = DeleteLogSubscription'{directoryId}

-- | Identifier of the directory whose log subscription you want to delete.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlsDirectoryId :: Lens.Lens' DeleteLogSubscription Types.DirectoryId
dlsDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE dlsDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

instance Core.ToQuery DeleteLogSubscription where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteLogSubscription where
        toHeaders DeleteLogSubscription{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.DeleteLogSubscription")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteLogSubscription where
        toJSON DeleteLogSubscription{..}
          = Core.object
              (Core.catMaybes [Core.Just ("DirectoryId" Core..= directoryId)])

instance Core.AWSRequest DeleteLogSubscription where
        type Rs DeleteLogSubscription = DeleteLogSubscriptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteLogSubscriptionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteLogSubscriptionResponse' smart constructor.
newtype DeleteLogSubscriptionResponse = DeleteLogSubscriptionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLogSubscriptionResponse' value with any optional fields omitted.
mkDeleteLogSubscriptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteLogSubscriptionResponse
mkDeleteLogSubscriptionResponse responseStatus
  = DeleteLogSubscriptionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlsrrsResponseStatus :: Lens.Lens' DeleteLogSubscriptionResponse Core.Int
dlsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
