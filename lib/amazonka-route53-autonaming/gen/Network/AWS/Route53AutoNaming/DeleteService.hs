{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.DeleteService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified service. If the service still contains one or more registered instances, the request fails.
module Network.AWS.Route53AutoNaming.DeleteService
    (
    -- * Creating a request
      DeleteService (..)
    , mkDeleteService
    -- ** Request lenses
    , dsId

    -- * Destructuring the response
    , DeleteServiceResponse (..)
    , mkDeleteServiceResponse
    -- ** Response lenses
    , dsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53AutoNaming.Types as Types

-- | /See:/ 'mkDeleteService' smart constructor.
newtype DeleteService = DeleteService'
  { id :: Types.ResourceId
    -- ^ The ID of the service that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteService' value with any optional fields omitted.
mkDeleteService
    :: Types.ResourceId -- ^ 'id'
    -> DeleteService
mkDeleteService id = DeleteService'{id}

-- | The ID of the service that you want to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsId :: Lens.Lens' DeleteService Types.ResourceId
dsId = Lens.field @"id"
{-# INLINEABLE dsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery DeleteService where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteService where
        toHeaders DeleteService{..}
          = Core.pure
              ("X-Amz-Target", "Route53AutoNaming_v20170314.DeleteService")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteService where
        toJSON DeleteService{..}
          = Core.object (Core.catMaybes [Core.Just ("Id" Core..= id)])

instance Core.AWSRequest DeleteService where
        type Rs DeleteService = DeleteServiceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteServiceResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteServiceResponse' smart constructor.
newtype DeleteServiceResponse = DeleteServiceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteServiceResponse' value with any optional fields omitted.
mkDeleteServiceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteServiceResponse
mkDeleteServiceResponse responseStatus
  = DeleteServiceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DeleteServiceResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
