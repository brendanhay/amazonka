{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DeleteResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified resource. 
module Network.AWS.WorkMail.DeleteResource
    (
    -- * Creating a request
      DeleteResource (..)
    , mkDeleteResource
    -- ** Request lenses
    , dOrganizationId
    , dResourceId

    -- * Destructuring the response
    , DeleteResourceResponse (..)
    , mkDeleteResourceResponse
    -- ** Response lenses
    , drsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkDeleteResource' smart constructor.
data DeleteResource = DeleteResource'
  { organizationId :: Types.OrganizationId
    -- ^ The identifier associated with the organization from which the resource is deleted.
  , resourceId :: Types.ResourceId
    -- ^ The identifier of the resource to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResource' value with any optional fields omitted.
mkDeleteResource
    :: Types.OrganizationId -- ^ 'organizationId'
    -> Types.ResourceId -- ^ 'resourceId'
    -> DeleteResource
mkDeleteResource organizationId resourceId
  = DeleteResource'{organizationId, resourceId}

-- | The identifier associated with the organization from which the resource is deleted.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOrganizationId :: Lens.Lens' DeleteResource Types.OrganizationId
dOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE dOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The identifier of the resource to be deleted.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dResourceId :: Lens.Lens' DeleteResource Types.ResourceId
dResourceId = Lens.field @"resourceId"
{-# INLINEABLE dResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

instance Core.ToQuery DeleteResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteResource where
        toHeaders DeleteResource{..}
          = Core.pure ("X-Amz-Target", "WorkMailService.DeleteResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteResource where
        toJSON DeleteResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId),
                  Core.Just ("ResourceId" Core..= resourceId)])

instance Core.AWSRequest DeleteResource where
        type Rs DeleteResource = DeleteResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteResourceResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteResourceResponse' smart constructor.
newtype DeleteResourceResponse = DeleteResourceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourceResponse' value with any optional fields omitted.
mkDeleteResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteResourceResponse
mkDeleteResourceResponse responseStatus
  = DeleteResourceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteResourceResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
