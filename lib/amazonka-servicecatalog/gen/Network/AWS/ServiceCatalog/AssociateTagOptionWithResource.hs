{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.AssociateTagOptionWithResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate the specified TagOption with the specified portfolio or product.
module Network.AWS.ServiceCatalog.AssociateTagOptionWithResource
    (
    -- * Creating a request
      AssociateTagOptionWithResource (..)
    , mkAssociateTagOptionWithResource
    -- ** Request lenses
    , atowrResourceId
    , atowrTagOptionId

    -- * Destructuring the response
    , AssociateTagOptionWithResourceResponse (..)
    , mkAssociateTagOptionWithResourceResponse
    -- ** Response lenses
    , atowrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkAssociateTagOptionWithResource' smart constructor.
data AssociateTagOptionWithResource = AssociateTagOptionWithResource'
  { resourceId :: Types.ResourceId
    -- ^ The resource identifier.
  , tagOptionId :: Types.TagOptionId
    -- ^ The TagOption identifier.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateTagOptionWithResource' value with any optional fields omitted.
mkAssociateTagOptionWithResource
    :: Types.ResourceId -- ^ 'resourceId'
    -> Types.TagOptionId -- ^ 'tagOptionId'
    -> AssociateTagOptionWithResource
mkAssociateTagOptionWithResource resourceId tagOptionId
  = AssociateTagOptionWithResource'{resourceId, tagOptionId}

-- | The resource identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atowrResourceId :: Lens.Lens' AssociateTagOptionWithResource Types.ResourceId
atowrResourceId = Lens.field @"resourceId"
{-# INLINEABLE atowrResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The TagOption identifier.
--
-- /Note:/ Consider using 'tagOptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atowrTagOptionId :: Lens.Lens' AssociateTagOptionWithResource Types.TagOptionId
atowrTagOptionId = Lens.field @"tagOptionId"
{-# INLINEABLE atowrTagOptionId #-}
{-# DEPRECATED tagOptionId "Use generic-lens or generic-optics with 'tagOptionId' instead"  #-}

instance Core.ToQuery AssociateTagOptionWithResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateTagOptionWithResource where
        toHeaders AssociateTagOptionWithResource{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.AssociateTagOptionWithResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateTagOptionWithResource where
        toJSON AssociateTagOptionWithResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceId" Core..= resourceId),
                  Core.Just ("TagOptionId" Core..= tagOptionId)])

instance Core.AWSRequest AssociateTagOptionWithResource where
        type Rs AssociateTagOptionWithResource =
             AssociateTagOptionWithResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AssociateTagOptionWithResourceResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateTagOptionWithResourceResponse' smart constructor.
newtype AssociateTagOptionWithResourceResponse = AssociateTagOptionWithResourceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateTagOptionWithResourceResponse' value with any optional fields omitted.
mkAssociateTagOptionWithResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateTagOptionWithResourceResponse
mkAssociateTagOptionWithResourceResponse responseStatus
  = AssociateTagOptionWithResourceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atowrrrsResponseStatus :: Lens.Lens' AssociateTagOptionWithResourceResponse Core.Int
atowrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE atowrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
