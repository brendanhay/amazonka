{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DisassociateTagOptionFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified TagOption from the specified resource.
module Network.AWS.ServiceCatalog.DisassociateTagOptionFromResource
    (
    -- * Creating a request
      DisassociateTagOptionFromResource (..)
    , mkDisassociateTagOptionFromResource
    -- ** Request lenses
    , dtofrResourceId
    , dtofrTagOptionId

    -- * Destructuring the response
    , DisassociateTagOptionFromResourceResponse (..)
    , mkDisassociateTagOptionFromResourceResponse
    -- ** Response lenses
    , dtofrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDisassociateTagOptionFromResource' smart constructor.
data DisassociateTagOptionFromResource = DisassociateTagOptionFromResource'
  { resourceId :: Types.ResourceId
    -- ^ The resource identifier.
  , tagOptionId :: Types.TagOptionId
    -- ^ The TagOption identifier.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateTagOptionFromResource' value with any optional fields omitted.
mkDisassociateTagOptionFromResource
    :: Types.ResourceId -- ^ 'resourceId'
    -> Types.TagOptionId -- ^ 'tagOptionId'
    -> DisassociateTagOptionFromResource
mkDisassociateTagOptionFromResource resourceId tagOptionId
  = DisassociateTagOptionFromResource'{resourceId, tagOptionId}

-- | The resource identifier.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtofrResourceId :: Lens.Lens' DisassociateTagOptionFromResource Types.ResourceId
dtofrResourceId = Lens.field @"resourceId"
{-# INLINEABLE dtofrResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The TagOption identifier.
--
-- /Note:/ Consider using 'tagOptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtofrTagOptionId :: Lens.Lens' DisassociateTagOptionFromResource Types.TagOptionId
dtofrTagOptionId = Lens.field @"tagOptionId"
{-# INLINEABLE dtofrTagOptionId #-}
{-# DEPRECATED tagOptionId "Use generic-lens or generic-optics with 'tagOptionId' instead"  #-}

instance Core.ToQuery DisassociateTagOptionFromResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateTagOptionFromResource where
        toHeaders DisassociateTagOptionFromResource{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.DisassociateTagOptionFromResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisassociateTagOptionFromResource where
        toJSON DisassociateTagOptionFromResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceId" Core..= resourceId),
                  Core.Just ("TagOptionId" Core..= tagOptionId)])

instance Core.AWSRequest DisassociateTagOptionFromResource where
        type Rs DisassociateTagOptionFromResource =
             DisassociateTagOptionFromResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisassociateTagOptionFromResourceResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateTagOptionFromResourceResponse' smart constructor.
newtype DisassociateTagOptionFromResourceResponse = DisassociateTagOptionFromResourceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateTagOptionFromResourceResponse' value with any optional fields omitted.
mkDisassociateTagOptionFromResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateTagOptionFromResourceResponse
mkDisassociateTagOptionFromResourceResponse responseStatus
  = DisassociateTagOptionFromResourceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtofrrrsResponseStatus :: Lens.Lens' DisassociateTagOptionFromResourceResponse Core.Int
dtofrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtofrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
