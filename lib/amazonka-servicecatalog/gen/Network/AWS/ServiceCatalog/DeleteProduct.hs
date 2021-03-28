{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DeleteProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified product.
--
-- You cannot delete a product if it was shared with you or is associated with a portfolio.
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.DeleteProduct
    (
    -- * Creating a request
      DeleteProduct (..)
    , mkDeleteProduct
    -- ** Request lenses
    , dpgId
    , dpgAcceptLanguage

    -- * Destructuring the response
    , DeleteProductResponse (..)
    , mkDeleteProductResponse
    -- ** Response lenses
    , dprgrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDeleteProduct' smart constructor.
data DeleteProduct = DeleteProduct'
  { id :: Types.Id
    -- ^ The product identifier.
  , acceptLanguage :: Core.Maybe Types.AcceptLanguage
    -- ^ The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProduct' value with any optional fields omitted.
mkDeleteProduct
    :: Types.Id -- ^ 'id'
    -> DeleteProduct
mkDeleteProduct id
  = DeleteProduct'{id, acceptLanguage = Core.Nothing}

-- | The product identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgId :: Lens.Lens' DeleteProduct Types.Id
dpgId = Lens.field @"id"
{-# INLINEABLE dpgId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgAcceptLanguage :: Lens.Lens' DeleteProduct (Core.Maybe Types.AcceptLanguage)
dpgAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE dpgAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

instance Core.ToQuery DeleteProduct where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteProduct where
        toHeaders DeleteProduct{..}
          = Core.pure
              ("X-Amz-Target", "AWS242ServiceCatalogService.DeleteProduct")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteProduct where
        toJSON DeleteProduct{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Id" Core..= id),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage])

instance Core.AWSRequest DeleteProduct where
        type Rs DeleteProduct = DeleteProductResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteProductResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteProductResponse' smart constructor.
newtype DeleteProductResponse = DeleteProductResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProductResponse' value with any optional fields omitted.
mkDeleteProductResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteProductResponse
mkDeleteProductResponse responseStatus
  = DeleteProductResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprgrsResponseStatus :: Lens.Lens' DeleteProductResponse Core.Int
dprgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dprgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
