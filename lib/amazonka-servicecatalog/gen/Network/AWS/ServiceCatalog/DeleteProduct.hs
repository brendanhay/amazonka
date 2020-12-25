{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteProduct (..),
    mkDeleteProduct,

    -- ** Request lenses
    dpgId,
    dpgAcceptLanguage,

    -- * Destructuring the response
    DeleteProductResponse (..),
    mkDeleteProductResponse,

    -- ** Response lenses
    dprgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDeleteProduct' smart constructor.
data DeleteProduct = DeleteProduct'
  { -- | The product identifier.
    id :: Types.Id,
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
    acceptLanguage :: Core.Maybe Types.AcceptLanguage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProduct' value with any optional fields omitted.
mkDeleteProduct ::
  -- | 'id'
  Types.Id ->
  DeleteProduct
mkDeleteProduct id =
  DeleteProduct' {id, acceptLanguage = Core.Nothing}

-- | The product identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgId :: Lens.Lens' DeleteProduct Types.Id
dpgId = Lens.field @"id"
{-# DEPRECATED dpgId "Use generic-lens or generic-optics with 'id' instead." #-}

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
{-# DEPRECATED dpgAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

instance Core.FromJSON DeleteProduct where
  toJSON DeleteProduct {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Id" Core..= id),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.AWSRequest DeleteProduct where
  type Rs DeleteProduct = DeleteProductResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWS242ServiceCatalogService.DeleteProduct")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProductResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteProductResponse' smart constructor.
newtype DeleteProductResponse = DeleteProductResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProductResponse' value with any optional fields omitted.
mkDeleteProductResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteProductResponse
mkDeleteProductResponse responseStatus =
  DeleteProductResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprgrsResponseStatus :: Lens.Lens' DeleteProductResponse Core.Int
dprgrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dprgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
