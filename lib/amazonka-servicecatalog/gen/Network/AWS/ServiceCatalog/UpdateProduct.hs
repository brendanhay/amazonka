{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.UpdateProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified product.
module Network.AWS.ServiceCatalog.UpdateProduct
  ( -- * Creating a request
    UpdateProduct (..),
    mkUpdateProduct,

    -- ** Request lenses
    upId,
    upAcceptLanguage,
    upAddTags,
    upDescription,
    upDistributor,
    upName,
    upOwner,
    upRemoveTags,
    upSupportDescription,
    upSupportEmail,
    upSupportUrl,

    -- * Destructuring the response
    UpdateProductResponse (..),
    mkUpdateProductResponse,

    -- ** Response lenses
    uprrsProductViewDetail,
    uprrsTags,
    uprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkUpdateProduct' smart constructor.
data UpdateProduct = UpdateProduct'
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
    acceptLanguage :: Core.Maybe Types.AcceptLanguage,
    -- | The tags to add to the product.
    addTags :: Core.Maybe [Types.Tag],
    -- | The updated description of the product.
    description :: Core.Maybe Types.Description,
    -- | The updated distributor of the product.
    distributor :: Core.Maybe Types.Distributor,
    -- | The updated product name.
    name :: Core.Maybe Types.Name,
    -- | The updated owner of the product.
    owner :: Core.Maybe Types.Owner,
    -- | The tags to remove from the product.
    removeTags :: Core.Maybe [Types.TagKey],
    -- | The updated support description for the product.
    supportDescription :: Core.Maybe Types.SupportDescription,
    -- | The updated support email for the product.
    supportEmail :: Core.Maybe Types.SupportEmail,
    -- | The updated support URL for the product.
    supportUrl :: Core.Maybe Types.SupportUrl
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProduct' value with any optional fields omitted.
mkUpdateProduct ::
  -- | 'id'
  Types.Id ->
  UpdateProduct
mkUpdateProduct id =
  UpdateProduct'
    { id,
      acceptLanguage = Core.Nothing,
      addTags = Core.Nothing,
      description = Core.Nothing,
      distributor = Core.Nothing,
      name = Core.Nothing,
      owner = Core.Nothing,
      removeTags = Core.Nothing,
      supportDescription = Core.Nothing,
      supportEmail = Core.Nothing,
      supportUrl = Core.Nothing
    }

-- | The product identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upId :: Lens.Lens' UpdateProduct Types.Id
upId = Lens.field @"id"
{-# DEPRECATED upId "Use generic-lens or generic-optics with 'id' instead." #-}

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
upAcceptLanguage :: Lens.Lens' UpdateProduct (Core.Maybe Types.AcceptLanguage)
upAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED upAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The tags to add to the product.
--
-- /Note:/ Consider using 'addTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upAddTags :: Lens.Lens' UpdateProduct (Core.Maybe [Types.Tag])
upAddTags = Lens.field @"addTags"
{-# DEPRECATED upAddTags "Use generic-lens or generic-optics with 'addTags' instead." #-}

-- | The updated description of the product.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDescription :: Lens.Lens' UpdateProduct (Core.Maybe Types.Description)
upDescription = Lens.field @"description"
{-# DEPRECATED upDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The updated distributor of the product.
--
-- /Note:/ Consider using 'distributor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDistributor :: Lens.Lens' UpdateProduct (Core.Maybe Types.Distributor)
upDistributor = Lens.field @"distributor"
{-# DEPRECATED upDistributor "Use generic-lens or generic-optics with 'distributor' instead." #-}

-- | The updated product name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upName :: Lens.Lens' UpdateProduct (Core.Maybe Types.Name)
upName = Lens.field @"name"
{-# DEPRECATED upName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The updated owner of the product.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upOwner :: Lens.Lens' UpdateProduct (Core.Maybe Types.Owner)
upOwner = Lens.field @"owner"
{-# DEPRECATED upOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The tags to remove from the product.
--
-- /Note:/ Consider using 'removeTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upRemoveTags :: Lens.Lens' UpdateProduct (Core.Maybe [Types.TagKey])
upRemoveTags = Lens.field @"removeTags"
{-# DEPRECATED upRemoveTags "Use generic-lens or generic-optics with 'removeTags' instead." #-}

-- | The updated support description for the product.
--
-- /Note:/ Consider using 'supportDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSupportDescription :: Lens.Lens' UpdateProduct (Core.Maybe Types.SupportDescription)
upSupportDescription = Lens.field @"supportDescription"
{-# DEPRECATED upSupportDescription "Use generic-lens or generic-optics with 'supportDescription' instead." #-}

-- | The updated support email for the product.
--
-- /Note:/ Consider using 'supportEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSupportEmail :: Lens.Lens' UpdateProduct (Core.Maybe Types.SupportEmail)
upSupportEmail = Lens.field @"supportEmail"
{-# DEPRECATED upSupportEmail "Use generic-lens or generic-optics with 'supportEmail' instead." #-}

-- | The updated support URL for the product.
--
-- /Note:/ Consider using 'supportUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSupportUrl :: Lens.Lens' UpdateProduct (Core.Maybe Types.SupportUrl)
upSupportUrl = Lens.field @"supportUrl"
{-# DEPRECATED upSupportUrl "Use generic-lens or generic-optics with 'supportUrl' instead." #-}

instance Core.FromJSON UpdateProduct where
  toJSON UpdateProduct {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Id" Core..= id),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("AddTags" Core..=) Core.<$> addTags,
            ("Description" Core..=) Core.<$> description,
            ("Distributor" Core..=) Core.<$> distributor,
            ("Name" Core..=) Core.<$> name,
            ("Owner" Core..=) Core.<$> owner,
            ("RemoveTags" Core..=) Core.<$> removeTags,
            ("SupportDescription" Core..=) Core.<$> supportDescription,
            ("SupportEmail" Core..=) Core.<$> supportEmail,
            ("SupportUrl" Core..=) Core.<$> supportUrl
          ]
      )

instance Core.AWSRequest UpdateProduct where
  type Rs UpdateProduct = UpdateProductResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWS242ServiceCatalogService.UpdateProduct")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProductResponse'
            Core.<$> (x Core..:? "ProductViewDetail")
            Core.<*> (x Core..:? "Tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateProductResponse' smart constructor.
data UpdateProductResponse = UpdateProductResponse'
  { -- | Information about the product view.
    productViewDetail :: Core.Maybe Types.ProductViewDetail,
    -- | Information about the tags associated with the product.
    tags :: Core.Maybe [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateProductResponse' value with any optional fields omitted.
mkUpdateProductResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateProductResponse
mkUpdateProductResponse responseStatus =
  UpdateProductResponse'
    { productViewDetail = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | Information about the product view.
--
-- /Note:/ Consider using 'productViewDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsProductViewDetail :: Lens.Lens' UpdateProductResponse (Core.Maybe Types.ProductViewDetail)
uprrsProductViewDetail = Lens.field @"productViewDetail"
{-# DEPRECATED uprrsProductViewDetail "Use generic-lens or generic-optics with 'productViewDetail' instead." #-}

-- | Information about the tags associated with the product.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsTags :: Lens.Lens' UpdateProductResponse (Core.Maybe [Types.Tag])
uprrsTags = Lens.field @"tags"
{-# DEPRECATED uprrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsResponseStatus :: Lens.Lens' UpdateProductResponse Core.Int
uprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
