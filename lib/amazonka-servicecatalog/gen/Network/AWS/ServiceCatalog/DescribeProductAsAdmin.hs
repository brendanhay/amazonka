{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeProductAsAdmin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified product. This operation is run with administrator access.
module Network.AWS.ServiceCatalog.DescribeProductAsAdmin
  ( -- * Creating a request
    DescribeProductAsAdmin (..),
    mkDescribeProductAsAdmin,

    -- ** Request lenses
    dpaaAcceptLanguage,
    dpaaId,
    dpaaName,

    -- * Destructuring the response
    DescribeProductAsAdminResponse (..),
    mkDescribeProductAsAdminResponse,

    -- ** Response lenses
    dpaarrsBudgets,
    dpaarrsProductViewDetail,
    dpaarrsProvisioningArtifactSummaries,
    dpaarrsTagOptions,
    dpaarrsTags,
    dpaarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDescribeProductAsAdmin' smart constructor.
data DescribeProductAsAdmin = DescribeProductAsAdmin'
  { -- | The language code.
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
    -- | The product identifier.
    id :: Core.Maybe Types.Id,
    -- | The product name.
    name :: Core.Maybe Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProductAsAdmin' value with any optional fields omitted.
mkDescribeProductAsAdmin ::
  DescribeProductAsAdmin
mkDescribeProductAsAdmin =
  DescribeProductAsAdmin'
    { acceptLanguage = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing
    }

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
dpaaAcceptLanguage :: Lens.Lens' DescribeProductAsAdmin (Core.Maybe Types.AcceptLanguage)
dpaaAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED dpaaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaaId :: Lens.Lens' DescribeProductAsAdmin (Core.Maybe Types.Id)
dpaaId = Lens.field @"id"
{-# DEPRECATED dpaaId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The product name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaaName :: Lens.Lens' DescribeProductAsAdmin (Core.Maybe Types.Name)
dpaaName = Lens.field @"name"
{-# DEPRECATED dpaaName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DescribeProductAsAdmin where
  toJSON DescribeProductAsAdmin {..} =
    Core.object
      ( Core.catMaybes
          [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("Id" Core..=) Core.<$> id,
            ("Name" Core..=) Core.<$> name
          ]
      )

instance Core.AWSRequest DescribeProductAsAdmin where
  type Rs DescribeProductAsAdmin = DescribeProductAsAdminResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.DescribeProductAsAdmin"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProductAsAdminResponse'
            Core.<$> (x Core..:? "Budgets")
            Core.<*> (x Core..:? "ProductViewDetail")
            Core.<*> (x Core..:? "ProvisioningArtifactSummaries")
            Core.<*> (x Core..:? "TagOptions")
            Core.<*> (x Core..:? "Tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeProductAsAdminResponse' smart constructor.
data DescribeProductAsAdminResponse = DescribeProductAsAdminResponse'
  { -- | Information about the associated budgets.
    budgets :: Core.Maybe [Types.BudgetDetail],
    -- | Information about the product view.
    productViewDetail :: Core.Maybe Types.ProductViewDetail,
    -- | Information about the provisioning artifacts (also known as versions) for the specified product.
    provisioningArtifactSummaries :: Core.Maybe [Types.ProvisioningArtifactSummary],
    -- | Information about the TagOptions associated with the product.
    tagOptions :: Core.Maybe [Types.TagOptionDetail],
    -- | Information about the tags associated with the product.
    tags :: Core.Maybe [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeProductAsAdminResponse' value with any optional fields omitted.
mkDescribeProductAsAdminResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeProductAsAdminResponse
mkDescribeProductAsAdminResponse responseStatus =
  DescribeProductAsAdminResponse'
    { budgets = Core.Nothing,
      productViewDetail = Core.Nothing,
      provisioningArtifactSummaries = Core.Nothing,
      tagOptions = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | Information about the associated budgets.
--
-- /Note:/ Consider using 'budgets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaarrsBudgets :: Lens.Lens' DescribeProductAsAdminResponse (Core.Maybe [Types.BudgetDetail])
dpaarrsBudgets = Lens.field @"budgets"
{-# DEPRECATED dpaarrsBudgets "Use generic-lens or generic-optics with 'budgets' instead." #-}

-- | Information about the product view.
--
-- /Note:/ Consider using 'productViewDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaarrsProductViewDetail :: Lens.Lens' DescribeProductAsAdminResponse (Core.Maybe Types.ProductViewDetail)
dpaarrsProductViewDetail = Lens.field @"productViewDetail"
{-# DEPRECATED dpaarrsProductViewDetail "Use generic-lens or generic-optics with 'productViewDetail' instead." #-}

-- | Information about the provisioning artifacts (also known as versions) for the specified product.
--
-- /Note:/ Consider using 'provisioningArtifactSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaarrsProvisioningArtifactSummaries :: Lens.Lens' DescribeProductAsAdminResponse (Core.Maybe [Types.ProvisioningArtifactSummary])
dpaarrsProvisioningArtifactSummaries = Lens.field @"provisioningArtifactSummaries"
{-# DEPRECATED dpaarrsProvisioningArtifactSummaries "Use generic-lens or generic-optics with 'provisioningArtifactSummaries' instead." #-}

-- | Information about the TagOptions associated with the product.
--
-- /Note:/ Consider using 'tagOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaarrsTagOptions :: Lens.Lens' DescribeProductAsAdminResponse (Core.Maybe [Types.TagOptionDetail])
dpaarrsTagOptions = Lens.field @"tagOptions"
{-# DEPRECATED dpaarrsTagOptions "Use generic-lens or generic-optics with 'tagOptions' instead." #-}

-- | Information about the tags associated with the product.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaarrsTags :: Lens.Lens' DescribeProductAsAdminResponse (Core.Maybe [Types.Tag])
dpaarrsTags = Lens.field @"tags"
{-# DEPRECATED dpaarrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpaarrsResponseStatus :: Lens.Lens' DescribeProductAsAdminResponse Core.Int
dpaarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpaarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
