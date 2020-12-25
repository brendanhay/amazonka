{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified product.
module Network.AWS.ServiceCatalog.DescribeProduct
  ( -- * Creating a request
    DescribeProduct (..),
    mkDescribeProduct,

    -- ** Request lenses
    dpAcceptLanguage,
    dpId,
    dpName,

    -- * Destructuring the response
    DescribeProductResponse (..),
    mkDescribeProductResponse,

    -- ** Response lenses
    dprrsBudgets,
    dprrsLaunchPaths,
    dprrsProductViewSummary,
    dprrsProvisioningArtifacts,
    dprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDescribeProduct' smart constructor.
data DescribeProduct = DescribeProduct'
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

-- | Creates a 'DescribeProduct' value with any optional fields omitted.
mkDescribeProduct ::
  DescribeProduct
mkDescribeProduct =
  DescribeProduct'
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
dpAcceptLanguage :: Lens.Lens' DescribeProduct (Core.Maybe Types.AcceptLanguage)
dpAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED dpAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpId :: Lens.Lens' DescribeProduct (Core.Maybe Types.Id)
dpId = Lens.field @"id"
{-# DEPRECATED dpId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The product name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpName :: Lens.Lens' DescribeProduct (Core.Maybe Types.Name)
dpName = Lens.field @"name"
{-# DEPRECATED dpName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DescribeProduct where
  toJSON DescribeProduct {..} =
    Core.object
      ( Core.catMaybes
          [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("Id" Core..=) Core.<$> id,
            ("Name" Core..=) Core.<$> name
          ]
      )

instance Core.AWSRequest DescribeProduct where
  type Rs DescribeProduct = DescribeProductResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWS242ServiceCatalogService.DescribeProduct")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProductResponse'
            Core.<$> (x Core..:? "Budgets")
            Core.<*> (x Core..:? "LaunchPaths")
            Core.<*> (x Core..:? "ProductViewSummary")
            Core.<*> (x Core..:? "ProvisioningArtifacts")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeProductResponse' smart constructor.
data DescribeProductResponse = DescribeProductResponse'
  { -- | Information about the associated budgets.
    budgets :: Core.Maybe [Types.BudgetDetail],
    -- | Information about the associated launch paths.
    launchPaths :: Core.Maybe [Types.LaunchPath],
    -- | Summary information about the product view.
    productViewSummary :: Core.Maybe Types.ProductViewSummary,
    -- | Information about the provisioning artifacts for the specified product.
    provisioningArtifacts :: Core.Maybe [Types.ProvisioningArtifact],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeProductResponse' value with any optional fields omitted.
mkDescribeProductResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeProductResponse
mkDescribeProductResponse responseStatus =
  DescribeProductResponse'
    { budgets = Core.Nothing,
      launchPaths = Core.Nothing,
      productViewSummary = Core.Nothing,
      provisioningArtifacts = Core.Nothing,
      responseStatus
    }

-- | Information about the associated budgets.
--
-- /Note:/ Consider using 'budgets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsBudgets :: Lens.Lens' DescribeProductResponse (Core.Maybe [Types.BudgetDetail])
dprrsBudgets = Lens.field @"budgets"
{-# DEPRECATED dprrsBudgets "Use generic-lens or generic-optics with 'budgets' instead." #-}

-- | Information about the associated launch paths.
--
-- /Note:/ Consider using 'launchPaths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsLaunchPaths :: Lens.Lens' DescribeProductResponse (Core.Maybe [Types.LaunchPath])
dprrsLaunchPaths = Lens.field @"launchPaths"
{-# DEPRECATED dprrsLaunchPaths "Use generic-lens or generic-optics with 'launchPaths' instead." #-}

-- | Summary information about the product view.
--
-- /Note:/ Consider using 'productViewSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsProductViewSummary :: Lens.Lens' DescribeProductResponse (Core.Maybe Types.ProductViewSummary)
dprrsProductViewSummary = Lens.field @"productViewSummary"
{-# DEPRECATED dprrsProductViewSummary "Use generic-lens or generic-optics with 'productViewSummary' instead." #-}

-- | Information about the provisioning artifacts for the specified product.
--
-- /Note:/ Consider using 'provisioningArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsProvisioningArtifacts :: Lens.Lens' DescribeProductResponse (Core.Maybe [Types.ProvisioningArtifact])
dprrsProvisioningArtifacts = Lens.field @"provisioningArtifacts"
{-# DEPRECATED dprrsProvisioningArtifacts "Use generic-lens or generic-optics with 'provisioningArtifacts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsResponseStatus :: Lens.Lens' DescribeProductResponse Core.Int
dprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
