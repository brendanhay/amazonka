{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeProductView
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified product.
module Network.AWS.ServiceCatalog.DescribeProductView
  ( -- * Creating a request
    DescribeProductView (..),
    mkDescribeProductView,

    -- ** Request lenses
    dpvId,
    dpvAcceptLanguage,

    -- * Destructuring the response
    DescribeProductViewResponse (..),
    mkDescribeProductViewResponse,

    -- ** Response lenses
    dpvrrsProductViewSummary,
    dpvrrsProvisioningArtifacts,
    dpvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDescribeProductView' smart constructor.
data DescribeProductView = DescribeProductView'
  { -- | The product view identifier.
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

-- | Creates a 'DescribeProductView' value with any optional fields omitted.
mkDescribeProductView ::
  -- | 'id'
  Types.Id ->
  DescribeProductView
mkDescribeProductView id =
  DescribeProductView' {id, acceptLanguage = Core.Nothing}

-- | The product view identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvId :: Lens.Lens' DescribeProductView Types.Id
dpvId = Lens.field @"id"
{-# DEPRECATED dpvId "Use generic-lens or generic-optics with 'id' instead." #-}

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
dpvAcceptLanguage :: Lens.Lens' DescribeProductView (Core.Maybe Types.AcceptLanguage)
dpvAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED dpvAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

instance Core.FromJSON DescribeProductView where
  toJSON DescribeProductView {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Id" Core..= id),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.AWSRequest DescribeProductView where
  type Rs DescribeProductView = DescribeProductViewResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWS242ServiceCatalogService.DescribeProductView")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProductViewResponse'
            Core.<$> (x Core..:? "ProductViewSummary")
            Core.<*> (x Core..:? "ProvisioningArtifacts")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeProductViewResponse' smart constructor.
data DescribeProductViewResponse = DescribeProductViewResponse'
  { -- | Summary information about the product.
    productViewSummary :: Core.Maybe Types.ProductViewSummary,
    -- | Information about the provisioning artifacts for the product.
    provisioningArtifacts :: Core.Maybe [Types.ProvisioningArtifact],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeProductViewResponse' value with any optional fields omitted.
mkDescribeProductViewResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeProductViewResponse
mkDescribeProductViewResponse responseStatus =
  DescribeProductViewResponse'
    { productViewSummary = Core.Nothing,
      provisioningArtifacts = Core.Nothing,
      responseStatus
    }

-- | Summary information about the product.
--
-- /Note:/ Consider using 'productViewSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvrrsProductViewSummary :: Lens.Lens' DescribeProductViewResponse (Core.Maybe Types.ProductViewSummary)
dpvrrsProductViewSummary = Lens.field @"productViewSummary"
{-# DEPRECATED dpvrrsProductViewSummary "Use generic-lens or generic-optics with 'productViewSummary' instead." #-}

-- | Information about the provisioning artifacts for the product.
--
-- /Note:/ Consider using 'provisioningArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvrrsProvisioningArtifacts :: Lens.Lens' DescribeProductViewResponse (Core.Maybe [Types.ProvisioningArtifact])
dpvrrsProvisioningArtifacts = Lens.field @"provisioningArtifacts"
{-# DEPRECATED dpvrrsProvisioningArtifacts "Use generic-lens or generic-optics with 'provisioningArtifacts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvrrsResponseStatus :: Lens.Lens' DescribeProductViewResponse Core.Int
dpvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
