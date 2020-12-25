{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListProvisioningArtifacts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all provisioning artifacts (also known as versions) for the specified product.
module Network.AWS.ServiceCatalog.ListProvisioningArtifacts
  ( -- * Creating a request
    ListProvisioningArtifacts (..),
    mkListProvisioningArtifacts,

    -- ** Request lenses
    lpaProductId,
    lpaAcceptLanguage,

    -- * Destructuring the response
    ListProvisioningArtifactsResponse (..),
    mkListProvisioningArtifactsResponse,

    -- ** Response lenses
    lparrsNextPageToken,
    lparrsProvisioningArtifactDetails,
    lparrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListProvisioningArtifacts' smart constructor.
data ListProvisioningArtifacts = ListProvisioningArtifacts'
  { -- | The product identifier.
    productId :: Types.ProductId,
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

-- | Creates a 'ListProvisioningArtifacts' value with any optional fields omitted.
mkListProvisioningArtifacts ::
  -- | 'productId'
  Types.ProductId ->
  ListProvisioningArtifacts
mkListProvisioningArtifacts productId =
  ListProvisioningArtifacts'
    { productId,
      acceptLanguage = Core.Nothing
    }

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpaProductId :: Lens.Lens' ListProvisioningArtifacts Types.ProductId
lpaProductId = Lens.field @"productId"
{-# DEPRECATED lpaProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

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
lpaAcceptLanguage :: Lens.Lens' ListProvisioningArtifacts (Core.Maybe Types.AcceptLanguage)
lpaAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED lpaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

instance Core.FromJSON ListProvisioningArtifacts where
  toJSON ListProvisioningArtifacts {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProductId" Core..= productId),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.AWSRequest ListProvisioningArtifacts where
  type
    Rs ListProvisioningArtifacts =
      ListProvisioningArtifactsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.ListProvisioningArtifacts"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProvisioningArtifactsResponse'
            Core.<$> (x Core..:? "NextPageToken")
            Core.<*> (x Core..:? "ProvisioningArtifactDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListProvisioningArtifactsResponse' smart constructor.
data ListProvisioningArtifactsResponse = ListProvisioningArtifactsResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | Information about the provisioning artifacts.
    provisioningArtifactDetails :: Core.Maybe [Types.ProvisioningArtifactDetail],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListProvisioningArtifactsResponse' value with any optional fields omitted.
mkListProvisioningArtifactsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListProvisioningArtifactsResponse
mkListProvisioningArtifactsResponse responseStatus =
  ListProvisioningArtifactsResponse'
    { nextPageToken = Core.Nothing,
      provisioningArtifactDetails = Core.Nothing,
      responseStatus
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lparrsNextPageToken :: Lens.Lens' ListProvisioningArtifactsResponse (Core.Maybe Types.NextPageToken)
lparrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED lparrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Information about the provisioning artifacts.
--
-- /Note:/ Consider using 'provisioningArtifactDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lparrsProvisioningArtifactDetails :: Lens.Lens' ListProvisioningArtifactsResponse (Core.Maybe [Types.ProvisioningArtifactDetail])
lparrsProvisioningArtifactDetails = Lens.field @"provisioningArtifactDetails"
{-# DEPRECATED lparrsProvisioningArtifactDetails "Use generic-lens or generic-optics with 'provisioningArtifactDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lparrsResponseStatus :: Lens.Lens' ListProvisioningArtifactsResponse Core.Int
lparrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lparrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
