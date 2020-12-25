{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListPortfoliosForProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all portfolios that the specified product is associated with.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListPortfoliosForProduct
  ( -- * Creating a request
    ListPortfoliosForProduct (..),
    mkListPortfoliosForProduct,

    -- ** Request lenses
    lpfpfProductId,
    lpfpfAcceptLanguage,
    lpfpfPageSize,
    lpfpfPageToken,

    -- * Destructuring the response
    ListPortfoliosForProductResponse (..),
    mkListPortfoliosForProductResponse,

    -- ** Response lenses
    lpfprfrsNextPageToken,
    lpfprfrsPortfolioDetails,
    lpfprfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListPortfoliosForProduct' smart constructor.
data ListPortfoliosForProduct = ListPortfoliosForProduct'
  { -- | The product identifier.
    productId :: Types.Id,
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
    -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of results, use null.
    pageToken :: Core.Maybe Types.PageToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPortfoliosForProduct' value with any optional fields omitted.
mkListPortfoliosForProduct ::
  -- | 'productId'
  Types.Id ->
  ListPortfoliosForProduct
mkListPortfoliosForProduct productId =
  ListPortfoliosForProduct'
    { productId,
      acceptLanguage = Core.Nothing,
      pageSize = Core.Nothing,
      pageToken = Core.Nothing
    }

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfpfProductId :: Lens.Lens' ListPortfoliosForProduct Types.Id
lpfpfProductId = Lens.field @"productId"
{-# DEPRECATED lpfpfProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

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
lpfpfAcceptLanguage :: Lens.Lens' ListPortfoliosForProduct (Core.Maybe Types.AcceptLanguage)
lpfpfAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED lpfpfAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfpfPageSize :: Lens.Lens' ListPortfoliosForProduct (Core.Maybe Core.Natural)
lpfpfPageSize = Lens.field @"pageSize"
{-# DEPRECATED lpfpfPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfpfPageToken :: Lens.Lens' ListPortfoliosForProduct (Core.Maybe Types.PageToken)
lpfpfPageToken = Lens.field @"pageToken"
{-# DEPRECATED lpfpfPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Core.FromJSON ListPortfoliosForProduct where
  toJSON ListPortfoliosForProduct {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProductId" Core..= productId),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken
          ]
      )

instance Core.AWSRequest ListPortfoliosForProduct where
  type Rs ListPortfoliosForProduct = ListPortfoliosForProductResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.ListPortfoliosForProduct"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPortfoliosForProductResponse'
            Core.<$> (x Core..:? "NextPageToken")
            Core.<*> (x Core..:? "PortfolioDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListPortfoliosForProduct where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
      Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"portfolioDetails" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"pageToken"
            Lens..~ rs Lens.^. Lens.field @"nextPageToken"
        )

-- | /See:/ 'mkListPortfoliosForProductResponse' smart constructor.
data ListPortfoliosForProductResponse = ListPortfoliosForProductResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | Information about the portfolios.
    portfolioDetails :: Core.Maybe [Types.PortfolioDetail],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListPortfoliosForProductResponse' value with any optional fields omitted.
mkListPortfoliosForProductResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListPortfoliosForProductResponse
mkListPortfoliosForProductResponse responseStatus =
  ListPortfoliosForProductResponse'
    { nextPageToken = Core.Nothing,
      portfolioDetails = Core.Nothing,
      responseStatus
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfprfrsNextPageToken :: Lens.Lens' ListPortfoliosForProductResponse (Core.Maybe Types.NextPageToken)
lpfprfrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED lpfprfrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Information about the portfolios.
--
-- /Note:/ Consider using 'portfolioDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfprfrsPortfolioDetails :: Lens.Lens' ListPortfoliosForProductResponse (Core.Maybe [Types.PortfolioDetail])
lpfprfrsPortfolioDetails = Lens.field @"portfolioDetails"
{-# DEPRECATED lpfprfrsPortfolioDetails "Use generic-lens or generic-optics with 'portfolioDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfprfrsResponseStatus :: Lens.Lens' ListPortfoliosForProductResponse Core.Int
lpfprfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lpfprfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
