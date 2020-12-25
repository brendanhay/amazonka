{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListConstraintsForPortfolio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the constraints for the specified portfolio and product.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListConstraintsForPortfolio
  ( -- * Creating a request
    ListConstraintsForPortfolio (..),
    mkListConstraintsForPortfolio,

    -- ** Request lenses
    lcfpPortfolioId,
    lcfpAcceptLanguage,
    lcfpPageSize,
    lcfpPageToken,
    lcfpProductId,

    -- * Destructuring the response
    ListConstraintsForPortfolioResponse (..),
    mkListConstraintsForPortfolioResponse,

    -- ** Response lenses
    lcfprrsConstraintDetails,
    lcfprrsNextPageToken,
    lcfprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListConstraintsForPortfolio' smart constructor.
data ListConstraintsForPortfolio = ListConstraintsForPortfolio'
  { -- | The portfolio identifier.
    portfolioId :: Types.PortfolioId,
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
    pageToken :: Core.Maybe Types.PageToken,
    -- | The product identifier.
    productId :: Core.Maybe Types.ProductId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListConstraintsForPortfolio' value with any optional fields omitted.
mkListConstraintsForPortfolio ::
  -- | 'portfolioId'
  Types.PortfolioId ->
  ListConstraintsForPortfolio
mkListConstraintsForPortfolio portfolioId =
  ListConstraintsForPortfolio'
    { portfolioId,
      acceptLanguage = Core.Nothing,
      pageSize = Core.Nothing,
      pageToken = Core.Nothing,
      productId = Core.Nothing
    }

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfpPortfolioId :: Lens.Lens' ListConstraintsForPortfolio Types.PortfolioId
lcfpPortfolioId = Lens.field @"portfolioId"
{-# DEPRECATED lcfpPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

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
lcfpAcceptLanguage :: Lens.Lens' ListConstraintsForPortfolio (Core.Maybe Types.AcceptLanguage)
lcfpAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED lcfpAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfpPageSize :: Lens.Lens' ListConstraintsForPortfolio (Core.Maybe Core.Natural)
lcfpPageSize = Lens.field @"pageSize"
{-# DEPRECATED lcfpPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfpPageToken :: Lens.Lens' ListConstraintsForPortfolio (Core.Maybe Types.PageToken)
lcfpPageToken = Lens.field @"pageToken"
{-# DEPRECATED lcfpPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfpProductId :: Lens.Lens' ListConstraintsForPortfolio (Core.Maybe Types.ProductId)
lcfpProductId = Lens.field @"productId"
{-# DEPRECATED lcfpProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

instance Core.FromJSON ListConstraintsForPortfolio where
  toJSON ListConstraintsForPortfolio {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PortfolioId" Core..= portfolioId),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("ProductId" Core..=) Core.<$> productId
          ]
      )

instance Core.AWSRequest ListConstraintsForPortfolio where
  type
    Rs ListConstraintsForPortfolio =
      ListConstraintsForPortfolioResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.ListConstraintsForPortfolio"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConstraintsForPortfolioResponse'
            Core.<$> (x Core..:? "ConstraintDetails")
            Core.<*> (x Core..:? "NextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListConstraintsForPortfolio where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
      Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"constraintDetails" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"pageToken"
            Lens..~ rs Lens.^. Lens.field @"nextPageToken"
        )

-- | /See:/ 'mkListConstraintsForPortfolioResponse' smart constructor.
data ListConstraintsForPortfolioResponse = ListConstraintsForPortfolioResponse'
  { -- | Information about the constraints.
    constraintDetails :: Core.Maybe [Types.ConstraintDetail],
    -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListConstraintsForPortfolioResponse' value with any optional fields omitted.
mkListConstraintsForPortfolioResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListConstraintsForPortfolioResponse
mkListConstraintsForPortfolioResponse responseStatus =
  ListConstraintsForPortfolioResponse'
    { constraintDetails =
        Core.Nothing,
      nextPageToken = Core.Nothing,
      responseStatus
    }

-- | Information about the constraints.
--
-- /Note:/ Consider using 'constraintDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfprrsConstraintDetails :: Lens.Lens' ListConstraintsForPortfolioResponse (Core.Maybe [Types.ConstraintDetail])
lcfprrsConstraintDetails = Lens.field @"constraintDetails"
{-# DEPRECATED lcfprrsConstraintDetails "Use generic-lens or generic-optics with 'constraintDetails' instead." #-}

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfprrsNextPageToken :: Lens.Lens' ListConstraintsForPortfolioResponse (Core.Maybe Types.NextPageToken)
lcfprrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED lcfprrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfprrsResponseStatus :: Lens.Lens' ListConstraintsForPortfolioResponse Core.Int
lcfprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcfprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
