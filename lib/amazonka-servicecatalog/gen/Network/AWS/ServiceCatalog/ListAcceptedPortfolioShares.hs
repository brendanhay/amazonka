{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListAcceptedPortfolioShares
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all portfolios for which sharing was accepted by this account.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListAcceptedPortfolioShares
  ( -- * Creating a request
    ListAcceptedPortfolioShares (..),
    mkListAcceptedPortfolioShares,

    -- ** Request lenses
    lapsAcceptLanguage,
    lapsPageSize,
    lapsPageToken,
    lapsPortfolioShareType,

    -- * Destructuring the response
    ListAcceptedPortfolioSharesResponse (..),
    mkListAcceptedPortfolioSharesResponse,

    -- ** Response lenses
    lapsrrsNextPageToken,
    lapsrrsPortfolioDetails,
    lapsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListAcceptedPortfolioShares' smart constructor.
data ListAcceptedPortfolioShares = ListAcceptedPortfolioShares'
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
    -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of results, use null.
    pageToken :: Core.Maybe Types.PageToken,
    -- | The type of shared portfolios to list. The default is to list imported portfolios.
    --
    --
    --     * @AWS_ORGANIZATIONS@ - List portfolios shared by the management account of your organization
    --
    --
    --     * @AWS_SERVICECATALOG@ - List default portfolios
    --
    --
    --     * @IMPORTED@ - List imported portfolios
    portfolioShareType :: Core.Maybe Types.PortfolioShareType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAcceptedPortfolioShares' value with any optional fields omitted.
mkListAcceptedPortfolioShares ::
  ListAcceptedPortfolioShares
mkListAcceptedPortfolioShares =
  ListAcceptedPortfolioShares'
    { acceptLanguage = Core.Nothing,
      pageSize = Core.Nothing,
      pageToken = Core.Nothing,
      portfolioShareType = Core.Nothing
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
lapsAcceptLanguage :: Lens.Lens' ListAcceptedPortfolioShares (Core.Maybe Types.AcceptLanguage)
lapsAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED lapsAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lapsPageSize :: Lens.Lens' ListAcceptedPortfolioShares (Core.Maybe Core.Natural)
lapsPageSize = Lens.field @"pageSize"
{-# DEPRECATED lapsPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lapsPageToken :: Lens.Lens' ListAcceptedPortfolioShares (Core.Maybe Types.PageToken)
lapsPageToken = Lens.field @"pageToken"
{-# DEPRECATED lapsPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The type of shared portfolios to list. The default is to list imported portfolios.
--
--
--     * @AWS_ORGANIZATIONS@ - List portfolios shared by the management account of your organization
--
--
--     * @AWS_SERVICECATALOG@ - List default portfolios
--
--
--     * @IMPORTED@ - List imported portfolios
--
--
--
-- /Note:/ Consider using 'portfolioShareType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lapsPortfolioShareType :: Lens.Lens' ListAcceptedPortfolioShares (Core.Maybe Types.PortfolioShareType)
lapsPortfolioShareType = Lens.field @"portfolioShareType"
{-# DEPRECATED lapsPortfolioShareType "Use generic-lens or generic-optics with 'portfolioShareType' instead." #-}

instance Core.FromJSON ListAcceptedPortfolioShares where
  toJSON ListAcceptedPortfolioShares {..} =
    Core.object
      ( Core.catMaybes
          [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("PortfolioShareType" Core..=) Core.<$> portfolioShareType
          ]
      )

instance Core.AWSRequest ListAcceptedPortfolioShares where
  type
    Rs ListAcceptedPortfolioShares =
      ListAcceptedPortfolioSharesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.ListAcceptedPortfolioShares"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAcceptedPortfolioSharesResponse'
            Core.<$> (x Core..:? "NextPageToken")
            Core.<*> (x Core..:? "PortfolioDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListAcceptedPortfolioShares where
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

-- | /See:/ 'mkListAcceptedPortfolioSharesResponse' smart constructor.
data ListAcceptedPortfolioSharesResponse = ListAcceptedPortfolioSharesResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | Information about the portfolios.
    portfolioDetails :: Core.Maybe [Types.PortfolioDetail],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListAcceptedPortfolioSharesResponse' value with any optional fields omitted.
mkListAcceptedPortfolioSharesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAcceptedPortfolioSharesResponse
mkListAcceptedPortfolioSharesResponse responseStatus =
  ListAcceptedPortfolioSharesResponse'
    { nextPageToken =
        Core.Nothing,
      portfolioDetails = Core.Nothing,
      responseStatus
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lapsrrsNextPageToken :: Lens.Lens' ListAcceptedPortfolioSharesResponse (Core.Maybe Types.NextPageToken)
lapsrrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED lapsrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Information about the portfolios.
--
-- /Note:/ Consider using 'portfolioDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lapsrrsPortfolioDetails :: Lens.Lens' ListAcceptedPortfolioSharesResponse (Core.Maybe [Types.PortfolioDetail])
lapsrrsPortfolioDetails = Lens.field @"portfolioDetails"
{-# DEPRECATED lapsrrsPortfolioDetails "Use generic-lens or generic-optics with 'portfolioDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lapsrrsResponseStatus :: Lens.Lens' ListAcceptedPortfolioSharesResponse Core.Int
lapsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lapsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
