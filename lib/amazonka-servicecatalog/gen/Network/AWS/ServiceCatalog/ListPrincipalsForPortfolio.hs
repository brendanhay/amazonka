{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListPrincipalsForPortfolio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all principal ARNs associated with the specified portfolio.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListPrincipalsForPortfolio
  ( -- * Creating a request
    ListPrincipalsForPortfolio (..),
    mkListPrincipalsForPortfolio,

    -- ** Request lenses
    lpfpPortfolioId,
    lpfpAcceptLanguage,
    lpfpPageSize,
    lpfpPageToken,

    -- * Destructuring the response
    ListPrincipalsForPortfolioResponse (..),
    mkListPrincipalsForPortfolioResponse,

    -- ** Response lenses
    lpfprrsNextPageToken,
    lpfprrsPrincipals,
    lpfprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListPrincipalsForPortfolio' smart constructor.
data ListPrincipalsForPortfolio = ListPrincipalsForPortfolio'
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
    pageToken :: Core.Maybe Types.PageToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPrincipalsForPortfolio' value with any optional fields omitted.
mkListPrincipalsForPortfolio ::
  -- | 'portfolioId'
  Types.PortfolioId ->
  ListPrincipalsForPortfolio
mkListPrincipalsForPortfolio portfolioId =
  ListPrincipalsForPortfolio'
    { portfolioId,
      acceptLanguage = Core.Nothing,
      pageSize = Core.Nothing,
      pageToken = Core.Nothing
    }

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfpPortfolioId :: Lens.Lens' ListPrincipalsForPortfolio Types.PortfolioId
lpfpPortfolioId = Lens.field @"portfolioId"
{-# DEPRECATED lpfpPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

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
lpfpAcceptLanguage :: Lens.Lens' ListPrincipalsForPortfolio (Core.Maybe Types.AcceptLanguage)
lpfpAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED lpfpAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfpPageSize :: Lens.Lens' ListPrincipalsForPortfolio (Core.Maybe Core.Natural)
lpfpPageSize = Lens.field @"pageSize"
{-# DEPRECATED lpfpPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfpPageToken :: Lens.Lens' ListPrincipalsForPortfolio (Core.Maybe Types.PageToken)
lpfpPageToken = Lens.field @"pageToken"
{-# DEPRECATED lpfpPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Core.FromJSON ListPrincipalsForPortfolio where
  toJSON ListPrincipalsForPortfolio {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PortfolioId" Core..= portfolioId),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken
          ]
      )

instance Core.AWSRequest ListPrincipalsForPortfolio where
  type
    Rs ListPrincipalsForPortfolio =
      ListPrincipalsForPortfolioResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.ListPrincipalsForPortfolio"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPrincipalsForPortfolioResponse'
            Core.<$> (x Core..:? "NextPageToken")
            Core.<*> (x Core..:? "Principals")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListPrincipalsForPortfolio where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
      Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"principals" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"pageToken"
            Lens..~ rs Lens.^. Lens.field @"nextPageToken"
        )

-- | /See:/ 'mkListPrincipalsForPortfolioResponse' smart constructor.
data ListPrincipalsForPortfolioResponse = ListPrincipalsForPortfolioResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | The IAM principals (users or roles) associated with the portfolio.
    principals :: Core.Maybe [Types.Principal],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPrincipalsForPortfolioResponse' value with any optional fields omitted.
mkListPrincipalsForPortfolioResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListPrincipalsForPortfolioResponse
mkListPrincipalsForPortfolioResponse responseStatus =
  ListPrincipalsForPortfolioResponse'
    { nextPageToken = Core.Nothing,
      principals = Core.Nothing,
      responseStatus
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfprrsNextPageToken :: Lens.Lens' ListPrincipalsForPortfolioResponse (Core.Maybe Types.NextPageToken)
lpfprrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED lpfprrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The IAM principals (users or roles) associated with the portfolio.
--
-- /Note:/ Consider using 'principals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfprrsPrincipals :: Lens.Lens' ListPrincipalsForPortfolioResponse (Core.Maybe [Types.Principal])
lpfprrsPrincipals = Lens.field @"principals"
{-# DEPRECATED lpfprrsPrincipals "Use generic-lens or generic-optics with 'principals' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpfprrsResponseStatus :: Lens.Lens' ListPrincipalsForPortfolioResponse Core.Int
lpfprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lpfprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
