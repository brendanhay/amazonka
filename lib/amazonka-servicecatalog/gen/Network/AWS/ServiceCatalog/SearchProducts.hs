{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.SearchProducts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the products to which the caller has access.
module Network.AWS.ServiceCatalog.SearchProducts
  ( -- * Creating a request
    SearchProducts (..),
    mkSearchProducts,

    -- ** Request lenses
    spAcceptLanguage,
    spFilters,
    spPageSize,
    spPageToken,
    spSortBy,
    spSortOrder,

    -- * Destructuring the response
    SearchProductsResponse (..),
    mkSearchProductsResponse,

    -- ** Response lenses
    sprrsNextPageToken,
    sprrsProductViewAggregations,
    sprrsProductViewSummaries,
    sprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkSearchProducts' smart constructor.
data SearchProducts = SearchProducts'
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
    -- | The search filters. If no search filters are specified, the output includes all products to which the caller has access.
    filters :: Core.Maybe (Core.HashMap Types.ProductViewFilterBy [Types.ProductViewFilterValue]),
    -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of results, use null.
    pageToken :: Core.Maybe Types.PageToken,
    -- | The sort field. If no value is specified, the results are not sorted.
    sortBy :: Core.Maybe Types.ProductViewSortBy,
    -- | The sort order. If no value is specified, the results are not sorted.
    sortOrder :: Core.Maybe Types.SortOrder
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchProducts' value with any optional fields omitted.
mkSearchProducts ::
  SearchProducts
mkSearchProducts =
  SearchProducts'
    { acceptLanguage = Core.Nothing,
      filters = Core.Nothing,
      pageSize = Core.Nothing,
      pageToken = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing
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
spAcceptLanguage :: Lens.Lens' SearchProducts (Core.Maybe Types.AcceptLanguage)
spAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED spAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The search filters. If no search filters are specified, the output includes all products to which the caller has access.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spFilters :: Lens.Lens' SearchProducts (Core.Maybe (Core.HashMap Types.ProductViewFilterBy [Types.ProductViewFilterValue]))
spFilters = Lens.field @"filters"
{-# DEPRECATED spFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spPageSize :: Lens.Lens' SearchProducts (Core.Maybe Core.Natural)
spPageSize = Lens.field @"pageSize"
{-# DEPRECATED spPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spPageToken :: Lens.Lens' SearchProducts (Core.Maybe Types.PageToken)
spPageToken = Lens.field @"pageToken"
{-# DEPRECATED spPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The sort field. If no value is specified, the results are not sorted.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spSortBy :: Lens.Lens' SearchProducts (Core.Maybe Types.ProductViewSortBy)
spSortBy = Lens.field @"sortBy"
{-# DEPRECATED spSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order. If no value is specified, the results are not sorted.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spSortOrder :: Lens.Lens' SearchProducts (Core.Maybe Types.SortOrder)
spSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED spSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON SearchProducts where
  toJSON SearchProducts {..} =
    Core.object
      ( Core.catMaybes
          [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("Filters" Core..=) Core.<$> filters,
            ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("SortOrder" Core..=) Core.<$> sortOrder
          ]
      )

instance Core.AWSRequest SearchProducts where
  type Rs SearchProducts = SearchProductsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWS242ServiceCatalogService.SearchProducts")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchProductsResponse'
            Core.<$> (x Core..:? "NextPageToken")
            Core.<*> (x Core..:? "ProductViewAggregations")
            Core.<*> (x Core..:? "ProductViewSummaries")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSearchProductsResponse' smart constructor.
data SearchProductsResponse = SearchProductsResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | The product view aggregations.
    productViewAggregations :: Core.Maybe (Core.HashMap Types.ProductViewAggregationType [Types.ProductViewAggregationValue]),
    -- | Information about the product views.
    productViewSummaries :: Core.Maybe [Types.ProductViewSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchProductsResponse' value with any optional fields omitted.
mkSearchProductsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SearchProductsResponse
mkSearchProductsResponse responseStatus =
  SearchProductsResponse'
    { nextPageToken = Core.Nothing,
      productViewAggregations = Core.Nothing,
      productViewSummaries = Core.Nothing,
      responseStatus
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprrsNextPageToken :: Lens.Lens' SearchProductsResponse (Core.Maybe Types.NextPageToken)
sprrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED sprrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The product view aggregations.
--
-- /Note:/ Consider using 'productViewAggregations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprrsProductViewAggregations :: Lens.Lens' SearchProductsResponse (Core.Maybe (Core.HashMap Types.ProductViewAggregationType [Types.ProductViewAggregationValue]))
sprrsProductViewAggregations = Lens.field @"productViewAggregations"
{-# DEPRECATED sprrsProductViewAggregations "Use generic-lens or generic-optics with 'productViewAggregations' instead." #-}

-- | Information about the product views.
--
-- /Note:/ Consider using 'productViewSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprrsProductViewSummaries :: Lens.Lens' SearchProductsResponse (Core.Maybe [Types.ProductViewSummary])
sprrsProductViewSummaries = Lens.field @"productViewSummaries"
{-# DEPRECATED sprrsProductViewSummaries "Use generic-lens or generic-optics with 'productViewSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprrsResponseStatus :: Lens.Lens' SearchProductsResponse Core.Int
sprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
