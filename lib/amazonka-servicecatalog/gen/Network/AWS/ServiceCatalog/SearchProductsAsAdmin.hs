{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.SearchProductsAsAdmin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the products for the specified portfolio or all products.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.SearchProductsAsAdmin
    (
    -- * Creating a request
      SearchProductsAsAdmin (..)
    , mkSearchProductsAsAdmin
    -- ** Request lenses
    , spaaAcceptLanguage
    , spaaFilters
    , spaaPageSize
    , spaaPageToken
    , spaaPortfolioId
    , spaaProductSource
    , spaaSortBy
    , spaaSortOrder

    -- * Destructuring the response
    , SearchProductsAsAdminResponse (..)
    , mkSearchProductsAsAdminResponse
    -- ** Response lenses
    , spaarrsNextPageToken
    , spaarrsProductViewDetails
    , spaarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkSearchProductsAsAdmin' smart constructor.
data SearchProductsAsAdmin = SearchProductsAsAdmin'
  { acceptLanguage :: Core.Maybe Types.AcceptLanguage
    -- ^ The language code.
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
  , filters :: Core.Maybe (Core.HashMap Types.ProductViewFilterBy [Types.ProductViewFilterValue])
    -- ^ The search filters. If no search filters are specified, the output includes all products to which the administrator has access.
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return with this call.
  , pageToken :: Core.Maybe Types.PageToken
    -- ^ The page token for the next set of results. To retrieve the first set of results, use null.
  , portfolioId :: Core.Maybe Types.Id
    -- ^ The portfolio identifier.
  , productSource :: Core.Maybe Types.ProductSource
    -- ^ Access level of the source of the product.
  , sortBy :: Core.Maybe Types.ProductViewSortBy
    -- ^ The sort field. If no value is specified, the results are not sorted.
  , sortOrder :: Core.Maybe Types.SortOrder
    -- ^ The sort order. If no value is specified, the results are not sorted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchProductsAsAdmin' value with any optional fields omitted.
mkSearchProductsAsAdmin
    :: SearchProductsAsAdmin
mkSearchProductsAsAdmin
  = SearchProductsAsAdmin'{acceptLanguage = Core.Nothing,
                           filters = Core.Nothing, pageSize = Core.Nothing,
                           pageToken = Core.Nothing, portfolioId = Core.Nothing,
                           productSource = Core.Nothing, sortBy = Core.Nothing,
                           sortOrder = Core.Nothing}

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
spaaAcceptLanguage :: Lens.Lens' SearchProductsAsAdmin (Core.Maybe Types.AcceptLanguage)
spaaAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE spaaAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The search filters. If no search filters are specified, the output includes all products to which the administrator has access.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaaFilters :: Lens.Lens' SearchProductsAsAdmin (Core.Maybe (Core.HashMap Types.ProductViewFilterBy [Types.ProductViewFilterValue]))
spaaFilters = Lens.field @"filters"
{-# INLINEABLE spaaFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaaPageSize :: Lens.Lens' SearchProductsAsAdmin (Core.Maybe Core.Natural)
spaaPageSize = Lens.field @"pageSize"
{-# INLINEABLE spaaPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaaPageToken :: Lens.Lens' SearchProductsAsAdmin (Core.Maybe Types.PageToken)
spaaPageToken = Lens.field @"pageToken"
{-# INLINEABLE spaaPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaaPortfolioId :: Lens.Lens' SearchProductsAsAdmin (Core.Maybe Types.Id)
spaaPortfolioId = Lens.field @"portfolioId"
{-# INLINEABLE spaaPortfolioId #-}
{-# DEPRECATED portfolioId "Use generic-lens or generic-optics with 'portfolioId' instead"  #-}

-- | Access level of the source of the product.
--
-- /Note:/ Consider using 'productSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaaProductSource :: Lens.Lens' SearchProductsAsAdmin (Core.Maybe Types.ProductSource)
spaaProductSource = Lens.field @"productSource"
{-# INLINEABLE spaaProductSource #-}
{-# DEPRECATED productSource "Use generic-lens or generic-optics with 'productSource' instead"  #-}

-- | The sort field. If no value is specified, the results are not sorted.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaaSortBy :: Lens.Lens' SearchProductsAsAdmin (Core.Maybe Types.ProductViewSortBy)
spaaSortBy = Lens.field @"sortBy"
{-# INLINEABLE spaaSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | The sort order. If no value is specified, the results are not sorted.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaaSortOrder :: Lens.Lens' SearchProductsAsAdmin (Core.Maybe Types.SortOrder)
spaaSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE spaaSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

instance Core.ToQuery SearchProductsAsAdmin where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SearchProductsAsAdmin where
        toHeaders SearchProductsAsAdmin{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.SearchProductsAsAdmin")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SearchProductsAsAdmin where
        toJSON SearchProductsAsAdmin{..}
          = Core.object
              (Core.catMaybes
                 [("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("Filters" Core..=) Core.<$> filters,
                  ("PageSize" Core..=) Core.<$> pageSize,
                  ("PageToken" Core..=) Core.<$> pageToken,
                  ("PortfolioId" Core..=) Core.<$> portfolioId,
                  ("ProductSource" Core..=) Core.<$> productSource,
                  ("SortBy" Core..=) Core.<$> sortBy,
                  ("SortOrder" Core..=) Core.<$> sortOrder])

instance Core.AWSRequest SearchProductsAsAdmin where
        type Rs SearchProductsAsAdmin = SearchProductsAsAdminResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SearchProductsAsAdminResponse' Core.<$>
                   (x Core..:? "NextPageToken") Core.<*>
                     x Core..:? "ProductViewDetails"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager SearchProductsAsAdmin where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"productViewDetails" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | /See:/ 'mkSearchProductsAsAdminResponse' smart constructor.
data SearchProductsAsAdminResponse = SearchProductsAsAdminResponse'
  { nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
  , productViewDetails :: Core.Maybe [Types.ProductViewDetail]
    -- ^ Information about the product views.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SearchProductsAsAdminResponse' value with any optional fields omitted.
mkSearchProductsAsAdminResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SearchProductsAsAdminResponse
mkSearchProductsAsAdminResponse responseStatus
  = SearchProductsAsAdminResponse'{nextPageToken = Core.Nothing,
                                   productViewDetails = Core.Nothing, responseStatus}

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaarrsNextPageToken :: Lens.Lens' SearchProductsAsAdminResponse (Core.Maybe Types.NextPageToken)
spaarrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE spaarrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | Information about the product views.
--
-- /Note:/ Consider using 'productViewDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaarrsProductViewDetails :: Lens.Lens' SearchProductsAsAdminResponse (Core.Maybe [Types.ProductViewDetail])
spaarrsProductViewDetails = Lens.field @"productViewDetails"
{-# INLINEABLE spaarrsProductViewDetails #-}
{-# DEPRECATED productViewDetails "Use generic-lens or generic-optics with 'productViewDetails' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spaarrsResponseStatus :: Lens.Lens' SearchProductsAsAdminResponse Core.Int
spaarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE spaarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
