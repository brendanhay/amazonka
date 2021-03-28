{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.SearchProvisionedProducts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the provisioned products that meet the specified criteria.
module Network.AWS.ServiceCatalog.SearchProvisionedProducts
    (
    -- * Creating a request
      SearchProvisionedProducts (..)
    , mkSearchProvisionedProducts
    -- ** Request lenses
    , sppAcceptLanguage
    , sppAccessLevelFilter
    , sppFilters
    , sppPageSize
    , sppPageToken
    , sppSortBy
    , sppSortOrder

    -- * Destructuring the response
    , SearchProvisionedProductsResponse (..)
    , mkSearchProvisionedProductsResponse
    -- ** Response lenses
    , spprrsNextPageToken
    , spprrsProvisionedProducts
    , spprrsTotalResultsCount
    , spprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkSearchProvisionedProducts' smart constructor.
data SearchProvisionedProducts = SearchProvisionedProducts'
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
  , accessLevelFilter :: Core.Maybe Types.AccessLevelFilter
    -- ^ The access level to use to obtain results. The default is @User@ .
  , filters :: Core.Maybe (Core.HashMap Types.ProvisionedProductViewFilterBy [Types.ProvisionedProductViewFilterValue])
    -- ^ The search filters.
--
-- When the key is @SearchQuery@ , the searchable fields are @arn@ , @createdTime@ , @id@ , @lastRecordId@ , @idempotencyToken@ , @name@ , @physicalId@ , @productId@ , @provisioningArtifact@ , @type@ , @status@ , @tags@ , @userArn@ , @userArnSession@ , @lastProvisioningRecordId@ , @lastSuccessfulProvisioningRecordId@ , @productName@ , and @provisioningArtifactName@ .
-- Example: @"SearchQuery":["status:AVAILABLE"]@ 
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return with this call.
  , pageToken :: Core.Maybe Types.PageToken
    -- ^ The page token for the next set of results. To retrieve the first set of results, use null.
  , sortBy :: Core.Maybe Types.SortBy
    -- ^ The sort field. If no value is specified, the results are not sorted. The valid values are @arn@ , @id@ , @name@ , and @lastRecordId@ .
  , sortOrder :: Core.Maybe Types.SortOrder
    -- ^ The sort order. If no value is specified, the results are not sorted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchProvisionedProducts' value with any optional fields omitted.
mkSearchProvisionedProducts
    :: SearchProvisionedProducts
mkSearchProvisionedProducts
  = SearchProvisionedProducts'{acceptLanguage = Core.Nothing,
                               accessLevelFilter = Core.Nothing, filters = Core.Nothing,
                               pageSize = Core.Nothing, pageToken = Core.Nothing,
                               sortBy = Core.Nothing, sortOrder = Core.Nothing}

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
sppAcceptLanguage :: Lens.Lens' SearchProvisionedProducts (Core.Maybe Types.AcceptLanguage)
sppAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE sppAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The access level to use to obtain results. The default is @User@ .
--
-- /Note:/ Consider using 'accessLevelFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppAccessLevelFilter :: Lens.Lens' SearchProvisionedProducts (Core.Maybe Types.AccessLevelFilter)
sppAccessLevelFilter = Lens.field @"accessLevelFilter"
{-# INLINEABLE sppAccessLevelFilter #-}
{-# DEPRECATED accessLevelFilter "Use generic-lens or generic-optics with 'accessLevelFilter' instead"  #-}

-- | The search filters.
--
-- When the key is @SearchQuery@ , the searchable fields are @arn@ , @createdTime@ , @id@ , @lastRecordId@ , @idempotencyToken@ , @name@ , @physicalId@ , @productId@ , @provisioningArtifact@ , @type@ , @status@ , @tags@ , @userArn@ , @userArnSession@ , @lastProvisioningRecordId@ , @lastSuccessfulProvisioningRecordId@ , @productName@ , and @provisioningArtifactName@ .
-- Example: @"SearchQuery":["status:AVAILABLE"]@ 
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppFilters :: Lens.Lens' SearchProvisionedProducts (Core.Maybe (Core.HashMap Types.ProvisionedProductViewFilterBy [Types.ProvisionedProductViewFilterValue]))
sppFilters = Lens.field @"filters"
{-# INLINEABLE sppFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppPageSize :: Lens.Lens' SearchProvisionedProducts (Core.Maybe Core.Natural)
sppPageSize = Lens.field @"pageSize"
{-# INLINEABLE sppPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppPageToken :: Lens.Lens' SearchProvisionedProducts (Core.Maybe Types.PageToken)
sppPageToken = Lens.field @"pageToken"
{-# INLINEABLE sppPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

-- | The sort field. If no value is specified, the results are not sorted. The valid values are @arn@ , @id@ , @name@ , and @lastRecordId@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppSortBy :: Lens.Lens' SearchProvisionedProducts (Core.Maybe Types.SortBy)
sppSortBy = Lens.field @"sortBy"
{-# INLINEABLE sppSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | The sort order. If no value is specified, the results are not sorted.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppSortOrder :: Lens.Lens' SearchProvisionedProducts (Core.Maybe Types.SortOrder)
sppSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE sppSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

instance Core.ToQuery SearchProvisionedProducts where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SearchProvisionedProducts where
        toHeaders SearchProvisionedProducts{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.SearchProvisionedProducts")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SearchProvisionedProducts where
        toJSON SearchProvisionedProducts{..}
          = Core.object
              (Core.catMaybes
                 [("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("AccessLevelFilter" Core..=) Core.<$> accessLevelFilter,
                  ("Filters" Core..=) Core.<$> filters,
                  ("PageSize" Core..=) Core.<$> pageSize,
                  ("PageToken" Core..=) Core.<$> pageToken,
                  ("SortBy" Core..=) Core.<$> sortBy,
                  ("SortOrder" Core..=) Core.<$> sortOrder])

instance Core.AWSRequest SearchProvisionedProducts where
        type Rs SearchProvisionedProducts =
             SearchProvisionedProductsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SearchProvisionedProductsResponse' Core.<$>
                   (x Core..:? "NextPageToken") Core.<*>
                     x Core..:? "ProvisionedProducts"
                     Core.<*> x Core..:? "TotalResultsCount"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSearchProvisionedProductsResponse' smart constructor.
data SearchProvisionedProductsResponse = SearchProvisionedProductsResponse'
  { nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
  , provisionedProducts :: Core.Maybe [Types.ProvisionedProductAttribute]
    -- ^ Information about the provisioned products.
  , totalResultsCount :: Core.Maybe Core.Int
    -- ^ The number of provisioned products found.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SearchProvisionedProductsResponse' value with any optional fields omitted.
mkSearchProvisionedProductsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SearchProvisionedProductsResponse
mkSearchProvisionedProductsResponse responseStatus
  = SearchProvisionedProductsResponse'{nextPageToken = Core.Nothing,
                                       provisionedProducts = Core.Nothing,
                                       totalResultsCount = Core.Nothing, responseStatus}

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprrsNextPageToken :: Lens.Lens' SearchProvisionedProductsResponse (Core.Maybe Types.NextPageToken)
spprrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE spprrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | Information about the provisioned products.
--
-- /Note:/ Consider using 'provisionedProducts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprrsProvisionedProducts :: Lens.Lens' SearchProvisionedProductsResponse (Core.Maybe [Types.ProvisionedProductAttribute])
spprrsProvisionedProducts = Lens.field @"provisionedProducts"
{-# INLINEABLE spprrsProvisionedProducts #-}
{-# DEPRECATED provisionedProducts "Use generic-lens or generic-optics with 'provisionedProducts' instead"  #-}

-- | The number of provisioned products found.
--
-- /Note:/ Consider using 'totalResultsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprrsTotalResultsCount :: Lens.Lens' SearchProvisionedProductsResponse (Core.Maybe Core.Int)
spprrsTotalResultsCount = Lens.field @"totalResultsCount"
{-# INLINEABLE spprrsTotalResultsCount #-}
{-# DEPRECATED totalResultsCount "Use generic-lens or generic-optics with 'totalResultsCount' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprrsResponseStatus :: Lens.Lens' SearchProvisionedProductsResponse Core.Int
spprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE spprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
