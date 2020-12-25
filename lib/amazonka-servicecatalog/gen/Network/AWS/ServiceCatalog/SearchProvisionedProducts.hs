{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    SearchProvisionedProducts (..),
    mkSearchProvisionedProducts,

    -- ** Request lenses
    sppAcceptLanguage,
    sppAccessLevelFilter,
    sppFilters,
    sppPageSize,
    sppPageToken,
    sppSortBy,
    sppSortOrder,

    -- * Destructuring the response
    SearchProvisionedProductsResponse (..),
    mkSearchProvisionedProductsResponse,

    -- ** Response lenses
    spprrsNextPageToken,
    spprrsProvisionedProducts,
    spprrsTotalResultsCount,
    spprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkSearchProvisionedProducts' smart constructor.
data SearchProvisionedProducts = SearchProvisionedProducts'
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
    -- | The access level to use to obtain results. The default is @User@ .
    accessLevelFilter :: Core.Maybe Types.AccessLevelFilter,
    -- | The search filters.
    --
    -- When the key is @SearchQuery@ , the searchable fields are @arn@ , @createdTime@ , @id@ , @lastRecordId@ , @idempotencyToken@ , @name@ , @physicalId@ , @productId@ , @provisioningArtifact@ , @type@ , @status@ , @tags@ , @userArn@ , @userArnSession@ , @lastProvisioningRecordId@ , @lastSuccessfulProvisioningRecordId@ , @productName@ , and @provisioningArtifactName@ .
    -- Example: @"SearchQuery":["status:AVAILABLE"]@
    filters :: Core.Maybe (Core.HashMap Types.ProvisionedProductViewFilterBy [Types.ProvisionedProductViewFilterValue]),
    -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of results, use null.
    pageToken :: Core.Maybe Types.PageToken,
    -- | The sort field. If no value is specified, the results are not sorted. The valid values are @arn@ , @id@ , @name@ , and @lastRecordId@ .
    sortBy :: Core.Maybe Types.SortBy,
    -- | The sort order. If no value is specified, the results are not sorted.
    sortOrder :: Core.Maybe Types.SortOrder
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchProvisionedProducts' value with any optional fields omitted.
mkSearchProvisionedProducts ::
  SearchProvisionedProducts
mkSearchProvisionedProducts =
  SearchProvisionedProducts'
    { acceptLanguage = Core.Nothing,
      accessLevelFilter = Core.Nothing,
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
sppAcceptLanguage :: Lens.Lens' SearchProvisionedProducts (Core.Maybe Types.AcceptLanguage)
sppAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED sppAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The access level to use to obtain results. The default is @User@ .
--
-- /Note:/ Consider using 'accessLevelFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppAccessLevelFilter :: Lens.Lens' SearchProvisionedProducts (Core.Maybe Types.AccessLevelFilter)
sppAccessLevelFilter = Lens.field @"accessLevelFilter"
{-# DEPRECATED sppAccessLevelFilter "Use generic-lens or generic-optics with 'accessLevelFilter' instead." #-}

-- | The search filters.
--
-- When the key is @SearchQuery@ , the searchable fields are @arn@ , @createdTime@ , @id@ , @lastRecordId@ , @idempotencyToken@ , @name@ , @physicalId@ , @productId@ , @provisioningArtifact@ , @type@ , @status@ , @tags@ , @userArn@ , @userArnSession@ , @lastProvisioningRecordId@ , @lastSuccessfulProvisioningRecordId@ , @productName@ , and @provisioningArtifactName@ .
-- Example: @"SearchQuery":["status:AVAILABLE"]@
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppFilters :: Lens.Lens' SearchProvisionedProducts (Core.Maybe (Core.HashMap Types.ProvisionedProductViewFilterBy [Types.ProvisionedProductViewFilterValue]))
sppFilters = Lens.field @"filters"
{-# DEPRECATED sppFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppPageSize :: Lens.Lens' SearchProvisionedProducts (Core.Maybe Core.Natural)
sppPageSize = Lens.field @"pageSize"
{-# DEPRECATED sppPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppPageToken :: Lens.Lens' SearchProvisionedProducts (Core.Maybe Types.PageToken)
sppPageToken = Lens.field @"pageToken"
{-# DEPRECATED sppPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The sort field. If no value is specified, the results are not sorted. The valid values are @arn@ , @id@ , @name@ , and @lastRecordId@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppSortBy :: Lens.Lens' SearchProvisionedProducts (Core.Maybe Types.SortBy)
sppSortBy = Lens.field @"sortBy"
{-# DEPRECATED sppSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order. If no value is specified, the results are not sorted.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppSortOrder :: Lens.Lens' SearchProvisionedProducts (Core.Maybe Types.SortOrder)
sppSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED sppSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON SearchProvisionedProducts where
  toJSON SearchProvisionedProducts {..} =
    Core.object
      ( Core.catMaybes
          [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("AccessLevelFilter" Core..=) Core.<$> accessLevelFilter,
            ("Filters" Core..=) Core.<$> filters,
            ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("SortOrder" Core..=) Core.<$> sortOrder
          ]
      )

instance Core.AWSRequest SearchProvisionedProducts where
  type
    Rs SearchProvisionedProducts =
      SearchProvisionedProductsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.SearchProvisionedProducts"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchProvisionedProductsResponse'
            Core.<$> (x Core..:? "NextPageToken")
            Core.<*> (x Core..:? "ProvisionedProducts")
            Core.<*> (x Core..:? "TotalResultsCount")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSearchProvisionedProductsResponse' smart constructor.
data SearchProvisionedProductsResponse = SearchProvisionedProductsResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | Information about the provisioned products.
    provisionedProducts :: Core.Maybe [Types.ProvisionedProductAttribute],
    -- | The number of provisioned products found.
    totalResultsCount :: Core.Maybe Core.Int,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SearchProvisionedProductsResponse' value with any optional fields omitted.
mkSearchProvisionedProductsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SearchProvisionedProductsResponse
mkSearchProvisionedProductsResponse responseStatus =
  SearchProvisionedProductsResponse'
    { nextPageToken = Core.Nothing,
      provisionedProducts = Core.Nothing,
      totalResultsCount = Core.Nothing,
      responseStatus
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprrsNextPageToken :: Lens.Lens' SearchProvisionedProductsResponse (Core.Maybe Types.NextPageToken)
spprrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED spprrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Information about the provisioned products.
--
-- /Note:/ Consider using 'provisionedProducts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprrsProvisionedProducts :: Lens.Lens' SearchProvisionedProductsResponse (Core.Maybe [Types.ProvisionedProductAttribute])
spprrsProvisionedProducts = Lens.field @"provisionedProducts"
{-# DEPRECATED spprrsProvisionedProducts "Use generic-lens or generic-optics with 'provisionedProducts' instead." #-}

-- | The number of provisioned products found.
--
-- /Note:/ Consider using 'totalResultsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprrsTotalResultsCount :: Lens.Lens' SearchProvisionedProductsResponse (Core.Maybe Core.Int)
spprrsTotalResultsCount = Lens.field @"totalResultsCount"
{-# DEPRECATED spprrsTotalResultsCount "Use generic-lens or generic-optics with 'totalResultsCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spprrsResponseStatus :: Lens.Lens' SearchProvisionedProductsResponse Core.Int
spprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED spprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
