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
    sppFilters,
    sppSortOrder,
    sppAcceptLanguage,
    sppAccessLevelFilter,
    sppPageToken,
    sppPageSize,
    sppSortBy,

    -- * Destructuring the response
    SearchProvisionedProductsResponse (..),
    mkSearchProvisionedProductsResponse,

    -- ** Response lenses
    srsNextPageToken,
    srsProvisionedProducts,
    srsTotalResultsCount,
    srsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkSearchProvisionedProducts' smart constructor.
data SearchProvisionedProducts = SearchProvisionedProducts'
  { -- | The search filters.
    --
    -- When the key is @SearchQuery@ , the searchable fields are @arn@ , @createdTime@ , @id@ , @lastRecordId@ , @idempotencyToken@ , @name@ , @physicalId@ , @productId@ , @provisioningArtifact@ , @type@ , @status@ , @tags@ , @userArn@ , @userArnSession@ , @lastProvisioningRecordId@ , @lastSuccessfulProvisioningRecordId@ , @productName@ , and @provisioningArtifactName@ .
    -- Example: @"SearchQuery":["status:AVAILABLE"]@
    filters :: Lude.Maybe (Lude.HashMap ProvisionedProductViewFilterBy ([Lude.Text])),
    -- | The sort order. If no value is specified, the results are not sorted.
    sortOrder :: Lude.Maybe SortOrder,
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
    acceptLanguage :: Lude.Maybe Lude.Text,
    -- | The access level to use to obtain results. The default is @User@ .
    accessLevelFilter :: Lude.Maybe AccessLevelFilter,
    -- | The page token for the next set of results. To retrieve the first set of results, use null.
    pageToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Lude.Maybe Lude.Natural,
    -- | The sort field. If no value is specified, the results are not sorted. The valid values are @arn@ , @id@ , @name@ , and @lastRecordId@ .
    sortBy :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchProvisionedProducts' with the minimum fields required to make a request.
--
-- * 'filters' - The search filters.
--
-- When the key is @SearchQuery@ , the searchable fields are @arn@ , @createdTime@ , @id@ , @lastRecordId@ , @idempotencyToken@ , @name@ , @physicalId@ , @productId@ , @provisioningArtifact@ , @type@ , @status@ , @tags@ , @userArn@ , @userArnSession@ , @lastProvisioningRecordId@ , @lastSuccessfulProvisioningRecordId@ , @productName@ , and @provisioningArtifactName@ .
-- Example: @"SearchQuery":["status:AVAILABLE"]@
-- * 'sortOrder' - The sort order. If no value is specified, the results are not sorted.
-- * 'acceptLanguage' - The language code.
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
-- * 'accessLevelFilter' - The access level to use to obtain results. The default is @User@ .
-- * 'pageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
-- * 'pageSize' - The maximum number of items to return with this call.
-- * 'sortBy' - The sort field. If no value is specified, the results are not sorted. The valid values are @arn@ , @id@ , @name@ , and @lastRecordId@ .
mkSearchProvisionedProducts ::
  SearchProvisionedProducts
mkSearchProvisionedProducts =
  SearchProvisionedProducts'
    { filters = Lude.Nothing,
      sortOrder = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      accessLevelFilter = Lude.Nothing,
      pageToken = Lude.Nothing,
      pageSize = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | The search filters.
--
-- When the key is @SearchQuery@ , the searchable fields are @arn@ , @createdTime@ , @id@ , @lastRecordId@ , @idempotencyToken@ , @name@ , @physicalId@ , @productId@ , @provisioningArtifact@ , @type@ , @status@ , @tags@ , @userArn@ , @userArnSession@ , @lastProvisioningRecordId@ , @lastSuccessfulProvisioningRecordId@ , @productName@ , and @provisioningArtifactName@ .
-- Example: @"SearchQuery":["status:AVAILABLE"]@
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppFilters :: Lens.Lens' SearchProvisionedProducts (Lude.Maybe (Lude.HashMap ProvisionedProductViewFilterBy ([Lude.Text])))
sppFilters = Lens.lens (filters :: SearchProvisionedProducts -> Lude.Maybe (Lude.HashMap ProvisionedProductViewFilterBy ([Lude.Text]))) (\s a -> s {filters = a} :: SearchProvisionedProducts)
{-# DEPRECATED sppFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The sort order. If no value is specified, the results are not sorted.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppSortOrder :: Lens.Lens' SearchProvisionedProducts (Lude.Maybe SortOrder)
sppSortOrder = Lens.lens (sortOrder :: SearchProvisionedProducts -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: SearchProvisionedProducts)
{-# DEPRECATED sppSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

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
sppAcceptLanguage :: Lens.Lens' SearchProvisionedProducts (Lude.Maybe Lude.Text)
sppAcceptLanguage = Lens.lens (acceptLanguage :: SearchProvisionedProducts -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: SearchProvisionedProducts)
{-# DEPRECATED sppAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The access level to use to obtain results. The default is @User@ .
--
-- /Note:/ Consider using 'accessLevelFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppAccessLevelFilter :: Lens.Lens' SearchProvisionedProducts (Lude.Maybe AccessLevelFilter)
sppAccessLevelFilter = Lens.lens (accessLevelFilter :: SearchProvisionedProducts -> Lude.Maybe AccessLevelFilter) (\s a -> s {accessLevelFilter = a} :: SearchProvisionedProducts)
{-# DEPRECATED sppAccessLevelFilter "Use generic-lens or generic-optics with 'accessLevelFilter' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppPageToken :: Lens.Lens' SearchProvisionedProducts (Lude.Maybe Lude.Text)
sppPageToken = Lens.lens (pageToken :: SearchProvisionedProducts -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: SearchProvisionedProducts)
{-# DEPRECATED sppPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppPageSize :: Lens.Lens' SearchProvisionedProducts (Lude.Maybe Lude.Natural)
sppPageSize = Lens.lens (pageSize :: SearchProvisionedProducts -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: SearchProvisionedProducts)
{-# DEPRECATED sppPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The sort field. If no value is specified, the results are not sorted. The valid values are @arn@ , @id@ , @name@ , and @lastRecordId@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sppSortBy :: Lens.Lens' SearchProvisionedProducts (Lude.Maybe Lude.Text)
sppSortBy = Lens.lens (sortBy :: SearchProvisionedProducts -> Lude.Maybe Lude.Text) (\s a -> s {sortBy = a} :: SearchProvisionedProducts)
{-# DEPRECATED sppSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Lude.AWSRequest SearchProvisionedProducts where
  type
    Rs SearchProvisionedProducts =
      SearchProvisionedProductsResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          SearchProvisionedProductsResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "ProvisionedProducts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "TotalResultsCount")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SearchProvisionedProducts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.SearchProvisionedProducts" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SearchProvisionedProducts where
  toJSON SearchProvisionedProducts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("AccessLevelFilter" Lude..=) Lude.<$> accessLevelFilter,
            ("PageToken" Lude..=) Lude.<$> pageToken,
            ("PageSize" Lude..=) Lude.<$> pageSize,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath SearchProvisionedProducts where
  toPath = Lude.const "/"

instance Lude.ToQuery SearchProvisionedProducts where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSearchProvisionedProductsResponse' smart constructor.
data SearchProvisionedProductsResponse = SearchProvisionedProductsResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | Information about the provisioned products.
    provisionedProducts :: Lude.Maybe [ProvisionedProductAttribute],
    -- | The number of provisioned products found.
    totalResultsCount :: Lude.Maybe Lude.Int,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchProvisionedProductsResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
-- * 'provisionedProducts' - Information about the provisioned products.
-- * 'totalResultsCount' - The number of provisioned products found.
-- * 'responseStatus' - The response status code.
mkSearchProvisionedProductsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchProvisionedProductsResponse
mkSearchProvisionedProductsResponse pResponseStatus_ =
  SearchProvisionedProductsResponse'
    { nextPageToken = Lude.Nothing,
      provisionedProducts = Lude.Nothing,
      totalResultsCount = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsNextPageToken :: Lens.Lens' SearchProvisionedProductsResponse (Lude.Maybe Lude.Text)
srsNextPageToken = Lens.lens (nextPageToken :: SearchProvisionedProductsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: SearchProvisionedProductsResponse)
{-# DEPRECATED srsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Information about the provisioned products.
--
-- /Note:/ Consider using 'provisionedProducts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsProvisionedProducts :: Lens.Lens' SearchProvisionedProductsResponse (Lude.Maybe [ProvisionedProductAttribute])
srsProvisionedProducts = Lens.lens (provisionedProducts :: SearchProvisionedProductsResponse -> Lude.Maybe [ProvisionedProductAttribute]) (\s a -> s {provisionedProducts = a} :: SearchProvisionedProductsResponse)
{-# DEPRECATED srsProvisionedProducts "Use generic-lens or generic-optics with 'provisionedProducts' instead." #-}

-- | The number of provisioned products found.
--
-- /Note:/ Consider using 'totalResultsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsTotalResultsCount :: Lens.Lens' SearchProvisionedProductsResponse (Lude.Maybe Lude.Int)
srsTotalResultsCount = Lens.lens (totalResultsCount :: SearchProvisionedProductsResponse -> Lude.Maybe Lude.Int) (\s a -> s {totalResultsCount = a} :: SearchProvisionedProductsResponse)
{-# DEPRECATED srsTotalResultsCount "Use generic-lens or generic-optics with 'totalResultsCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' SearchProvisionedProductsResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: SearchProvisionedProductsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchProvisionedProductsResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
