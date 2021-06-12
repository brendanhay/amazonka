{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.SearchProvisionedProducts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the provisioned products that meet the specified
-- criteria.
module Network.AWS.ServiceCatalog.SearchProvisionedProducts
  ( -- * Creating a Request
    SearchProvisionedProducts (..),
    newSearchProvisionedProducts,

    -- * Request Lenses
    searchProvisionedProducts_sortOrder,
    searchProvisionedProducts_pageSize,
    searchProvisionedProducts_pageToken,
    searchProvisionedProducts_accessLevelFilter,
    searchProvisionedProducts_sortBy,
    searchProvisionedProducts_filters,
    searchProvisionedProducts_acceptLanguage,

    -- * Destructuring the Response
    SearchProvisionedProductsResponse (..),
    newSearchProvisionedProductsResponse,

    -- * Response Lenses
    searchProvisionedProductsResponse_totalResultsCount,
    searchProvisionedProductsResponse_provisionedProducts,
    searchProvisionedProductsResponse_nextPageToken,
    searchProvisionedProductsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newSearchProvisionedProducts' smart constructor.
data SearchProvisionedProducts = SearchProvisionedProducts'
  { -- | The sort order. If no value is specified, the results are not sorted.
    sortOrder :: Core.Maybe SortOrder,
    -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Core.Maybe Core.Text,
    -- | The access level to use to obtain results. The default is @User@.
    accessLevelFilter :: Core.Maybe AccessLevelFilter,
    -- | The sort field. If no value is specified, the results are not sorted.
    -- The valid values are @arn@, @id@, @name@, and @lastRecordId@.
    sortBy :: Core.Maybe Core.Text,
    -- | The search filters.
    --
    -- When the key is @SearchQuery@, the searchable fields are @arn@,
    -- @createdTime@, @id@, @lastRecordId@, @idempotencyToken@, @name@,
    -- @physicalId@, @productId@, @provisioningArtifact@, @type@, @status@,
    -- @tags@, @userArn@, @userArnSession@, @lastProvisioningRecordId@,
    -- @lastSuccessfulProvisioningRecordId@, @productName@, and
    -- @provisioningArtifactName@.
    --
    -- Example: @\"SearchQuery\":[\"status:AVAILABLE\"]@
    filters :: Core.Maybe (Core.HashMap ProvisionedProductViewFilterBy [Core.Text]),
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchProvisionedProducts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'searchProvisionedProducts_sortOrder' - The sort order. If no value is specified, the results are not sorted.
--
-- 'pageSize', 'searchProvisionedProducts_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'searchProvisionedProducts_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'accessLevelFilter', 'searchProvisionedProducts_accessLevelFilter' - The access level to use to obtain results. The default is @User@.
--
-- 'sortBy', 'searchProvisionedProducts_sortBy' - The sort field. If no value is specified, the results are not sorted.
-- The valid values are @arn@, @id@, @name@, and @lastRecordId@.
--
-- 'filters', 'searchProvisionedProducts_filters' - The search filters.
--
-- When the key is @SearchQuery@, the searchable fields are @arn@,
-- @createdTime@, @id@, @lastRecordId@, @idempotencyToken@, @name@,
-- @physicalId@, @productId@, @provisioningArtifact@, @type@, @status@,
-- @tags@, @userArn@, @userArnSession@, @lastProvisioningRecordId@,
-- @lastSuccessfulProvisioningRecordId@, @productName@, and
-- @provisioningArtifactName@.
--
-- Example: @\"SearchQuery\":[\"status:AVAILABLE\"]@
--
-- 'acceptLanguage', 'searchProvisionedProducts_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
newSearchProvisionedProducts ::
  SearchProvisionedProducts
newSearchProvisionedProducts =
  SearchProvisionedProducts'
    { sortOrder =
        Core.Nothing,
      pageSize = Core.Nothing,
      pageToken = Core.Nothing,
      accessLevelFilter = Core.Nothing,
      sortBy = Core.Nothing,
      filters = Core.Nothing,
      acceptLanguage = Core.Nothing
    }

-- | The sort order. If no value is specified, the results are not sorted.
searchProvisionedProducts_sortOrder :: Lens.Lens' SearchProvisionedProducts (Core.Maybe SortOrder)
searchProvisionedProducts_sortOrder = Lens.lens (\SearchProvisionedProducts' {sortOrder} -> sortOrder) (\s@SearchProvisionedProducts' {} a -> s {sortOrder = a} :: SearchProvisionedProducts)

-- | The maximum number of items to return with this call.
searchProvisionedProducts_pageSize :: Lens.Lens' SearchProvisionedProducts (Core.Maybe Core.Natural)
searchProvisionedProducts_pageSize = Lens.lens (\SearchProvisionedProducts' {pageSize} -> pageSize) (\s@SearchProvisionedProducts' {} a -> s {pageSize = a} :: SearchProvisionedProducts)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
searchProvisionedProducts_pageToken :: Lens.Lens' SearchProvisionedProducts (Core.Maybe Core.Text)
searchProvisionedProducts_pageToken = Lens.lens (\SearchProvisionedProducts' {pageToken} -> pageToken) (\s@SearchProvisionedProducts' {} a -> s {pageToken = a} :: SearchProvisionedProducts)

-- | The access level to use to obtain results. The default is @User@.
searchProvisionedProducts_accessLevelFilter :: Lens.Lens' SearchProvisionedProducts (Core.Maybe AccessLevelFilter)
searchProvisionedProducts_accessLevelFilter = Lens.lens (\SearchProvisionedProducts' {accessLevelFilter} -> accessLevelFilter) (\s@SearchProvisionedProducts' {} a -> s {accessLevelFilter = a} :: SearchProvisionedProducts)

-- | The sort field. If no value is specified, the results are not sorted.
-- The valid values are @arn@, @id@, @name@, and @lastRecordId@.
searchProvisionedProducts_sortBy :: Lens.Lens' SearchProvisionedProducts (Core.Maybe Core.Text)
searchProvisionedProducts_sortBy = Lens.lens (\SearchProvisionedProducts' {sortBy} -> sortBy) (\s@SearchProvisionedProducts' {} a -> s {sortBy = a} :: SearchProvisionedProducts)

-- | The search filters.
--
-- When the key is @SearchQuery@, the searchable fields are @arn@,
-- @createdTime@, @id@, @lastRecordId@, @idempotencyToken@, @name@,
-- @physicalId@, @productId@, @provisioningArtifact@, @type@, @status@,
-- @tags@, @userArn@, @userArnSession@, @lastProvisioningRecordId@,
-- @lastSuccessfulProvisioningRecordId@, @productName@, and
-- @provisioningArtifactName@.
--
-- Example: @\"SearchQuery\":[\"status:AVAILABLE\"]@
searchProvisionedProducts_filters :: Lens.Lens' SearchProvisionedProducts (Core.Maybe (Core.HashMap ProvisionedProductViewFilterBy [Core.Text]))
searchProvisionedProducts_filters = Lens.lens (\SearchProvisionedProducts' {filters} -> filters) (\s@SearchProvisionedProducts' {} a -> s {filters = a} :: SearchProvisionedProducts) Core.. Lens.mapping Lens._Coerce

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
searchProvisionedProducts_acceptLanguage :: Lens.Lens' SearchProvisionedProducts (Core.Maybe Core.Text)
searchProvisionedProducts_acceptLanguage = Lens.lens (\SearchProvisionedProducts' {acceptLanguage} -> acceptLanguage) (\s@SearchProvisionedProducts' {} a -> s {acceptLanguage = a} :: SearchProvisionedProducts)

instance Core.AWSRequest SearchProvisionedProducts where
  type
    AWSResponse SearchProvisionedProducts =
      SearchProvisionedProductsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchProvisionedProductsResponse'
            Core.<$> (x Core..?> "TotalResultsCount")
            Core.<*> ( x Core..?> "ProvisionedProducts"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "NextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SearchProvisionedProducts

instance Core.NFData SearchProvisionedProducts

instance Core.ToHeaders SearchProvisionedProducts where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.SearchProvisionedProducts" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SearchProvisionedProducts where
  toJSON SearchProvisionedProducts' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SortOrder" Core..=) Core.<$> sortOrder,
            ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("AccessLevelFilter" Core..=)
              Core.<$> accessLevelFilter,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("Filters" Core..=) Core.<$> filters,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.ToPath SearchProvisionedProducts where
  toPath = Core.const "/"

instance Core.ToQuery SearchProvisionedProducts where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSearchProvisionedProductsResponse' smart constructor.
data SearchProvisionedProductsResponse = SearchProvisionedProductsResponse'
  { -- | The number of provisioned products found.
    totalResultsCount :: Core.Maybe Core.Int,
    -- | Information about the provisioned products.
    provisionedProducts :: Core.Maybe [ProvisionedProductAttribute],
    -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchProvisionedProductsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalResultsCount', 'searchProvisionedProductsResponse_totalResultsCount' - The number of provisioned products found.
--
-- 'provisionedProducts', 'searchProvisionedProductsResponse_provisionedProducts' - Information about the provisioned products.
--
-- 'nextPageToken', 'searchProvisionedProductsResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'httpStatus', 'searchProvisionedProductsResponse_httpStatus' - The response's http status code.
newSearchProvisionedProductsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SearchProvisionedProductsResponse
newSearchProvisionedProductsResponse pHttpStatus_ =
  SearchProvisionedProductsResponse'
    { totalResultsCount =
        Core.Nothing,
      provisionedProducts = Core.Nothing,
      nextPageToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of provisioned products found.
searchProvisionedProductsResponse_totalResultsCount :: Lens.Lens' SearchProvisionedProductsResponse (Core.Maybe Core.Int)
searchProvisionedProductsResponse_totalResultsCount = Lens.lens (\SearchProvisionedProductsResponse' {totalResultsCount} -> totalResultsCount) (\s@SearchProvisionedProductsResponse' {} a -> s {totalResultsCount = a} :: SearchProvisionedProductsResponse)

-- | Information about the provisioned products.
searchProvisionedProductsResponse_provisionedProducts :: Lens.Lens' SearchProvisionedProductsResponse (Core.Maybe [ProvisionedProductAttribute])
searchProvisionedProductsResponse_provisionedProducts = Lens.lens (\SearchProvisionedProductsResponse' {provisionedProducts} -> provisionedProducts) (\s@SearchProvisionedProductsResponse' {} a -> s {provisionedProducts = a} :: SearchProvisionedProductsResponse) Core.. Lens.mapping Lens._Coerce

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
searchProvisionedProductsResponse_nextPageToken :: Lens.Lens' SearchProvisionedProductsResponse (Core.Maybe Core.Text)
searchProvisionedProductsResponse_nextPageToken = Lens.lens (\SearchProvisionedProductsResponse' {nextPageToken} -> nextPageToken) (\s@SearchProvisionedProductsResponse' {} a -> s {nextPageToken = a} :: SearchProvisionedProductsResponse)

-- | The response's http status code.
searchProvisionedProductsResponse_httpStatus :: Lens.Lens' SearchProvisionedProductsResponse Core.Int
searchProvisionedProductsResponse_httpStatus = Lens.lens (\SearchProvisionedProductsResponse' {httpStatus} -> httpStatus) (\s@SearchProvisionedProductsResponse' {} a -> s {httpStatus = a} :: SearchProvisionedProductsResponse)

instance
  Core.NFData
    SearchProvisionedProductsResponse
