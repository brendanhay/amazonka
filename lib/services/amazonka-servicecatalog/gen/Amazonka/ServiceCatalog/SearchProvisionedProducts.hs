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
-- Module      : Amazonka.ServiceCatalog.SearchProvisionedProducts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the provisioned products that meet the specified
-- criteria.
module Amazonka.ServiceCatalog.SearchProvisionedProducts
  ( -- * Creating a Request
    SearchProvisionedProducts (..),
    newSearchProvisionedProducts,

    -- * Request Lenses
    searchProvisionedProducts_acceptLanguage,
    searchProvisionedProducts_accessLevelFilter,
    searchProvisionedProducts_filters,
    searchProvisionedProducts_pageSize,
    searchProvisionedProducts_pageToken,
    searchProvisionedProducts_sortBy,
    searchProvisionedProducts_sortOrder,

    -- * Destructuring the Response
    SearchProvisionedProductsResponse (..),
    newSearchProvisionedProductsResponse,

    -- * Response Lenses
    searchProvisionedProductsResponse_nextPageToken,
    searchProvisionedProductsResponse_provisionedProducts,
    searchProvisionedProductsResponse_totalResultsCount,
    searchProvisionedProductsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newSearchProvisionedProducts' smart constructor.
data SearchProvisionedProducts = SearchProvisionedProducts'
  { -- | The language code.
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The access level to use to obtain results. The default is @User@.
    accessLevelFilter :: Prelude.Maybe AccessLevelFilter,
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
    filters :: Prelude.Maybe (Prelude.HashMap ProvisionedProductViewFilterBy [Prelude.Text]),
    -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The sort field. If no value is specified, the results are not sorted.
    -- The valid values are @arn@, @id@, @name@, and @lastRecordId@.
    sortBy :: Prelude.Maybe Prelude.Text,
    -- | The sort order. If no value is specified, the results are not sorted.
    sortOrder :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchProvisionedProducts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'searchProvisionedProducts_acceptLanguage' - The language code.
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'accessLevelFilter', 'searchProvisionedProducts_accessLevelFilter' - The access level to use to obtain results. The default is @User@.
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
-- 'pageSize', 'searchProvisionedProducts_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'searchProvisionedProducts_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'sortBy', 'searchProvisionedProducts_sortBy' - The sort field. If no value is specified, the results are not sorted.
-- The valid values are @arn@, @id@, @name@, and @lastRecordId@.
--
-- 'sortOrder', 'searchProvisionedProducts_sortOrder' - The sort order. If no value is specified, the results are not sorted.
newSearchProvisionedProducts ::
  SearchProvisionedProducts
newSearchProvisionedProducts =
  SearchProvisionedProducts'
    { acceptLanguage =
        Prelude.Nothing,
      accessLevelFilter = Prelude.Nothing,
      filters = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      pageToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | The language code.
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
searchProvisionedProducts_acceptLanguage :: Lens.Lens' SearchProvisionedProducts (Prelude.Maybe Prelude.Text)
searchProvisionedProducts_acceptLanguage = Lens.lens (\SearchProvisionedProducts' {acceptLanguage} -> acceptLanguage) (\s@SearchProvisionedProducts' {} a -> s {acceptLanguage = a} :: SearchProvisionedProducts)

-- | The access level to use to obtain results. The default is @User@.
searchProvisionedProducts_accessLevelFilter :: Lens.Lens' SearchProvisionedProducts (Prelude.Maybe AccessLevelFilter)
searchProvisionedProducts_accessLevelFilter = Lens.lens (\SearchProvisionedProducts' {accessLevelFilter} -> accessLevelFilter) (\s@SearchProvisionedProducts' {} a -> s {accessLevelFilter = a} :: SearchProvisionedProducts)

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
searchProvisionedProducts_filters :: Lens.Lens' SearchProvisionedProducts (Prelude.Maybe (Prelude.HashMap ProvisionedProductViewFilterBy [Prelude.Text]))
searchProvisionedProducts_filters = Lens.lens (\SearchProvisionedProducts' {filters} -> filters) (\s@SearchProvisionedProducts' {} a -> s {filters = a} :: SearchProvisionedProducts) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return with this call.
searchProvisionedProducts_pageSize :: Lens.Lens' SearchProvisionedProducts (Prelude.Maybe Prelude.Natural)
searchProvisionedProducts_pageSize = Lens.lens (\SearchProvisionedProducts' {pageSize} -> pageSize) (\s@SearchProvisionedProducts' {} a -> s {pageSize = a} :: SearchProvisionedProducts)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
searchProvisionedProducts_pageToken :: Lens.Lens' SearchProvisionedProducts (Prelude.Maybe Prelude.Text)
searchProvisionedProducts_pageToken = Lens.lens (\SearchProvisionedProducts' {pageToken} -> pageToken) (\s@SearchProvisionedProducts' {} a -> s {pageToken = a} :: SearchProvisionedProducts)

-- | The sort field. If no value is specified, the results are not sorted.
-- The valid values are @arn@, @id@, @name@, and @lastRecordId@.
searchProvisionedProducts_sortBy :: Lens.Lens' SearchProvisionedProducts (Prelude.Maybe Prelude.Text)
searchProvisionedProducts_sortBy = Lens.lens (\SearchProvisionedProducts' {sortBy} -> sortBy) (\s@SearchProvisionedProducts' {} a -> s {sortBy = a} :: SearchProvisionedProducts)

-- | The sort order. If no value is specified, the results are not sorted.
searchProvisionedProducts_sortOrder :: Lens.Lens' SearchProvisionedProducts (Prelude.Maybe SortOrder)
searchProvisionedProducts_sortOrder = Lens.lens (\SearchProvisionedProducts' {sortOrder} -> sortOrder) (\s@SearchProvisionedProducts' {} a -> s {sortOrder = a} :: SearchProvisionedProducts)

instance Core.AWSRequest SearchProvisionedProducts where
  type
    AWSResponse SearchProvisionedProducts =
      SearchProvisionedProductsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchProvisionedProductsResponse'
            Prelude.<$> (x Data..?> "NextPageToken")
            Prelude.<*> ( x
                            Data..?> "ProvisionedProducts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "TotalResultsCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchProvisionedProducts where
  hashWithSalt _salt SearchProvisionedProducts' {..} =
    _salt
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` accessLevelFilter
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` pageToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData SearchProvisionedProducts where
  rnf SearchProvisionedProducts' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf accessLevelFilter
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToHeaders SearchProvisionedProducts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.SearchProvisionedProducts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchProvisionedProducts where
  toJSON SearchProvisionedProducts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("AccessLevelFilter" Data..=)
              Prelude.<$> accessLevelFilter,
            ("Filters" Data..=) Prelude.<$> filters,
            ("PageSize" Data..=) Prelude.<$> pageSize,
            ("PageToken" Data..=) Prelude.<$> pageToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance Data.ToPath SearchProvisionedProducts where
  toPath = Prelude.const "/"

instance Data.ToQuery SearchProvisionedProducts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchProvisionedProductsResponse' smart constructor.
data SearchProvisionedProductsResponse = SearchProvisionedProductsResponse'
  { -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the provisioned products.
    provisionedProducts :: Prelude.Maybe [ProvisionedProductAttribute],
    -- | The number of provisioned products found.
    totalResultsCount :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchProvisionedProductsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'searchProvisionedProductsResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'provisionedProducts', 'searchProvisionedProductsResponse_provisionedProducts' - Information about the provisioned products.
--
-- 'totalResultsCount', 'searchProvisionedProductsResponse_totalResultsCount' - The number of provisioned products found.
--
-- 'httpStatus', 'searchProvisionedProductsResponse_httpStatus' - The response's http status code.
newSearchProvisionedProductsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchProvisionedProductsResponse
newSearchProvisionedProductsResponse pHttpStatus_ =
  SearchProvisionedProductsResponse'
    { nextPageToken =
        Prelude.Nothing,
      provisionedProducts = Prelude.Nothing,
      totalResultsCount = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
searchProvisionedProductsResponse_nextPageToken :: Lens.Lens' SearchProvisionedProductsResponse (Prelude.Maybe Prelude.Text)
searchProvisionedProductsResponse_nextPageToken = Lens.lens (\SearchProvisionedProductsResponse' {nextPageToken} -> nextPageToken) (\s@SearchProvisionedProductsResponse' {} a -> s {nextPageToken = a} :: SearchProvisionedProductsResponse)

-- | Information about the provisioned products.
searchProvisionedProductsResponse_provisionedProducts :: Lens.Lens' SearchProvisionedProductsResponse (Prelude.Maybe [ProvisionedProductAttribute])
searchProvisionedProductsResponse_provisionedProducts = Lens.lens (\SearchProvisionedProductsResponse' {provisionedProducts} -> provisionedProducts) (\s@SearchProvisionedProductsResponse' {} a -> s {provisionedProducts = a} :: SearchProvisionedProductsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The number of provisioned products found.
searchProvisionedProductsResponse_totalResultsCount :: Lens.Lens' SearchProvisionedProductsResponse (Prelude.Maybe Prelude.Int)
searchProvisionedProductsResponse_totalResultsCount = Lens.lens (\SearchProvisionedProductsResponse' {totalResultsCount} -> totalResultsCount) (\s@SearchProvisionedProductsResponse' {} a -> s {totalResultsCount = a} :: SearchProvisionedProductsResponse)

-- | The response's http status code.
searchProvisionedProductsResponse_httpStatus :: Lens.Lens' SearchProvisionedProductsResponse Prelude.Int
searchProvisionedProductsResponse_httpStatus = Lens.lens (\SearchProvisionedProductsResponse' {httpStatus} -> httpStatus) (\s@SearchProvisionedProductsResponse' {} a -> s {httpStatus = a} :: SearchProvisionedProductsResponse)

instance
  Prelude.NFData
    SearchProvisionedProductsResponse
  where
  rnf SearchProvisionedProductsResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf provisionedProducts
      `Prelude.seq` Prelude.rnf totalResultsCount
      `Prelude.seq` Prelude.rnf httpStatus
