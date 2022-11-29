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
-- Module      : Amazonka.ServiceCatalog.SearchProducts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the products to which the caller has access.
module Amazonka.ServiceCatalog.SearchProducts
  ( -- * Creating a Request
    SearchProducts (..),
    newSearchProducts,

    -- * Request Lenses
    searchProducts_sortOrder,
    searchProducts_filters,
    searchProducts_sortBy,
    searchProducts_pageToken,
    searchProducts_pageSize,
    searchProducts_acceptLanguage,

    -- * Destructuring the Response
    SearchProductsResponse (..),
    newSearchProductsResponse,

    -- * Response Lenses
    searchProductsResponse_nextPageToken,
    searchProductsResponse_productViewSummaries,
    searchProductsResponse_productViewAggregations,
    searchProductsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newSearchProducts' smart constructor.
data SearchProducts = SearchProducts'
  { -- | The sort order. If no value is specified, the results are not sorted.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The search filters. If no search filters are specified, the output
    -- includes all products to which the caller has access.
    filters :: Prelude.Maybe (Prelude.HashMap ProductViewFilterBy [Prelude.Text]),
    -- | The sort field. If no value is specified, the results are not sorted.
    sortBy :: Prelude.Maybe ProductViewSortBy,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchProducts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'searchProducts_sortOrder' - The sort order. If no value is specified, the results are not sorted.
--
-- 'filters', 'searchProducts_filters' - The search filters. If no search filters are specified, the output
-- includes all products to which the caller has access.
--
-- 'sortBy', 'searchProducts_sortBy' - The sort field. If no value is specified, the results are not sorted.
--
-- 'pageToken', 'searchProducts_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'pageSize', 'searchProducts_pageSize' - The maximum number of items to return with this call.
--
-- 'acceptLanguage', 'searchProducts_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
newSearchProducts ::
  SearchProducts
newSearchProducts =
  SearchProducts'
    { sortOrder = Prelude.Nothing,
      filters = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      pageToken = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing
    }

-- | The sort order. If no value is specified, the results are not sorted.
searchProducts_sortOrder :: Lens.Lens' SearchProducts (Prelude.Maybe SortOrder)
searchProducts_sortOrder = Lens.lens (\SearchProducts' {sortOrder} -> sortOrder) (\s@SearchProducts' {} a -> s {sortOrder = a} :: SearchProducts)

-- | The search filters. If no search filters are specified, the output
-- includes all products to which the caller has access.
searchProducts_filters :: Lens.Lens' SearchProducts (Prelude.Maybe (Prelude.HashMap ProductViewFilterBy [Prelude.Text]))
searchProducts_filters = Lens.lens (\SearchProducts' {filters} -> filters) (\s@SearchProducts' {} a -> s {filters = a} :: SearchProducts) Prelude.. Lens.mapping Lens.coerced

-- | The sort field. If no value is specified, the results are not sorted.
searchProducts_sortBy :: Lens.Lens' SearchProducts (Prelude.Maybe ProductViewSortBy)
searchProducts_sortBy = Lens.lens (\SearchProducts' {sortBy} -> sortBy) (\s@SearchProducts' {} a -> s {sortBy = a} :: SearchProducts)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
searchProducts_pageToken :: Lens.Lens' SearchProducts (Prelude.Maybe Prelude.Text)
searchProducts_pageToken = Lens.lens (\SearchProducts' {pageToken} -> pageToken) (\s@SearchProducts' {} a -> s {pageToken = a} :: SearchProducts)

-- | The maximum number of items to return with this call.
searchProducts_pageSize :: Lens.Lens' SearchProducts (Prelude.Maybe Prelude.Natural)
searchProducts_pageSize = Lens.lens (\SearchProducts' {pageSize} -> pageSize) (\s@SearchProducts' {} a -> s {pageSize = a} :: SearchProducts)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
searchProducts_acceptLanguage :: Lens.Lens' SearchProducts (Prelude.Maybe Prelude.Text)
searchProducts_acceptLanguage = Lens.lens (\SearchProducts' {acceptLanguage} -> acceptLanguage) (\s@SearchProducts' {} a -> s {acceptLanguage = a} :: SearchProducts)

instance Core.AWSRequest SearchProducts where
  type
    AWSResponse SearchProducts =
      SearchProductsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchProductsResponse'
            Prelude.<$> (x Core..?> "NextPageToken")
            Prelude.<*> ( x Core..?> "ProductViewSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Core..?> "ProductViewAggregations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchProducts where
  hashWithSalt _salt SearchProducts' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` pageToken
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` acceptLanguage

instance Prelude.NFData SearchProducts where
  rnf SearchProducts' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf acceptLanguage

instance Core.ToHeaders SearchProducts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.SearchProducts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchProducts where
  toJSON SearchProducts' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("Filters" Core..=) Prelude.<$> filters,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("PageToken" Core..=) Prelude.<$> pageToken,
            ("PageSize" Core..=) Prelude.<$> pageSize,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage
          ]
      )

instance Core.ToPath SearchProducts where
  toPath = Prelude.const "/"

instance Core.ToQuery SearchProducts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchProductsResponse' smart constructor.
data SearchProductsResponse = SearchProductsResponse'
  { -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the product views.
    productViewSummaries :: Prelude.Maybe [ProductViewSummary],
    -- | The product view aggregations.
    productViewAggregations :: Prelude.Maybe (Prelude.HashMap Prelude.Text [ProductViewAggregationValue]),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchProductsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'searchProductsResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'productViewSummaries', 'searchProductsResponse_productViewSummaries' - Information about the product views.
--
-- 'productViewAggregations', 'searchProductsResponse_productViewAggregations' - The product view aggregations.
--
-- 'httpStatus', 'searchProductsResponse_httpStatus' - The response's http status code.
newSearchProductsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchProductsResponse
newSearchProductsResponse pHttpStatus_ =
  SearchProductsResponse'
    { nextPageToken =
        Prelude.Nothing,
      productViewSummaries = Prelude.Nothing,
      productViewAggregations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
searchProductsResponse_nextPageToken :: Lens.Lens' SearchProductsResponse (Prelude.Maybe Prelude.Text)
searchProductsResponse_nextPageToken = Lens.lens (\SearchProductsResponse' {nextPageToken} -> nextPageToken) (\s@SearchProductsResponse' {} a -> s {nextPageToken = a} :: SearchProductsResponse)

-- | Information about the product views.
searchProductsResponse_productViewSummaries :: Lens.Lens' SearchProductsResponse (Prelude.Maybe [ProductViewSummary])
searchProductsResponse_productViewSummaries = Lens.lens (\SearchProductsResponse' {productViewSummaries} -> productViewSummaries) (\s@SearchProductsResponse' {} a -> s {productViewSummaries = a} :: SearchProductsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The product view aggregations.
searchProductsResponse_productViewAggregations :: Lens.Lens' SearchProductsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text [ProductViewAggregationValue]))
searchProductsResponse_productViewAggregations = Lens.lens (\SearchProductsResponse' {productViewAggregations} -> productViewAggregations) (\s@SearchProductsResponse' {} a -> s {productViewAggregations = a} :: SearchProductsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchProductsResponse_httpStatus :: Lens.Lens' SearchProductsResponse Prelude.Int
searchProductsResponse_httpStatus = Lens.lens (\SearchProductsResponse' {httpStatus} -> httpStatus) (\s@SearchProductsResponse' {} a -> s {httpStatus = a} :: SearchProductsResponse)

instance Prelude.NFData SearchProductsResponse where
  rnf SearchProductsResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf productViewSummaries
      `Prelude.seq` Prelude.rnf productViewAggregations
      `Prelude.seq` Prelude.rnf httpStatus
