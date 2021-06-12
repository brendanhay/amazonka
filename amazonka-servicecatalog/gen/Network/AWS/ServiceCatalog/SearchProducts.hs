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
-- Module      : Network.AWS.ServiceCatalog.SearchProducts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the products to which the caller has access.
module Network.AWS.ServiceCatalog.SearchProducts
  ( -- * Creating a Request
    SearchProducts (..),
    newSearchProducts,

    -- * Request Lenses
    searchProducts_sortOrder,
    searchProducts_pageSize,
    searchProducts_pageToken,
    searchProducts_sortBy,
    searchProducts_filters,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newSearchProducts' smart constructor.
data SearchProducts = SearchProducts'
  { -- | The sort order. If no value is specified, the results are not sorted.
    sortOrder :: Core.Maybe SortOrder,
    -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Core.Maybe Core.Text,
    -- | The sort field. If no value is specified, the results are not sorted.
    sortBy :: Core.Maybe ProductViewSortBy,
    -- | The search filters. If no search filters are specified, the output
    -- includes all products to which the caller has access.
    filters :: Core.Maybe (Core.HashMap ProductViewFilterBy [Core.Text]),
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
-- Create a value of 'SearchProducts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'searchProducts_sortOrder' - The sort order. If no value is specified, the results are not sorted.
--
-- 'pageSize', 'searchProducts_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'searchProducts_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'sortBy', 'searchProducts_sortBy' - The sort field. If no value is specified, the results are not sorted.
--
-- 'filters', 'searchProducts_filters' - The search filters. If no search filters are specified, the output
-- includes all products to which the caller has access.
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
    { sortOrder = Core.Nothing,
      pageSize = Core.Nothing,
      pageToken = Core.Nothing,
      sortBy = Core.Nothing,
      filters = Core.Nothing,
      acceptLanguage = Core.Nothing
    }

-- | The sort order. If no value is specified, the results are not sorted.
searchProducts_sortOrder :: Lens.Lens' SearchProducts (Core.Maybe SortOrder)
searchProducts_sortOrder = Lens.lens (\SearchProducts' {sortOrder} -> sortOrder) (\s@SearchProducts' {} a -> s {sortOrder = a} :: SearchProducts)

-- | The maximum number of items to return with this call.
searchProducts_pageSize :: Lens.Lens' SearchProducts (Core.Maybe Core.Natural)
searchProducts_pageSize = Lens.lens (\SearchProducts' {pageSize} -> pageSize) (\s@SearchProducts' {} a -> s {pageSize = a} :: SearchProducts)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
searchProducts_pageToken :: Lens.Lens' SearchProducts (Core.Maybe Core.Text)
searchProducts_pageToken = Lens.lens (\SearchProducts' {pageToken} -> pageToken) (\s@SearchProducts' {} a -> s {pageToken = a} :: SearchProducts)

-- | The sort field. If no value is specified, the results are not sorted.
searchProducts_sortBy :: Lens.Lens' SearchProducts (Core.Maybe ProductViewSortBy)
searchProducts_sortBy = Lens.lens (\SearchProducts' {sortBy} -> sortBy) (\s@SearchProducts' {} a -> s {sortBy = a} :: SearchProducts)

-- | The search filters. If no search filters are specified, the output
-- includes all products to which the caller has access.
searchProducts_filters :: Lens.Lens' SearchProducts (Core.Maybe (Core.HashMap ProductViewFilterBy [Core.Text]))
searchProducts_filters = Lens.lens (\SearchProducts' {filters} -> filters) (\s@SearchProducts' {} a -> s {filters = a} :: SearchProducts) Core.. Lens.mapping Lens._Coerce

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
searchProducts_acceptLanguage :: Lens.Lens' SearchProducts (Core.Maybe Core.Text)
searchProducts_acceptLanguage = Lens.lens (\SearchProducts' {acceptLanguage} -> acceptLanguage) (\s@SearchProducts' {} a -> s {acceptLanguage = a} :: SearchProducts)

instance Core.AWSRequest SearchProducts where
  type
    AWSResponse SearchProducts =
      SearchProductsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchProductsResponse'
            Core.<$> (x Core..?> "NextPageToken")
            Core.<*> ( x Core..?> "ProductViewSummaries"
                         Core..!@ Core.mempty
                     )
            Core.<*> ( x Core..?> "ProductViewAggregations"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SearchProducts

instance Core.NFData SearchProducts

instance Core.ToHeaders SearchProducts where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.SearchProducts" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SearchProducts where
  toJSON SearchProducts' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SortOrder" Core..=) Core.<$> sortOrder,
            ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("Filters" Core..=) Core.<$> filters,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.ToPath SearchProducts where
  toPath = Core.const "/"

instance Core.ToQuery SearchProducts where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSearchProductsResponse' smart constructor.
data SearchProductsResponse = SearchProductsResponse'
  { -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Core.Maybe Core.Text,
    -- | Information about the product views.
    productViewSummaries :: Core.Maybe [ProductViewSummary],
    -- | The product view aggregations.
    productViewAggregations :: Core.Maybe (Core.HashMap Core.Text [ProductViewAggregationValue]),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  SearchProductsResponse
newSearchProductsResponse pHttpStatus_ =
  SearchProductsResponse'
    { nextPageToken =
        Core.Nothing,
      productViewSummaries = Core.Nothing,
      productViewAggregations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
searchProductsResponse_nextPageToken :: Lens.Lens' SearchProductsResponse (Core.Maybe Core.Text)
searchProductsResponse_nextPageToken = Lens.lens (\SearchProductsResponse' {nextPageToken} -> nextPageToken) (\s@SearchProductsResponse' {} a -> s {nextPageToken = a} :: SearchProductsResponse)

-- | Information about the product views.
searchProductsResponse_productViewSummaries :: Lens.Lens' SearchProductsResponse (Core.Maybe [ProductViewSummary])
searchProductsResponse_productViewSummaries = Lens.lens (\SearchProductsResponse' {productViewSummaries} -> productViewSummaries) (\s@SearchProductsResponse' {} a -> s {productViewSummaries = a} :: SearchProductsResponse) Core.. Lens.mapping Lens._Coerce

-- | The product view aggregations.
searchProductsResponse_productViewAggregations :: Lens.Lens' SearchProductsResponse (Core.Maybe (Core.HashMap Core.Text [ProductViewAggregationValue]))
searchProductsResponse_productViewAggregations = Lens.lens (\SearchProductsResponse' {productViewAggregations} -> productViewAggregations) (\s@SearchProductsResponse' {} a -> s {productViewAggregations = a} :: SearchProductsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
searchProductsResponse_httpStatus :: Lens.Lens' SearchProductsResponse Core.Int
searchProductsResponse_httpStatus = Lens.lens (\SearchProductsResponse' {httpStatus} -> httpStatus) (\s@SearchProductsResponse' {} a -> s {httpStatus = a} :: SearchProductsResponse)

instance Core.NFData SearchProductsResponse
