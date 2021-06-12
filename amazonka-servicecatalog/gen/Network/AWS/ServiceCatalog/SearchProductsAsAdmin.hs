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
-- Module      : Network.AWS.ServiceCatalog.SearchProductsAsAdmin
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the products for the specified portfolio or all
-- products.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.SearchProductsAsAdmin
  ( -- * Creating a Request
    SearchProductsAsAdmin (..),
    newSearchProductsAsAdmin,

    -- * Request Lenses
    searchProductsAsAdmin_sortOrder,
    searchProductsAsAdmin_pageSize,
    searchProductsAsAdmin_pageToken,
    searchProductsAsAdmin_portfolioId,
    searchProductsAsAdmin_sortBy,
    searchProductsAsAdmin_productSource,
    searchProductsAsAdmin_filters,
    searchProductsAsAdmin_acceptLanguage,

    -- * Destructuring the Response
    SearchProductsAsAdminResponse (..),
    newSearchProductsAsAdminResponse,

    -- * Response Lenses
    searchProductsAsAdminResponse_nextPageToken,
    searchProductsAsAdminResponse_productViewDetails,
    searchProductsAsAdminResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newSearchProductsAsAdmin' smart constructor.
data SearchProductsAsAdmin = SearchProductsAsAdmin'
  { -- | The sort order. If no value is specified, the results are not sorted.
    sortOrder :: Core.Maybe SortOrder,
    -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Core.Maybe Core.Text,
    -- | The portfolio identifier.
    portfolioId :: Core.Maybe Core.Text,
    -- | The sort field. If no value is specified, the results are not sorted.
    sortBy :: Core.Maybe ProductViewSortBy,
    -- | Access level of the source of the product.
    productSource :: Core.Maybe ProductSource,
    -- | The search filters. If no search filters are specified, the output
    -- includes all products to which the administrator has access.
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
-- Create a value of 'SearchProductsAsAdmin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'searchProductsAsAdmin_sortOrder' - The sort order. If no value is specified, the results are not sorted.
--
-- 'pageSize', 'searchProductsAsAdmin_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'searchProductsAsAdmin_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'portfolioId', 'searchProductsAsAdmin_portfolioId' - The portfolio identifier.
--
-- 'sortBy', 'searchProductsAsAdmin_sortBy' - The sort field. If no value is specified, the results are not sorted.
--
-- 'productSource', 'searchProductsAsAdmin_productSource' - Access level of the source of the product.
--
-- 'filters', 'searchProductsAsAdmin_filters' - The search filters. If no search filters are specified, the output
-- includes all products to which the administrator has access.
--
-- 'acceptLanguage', 'searchProductsAsAdmin_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
newSearchProductsAsAdmin ::
  SearchProductsAsAdmin
newSearchProductsAsAdmin =
  SearchProductsAsAdmin'
    { sortOrder = Core.Nothing,
      pageSize = Core.Nothing,
      pageToken = Core.Nothing,
      portfolioId = Core.Nothing,
      sortBy = Core.Nothing,
      productSource = Core.Nothing,
      filters = Core.Nothing,
      acceptLanguage = Core.Nothing
    }

-- | The sort order. If no value is specified, the results are not sorted.
searchProductsAsAdmin_sortOrder :: Lens.Lens' SearchProductsAsAdmin (Core.Maybe SortOrder)
searchProductsAsAdmin_sortOrder = Lens.lens (\SearchProductsAsAdmin' {sortOrder} -> sortOrder) (\s@SearchProductsAsAdmin' {} a -> s {sortOrder = a} :: SearchProductsAsAdmin)

-- | The maximum number of items to return with this call.
searchProductsAsAdmin_pageSize :: Lens.Lens' SearchProductsAsAdmin (Core.Maybe Core.Natural)
searchProductsAsAdmin_pageSize = Lens.lens (\SearchProductsAsAdmin' {pageSize} -> pageSize) (\s@SearchProductsAsAdmin' {} a -> s {pageSize = a} :: SearchProductsAsAdmin)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
searchProductsAsAdmin_pageToken :: Lens.Lens' SearchProductsAsAdmin (Core.Maybe Core.Text)
searchProductsAsAdmin_pageToken = Lens.lens (\SearchProductsAsAdmin' {pageToken} -> pageToken) (\s@SearchProductsAsAdmin' {} a -> s {pageToken = a} :: SearchProductsAsAdmin)

-- | The portfolio identifier.
searchProductsAsAdmin_portfolioId :: Lens.Lens' SearchProductsAsAdmin (Core.Maybe Core.Text)
searchProductsAsAdmin_portfolioId = Lens.lens (\SearchProductsAsAdmin' {portfolioId} -> portfolioId) (\s@SearchProductsAsAdmin' {} a -> s {portfolioId = a} :: SearchProductsAsAdmin)

-- | The sort field. If no value is specified, the results are not sorted.
searchProductsAsAdmin_sortBy :: Lens.Lens' SearchProductsAsAdmin (Core.Maybe ProductViewSortBy)
searchProductsAsAdmin_sortBy = Lens.lens (\SearchProductsAsAdmin' {sortBy} -> sortBy) (\s@SearchProductsAsAdmin' {} a -> s {sortBy = a} :: SearchProductsAsAdmin)

-- | Access level of the source of the product.
searchProductsAsAdmin_productSource :: Lens.Lens' SearchProductsAsAdmin (Core.Maybe ProductSource)
searchProductsAsAdmin_productSource = Lens.lens (\SearchProductsAsAdmin' {productSource} -> productSource) (\s@SearchProductsAsAdmin' {} a -> s {productSource = a} :: SearchProductsAsAdmin)

-- | The search filters. If no search filters are specified, the output
-- includes all products to which the administrator has access.
searchProductsAsAdmin_filters :: Lens.Lens' SearchProductsAsAdmin (Core.Maybe (Core.HashMap ProductViewFilterBy [Core.Text]))
searchProductsAsAdmin_filters = Lens.lens (\SearchProductsAsAdmin' {filters} -> filters) (\s@SearchProductsAsAdmin' {} a -> s {filters = a} :: SearchProductsAsAdmin) Core.. Lens.mapping Lens._Coerce

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
searchProductsAsAdmin_acceptLanguage :: Lens.Lens' SearchProductsAsAdmin (Core.Maybe Core.Text)
searchProductsAsAdmin_acceptLanguage = Lens.lens (\SearchProductsAsAdmin' {acceptLanguage} -> acceptLanguage) (\s@SearchProductsAsAdmin' {} a -> s {acceptLanguage = a} :: SearchProductsAsAdmin)

instance Core.AWSPager SearchProductsAsAdmin where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchProductsAsAdminResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? searchProductsAsAdminResponse_productViewDetails
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& searchProductsAsAdmin_pageToken
          Lens..~ rs
          Lens.^? searchProductsAsAdminResponse_nextPageToken
            Core.. Lens._Just

instance Core.AWSRequest SearchProductsAsAdmin where
  type
    AWSResponse SearchProductsAsAdmin =
      SearchProductsAsAdminResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchProductsAsAdminResponse'
            Core.<$> (x Core..?> "NextPageToken")
            Core.<*> ( x Core..?> "ProductViewDetails"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SearchProductsAsAdmin

instance Core.NFData SearchProductsAsAdmin

instance Core.ToHeaders SearchProductsAsAdmin where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.SearchProductsAsAdmin" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SearchProductsAsAdmin where
  toJSON SearchProductsAsAdmin' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SortOrder" Core..=) Core.<$> sortOrder,
            ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("PortfolioId" Core..=) Core.<$> portfolioId,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("ProductSource" Core..=) Core.<$> productSource,
            ("Filters" Core..=) Core.<$> filters,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.ToPath SearchProductsAsAdmin where
  toPath = Core.const "/"

instance Core.ToQuery SearchProductsAsAdmin where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSearchProductsAsAdminResponse' smart constructor.
data SearchProductsAsAdminResponse = SearchProductsAsAdminResponse'
  { -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Core.Maybe Core.Text,
    -- | Information about the product views.
    productViewDetails :: Core.Maybe [ProductViewDetail],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchProductsAsAdminResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'searchProductsAsAdminResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'productViewDetails', 'searchProductsAsAdminResponse_productViewDetails' - Information about the product views.
--
-- 'httpStatus', 'searchProductsAsAdminResponse_httpStatus' - The response's http status code.
newSearchProductsAsAdminResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SearchProductsAsAdminResponse
newSearchProductsAsAdminResponse pHttpStatus_ =
  SearchProductsAsAdminResponse'
    { nextPageToken =
        Core.Nothing,
      productViewDetails = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
searchProductsAsAdminResponse_nextPageToken :: Lens.Lens' SearchProductsAsAdminResponse (Core.Maybe Core.Text)
searchProductsAsAdminResponse_nextPageToken = Lens.lens (\SearchProductsAsAdminResponse' {nextPageToken} -> nextPageToken) (\s@SearchProductsAsAdminResponse' {} a -> s {nextPageToken = a} :: SearchProductsAsAdminResponse)

-- | Information about the product views.
searchProductsAsAdminResponse_productViewDetails :: Lens.Lens' SearchProductsAsAdminResponse (Core.Maybe [ProductViewDetail])
searchProductsAsAdminResponse_productViewDetails = Lens.lens (\SearchProductsAsAdminResponse' {productViewDetails} -> productViewDetails) (\s@SearchProductsAsAdminResponse' {} a -> s {productViewDetails = a} :: SearchProductsAsAdminResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
searchProductsAsAdminResponse_httpStatus :: Lens.Lens' SearchProductsAsAdminResponse Core.Int
searchProductsAsAdminResponse_httpStatus = Lens.lens (\SearchProductsAsAdminResponse' {httpStatus} -> httpStatus) (\s@SearchProductsAsAdminResponse' {} a -> s {httpStatus = a} :: SearchProductsAsAdminResponse)

instance Core.NFData SearchProductsAsAdminResponse
