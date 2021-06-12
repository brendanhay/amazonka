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
-- Module      : Network.AWS.ServiceCatalog.ListPortfoliosForProduct
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all portfolios that the specified product is associated with.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListPortfoliosForProduct
  ( -- * Creating a Request
    ListPortfoliosForProduct (..),
    newListPortfoliosForProduct,

    -- * Request Lenses
    listPortfoliosForProduct_pageSize,
    listPortfoliosForProduct_pageToken,
    listPortfoliosForProduct_acceptLanguage,
    listPortfoliosForProduct_productId,

    -- * Destructuring the Response
    ListPortfoliosForProductResponse (..),
    newListPortfoliosForProductResponse,

    -- * Response Lenses
    listPortfoliosForProductResponse_portfolioDetails,
    listPortfoliosForProductResponse_nextPageToken,
    listPortfoliosForProductResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListPortfoliosForProduct' smart constructor.
data ListPortfoliosForProduct = ListPortfoliosForProduct'
  { -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Core.Maybe Core.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The product identifier.
    productId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPortfoliosForProduct' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listPortfoliosForProduct_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listPortfoliosForProduct_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'acceptLanguage', 'listPortfoliosForProduct_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'productId', 'listPortfoliosForProduct_productId' - The product identifier.
newListPortfoliosForProduct ::
  -- | 'productId'
  Core.Text ->
  ListPortfoliosForProduct
newListPortfoliosForProduct pProductId_ =
  ListPortfoliosForProduct'
    { pageSize = Core.Nothing,
      pageToken = Core.Nothing,
      acceptLanguage = Core.Nothing,
      productId = pProductId_
    }

-- | The maximum number of items to return with this call.
listPortfoliosForProduct_pageSize :: Lens.Lens' ListPortfoliosForProduct (Core.Maybe Core.Natural)
listPortfoliosForProduct_pageSize = Lens.lens (\ListPortfoliosForProduct' {pageSize} -> pageSize) (\s@ListPortfoliosForProduct' {} a -> s {pageSize = a} :: ListPortfoliosForProduct)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listPortfoliosForProduct_pageToken :: Lens.Lens' ListPortfoliosForProduct (Core.Maybe Core.Text)
listPortfoliosForProduct_pageToken = Lens.lens (\ListPortfoliosForProduct' {pageToken} -> pageToken) (\s@ListPortfoliosForProduct' {} a -> s {pageToken = a} :: ListPortfoliosForProduct)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listPortfoliosForProduct_acceptLanguage :: Lens.Lens' ListPortfoliosForProduct (Core.Maybe Core.Text)
listPortfoliosForProduct_acceptLanguage = Lens.lens (\ListPortfoliosForProduct' {acceptLanguage} -> acceptLanguage) (\s@ListPortfoliosForProduct' {} a -> s {acceptLanguage = a} :: ListPortfoliosForProduct)

-- | The product identifier.
listPortfoliosForProduct_productId :: Lens.Lens' ListPortfoliosForProduct Core.Text
listPortfoliosForProduct_productId = Lens.lens (\ListPortfoliosForProduct' {productId} -> productId) (\s@ListPortfoliosForProduct' {} a -> s {productId = a} :: ListPortfoliosForProduct)

instance Core.AWSPager ListPortfoliosForProduct where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPortfoliosForProductResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listPortfoliosForProductResponse_portfolioDetails
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listPortfoliosForProduct_pageToken
          Lens..~ rs
          Lens.^? listPortfoliosForProductResponse_nextPageToken
            Core.. Lens._Just

instance Core.AWSRequest ListPortfoliosForProduct where
  type
    AWSResponse ListPortfoliosForProduct =
      ListPortfoliosForProductResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPortfoliosForProductResponse'
            Core.<$> (x Core..?> "PortfolioDetails" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListPortfoliosForProduct

instance Core.NFData ListPortfoliosForProduct

instance Core.ToHeaders ListPortfoliosForProduct where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListPortfoliosForProduct" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListPortfoliosForProduct where
  toJSON ListPortfoliosForProduct' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("ProductId" Core..= productId)
          ]
      )

instance Core.ToPath ListPortfoliosForProduct where
  toPath = Core.const "/"

instance Core.ToQuery ListPortfoliosForProduct where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListPortfoliosForProductResponse' smart constructor.
data ListPortfoliosForProductResponse = ListPortfoliosForProductResponse'
  { -- | Information about the portfolios.
    portfolioDetails :: Core.Maybe [PortfolioDetail],
    -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPortfoliosForProductResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portfolioDetails', 'listPortfoliosForProductResponse_portfolioDetails' - Information about the portfolios.
--
-- 'nextPageToken', 'listPortfoliosForProductResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'httpStatus', 'listPortfoliosForProductResponse_httpStatus' - The response's http status code.
newListPortfoliosForProductResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPortfoliosForProductResponse
newListPortfoliosForProductResponse pHttpStatus_ =
  ListPortfoliosForProductResponse'
    { portfolioDetails =
        Core.Nothing,
      nextPageToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the portfolios.
listPortfoliosForProductResponse_portfolioDetails :: Lens.Lens' ListPortfoliosForProductResponse (Core.Maybe [PortfolioDetail])
listPortfoliosForProductResponse_portfolioDetails = Lens.lens (\ListPortfoliosForProductResponse' {portfolioDetails} -> portfolioDetails) (\s@ListPortfoliosForProductResponse' {} a -> s {portfolioDetails = a} :: ListPortfoliosForProductResponse) Core.. Lens.mapping Lens._Coerce

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listPortfoliosForProductResponse_nextPageToken :: Lens.Lens' ListPortfoliosForProductResponse (Core.Maybe Core.Text)
listPortfoliosForProductResponse_nextPageToken = Lens.lens (\ListPortfoliosForProductResponse' {nextPageToken} -> nextPageToken) (\s@ListPortfoliosForProductResponse' {} a -> s {nextPageToken = a} :: ListPortfoliosForProductResponse)

-- | The response's http status code.
listPortfoliosForProductResponse_httpStatus :: Lens.Lens' ListPortfoliosForProductResponse Core.Int
listPortfoliosForProductResponse_httpStatus = Lens.lens (\ListPortfoliosForProductResponse' {httpStatus} -> httpStatus) (\s@ListPortfoliosForProductResponse' {} a -> s {httpStatus = a} :: ListPortfoliosForProductResponse)

instance Core.NFData ListPortfoliosForProductResponse
