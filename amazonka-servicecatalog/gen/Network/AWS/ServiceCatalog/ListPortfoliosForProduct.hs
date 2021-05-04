{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListPortfoliosForProduct' smart constructor.
data ListPortfoliosForProduct = ListPortfoliosForProduct'
  { -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    productId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ListPortfoliosForProduct
newListPortfoliosForProduct pProductId_ =
  ListPortfoliosForProduct'
    { pageSize =
        Prelude.Nothing,
      pageToken = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      productId = pProductId_
    }

-- | The maximum number of items to return with this call.
listPortfoliosForProduct_pageSize :: Lens.Lens' ListPortfoliosForProduct (Prelude.Maybe Prelude.Natural)
listPortfoliosForProduct_pageSize = Lens.lens (\ListPortfoliosForProduct' {pageSize} -> pageSize) (\s@ListPortfoliosForProduct' {} a -> s {pageSize = a} :: ListPortfoliosForProduct)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listPortfoliosForProduct_pageToken :: Lens.Lens' ListPortfoliosForProduct (Prelude.Maybe Prelude.Text)
listPortfoliosForProduct_pageToken = Lens.lens (\ListPortfoliosForProduct' {pageToken} -> pageToken) (\s@ListPortfoliosForProduct' {} a -> s {pageToken = a} :: ListPortfoliosForProduct)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listPortfoliosForProduct_acceptLanguage :: Lens.Lens' ListPortfoliosForProduct (Prelude.Maybe Prelude.Text)
listPortfoliosForProduct_acceptLanguage = Lens.lens (\ListPortfoliosForProduct' {acceptLanguage} -> acceptLanguage) (\s@ListPortfoliosForProduct' {} a -> s {acceptLanguage = a} :: ListPortfoliosForProduct)

-- | The product identifier.
listPortfoliosForProduct_productId :: Lens.Lens' ListPortfoliosForProduct Prelude.Text
listPortfoliosForProduct_productId = Lens.lens (\ListPortfoliosForProduct' {productId} -> productId) (\s@ListPortfoliosForProduct' {} a -> s {productId = a} :: ListPortfoliosForProduct)

instance Pager.AWSPager ListPortfoliosForProduct where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listPortfoliosForProductResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listPortfoliosForProductResponse_portfolioDetails
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listPortfoliosForProduct_pageToken
          Lens..~ rs
          Lens.^? listPortfoliosForProductResponse_nextPageToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListPortfoliosForProduct where
  type
    Rs ListPortfoliosForProduct =
      ListPortfoliosForProductResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPortfoliosForProductResponse'
            Prelude.<$> ( x Prelude..?> "PortfolioDetails"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "NextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPortfoliosForProduct

instance Prelude.NFData ListPortfoliosForProduct

instance Prelude.ToHeaders ListPortfoliosForProduct where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWS242ServiceCatalogService.ListPortfoliosForProduct" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListPortfoliosForProduct where
  toJSON ListPortfoliosForProduct' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PageSize" Prelude..=) Prelude.<$> pageSize,
            ("PageToken" Prelude..=) Prelude.<$> pageToken,
            ("AcceptLanguage" Prelude..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("ProductId" Prelude..= productId)
          ]
      )

instance Prelude.ToPath ListPortfoliosForProduct where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListPortfoliosForProduct where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPortfoliosForProductResponse' smart constructor.
data ListPortfoliosForProductResponse = ListPortfoliosForProductResponse'
  { -- | Information about the portfolios.
    portfolioDetails :: Prelude.Maybe [PortfolioDetail],
    -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListPortfoliosForProductResponse
newListPortfoliosForProductResponse pHttpStatus_ =
  ListPortfoliosForProductResponse'
    { portfolioDetails =
        Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the portfolios.
listPortfoliosForProductResponse_portfolioDetails :: Lens.Lens' ListPortfoliosForProductResponse (Prelude.Maybe [PortfolioDetail])
listPortfoliosForProductResponse_portfolioDetails = Lens.lens (\ListPortfoliosForProductResponse' {portfolioDetails} -> portfolioDetails) (\s@ListPortfoliosForProductResponse' {} a -> s {portfolioDetails = a} :: ListPortfoliosForProductResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listPortfoliosForProductResponse_nextPageToken :: Lens.Lens' ListPortfoliosForProductResponse (Prelude.Maybe Prelude.Text)
listPortfoliosForProductResponse_nextPageToken = Lens.lens (\ListPortfoliosForProductResponse' {nextPageToken} -> nextPageToken) (\s@ListPortfoliosForProductResponse' {} a -> s {nextPageToken = a} :: ListPortfoliosForProductResponse)

-- | The response's http status code.
listPortfoliosForProductResponse_httpStatus :: Lens.Lens' ListPortfoliosForProductResponse Prelude.Int
listPortfoliosForProductResponse_httpStatus = Lens.lens (\ListPortfoliosForProductResponse' {httpStatus} -> httpStatus) (\s@ListPortfoliosForProductResponse' {} a -> s {httpStatus = a} :: ListPortfoliosForProductResponse)

instance
  Prelude.NFData
    ListPortfoliosForProductResponse
