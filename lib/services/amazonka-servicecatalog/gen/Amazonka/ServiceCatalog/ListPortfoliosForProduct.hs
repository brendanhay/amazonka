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
-- Module      : Amazonka.ServiceCatalog.ListPortfoliosForProduct
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all portfolios that the specified product is associated with.
--
-- This operation returns paginated results.
module Amazonka.ServiceCatalog.ListPortfoliosForProduct
  ( -- * Creating a Request
    ListPortfoliosForProduct (..),
    newListPortfoliosForProduct,

    -- * Request Lenses
    listPortfoliosForProduct_acceptLanguage,
    listPortfoliosForProduct_pageSize,
    listPortfoliosForProduct_pageToken,
    listPortfoliosForProduct_productId,

    -- * Destructuring the Response
    ListPortfoliosForProductResponse (..),
    newListPortfoliosForProductResponse,

    -- * Response Lenses
    listPortfoliosForProductResponse_nextPageToken,
    listPortfoliosForProductResponse_portfolioDetails,
    listPortfoliosForProductResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newListPortfoliosForProduct' smart constructor.
data ListPortfoliosForProduct = ListPortfoliosForProduct'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    productId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPortfoliosForProduct' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'listPortfoliosForProduct_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'pageSize', 'listPortfoliosForProduct_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listPortfoliosForProduct_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'productId', 'listPortfoliosForProduct_productId' - The product identifier.
newListPortfoliosForProduct ::
  -- | 'productId'
  Prelude.Text ->
  ListPortfoliosForProduct
newListPortfoliosForProduct pProductId_ =
  ListPortfoliosForProduct'
    { acceptLanguage =
        Prelude.Nothing,
      pageSize = Prelude.Nothing,
      pageToken = Prelude.Nothing,
      productId = pProductId_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listPortfoliosForProduct_acceptLanguage :: Lens.Lens' ListPortfoliosForProduct (Prelude.Maybe Prelude.Text)
listPortfoliosForProduct_acceptLanguage = Lens.lens (\ListPortfoliosForProduct' {acceptLanguage} -> acceptLanguage) (\s@ListPortfoliosForProduct' {} a -> s {acceptLanguage = a} :: ListPortfoliosForProduct)

-- | The maximum number of items to return with this call.
listPortfoliosForProduct_pageSize :: Lens.Lens' ListPortfoliosForProduct (Prelude.Maybe Prelude.Natural)
listPortfoliosForProduct_pageSize = Lens.lens (\ListPortfoliosForProduct' {pageSize} -> pageSize) (\s@ListPortfoliosForProduct' {} a -> s {pageSize = a} :: ListPortfoliosForProduct)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listPortfoliosForProduct_pageToken :: Lens.Lens' ListPortfoliosForProduct (Prelude.Maybe Prelude.Text)
listPortfoliosForProduct_pageToken = Lens.lens (\ListPortfoliosForProduct' {pageToken} -> pageToken) (\s@ListPortfoliosForProduct' {} a -> s {pageToken = a} :: ListPortfoliosForProduct)

-- | The product identifier.
listPortfoliosForProduct_productId :: Lens.Lens' ListPortfoliosForProduct Prelude.Text
listPortfoliosForProduct_productId = Lens.lens (\ListPortfoliosForProduct' {productId} -> productId) (\s@ListPortfoliosForProduct' {} a -> s {productId = a} :: ListPortfoliosForProduct)

instance Core.AWSPager ListPortfoliosForProduct where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPortfoliosForProductResponse_nextPageToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPortfoliosForProductResponse_portfolioDetails
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listPortfoliosForProduct_pageToken
          Lens..~ rs
          Lens.^? listPortfoliosForProductResponse_nextPageToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListPortfoliosForProduct where
  type
    AWSResponse ListPortfoliosForProduct =
      ListPortfoliosForProductResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPortfoliosForProductResponse'
            Prelude.<$> (x Data..?> "NextPageToken")
            Prelude.<*> ( x
                            Data..?> "PortfolioDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPortfoliosForProduct where
  hashWithSalt _salt ListPortfoliosForProduct' {..} =
    _salt
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` pageToken
      `Prelude.hashWithSalt` productId

instance Prelude.NFData ListPortfoliosForProduct where
  rnf ListPortfoliosForProduct' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf productId

instance Data.ToHeaders ListPortfoliosForProduct where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.ListPortfoliosForProduct" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPortfoliosForProduct where
  toJSON ListPortfoliosForProduct' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("PageSize" Data..=) Prelude.<$> pageSize,
            ("PageToken" Data..=) Prelude.<$> pageToken,
            Prelude.Just ("ProductId" Data..= productId)
          ]
      )

instance Data.ToPath ListPortfoliosForProduct where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPortfoliosForProduct where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPortfoliosForProductResponse' smart constructor.
data ListPortfoliosForProductResponse = ListPortfoliosForProductResponse'
  { -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the portfolios.
    portfolioDetails :: Prelude.Maybe [PortfolioDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPortfoliosForProductResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'listPortfoliosForProductResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'portfolioDetails', 'listPortfoliosForProductResponse_portfolioDetails' - Information about the portfolios.
--
-- 'httpStatus', 'listPortfoliosForProductResponse_httpStatus' - The response's http status code.
newListPortfoliosForProductResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPortfoliosForProductResponse
newListPortfoliosForProductResponse pHttpStatus_ =
  ListPortfoliosForProductResponse'
    { nextPageToken =
        Prelude.Nothing,
      portfolioDetails = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listPortfoliosForProductResponse_nextPageToken :: Lens.Lens' ListPortfoliosForProductResponse (Prelude.Maybe Prelude.Text)
listPortfoliosForProductResponse_nextPageToken = Lens.lens (\ListPortfoliosForProductResponse' {nextPageToken} -> nextPageToken) (\s@ListPortfoliosForProductResponse' {} a -> s {nextPageToken = a} :: ListPortfoliosForProductResponse)

-- | Information about the portfolios.
listPortfoliosForProductResponse_portfolioDetails :: Lens.Lens' ListPortfoliosForProductResponse (Prelude.Maybe [PortfolioDetail])
listPortfoliosForProductResponse_portfolioDetails = Lens.lens (\ListPortfoliosForProductResponse' {portfolioDetails} -> portfolioDetails) (\s@ListPortfoliosForProductResponse' {} a -> s {portfolioDetails = a} :: ListPortfoliosForProductResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPortfoliosForProductResponse_httpStatus :: Lens.Lens' ListPortfoliosForProductResponse Prelude.Int
listPortfoliosForProductResponse_httpStatus = Lens.lens (\ListPortfoliosForProductResponse' {httpStatus} -> httpStatus) (\s@ListPortfoliosForProductResponse' {} a -> s {httpStatus = a} :: ListPortfoliosForProductResponse)

instance
  Prelude.NFData
    ListPortfoliosForProductResponse
  where
  rnf ListPortfoliosForProductResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf portfolioDetails
      `Prelude.seq` Prelude.rnf httpStatus
