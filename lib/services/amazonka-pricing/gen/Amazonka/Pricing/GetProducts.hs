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
-- Module      : Amazonka.Pricing.GetProducts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all products that match the filter criteria.
--
-- This operation returns paginated results.
module Amazonka.Pricing.GetProducts
  ( -- * Creating a Request
    GetProducts (..),
    newGetProducts,

    -- * Request Lenses
    getProducts_filters,
    getProducts_formatVersion,
    getProducts_maxResults,
    getProducts_nextToken,
    getProducts_serviceCode,

    -- * Destructuring the Response
    GetProductsResponse (..),
    newGetProductsResponse,

    -- * Response Lenses
    getProductsResponse_formatVersion,
    getProductsResponse_nextToken,
    getProductsResponse_priceList,
    getProductsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Pricing.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetProducts' smart constructor.
data GetProducts = GetProducts'
  { -- | The list of filters that limit the returned products. only products that
    -- match all filters are returned.
    filters :: Prelude.Maybe [Filter],
    -- | The format version that you want the response to be in.
    --
    -- Valid values are: @aws_v1@
    formatVersion :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that indicates the next set of results that you
    -- want to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The code for the service whose products you want to retrieve.
    serviceCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProducts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'getProducts_filters' - The list of filters that limit the returned products. only products that
-- match all filters are returned.
--
-- 'formatVersion', 'getProducts_formatVersion' - The format version that you want the response to be in.
--
-- Valid values are: @aws_v1@
--
-- 'maxResults', 'getProducts_maxResults' - The maximum number of results to return in the response.
--
-- 'nextToken', 'getProducts_nextToken' - The pagination token that indicates the next set of results that you
-- want to retrieve.
--
-- 'serviceCode', 'getProducts_serviceCode' - The code for the service whose products you want to retrieve.
newGetProducts ::
  -- | 'serviceCode'
  Prelude.Text ->
  GetProducts
newGetProducts pServiceCode_ =
  GetProducts'
    { filters = Prelude.Nothing,
      formatVersion = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serviceCode = pServiceCode_
    }

-- | The list of filters that limit the returned products. only products that
-- match all filters are returned.
getProducts_filters :: Lens.Lens' GetProducts (Prelude.Maybe [Filter])
getProducts_filters = Lens.lens (\GetProducts' {filters} -> filters) (\s@GetProducts' {} a -> s {filters = a} :: GetProducts) Prelude.. Lens.mapping Lens.coerced

-- | The format version that you want the response to be in.
--
-- Valid values are: @aws_v1@
getProducts_formatVersion :: Lens.Lens' GetProducts (Prelude.Maybe Prelude.Text)
getProducts_formatVersion = Lens.lens (\GetProducts' {formatVersion} -> formatVersion) (\s@GetProducts' {} a -> s {formatVersion = a} :: GetProducts)

-- | The maximum number of results to return in the response.
getProducts_maxResults :: Lens.Lens' GetProducts (Prelude.Maybe Prelude.Natural)
getProducts_maxResults = Lens.lens (\GetProducts' {maxResults} -> maxResults) (\s@GetProducts' {} a -> s {maxResults = a} :: GetProducts)

-- | The pagination token that indicates the next set of results that you
-- want to retrieve.
getProducts_nextToken :: Lens.Lens' GetProducts (Prelude.Maybe Prelude.Text)
getProducts_nextToken = Lens.lens (\GetProducts' {nextToken} -> nextToken) (\s@GetProducts' {} a -> s {nextToken = a} :: GetProducts)

-- | The code for the service whose products you want to retrieve.
getProducts_serviceCode :: Lens.Lens' GetProducts Prelude.Text
getProducts_serviceCode = Lens.lens (\GetProducts' {serviceCode} -> serviceCode) (\s@GetProducts' {} a -> s {serviceCode = a} :: GetProducts)

instance Core.AWSPager GetProducts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getProductsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getProductsResponse_priceList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getProducts_nextToken
              Lens..~ rs
              Lens.^? getProductsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest GetProducts where
  type AWSResponse GetProducts = GetProductsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProductsResponse'
            Prelude.<$> (x Data..?> "FormatVersion")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "PriceList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetProducts where
  hashWithSalt _salt GetProducts' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` formatVersion
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` serviceCode

instance Prelude.NFData GetProducts where
  rnf GetProducts' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf formatVersion `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken `Prelude.seq`
            Prelude.rnf serviceCode

instance Data.ToHeaders GetProducts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSPriceListService.GetProducts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetProducts where
  toJSON GetProducts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("FormatVersion" Data..=) Prelude.<$> formatVersion,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("ServiceCode" Data..= serviceCode)
          ]
      )

instance Data.ToPath GetProducts where
  toPath = Prelude.const "/"

instance Data.ToQuery GetProducts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetProductsResponse' smart constructor.
data GetProductsResponse = GetProductsResponse'
  { -- | The format version of the response. For example, aws_v1.
    formatVersion :: Prelude.Maybe Prelude.Text,
    -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of products that match your filters. The list contains both the
    -- product metadata and the price information.
    priceList :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProductsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'formatVersion', 'getProductsResponse_formatVersion' - The format version of the response. For example, aws_v1.
--
-- 'nextToken', 'getProductsResponse_nextToken' - The pagination token that indicates the next set of results to retrieve.
--
-- 'priceList', 'getProductsResponse_priceList' - The list of products that match your filters. The list contains both the
-- product metadata and the price information.
--
-- 'httpStatus', 'getProductsResponse_httpStatus' - The response's http status code.
newGetProductsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetProductsResponse
newGetProductsResponse pHttpStatus_ =
  GetProductsResponse'
    { formatVersion =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      priceList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The format version of the response. For example, aws_v1.
getProductsResponse_formatVersion :: Lens.Lens' GetProductsResponse (Prelude.Maybe Prelude.Text)
getProductsResponse_formatVersion = Lens.lens (\GetProductsResponse' {formatVersion} -> formatVersion) (\s@GetProductsResponse' {} a -> s {formatVersion = a} :: GetProductsResponse)

-- | The pagination token that indicates the next set of results to retrieve.
getProductsResponse_nextToken :: Lens.Lens' GetProductsResponse (Prelude.Maybe Prelude.Text)
getProductsResponse_nextToken = Lens.lens (\GetProductsResponse' {nextToken} -> nextToken) (\s@GetProductsResponse' {} a -> s {nextToken = a} :: GetProductsResponse)

-- | The list of products that match your filters. The list contains both the
-- product metadata and the price information.
getProductsResponse_priceList :: Lens.Lens' GetProductsResponse (Prelude.Maybe [Prelude.Text])
getProductsResponse_priceList = Lens.lens (\GetProductsResponse' {priceList} -> priceList) (\s@GetProductsResponse' {} a -> s {priceList = a} :: GetProductsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getProductsResponse_httpStatus :: Lens.Lens' GetProductsResponse Prelude.Int
getProductsResponse_httpStatus = Lens.lens (\GetProductsResponse' {httpStatus} -> httpStatus) (\s@GetProductsResponse' {} a -> s {httpStatus = a} :: GetProductsResponse)

instance Prelude.NFData GetProductsResponse where
  rnf GetProductsResponse' {..} =
    Prelude.rnf formatVersion `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf priceList `Prelude.seq`
          Prelude.rnf httpStatus
