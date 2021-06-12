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
-- Module      : Network.AWS.Pricing.GetProducts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all products that match the filter criteria.
--
-- This operation returns paginated results.
module Network.AWS.Pricing.GetProducts
  ( -- * Creating a Request
    GetProducts (..),
    newGetProducts,

    -- * Request Lenses
    getProducts_nextToken,
    getProducts_maxResults,
    getProducts_serviceCode,
    getProducts_formatVersion,
    getProducts_filters,

    -- * Destructuring the Response
    GetProductsResponse (..),
    newGetProductsResponse,

    -- * Response Lenses
    getProductsResponse_priceList,
    getProductsResponse_nextToken,
    getProductsResponse_formatVersion,
    getProductsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pricing.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetProducts' smart constructor.
data GetProducts = GetProducts'
  { -- | The pagination token that indicates the next set of results that you
    -- want to retrieve.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | The code for the service whose products you want to retrieve.
    serviceCode :: Core.Maybe Core.Text,
    -- | The format version that you want the response to be in.
    --
    -- Valid values are: @aws_v1@
    formatVersion :: Core.Maybe Core.Text,
    -- | The list of filters that limit the returned products. only products that
    -- match all filters are returned.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetProducts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getProducts_nextToken' - The pagination token that indicates the next set of results that you
-- want to retrieve.
--
-- 'maxResults', 'getProducts_maxResults' - The maximum number of results to return in the response.
--
-- 'serviceCode', 'getProducts_serviceCode' - The code for the service whose products you want to retrieve.
--
-- 'formatVersion', 'getProducts_formatVersion' - The format version that you want the response to be in.
--
-- Valid values are: @aws_v1@
--
-- 'filters', 'getProducts_filters' - The list of filters that limit the returned products. only products that
-- match all filters are returned.
newGetProducts ::
  GetProducts
newGetProducts =
  GetProducts'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      serviceCode = Core.Nothing,
      formatVersion = Core.Nothing,
      filters = Core.Nothing
    }

-- | The pagination token that indicates the next set of results that you
-- want to retrieve.
getProducts_nextToken :: Lens.Lens' GetProducts (Core.Maybe Core.Text)
getProducts_nextToken = Lens.lens (\GetProducts' {nextToken} -> nextToken) (\s@GetProducts' {} a -> s {nextToken = a} :: GetProducts)

-- | The maximum number of results to return in the response.
getProducts_maxResults :: Lens.Lens' GetProducts (Core.Maybe Core.Natural)
getProducts_maxResults = Lens.lens (\GetProducts' {maxResults} -> maxResults) (\s@GetProducts' {} a -> s {maxResults = a} :: GetProducts)

-- | The code for the service whose products you want to retrieve.
getProducts_serviceCode :: Lens.Lens' GetProducts (Core.Maybe Core.Text)
getProducts_serviceCode = Lens.lens (\GetProducts' {serviceCode} -> serviceCode) (\s@GetProducts' {} a -> s {serviceCode = a} :: GetProducts)

-- | The format version that you want the response to be in.
--
-- Valid values are: @aws_v1@
getProducts_formatVersion :: Lens.Lens' GetProducts (Core.Maybe Core.Text)
getProducts_formatVersion = Lens.lens (\GetProducts' {formatVersion} -> formatVersion) (\s@GetProducts' {} a -> s {formatVersion = a} :: GetProducts)

-- | The list of filters that limit the returned products. only products that
-- match all filters are returned.
getProducts_filters :: Lens.Lens' GetProducts (Core.Maybe [Filter])
getProducts_filters = Lens.lens (\GetProducts' {filters} -> filters) (\s@GetProducts' {} a -> s {filters = a} :: GetProducts) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager GetProducts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getProductsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getProductsResponse_priceList Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getProducts_nextToken
          Lens..~ rs
          Lens.^? getProductsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetProducts where
  type AWSResponse GetProducts = GetProductsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProductsResponse'
            Core.<$> (x Core..?> "PriceList" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "FormatVersion")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetProducts

instance Core.NFData GetProducts

instance Core.ToHeaders GetProducts where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPriceListService.GetProducts" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetProducts where
  toJSON GetProducts' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("ServiceCode" Core..=) Core.<$> serviceCode,
            ("FormatVersion" Core..=) Core.<$> formatVersion,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath GetProducts where
  toPath = Core.const "/"

instance Core.ToQuery GetProducts where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetProductsResponse' smart constructor.
data GetProductsResponse = GetProductsResponse'
  { -- | The list of products that match your filters. The list contains both the
    -- product metadata and the price information.
    priceList :: Core.Maybe [Core.Text],
    -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Core.Maybe Core.Text,
    -- | The format version of the response. For example, aws_v1.
    formatVersion :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetProductsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'priceList', 'getProductsResponse_priceList' - The list of products that match your filters. The list contains both the
-- product metadata and the price information.
--
-- 'nextToken', 'getProductsResponse_nextToken' - The pagination token that indicates the next set of results to retrieve.
--
-- 'formatVersion', 'getProductsResponse_formatVersion' - The format version of the response. For example, aws_v1.
--
-- 'httpStatus', 'getProductsResponse_httpStatus' - The response's http status code.
newGetProductsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetProductsResponse
newGetProductsResponse pHttpStatus_ =
  GetProductsResponse'
    { priceList = Core.Nothing,
      nextToken = Core.Nothing,
      formatVersion = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of products that match your filters. The list contains both the
-- product metadata and the price information.
getProductsResponse_priceList :: Lens.Lens' GetProductsResponse (Core.Maybe [Core.Text])
getProductsResponse_priceList = Lens.lens (\GetProductsResponse' {priceList} -> priceList) (\s@GetProductsResponse' {} a -> s {priceList = a} :: GetProductsResponse) Core.. Lens.mapping Lens._Coerce

-- | The pagination token that indicates the next set of results to retrieve.
getProductsResponse_nextToken :: Lens.Lens' GetProductsResponse (Core.Maybe Core.Text)
getProductsResponse_nextToken = Lens.lens (\GetProductsResponse' {nextToken} -> nextToken) (\s@GetProductsResponse' {} a -> s {nextToken = a} :: GetProductsResponse)

-- | The format version of the response. For example, aws_v1.
getProductsResponse_formatVersion :: Lens.Lens' GetProductsResponse (Core.Maybe Core.Text)
getProductsResponse_formatVersion = Lens.lens (\GetProductsResponse' {formatVersion} -> formatVersion) (\s@GetProductsResponse' {} a -> s {formatVersion = a} :: GetProductsResponse)

-- | The response's http status code.
getProductsResponse_httpStatus :: Lens.Lens' GetProductsResponse Core.Int
getProductsResponse_httpStatus = Lens.lens (\GetProductsResponse' {httpStatus} -> httpStatus) (\s@GetProductsResponse' {} a -> s {httpStatus = a} :: GetProductsResponse)

instance Core.NFData GetProductsResponse
