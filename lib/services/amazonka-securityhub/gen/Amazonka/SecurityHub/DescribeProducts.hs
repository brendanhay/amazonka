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
-- Module      : Amazonka.SecurityHub.DescribeProducts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about product integrations in Security Hub.
--
-- You can optionally provide an integration ARN. If you provide an
-- integration ARN, then the results only include that integration.
--
-- If you do not provide an integration ARN, then the results include all
-- of the available product integrations.
--
-- This operation returns paginated results.
module Amazonka.SecurityHub.DescribeProducts
  ( -- * Creating a Request
    DescribeProducts (..),
    newDescribeProducts,

    -- * Request Lenses
    describeProducts_maxResults,
    describeProducts_nextToken,
    describeProducts_productArn,

    -- * Destructuring the Response
    DescribeProductsResponse (..),
    newDescribeProductsResponse,

    -- * Response Lenses
    describeProductsResponse_nextToken,
    describeProductsResponse_httpStatus,
    describeProductsResponse_products,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newDescribeProducts' smart constructor.
data DescribeProducts = DescribeProducts'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that is required for pagination. On your first call to the
    -- @DescribeProducts@ operation, set the value of this parameter to @NULL@.
    --
    -- For subsequent calls to the operation, to continue listing data, set the
    -- value of this parameter to the value returned from the previous
    -- response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the integration to return.
    productArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProducts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeProducts_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'describeProducts_nextToken' - The token that is required for pagination. On your first call to the
-- @DescribeProducts@ operation, set the value of this parameter to @NULL@.
--
-- For subsequent calls to the operation, to continue listing data, set the
-- value of this parameter to the value returned from the previous
-- response.
--
-- 'productArn', 'describeProducts_productArn' - The ARN of the integration to return.
newDescribeProducts ::
  DescribeProducts
newDescribeProducts =
  DescribeProducts'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      productArn = Prelude.Nothing
    }

-- | The maximum number of results to return.
describeProducts_maxResults :: Lens.Lens' DescribeProducts (Prelude.Maybe Prelude.Natural)
describeProducts_maxResults = Lens.lens (\DescribeProducts' {maxResults} -> maxResults) (\s@DescribeProducts' {} a -> s {maxResults = a} :: DescribeProducts)

-- | The token that is required for pagination. On your first call to the
-- @DescribeProducts@ operation, set the value of this parameter to @NULL@.
--
-- For subsequent calls to the operation, to continue listing data, set the
-- value of this parameter to the value returned from the previous
-- response.
describeProducts_nextToken :: Lens.Lens' DescribeProducts (Prelude.Maybe Prelude.Text)
describeProducts_nextToken = Lens.lens (\DescribeProducts' {nextToken} -> nextToken) (\s@DescribeProducts' {} a -> s {nextToken = a} :: DescribeProducts)

-- | The ARN of the integration to return.
describeProducts_productArn :: Lens.Lens' DescribeProducts (Prelude.Maybe Prelude.Text)
describeProducts_productArn = Lens.lens (\DescribeProducts' {productArn} -> productArn) (\s@DescribeProducts' {} a -> s {productArn = a} :: DescribeProducts)

instance Core.AWSPager DescribeProducts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeProductsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. describeProductsResponse_products) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeProducts_nextToken
          Lens..~ rs
          Lens.^? describeProductsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeProducts where
  type
    AWSResponse DescribeProducts =
      DescribeProductsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProductsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Products" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable DescribeProducts where
  hashWithSalt _salt DescribeProducts' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` productArn

instance Prelude.NFData DescribeProducts where
  rnf DescribeProducts' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf productArn

instance Data.ToHeaders DescribeProducts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeProducts where
  toPath = Prelude.const "/products"

instance Data.ToQuery DescribeProducts where
  toQuery DescribeProducts' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "ProductArn" Data.=: productArn
      ]

-- | /See:/ 'newDescribeProductsResponse' smart constructor.
data DescribeProductsResponse = DescribeProductsResponse'
  { -- | The pagination token to use to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of products, including details for each product.
    products :: [Product]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProductsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeProductsResponse_nextToken' - The pagination token to use to request the next page of results.
--
-- 'httpStatus', 'describeProductsResponse_httpStatus' - The response's http status code.
--
-- 'products', 'describeProductsResponse_products' - A list of products, including details for each product.
newDescribeProductsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProductsResponse
newDescribeProductsResponse pHttpStatus_ =
  DescribeProductsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      products = Prelude.mempty
    }

-- | The pagination token to use to request the next page of results.
describeProductsResponse_nextToken :: Lens.Lens' DescribeProductsResponse (Prelude.Maybe Prelude.Text)
describeProductsResponse_nextToken = Lens.lens (\DescribeProductsResponse' {nextToken} -> nextToken) (\s@DescribeProductsResponse' {} a -> s {nextToken = a} :: DescribeProductsResponse)

-- | The response's http status code.
describeProductsResponse_httpStatus :: Lens.Lens' DescribeProductsResponse Prelude.Int
describeProductsResponse_httpStatus = Lens.lens (\DescribeProductsResponse' {httpStatus} -> httpStatus) (\s@DescribeProductsResponse' {} a -> s {httpStatus = a} :: DescribeProductsResponse)

-- | A list of products, including details for each product.
describeProductsResponse_products :: Lens.Lens' DescribeProductsResponse [Product]
describeProductsResponse_products = Lens.lens (\DescribeProductsResponse' {products} -> products) (\s@DescribeProductsResponse' {} a -> s {products = a} :: DescribeProductsResponse) Prelude.. Lens.coerced

instance Prelude.NFData DescribeProductsResponse where
  rnf DescribeProductsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf products
