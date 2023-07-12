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
-- Module      : Amazonka.LicenseManagerUserSubscriptions.ListProductSubscriptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the user-based subscription products available from an identity
-- provider.
--
-- This operation returns paginated results.
module Amazonka.LicenseManagerUserSubscriptions.ListProductSubscriptions
  ( -- * Creating a Request
    ListProductSubscriptions (..),
    newListProductSubscriptions,

    -- * Request Lenses
    listProductSubscriptions_filters,
    listProductSubscriptions_maxResults,
    listProductSubscriptions_nextToken,
    listProductSubscriptions_identityProvider,
    listProductSubscriptions_product,

    -- * Destructuring the Response
    ListProductSubscriptionsResponse (..),
    newListProductSubscriptionsResponse,

    -- * Response Lenses
    listProductSubscriptionsResponse_nextToken,
    listProductSubscriptionsResponse_productUserSummaries,
    listProductSubscriptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManagerUserSubscriptions.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListProductSubscriptions' smart constructor.
data ListProductSubscriptions = ListProductSubscriptions'
  { -- | An array of structures that you can use to filter the results to those
    -- that match one or more sets of key-value pairs that you specify.
    filters :: Prelude.Maybe [Filter],
    -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An object that specifies details for the identity provider.
    identityProvider :: IdentityProvider,
    -- | The name of the user-based subscription product.
    product :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProductSubscriptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listProductSubscriptions_filters' - An array of structures that you can use to filter the results to those
-- that match one or more sets of key-value pairs that you specify.
--
-- 'maxResults', 'listProductSubscriptions_maxResults' - Maximum number of results to return in a single call.
--
-- 'nextToken', 'listProductSubscriptions_nextToken' - Token for the next set of results.
--
-- 'identityProvider', 'listProductSubscriptions_identityProvider' - An object that specifies details for the identity provider.
--
-- 'product', 'listProductSubscriptions_product' - The name of the user-based subscription product.
newListProductSubscriptions ::
  -- | 'identityProvider'
  IdentityProvider ->
  -- | 'product'
  Prelude.Text ->
  ListProductSubscriptions
newListProductSubscriptions
  pIdentityProvider_
  pProduct_ =
    ListProductSubscriptions'
      { filters =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        identityProvider = pIdentityProvider_,
        product = pProduct_
      }

-- | An array of structures that you can use to filter the results to those
-- that match one or more sets of key-value pairs that you specify.
listProductSubscriptions_filters :: Lens.Lens' ListProductSubscriptions (Prelude.Maybe [Filter])
listProductSubscriptions_filters = Lens.lens (\ListProductSubscriptions' {filters} -> filters) (\s@ListProductSubscriptions' {} a -> s {filters = a} :: ListProductSubscriptions) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of results to return in a single call.
listProductSubscriptions_maxResults :: Lens.Lens' ListProductSubscriptions (Prelude.Maybe Prelude.Int)
listProductSubscriptions_maxResults = Lens.lens (\ListProductSubscriptions' {maxResults} -> maxResults) (\s@ListProductSubscriptions' {} a -> s {maxResults = a} :: ListProductSubscriptions)

-- | Token for the next set of results.
listProductSubscriptions_nextToken :: Lens.Lens' ListProductSubscriptions (Prelude.Maybe Prelude.Text)
listProductSubscriptions_nextToken = Lens.lens (\ListProductSubscriptions' {nextToken} -> nextToken) (\s@ListProductSubscriptions' {} a -> s {nextToken = a} :: ListProductSubscriptions)

-- | An object that specifies details for the identity provider.
listProductSubscriptions_identityProvider :: Lens.Lens' ListProductSubscriptions IdentityProvider
listProductSubscriptions_identityProvider = Lens.lens (\ListProductSubscriptions' {identityProvider} -> identityProvider) (\s@ListProductSubscriptions' {} a -> s {identityProvider = a} :: ListProductSubscriptions)

-- | The name of the user-based subscription product.
listProductSubscriptions_product :: Lens.Lens' ListProductSubscriptions Prelude.Text
listProductSubscriptions_product = Lens.lens (\ListProductSubscriptions' {product} -> product) (\s@ListProductSubscriptions' {} a -> s {product = a} :: ListProductSubscriptions)

instance Core.AWSPager ListProductSubscriptions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProductSubscriptionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listProductSubscriptionsResponse_productUserSummaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listProductSubscriptions_nextToken
          Lens..~ rs
          Lens.^? listProductSubscriptionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListProductSubscriptions where
  type
    AWSResponse ListProductSubscriptions =
      ListProductSubscriptionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProductSubscriptionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "ProductUserSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProductSubscriptions where
  hashWithSalt _salt ListProductSubscriptions' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` identityProvider
      `Prelude.hashWithSalt` product

instance Prelude.NFData ListProductSubscriptions where
  rnf ListProductSubscriptions' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf identityProvider
      `Prelude.seq` Prelude.rnf product

instance Data.ToHeaders ListProductSubscriptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListProductSubscriptions where
  toJSON ListProductSubscriptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("IdentityProvider" Data..= identityProvider),
            Prelude.Just ("Product" Data..= product)
          ]
      )

instance Data.ToPath ListProductSubscriptions where
  toPath =
    Prelude.const "/user/ListProductSubscriptions"

instance Data.ToQuery ListProductSubscriptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListProductSubscriptionsResponse' smart constructor.
data ListProductSubscriptionsResponse = ListProductSubscriptionsResponse'
  { -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Metadata that describes the list product subscriptions operation.
    productUserSummaries :: Prelude.Maybe [ProductUserSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProductSubscriptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProductSubscriptionsResponse_nextToken' - Token for the next set of results.
--
-- 'productUserSummaries', 'listProductSubscriptionsResponse_productUserSummaries' - Metadata that describes the list product subscriptions operation.
--
-- 'httpStatus', 'listProductSubscriptionsResponse_httpStatus' - The response's http status code.
newListProductSubscriptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProductSubscriptionsResponse
newListProductSubscriptionsResponse pHttpStatus_ =
  ListProductSubscriptionsResponse'
    { nextToken =
        Prelude.Nothing,
      productUserSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token for the next set of results.
listProductSubscriptionsResponse_nextToken :: Lens.Lens' ListProductSubscriptionsResponse (Prelude.Maybe Prelude.Text)
listProductSubscriptionsResponse_nextToken = Lens.lens (\ListProductSubscriptionsResponse' {nextToken} -> nextToken) (\s@ListProductSubscriptionsResponse' {} a -> s {nextToken = a} :: ListProductSubscriptionsResponse)

-- | Metadata that describes the list product subscriptions operation.
listProductSubscriptionsResponse_productUserSummaries :: Lens.Lens' ListProductSubscriptionsResponse (Prelude.Maybe [ProductUserSummary])
listProductSubscriptionsResponse_productUserSummaries = Lens.lens (\ListProductSubscriptionsResponse' {productUserSummaries} -> productUserSummaries) (\s@ListProductSubscriptionsResponse' {} a -> s {productUserSummaries = a} :: ListProductSubscriptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listProductSubscriptionsResponse_httpStatus :: Lens.Lens' ListProductSubscriptionsResponse Prelude.Int
listProductSubscriptionsResponse_httpStatus = Lens.lens (\ListProductSubscriptionsResponse' {httpStatus} -> httpStatus) (\s@ListProductSubscriptionsResponse' {} a -> s {httpStatus = a} :: ListProductSubscriptionsResponse)

instance
  Prelude.NFData
    ListProductSubscriptionsResponse
  where
  rnf ListProductSubscriptionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf productUserSummaries
      `Prelude.seq` Prelude.rnf httpStatus
