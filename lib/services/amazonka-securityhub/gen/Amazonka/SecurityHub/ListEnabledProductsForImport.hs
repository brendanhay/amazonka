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
-- Module      : Amazonka.SecurityHub.ListEnabledProductsForImport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all findings-generating solutions (products) that you are
-- subscribed to receive findings from in Security Hub.
--
-- This operation returns paginated results.
module Amazonka.SecurityHub.ListEnabledProductsForImport
  ( -- * Creating a Request
    ListEnabledProductsForImport (..),
    newListEnabledProductsForImport,

    -- * Request Lenses
    listEnabledProductsForImport_maxResults,
    listEnabledProductsForImport_nextToken,

    -- * Destructuring the Response
    ListEnabledProductsForImportResponse (..),
    newListEnabledProductsForImportResponse,

    -- * Response Lenses
    listEnabledProductsForImportResponse_nextToken,
    listEnabledProductsForImportResponse_productSubscriptions,
    listEnabledProductsForImportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newListEnabledProductsForImport' smart constructor.
data ListEnabledProductsForImport = ListEnabledProductsForImport'
  { -- | The maximum number of items to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that is required for pagination. On your first call to the
    -- @ListEnabledProductsForImport@ operation, set the value of this
    -- parameter to @NULL@.
    --
    -- For subsequent calls to the operation, to continue listing data, set the
    -- value of this parameter to the value returned from the previous
    -- response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnabledProductsForImport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listEnabledProductsForImport_maxResults' - The maximum number of items to return in the response.
--
-- 'nextToken', 'listEnabledProductsForImport_nextToken' - The token that is required for pagination. On your first call to the
-- @ListEnabledProductsForImport@ operation, set the value of this
-- parameter to @NULL@.
--
-- For subsequent calls to the operation, to continue listing data, set the
-- value of this parameter to the value returned from the previous
-- response.
newListEnabledProductsForImport ::
  ListEnabledProductsForImport
newListEnabledProductsForImport =
  ListEnabledProductsForImport'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of items to return in the response.
listEnabledProductsForImport_maxResults :: Lens.Lens' ListEnabledProductsForImport (Prelude.Maybe Prelude.Natural)
listEnabledProductsForImport_maxResults = Lens.lens (\ListEnabledProductsForImport' {maxResults} -> maxResults) (\s@ListEnabledProductsForImport' {} a -> s {maxResults = a} :: ListEnabledProductsForImport)

-- | The token that is required for pagination. On your first call to the
-- @ListEnabledProductsForImport@ operation, set the value of this
-- parameter to @NULL@.
--
-- For subsequent calls to the operation, to continue listing data, set the
-- value of this parameter to the value returned from the previous
-- response.
listEnabledProductsForImport_nextToken :: Lens.Lens' ListEnabledProductsForImport (Prelude.Maybe Prelude.Text)
listEnabledProductsForImport_nextToken = Lens.lens (\ListEnabledProductsForImport' {nextToken} -> nextToken) (\s@ListEnabledProductsForImport' {} a -> s {nextToken = a} :: ListEnabledProductsForImport)

instance Core.AWSPager ListEnabledProductsForImport where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEnabledProductsForImportResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listEnabledProductsForImportResponse_productSubscriptions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listEnabledProductsForImport_nextToken
          Lens..~ rs
          Lens.^? listEnabledProductsForImportResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListEnabledProductsForImport where
  type
    AWSResponse ListEnabledProductsForImport =
      ListEnabledProductsForImportResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEnabledProductsForImportResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "ProductSubscriptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListEnabledProductsForImport
  where
  hashWithSalt _salt ListEnabledProductsForImport' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListEnabledProductsForImport where
  rnf ListEnabledProductsForImport' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListEnabledProductsForImport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListEnabledProductsForImport where
  toPath = Prelude.const "/productSubscriptions"

instance Data.ToQuery ListEnabledProductsForImport where
  toQuery ListEnabledProductsForImport' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListEnabledProductsForImportResponse' smart constructor.
data ListEnabledProductsForImportResponse = ListEnabledProductsForImportResponse'
  { -- | The pagination token to use to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of ARNs for the resources that represent your subscriptions to
    -- products.
    productSubscriptions :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnabledProductsForImportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEnabledProductsForImportResponse_nextToken' - The pagination token to use to request the next page of results.
--
-- 'productSubscriptions', 'listEnabledProductsForImportResponse_productSubscriptions' - The list of ARNs for the resources that represent your subscriptions to
-- products.
--
-- 'httpStatus', 'listEnabledProductsForImportResponse_httpStatus' - The response's http status code.
newListEnabledProductsForImportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEnabledProductsForImportResponse
newListEnabledProductsForImportResponse pHttpStatus_ =
  ListEnabledProductsForImportResponse'
    { nextToken =
        Prelude.Nothing,
      productSubscriptions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use to request the next page of results.
listEnabledProductsForImportResponse_nextToken :: Lens.Lens' ListEnabledProductsForImportResponse (Prelude.Maybe Prelude.Text)
listEnabledProductsForImportResponse_nextToken = Lens.lens (\ListEnabledProductsForImportResponse' {nextToken} -> nextToken) (\s@ListEnabledProductsForImportResponse' {} a -> s {nextToken = a} :: ListEnabledProductsForImportResponse)

-- | The list of ARNs for the resources that represent your subscriptions to
-- products.
listEnabledProductsForImportResponse_productSubscriptions :: Lens.Lens' ListEnabledProductsForImportResponse (Prelude.Maybe [Prelude.Text])
listEnabledProductsForImportResponse_productSubscriptions = Lens.lens (\ListEnabledProductsForImportResponse' {productSubscriptions} -> productSubscriptions) (\s@ListEnabledProductsForImportResponse' {} a -> s {productSubscriptions = a} :: ListEnabledProductsForImportResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listEnabledProductsForImportResponse_httpStatus :: Lens.Lens' ListEnabledProductsForImportResponse Prelude.Int
listEnabledProductsForImportResponse_httpStatus = Lens.lens (\ListEnabledProductsForImportResponse' {httpStatus} -> httpStatus) (\s@ListEnabledProductsForImportResponse' {} a -> s {httpStatus = a} :: ListEnabledProductsForImportResponse)

instance
  Prelude.NFData
    ListEnabledProductsForImportResponse
  where
  rnf ListEnabledProductsForImportResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf productSubscriptions
      `Prelude.seq` Prelude.rnf httpStatus
