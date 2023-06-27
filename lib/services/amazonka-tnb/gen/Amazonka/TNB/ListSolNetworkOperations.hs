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
-- Module      : Amazonka.TNB.ListSolNetworkOperations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists details for a network operation, including when the operation
-- started and the status of the operation.
--
-- A network operation is any operation that is done to your network, such
-- as network instance instantiation or termination.
--
-- This operation returns paginated results.
module Amazonka.TNB.ListSolNetworkOperations
  ( -- * Creating a Request
    ListSolNetworkOperations (..),
    newListSolNetworkOperations,

    -- * Request Lenses
    listSolNetworkOperations_maxResults,
    listSolNetworkOperations_nextToken,

    -- * Destructuring the Response
    ListSolNetworkOperationsResponse (..),
    newListSolNetworkOperationsResponse,

    -- * Response Lenses
    listSolNetworkOperationsResponse_networkOperations,
    listSolNetworkOperationsResponse_nextToken,
    listSolNetworkOperationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newListSolNetworkOperations' smart constructor.
data ListSolNetworkOperations = ListSolNetworkOperations'
  { -- | The maximum number of results to include in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolNetworkOperations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSolNetworkOperations_maxResults' - The maximum number of results to include in the response.
--
-- 'nextToken', 'listSolNetworkOperations_nextToken' - The token for the next page of results.
newListSolNetworkOperations ::
  ListSolNetworkOperations
newListSolNetworkOperations =
  ListSolNetworkOperations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to include in the response.
listSolNetworkOperations_maxResults :: Lens.Lens' ListSolNetworkOperations (Prelude.Maybe Prelude.Natural)
listSolNetworkOperations_maxResults = Lens.lens (\ListSolNetworkOperations' {maxResults} -> maxResults) (\s@ListSolNetworkOperations' {} a -> s {maxResults = a} :: ListSolNetworkOperations)

-- | The token for the next page of results.
listSolNetworkOperations_nextToken :: Lens.Lens' ListSolNetworkOperations (Prelude.Maybe Prelude.Text)
listSolNetworkOperations_nextToken = Lens.lens (\ListSolNetworkOperations' {nextToken} -> nextToken) (\s@ListSolNetworkOperations' {} a -> s {nextToken = a} :: ListSolNetworkOperations)

instance Core.AWSPager ListSolNetworkOperations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSolNetworkOperationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSolNetworkOperationsResponse_networkOperations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSolNetworkOperations_nextToken
          Lens..~ rs
          Lens.^? listSolNetworkOperationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSolNetworkOperations where
  type
    AWSResponse ListSolNetworkOperations =
      ListSolNetworkOperationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSolNetworkOperationsResponse'
            Prelude.<$> ( x
                            Data..?> "networkOperations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSolNetworkOperations where
  hashWithSalt _salt ListSolNetworkOperations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSolNetworkOperations where
  rnf ListSolNetworkOperations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListSolNetworkOperations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSolNetworkOperations where
  toPath = Prelude.const "/sol/nslcm/v1/ns_lcm_op_occs"

instance Data.ToQuery ListSolNetworkOperations where
  toQuery ListSolNetworkOperations' {..} =
    Prelude.mconcat
      [ "max_results" Data.=: maxResults,
        "nextpage_opaque_marker" Data.=: nextToken
      ]

-- | /See:/ 'newListSolNetworkOperationsResponse' smart constructor.
data ListSolNetworkOperationsResponse = ListSolNetworkOperationsResponse'
  { -- | Lists network operation occurrences. Lifecycle management operations are
    -- deploy, update, or delete operations.
    networkOperations :: Prelude.Maybe [ListSolNetworkOperationsInfo],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolNetworkOperationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkOperations', 'listSolNetworkOperationsResponse_networkOperations' - Lists network operation occurrences. Lifecycle management operations are
-- deploy, update, or delete operations.
--
-- 'nextToken', 'listSolNetworkOperationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'listSolNetworkOperationsResponse_httpStatus' - The response's http status code.
newListSolNetworkOperationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSolNetworkOperationsResponse
newListSolNetworkOperationsResponse pHttpStatus_ =
  ListSolNetworkOperationsResponse'
    { networkOperations =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Lists network operation occurrences. Lifecycle management operations are
-- deploy, update, or delete operations.
listSolNetworkOperationsResponse_networkOperations :: Lens.Lens' ListSolNetworkOperationsResponse (Prelude.Maybe [ListSolNetworkOperationsInfo])
listSolNetworkOperationsResponse_networkOperations = Lens.lens (\ListSolNetworkOperationsResponse' {networkOperations} -> networkOperations) (\s@ListSolNetworkOperationsResponse' {} a -> s {networkOperations = a} :: ListSolNetworkOperationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listSolNetworkOperationsResponse_nextToken :: Lens.Lens' ListSolNetworkOperationsResponse (Prelude.Maybe Prelude.Text)
listSolNetworkOperationsResponse_nextToken = Lens.lens (\ListSolNetworkOperationsResponse' {nextToken} -> nextToken) (\s@ListSolNetworkOperationsResponse' {} a -> s {nextToken = a} :: ListSolNetworkOperationsResponse)

-- | The response's http status code.
listSolNetworkOperationsResponse_httpStatus :: Lens.Lens' ListSolNetworkOperationsResponse Prelude.Int
listSolNetworkOperationsResponse_httpStatus = Lens.lens (\ListSolNetworkOperationsResponse' {httpStatus} -> httpStatus) (\s@ListSolNetworkOperationsResponse' {} a -> s {httpStatus = a} :: ListSolNetworkOperationsResponse)

instance
  Prelude.NFData
    ListSolNetworkOperationsResponse
  where
  rnf ListSolNetworkOperationsResponse' {..} =
    Prelude.rnf networkOperations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
