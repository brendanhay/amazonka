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
-- Module      : Amazonka.TNB.ListSolFunctionInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists network function instances.
--
-- A network function instance is a function in a function package .
--
-- This operation returns paginated results.
module Amazonka.TNB.ListSolFunctionInstances
  ( -- * Creating a Request
    ListSolFunctionInstances (..),
    newListSolFunctionInstances,

    -- * Request Lenses
    listSolFunctionInstances_maxResults,
    listSolFunctionInstances_nextToken,

    -- * Destructuring the Response
    ListSolFunctionInstancesResponse (..),
    newListSolFunctionInstancesResponse,

    -- * Response Lenses
    listSolFunctionInstancesResponse_functionInstances,
    listSolFunctionInstancesResponse_nextToken,
    listSolFunctionInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newListSolFunctionInstances' smart constructor.
data ListSolFunctionInstances = ListSolFunctionInstances'
  { -- | The maximum number of results to include in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolFunctionInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSolFunctionInstances_maxResults' - The maximum number of results to include in the response.
--
-- 'nextToken', 'listSolFunctionInstances_nextToken' - The token for the next page of results.
newListSolFunctionInstances ::
  ListSolFunctionInstances
newListSolFunctionInstances =
  ListSolFunctionInstances'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to include in the response.
listSolFunctionInstances_maxResults :: Lens.Lens' ListSolFunctionInstances (Prelude.Maybe Prelude.Natural)
listSolFunctionInstances_maxResults = Lens.lens (\ListSolFunctionInstances' {maxResults} -> maxResults) (\s@ListSolFunctionInstances' {} a -> s {maxResults = a} :: ListSolFunctionInstances)

-- | The token for the next page of results.
listSolFunctionInstances_nextToken :: Lens.Lens' ListSolFunctionInstances (Prelude.Maybe Prelude.Text)
listSolFunctionInstances_nextToken = Lens.lens (\ListSolFunctionInstances' {nextToken} -> nextToken) (\s@ListSolFunctionInstances' {} a -> s {nextToken = a} :: ListSolFunctionInstances)

instance Core.AWSPager ListSolFunctionInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSolFunctionInstancesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSolFunctionInstancesResponse_functionInstances
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSolFunctionInstances_nextToken
          Lens..~ rs
          Lens.^? listSolFunctionInstancesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSolFunctionInstances where
  type
    AWSResponse ListSolFunctionInstances =
      ListSolFunctionInstancesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSolFunctionInstancesResponse'
            Prelude.<$> ( x
                            Data..?> "functionInstances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSolFunctionInstances where
  hashWithSalt _salt ListSolFunctionInstances' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSolFunctionInstances where
  rnf ListSolFunctionInstances' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListSolFunctionInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSolFunctionInstances where
  toPath = Prelude.const "/sol/vnflcm/v1/vnf_instances"

instance Data.ToQuery ListSolFunctionInstances where
  toQuery ListSolFunctionInstances' {..} =
    Prelude.mconcat
      [ "max_results" Data.=: maxResults,
        "nextpage_opaque_marker" Data.=: nextToken
      ]

-- | /See:/ 'newListSolFunctionInstancesResponse' smart constructor.
data ListSolFunctionInstancesResponse = ListSolFunctionInstancesResponse'
  { -- | Network function instances.
    functionInstances :: Prelude.Maybe [ListSolFunctionInstanceInfo],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolFunctionInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionInstances', 'listSolFunctionInstancesResponse_functionInstances' - Network function instances.
--
-- 'nextToken', 'listSolFunctionInstancesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'listSolFunctionInstancesResponse_httpStatus' - The response's http status code.
newListSolFunctionInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSolFunctionInstancesResponse
newListSolFunctionInstancesResponse pHttpStatus_ =
  ListSolFunctionInstancesResponse'
    { functionInstances =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Network function instances.
listSolFunctionInstancesResponse_functionInstances :: Lens.Lens' ListSolFunctionInstancesResponse (Prelude.Maybe [ListSolFunctionInstanceInfo])
listSolFunctionInstancesResponse_functionInstances = Lens.lens (\ListSolFunctionInstancesResponse' {functionInstances} -> functionInstances) (\s@ListSolFunctionInstancesResponse' {} a -> s {functionInstances = a} :: ListSolFunctionInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listSolFunctionInstancesResponse_nextToken :: Lens.Lens' ListSolFunctionInstancesResponse (Prelude.Maybe Prelude.Text)
listSolFunctionInstancesResponse_nextToken = Lens.lens (\ListSolFunctionInstancesResponse' {nextToken} -> nextToken) (\s@ListSolFunctionInstancesResponse' {} a -> s {nextToken = a} :: ListSolFunctionInstancesResponse)

-- | The response's http status code.
listSolFunctionInstancesResponse_httpStatus :: Lens.Lens' ListSolFunctionInstancesResponse Prelude.Int
listSolFunctionInstancesResponse_httpStatus = Lens.lens (\ListSolFunctionInstancesResponse' {httpStatus} -> httpStatus) (\s@ListSolFunctionInstancesResponse' {} a -> s {httpStatus = a} :: ListSolFunctionInstancesResponse)

instance
  Prelude.NFData
    ListSolFunctionInstancesResponse
  where
  rnf ListSolFunctionInstancesResponse' {..} =
    Prelude.rnf functionInstances
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
