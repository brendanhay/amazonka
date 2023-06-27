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
-- Module      : Amazonka.TNB.ListSolNetworkInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your network instances.
--
-- A network instance is a single network created in Amazon Web Services
-- TNB that can be deployed and on which life-cycle operations (like
-- terminate, update, and delete) can be performed.
--
-- This operation returns paginated results.
module Amazonka.TNB.ListSolNetworkInstances
  ( -- * Creating a Request
    ListSolNetworkInstances (..),
    newListSolNetworkInstances,

    -- * Request Lenses
    listSolNetworkInstances_maxResults,
    listSolNetworkInstances_nextToken,

    -- * Destructuring the Response
    ListSolNetworkInstancesResponse (..),
    newListSolNetworkInstancesResponse,

    -- * Response Lenses
    listSolNetworkInstancesResponse_networkInstances,
    listSolNetworkInstancesResponse_nextToken,
    listSolNetworkInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newListSolNetworkInstances' smart constructor.
data ListSolNetworkInstances = ListSolNetworkInstances'
  { -- | The maximum number of results to include in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolNetworkInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSolNetworkInstances_maxResults' - The maximum number of results to include in the response.
--
-- 'nextToken', 'listSolNetworkInstances_nextToken' - The token for the next page of results.
newListSolNetworkInstances ::
  ListSolNetworkInstances
newListSolNetworkInstances =
  ListSolNetworkInstances'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to include in the response.
listSolNetworkInstances_maxResults :: Lens.Lens' ListSolNetworkInstances (Prelude.Maybe Prelude.Natural)
listSolNetworkInstances_maxResults = Lens.lens (\ListSolNetworkInstances' {maxResults} -> maxResults) (\s@ListSolNetworkInstances' {} a -> s {maxResults = a} :: ListSolNetworkInstances)

-- | The token for the next page of results.
listSolNetworkInstances_nextToken :: Lens.Lens' ListSolNetworkInstances (Prelude.Maybe Prelude.Text)
listSolNetworkInstances_nextToken = Lens.lens (\ListSolNetworkInstances' {nextToken} -> nextToken) (\s@ListSolNetworkInstances' {} a -> s {nextToken = a} :: ListSolNetworkInstances)

instance Core.AWSPager ListSolNetworkInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSolNetworkInstancesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSolNetworkInstancesResponse_networkInstances
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSolNetworkInstances_nextToken
          Lens..~ rs
          Lens.^? listSolNetworkInstancesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSolNetworkInstances where
  type
    AWSResponse ListSolNetworkInstances =
      ListSolNetworkInstancesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSolNetworkInstancesResponse'
            Prelude.<$> ( x
                            Data..?> "networkInstances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSolNetworkInstances where
  hashWithSalt _salt ListSolNetworkInstances' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSolNetworkInstances where
  rnf ListSolNetworkInstances' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListSolNetworkInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSolNetworkInstances where
  toPath = Prelude.const "/sol/nslcm/v1/ns_instances"

instance Data.ToQuery ListSolNetworkInstances where
  toQuery ListSolNetworkInstances' {..} =
    Prelude.mconcat
      [ "max_results" Data.=: maxResults,
        "nextpage_opaque_marker" Data.=: nextToken
      ]

-- | /See:/ 'newListSolNetworkInstancesResponse' smart constructor.
data ListSolNetworkInstancesResponse = ListSolNetworkInstancesResponse'
  { -- | Lists network instances.
    networkInstances :: Prelude.Maybe [ListSolNetworkInstanceInfo],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSolNetworkInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInstances', 'listSolNetworkInstancesResponse_networkInstances' - Lists network instances.
--
-- 'nextToken', 'listSolNetworkInstancesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'listSolNetworkInstancesResponse_httpStatus' - The response's http status code.
newListSolNetworkInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSolNetworkInstancesResponse
newListSolNetworkInstancesResponse pHttpStatus_ =
  ListSolNetworkInstancesResponse'
    { networkInstances =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Lists network instances.
listSolNetworkInstancesResponse_networkInstances :: Lens.Lens' ListSolNetworkInstancesResponse (Prelude.Maybe [ListSolNetworkInstanceInfo])
listSolNetworkInstancesResponse_networkInstances = Lens.lens (\ListSolNetworkInstancesResponse' {networkInstances} -> networkInstances) (\s@ListSolNetworkInstancesResponse' {} a -> s {networkInstances = a} :: ListSolNetworkInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listSolNetworkInstancesResponse_nextToken :: Lens.Lens' ListSolNetworkInstancesResponse (Prelude.Maybe Prelude.Text)
listSolNetworkInstancesResponse_nextToken = Lens.lens (\ListSolNetworkInstancesResponse' {nextToken} -> nextToken) (\s@ListSolNetworkInstancesResponse' {} a -> s {nextToken = a} :: ListSolNetworkInstancesResponse)

-- | The response's http status code.
listSolNetworkInstancesResponse_httpStatus :: Lens.Lens' ListSolNetworkInstancesResponse Prelude.Int
listSolNetworkInstancesResponse_httpStatus = Lens.lens (\ListSolNetworkInstancesResponse' {httpStatus} -> httpStatus) (\s@ListSolNetworkInstancesResponse' {} a -> s {httpStatus = a} :: ListSolNetworkInstancesResponse)

instance
  Prelude.NFData
    ListSolNetworkInstancesResponse
  where
  rnf ListSolNetworkInstancesResponse' {..} =
    Prelude.rnf networkInstances
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
