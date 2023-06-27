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
-- Module      : Amazonka.CleanRooms.ListProtectedQueries
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists protected queries, sorted by the most recent query.
--
-- This operation returns paginated results.
module Amazonka.CleanRooms.ListProtectedQueries
  ( -- * Creating a Request
    ListProtectedQueries (..),
    newListProtectedQueries,

    -- * Request Lenses
    listProtectedQueries_maxResults,
    listProtectedQueries_nextToken,
    listProtectedQueries_status,
    listProtectedQueries_membershipIdentifier,

    -- * Destructuring the Response
    ListProtectedQueriesResponse (..),
    newListProtectedQueriesResponse,

    -- * Response Lenses
    listProtectedQueriesResponse_nextToken,
    listProtectedQueriesResponse_httpStatus,
    listProtectedQueriesResponse_protectedQueries,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListProtectedQueries' smart constructor.
data ListProtectedQueries = ListProtectedQueries'
  { -- | The maximum size of the results that is returned per call. Service
    -- chooses a default if it has not been set. Service can return a nextToken
    -- even if the maximum results has not been met.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token value retrieved from a previous call to access the next page
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter on the status of the protected query.
    status :: Prelude.Maybe ProtectedQueryStatus,
    -- | The identifier for the membership in the collaboration.
    membershipIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProtectedQueries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listProtectedQueries_maxResults' - The maximum size of the results that is returned per call. Service
-- chooses a default if it has not been set. Service can return a nextToken
-- even if the maximum results has not been met.
--
-- 'nextToken', 'listProtectedQueries_nextToken' - The token value retrieved from a previous call to access the next page
-- of results.
--
-- 'status', 'listProtectedQueries_status' - A filter on the status of the protected query.
--
-- 'membershipIdentifier', 'listProtectedQueries_membershipIdentifier' - The identifier for the membership in the collaboration.
newListProtectedQueries ::
  -- | 'membershipIdentifier'
  Prelude.Text ->
  ListProtectedQueries
newListProtectedQueries pMembershipIdentifier_ =
  ListProtectedQueries'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      membershipIdentifier = pMembershipIdentifier_
    }

-- | The maximum size of the results that is returned per call. Service
-- chooses a default if it has not been set. Service can return a nextToken
-- even if the maximum results has not been met.
listProtectedQueries_maxResults :: Lens.Lens' ListProtectedQueries (Prelude.Maybe Prelude.Natural)
listProtectedQueries_maxResults = Lens.lens (\ListProtectedQueries' {maxResults} -> maxResults) (\s@ListProtectedQueries' {} a -> s {maxResults = a} :: ListProtectedQueries)

-- | The token value retrieved from a previous call to access the next page
-- of results.
listProtectedQueries_nextToken :: Lens.Lens' ListProtectedQueries (Prelude.Maybe Prelude.Text)
listProtectedQueries_nextToken = Lens.lens (\ListProtectedQueries' {nextToken} -> nextToken) (\s@ListProtectedQueries' {} a -> s {nextToken = a} :: ListProtectedQueries)

-- | A filter on the status of the protected query.
listProtectedQueries_status :: Lens.Lens' ListProtectedQueries (Prelude.Maybe ProtectedQueryStatus)
listProtectedQueries_status = Lens.lens (\ListProtectedQueries' {status} -> status) (\s@ListProtectedQueries' {} a -> s {status = a} :: ListProtectedQueries)

-- | The identifier for the membership in the collaboration.
listProtectedQueries_membershipIdentifier :: Lens.Lens' ListProtectedQueries Prelude.Text
listProtectedQueries_membershipIdentifier = Lens.lens (\ListProtectedQueries' {membershipIdentifier} -> membershipIdentifier) (\s@ListProtectedQueries' {} a -> s {membershipIdentifier = a} :: ListProtectedQueries)

instance Core.AWSPager ListProtectedQueries where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProtectedQueriesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listProtectedQueriesResponse_protectedQueries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listProtectedQueries_nextToken
          Lens..~ rs
          Lens.^? listProtectedQueriesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListProtectedQueries where
  type
    AWSResponse ListProtectedQueries =
      ListProtectedQueriesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProtectedQueriesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "protectedQueries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListProtectedQueries where
  hashWithSalt _salt ListProtectedQueries' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` membershipIdentifier

instance Prelude.NFData ListProtectedQueries where
  rnf ListProtectedQueries' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf membershipIdentifier

instance Data.ToHeaders ListProtectedQueries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListProtectedQueries where
  toPath ListProtectedQueries' {..} =
    Prelude.mconcat
      [ "/memberships/",
        Data.toBS membershipIdentifier,
        "/protectedQueries"
      ]

instance Data.ToQuery ListProtectedQueries where
  toQuery ListProtectedQueries' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "status" Data.=: status
      ]

-- | /See:/ 'newListProtectedQueriesResponse' smart constructor.
data ListProtectedQueriesResponse = ListProtectedQueriesResponse'
  { -- | The token value retrieved from a previous call to access the next page
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of protected queries.
    protectedQueries :: [ProtectedQuerySummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProtectedQueriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProtectedQueriesResponse_nextToken' - The token value retrieved from a previous call to access the next page
-- of results.
--
-- 'httpStatus', 'listProtectedQueriesResponse_httpStatus' - The response's http status code.
--
-- 'protectedQueries', 'listProtectedQueriesResponse_protectedQueries' - A list of protected queries.
newListProtectedQueriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProtectedQueriesResponse
newListProtectedQueriesResponse pHttpStatus_ =
  ListProtectedQueriesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      protectedQueries = Prelude.mempty
    }

-- | The token value retrieved from a previous call to access the next page
-- of results.
listProtectedQueriesResponse_nextToken :: Lens.Lens' ListProtectedQueriesResponse (Prelude.Maybe Prelude.Text)
listProtectedQueriesResponse_nextToken = Lens.lens (\ListProtectedQueriesResponse' {nextToken} -> nextToken) (\s@ListProtectedQueriesResponse' {} a -> s {nextToken = a} :: ListProtectedQueriesResponse)

-- | The response's http status code.
listProtectedQueriesResponse_httpStatus :: Lens.Lens' ListProtectedQueriesResponse Prelude.Int
listProtectedQueriesResponse_httpStatus = Lens.lens (\ListProtectedQueriesResponse' {httpStatus} -> httpStatus) (\s@ListProtectedQueriesResponse' {} a -> s {httpStatus = a} :: ListProtectedQueriesResponse)

-- | A list of protected queries.
listProtectedQueriesResponse_protectedQueries :: Lens.Lens' ListProtectedQueriesResponse [ProtectedQuerySummary]
listProtectedQueriesResponse_protectedQueries = Lens.lens (\ListProtectedQueriesResponse' {protectedQueries} -> protectedQueries) (\s@ListProtectedQueriesResponse' {} a -> s {protectedQueries = a} :: ListProtectedQueriesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListProtectedQueriesResponse where
  rnf ListProtectedQueriesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf protectedQueries
