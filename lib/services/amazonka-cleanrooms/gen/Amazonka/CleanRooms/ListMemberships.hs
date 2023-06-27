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
-- Module      : Amazonka.CleanRooms.ListMemberships
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all memberships resources within the caller\'s account.
--
-- This operation returns paginated results.
module Amazonka.CleanRooms.ListMemberships
  ( -- * Creating a Request
    ListMemberships (..),
    newListMemberships,

    -- * Request Lenses
    listMemberships_maxResults,
    listMemberships_nextToken,
    listMemberships_status,

    -- * Destructuring the Response
    ListMembershipsResponse (..),
    newListMembershipsResponse,

    -- * Response Lenses
    listMembershipsResponse_nextToken,
    listMembershipsResponse_httpStatus,
    listMembershipsResponse_membershipSummaries,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMemberships' smart constructor.
data ListMemberships = ListMemberships'
  { -- | The maximum size of the results that is returned per call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token value retrieved from a previous call to access the next page
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter which will return only memberships in the specified status.
    status :: Prelude.Maybe MembershipStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMemberships' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listMemberships_maxResults' - The maximum size of the results that is returned per call.
--
-- 'nextToken', 'listMemberships_nextToken' - The token value retrieved from a previous call to access the next page
-- of results.
--
-- 'status', 'listMemberships_status' - A filter which will return only memberships in the specified status.
newListMemberships ::
  ListMemberships
newListMemberships =
  ListMemberships'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The maximum size of the results that is returned per call.
listMemberships_maxResults :: Lens.Lens' ListMemberships (Prelude.Maybe Prelude.Natural)
listMemberships_maxResults = Lens.lens (\ListMemberships' {maxResults} -> maxResults) (\s@ListMemberships' {} a -> s {maxResults = a} :: ListMemberships)

-- | The token value retrieved from a previous call to access the next page
-- of results.
listMemberships_nextToken :: Lens.Lens' ListMemberships (Prelude.Maybe Prelude.Text)
listMemberships_nextToken = Lens.lens (\ListMemberships' {nextToken} -> nextToken) (\s@ListMemberships' {} a -> s {nextToken = a} :: ListMemberships)

-- | A filter which will return only memberships in the specified status.
listMemberships_status :: Lens.Lens' ListMemberships (Prelude.Maybe MembershipStatus)
listMemberships_status = Lens.lens (\ListMemberships' {status} -> status) (\s@ListMemberships' {} a -> s {status = a} :: ListMemberships)

instance Core.AWSPager ListMemberships where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMembershipsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listMembershipsResponse_membershipSummaries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listMemberships_nextToken
          Lens..~ rs
          Lens.^? listMembershipsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListMemberships where
  type
    AWSResponse ListMemberships =
      ListMembershipsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMembershipsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "membershipSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListMemberships where
  hashWithSalt _salt ListMemberships' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status

instance Prelude.NFData ListMemberships where
  rnf ListMemberships' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status

instance Data.ToHeaders ListMemberships where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListMemberships where
  toPath = Prelude.const "/memberships"

instance Data.ToQuery ListMemberships where
  toQuery ListMemberships' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "status" Data.=: status
      ]

-- | /See:/ 'newListMembershipsResponse' smart constructor.
data ListMembershipsResponse = ListMembershipsResponse'
  { -- | The token value retrieved from a previous call to access the next page
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of memberships returned from the ListMemberships operation.
    membershipSummaries :: [MembershipSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMembershipsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMembershipsResponse_nextToken' - The token value retrieved from a previous call to access the next page
-- of results.
--
-- 'httpStatus', 'listMembershipsResponse_httpStatus' - The response's http status code.
--
-- 'membershipSummaries', 'listMembershipsResponse_membershipSummaries' - The list of memberships returned from the ListMemberships operation.
newListMembershipsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMembershipsResponse
newListMembershipsResponse pHttpStatus_ =
  ListMembershipsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      membershipSummaries = Prelude.mempty
    }

-- | The token value retrieved from a previous call to access the next page
-- of results.
listMembershipsResponse_nextToken :: Lens.Lens' ListMembershipsResponse (Prelude.Maybe Prelude.Text)
listMembershipsResponse_nextToken = Lens.lens (\ListMembershipsResponse' {nextToken} -> nextToken) (\s@ListMembershipsResponse' {} a -> s {nextToken = a} :: ListMembershipsResponse)

-- | The response's http status code.
listMembershipsResponse_httpStatus :: Lens.Lens' ListMembershipsResponse Prelude.Int
listMembershipsResponse_httpStatus = Lens.lens (\ListMembershipsResponse' {httpStatus} -> httpStatus) (\s@ListMembershipsResponse' {} a -> s {httpStatus = a} :: ListMembershipsResponse)

-- | The list of memberships returned from the ListMemberships operation.
listMembershipsResponse_membershipSummaries :: Lens.Lens' ListMembershipsResponse [MembershipSummary]
listMembershipsResponse_membershipSummaries = Lens.lens (\ListMembershipsResponse' {membershipSummaries} -> membershipSummaries) (\s@ListMembershipsResponse' {} a -> s {membershipSummaries = a} :: ListMembershipsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListMembershipsResponse where
  rnf ListMembershipsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf membershipSummaries
