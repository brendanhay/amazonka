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
-- Module      : Amazonka.Detective.ListMembers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of member accounts for a behavior graph.
--
-- For invited accounts, the results do not include member accounts that
-- were removed from the behavior graph.
--
-- For the organization behavior graph, the results do not include
-- organization accounts that the Detective administrator account has not
-- enabled as member accounts.
module Amazonka.Detective.ListMembers
  ( -- * Creating a Request
    ListMembers (..),
    newListMembers,

    -- * Request Lenses
    listMembers_maxResults,
    listMembers_nextToken,
    listMembers_graphArn,

    -- * Destructuring the Response
    ListMembersResponse (..),
    newListMembersResponse,

    -- * Response Lenses
    listMembersResponse_memberDetails,
    listMembersResponse_nextToken,
    listMembersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Detective.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMembers' smart constructor.
data ListMembers = ListMembers'
  { -- | The maximum number of member accounts to include in the response. The
    -- total must be less than the overall limit on the number of results to
    -- return, which is currently 200.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | For requests to retrieve the next page of member account results, the
    -- pagination token that was returned with the previous page of results.
    -- The initial request does not include a pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the behavior graph for which to retrieve the list of member
    -- accounts.
    graphArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listMembers_maxResults' - The maximum number of member accounts to include in the response. The
-- total must be less than the overall limit on the number of results to
-- return, which is currently 200.
--
-- 'nextToken', 'listMembers_nextToken' - For requests to retrieve the next page of member account results, the
-- pagination token that was returned with the previous page of results.
-- The initial request does not include a pagination token.
--
-- 'graphArn', 'listMembers_graphArn' - The ARN of the behavior graph for which to retrieve the list of member
-- accounts.
newListMembers ::
  -- | 'graphArn'
  Prelude.Text ->
  ListMembers
newListMembers pGraphArn_ =
  ListMembers'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      graphArn = pGraphArn_
    }

-- | The maximum number of member accounts to include in the response. The
-- total must be less than the overall limit on the number of results to
-- return, which is currently 200.
listMembers_maxResults :: Lens.Lens' ListMembers (Prelude.Maybe Prelude.Natural)
listMembers_maxResults = Lens.lens (\ListMembers' {maxResults} -> maxResults) (\s@ListMembers' {} a -> s {maxResults = a} :: ListMembers)

-- | For requests to retrieve the next page of member account results, the
-- pagination token that was returned with the previous page of results.
-- The initial request does not include a pagination token.
listMembers_nextToken :: Lens.Lens' ListMembers (Prelude.Maybe Prelude.Text)
listMembers_nextToken = Lens.lens (\ListMembers' {nextToken} -> nextToken) (\s@ListMembers' {} a -> s {nextToken = a} :: ListMembers)

-- | The ARN of the behavior graph for which to retrieve the list of member
-- accounts.
listMembers_graphArn :: Lens.Lens' ListMembers Prelude.Text
listMembers_graphArn = Lens.lens (\ListMembers' {graphArn} -> graphArn) (\s@ListMembers' {} a -> s {graphArn = a} :: ListMembers)

instance Core.AWSRequest ListMembers where
  type AWSResponse ListMembers = ListMembersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMembersResponse'
            Prelude.<$> (x Data..?> "MemberDetails" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMembers where
  hashWithSalt _salt ListMembers' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` graphArn

instance Prelude.NFData ListMembers where
  rnf ListMembers' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf graphArn

instance Data.ToHeaders ListMembers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMembers where
  toJSON ListMembers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("GraphArn" Data..= graphArn)
          ]
      )

instance Data.ToPath ListMembers where
  toPath = Prelude.const "/graph/members/list"

instance Data.ToQuery ListMembers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMembersResponse' smart constructor.
data ListMembersResponse = ListMembersResponse'
  { -- | The list of member accounts in the behavior graph.
    --
    -- For invited accounts, the results include member accounts that did not
    -- pass verification and member accounts that have not yet accepted the
    -- invitation to the behavior graph. The results do not include member
    -- accounts that were removed from the behavior graph.
    --
    -- For the organization behavior graph, the results do not include
    -- organization accounts that the Detective administrator account has not
    -- enabled as member accounts.
    memberDetails :: Prelude.Maybe [MemberDetail],
    -- | If there are more member accounts remaining in the results, then use
    -- this pagination token to request the next page of member accounts.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberDetails', 'listMembersResponse_memberDetails' - The list of member accounts in the behavior graph.
--
-- For invited accounts, the results include member accounts that did not
-- pass verification and member accounts that have not yet accepted the
-- invitation to the behavior graph. The results do not include member
-- accounts that were removed from the behavior graph.
--
-- For the organization behavior graph, the results do not include
-- organization accounts that the Detective administrator account has not
-- enabled as member accounts.
--
-- 'nextToken', 'listMembersResponse_nextToken' - If there are more member accounts remaining in the results, then use
-- this pagination token to request the next page of member accounts.
--
-- 'httpStatus', 'listMembersResponse_httpStatus' - The response's http status code.
newListMembersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMembersResponse
newListMembersResponse pHttpStatus_ =
  ListMembersResponse'
    { memberDetails =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of member accounts in the behavior graph.
--
-- For invited accounts, the results include member accounts that did not
-- pass verification and member accounts that have not yet accepted the
-- invitation to the behavior graph. The results do not include member
-- accounts that were removed from the behavior graph.
--
-- For the organization behavior graph, the results do not include
-- organization accounts that the Detective administrator account has not
-- enabled as member accounts.
listMembersResponse_memberDetails :: Lens.Lens' ListMembersResponse (Prelude.Maybe [MemberDetail])
listMembersResponse_memberDetails = Lens.lens (\ListMembersResponse' {memberDetails} -> memberDetails) (\s@ListMembersResponse' {} a -> s {memberDetails = a} :: ListMembersResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are more member accounts remaining in the results, then use
-- this pagination token to request the next page of member accounts.
listMembersResponse_nextToken :: Lens.Lens' ListMembersResponse (Prelude.Maybe Prelude.Text)
listMembersResponse_nextToken = Lens.lens (\ListMembersResponse' {nextToken} -> nextToken) (\s@ListMembersResponse' {} a -> s {nextToken = a} :: ListMembersResponse)

-- | The response's http status code.
listMembersResponse_httpStatus :: Lens.Lens' ListMembersResponse Prelude.Int
listMembersResponse_httpStatus = Lens.lens (\ListMembersResponse' {httpStatus} -> httpStatus) (\s@ListMembersResponse' {} a -> s {httpStatus = a} :: ListMembersResponse)

instance Prelude.NFData ListMembersResponse where
  rnf ListMembersResponse' {..} =
    Prelude.rnf memberDetails
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
