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
-- Module      : Amazonka.FMS.ListMemberAccounts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a @MemberAccounts@ object that lists the member accounts in the
-- administrator\'s Amazon Web Services organization.
--
-- The @ListMemberAccounts@ must be submitted by the account that is set as
-- the Firewall Manager administrator.
--
-- This operation returns paginated results.
module Amazonka.FMS.ListMemberAccounts
  ( -- * Creating a Request
    ListMemberAccounts (..),
    newListMemberAccounts,

    -- * Request Lenses
    listMemberAccounts_maxResults,
    listMemberAccounts_nextToken,

    -- * Destructuring the Response
    ListMemberAccountsResponse (..),
    newListMemberAccountsResponse,

    -- * Response Lenses
    listMemberAccountsResponse_memberAccounts,
    listMemberAccountsResponse_nextToken,
    listMemberAccountsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMemberAccounts' smart constructor.
data ListMemberAccounts = ListMemberAccounts'
  { -- | Specifies the number of member account IDs that you want Firewall
    -- Manager to return for this request. If you have more IDs than the number
    -- that you specify for @MaxResults@, the response includes a @NextToken@
    -- value that you can use to get another batch of member account IDs.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If you specify a value for @MaxResults@ and you have more account IDs
    -- than the number that you specify for @MaxResults@, Firewall Manager
    -- returns a @NextToken@ value in the response that allows you to list
    -- another group of IDs. For the second and subsequent
    -- @ListMemberAccountsRequest@ requests, specify the value of @NextToken@
    -- from the previous response to get information about another batch of
    -- member account IDs.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMemberAccounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listMemberAccounts_maxResults' - Specifies the number of member account IDs that you want Firewall
-- Manager to return for this request. If you have more IDs than the number
-- that you specify for @MaxResults@, the response includes a @NextToken@
-- value that you can use to get another batch of member account IDs.
--
-- 'nextToken', 'listMemberAccounts_nextToken' - If you specify a value for @MaxResults@ and you have more account IDs
-- than the number that you specify for @MaxResults@, Firewall Manager
-- returns a @NextToken@ value in the response that allows you to list
-- another group of IDs. For the second and subsequent
-- @ListMemberAccountsRequest@ requests, specify the value of @NextToken@
-- from the previous response to get information about another batch of
-- member account IDs.
newListMemberAccounts ::
  ListMemberAccounts
newListMemberAccounts =
  ListMemberAccounts'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Specifies the number of member account IDs that you want Firewall
-- Manager to return for this request. If you have more IDs than the number
-- that you specify for @MaxResults@, the response includes a @NextToken@
-- value that you can use to get another batch of member account IDs.
listMemberAccounts_maxResults :: Lens.Lens' ListMemberAccounts (Prelude.Maybe Prelude.Natural)
listMemberAccounts_maxResults = Lens.lens (\ListMemberAccounts' {maxResults} -> maxResults) (\s@ListMemberAccounts' {} a -> s {maxResults = a} :: ListMemberAccounts)

-- | If you specify a value for @MaxResults@ and you have more account IDs
-- than the number that you specify for @MaxResults@, Firewall Manager
-- returns a @NextToken@ value in the response that allows you to list
-- another group of IDs. For the second and subsequent
-- @ListMemberAccountsRequest@ requests, specify the value of @NextToken@
-- from the previous response to get information about another batch of
-- member account IDs.
listMemberAccounts_nextToken :: Lens.Lens' ListMemberAccounts (Prelude.Maybe Prelude.Text)
listMemberAccounts_nextToken = Lens.lens (\ListMemberAccounts' {nextToken} -> nextToken) (\s@ListMemberAccounts' {} a -> s {nextToken = a} :: ListMemberAccounts)

instance Core.AWSPager ListMemberAccounts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMemberAccountsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listMemberAccountsResponse_memberAccounts
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listMemberAccounts_nextToken
              Lens..~ rs
              Lens.^? listMemberAccountsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListMemberAccounts where
  type
    AWSResponse ListMemberAccounts =
      ListMemberAccountsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMemberAccountsResponse'
            Prelude.<$> (x Data..?> "MemberAccounts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMemberAccounts where
  hashWithSalt _salt ListMemberAccounts' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListMemberAccounts where
  rnf ListMemberAccounts' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListMemberAccounts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.ListMemberAccounts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMemberAccounts where
  toJSON ListMemberAccounts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListMemberAccounts where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMemberAccounts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMemberAccountsResponse' smart constructor.
data ListMemberAccountsResponse = ListMemberAccountsResponse'
  { -- | An array of account IDs.
    memberAccounts :: Prelude.Maybe [Prelude.Text],
    -- | If you have more member account IDs than the number that you specified
    -- for @MaxResults@ in the request, the response includes a @NextToken@
    -- value. To list more IDs, submit another @ListMemberAccounts@ request,
    -- and specify the @NextToken@ value from the response in the @NextToken@
    -- value in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMemberAccountsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberAccounts', 'listMemberAccountsResponse_memberAccounts' - An array of account IDs.
--
-- 'nextToken', 'listMemberAccountsResponse_nextToken' - If you have more member account IDs than the number that you specified
-- for @MaxResults@ in the request, the response includes a @NextToken@
-- value. To list more IDs, submit another @ListMemberAccounts@ request,
-- and specify the @NextToken@ value from the response in the @NextToken@
-- value in the next request.
--
-- 'httpStatus', 'listMemberAccountsResponse_httpStatus' - The response's http status code.
newListMemberAccountsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMemberAccountsResponse
newListMemberAccountsResponse pHttpStatus_ =
  ListMemberAccountsResponse'
    { memberAccounts =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of account IDs.
listMemberAccountsResponse_memberAccounts :: Lens.Lens' ListMemberAccountsResponse (Prelude.Maybe [Prelude.Text])
listMemberAccountsResponse_memberAccounts = Lens.lens (\ListMemberAccountsResponse' {memberAccounts} -> memberAccounts) (\s@ListMemberAccountsResponse' {} a -> s {memberAccounts = a} :: ListMemberAccountsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If you have more member account IDs than the number that you specified
-- for @MaxResults@ in the request, the response includes a @NextToken@
-- value. To list more IDs, submit another @ListMemberAccounts@ request,
-- and specify the @NextToken@ value from the response in the @NextToken@
-- value in the next request.
listMemberAccountsResponse_nextToken :: Lens.Lens' ListMemberAccountsResponse (Prelude.Maybe Prelude.Text)
listMemberAccountsResponse_nextToken = Lens.lens (\ListMemberAccountsResponse' {nextToken} -> nextToken) (\s@ListMemberAccountsResponse' {} a -> s {nextToken = a} :: ListMemberAccountsResponse)

-- | The response's http status code.
listMemberAccountsResponse_httpStatus :: Lens.Lens' ListMemberAccountsResponse Prelude.Int
listMemberAccountsResponse_httpStatus = Lens.lens (\ListMemberAccountsResponse' {httpStatus} -> httpStatus) (\s@ListMemberAccountsResponse' {} a -> s {httpStatus = a} :: ListMemberAccountsResponse)

instance Prelude.NFData ListMemberAccountsResponse where
  rnf ListMemberAccountsResponse' {..} =
    Prelude.rnf memberAccounts `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
