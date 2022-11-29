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
-- Module      : Amazonka.Organizations.ListAccounts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the accounts in the organization. To request only the accounts
-- in a specified root or organizational unit (OU), use the
-- ListAccountsForParent operation instead.
--
-- Always check the @NextToken@ response parameter for a @null@ value when
-- calling a @List*@ operation. These operations can occasionally return an
-- empty set of results even when there are more results available. The
-- @NextToken@ response parameter value is @null@ /only/ when there are no
-- more results to display.
--
-- This operation can be called only from the organization\'s management
-- account or by a member account that is a delegated administrator for an
-- Amazon Web Services service.
--
-- This operation returns paginated results.
module Amazonka.Organizations.ListAccounts
  ( -- * Creating a Request
    ListAccounts (..),
    newListAccounts,

    -- * Request Lenses
    listAccounts_nextToken,
    listAccounts_maxResults,

    -- * Destructuring the Response
    ListAccountsResponse (..),
    newListAccountsResponse,

    -- * Response Lenses
    listAccountsResponse_nextToken,
    listAccountsResponse_accounts,
    listAccountsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAccounts' smart constructor.
data ListAccounts = ListAccounts'
  { -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- of the previous call\'s @NextToken@ response to indicate where the
    -- output should continue from.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The total number of results that you want included on each page of the
    -- response. If you do not include this parameter, it defaults to a value
    -- that is specific to the operation. If additional items exist beyond the
    -- maximum you specify, the @NextToken@ response element is present and has
    -- a value (is not null). Include that value as the @NextToken@ request
    -- parameter in the next call to the operation to get the next part of the
    -- results. Note that Organizations might return fewer results than the
    -- maximum even when there are more results available. You should check
    -- @NextToken@ after every operation to ensure that you receive all of the
    -- results.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAccounts_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
--
-- 'maxResults', 'listAccounts_maxResults' - The total number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- that is specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (is not null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that Organizations might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
newListAccounts ::
  ListAccounts
newListAccounts =
  ListAccounts'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listAccounts_nextToken :: Lens.Lens' ListAccounts (Prelude.Maybe Prelude.Text)
listAccounts_nextToken = Lens.lens (\ListAccounts' {nextToken} -> nextToken) (\s@ListAccounts' {} a -> s {nextToken = a} :: ListAccounts)

-- | The total number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- that is specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (is not null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that Organizations might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
listAccounts_maxResults :: Lens.Lens' ListAccounts (Prelude.Maybe Prelude.Natural)
listAccounts_maxResults = Lens.lens (\ListAccounts' {maxResults} -> maxResults) (\s@ListAccounts' {} a -> s {maxResults = a} :: ListAccounts)

instance Core.AWSPager ListAccounts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAccountsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAccountsResponse_accounts Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAccounts_nextToken
          Lens..~ rs
          Lens.^? listAccountsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListAccounts where
  type AWSResponse ListAccounts = ListAccountsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAccountsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Accounts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAccounts where
  hashWithSalt _salt ListAccounts' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListAccounts where
  rnf ListAccounts' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListAccounts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.ListAccounts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListAccounts where
  toJSON ListAccounts' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListAccounts where
  toPath = Prelude.const "/"

instance Core.ToQuery ListAccounts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAccountsResponse' smart constructor.
data ListAccountsResponse = ListAccountsResponse'
  { -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of objects in the organization.
    accounts :: Prelude.Maybe [Account],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccountsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAccountsResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'accounts', 'listAccountsResponse_accounts' - A list of objects in the organization.
--
-- 'httpStatus', 'listAccountsResponse_httpStatus' - The response's http status code.
newListAccountsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAccountsResponse
newListAccountsResponse pHttpStatus_ =
  ListAccountsResponse'
    { nextToken = Prelude.Nothing,
      accounts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listAccountsResponse_nextToken :: Lens.Lens' ListAccountsResponse (Prelude.Maybe Prelude.Text)
listAccountsResponse_nextToken = Lens.lens (\ListAccountsResponse' {nextToken} -> nextToken) (\s@ListAccountsResponse' {} a -> s {nextToken = a} :: ListAccountsResponse)

-- | A list of objects in the organization.
listAccountsResponse_accounts :: Lens.Lens' ListAccountsResponse (Prelude.Maybe [Account])
listAccountsResponse_accounts = Lens.lens (\ListAccountsResponse' {accounts} -> accounts) (\s@ListAccountsResponse' {} a -> s {accounts = a} :: ListAccountsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAccountsResponse_httpStatus :: Lens.Lens' ListAccountsResponse Prelude.Int
listAccountsResponse_httpStatus = Lens.lens (\ListAccountsResponse' {httpStatus} -> httpStatus) (\s@ListAccountsResponse' {} a -> s {httpStatus = a} :: ListAccountsResponse)

instance Prelude.NFData ListAccountsResponse where
  rnf ListAccountsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf accounts
      `Prelude.seq` Prelude.rnf httpStatus
