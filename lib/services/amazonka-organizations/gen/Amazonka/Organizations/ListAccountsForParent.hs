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
-- Module      : Amazonka.Organizations.ListAccountsForParent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the accounts in an organization that are contained by the
-- specified target root or organizational unit (OU). If you specify the
-- root, you get a list of all the accounts that aren\'t in any OU. If you
-- specify an OU, you get a list of all the accounts in only that OU and
-- not in any child OUs. To get a list of all accounts in the organization,
-- use the ListAccounts operation.
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
module Amazonka.Organizations.ListAccountsForParent
  ( -- * Creating a Request
    ListAccountsForParent (..),
    newListAccountsForParent,

    -- * Request Lenses
    listAccountsForParent_maxResults,
    listAccountsForParent_nextToken,
    listAccountsForParent_parentId,

    -- * Destructuring the Response
    ListAccountsForParentResponse (..),
    newListAccountsForParentResponse,

    -- * Response Lenses
    listAccountsForParentResponse_accounts,
    listAccountsForParentResponse_nextToken,
    listAccountsForParentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAccountsForParent' smart constructor.
data ListAccountsForParent = ListAccountsForParent'
  { -- | The total number of results that you want included on each page of the
    -- response. If you do not include this parameter, it defaults to a value
    -- that is specific to the operation. If additional items exist beyond the
    -- maximum you specify, the @NextToken@ response element is present and has
    -- a value (is not null). Include that value as the @NextToken@ request
    -- parameter in the next call to the operation to get the next part of the
    -- results. Note that Organizations might return fewer results than the
    -- maximum even when there are more results available. You should check
    -- @NextToken@ after every operation to ensure that you receive all of the
    -- results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- of the previous call\'s @NextToken@ response to indicate where the
    -- output should continue from.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier (ID) for the parent root or organization unit (OU)
    -- whose accounts you want to list.
    parentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccountsForParent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAccountsForParent_maxResults' - The total number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- that is specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (is not null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that Organizations might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
--
-- 'nextToken', 'listAccountsForParent_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
--
-- 'parentId', 'listAccountsForParent_parentId' - The unique identifier (ID) for the parent root or organization unit (OU)
-- whose accounts you want to list.
newListAccountsForParent ::
  -- | 'parentId'
  Prelude.Text ->
  ListAccountsForParent
newListAccountsForParent pParentId_ =
  ListAccountsForParent'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      parentId = pParentId_
    }

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
listAccountsForParent_maxResults :: Lens.Lens' ListAccountsForParent (Prelude.Maybe Prelude.Natural)
listAccountsForParent_maxResults = Lens.lens (\ListAccountsForParent' {maxResults} -> maxResults) (\s@ListAccountsForParent' {} a -> s {maxResults = a} :: ListAccountsForParent)

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listAccountsForParent_nextToken :: Lens.Lens' ListAccountsForParent (Prelude.Maybe Prelude.Text)
listAccountsForParent_nextToken = Lens.lens (\ListAccountsForParent' {nextToken} -> nextToken) (\s@ListAccountsForParent' {} a -> s {nextToken = a} :: ListAccountsForParent)

-- | The unique identifier (ID) for the parent root or organization unit (OU)
-- whose accounts you want to list.
listAccountsForParent_parentId :: Lens.Lens' ListAccountsForParent Prelude.Text
listAccountsForParent_parentId = Lens.lens (\ListAccountsForParent' {parentId} -> parentId) (\s@ListAccountsForParent' {} a -> s {parentId = a} :: ListAccountsForParent)

instance Core.AWSPager ListAccountsForParent where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAccountsForParentResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAccountsForParentResponse_accounts
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAccountsForParent_nextToken
          Lens..~ rs
          Lens.^? listAccountsForParentResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAccountsForParent where
  type
    AWSResponse ListAccountsForParent =
      ListAccountsForParentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAccountsForParentResponse'
            Prelude.<$> (x Data..?> "Accounts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAccountsForParent where
  hashWithSalt _salt ListAccountsForParent' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` parentId

instance Prelude.NFData ListAccountsForParent where
  rnf ListAccountsForParent' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf parentId

instance Data.ToHeaders ListAccountsForParent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.ListAccountsForParent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAccountsForParent where
  toJSON ListAccountsForParent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("ParentId" Data..= parentId)
          ]
      )

instance Data.ToPath ListAccountsForParent where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAccountsForParent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAccountsForParentResponse' smart constructor.
data ListAccountsForParentResponse = ListAccountsForParentResponse'
  { -- | A list of the accounts in the specified root or OU.
    accounts :: Prelude.Maybe [Account],
    -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccountsForParentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accounts', 'listAccountsForParentResponse_accounts' - A list of the accounts in the specified root or OU.
--
-- 'nextToken', 'listAccountsForParentResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'httpStatus', 'listAccountsForParentResponse_httpStatus' - The response's http status code.
newListAccountsForParentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAccountsForParentResponse
newListAccountsForParentResponse pHttpStatus_ =
  ListAccountsForParentResponse'
    { accounts =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the accounts in the specified root or OU.
listAccountsForParentResponse_accounts :: Lens.Lens' ListAccountsForParentResponse (Prelude.Maybe [Account])
listAccountsForParentResponse_accounts = Lens.lens (\ListAccountsForParentResponse' {accounts} -> accounts) (\s@ListAccountsForParentResponse' {} a -> s {accounts = a} :: ListAccountsForParentResponse) Prelude.. Lens.mapping Lens.coerced

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listAccountsForParentResponse_nextToken :: Lens.Lens' ListAccountsForParentResponse (Prelude.Maybe Prelude.Text)
listAccountsForParentResponse_nextToken = Lens.lens (\ListAccountsForParentResponse' {nextToken} -> nextToken) (\s@ListAccountsForParentResponse' {} a -> s {nextToken = a} :: ListAccountsForParentResponse)

-- | The response's http status code.
listAccountsForParentResponse_httpStatus :: Lens.Lens' ListAccountsForParentResponse Prelude.Int
listAccountsForParentResponse_httpStatus = Lens.lens (\ListAccountsForParentResponse' {httpStatus} -> httpStatus) (\s@ListAccountsForParentResponse' {} a -> s {httpStatus = a} :: ListAccountsForParentResponse)

instance Prelude.NFData ListAccountsForParentResponse where
  rnf ListAccountsForParentResponse' {..} =
    Prelude.rnf accounts
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
