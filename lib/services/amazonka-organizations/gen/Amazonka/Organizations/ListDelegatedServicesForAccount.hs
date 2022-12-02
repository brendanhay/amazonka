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
-- Module      : Amazonka.Organizations.ListDelegatedServicesForAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the Amazon Web Services services for which the specified account is
-- a delegated administrator.
--
-- This operation can be called only from the organization\'s management
-- account or by a member account that is a delegated administrator for an
-- Amazon Web Services service.
--
-- This operation returns paginated results.
module Amazonka.Organizations.ListDelegatedServicesForAccount
  ( -- * Creating a Request
    ListDelegatedServicesForAccount (..),
    newListDelegatedServicesForAccount,

    -- * Request Lenses
    listDelegatedServicesForAccount_nextToken,
    listDelegatedServicesForAccount_maxResults,
    listDelegatedServicesForAccount_accountId,

    -- * Destructuring the Response
    ListDelegatedServicesForAccountResponse (..),
    newListDelegatedServicesForAccountResponse,

    -- * Response Lenses
    listDelegatedServicesForAccountResponse_nextToken,
    listDelegatedServicesForAccountResponse_delegatedServices,
    listDelegatedServicesForAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDelegatedServicesForAccount' smart constructor.
data ListDelegatedServicesForAccount = ListDelegatedServicesForAccount'
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
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The account ID number of a delegated administrator account in the
    -- organization.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDelegatedServicesForAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDelegatedServicesForAccount_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
--
-- 'maxResults', 'listDelegatedServicesForAccount_maxResults' - The total number of results that you want included on each page of the
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
-- 'accountId', 'listDelegatedServicesForAccount_accountId' - The account ID number of a delegated administrator account in the
-- organization.
newListDelegatedServicesForAccount ::
  -- | 'accountId'
  Prelude.Text ->
  ListDelegatedServicesForAccount
newListDelegatedServicesForAccount pAccountId_ =
  ListDelegatedServicesForAccount'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      accountId = pAccountId_
    }

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listDelegatedServicesForAccount_nextToken :: Lens.Lens' ListDelegatedServicesForAccount (Prelude.Maybe Prelude.Text)
listDelegatedServicesForAccount_nextToken = Lens.lens (\ListDelegatedServicesForAccount' {nextToken} -> nextToken) (\s@ListDelegatedServicesForAccount' {} a -> s {nextToken = a} :: ListDelegatedServicesForAccount)

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
listDelegatedServicesForAccount_maxResults :: Lens.Lens' ListDelegatedServicesForAccount (Prelude.Maybe Prelude.Natural)
listDelegatedServicesForAccount_maxResults = Lens.lens (\ListDelegatedServicesForAccount' {maxResults} -> maxResults) (\s@ListDelegatedServicesForAccount' {} a -> s {maxResults = a} :: ListDelegatedServicesForAccount)

-- | The account ID number of a delegated administrator account in the
-- organization.
listDelegatedServicesForAccount_accountId :: Lens.Lens' ListDelegatedServicesForAccount Prelude.Text
listDelegatedServicesForAccount_accountId = Lens.lens (\ListDelegatedServicesForAccount' {accountId} -> accountId) (\s@ListDelegatedServicesForAccount' {} a -> s {accountId = a} :: ListDelegatedServicesForAccount)

instance
  Core.AWSPager
    ListDelegatedServicesForAccount
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDelegatedServicesForAccountResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDelegatedServicesForAccountResponse_delegatedServices
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDelegatedServicesForAccount_nextToken
          Lens..~ rs
          Lens.^? listDelegatedServicesForAccountResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListDelegatedServicesForAccount
  where
  type
    AWSResponse ListDelegatedServicesForAccount =
      ListDelegatedServicesForAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDelegatedServicesForAccountResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "DelegatedServices"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDelegatedServicesForAccount
  where
  hashWithSalt
    _salt
    ListDelegatedServicesForAccount' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` accountId

instance
  Prelude.NFData
    ListDelegatedServicesForAccount
  where
  rnf ListDelegatedServicesForAccount' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf accountId

instance
  Data.ToHeaders
    ListDelegatedServicesForAccount
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.ListDelegatedServicesForAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDelegatedServicesForAccount where
  toJSON ListDelegatedServicesForAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("AccountId" Data..= accountId)
          ]
      )

instance Data.ToPath ListDelegatedServicesForAccount where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDelegatedServicesForAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDelegatedServicesForAccountResponse' smart constructor.
data ListDelegatedServicesForAccountResponse = ListDelegatedServicesForAccountResponse'
  { -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The services for which the account is a delegated administrator.
    delegatedServices :: Prelude.Maybe [DelegatedService],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDelegatedServicesForAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDelegatedServicesForAccountResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'delegatedServices', 'listDelegatedServicesForAccountResponse_delegatedServices' - The services for which the account is a delegated administrator.
--
-- 'httpStatus', 'listDelegatedServicesForAccountResponse_httpStatus' - The response's http status code.
newListDelegatedServicesForAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDelegatedServicesForAccountResponse
newListDelegatedServicesForAccountResponse
  pHttpStatus_ =
    ListDelegatedServicesForAccountResponse'
      { nextToken =
          Prelude.Nothing,
        delegatedServices =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listDelegatedServicesForAccountResponse_nextToken :: Lens.Lens' ListDelegatedServicesForAccountResponse (Prelude.Maybe Prelude.Text)
listDelegatedServicesForAccountResponse_nextToken = Lens.lens (\ListDelegatedServicesForAccountResponse' {nextToken} -> nextToken) (\s@ListDelegatedServicesForAccountResponse' {} a -> s {nextToken = a} :: ListDelegatedServicesForAccountResponse)

-- | The services for which the account is a delegated administrator.
listDelegatedServicesForAccountResponse_delegatedServices :: Lens.Lens' ListDelegatedServicesForAccountResponse (Prelude.Maybe [DelegatedService])
listDelegatedServicesForAccountResponse_delegatedServices = Lens.lens (\ListDelegatedServicesForAccountResponse' {delegatedServices} -> delegatedServices) (\s@ListDelegatedServicesForAccountResponse' {} a -> s {delegatedServices = a} :: ListDelegatedServicesForAccountResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDelegatedServicesForAccountResponse_httpStatus :: Lens.Lens' ListDelegatedServicesForAccountResponse Prelude.Int
listDelegatedServicesForAccountResponse_httpStatus = Lens.lens (\ListDelegatedServicesForAccountResponse' {httpStatus} -> httpStatus) (\s@ListDelegatedServicesForAccountResponse' {} a -> s {httpStatus = a} :: ListDelegatedServicesForAccountResponse)

instance
  Prelude.NFData
    ListDelegatedServicesForAccountResponse
  where
  rnf ListDelegatedServicesForAccountResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf delegatedServices
      `Prelude.seq` Prelude.rnf httpStatus
