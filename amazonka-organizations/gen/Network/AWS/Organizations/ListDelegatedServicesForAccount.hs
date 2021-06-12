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
-- Module      : Network.AWS.Organizations.ListDelegatedServicesForAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the AWS services for which the specified account is a delegated
-- administrator.
--
-- This operation can be called only from the organization\'s management
-- account or by a member account that is a delegated administrator for an
-- AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListDelegatedServicesForAccount
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDelegatedServicesForAccount' smart constructor.
data ListDelegatedServicesForAccount = ListDelegatedServicesForAccount'
  { -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- of the previous call\'s @NextToken@ response to indicate where the
    -- output should continue from.
    nextToken :: Core.Maybe Core.Text,
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
    maxResults :: Core.Maybe Core.Natural,
    -- | The account ID number of a delegated administrator account in the
    -- organization.
    accountId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ListDelegatedServicesForAccount
newListDelegatedServicesForAccount pAccountId_ =
  ListDelegatedServicesForAccount'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      accountId = pAccountId_
    }

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listDelegatedServicesForAccount_nextToken :: Lens.Lens' ListDelegatedServicesForAccount (Core.Maybe Core.Text)
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
listDelegatedServicesForAccount_maxResults :: Lens.Lens' ListDelegatedServicesForAccount (Core.Maybe Core.Natural)
listDelegatedServicesForAccount_maxResults = Lens.lens (\ListDelegatedServicesForAccount' {maxResults} -> maxResults) (\s@ListDelegatedServicesForAccount' {} a -> s {maxResults = a} :: ListDelegatedServicesForAccount)

-- | The account ID number of a delegated administrator account in the
-- organization.
listDelegatedServicesForAccount_accountId :: Lens.Lens' ListDelegatedServicesForAccount Core.Text
listDelegatedServicesForAccount_accountId = Lens.lens (\ListDelegatedServicesForAccount' {accountId} -> accountId) (\s@ListDelegatedServicesForAccount' {} a -> s {accountId = a} :: ListDelegatedServicesForAccount)

instance
  Core.AWSPager
    ListDelegatedServicesForAccount
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDelegatedServicesForAccountResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDelegatedServicesForAccountResponse_delegatedServices
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDelegatedServicesForAccount_nextToken
          Lens..~ rs
          Lens.^? listDelegatedServicesForAccountResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListDelegatedServicesForAccount
  where
  type
    AWSResponse ListDelegatedServicesForAccount =
      ListDelegatedServicesForAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDelegatedServicesForAccountResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "DelegatedServices" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListDelegatedServicesForAccount

instance Core.NFData ListDelegatedServicesForAccount

instance
  Core.ToHeaders
    ListDelegatedServicesForAccount
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.ListDelegatedServicesForAccount" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDelegatedServicesForAccount where
  toJSON ListDelegatedServicesForAccount' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("AccountId" Core..= accountId)
          ]
      )

instance Core.ToPath ListDelegatedServicesForAccount where
  toPath = Core.const "/"

instance Core.ToQuery ListDelegatedServicesForAccount where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListDelegatedServicesForAccountResponse' smart constructor.
data ListDelegatedServicesForAccountResponse = ListDelegatedServicesForAccountResponse'
  { -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Core.Maybe Core.Text,
    -- | The services for which the account is a delegated administrator.
    delegatedServices :: Core.Maybe [DelegatedService],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListDelegatedServicesForAccountResponse
newListDelegatedServicesForAccountResponse
  pHttpStatus_ =
    ListDelegatedServicesForAccountResponse'
      { nextToken =
          Core.Nothing,
        delegatedServices = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listDelegatedServicesForAccountResponse_nextToken :: Lens.Lens' ListDelegatedServicesForAccountResponse (Core.Maybe Core.Text)
listDelegatedServicesForAccountResponse_nextToken = Lens.lens (\ListDelegatedServicesForAccountResponse' {nextToken} -> nextToken) (\s@ListDelegatedServicesForAccountResponse' {} a -> s {nextToken = a} :: ListDelegatedServicesForAccountResponse)

-- | The services for which the account is a delegated administrator.
listDelegatedServicesForAccountResponse_delegatedServices :: Lens.Lens' ListDelegatedServicesForAccountResponse (Core.Maybe [DelegatedService])
listDelegatedServicesForAccountResponse_delegatedServices = Lens.lens (\ListDelegatedServicesForAccountResponse' {delegatedServices} -> delegatedServices) (\s@ListDelegatedServicesForAccountResponse' {} a -> s {delegatedServices = a} :: ListDelegatedServicesForAccountResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDelegatedServicesForAccountResponse_httpStatus :: Lens.Lens' ListDelegatedServicesForAccountResponse Core.Int
listDelegatedServicesForAccountResponse_httpStatus = Lens.lens (\ListDelegatedServicesForAccountResponse' {httpStatus} -> httpStatus) (\s@ListDelegatedServicesForAccountResponse' {} a -> s {httpStatus = a} :: ListDelegatedServicesForAccountResponse)

instance
  Core.NFData
    ListDelegatedServicesForAccountResponse
