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
-- Module      : Network.AWS.FMS.ListMemberAccounts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a @MemberAccounts@ object that lists the member accounts in the
-- administrator\'s AWS organization.
--
-- The @ListMemberAccounts@ must be submitted by the account that is set as
-- the AWS Firewall Manager administrator.
--
-- This operation returns paginated results.
module Network.AWS.FMS.ListMemberAccounts
  ( -- * Creating a Request
    ListMemberAccounts (..),
    newListMemberAccounts,

    -- * Request Lenses
    listMemberAccounts_nextToken,
    listMemberAccounts_maxResults,

    -- * Destructuring the Response
    ListMemberAccountsResponse (..),
    newListMemberAccountsResponse,

    -- * Response Lenses
    listMemberAccountsResponse_nextToken,
    listMemberAccountsResponse_memberAccounts,
    listMemberAccountsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListMemberAccounts' smart constructor.
data ListMemberAccounts = ListMemberAccounts'
  { -- | If you specify a value for @MaxResults@ and you have more account IDs
    -- than the number that you specify for @MaxResults@, AWS Firewall Manager
    -- returns a @NextToken@ value in the response that allows you to list
    -- another group of IDs. For the second and subsequent
    -- @ListMemberAccountsRequest@ requests, specify the value of @NextToken@
    -- from the previous response to get information about another batch of
    -- member account IDs.
    nextToken :: Core.Maybe Core.Text,
    -- | Specifies the number of member account IDs that you want AWS Firewall
    -- Manager to return for this request. If you have more IDs than the number
    -- that you specify for @MaxResults@, the response includes a @NextToken@
    -- value that you can use to get another batch of member account IDs.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListMemberAccounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMemberAccounts_nextToken' - If you specify a value for @MaxResults@ and you have more account IDs
-- than the number that you specify for @MaxResults@, AWS Firewall Manager
-- returns a @NextToken@ value in the response that allows you to list
-- another group of IDs. For the second and subsequent
-- @ListMemberAccountsRequest@ requests, specify the value of @NextToken@
-- from the previous response to get information about another batch of
-- member account IDs.
--
-- 'maxResults', 'listMemberAccounts_maxResults' - Specifies the number of member account IDs that you want AWS Firewall
-- Manager to return for this request. If you have more IDs than the number
-- that you specify for @MaxResults@, the response includes a @NextToken@
-- value that you can use to get another batch of member account IDs.
newListMemberAccounts ::
  ListMemberAccounts
newListMemberAccounts =
  ListMemberAccounts'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | If you specify a value for @MaxResults@ and you have more account IDs
-- than the number that you specify for @MaxResults@, AWS Firewall Manager
-- returns a @NextToken@ value in the response that allows you to list
-- another group of IDs. For the second and subsequent
-- @ListMemberAccountsRequest@ requests, specify the value of @NextToken@
-- from the previous response to get information about another batch of
-- member account IDs.
listMemberAccounts_nextToken :: Lens.Lens' ListMemberAccounts (Core.Maybe Core.Text)
listMemberAccounts_nextToken = Lens.lens (\ListMemberAccounts' {nextToken} -> nextToken) (\s@ListMemberAccounts' {} a -> s {nextToken = a} :: ListMemberAccounts)

-- | Specifies the number of member account IDs that you want AWS Firewall
-- Manager to return for this request. If you have more IDs than the number
-- that you specify for @MaxResults@, the response includes a @NextToken@
-- value that you can use to get another batch of member account IDs.
listMemberAccounts_maxResults :: Lens.Lens' ListMemberAccounts (Core.Maybe Core.Natural)
listMemberAccounts_maxResults = Lens.lens (\ListMemberAccounts' {maxResults} -> maxResults) (\s@ListMemberAccounts' {} a -> s {maxResults = a} :: ListMemberAccounts)

instance Core.AWSPager ListMemberAccounts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMemberAccountsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listMemberAccountsResponse_memberAccounts
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listMemberAccounts_nextToken
          Lens..~ rs
          Lens.^? listMemberAccountsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListMemberAccounts where
  type
    AWSResponse ListMemberAccounts =
      ListMemberAccountsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMemberAccountsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "MemberAccounts" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListMemberAccounts

instance Core.NFData ListMemberAccounts

instance Core.ToHeaders ListMemberAccounts where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSFMS_20180101.ListMemberAccounts" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListMemberAccounts where
  toJSON ListMemberAccounts' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListMemberAccounts where
  toPath = Core.const "/"

instance Core.ToQuery ListMemberAccounts where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListMemberAccountsResponse' smart constructor.
data ListMemberAccountsResponse = ListMemberAccountsResponse'
  { -- | If you have more member account IDs than the number that you specified
    -- for @MaxResults@ in the request, the response includes a @NextToken@
    -- value. To list more IDs, submit another @ListMemberAccounts@ request,
    -- and specify the @NextToken@ value from the response in the @NextToken@
    -- value in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of account IDs.
    memberAccounts :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListMemberAccountsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMemberAccountsResponse_nextToken' - If you have more member account IDs than the number that you specified
-- for @MaxResults@ in the request, the response includes a @NextToken@
-- value. To list more IDs, submit another @ListMemberAccounts@ request,
-- and specify the @NextToken@ value from the response in the @NextToken@
-- value in the next request.
--
-- 'memberAccounts', 'listMemberAccountsResponse_memberAccounts' - An array of account IDs.
--
-- 'httpStatus', 'listMemberAccountsResponse_httpStatus' - The response's http status code.
newListMemberAccountsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListMemberAccountsResponse
newListMemberAccountsResponse pHttpStatus_ =
  ListMemberAccountsResponse'
    { nextToken =
        Core.Nothing,
      memberAccounts = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you have more member account IDs than the number that you specified
-- for @MaxResults@ in the request, the response includes a @NextToken@
-- value. To list more IDs, submit another @ListMemberAccounts@ request,
-- and specify the @NextToken@ value from the response in the @NextToken@
-- value in the next request.
listMemberAccountsResponse_nextToken :: Lens.Lens' ListMemberAccountsResponse (Core.Maybe Core.Text)
listMemberAccountsResponse_nextToken = Lens.lens (\ListMemberAccountsResponse' {nextToken} -> nextToken) (\s@ListMemberAccountsResponse' {} a -> s {nextToken = a} :: ListMemberAccountsResponse)

-- | An array of account IDs.
listMemberAccountsResponse_memberAccounts :: Lens.Lens' ListMemberAccountsResponse (Core.Maybe [Core.Text])
listMemberAccountsResponse_memberAccounts = Lens.lens (\ListMemberAccountsResponse' {memberAccounts} -> memberAccounts) (\s@ListMemberAccountsResponse' {} a -> s {memberAccounts = a} :: ListMemberAccountsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listMemberAccountsResponse_httpStatus :: Lens.Lens' ListMemberAccountsResponse Core.Int
listMemberAccountsResponse_httpStatus = Lens.lens (\ListMemberAccountsResponse' {httpStatus} -> httpStatus) (\s@ListMemberAccountsResponse' {} a -> s {httpStatus = a} :: ListMemberAccountsResponse)

instance Core.NFData ListMemberAccountsResponse
