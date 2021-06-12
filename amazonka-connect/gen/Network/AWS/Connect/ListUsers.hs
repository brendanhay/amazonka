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
-- Module      : Network.AWS.Connect.ListUsers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summary information about the users for the specified Amazon
-- Connect instance.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListUsers
  ( -- * Creating a Request
    ListUsers (..),
    newListUsers,

    -- * Request Lenses
    listUsers_nextToken,
    listUsers_maxResults,
    listUsers_instanceId,

    -- * Destructuring the Response
    ListUsersResponse (..),
    newListUsersResponse,

    -- * Response Lenses
    listUsersResponse_userSummaryList,
    listUsersResponse_nextToken,
    listUsersResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListUsers' smart constructor.
data ListUsers = ListUsers'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUsers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUsers_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listUsers_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'listUsers_instanceId' - The identifier of the Amazon Connect instance.
newListUsers ::
  -- | 'instanceId'
  Core.Text ->
  ListUsers
newListUsers pInstanceId_ =
  ListUsers'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listUsers_nextToken :: Lens.Lens' ListUsers (Core.Maybe Core.Text)
listUsers_nextToken = Lens.lens (\ListUsers' {nextToken} -> nextToken) (\s@ListUsers' {} a -> s {nextToken = a} :: ListUsers)

-- | The maximum number of results to return per page.
listUsers_maxResults :: Lens.Lens' ListUsers (Core.Maybe Core.Natural)
listUsers_maxResults = Lens.lens (\ListUsers' {maxResults} -> maxResults) (\s@ListUsers' {} a -> s {maxResults = a} :: ListUsers)

-- | The identifier of the Amazon Connect instance.
listUsers_instanceId :: Lens.Lens' ListUsers Core.Text
listUsers_instanceId = Lens.lens (\ListUsers' {instanceId} -> instanceId) (\s@ListUsers' {} a -> s {instanceId = a} :: ListUsers)

instance Core.AWSPager ListUsers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUsersResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listUsersResponse_userSummaryList Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listUsers_nextToken
          Lens..~ rs
          Lens.^? listUsersResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListUsers where
  type AWSResponse ListUsers = ListUsersResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUsersResponse'
            Core.<$> (x Core..?> "UserSummaryList" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListUsers

instance Core.NFData ListUsers

instance Core.ToHeaders ListUsers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListUsers where
  toPath ListUsers' {..} =
    Core.mconcat
      ["/users-summary/", Core.toBS instanceId]

instance Core.ToQuery ListUsers where
  toQuery ListUsers' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { -- | Information about the users.
    userSummaryList :: Core.Maybe [UserSummary],
    -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userSummaryList', 'listUsersResponse_userSummaryList' - Information about the users.
--
-- 'nextToken', 'listUsersResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listUsersResponse_httpStatus' - The response's http status code.
newListUsersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListUsersResponse
newListUsersResponse pHttpStatus_ =
  ListUsersResponse'
    { userSummaryList = Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the users.
listUsersResponse_userSummaryList :: Lens.Lens' ListUsersResponse (Core.Maybe [UserSummary])
listUsersResponse_userSummaryList = Lens.lens (\ListUsersResponse' {userSummaryList} -> userSummaryList) (\s@ListUsersResponse' {} a -> s {userSummaryList = a} :: ListUsersResponse) Core.. Lens.mapping Lens._Coerce

-- | If there are additional results, this is the token for the next set of
-- results.
listUsersResponse_nextToken :: Lens.Lens' ListUsersResponse (Core.Maybe Core.Text)
listUsersResponse_nextToken = Lens.lens (\ListUsersResponse' {nextToken} -> nextToken) (\s@ListUsersResponse' {} a -> s {nextToken = a} :: ListUsersResponse)

-- | The response's http status code.
listUsersResponse_httpStatus :: Lens.Lens' ListUsersResponse Core.Int
listUsersResponse_httpStatus = Lens.lens (\ListUsersResponse' {httpStatus} -> httpStatus) (\s@ListUsersResponse' {} a -> s {httpStatus = a} :: ListUsersResponse)

instance Core.NFData ListUsersResponse
