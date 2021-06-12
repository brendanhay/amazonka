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
-- Module      : Network.AWS.WorkMail.ListUsers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summaries of the organization\'s users.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListUsers
  ( -- * Creating a Request
    ListUsers (..),
    newListUsers,

    -- * Request Lenses
    listUsers_nextToken,
    listUsers_maxResults,
    listUsers_organizationId,

    -- * Destructuring the Response
    ListUsersResponse (..),
    newListUsersResponse,

    -- * Response Lenses
    listUsersResponse_nextToken,
    listUsersResponse_users,
    listUsersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newListUsers' smart constructor.
data ListUsers = ListUsers'
  { -- | The token to use to retrieve the next page of results. The first call
    -- does not contain any tokens.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier for the organization under which the users exist.
    organizationId :: Core.Text
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
-- 'nextToken', 'listUsers_nextToken' - The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
--
-- 'maxResults', 'listUsers_maxResults' - The maximum number of results to return in a single call.
--
-- 'organizationId', 'listUsers_organizationId' - The identifier for the organization under which the users exist.
newListUsers ::
  -- | 'organizationId'
  Core.Text ->
  ListUsers
newListUsers pOrganizationId_ =
  ListUsers'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      organizationId = pOrganizationId_
    }

-- | The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
listUsers_nextToken :: Lens.Lens' ListUsers (Core.Maybe Core.Text)
listUsers_nextToken = Lens.lens (\ListUsers' {nextToken} -> nextToken) (\s@ListUsers' {} a -> s {nextToken = a} :: ListUsers)

-- | The maximum number of results to return in a single call.
listUsers_maxResults :: Lens.Lens' ListUsers (Core.Maybe Core.Natural)
listUsers_maxResults = Lens.lens (\ListUsers' {maxResults} -> maxResults) (\s@ListUsers' {} a -> s {maxResults = a} :: ListUsers)

-- | The identifier for the organization under which the users exist.
listUsers_organizationId :: Lens.Lens' ListUsers Core.Text
listUsers_organizationId = Lens.lens (\ListUsers' {organizationId} -> organizationId) (\s@ListUsers' {} a -> s {organizationId = a} :: ListUsers)

instance Core.AWSPager ListUsers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUsersResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listUsersResponse_users Core.. Lens._Just
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUsersResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Users" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListUsers

instance Core.NFData ListUsers

instance Core.ToHeaders ListUsers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("WorkMailService.ListUsers" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListUsers where
  toJSON ListUsers' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("OrganizationId" Core..= organizationId)
          ]
      )

instance Core.ToPath ListUsers where
  toPath = Core.const "/"

instance Core.ToQuery ListUsers where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- \`null\` when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The overview of users for an organization.
    users :: Core.Maybe [User],
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
-- 'nextToken', 'listUsersResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- \`null\` when there are no more results to return.
--
-- 'users', 'listUsersResponse_users' - The overview of users for an organization.
--
-- 'httpStatus', 'listUsersResponse_httpStatus' - The response's http status code.
newListUsersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListUsersResponse
newListUsersResponse pHttpStatus_ =
  ListUsersResponse'
    { nextToken = Core.Nothing,
      users = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- \`null\` when there are no more results to return.
listUsersResponse_nextToken :: Lens.Lens' ListUsersResponse (Core.Maybe Core.Text)
listUsersResponse_nextToken = Lens.lens (\ListUsersResponse' {nextToken} -> nextToken) (\s@ListUsersResponse' {} a -> s {nextToken = a} :: ListUsersResponse)

-- | The overview of users for an organization.
listUsersResponse_users :: Lens.Lens' ListUsersResponse (Core.Maybe [User])
listUsersResponse_users = Lens.lens (\ListUsersResponse' {users} -> users) (\s@ListUsersResponse' {} a -> s {users = a} :: ListUsersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listUsersResponse_httpStatus :: Lens.Lens' ListUsersResponse Core.Int
listUsersResponse_httpStatus = Lens.lens (\ListUsersResponse' {httpStatus} -> httpStatus) (\s@ListUsersResponse' {} a -> s {httpStatus = a} :: ListUsersResponse)

instance Core.NFData ListUsersResponse
