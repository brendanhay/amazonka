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
-- Module      : Network.AWS.CognitoIdentityProvider.ListUsersInGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the users in the specified group.
--
-- Calling this action requires developer credentials.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.ListUsersInGroup
  ( -- * Creating a Request
    ListUsersInGroup (..),
    newListUsersInGroup,

    -- * Request Lenses
    listUsersInGroup_nextToken,
    listUsersInGroup_limit,
    listUsersInGroup_userPoolId,
    listUsersInGroup_groupName,

    -- * Destructuring the Response
    ListUsersInGroupResponse (..),
    newListUsersInGroupResponse,

    -- * Response Lenses
    listUsersInGroupResponse_nextToken,
    listUsersInGroupResponse_users,
    listUsersInGroupResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListUsersInGroup' smart constructor.
data ListUsersInGroup = ListUsersInGroup'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The limit of the request to list users.
    limit :: Core.Maybe Core.Natural,
    -- | The user pool ID for the user pool.
    userPoolId :: Core.Text,
    -- | The name of the group.
    groupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUsersInGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUsersInGroup_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'limit', 'listUsersInGroup_limit' - The limit of the request to list users.
--
-- 'userPoolId', 'listUsersInGroup_userPoolId' - The user pool ID for the user pool.
--
-- 'groupName', 'listUsersInGroup_groupName' - The name of the group.
newListUsersInGroup ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'groupName'
  Core.Text ->
  ListUsersInGroup
newListUsersInGroup pUserPoolId_ pGroupName_ =
  ListUsersInGroup'
    { nextToken = Core.Nothing,
      limit = Core.Nothing,
      userPoolId = pUserPoolId_,
      groupName = pGroupName_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listUsersInGroup_nextToken :: Lens.Lens' ListUsersInGroup (Core.Maybe Core.Text)
listUsersInGroup_nextToken = Lens.lens (\ListUsersInGroup' {nextToken} -> nextToken) (\s@ListUsersInGroup' {} a -> s {nextToken = a} :: ListUsersInGroup)

-- | The limit of the request to list users.
listUsersInGroup_limit :: Lens.Lens' ListUsersInGroup (Core.Maybe Core.Natural)
listUsersInGroup_limit = Lens.lens (\ListUsersInGroup' {limit} -> limit) (\s@ListUsersInGroup' {} a -> s {limit = a} :: ListUsersInGroup)

-- | The user pool ID for the user pool.
listUsersInGroup_userPoolId :: Lens.Lens' ListUsersInGroup Core.Text
listUsersInGroup_userPoolId = Lens.lens (\ListUsersInGroup' {userPoolId} -> userPoolId) (\s@ListUsersInGroup' {} a -> s {userPoolId = a} :: ListUsersInGroup)

-- | The name of the group.
listUsersInGroup_groupName :: Lens.Lens' ListUsersInGroup Core.Text
listUsersInGroup_groupName = Lens.lens (\ListUsersInGroup' {groupName} -> groupName) (\s@ListUsersInGroup' {} a -> s {groupName = a} :: ListUsersInGroup)

instance Core.AWSPager ListUsersInGroup where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUsersInGroupResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listUsersInGroupResponse_users Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listUsersInGroup_nextToken
          Lens..~ rs
          Lens.^? listUsersInGroupResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListUsersInGroup where
  type
    AWSResponse ListUsersInGroup =
      ListUsersInGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUsersInGroupResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Users" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListUsersInGroup

instance Core.NFData ListUsersInGroup

instance Core.ToHeaders ListUsersInGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.ListUsersInGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListUsersInGroup where
  toJSON ListUsersInGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("GroupName" Core..= groupName)
          ]
      )

instance Core.ToPath ListUsersInGroup where
  toPath = Core.const "/"

instance Core.ToQuery ListUsersInGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListUsersInGroupResponse' smart constructor.
data ListUsersInGroupResponse = ListUsersInGroupResponse'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The users returned in the request to list users.
    users :: Core.Maybe [UserType],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUsersInGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUsersInGroupResponse_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'users', 'listUsersInGroupResponse_users' - The users returned in the request to list users.
--
-- 'httpStatus', 'listUsersInGroupResponse_httpStatus' - The response's http status code.
newListUsersInGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListUsersInGroupResponse
newListUsersInGroupResponse pHttpStatus_ =
  ListUsersInGroupResponse'
    { nextToken = Core.Nothing,
      users = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listUsersInGroupResponse_nextToken :: Lens.Lens' ListUsersInGroupResponse (Core.Maybe Core.Text)
listUsersInGroupResponse_nextToken = Lens.lens (\ListUsersInGroupResponse' {nextToken} -> nextToken) (\s@ListUsersInGroupResponse' {} a -> s {nextToken = a} :: ListUsersInGroupResponse)

-- | The users returned in the request to list users.
listUsersInGroupResponse_users :: Lens.Lens' ListUsersInGroupResponse (Core.Maybe [UserType])
listUsersInGroupResponse_users = Lens.lens (\ListUsersInGroupResponse' {users} -> users) (\s@ListUsersInGroupResponse' {} a -> s {users = a} :: ListUsersInGroupResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listUsersInGroupResponse_httpStatus :: Lens.Lens' ListUsersInGroupResponse Core.Int
listUsersInGroupResponse_httpStatus = Lens.lens (\ListUsersInGroupResponse' {httpStatus} -> httpStatus) (\s@ListUsersInGroupResponse' {} a -> s {httpStatus = a} :: ListUsersInGroupResponse)

instance Core.NFData ListUsersInGroupResponse
