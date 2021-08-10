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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListUsersInGroup' smart constructor.
data ListUsersInGroup = ListUsersInGroup'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The limit of the request to list users.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The user pool ID for the user pool.
    userPoolId :: Prelude.Text,
    -- | The name of the group.
    groupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'groupName'
  Prelude.Text ->
  ListUsersInGroup
newListUsersInGroup pUserPoolId_ pGroupName_ =
  ListUsersInGroup'
    { nextToken = Prelude.Nothing,
      limit = Prelude.Nothing,
      userPoolId = pUserPoolId_,
      groupName = pGroupName_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listUsersInGroup_nextToken :: Lens.Lens' ListUsersInGroup (Prelude.Maybe Prelude.Text)
listUsersInGroup_nextToken = Lens.lens (\ListUsersInGroup' {nextToken} -> nextToken) (\s@ListUsersInGroup' {} a -> s {nextToken = a} :: ListUsersInGroup)

-- | The limit of the request to list users.
listUsersInGroup_limit :: Lens.Lens' ListUsersInGroup (Prelude.Maybe Prelude.Natural)
listUsersInGroup_limit = Lens.lens (\ListUsersInGroup' {limit} -> limit) (\s@ListUsersInGroup' {} a -> s {limit = a} :: ListUsersInGroup)

-- | The user pool ID for the user pool.
listUsersInGroup_userPoolId :: Lens.Lens' ListUsersInGroup Prelude.Text
listUsersInGroup_userPoolId = Lens.lens (\ListUsersInGroup' {userPoolId} -> userPoolId) (\s@ListUsersInGroup' {} a -> s {userPoolId = a} :: ListUsersInGroup)

-- | The name of the group.
listUsersInGroup_groupName :: Lens.Lens' ListUsersInGroup Prelude.Text
listUsersInGroup_groupName = Lens.lens (\ListUsersInGroup' {groupName} -> groupName) (\s@ListUsersInGroup' {} a -> s {groupName = a} :: ListUsersInGroup)

instance Core.AWSPager ListUsersInGroup where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUsersInGroupResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listUsersInGroupResponse_users Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listUsersInGroup_nextToken
          Lens..~ rs
          Lens.^? listUsersInGroupResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListUsersInGroup where
  type
    AWSResponse ListUsersInGroup =
      ListUsersInGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUsersInGroupResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Users" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListUsersInGroup

instance Prelude.NFData ListUsersInGroup

instance Core.ToHeaders ListUsersInGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.ListUsersInGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListUsersInGroup where
  toJSON ListUsersInGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just ("UserPoolId" Core..= userPoolId),
            Prelude.Just ("GroupName" Core..= groupName)
          ]
      )

instance Core.ToPath ListUsersInGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery ListUsersInGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListUsersInGroupResponse' smart constructor.
data ListUsersInGroupResponse = ListUsersInGroupResponse'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The users returned in the request to list users.
    users :: Prelude.Maybe [UserType],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListUsersInGroupResponse
newListUsersInGroupResponse pHttpStatus_ =
  ListUsersInGroupResponse'
    { nextToken =
        Prelude.Nothing,
      users = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listUsersInGroupResponse_nextToken :: Lens.Lens' ListUsersInGroupResponse (Prelude.Maybe Prelude.Text)
listUsersInGroupResponse_nextToken = Lens.lens (\ListUsersInGroupResponse' {nextToken} -> nextToken) (\s@ListUsersInGroupResponse' {} a -> s {nextToken = a} :: ListUsersInGroupResponse)

-- | The users returned in the request to list users.
listUsersInGroupResponse_users :: Lens.Lens' ListUsersInGroupResponse (Prelude.Maybe [UserType])
listUsersInGroupResponse_users = Lens.lens (\ListUsersInGroupResponse' {users} -> users) (\s@ListUsersInGroupResponse' {} a -> s {users = a} :: ListUsersInGroupResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listUsersInGroupResponse_httpStatus :: Lens.Lens' ListUsersInGroupResponse Prelude.Int
listUsersInGroupResponse_httpStatus = Lens.lens (\ListUsersInGroupResponse' {httpStatus} -> httpStatus) (\s@ListUsersInGroupResponse' {} a -> s {httpStatus = a} :: ListUsersInGroupResponse)

instance Prelude.NFData ListUsersInGroupResponse
