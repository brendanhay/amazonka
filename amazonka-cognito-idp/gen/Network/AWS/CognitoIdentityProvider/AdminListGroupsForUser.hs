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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminListGroupsForUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the groups that the user belongs to.
--
-- Calling this action requires developer credentials.
--
-- This operation returns paginated results.
module Network.AWS.CognitoIdentityProvider.AdminListGroupsForUser
  ( -- * Creating a Request
    AdminListGroupsForUser (..),
    newAdminListGroupsForUser,

    -- * Request Lenses
    adminListGroupsForUser_nextToken,
    adminListGroupsForUser_limit,
    adminListGroupsForUser_username,
    adminListGroupsForUser_userPoolId,

    -- * Destructuring the Response
    AdminListGroupsForUserResponse (..),
    newAdminListGroupsForUserResponse,

    -- * Response Lenses
    adminListGroupsForUserResponse_groups,
    adminListGroupsForUserResponse_nextToken,
    adminListGroupsForUserResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAdminListGroupsForUser' smart constructor.
data AdminListGroupsForUser = AdminListGroupsForUser'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The limit of the request to list groups.
    limit :: Core.Maybe Core.Natural,
    -- | The username for the user.
    username :: Core.Sensitive Core.Text,
    -- | The user pool ID for the user pool.
    userPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminListGroupsForUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'adminListGroupsForUser_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'limit', 'adminListGroupsForUser_limit' - The limit of the request to list groups.
--
-- 'username', 'adminListGroupsForUser_username' - The username for the user.
--
-- 'userPoolId', 'adminListGroupsForUser_userPoolId' - The user pool ID for the user pool.
newAdminListGroupsForUser ::
  -- | 'username'
  Core.Text ->
  -- | 'userPoolId'
  Core.Text ->
  AdminListGroupsForUser
newAdminListGroupsForUser pUsername_ pUserPoolId_ =
  AdminListGroupsForUser'
    { nextToken = Core.Nothing,
      limit = Core.Nothing,
      username = Core._Sensitive Lens.# pUsername_,
      userPoolId = pUserPoolId_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
adminListGroupsForUser_nextToken :: Lens.Lens' AdminListGroupsForUser (Core.Maybe Core.Text)
adminListGroupsForUser_nextToken = Lens.lens (\AdminListGroupsForUser' {nextToken} -> nextToken) (\s@AdminListGroupsForUser' {} a -> s {nextToken = a} :: AdminListGroupsForUser)

-- | The limit of the request to list groups.
adminListGroupsForUser_limit :: Lens.Lens' AdminListGroupsForUser (Core.Maybe Core.Natural)
adminListGroupsForUser_limit = Lens.lens (\AdminListGroupsForUser' {limit} -> limit) (\s@AdminListGroupsForUser' {} a -> s {limit = a} :: AdminListGroupsForUser)

-- | The username for the user.
adminListGroupsForUser_username :: Lens.Lens' AdminListGroupsForUser Core.Text
adminListGroupsForUser_username = Lens.lens (\AdminListGroupsForUser' {username} -> username) (\s@AdminListGroupsForUser' {} a -> s {username = a} :: AdminListGroupsForUser) Core.. Core._Sensitive

-- | The user pool ID for the user pool.
adminListGroupsForUser_userPoolId :: Lens.Lens' AdminListGroupsForUser Core.Text
adminListGroupsForUser_userPoolId = Lens.lens (\AdminListGroupsForUser' {userPoolId} -> userPoolId) (\s@AdminListGroupsForUser' {} a -> s {userPoolId = a} :: AdminListGroupsForUser)

instance Core.AWSPager AdminListGroupsForUser where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? adminListGroupsForUserResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? adminListGroupsForUserResponse_groups
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& adminListGroupsForUser_nextToken
          Lens..~ rs
          Lens.^? adminListGroupsForUserResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest AdminListGroupsForUser where
  type
    AWSResponse AdminListGroupsForUser =
      AdminListGroupsForUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AdminListGroupsForUserResponse'
            Core.<$> (x Core..?> "Groups" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AdminListGroupsForUser

instance Core.NFData AdminListGroupsForUser

instance Core.ToHeaders AdminListGroupsForUser where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.AdminListGroupsForUser" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AdminListGroupsForUser where
  toJSON AdminListGroupsForUser' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just ("Username" Core..= username),
            Core.Just ("UserPoolId" Core..= userPoolId)
          ]
      )

instance Core.ToPath AdminListGroupsForUser where
  toPath = Core.const "/"

instance Core.ToQuery AdminListGroupsForUser where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAdminListGroupsForUserResponse' smart constructor.
data AdminListGroupsForUserResponse = AdminListGroupsForUserResponse'
  { -- | The groups that the user belongs to.
    groups :: Core.Maybe [GroupType],
    -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminListGroupsForUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'adminListGroupsForUserResponse_groups' - The groups that the user belongs to.
--
-- 'nextToken', 'adminListGroupsForUserResponse_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'httpStatus', 'adminListGroupsForUserResponse_httpStatus' - The response's http status code.
newAdminListGroupsForUserResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AdminListGroupsForUserResponse
newAdminListGroupsForUserResponse pHttpStatus_ =
  AdminListGroupsForUserResponse'
    { groups =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The groups that the user belongs to.
adminListGroupsForUserResponse_groups :: Lens.Lens' AdminListGroupsForUserResponse (Core.Maybe [GroupType])
adminListGroupsForUserResponse_groups = Lens.lens (\AdminListGroupsForUserResponse' {groups} -> groups) (\s@AdminListGroupsForUserResponse' {} a -> s {groups = a} :: AdminListGroupsForUserResponse) Core.. Lens.mapping Lens._Coerce

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
adminListGroupsForUserResponse_nextToken :: Lens.Lens' AdminListGroupsForUserResponse (Core.Maybe Core.Text)
adminListGroupsForUserResponse_nextToken = Lens.lens (\AdminListGroupsForUserResponse' {nextToken} -> nextToken) (\s@AdminListGroupsForUserResponse' {} a -> s {nextToken = a} :: AdminListGroupsForUserResponse)

-- | The response's http status code.
adminListGroupsForUserResponse_httpStatus :: Lens.Lens' AdminListGroupsForUserResponse Core.Int
adminListGroupsForUserResponse_httpStatus = Lens.lens (\AdminListGroupsForUserResponse' {httpStatus} -> httpStatus) (\s@AdminListGroupsForUserResponse' {} a -> s {httpStatus = a} :: AdminListGroupsForUserResponse)

instance Core.NFData AdminListGroupsForUserResponse
