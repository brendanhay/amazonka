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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminAddUserToGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified user to the specified group.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminAddUserToGroup
  ( -- * Creating a Request
    AdminAddUserToGroup (..),
    newAdminAddUserToGroup,

    -- * Request Lenses
    adminAddUserToGroup_userPoolId,
    adminAddUserToGroup_username,
    adminAddUserToGroup_groupName,

    -- * Destructuring the Response
    AdminAddUserToGroupResponse (..),
    newAdminAddUserToGroupResponse,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAdminAddUserToGroup' smart constructor.
data AdminAddUserToGroup = AdminAddUserToGroup'
  { -- | The user pool ID for the user pool.
    userPoolId :: Core.Text,
    -- | The username for the user.
    username :: Core.Sensitive Core.Text,
    -- | The group name.
    groupName :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminAddUserToGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'adminAddUserToGroup_userPoolId' - The user pool ID for the user pool.
--
-- 'username', 'adminAddUserToGroup_username' - The username for the user.
--
-- 'groupName', 'adminAddUserToGroup_groupName' - The group name.
newAdminAddUserToGroup ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'username'
  Core.Text ->
  -- | 'groupName'
  Core.Text ->
  AdminAddUserToGroup
newAdminAddUserToGroup
  pUserPoolId_
  pUsername_
  pGroupName_ =
    AdminAddUserToGroup'
      { userPoolId = pUserPoolId_,
        username = Core._Sensitive Lens.# pUsername_,
        groupName = pGroupName_
      }

-- | The user pool ID for the user pool.
adminAddUserToGroup_userPoolId :: Lens.Lens' AdminAddUserToGroup Core.Text
adminAddUserToGroup_userPoolId = Lens.lens (\AdminAddUserToGroup' {userPoolId} -> userPoolId) (\s@AdminAddUserToGroup' {} a -> s {userPoolId = a} :: AdminAddUserToGroup)

-- | The username for the user.
adminAddUserToGroup_username :: Lens.Lens' AdminAddUserToGroup Core.Text
adminAddUserToGroup_username = Lens.lens (\AdminAddUserToGroup' {username} -> username) (\s@AdminAddUserToGroup' {} a -> s {username = a} :: AdminAddUserToGroup) Core.. Core._Sensitive

-- | The group name.
adminAddUserToGroup_groupName :: Lens.Lens' AdminAddUserToGroup Core.Text
adminAddUserToGroup_groupName = Lens.lens (\AdminAddUserToGroup' {groupName} -> groupName) (\s@AdminAddUserToGroup' {} a -> s {groupName = a} :: AdminAddUserToGroup)

instance Core.AWSRequest AdminAddUserToGroup where
  type
    AWSResponse AdminAddUserToGroup =
      AdminAddUserToGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull AdminAddUserToGroupResponse'

instance Core.Hashable AdminAddUserToGroup

instance Core.NFData AdminAddUserToGroup

instance Core.ToHeaders AdminAddUserToGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.AdminAddUserToGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AdminAddUserToGroup where
  toJSON AdminAddUserToGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Username" Core..= username),
            Core.Just ("GroupName" Core..= groupName)
          ]
      )

instance Core.ToPath AdminAddUserToGroup where
  toPath = Core.const "/"

instance Core.ToQuery AdminAddUserToGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAdminAddUserToGroupResponse' smart constructor.
data AdminAddUserToGroupResponse = AdminAddUserToGroupResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminAddUserToGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAdminAddUserToGroupResponse ::
  AdminAddUserToGroupResponse
newAdminAddUserToGroupResponse =
  AdminAddUserToGroupResponse'

instance Core.NFData AdminAddUserToGroupResponse
