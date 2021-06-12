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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminRemoveUserFromGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified user from the specified group.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminRemoveUserFromGroup
  ( -- * Creating a Request
    AdminRemoveUserFromGroup (..),
    newAdminRemoveUserFromGroup,

    -- * Request Lenses
    adminRemoveUserFromGroup_userPoolId,
    adminRemoveUserFromGroup_username,
    adminRemoveUserFromGroup_groupName,

    -- * Destructuring the Response
    AdminRemoveUserFromGroupResponse (..),
    newAdminRemoveUserFromGroupResponse,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAdminRemoveUserFromGroup' smart constructor.
data AdminRemoveUserFromGroup = AdminRemoveUserFromGroup'
  { -- | The user pool ID for the user pool.
    userPoolId :: Core.Text,
    -- | The username for the user.
    username :: Core.Sensitive Core.Text,
    -- | The group name.
    groupName :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminRemoveUserFromGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'adminRemoveUserFromGroup_userPoolId' - The user pool ID for the user pool.
--
-- 'username', 'adminRemoveUserFromGroup_username' - The username for the user.
--
-- 'groupName', 'adminRemoveUserFromGroup_groupName' - The group name.
newAdminRemoveUserFromGroup ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'username'
  Core.Text ->
  -- | 'groupName'
  Core.Text ->
  AdminRemoveUserFromGroup
newAdminRemoveUserFromGroup
  pUserPoolId_
  pUsername_
  pGroupName_ =
    AdminRemoveUserFromGroup'
      { userPoolId =
          pUserPoolId_,
        username = Core._Sensitive Lens.# pUsername_,
        groupName = pGroupName_
      }

-- | The user pool ID for the user pool.
adminRemoveUserFromGroup_userPoolId :: Lens.Lens' AdminRemoveUserFromGroup Core.Text
adminRemoveUserFromGroup_userPoolId = Lens.lens (\AdminRemoveUserFromGroup' {userPoolId} -> userPoolId) (\s@AdminRemoveUserFromGroup' {} a -> s {userPoolId = a} :: AdminRemoveUserFromGroup)

-- | The username for the user.
adminRemoveUserFromGroup_username :: Lens.Lens' AdminRemoveUserFromGroup Core.Text
adminRemoveUserFromGroup_username = Lens.lens (\AdminRemoveUserFromGroup' {username} -> username) (\s@AdminRemoveUserFromGroup' {} a -> s {username = a} :: AdminRemoveUserFromGroup) Core.. Core._Sensitive

-- | The group name.
adminRemoveUserFromGroup_groupName :: Lens.Lens' AdminRemoveUserFromGroup Core.Text
adminRemoveUserFromGroup_groupName = Lens.lens (\AdminRemoveUserFromGroup' {groupName} -> groupName) (\s@AdminRemoveUserFromGroup' {} a -> s {groupName = a} :: AdminRemoveUserFromGroup)

instance Core.AWSRequest AdminRemoveUserFromGroup where
  type
    AWSResponse AdminRemoveUserFromGroup =
      AdminRemoveUserFromGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      AdminRemoveUserFromGroupResponse'

instance Core.Hashable AdminRemoveUserFromGroup

instance Core.NFData AdminRemoveUserFromGroup

instance Core.ToHeaders AdminRemoveUserFromGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.AdminRemoveUserFromGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AdminRemoveUserFromGroup where
  toJSON AdminRemoveUserFromGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Username" Core..= username),
            Core.Just ("GroupName" Core..= groupName)
          ]
      )

instance Core.ToPath AdminRemoveUserFromGroup where
  toPath = Core.const "/"

instance Core.ToQuery AdminRemoveUserFromGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAdminRemoveUserFromGroupResponse' smart constructor.
data AdminRemoveUserFromGroupResponse = AdminRemoveUserFromGroupResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminRemoveUserFromGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAdminRemoveUserFromGroupResponse ::
  AdminRemoveUserFromGroupResponse
newAdminRemoveUserFromGroupResponse =
  AdminRemoveUserFromGroupResponse'

instance Core.NFData AdminRemoveUserFromGroupResponse
