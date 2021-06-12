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
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified group with the specified attributes.
--
-- Calling this action requires developer credentials.
--
-- If you don\'t provide a value for an attribute, it will be set to the
-- default value.
module Network.AWS.CognitoIdentityProvider.UpdateGroup
  ( -- * Creating a Request
    UpdateGroup (..),
    newUpdateGroup,

    -- * Request Lenses
    updateGroup_roleArn,
    updateGroup_description,
    updateGroup_precedence,
    updateGroup_groupName,
    updateGroup_userPoolId,

    -- * Destructuring the Response
    UpdateGroupResponse (..),
    newUpdateGroupResponse,

    -- * Response Lenses
    updateGroupResponse_group,
    updateGroupResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { -- | The new role ARN for the group. This is used for setting the
    -- @cognito:roles@ and @cognito:preferred_role@ claims in the token.
    roleArn :: Core.Maybe Core.Text,
    -- | A string containing the new description of the group.
    description :: Core.Maybe Core.Text,
    -- | The new precedence value for the group. For more information about this
    -- parameter, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_CreateGroup.html CreateGroup>.
    precedence :: Core.Maybe Core.Natural,
    -- | The name of the group.
    groupName :: Core.Text,
    -- | The user pool ID for the user pool.
    userPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'updateGroup_roleArn' - The new role ARN for the group. This is used for setting the
-- @cognito:roles@ and @cognito:preferred_role@ claims in the token.
--
-- 'description', 'updateGroup_description' - A string containing the new description of the group.
--
-- 'precedence', 'updateGroup_precedence' - The new precedence value for the group. For more information about this
-- parameter, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_CreateGroup.html CreateGroup>.
--
-- 'groupName', 'updateGroup_groupName' - The name of the group.
--
-- 'userPoolId', 'updateGroup_userPoolId' - The user pool ID for the user pool.
newUpdateGroup ::
  -- | 'groupName'
  Core.Text ->
  -- | 'userPoolId'
  Core.Text ->
  UpdateGroup
newUpdateGroup pGroupName_ pUserPoolId_ =
  UpdateGroup'
    { roleArn = Core.Nothing,
      description = Core.Nothing,
      precedence = Core.Nothing,
      groupName = pGroupName_,
      userPoolId = pUserPoolId_
    }

-- | The new role ARN for the group. This is used for setting the
-- @cognito:roles@ and @cognito:preferred_role@ claims in the token.
updateGroup_roleArn :: Lens.Lens' UpdateGroup (Core.Maybe Core.Text)
updateGroup_roleArn = Lens.lens (\UpdateGroup' {roleArn} -> roleArn) (\s@UpdateGroup' {} a -> s {roleArn = a} :: UpdateGroup)

-- | A string containing the new description of the group.
updateGroup_description :: Lens.Lens' UpdateGroup (Core.Maybe Core.Text)
updateGroup_description = Lens.lens (\UpdateGroup' {description} -> description) (\s@UpdateGroup' {} a -> s {description = a} :: UpdateGroup)

-- | The new precedence value for the group. For more information about this
-- parameter, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_CreateGroup.html CreateGroup>.
updateGroup_precedence :: Lens.Lens' UpdateGroup (Core.Maybe Core.Natural)
updateGroup_precedence = Lens.lens (\UpdateGroup' {precedence} -> precedence) (\s@UpdateGroup' {} a -> s {precedence = a} :: UpdateGroup)

-- | The name of the group.
updateGroup_groupName :: Lens.Lens' UpdateGroup Core.Text
updateGroup_groupName = Lens.lens (\UpdateGroup' {groupName} -> groupName) (\s@UpdateGroup' {} a -> s {groupName = a} :: UpdateGroup)

-- | The user pool ID for the user pool.
updateGroup_userPoolId :: Lens.Lens' UpdateGroup Core.Text
updateGroup_userPoolId = Lens.lens (\UpdateGroup' {userPoolId} -> userPoolId) (\s@UpdateGroup' {} a -> s {userPoolId = a} :: UpdateGroup)

instance Core.AWSRequest UpdateGroup where
  type AWSResponse UpdateGroup = UpdateGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGroupResponse'
            Core.<$> (x Core..?> "Group")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateGroup

instance Core.NFData UpdateGroup

instance Core.ToHeaders UpdateGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.UpdateGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateGroup where
  toJSON UpdateGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RoleArn" Core..=) Core.<$> roleArn,
            ("Description" Core..=) Core.<$> description,
            ("Precedence" Core..=) Core.<$> precedence,
            Core.Just ("GroupName" Core..= groupName),
            Core.Just ("UserPoolId" Core..= userPoolId)
          ]
      )

instance Core.ToPath UpdateGroup where
  toPath = Core.const "/"

instance Core.ToQuery UpdateGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse'
  { -- | The group object for the group.
    group' :: Core.Maybe GroupType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'group'', 'updateGroupResponse_group' - The group object for the group.
--
-- 'httpStatus', 'updateGroupResponse_httpStatus' - The response's http status code.
newUpdateGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateGroupResponse
newUpdateGroupResponse pHttpStatus_ =
  UpdateGroupResponse'
    { group' = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The group object for the group.
updateGroupResponse_group :: Lens.Lens' UpdateGroupResponse (Core.Maybe GroupType)
updateGroupResponse_group = Lens.lens (\UpdateGroupResponse' {group'} -> group') (\s@UpdateGroupResponse' {} a -> s {group' = a} :: UpdateGroupResponse)

-- | The response's http status code.
updateGroupResponse_httpStatus :: Lens.Lens' UpdateGroupResponse Core.Int
updateGroupResponse_httpStatus = Lens.lens (\UpdateGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateGroupResponse' {} a -> s {httpStatus = a} :: UpdateGroupResponse)

instance Core.NFData UpdateGroupResponse
