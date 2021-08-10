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
-- Module      : Network.AWS.CognitoIdentityProvider.CreateGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new group in the specified user pool.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.CreateGroup
  ( -- * Creating a Request
    CreateGroup (..),
    newCreateGroup,

    -- * Request Lenses
    createGroup_roleArn,
    createGroup_description,
    createGroup_precedence,
    createGroup_groupName,
    createGroup_userPoolId,

    -- * Destructuring the Response
    CreateGroupResponse (..),
    newCreateGroupResponse,

    -- * Response Lenses
    createGroupResponse_group,
    createGroupResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { -- | The role ARN for the group.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | A string containing the description of the group.
    description :: Prelude.Maybe Prelude.Text,
    -- | A nonnegative integer value that specifies the precedence of this group
    -- relative to the other groups that a user can belong to in the user pool.
    -- Zero is the highest precedence value. Groups with lower @Precedence@
    -- values take precedence over groups with higher or null @Precedence@
    -- values. If a user belongs to two or more groups, it is the group with
    -- the lowest precedence value whose role ARN will be used in the
    -- @cognito:roles@ and @cognito:preferred_role@ claims in the user\'s
    -- tokens.
    --
    -- Two groups can have the same @Precedence@ value. If this happens,
    -- neither group takes precedence over the other. If two groups with the
    -- same @Precedence@ have the same role ARN, that role is used in the
    -- @cognito:preferred_role@ claim in tokens for users in each group. If the
    -- two groups have different role ARNs, the @cognito:preferred_role@ claim
    -- is not set in users\' tokens.
    --
    -- The default @Precedence@ value is null.
    precedence :: Prelude.Maybe Prelude.Natural,
    -- | The name of the group. Must be unique.
    groupName :: Prelude.Text,
    -- | The user pool ID for the user pool.
    userPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'createGroup_roleArn' - The role ARN for the group.
--
-- 'description', 'createGroup_description' - A string containing the description of the group.
--
-- 'precedence', 'createGroup_precedence' - A nonnegative integer value that specifies the precedence of this group
-- relative to the other groups that a user can belong to in the user pool.
-- Zero is the highest precedence value. Groups with lower @Precedence@
-- values take precedence over groups with higher or null @Precedence@
-- values. If a user belongs to two or more groups, it is the group with
-- the lowest precedence value whose role ARN will be used in the
-- @cognito:roles@ and @cognito:preferred_role@ claims in the user\'s
-- tokens.
--
-- Two groups can have the same @Precedence@ value. If this happens,
-- neither group takes precedence over the other. If two groups with the
-- same @Precedence@ have the same role ARN, that role is used in the
-- @cognito:preferred_role@ claim in tokens for users in each group. If the
-- two groups have different role ARNs, the @cognito:preferred_role@ claim
-- is not set in users\' tokens.
--
-- The default @Precedence@ value is null.
--
-- 'groupName', 'createGroup_groupName' - The name of the group. Must be unique.
--
-- 'userPoolId', 'createGroup_userPoolId' - The user pool ID for the user pool.
newCreateGroup ::
  -- | 'groupName'
  Prelude.Text ->
  -- | 'userPoolId'
  Prelude.Text ->
  CreateGroup
newCreateGroup pGroupName_ pUserPoolId_ =
  CreateGroup'
    { roleArn = Prelude.Nothing,
      description = Prelude.Nothing,
      precedence = Prelude.Nothing,
      groupName = pGroupName_,
      userPoolId = pUserPoolId_
    }

-- | The role ARN for the group.
createGroup_roleArn :: Lens.Lens' CreateGroup (Prelude.Maybe Prelude.Text)
createGroup_roleArn = Lens.lens (\CreateGroup' {roleArn} -> roleArn) (\s@CreateGroup' {} a -> s {roleArn = a} :: CreateGroup)

-- | A string containing the description of the group.
createGroup_description :: Lens.Lens' CreateGroup (Prelude.Maybe Prelude.Text)
createGroup_description = Lens.lens (\CreateGroup' {description} -> description) (\s@CreateGroup' {} a -> s {description = a} :: CreateGroup)

-- | A nonnegative integer value that specifies the precedence of this group
-- relative to the other groups that a user can belong to in the user pool.
-- Zero is the highest precedence value. Groups with lower @Precedence@
-- values take precedence over groups with higher or null @Precedence@
-- values. If a user belongs to two or more groups, it is the group with
-- the lowest precedence value whose role ARN will be used in the
-- @cognito:roles@ and @cognito:preferred_role@ claims in the user\'s
-- tokens.
--
-- Two groups can have the same @Precedence@ value. If this happens,
-- neither group takes precedence over the other. If two groups with the
-- same @Precedence@ have the same role ARN, that role is used in the
-- @cognito:preferred_role@ claim in tokens for users in each group. If the
-- two groups have different role ARNs, the @cognito:preferred_role@ claim
-- is not set in users\' tokens.
--
-- The default @Precedence@ value is null.
createGroup_precedence :: Lens.Lens' CreateGroup (Prelude.Maybe Prelude.Natural)
createGroup_precedence = Lens.lens (\CreateGroup' {precedence} -> precedence) (\s@CreateGroup' {} a -> s {precedence = a} :: CreateGroup)

-- | The name of the group. Must be unique.
createGroup_groupName :: Lens.Lens' CreateGroup Prelude.Text
createGroup_groupName = Lens.lens (\CreateGroup' {groupName} -> groupName) (\s@CreateGroup' {} a -> s {groupName = a} :: CreateGroup)

-- | The user pool ID for the user pool.
createGroup_userPoolId :: Lens.Lens' CreateGroup Prelude.Text
createGroup_userPoolId = Lens.lens (\CreateGroup' {userPoolId} -> userPoolId) (\s@CreateGroup' {} a -> s {userPoolId = a} :: CreateGroup)

instance Core.AWSRequest CreateGroup where
  type AWSResponse CreateGroup = CreateGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGroupResponse'
            Prelude.<$> (x Core..?> "Group")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGroup

instance Prelude.NFData CreateGroup

instance Core.ToHeaders CreateGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.CreateGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateGroup where
  toJSON CreateGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RoleArn" Core..=) Prelude.<$> roleArn,
            ("Description" Core..=) Prelude.<$> description,
            ("Precedence" Core..=) Prelude.<$> precedence,
            Prelude.Just ("GroupName" Core..= groupName),
            Prelude.Just ("UserPoolId" Core..= userPoolId)
          ]
      )

instance Core.ToPath CreateGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { -- | The group object for the group.
    group' :: Prelude.Maybe GroupType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'group'', 'createGroupResponse_group' - The group object for the group.
--
-- 'httpStatus', 'createGroupResponse_httpStatus' - The response's http status code.
newCreateGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGroupResponse
newCreateGroupResponse pHttpStatus_ =
  CreateGroupResponse'
    { group' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The group object for the group.
createGroupResponse_group :: Lens.Lens' CreateGroupResponse (Prelude.Maybe GroupType)
createGroupResponse_group = Lens.lens (\CreateGroupResponse' {group'} -> group') (\s@CreateGroupResponse' {} a -> s {group' = a} :: CreateGroupResponse)

-- | The response's http status code.
createGroupResponse_httpStatus :: Lens.Lens' CreateGroupResponse Prelude.Int
createGroupResponse_httpStatus = Lens.lens (\CreateGroupResponse' {httpStatus} -> httpStatus) (\s@CreateGroupResponse' {} a -> s {httpStatus = a} :: CreateGroupResponse)

instance Prelude.NFData CreateGroupResponse
