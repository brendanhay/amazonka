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
-- Module      : Amazonka.CognitoIdentityProvider.CreateGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new group in the specified user pool.
--
-- Calling this action requires developer credentials.
module Amazonka.CognitoIdentityProvider.CreateGroup
  ( -- * Creating a Request
    CreateGroup (..),
    newCreateGroup,

    -- * Request Lenses
    createGroup_description,
    createGroup_precedence,
    createGroup_roleArn,
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { -- | A string containing the description of the group.
    description :: Prelude.Maybe Prelude.Text,
    -- | A non-negative integer value that specifies the precedence of this group
    -- relative to the other groups that a user can belong to in the user pool.
    -- Zero is the highest precedence value. Groups with lower @Precedence@
    -- values take precedence over groups with higher or null @Precedence@
    -- values. If a user belongs to two or more groups, it is the group with
    -- the lowest precedence value whose role ARN is given in the user\'s
    -- tokens for the @cognito:roles@ and @cognito:preferred_role@ claims.
    --
    -- Two groups can have the same @Precedence@ value. If this happens,
    -- neither group takes precedence over the other. If two groups with the
    -- same @Precedence@ have the same role ARN, that role is used in the
    -- @cognito:preferred_role@ claim in tokens for users in each group. If the
    -- two groups have different role ARNs, the @cognito:preferred_role@ claim
    -- isn\'t set in users\' tokens.
    --
    -- The default @Precedence@ value is null. The maximum @Precedence@ value
    -- is @2^31-1@.
    precedence :: Prelude.Maybe Prelude.Natural,
    -- | The role Amazon Resource Name (ARN) for the group.
    roleArn :: Prelude.Maybe Prelude.Text,
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
-- 'description', 'createGroup_description' - A string containing the description of the group.
--
-- 'precedence', 'createGroup_precedence' - A non-negative integer value that specifies the precedence of this group
-- relative to the other groups that a user can belong to in the user pool.
-- Zero is the highest precedence value. Groups with lower @Precedence@
-- values take precedence over groups with higher or null @Precedence@
-- values. If a user belongs to two or more groups, it is the group with
-- the lowest precedence value whose role ARN is given in the user\'s
-- tokens for the @cognito:roles@ and @cognito:preferred_role@ claims.
--
-- Two groups can have the same @Precedence@ value. If this happens,
-- neither group takes precedence over the other. If two groups with the
-- same @Precedence@ have the same role ARN, that role is used in the
-- @cognito:preferred_role@ claim in tokens for users in each group. If the
-- two groups have different role ARNs, the @cognito:preferred_role@ claim
-- isn\'t set in users\' tokens.
--
-- The default @Precedence@ value is null. The maximum @Precedence@ value
-- is @2^31-1@.
--
-- 'roleArn', 'createGroup_roleArn' - The role Amazon Resource Name (ARN) for the group.
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
    { description = Prelude.Nothing,
      precedence = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      groupName = pGroupName_,
      userPoolId = pUserPoolId_
    }

-- | A string containing the description of the group.
createGroup_description :: Lens.Lens' CreateGroup (Prelude.Maybe Prelude.Text)
createGroup_description = Lens.lens (\CreateGroup' {description} -> description) (\s@CreateGroup' {} a -> s {description = a} :: CreateGroup)

-- | A non-negative integer value that specifies the precedence of this group
-- relative to the other groups that a user can belong to in the user pool.
-- Zero is the highest precedence value. Groups with lower @Precedence@
-- values take precedence over groups with higher or null @Precedence@
-- values. If a user belongs to two or more groups, it is the group with
-- the lowest precedence value whose role ARN is given in the user\'s
-- tokens for the @cognito:roles@ and @cognito:preferred_role@ claims.
--
-- Two groups can have the same @Precedence@ value. If this happens,
-- neither group takes precedence over the other. If two groups with the
-- same @Precedence@ have the same role ARN, that role is used in the
-- @cognito:preferred_role@ claim in tokens for users in each group. If the
-- two groups have different role ARNs, the @cognito:preferred_role@ claim
-- isn\'t set in users\' tokens.
--
-- The default @Precedence@ value is null. The maximum @Precedence@ value
-- is @2^31-1@.
createGroup_precedence :: Lens.Lens' CreateGroup (Prelude.Maybe Prelude.Natural)
createGroup_precedence = Lens.lens (\CreateGroup' {precedence} -> precedence) (\s@CreateGroup' {} a -> s {precedence = a} :: CreateGroup)

-- | The role Amazon Resource Name (ARN) for the group.
createGroup_roleArn :: Lens.Lens' CreateGroup (Prelude.Maybe Prelude.Text)
createGroup_roleArn = Lens.lens (\CreateGroup' {roleArn} -> roleArn) (\s@CreateGroup' {} a -> s {roleArn = a} :: CreateGroup)

-- | The name of the group. Must be unique.
createGroup_groupName :: Lens.Lens' CreateGroup Prelude.Text
createGroup_groupName = Lens.lens (\CreateGroup' {groupName} -> groupName) (\s@CreateGroup' {} a -> s {groupName = a} :: CreateGroup)

-- | The user pool ID for the user pool.
createGroup_userPoolId :: Lens.Lens' CreateGroup Prelude.Text
createGroup_userPoolId = Lens.lens (\CreateGroup' {userPoolId} -> userPoolId) (\s@CreateGroup' {} a -> s {userPoolId = a} :: CreateGroup)

instance Core.AWSRequest CreateGroup where
  type AWSResponse CreateGroup = CreateGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGroupResponse'
            Prelude.<$> (x Data..?> "Group")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGroup where
  hashWithSalt _salt CreateGroup' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` precedence
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` userPoolId

instance Prelude.NFData CreateGroup where
  rnf CreateGroup' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf precedence
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf userPoolId

instance Data.ToHeaders CreateGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.CreateGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateGroup where
  toJSON CreateGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Precedence" Data..=) Prelude.<$> precedence,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            Prelude.Just ("GroupName" Data..= groupName),
            Prelude.Just ("UserPoolId" Data..= userPoolId)
          ]
      )

instance Data.ToPath CreateGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateGroup where
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

instance Prelude.NFData CreateGroupResponse where
  rnf CreateGroupResponse' {..} =
    Prelude.rnf group'
      `Prelude.seq` Prelude.rnf httpStatus
