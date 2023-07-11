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
-- Module      : Amazonka.CognitoIdentityProvider.UpdateGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified group with the specified attributes.
--
-- Calling this action requires developer credentials.
module Amazonka.CognitoIdentityProvider.UpdateGroup
  ( -- * Creating a Request
    UpdateGroup (..),
    newUpdateGroup,

    -- * Request Lenses
    updateGroup_description,
    updateGroup_precedence,
    updateGroup_roleArn,
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { -- | A string containing the new description of the group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The new precedence value for the group. For more information about this
    -- parameter, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_CreateGroup.html CreateGroup>.
    precedence :: Prelude.Maybe Prelude.Natural,
    -- | The new role Amazon Resource Name (ARN) for the group. This is used for
    -- setting the @cognito:roles@ and @cognito:preferred_role@ claims in the
    -- token.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the group.
    groupName :: Prelude.Text,
    -- | The user pool ID for the user pool.
    userPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateGroup_description' - A string containing the new description of the group.
--
-- 'precedence', 'updateGroup_precedence' - The new precedence value for the group. For more information about this
-- parameter, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_CreateGroup.html CreateGroup>.
--
-- 'roleArn', 'updateGroup_roleArn' - The new role Amazon Resource Name (ARN) for the group. This is used for
-- setting the @cognito:roles@ and @cognito:preferred_role@ claims in the
-- token.
--
-- 'groupName', 'updateGroup_groupName' - The name of the group.
--
-- 'userPoolId', 'updateGroup_userPoolId' - The user pool ID for the user pool.
newUpdateGroup ::
  -- | 'groupName'
  Prelude.Text ->
  -- | 'userPoolId'
  Prelude.Text ->
  UpdateGroup
newUpdateGroup pGroupName_ pUserPoolId_ =
  UpdateGroup'
    { description = Prelude.Nothing,
      precedence = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      groupName = pGroupName_,
      userPoolId = pUserPoolId_
    }

-- | A string containing the new description of the group.
updateGroup_description :: Lens.Lens' UpdateGroup (Prelude.Maybe Prelude.Text)
updateGroup_description = Lens.lens (\UpdateGroup' {description} -> description) (\s@UpdateGroup' {} a -> s {description = a} :: UpdateGroup)

-- | The new precedence value for the group. For more information about this
-- parameter, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_CreateGroup.html CreateGroup>.
updateGroup_precedence :: Lens.Lens' UpdateGroup (Prelude.Maybe Prelude.Natural)
updateGroup_precedence = Lens.lens (\UpdateGroup' {precedence} -> precedence) (\s@UpdateGroup' {} a -> s {precedence = a} :: UpdateGroup)

-- | The new role Amazon Resource Name (ARN) for the group. This is used for
-- setting the @cognito:roles@ and @cognito:preferred_role@ claims in the
-- token.
updateGroup_roleArn :: Lens.Lens' UpdateGroup (Prelude.Maybe Prelude.Text)
updateGroup_roleArn = Lens.lens (\UpdateGroup' {roleArn} -> roleArn) (\s@UpdateGroup' {} a -> s {roleArn = a} :: UpdateGroup)

-- | The name of the group.
updateGroup_groupName :: Lens.Lens' UpdateGroup Prelude.Text
updateGroup_groupName = Lens.lens (\UpdateGroup' {groupName} -> groupName) (\s@UpdateGroup' {} a -> s {groupName = a} :: UpdateGroup)

-- | The user pool ID for the user pool.
updateGroup_userPoolId :: Lens.Lens' UpdateGroup Prelude.Text
updateGroup_userPoolId = Lens.lens (\UpdateGroup' {userPoolId} -> userPoolId) (\s@UpdateGroup' {} a -> s {userPoolId = a} :: UpdateGroup)

instance Core.AWSRequest UpdateGroup where
  type AWSResponse UpdateGroup = UpdateGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGroupResponse'
            Prelude.<$> (x Data..?> "Group")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGroup where
  hashWithSalt _salt UpdateGroup' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` precedence
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` userPoolId

instance Prelude.NFData UpdateGroup where
  rnf UpdateGroup' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf precedence
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf userPoolId

instance Data.ToHeaders UpdateGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.UpdateGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateGroup where
  toJSON UpdateGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Precedence" Data..=) Prelude.<$> precedence,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            Prelude.Just ("GroupName" Data..= groupName),
            Prelude.Just ("UserPoolId" Data..= userPoolId)
          ]
      )

instance Data.ToPath UpdateGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse'
  { -- | The group object for the group.
    group' :: Prelude.Maybe GroupType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateGroupResponse
newUpdateGroupResponse pHttpStatus_ =
  UpdateGroupResponse'
    { group' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The group object for the group.
updateGroupResponse_group :: Lens.Lens' UpdateGroupResponse (Prelude.Maybe GroupType)
updateGroupResponse_group = Lens.lens (\UpdateGroupResponse' {group'} -> group') (\s@UpdateGroupResponse' {} a -> s {group' = a} :: UpdateGroupResponse)

-- | The response's http status code.
updateGroupResponse_httpStatus :: Lens.Lens' UpdateGroupResponse Prelude.Int
updateGroupResponse_httpStatus = Lens.lens (\UpdateGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateGroupResponse' {} a -> s {httpStatus = a} :: UpdateGroupResponse)

instance Prelude.NFData UpdateGroupResponse where
  rnf UpdateGroupResponse' {..} =
    Prelude.rnf group'
      `Prelude.seq` Prelude.rnf httpStatus
