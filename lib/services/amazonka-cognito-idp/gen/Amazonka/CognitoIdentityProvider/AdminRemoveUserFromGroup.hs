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
-- Module      : Amazonka.CognitoIdentityProvider.AdminRemoveUserFromGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified user from the specified group.
--
-- Calling this action requires developer credentials.
module Amazonka.CognitoIdentityProvider.AdminRemoveUserFromGroup
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAdminRemoveUserFromGroup' smart constructor.
data AdminRemoveUserFromGroup = AdminRemoveUserFromGroup'
  { -- | The user pool ID for the user pool.
    userPoolId :: Prelude.Text,
    -- | The username for the user.
    username :: Data.Sensitive Prelude.Text,
    -- | The group name.
    groupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  -- | 'groupName'
  Prelude.Text ->
  AdminRemoveUserFromGroup
newAdminRemoveUserFromGroup
  pUserPoolId_
  pUsername_
  pGroupName_ =
    AdminRemoveUserFromGroup'
      { userPoolId =
          pUserPoolId_,
        username = Data._Sensitive Lens.# pUsername_,
        groupName = pGroupName_
      }

-- | The user pool ID for the user pool.
adminRemoveUserFromGroup_userPoolId :: Lens.Lens' AdminRemoveUserFromGroup Prelude.Text
adminRemoveUserFromGroup_userPoolId = Lens.lens (\AdminRemoveUserFromGroup' {userPoolId} -> userPoolId) (\s@AdminRemoveUserFromGroup' {} a -> s {userPoolId = a} :: AdminRemoveUserFromGroup)

-- | The username for the user.
adminRemoveUserFromGroup_username :: Lens.Lens' AdminRemoveUserFromGroup Prelude.Text
adminRemoveUserFromGroup_username = Lens.lens (\AdminRemoveUserFromGroup' {username} -> username) (\s@AdminRemoveUserFromGroup' {} a -> s {username = a} :: AdminRemoveUserFromGroup) Prelude.. Data._Sensitive

-- | The group name.
adminRemoveUserFromGroup_groupName :: Lens.Lens' AdminRemoveUserFromGroup Prelude.Text
adminRemoveUserFromGroup_groupName = Lens.lens (\AdminRemoveUserFromGroup' {groupName} -> groupName) (\s@AdminRemoveUserFromGroup' {} a -> s {groupName = a} :: AdminRemoveUserFromGroup)

instance Core.AWSRequest AdminRemoveUserFromGroup where
  type
    AWSResponse AdminRemoveUserFromGroup =
      AdminRemoveUserFromGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      AdminRemoveUserFromGroupResponse'

instance Prelude.Hashable AdminRemoveUserFromGroup where
  hashWithSalt _salt AdminRemoveUserFromGroup' {..} =
    _salt
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` groupName

instance Prelude.NFData AdminRemoveUserFromGroup where
  rnf AdminRemoveUserFromGroup' {..} =
    Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf groupName

instance Data.ToHeaders AdminRemoveUserFromGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.AdminRemoveUserFromGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AdminRemoveUserFromGroup where
  toJSON AdminRemoveUserFromGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("Username" Data..= username),
            Prelude.Just ("GroupName" Data..= groupName)
          ]
      )

instance Data.ToPath AdminRemoveUserFromGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery AdminRemoveUserFromGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAdminRemoveUserFromGroupResponse' smart constructor.
data AdminRemoveUserFromGroupResponse = AdminRemoveUserFromGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminRemoveUserFromGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAdminRemoveUserFromGroupResponse ::
  AdminRemoveUserFromGroupResponse
newAdminRemoveUserFromGroupResponse =
  AdminRemoveUserFromGroupResponse'

instance
  Prelude.NFData
    AdminRemoveUserFromGroupResponse
  where
  rnf _ = ()
