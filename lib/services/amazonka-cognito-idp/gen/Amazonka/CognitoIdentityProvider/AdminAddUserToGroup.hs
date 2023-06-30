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
-- Module      : Amazonka.CognitoIdentityProvider.AdminAddUserToGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified user to the specified group.
--
-- Calling this action requires developer credentials.
module Amazonka.CognitoIdentityProvider.AdminAddUserToGroup
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAdminAddUserToGroup' smart constructor.
data AdminAddUserToGroup = AdminAddUserToGroup'
  { -- | The user pool ID for the user pool.
    userPoolId :: Prelude.Text,
    -- | The username for the user.
    username :: Data.Sensitive Prelude.Text,
    -- | The group name.
    groupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  -- | 'groupName'
  Prelude.Text ->
  AdminAddUserToGroup
newAdminAddUserToGroup
  pUserPoolId_
  pUsername_
  pGroupName_ =
    AdminAddUserToGroup'
      { userPoolId = pUserPoolId_,
        username = Data._Sensitive Lens.# pUsername_,
        groupName = pGroupName_
      }

-- | The user pool ID for the user pool.
adminAddUserToGroup_userPoolId :: Lens.Lens' AdminAddUserToGroup Prelude.Text
adminAddUserToGroup_userPoolId = Lens.lens (\AdminAddUserToGroup' {userPoolId} -> userPoolId) (\s@AdminAddUserToGroup' {} a -> s {userPoolId = a} :: AdminAddUserToGroup)

-- | The username for the user.
adminAddUserToGroup_username :: Lens.Lens' AdminAddUserToGroup Prelude.Text
adminAddUserToGroup_username = Lens.lens (\AdminAddUserToGroup' {username} -> username) (\s@AdminAddUserToGroup' {} a -> s {username = a} :: AdminAddUserToGroup) Prelude.. Data._Sensitive

-- | The group name.
adminAddUserToGroup_groupName :: Lens.Lens' AdminAddUserToGroup Prelude.Text
adminAddUserToGroup_groupName = Lens.lens (\AdminAddUserToGroup' {groupName} -> groupName) (\s@AdminAddUserToGroup' {} a -> s {groupName = a} :: AdminAddUserToGroup)

instance Core.AWSRequest AdminAddUserToGroup where
  type
    AWSResponse AdminAddUserToGroup =
      AdminAddUserToGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull AdminAddUserToGroupResponse'

instance Prelude.Hashable AdminAddUserToGroup where
  hashWithSalt _salt AdminAddUserToGroup' {..} =
    _salt
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` groupName

instance Prelude.NFData AdminAddUserToGroup where
  rnf AdminAddUserToGroup' {..} =
    Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf groupName

instance Data.ToHeaders AdminAddUserToGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.AdminAddUserToGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AdminAddUserToGroup where
  toJSON AdminAddUserToGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("Username" Data..= username),
            Prelude.Just ("GroupName" Data..= groupName)
          ]
      )

instance Data.ToPath AdminAddUserToGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery AdminAddUserToGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAdminAddUserToGroupResponse' smart constructor.
data AdminAddUserToGroupResponse = AdminAddUserToGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminAddUserToGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAdminAddUserToGroupResponse ::
  AdminAddUserToGroupResponse
newAdminAddUserToGroupResponse =
  AdminAddUserToGroupResponse'

instance Prelude.NFData AdminAddUserToGroupResponse where
  rnf _ = ()
