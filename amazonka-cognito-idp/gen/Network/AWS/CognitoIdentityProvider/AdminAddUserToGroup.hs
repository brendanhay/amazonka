{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAdminAddUserToGroup' smart constructor.
data AdminAddUserToGroup = AdminAddUserToGroup'
  { -- | The user pool ID for the user pool.
    userPoolId :: Prelude.Text,
    -- | The username for the user.
    username :: Prelude.Sensitive Prelude.Text,
    -- | The group name.
    groupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        username = Prelude._Sensitive Lens.# pUsername_,
        groupName = pGroupName_
      }

-- | The user pool ID for the user pool.
adminAddUserToGroup_userPoolId :: Lens.Lens' AdminAddUserToGroup Prelude.Text
adminAddUserToGroup_userPoolId = Lens.lens (\AdminAddUserToGroup' {userPoolId} -> userPoolId) (\s@AdminAddUserToGroup' {} a -> s {userPoolId = a} :: AdminAddUserToGroup)

-- | The username for the user.
adminAddUserToGroup_username :: Lens.Lens' AdminAddUserToGroup Prelude.Text
adminAddUserToGroup_username = Lens.lens (\AdminAddUserToGroup' {username} -> username) (\s@AdminAddUserToGroup' {} a -> s {username = a} :: AdminAddUserToGroup) Prelude.. Prelude._Sensitive

-- | The group name.
adminAddUserToGroup_groupName :: Lens.Lens' AdminAddUserToGroup Prelude.Text
adminAddUserToGroup_groupName = Lens.lens (\AdminAddUserToGroup' {groupName} -> groupName) (\s@AdminAddUserToGroup' {} a -> s {groupName = a} :: AdminAddUserToGroup)

instance Prelude.AWSRequest AdminAddUserToGroup where
  type
    Rs AdminAddUserToGroup =
      AdminAddUserToGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull AdminAddUserToGroupResponse'

instance Prelude.Hashable AdminAddUserToGroup

instance Prelude.NFData AdminAddUserToGroup

instance Prelude.ToHeaders AdminAddUserToGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.AdminAddUserToGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AdminAddUserToGroup where
  toJSON AdminAddUserToGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Prelude..= userPoolId),
            Prelude.Just ("Username" Prelude..= username),
            Prelude.Just ("GroupName" Prelude..= groupName)
          ]
      )

instance Prelude.ToPath AdminAddUserToGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AdminAddUserToGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAdminAddUserToGroupResponse' smart constructor.
data AdminAddUserToGroupResponse = AdminAddUserToGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdminAddUserToGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAdminAddUserToGroupResponse ::
  AdminAddUserToGroupResponse
newAdminAddUserToGroupResponse =
  AdminAddUserToGroupResponse'

instance Prelude.NFData AdminAddUserToGroupResponse
