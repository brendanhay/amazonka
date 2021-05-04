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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminDeleteUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user as an administrator. Works on any user.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminDeleteUser
  ( -- * Creating a Request
    AdminDeleteUser (..),
    newAdminDeleteUser,

    -- * Request Lenses
    adminDeleteUser_userPoolId,
    adminDeleteUser_username,

    -- * Destructuring the Response
    AdminDeleteUserResponse (..),
    newAdminDeleteUserResponse,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to delete a user as an administrator.
--
-- /See:/ 'newAdminDeleteUser' smart constructor.
data AdminDeleteUser = AdminDeleteUser'
  { -- | The user pool ID for the user pool where you want to delete the user.
    userPoolId :: Prelude.Text,
    -- | The user name of the user you wish to delete.
    username :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdminDeleteUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'adminDeleteUser_userPoolId' - The user pool ID for the user pool where you want to delete the user.
--
-- 'username', 'adminDeleteUser_username' - The user name of the user you wish to delete.
newAdminDeleteUser ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  AdminDeleteUser
newAdminDeleteUser pUserPoolId_ pUsername_ =
  AdminDeleteUser'
    { userPoolId = pUserPoolId_,
      username = Prelude._Sensitive Lens.# pUsername_
    }

-- | The user pool ID for the user pool where you want to delete the user.
adminDeleteUser_userPoolId :: Lens.Lens' AdminDeleteUser Prelude.Text
adminDeleteUser_userPoolId = Lens.lens (\AdminDeleteUser' {userPoolId} -> userPoolId) (\s@AdminDeleteUser' {} a -> s {userPoolId = a} :: AdminDeleteUser)

-- | The user name of the user you wish to delete.
adminDeleteUser_username :: Lens.Lens' AdminDeleteUser Prelude.Text
adminDeleteUser_username = Lens.lens (\AdminDeleteUser' {username} -> username) (\s@AdminDeleteUser' {} a -> s {username = a} :: AdminDeleteUser) Prelude.. Prelude._Sensitive

instance Prelude.AWSRequest AdminDeleteUser where
  type Rs AdminDeleteUser = AdminDeleteUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull AdminDeleteUserResponse'

instance Prelude.Hashable AdminDeleteUser

instance Prelude.NFData AdminDeleteUser

instance Prelude.ToHeaders AdminDeleteUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.AdminDeleteUser" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AdminDeleteUser where
  toJSON AdminDeleteUser' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Prelude..= userPoolId),
            Prelude.Just ("Username" Prelude..= username)
          ]
      )

instance Prelude.ToPath AdminDeleteUser where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AdminDeleteUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAdminDeleteUserResponse' smart constructor.
data AdminDeleteUserResponse = AdminDeleteUserResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdminDeleteUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAdminDeleteUserResponse ::
  AdminDeleteUserResponse
newAdminDeleteUserResponse = AdminDeleteUserResponse'

instance Prelude.NFData AdminDeleteUserResponse
