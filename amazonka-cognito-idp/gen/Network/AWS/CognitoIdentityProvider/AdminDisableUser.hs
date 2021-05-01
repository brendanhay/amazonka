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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminDisableUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified user.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminDisableUser
  ( -- * Creating a Request
    AdminDisableUser (..),
    newAdminDisableUser,

    -- * Request Lenses
    adminDisableUser_userPoolId,
    adminDisableUser_username,

    -- * Destructuring the Response
    AdminDisableUserResponse (..),
    newAdminDisableUserResponse,

    -- * Response Lenses
    adminDisableUserResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to disable any user as an administrator.
--
-- /See:/ 'newAdminDisableUser' smart constructor.
data AdminDisableUser = AdminDisableUser'
  { -- | The user pool ID for the user pool where you want to disable the user.
    userPoolId :: Prelude.Text,
    -- | The user name of the user you wish to disable.
    username :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdminDisableUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'adminDisableUser_userPoolId' - The user pool ID for the user pool where you want to disable the user.
--
-- 'username', 'adminDisableUser_username' - The user name of the user you wish to disable.
newAdminDisableUser ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  AdminDisableUser
newAdminDisableUser pUserPoolId_ pUsername_ =
  AdminDisableUser'
    { userPoolId = pUserPoolId_,
      username = Prelude._Sensitive Lens.# pUsername_
    }

-- | The user pool ID for the user pool where you want to disable the user.
adminDisableUser_userPoolId :: Lens.Lens' AdminDisableUser Prelude.Text
adminDisableUser_userPoolId = Lens.lens (\AdminDisableUser' {userPoolId} -> userPoolId) (\s@AdminDisableUser' {} a -> s {userPoolId = a} :: AdminDisableUser)

-- | The user name of the user you wish to disable.
adminDisableUser_username :: Lens.Lens' AdminDisableUser Prelude.Text
adminDisableUser_username = Lens.lens (\AdminDisableUser' {username} -> username) (\s@AdminDisableUser' {} a -> s {username = a} :: AdminDisableUser) Prelude.. Prelude._Sensitive

instance Prelude.AWSRequest AdminDisableUser where
  type Rs AdminDisableUser = AdminDisableUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminDisableUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminDisableUser

instance Prelude.NFData AdminDisableUser

instance Prelude.ToHeaders AdminDisableUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.AdminDisableUser" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AdminDisableUser where
  toJSON AdminDisableUser' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Prelude..= userPoolId),
            Prelude.Just ("Username" Prelude..= username)
          ]
      )

instance Prelude.ToPath AdminDisableUser where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AdminDisableUser where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response received from the server to disable the user as
-- an administrator.
--
-- /See:/ 'newAdminDisableUserResponse' smart constructor.
data AdminDisableUserResponse = AdminDisableUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdminDisableUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'adminDisableUserResponse_httpStatus' - The response's http status code.
newAdminDisableUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AdminDisableUserResponse
newAdminDisableUserResponse pHttpStatus_ =
  AdminDisableUserResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
adminDisableUserResponse_httpStatus :: Lens.Lens' AdminDisableUserResponse Prelude.Int
adminDisableUserResponse_httpStatus = Lens.lens (\AdminDisableUserResponse' {httpStatus} -> httpStatus) (\s@AdminDisableUserResponse' {} a -> s {httpStatus = a} :: AdminDisableUserResponse)

instance Prelude.NFData AdminDisableUserResponse
