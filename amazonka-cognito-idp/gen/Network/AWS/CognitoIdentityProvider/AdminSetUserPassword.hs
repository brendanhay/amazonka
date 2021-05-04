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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminSetUserPassword
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified user\'s password in a user pool as an administrator.
-- Works on any user.
--
-- The password can be temporary or permanent. If it is temporary, the user
-- status will be placed into the @FORCE_CHANGE_PASSWORD@ state. When the
-- user next tries to sign in, the InitiateAuth\/AdminInitiateAuth response
-- will contain the @NEW_PASSWORD_REQUIRED@ challenge. If the user does not
-- sign in before it expires, the user will not be able to sign in and
-- their password will need to be reset by an administrator.
--
-- Once the user has set a new password, or the password is permanent, the
-- user status will be set to @Confirmed@.
module Network.AWS.CognitoIdentityProvider.AdminSetUserPassword
  ( -- * Creating a Request
    AdminSetUserPassword (..),
    newAdminSetUserPassword,

    -- * Request Lenses
    adminSetUserPassword_permanent,
    adminSetUserPassword_userPoolId,
    adminSetUserPassword_username,
    adminSetUserPassword_password,

    -- * Destructuring the Response
    AdminSetUserPasswordResponse (..),
    newAdminSetUserPasswordResponse,

    -- * Response Lenses
    adminSetUserPasswordResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAdminSetUserPassword' smart constructor.
data AdminSetUserPassword = AdminSetUserPassword'
  { -- | @True@ if the password is permanent, @False@ if it is temporary.
    permanent :: Prelude.Maybe Prelude.Bool,
    -- | The user pool ID for the user pool where you want to set the user\'s
    -- password.
    userPoolId :: Prelude.Text,
    -- | The user name of the user whose password you wish to set.
    username :: Prelude.Sensitive Prelude.Text,
    -- | The password for the user.
    password :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdminSetUserPassword' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permanent', 'adminSetUserPassword_permanent' - @True@ if the password is permanent, @False@ if it is temporary.
--
-- 'userPoolId', 'adminSetUserPassword_userPoolId' - The user pool ID for the user pool where you want to set the user\'s
-- password.
--
-- 'username', 'adminSetUserPassword_username' - The user name of the user whose password you wish to set.
--
-- 'password', 'adminSetUserPassword_password' - The password for the user.
newAdminSetUserPassword ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  AdminSetUserPassword
newAdminSetUserPassword
  pUserPoolId_
  pUsername_
  pPassword_ =
    AdminSetUserPassword'
      { permanent = Prelude.Nothing,
        userPoolId = pUserPoolId_,
        username = Prelude._Sensitive Lens.# pUsername_,
        password = Prelude._Sensitive Lens.# pPassword_
      }

-- | @True@ if the password is permanent, @False@ if it is temporary.
adminSetUserPassword_permanent :: Lens.Lens' AdminSetUserPassword (Prelude.Maybe Prelude.Bool)
adminSetUserPassword_permanent = Lens.lens (\AdminSetUserPassword' {permanent} -> permanent) (\s@AdminSetUserPassword' {} a -> s {permanent = a} :: AdminSetUserPassword)

-- | The user pool ID for the user pool where you want to set the user\'s
-- password.
adminSetUserPassword_userPoolId :: Lens.Lens' AdminSetUserPassword Prelude.Text
adminSetUserPassword_userPoolId = Lens.lens (\AdminSetUserPassword' {userPoolId} -> userPoolId) (\s@AdminSetUserPassword' {} a -> s {userPoolId = a} :: AdminSetUserPassword)

-- | The user name of the user whose password you wish to set.
adminSetUserPassword_username :: Lens.Lens' AdminSetUserPassword Prelude.Text
adminSetUserPassword_username = Lens.lens (\AdminSetUserPassword' {username} -> username) (\s@AdminSetUserPassword' {} a -> s {username = a} :: AdminSetUserPassword) Prelude.. Prelude._Sensitive

-- | The password for the user.
adminSetUserPassword_password :: Lens.Lens' AdminSetUserPassword Prelude.Text
adminSetUserPassword_password = Lens.lens (\AdminSetUserPassword' {password} -> password) (\s@AdminSetUserPassword' {} a -> s {password = a} :: AdminSetUserPassword) Prelude.. Prelude._Sensitive

instance Prelude.AWSRequest AdminSetUserPassword where
  type
    Rs AdminSetUserPassword =
      AdminSetUserPasswordResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminSetUserPasswordResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminSetUserPassword

instance Prelude.NFData AdminSetUserPassword

instance Prelude.ToHeaders AdminSetUserPassword where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.AdminSetUserPassword" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AdminSetUserPassword where
  toJSON AdminSetUserPassword' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Permanent" Prelude..=) Prelude.<$> permanent,
            Prelude.Just ("UserPoolId" Prelude..= userPoolId),
            Prelude.Just ("Username" Prelude..= username),
            Prelude.Just ("Password" Prelude..= password)
          ]
      )

instance Prelude.ToPath AdminSetUserPassword where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AdminSetUserPassword where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAdminSetUserPasswordResponse' smart constructor.
data AdminSetUserPasswordResponse = AdminSetUserPasswordResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdminSetUserPasswordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'adminSetUserPasswordResponse_httpStatus' - The response's http status code.
newAdminSetUserPasswordResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AdminSetUserPasswordResponse
newAdminSetUserPasswordResponse pHttpStatus_ =
  AdminSetUserPasswordResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
adminSetUserPasswordResponse_httpStatus :: Lens.Lens' AdminSetUserPasswordResponse Prelude.Int
adminSetUserPasswordResponse_httpStatus = Lens.lens (\AdminSetUserPasswordResponse' {httpStatus} -> httpStatus) (\s@AdminSetUserPasswordResponse' {} a -> s {httpStatus = a} :: AdminSetUserPasswordResponse)

instance Prelude.NFData AdminSetUserPasswordResponse
