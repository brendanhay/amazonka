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
-- Module      : Amazonka.CognitoIdentityProvider.AdminDisableUser
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates a user and revokes all access tokens for the user. A
-- deactivated user can\'t sign in, but still appears in the responses to
-- @GetUser@ and @ListUsers@ API requests.
--
-- You must make this API request with Amazon Web Services credentials that
-- have @cognito-idp:AdminDisableUser@ permissions.
module Amazonka.CognitoIdentityProvider.AdminDisableUser
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to disable the user as an administrator.
--
-- /See:/ 'newAdminDisableUser' smart constructor.
data AdminDisableUser = AdminDisableUser'
  { -- | The user pool ID for the user pool where you want to disable the user.
    userPoolId :: Prelude.Text,
    -- | The user name of the user you want to disable.
    username :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- 'username', 'adminDisableUser_username' - The user name of the user you want to disable.
newAdminDisableUser ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  AdminDisableUser
newAdminDisableUser pUserPoolId_ pUsername_ =
  AdminDisableUser'
    { userPoolId = pUserPoolId_,
      username = Data._Sensitive Lens.# pUsername_
    }

-- | The user pool ID for the user pool where you want to disable the user.
adminDisableUser_userPoolId :: Lens.Lens' AdminDisableUser Prelude.Text
adminDisableUser_userPoolId = Lens.lens (\AdminDisableUser' {userPoolId} -> userPoolId) (\s@AdminDisableUser' {} a -> s {userPoolId = a} :: AdminDisableUser)

-- | The user name of the user you want to disable.
adminDisableUser_username :: Lens.Lens' AdminDisableUser Prelude.Text
adminDisableUser_username = Lens.lens (\AdminDisableUser' {username} -> username) (\s@AdminDisableUser' {} a -> s {username = a} :: AdminDisableUser) Prelude.. Data._Sensitive

instance Core.AWSRequest AdminDisableUser where
  type
    AWSResponse AdminDisableUser =
      AdminDisableUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminDisableUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminDisableUser where
  hashWithSalt _salt AdminDisableUser' {..} =
    _salt `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` username

instance Prelude.NFData AdminDisableUser where
  rnf AdminDisableUser' {..} =
    Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf username

instance Data.ToHeaders AdminDisableUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.AdminDisableUser" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AdminDisableUser where
  toJSON AdminDisableUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("Username" Data..= username)
          ]
      )

instance Data.ToPath AdminDisableUser where
  toPath = Prelude.const "/"

instance Data.ToQuery AdminDisableUser where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response received from the server to disable the user as
-- an administrator.
--
-- /See:/ 'newAdminDisableUserResponse' smart constructor.
data AdminDisableUserResponse = AdminDisableUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData AdminDisableUserResponse where
  rnf AdminDisableUserResponse' {..} =
    Prelude.rnf httpStatus
