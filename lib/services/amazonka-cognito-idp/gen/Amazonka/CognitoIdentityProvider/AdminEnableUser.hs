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
-- Module      : Amazonka.CognitoIdentityProvider.AdminEnableUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified user as an administrator. Works on any user.
--
-- Calling this action requires developer credentials.
module Amazonka.CognitoIdentityProvider.AdminEnableUser
  ( -- * Creating a Request
    AdminEnableUser (..),
    newAdminEnableUser,

    -- * Request Lenses
    adminEnableUser_userPoolId,
    adminEnableUser_username,

    -- * Destructuring the Response
    AdminEnableUserResponse (..),
    newAdminEnableUserResponse,

    -- * Response Lenses
    adminEnableUserResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request that enables the user as an administrator.
--
-- /See:/ 'newAdminEnableUser' smart constructor.
data AdminEnableUser = AdminEnableUser'
  { -- | The user pool ID for the user pool where you want to enable the user.
    userPoolId :: Prelude.Text,
    -- | The user name of the user you want to enable.
    username :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminEnableUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'adminEnableUser_userPoolId' - The user pool ID for the user pool where you want to enable the user.
--
-- 'username', 'adminEnableUser_username' - The user name of the user you want to enable.
newAdminEnableUser ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  AdminEnableUser
newAdminEnableUser pUserPoolId_ pUsername_ =
  AdminEnableUser'
    { userPoolId = pUserPoolId_,
      username = Data._Sensitive Lens.# pUsername_
    }

-- | The user pool ID for the user pool where you want to enable the user.
adminEnableUser_userPoolId :: Lens.Lens' AdminEnableUser Prelude.Text
adminEnableUser_userPoolId = Lens.lens (\AdminEnableUser' {userPoolId} -> userPoolId) (\s@AdminEnableUser' {} a -> s {userPoolId = a} :: AdminEnableUser)

-- | The user name of the user you want to enable.
adminEnableUser_username :: Lens.Lens' AdminEnableUser Prelude.Text
adminEnableUser_username = Lens.lens (\AdminEnableUser' {username} -> username) (\s@AdminEnableUser' {} a -> s {username = a} :: AdminEnableUser) Prelude.. Data._Sensitive

instance Core.AWSRequest AdminEnableUser where
  type
    AWSResponse AdminEnableUser =
      AdminEnableUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminEnableUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminEnableUser where
  hashWithSalt _salt AdminEnableUser' {..} =
    _salt `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` username

instance Prelude.NFData AdminEnableUser where
  rnf AdminEnableUser' {..} =
    Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf username

instance Data.ToHeaders AdminEnableUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.AdminEnableUser" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AdminEnableUser where
  toJSON AdminEnableUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("Username" Data..= username)
          ]
      )

instance Data.ToPath AdminEnableUser where
  toPath = Prelude.const "/"

instance Data.ToQuery AdminEnableUser where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server for the request to enable a user
-- as an administrator.
--
-- /See:/ 'newAdminEnableUserResponse' smart constructor.
data AdminEnableUserResponse = AdminEnableUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminEnableUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'adminEnableUserResponse_httpStatus' - The response's http status code.
newAdminEnableUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AdminEnableUserResponse
newAdminEnableUserResponse pHttpStatus_ =
  AdminEnableUserResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
adminEnableUserResponse_httpStatus :: Lens.Lens' AdminEnableUserResponse Prelude.Int
adminEnableUserResponse_httpStatus = Lens.lens (\AdminEnableUserResponse' {httpStatus} -> httpStatus) (\s@AdminEnableUserResponse' {} a -> s {httpStatus = a} :: AdminEnableUserResponse)

instance Prelude.NFData AdminEnableUserResponse where
  rnf AdminEnableUserResponse' {..} =
    Prelude.rnf httpStatus
