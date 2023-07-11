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
-- Module      : Amazonka.CognitoIdentityProvider.AdminDeleteUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user as an administrator. Works on any user.
--
-- Calling this action requires developer credentials.
module Amazonka.CognitoIdentityProvider.AdminDeleteUser
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to delete a user as an administrator.
--
-- /See:/ 'newAdminDeleteUser' smart constructor.
data AdminDeleteUser = AdminDeleteUser'
  { -- | The user pool ID for the user pool where you want to delete the user.
    userPoolId :: Prelude.Text,
    -- | The user name of the user you want to delete.
    username :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- 'username', 'adminDeleteUser_username' - The user name of the user you want to delete.
newAdminDeleteUser ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  AdminDeleteUser
newAdminDeleteUser pUserPoolId_ pUsername_ =
  AdminDeleteUser'
    { userPoolId = pUserPoolId_,
      username = Data._Sensitive Lens.# pUsername_
    }

-- | The user pool ID for the user pool where you want to delete the user.
adminDeleteUser_userPoolId :: Lens.Lens' AdminDeleteUser Prelude.Text
adminDeleteUser_userPoolId = Lens.lens (\AdminDeleteUser' {userPoolId} -> userPoolId) (\s@AdminDeleteUser' {} a -> s {userPoolId = a} :: AdminDeleteUser)

-- | The user name of the user you want to delete.
adminDeleteUser_username :: Lens.Lens' AdminDeleteUser Prelude.Text
adminDeleteUser_username = Lens.lens (\AdminDeleteUser' {username} -> username) (\s@AdminDeleteUser' {} a -> s {username = a} :: AdminDeleteUser) Prelude.. Data._Sensitive

instance Core.AWSRequest AdminDeleteUser where
  type
    AWSResponse AdminDeleteUser =
      AdminDeleteUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull AdminDeleteUserResponse'

instance Prelude.Hashable AdminDeleteUser where
  hashWithSalt _salt AdminDeleteUser' {..} =
    _salt
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` username

instance Prelude.NFData AdminDeleteUser where
  rnf AdminDeleteUser' {..} =
    Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf username

instance Data.ToHeaders AdminDeleteUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.AdminDeleteUser" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AdminDeleteUser where
  toJSON AdminDeleteUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("Username" Data..= username)
          ]
      )

instance Data.ToPath AdminDeleteUser where
  toPath = Prelude.const "/"

instance Data.ToQuery AdminDeleteUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAdminDeleteUserResponse' smart constructor.
data AdminDeleteUserResponse = AdminDeleteUserResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminDeleteUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAdminDeleteUserResponse ::
  AdminDeleteUserResponse
newAdminDeleteUserResponse = AdminDeleteUserResponse'

instance Prelude.NFData AdminDeleteUserResponse where
  rnf _ = ()
