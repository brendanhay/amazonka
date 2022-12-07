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
-- Module      : Amazonka.FinSpaceData.UpdateUser
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the details of the specified user account. You cannot update
-- the @userId@ for a user.
module Amazonka.FinSpaceData.UpdateUser
  ( -- * Creating a Request
    UpdateUser (..),
    newUpdateUser,

    -- * Request Lenses
    updateUser_clientToken,
    updateUser_type,
    updateUser_firstName,
    updateUser_lastName,
    updateUser_apiAccess,
    updateUser_apiAccessPrincipalArn,
    updateUser_userId,

    -- * Destructuring the Response
    UpdateUserResponse (..),
    newUpdateUserResponse,

    -- * Response Lenses
    updateUserResponse_userId,
    updateUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateUser' smart constructor.
data UpdateUser = UpdateUser'
  { -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The option to indicate the type of user.
    --
    -- -   @SUPER_USER@– A user with permission to all the functionality and
    --     data in FinSpace.
    --
    -- -   @APP_USER@ – A user with specific permissions in FinSpace. The users
    --     are assigned permissions by adding them to a permission group.
    type' :: Prelude.Maybe UserType,
    -- | The first name of the user.
    firstName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The last name of the user.
    lastName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The option to indicate whether the user can use the
    -- @GetProgrammaticAccessCredentials@ API to obtain credentials that can
    -- then be used to access other FinSpace Data API operations.
    --
    -- -   @ENABLED@ – The user has permissions to use the APIs.
    --
    -- -   @DISABLED@ – The user does not have permissions to use any APIs.
    apiAccess :: Prelude.Maybe ApiAccess,
    -- | The ARN identifier of an AWS user or role that is allowed to call the
    -- @GetProgrammaticAccessCredentials@ API to obtain a credentials token for
    -- a specific FinSpace user. This must be an IAM role within your FinSpace
    -- account.
    apiAccessPrincipalArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the user account to update.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateUser_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'type'', 'updateUser_type' - The option to indicate the type of user.
--
-- -   @SUPER_USER@– A user with permission to all the functionality and
--     data in FinSpace.
--
-- -   @APP_USER@ – A user with specific permissions in FinSpace. The users
--     are assigned permissions by adding them to a permission group.
--
-- 'firstName', 'updateUser_firstName' - The first name of the user.
--
-- 'lastName', 'updateUser_lastName' - The last name of the user.
--
-- 'apiAccess', 'updateUser_apiAccess' - The option to indicate whether the user can use the
-- @GetProgrammaticAccessCredentials@ API to obtain credentials that can
-- then be used to access other FinSpace Data API operations.
--
-- -   @ENABLED@ – The user has permissions to use the APIs.
--
-- -   @DISABLED@ – The user does not have permissions to use any APIs.
--
-- 'apiAccessPrincipalArn', 'updateUser_apiAccessPrincipalArn' - The ARN identifier of an AWS user or role that is allowed to call the
-- @GetProgrammaticAccessCredentials@ API to obtain a credentials token for
-- a specific FinSpace user. This must be an IAM role within your FinSpace
-- account.
--
-- 'userId', 'updateUser_userId' - The unique identifier for the user account to update.
newUpdateUser ::
  -- | 'userId'
  Prelude.Text ->
  UpdateUser
newUpdateUser pUserId_ =
  UpdateUser'
    { clientToken = Prelude.Nothing,
      type' = Prelude.Nothing,
      firstName = Prelude.Nothing,
      lastName = Prelude.Nothing,
      apiAccess = Prelude.Nothing,
      apiAccessPrincipalArn = Prelude.Nothing,
      userId = pUserId_
    }

-- | A token that ensures idempotency. This token expires in 10 minutes.
updateUser_clientToken :: Lens.Lens' UpdateUser (Prelude.Maybe Prelude.Text)
updateUser_clientToken = Lens.lens (\UpdateUser' {clientToken} -> clientToken) (\s@UpdateUser' {} a -> s {clientToken = a} :: UpdateUser)

-- | The option to indicate the type of user.
--
-- -   @SUPER_USER@– A user with permission to all the functionality and
--     data in FinSpace.
--
-- -   @APP_USER@ – A user with specific permissions in FinSpace. The users
--     are assigned permissions by adding them to a permission group.
updateUser_type :: Lens.Lens' UpdateUser (Prelude.Maybe UserType)
updateUser_type = Lens.lens (\UpdateUser' {type'} -> type') (\s@UpdateUser' {} a -> s {type' = a} :: UpdateUser)

-- | The first name of the user.
updateUser_firstName :: Lens.Lens' UpdateUser (Prelude.Maybe Prelude.Text)
updateUser_firstName = Lens.lens (\UpdateUser' {firstName} -> firstName) (\s@UpdateUser' {} a -> s {firstName = a} :: UpdateUser) Prelude.. Lens.mapping Data._Sensitive

-- | The last name of the user.
updateUser_lastName :: Lens.Lens' UpdateUser (Prelude.Maybe Prelude.Text)
updateUser_lastName = Lens.lens (\UpdateUser' {lastName} -> lastName) (\s@UpdateUser' {} a -> s {lastName = a} :: UpdateUser) Prelude.. Lens.mapping Data._Sensitive

-- | The option to indicate whether the user can use the
-- @GetProgrammaticAccessCredentials@ API to obtain credentials that can
-- then be used to access other FinSpace Data API operations.
--
-- -   @ENABLED@ – The user has permissions to use the APIs.
--
-- -   @DISABLED@ – The user does not have permissions to use any APIs.
updateUser_apiAccess :: Lens.Lens' UpdateUser (Prelude.Maybe ApiAccess)
updateUser_apiAccess = Lens.lens (\UpdateUser' {apiAccess} -> apiAccess) (\s@UpdateUser' {} a -> s {apiAccess = a} :: UpdateUser)

-- | The ARN identifier of an AWS user or role that is allowed to call the
-- @GetProgrammaticAccessCredentials@ API to obtain a credentials token for
-- a specific FinSpace user. This must be an IAM role within your FinSpace
-- account.
updateUser_apiAccessPrincipalArn :: Lens.Lens' UpdateUser (Prelude.Maybe Prelude.Text)
updateUser_apiAccessPrincipalArn = Lens.lens (\UpdateUser' {apiAccessPrincipalArn} -> apiAccessPrincipalArn) (\s@UpdateUser' {} a -> s {apiAccessPrincipalArn = a} :: UpdateUser)

-- | The unique identifier for the user account to update.
updateUser_userId :: Lens.Lens' UpdateUser Prelude.Text
updateUser_userId = Lens.lens (\UpdateUser' {userId} -> userId) (\s@UpdateUser' {} a -> s {userId = a} :: UpdateUser)

instance Core.AWSRequest UpdateUser where
  type AWSResponse UpdateUser = UpdateUserResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUserResponse'
            Prelude.<$> (x Data..?> "userId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateUser where
  hashWithSalt _salt UpdateUser' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` firstName
      `Prelude.hashWithSalt` lastName
      `Prelude.hashWithSalt` apiAccess
      `Prelude.hashWithSalt` apiAccessPrincipalArn
      `Prelude.hashWithSalt` userId

instance Prelude.NFData UpdateUser where
  rnf UpdateUser' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf firstName
      `Prelude.seq` Prelude.rnf lastName
      `Prelude.seq` Prelude.rnf apiAccess
      `Prelude.seq` Prelude.rnf apiAccessPrincipalArn
      `Prelude.seq` Prelude.rnf userId

instance Data.ToHeaders UpdateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateUser where
  toJSON UpdateUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("type" Data..=) Prelude.<$> type',
            ("firstName" Data..=) Prelude.<$> firstName,
            ("lastName" Data..=) Prelude.<$> lastName,
            ("apiAccess" Data..=) Prelude.<$> apiAccess,
            ("apiAccessPrincipalArn" Data..=)
              Prelude.<$> apiAccessPrincipalArn
          ]
      )

instance Data.ToPath UpdateUser where
  toPath UpdateUser' {..} =
    Prelude.mconcat ["/user/", Data.toBS userId]

instance Data.ToQuery UpdateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserResponse' smart constructor.
data UpdateUserResponse = UpdateUserResponse'
  { -- | The unique identifier of the updated user account.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'updateUserResponse_userId' - The unique identifier of the updated user account.
--
-- 'httpStatus', 'updateUserResponse_httpStatus' - The response's http status code.
newUpdateUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateUserResponse
newUpdateUserResponse pHttpStatus_ =
  UpdateUserResponse'
    { userId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the updated user account.
updateUserResponse_userId :: Lens.Lens' UpdateUserResponse (Prelude.Maybe Prelude.Text)
updateUserResponse_userId = Lens.lens (\UpdateUserResponse' {userId} -> userId) (\s@UpdateUserResponse' {} a -> s {userId = a} :: UpdateUserResponse)

-- | The response's http status code.
updateUserResponse_httpStatus :: Lens.Lens' UpdateUserResponse Prelude.Int
updateUserResponse_httpStatus = Lens.lens (\UpdateUserResponse' {httpStatus} -> httpStatus) (\s@UpdateUserResponse' {} a -> s {httpStatus = a} :: UpdateUserResponse)

instance Prelude.NFData UpdateUserResponse where
  rnf UpdateUserResponse' {..} =
    Prelude.rnf userId
      `Prelude.seq` Prelude.rnf httpStatus
