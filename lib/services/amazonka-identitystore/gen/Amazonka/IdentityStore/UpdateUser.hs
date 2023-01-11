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
-- Module      : Amazonka.IdentityStore.UpdateUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For the specified user in the specified identity store, updates the user
-- metadata and attributes.
module Amazonka.IdentityStore.UpdateUser
  ( -- * Creating a Request
    UpdateUser (..),
    newUpdateUser,

    -- * Request Lenses
    updateUser_identityStoreId,
    updateUser_userId,
    updateUser_operations,

    -- * Destructuring the Response
    UpdateUserResponse (..),
    newUpdateUserResponse,

    -- * Response Lenses
    updateUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IdentityStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateUser' smart constructor.
data UpdateUser = UpdateUser'
  { -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text,
    -- | The identifier for a user in the identity store.
    userId :: Prelude.Text,
    -- | A list of @AttributeOperation@ objects to apply to the requested user.
    -- These operations might add, replace, or remove an attribute.
    operations :: Prelude.NonEmpty AttributeOperation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityStoreId', 'updateUser_identityStoreId' - The globally unique identifier for the identity store.
--
-- 'userId', 'updateUser_userId' - The identifier for a user in the identity store.
--
-- 'operations', 'updateUser_operations' - A list of @AttributeOperation@ objects to apply to the requested user.
-- These operations might add, replace, or remove an attribute.
newUpdateUser ::
  -- | 'identityStoreId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  -- | 'operations'
  Prelude.NonEmpty AttributeOperation ->
  UpdateUser
newUpdateUser pIdentityStoreId_ pUserId_ pOperations_ =
  UpdateUser'
    { identityStoreId = pIdentityStoreId_,
      userId = pUserId_,
      operations = Lens.coerced Lens.# pOperations_
    }

-- | The globally unique identifier for the identity store.
updateUser_identityStoreId :: Lens.Lens' UpdateUser Prelude.Text
updateUser_identityStoreId = Lens.lens (\UpdateUser' {identityStoreId} -> identityStoreId) (\s@UpdateUser' {} a -> s {identityStoreId = a} :: UpdateUser)

-- | The identifier for a user in the identity store.
updateUser_userId :: Lens.Lens' UpdateUser Prelude.Text
updateUser_userId = Lens.lens (\UpdateUser' {userId} -> userId) (\s@UpdateUser' {} a -> s {userId = a} :: UpdateUser)

-- | A list of @AttributeOperation@ objects to apply to the requested user.
-- These operations might add, replace, or remove an attribute.
updateUser_operations :: Lens.Lens' UpdateUser (Prelude.NonEmpty AttributeOperation)
updateUser_operations = Lens.lens (\UpdateUser' {operations} -> operations) (\s@UpdateUser' {} a -> s {operations = a} :: UpdateUser) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateUser where
  type AWSResponse UpdateUser = UpdateUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateUser where
  hashWithSalt _salt UpdateUser' {..} =
    _salt `Prelude.hashWithSalt` identityStoreId
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` operations

instance Prelude.NFData UpdateUser where
  rnf UpdateUser' {..} =
    Prelude.rnf identityStoreId
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf operations

instance Data.ToHeaders UpdateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIdentityStore.UpdateUser" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateUser where
  toJSON UpdateUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityStoreId" Data..= identityStoreId),
            Prelude.Just ("UserId" Data..= userId),
            Prelude.Just ("Operations" Data..= operations)
          ]
      )

instance Data.ToPath UpdateUser where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserResponse' smart constructor.
data UpdateUserResponse = UpdateUserResponse'
  { -- | The response's http status code.
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
-- 'httpStatus', 'updateUserResponse_httpStatus' - The response's http status code.
newUpdateUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateUserResponse
newUpdateUserResponse pHttpStatus_ =
  UpdateUserResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateUserResponse_httpStatus :: Lens.Lens' UpdateUserResponse Prelude.Int
updateUserResponse_httpStatus = Lens.lens (\UpdateUserResponse' {httpStatus} -> httpStatus) (\s@UpdateUserResponse' {} a -> s {httpStatus = a} :: UpdateUserResponse)

instance Prelude.NFData UpdateUserResponse where
  rnf UpdateUserResponse' {..} = Prelude.rnf httpStatus
