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
-- Module      : Amazonka.IdentityStore.DeleteUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user within an identity store given @UserId@.
module Amazonka.IdentityStore.DeleteUser
  ( -- * Creating a Request
    DeleteUser (..),
    newDeleteUser,

    -- * Request Lenses
    deleteUser_identityStoreId,
    deleteUser_userId,

    -- * Destructuring the Response
    DeleteUserResponse (..),
    newDeleteUserResponse,

    -- * Response Lenses
    deleteUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IdentityStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text,
    -- | The identifier for a user in the identity store.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityStoreId', 'deleteUser_identityStoreId' - The globally unique identifier for the identity store.
--
-- 'userId', 'deleteUser_userId' - The identifier for a user in the identity store.
newDeleteUser ::
  -- | 'identityStoreId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  DeleteUser
newDeleteUser pIdentityStoreId_ pUserId_ =
  DeleteUser'
    { identityStoreId = pIdentityStoreId_,
      userId = pUserId_
    }

-- | The globally unique identifier for the identity store.
deleteUser_identityStoreId :: Lens.Lens' DeleteUser Prelude.Text
deleteUser_identityStoreId = Lens.lens (\DeleteUser' {identityStoreId} -> identityStoreId) (\s@DeleteUser' {} a -> s {identityStoreId = a} :: DeleteUser)

-- | The identifier for a user in the identity store.
deleteUser_userId :: Lens.Lens' DeleteUser Prelude.Text
deleteUser_userId = Lens.lens (\DeleteUser' {userId} -> userId) (\s@DeleteUser' {} a -> s {userId = a} :: DeleteUser)

instance Core.AWSRequest DeleteUser where
  type AWSResponse DeleteUser = DeleteUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteUser where
  hashWithSalt _salt DeleteUser' {..} =
    _salt
      `Prelude.hashWithSalt` identityStoreId
      `Prelude.hashWithSalt` userId

instance Prelude.NFData DeleteUser where
  rnf DeleteUser' {..} =
    Prelude.rnf identityStoreId
      `Prelude.seq` Prelude.rnf userId

instance Data.ToHeaders DeleteUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIdentityStore.DeleteUser" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteUser where
  toJSON DeleteUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityStoreId" Data..= identityStoreId),
            Prelude.Just ("UserId" Data..= userId)
          ]
      )

instance Data.ToPath DeleteUser where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUserResponse' smart constructor.
data DeleteUserResponse = DeleteUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteUserResponse_httpStatus' - The response's http status code.
newDeleteUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteUserResponse
newDeleteUserResponse pHttpStatus_ =
  DeleteUserResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteUserResponse_httpStatus :: Lens.Lens' DeleteUserResponse Prelude.Int
deleteUserResponse_httpStatus = Lens.lens (\DeleteUserResponse' {httpStatus} -> httpStatus) (\s@DeleteUserResponse' {} a -> s {httpStatus = a} :: DeleteUserResponse)

instance Prelude.NFData DeleteUserResponse where
  rnf DeleteUserResponse' {..} = Prelude.rnf httpStatus
