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
-- Module      : Amazonka.Rekognition.DeleteUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified UserID within the collection. Faces that are
-- associated with the UserID are disassociated from the UserID before
-- deleting the specified UserID. If the specified @Collection@ or @UserID@
-- is already deleted or not found, a @ResourceNotFoundException@ will be
-- thrown. If the action is successful with a 200 response, an empty HTTP
-- body is returned.
module Amazonka.Rekognition.DeleteUser
  ( -- * Creating a Request
    DeleteUser (..),
    newDeleteUser,

    -- * Request Lenses
    deleteUser_clientRequestToken,
    deleteUser_collectionId,
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
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { -- | Idempotent token used to identify the request to @DeleteUser@. If you
    -- use the same token with multiple @DeleteUser @requests, the same
    -- response is returned. Use ClientRequestToken to prevent the same request
    -- from being processed more than once.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of an existing collection from which the UserID needs to be
    -- deleted.
    collectionId :: Prelude.Text,
    -- | ID for the UserID to be deleted.
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
-- 'clientRequestToken', 'deleteUser_clientRequestToken' - Idempotent token used to identify the request to @DeleteUser@. If you
-- use the same token with multiple @DeleteUser @requests, the same
-- response is returned. Use ClientRequestToken to prevent the same request
-- from being processed more than once.
--
-- 'collectionId', 'deleteUser_collectionId' - The ID of an existing collection from which the UserID needs to be
-- deleted.
--
-- 'userId', 'deleteUser_userId' - ID for the UserID to be deleted.
newDeleteUser ::
  -- | 'collectionId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  DeleteUser
newDeleteUser pCollectionId_ pUserId_ =
  DeleteUser'
    { clientRequestToken = Prelude.Nothing,
      collectionId = pCollectionId_,
      userId = pUserId_
    }

-- | Idempotent token used to identify the request to @DeleteUser@. If you
-- use the same token with multiple @DeleteUser @requests, the same
-- response is returned. Use ClientRequestToken to prevent the same request
-- from being processed more than once.
deleteUser_clientRequestToken :: Lens.Lens' DeleteUser (Prelude.Maybe Prelude.Text)
deleteUser_clientRequestToken = Lens.lens (\DeleteUser' {clientRequestToken} -> clientRequestToken) (\s@DeleteUser' {} a -> s {clientRequestToken = a} :: DeleteUser)

-- | The ID of an existing collection from which the UserID needs to be
-- deleted.
deleteUser_collectionId :: Lens.Lens' DeleteUser Prelude.Text
deleteUser_collectionId = Lens.lens (\DeleteUser' {collectionId} -> collectionId) (\s@DeleteUser' {} a -> s {collectionId = a} :: DeleteUser)

-- | ID for the UserID to be deleted.
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
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` collectionId
      `Prelude.hashWithSalt` userId

instance Prelude.NFData DeleteUser where
  rnf DeleteUser' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf collectionId
      `Prelude.seq` Prelude.rnf userId

instance Data.ToHeaders DeleteUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.DeleteUser" ::
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
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("CollectionId" Data..= collectionId),
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
