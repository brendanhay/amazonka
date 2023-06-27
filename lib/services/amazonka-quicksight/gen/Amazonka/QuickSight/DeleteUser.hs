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
-- Module      : Amazonka.QuickSight.DeleteUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the Amazon QuickSight user that is associated with the identity
-- of the IAM user or role that\'s making the call. The IAM user isn\'t
-- deleted as a result of this call.
module Amazonka.QuickSight.DeleteUser
  ( -- * Creating a Request
    DeleteUser (..),
    newDeleteUser,

    -- * Request Lenses
    deleteUser_userName,
    deleteUser_awsAccountId,
    deleteUser_namespace,

    -- * Destructuring the Response
    DeleteUserResponse (..),
    newDeleteUserResponse,

    -- * Response Lenses
    deleteUserResponse_requestId,
    deleteUserResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { -- | The name of the user that you want to delete.
    userName :: Prelude.Text,
    -- | The ID for the Amazon Web Services account that the user is in.
    -- Currently, you use the ID for the Amazon Web Services account that
    -- contains your Amazon QuickSight account.
    awsAccountId :: Prelude.Text,
    -- | The namespace. Currently, you should set this to @default@.
    namespace :: Prelude.Text
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
-- 'userName', 'deleteUser_userName' - The name of the user that you want to delete.
--
-- 'awsAccountId', 'deleteUser_awsAccountId' - The ID for the Amazon Web Services account that the user is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
--
-- 'namespace', 'deleteUser_namespace' - The namespace. Currently, you should set this to @default@.
newDeleteUser ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  DeleteUser
newDeleteUser pUserName_ pAwsAccountId_ pNamespace_ =
  DeleteUser'
    { userName = pUserName_,
      awsAccountId = pAwsAccountId_,
      namespace = pNamespace_
    }

-- | The name of the user that you want to delete.
deleteUser_userName :: Lens.Lens' DeleteUser Prelude.Text
deleteUser_userName = Lens.lens (\DeleteUser' {userName} -> userName) (\s@DeleteUser' {} a -> s {userName = a} :: DeleteUser)

-- | The ID for the Amazon Web Services account that the user is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
deleteUser_awsAccountId :: Lens.Lens' DeleteUser Prelude.Text
deleteUser_awsAccountId = Lens.lens (\DeleteUser' {awsAccountId} -> awsAccountId) (\s@DeleteUser' {} a -> s {awsAccountId = a} :: DeleteUser)

-- | The namespace. Currently, you should set this to @default@.
deleteUser_namespace :: Lens.Lens' DeleteUser Prelude.Text
deleteUser_namespace = Lens.lens (\DeleteUser' {namespace} -> namespace) (\s@DeleteUser' {} a -> s {namespace = a} :: DeleteUser)

instance Core.AWSRequest DeleteUser where
  type AWSResponse DeleteUser = DeleteUserResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteUserResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteUser where
  hashWithSalt _salt DeleteUser' {..} =
    _salt
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData DeleteUser where
  rnf DeleteUser' {..} =
    Prelude.rnf userName
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf namespace

instance Data.ToHeaders DeleteUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteUser where
  toPath DeleteUser' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/namespaces/",
        Data.toBS namespace,
        "/users/",
        Data.toBS userName
      ]

instance Data.ToQuery DeleteUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUserResponse' smart constructor.
data DeleteUserResponse = DeleteUserResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
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
-- 'requestId', 'deleteUserResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'deleteUserResponse_status' - The HTTP status of the request.
newDeleteUserResponse ::
  -- | 'status'
  Prelude.Int ->
  DeleteUserResponse
newDeleteUserResponse pStatus_ =
  DeleteUserResponse'
    { requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
deleteUserResponse_requestId :: Lens.Lens' DeleteUserResponse (Prelude.Maybe Prelude.Text)
deleteUserResponse_requestId = Lens.lens (\DeleteUserResponse' {requestId} -> requestId) (\s@DeleteUserResponse' {} a -> s {requestId = a} :: DeleteUserResponse)

-- | The HTTP status of the request.
deleteUserResponse_status :: Lens.Lens' DeleteUserResponse Prelude.Int
deleteUserResponse_status = Lens.lens (\DeleteUserResponse' {status} -> status) (\s@DeleteUserResponse' {} a -> s {status = a} :: DeleteUserResponse)

instance Prelude.NFData DeleteUserResponse where
  rnf DeleteUserResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
