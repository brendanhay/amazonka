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
-- Module      : Amazonka.QuickSight.DeleteUserByPrincipalId
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user identified by its principal ID.
module Amazonka.QuickSight.DeleteUserByPrincipalId
  ( -- * Creating a Request
    DeleteUserByPrincipalId (..),
    newDeleteUserByPrincipalId,

    -- * Request Lenses
    deleteUserByPrincipalId_principalId,
    deleteUserByPrincipalId_awsAccountId,
    deleteUserByPrincipalId_namespace,

    -- * Destructuring the Response
    DeleteUserByPrincipalIdResponse (..),
    newDeleteUserByPrincipalIdResponse,

    -- * Response Lenses
    deleteUserByPrincipalIdResponse_requestId,
    deleteUserByPrincipalIdResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDeleteUserByPrincipalId' smart constructor.
data DeleteUserByPrincipalId = DeleteUserByPrincipalId'
  { -- | The principal ID of the user.
    principalId :: Prelude.Text,
    -- | The ID for the Amazon Web Services account that the user is in.
    -- Currently, you use the ID for the Amazon Web Services account that
    -- contains your Amazon QuickSight account.
    awsAccountId :: Prelude.Text,
    -- | The namespace. Currently, you should set this to @default@.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserByPrincipalId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalId', 'deleteUserByPrincipalId_principalId' - The principal ID of the user.
--
-- 'awsAccountId', 'deleteUserByPrincipalId_awsAccountId' - The ID for the Amazon Web Services account that the user is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
--
-- 'namespace', 'deleteUserByPrincipalId_namespace' - The namespace. Currently, you should set this to @default@.
newDeleteUserByPrincipalId ::
  -- | 'principalId'
  Prelude.Text ->
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  DeleteUserByPrincipalId
newDeleteUserByPrincipalId
  pPrincipalId_
  pAwsAccountId_
  pNamespace_ =
    DeleteUserByPrincipalId'
      { principalId =
          pPrincipalId_,
        awsAccountId = pAwsAccountId_,
        namespace = pNamespace_
      }

-- | The principal ID of the user.
deleteUserByPrincipalId_principalId :: Lens.Lens' DeleteUserByPrincipalId Prelude.Text
deleteUserByPrincipalId_principalId = Lens.lens (\DeleteUserByPrincipalId' {principalId} -> principalId) (\s@DeleteUserByPrincipalId' {} a -> s {principalId = a} :: DeleteUserByPrincipalId)

-- | The ID for the Amazon Web Services account that the user is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
deleteUserByPrincipalId_awsAccountId :: Lens.Lens' DeleteUserByPrincipalId Prelude.Text
deleteUserByPrincipalId_awsAccountId = Lens.lens (\DeleteUserByPrincipalId' {awsAccountId} -> awsAccountId) (\s@DeleteUserByPrincipalId' {} a -> s {awsAccountId = a} :: DeleteUserByPrincipalId)

-- | The namespace. Currently, you should set this to @default@.
deleteUserByPrincipalId_namespace :: Lens.Lens' DeleteUserByPrincipalId Prelude.Text
deleteUserByPrincipalId_namespace = Lens.lens (\DeleteUserByPrincipalId' {namespace} -> namespace) (\s@DeleteUserByPrincipalId' {} a -> s {namespace = a} :: DeleteUserByPrincipalId)

instance Core.AWSRequest DeleteUserByPrincipalId where
  type
    AWSResponse DeleteUserByPrincipalId =
      DeleteUserByPrincipalIdResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteUserByPrincipalIdResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteUserByPrincipalId where
  hashWithSalt _salt DeleteUserByPrincipalId' {..} =
    _salt `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData DeleteUserByPrincipalId where
  rnf DeleteUserByPrincipalId' {..} =
    Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf namespace

instance Data.ToHeaders DeleteUserByPrincipalId where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteUserByPrincipalId where
  toPath DeleteUserByPrincipalId' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/namespaces/",
        Data.toBS namespace,
        "/user-principals/",
        Data.toBS principalId
      ]

instance Data.ToQuery DeleteUserByPrincipalId where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUserByPrincipalIdResponse' smart constructor.
data DeleteUserByPrincipalIdResponse = DeleteUserByPrincipalIdResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserByPrincipalIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'deleteUserByPrincipalIdResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'deleteUserByPrincipalIdResponse_status' - The HTTP status of the request.
newDeleteUserByPrincipalIdResponse ::
  -- | 'status'
  Prelude.Int ->
  DeleteUserByPrincipalIdResponse
newDeleteUserByPrincipalIdResponse pStatus_ =
  DeleteUserByPrincipalIdResponse'
    { requestId =
        Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
deleteUserByPrincipalIdResponse_requestId :: Lens.Lens' DeleteUserByPrincipalIdResponse (Prelude.Maybe Prelude.Text)
deleteUserByPrincipalIdResponse_requestId = Lens.lens (\DeleteUserByPrincipalIdResponse' {requestId} -> requestId) (\s@DeleteUserByPrincipalIdResponse' {} a -> s {requestId = a} :: DeleteUserByPrincipalIdResponse)

-- | The HTTP status of the request.
deleteUserByPrincipalIdResponse_status :: Lens.Lens' DeleteUserByPrincipalIdResponse Prelude.Int
deleteUserByPrincipalIdResponse_status = Lens.lens (\DeleteUserByPrincipalIdResponse' {status} -> status) (\s@DeleteUserByPrincipalIdResponse' {} a -> s {status = a} :: DeleteUserByPrincipalIdResponse)

instance
  Prelude.NFData
    DeleteUserByPrincipalIdResponse
  where
  rnf DeleteUserByPrincipalIdResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
