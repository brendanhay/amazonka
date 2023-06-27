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
-- Module      : Amazonka.QuickSight.DeleteVPCConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a VPC connection.
module Amazonka.QuickSight.DeleteVPCConnection
  ( -- * Creating a Request
    DeleteVPCConnection (..),
    newDeleteVPCConnection,

    -- * Request Lenses
    deleteVPCConnection_awsAccountId,
    deleteVPCConnection_vPCConnectionId,

    -- * Destructuring the Response
    DeleteVPCConnectionResponse (..),
    newDeleteVPCConnectionResponse,

    -- * Response Lenses
    deleteVPCConnectionResponse_arn,
    deleteVPCConnectionResponse_availabilityStatus,
    deleteVPCConnectionResponse_deletionStatus,
    deleteVPCConnectionResponse_requestId,
    deleteVPCConnectionResponse_vPCConnectionId,
    deleteVPCConnectionResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVPCConnection' smart constructor.
data DeleteVPCConnection = DeleteVPCConnection'
  { -- | The Amazon Web Services account ID of the account where you want to
    -- delete a VPC connection.
    awsAccountId :: Prelude.Text,
    -- | The ID of the VPC connection that you\'re creating. This ID is a unique
    -- identifier for each Amazon Web Services Region in an Amazon Web Services
    -- account.
    vPCConnectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVPCConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'deleteVPCConnection_awsAccountId' - The Amazon Web Services account ID of the account where you want to
-- delete a VPC connection.
--
-- 'vPCConnectionId', 'deleteVPCConnection_vPCConnectionId' - The ID of the VPC connection that you\'re creating. This ID is a unique
-- identifier for each Amazon Web Services Region in an Amazon Web Services
-- account.
newDeleteVPCConnection ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'vPCConnectionId'
  Prelude.Text ->
  DeleteVPCConnection
newDeleteVPCConnection
  pAwsAccountId_
  pVPCConnectionId_ =
    DeleteVPCConnection'
      { awsAccountId = pAwsAccountId_,
        vPCConnectionId = pVPCConnectionId_
      }

-- | The Amazon Web Services account ID of the account where you want to
-- delete a VPC connection.
deleteVPCConnection_awsAccountId :: Lens.Lens' DeleteVPCConnection Prelude.Text
deleteVPCConnection_awsAccountId = Lens.lens (\DeleteVPCConnection' {awsAccountId} -> awsAccountId) (\s@DeleteVPCConnection' {} a -> s {awsAccountId = a} :: DeleteVPCConnection)

-- | The ID of the VPC connection that you\'re creating. This ID is a unique
-- identifier for each Amazon Web Services Region in an Amazon Web Services
-- account.
deleteVPCConnection_vPCConnectionId :: Lens.Lens' DeleteVPCConnection Prelude.Text
deleteVPCConnection_vPCConnectionId = Lens.lens (\DeleteVPCConnection' {vPCConnectionId} -> vPCConnectionId) (\s@DeleteVPCConnection' {} a -> s {vPCConnectionId = a} :: DeleteVPCConnection)

instance Core.AWSRequest DeleteVPCConnection where
  type
    AWSResponse DeleteVPCConnection =
      DeleteVPCConnectionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVPCConnectionResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "AvailabilityStatus")
            Prelude.<*> (x Data..?> "DeletionStatus")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "VPCConnectionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteVPCConnection where
  hashWithSalt _salt DeleteVPCConnection' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` vPCConnectionId

instance Prelude.NFData DeleteVPCConnection where
  rnf DeleteVPCConnection' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf vPCConnectionId

instance Data.ToHeaders DeleteVPCConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteVPCConnection where
  toPath DeleteVPCConnection' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/vpc-connections/",
        Data.toBS vPCConnectionId
      ]

instance Data.ToQuery DeleteVPCConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVPCConnectionResponse' smart constructor.
data DeleteVPCConnectionResponse = DeleteVPCConnectionResponse'
  { -- | The Amazon Resource Name (ARN) of the deleted VPC connection.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The availability status of the VPC connection.
    availabilityStatus :: Prelude.Maybe VPCConnectionAvailabilityStatus,
    -- | The deletion status of the VPC connection.
    deletionStatus :: Prelude.Maybe VPCConnectionResourceStatus,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC connection that you\'re creating. This ID is a unique
    -- identifier for each Amazon Web Services Region in an Amazon Web Services
    -- account.
    vPCConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVPCConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteVPCConnectionResponse_arn' - The Amazon Resource Name (ARN) of the deleted VPC connection.
--
-- 'availabilityStatus', 'deleteVPCConnectionResponse_availabilityStatus' - The availability status of the VPC connection.
--
-- 'deletionStatus', 'deleteVPCConnectionResponse_deletionStatus' - The deletion status of the VPC connection.
--
-- 'requestId', 'deleteVPCConnectionResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'vPCConnectionId', 'deleteVPCConnectionResponse_vPCConnectionId' - The ID of the VPC connection that you\'re creating. This ID is a unique
-- identifier for each Amazon Web Services Region in an Amazon Web Services
-- account.
--
-- 'status', 'deleteVPCConnectionResponse_status' - The HTTP status of the request.
newDeleteVPCConnectionResponse ::
  -- | 'status'
  Prelude.Int ->
  DeleteVPCConnectionResponse
newDeleteVPCConnectionResponse pStatus_ =
  DeleteVPCConnectionResponse'
    { arn = Prelude.Nothing,
      availabilityStatus = Prelude.Nothing,
      deletionStatus = Prelude.Nothing,
      requestId = Prelude.Nothing,
      vPCConnectionId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the deleted VPC connection.
deleteVPCConnectionResponse_arn :: Lens.Lens' DeleteVPCConnectionResponse (Prelude.Maybe Prelude.Text)
deleteVPCConnectionResponse_arn = Lens.lens (\DeleteVPCConnectionResponse' {arn} -> arn) (\s@DeleteVPCConnectionResponse' {} a -> s {arn = a} :: DeleteVPCConnectionResponse)

-- | The availability status of the VPC connection.
deleteVPCConnectionResponse_availabilityStatus :: Lens.Lens' DeleteVPCConnectionResponse (Prelude.Maybe VPCConnectionAvailabilityStatus)
deleteVPCConnectionResponse_availabilityStatus = Lens.lens (\DeleteVPCConnectionResponse' {availabilityStatus} -> availabilityStatus) (\s@DeleteVPCConnectionResponse' {} a -> s {availabilityStatus = a} :: DeleteVPCConnectionResponse)

-- | The deletion status of the VPC connection.
deleteVPCConnectionResponse_deletionStatus :: Lens.Lens' DeleteVPCConnectionResponse (Prelude.Maybe VPCConnectionResourceStatus)
deleteVPCConnectionResponse_deletionStatus = Lens.lens (\DeleteVPCConnectionResponse' {deletionStatus} -> deletionStatus) (\s@DeleteVPCConnectionResponse' {} a -> s {deletionStatus = a} :: DeleteVPCConnectionResponse)

-- | The Amazon Web Services request ID for this operation.
deleteVPCConnectionResponse_requestId :: Lens.Lens' DeleteVPCConnectionResponse (Prelude.Maybe Prelude.Text)
deleteVPCConnectionResponse_requestId = Lens.lens (\DeleteVPCConnectionResponse' {requestId} -> requestId) (\s@DeleteVPCConnectionResponse' {} a -> s {requestId = a} :: DeleteVPCConnectionResponse)

-- | The ID of the VPC connection that you\'re creating. This ID is a unique
-- identifier for each Amazon Web Services Region in an Amazon Web Services
-- account.
deleteVPCConnectionResponse_vPCConnectionId :: Lens.Lens' DeleteVPCConnectionResponse (Prelude.Maybe Prelude.Text)
deleteVPCConnectionResponse_vPCConnectionId = Lens.lens (\DeleteVPCConnectionResponse' {vPCConnectionId} -> vPCConnectionId) (\s@DeleteVPCConnectionResponse' {} a -> s {vPCConnectionId = a} :: DeleteVPCConnectionResponse)

-- | The HTTP status of the request.
deleteVPCConnectionResponse_status :: Lens.Lens' DeleteVPCConnectionResponse Prelude.Int
deleteVPCConnectionResponse_status = Lens.lens (\DeleteVPCConnectionResponse' {status} -> status) (\s@DeleteVPCConnectionResponse' {} a -> s {status = a} :: DeleteVPCConnectionResponse)

instance Prelude.NFData DeleteVPCConnectionResponse where
  rnf DeleteVPCConnectionResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf availabilityStatus
      `Prelude.seq` Prelude.rnf deletionStatus
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf vPCConnectionId
      `Prelude.seq` Prelude.rnf status
