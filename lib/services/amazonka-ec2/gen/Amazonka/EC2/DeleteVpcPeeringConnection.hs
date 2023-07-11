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
-- Module      : Amazonka.EC2.DeleteVpcPeeringConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a VPC peering connection. Either the owner of the requester VPC
-- or the owner of the accepter VPC can delete the VPC peering connection
-- if it\'s in the @active@ state. The owner of the requester VPC can
-- delete a VPC peering connection in the @pending-acceptance@ state. You
-- cannot delete a VPC peering connection that\'s in the @failed@ state.
module Amazonka.EC2.DeleteVpcPeeringConnection
  ( -- * Creating a Request
    DeleteVpcPeeringConnection (..),
    newDeleteVpcPeeringConnection,

    -- * Request Lenses
    deleteVpcPeeringConnection_dryRun,
    deleteVpcPeeringConnection_vpcPeeringConnectionId,

    -- * Destructuring the Response
    DeleteVpcPeeringConnectionResponse (..),
    newDeleteVpcPeeringConnectionResponse,

    -- * Response Lenses
    deleteVpcPeeringConnectionResponse_return,
    deleteVpcPeeringConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVpcPeeringConnection' smart constructor.
data DeleteVpcPeeringConnection = DeleteVpcPeeringConnection'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the VPC peering connection.
    vpcPeeringConnectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcPeeringConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteVpcPeeringConnection_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpcPeeringConnectionId', 'deleteVpcPeeringConnection_vpcPeeringConnectionId' - The ID of the VPC peering connection.
newDeleteVpcPeeringConnection ::
  -- | 'vpcPeeringConnectionId'
  Prelude.Text ->
  DeleteVpcPeeringConnection
newDeleteVpcPeeringConnection
  pVpcPeeringConnectionId_ =
    DeleteVpcPeeringConnection'
      { dryRun =
          Prelude.Nothing,
        vpcPeeringConnectionId =
          pVpcPeeringConnectionId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteVpcPeeringConnection_dryRun :: Lens.Lens' DeleteVpcPeeringConnection (Prelude.Maybe Prelude.Bool)
deleteVpcPeeringConnection_dryRun = Lens.lens (\DeleteVpcPeeringConnection' {dryRun} -> dryRun) (\s@DeleteVpcPeeringConnection' {} a -> s {dryRun = a} :: DeleteVpcPeeringConnection)

-- | The ID of the VPC peering connection.
deleteVpcPeeringConnection_vpcPeeringConnectionId :: Lens.Lens' DeleteVpcPeeringConnection Prelude.Text
deleteVpcPeeringConnection_vpcPeeringConnectionId = Lens.lens (\DeleteVpcPeeringConnection' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@DeleteVpcPeeringConnection' {} a -> s {vpcPeeringConnectionId = a} :: DeleteVpcPeeringConnection)

instance Core.AWSRequest DeleteVpcPeeringConnection where
  type
    AWSResponse DeleteVpcPeeringConnection =
      DeleteVpcPeeringConnectionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteVpcPeeringConnectionResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteVpcPeeringConnection where
  hashWithSalt _salt DeleteVpcPeeringConnection' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` vpcPeeringConnectionId

instance Prelude.NFData DeleteVpcPeeringConnection where
  rnf DeleteVpcPeeringConnection' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf vpcPeeringConnectionId

instance Data.ToHeaders DeleteVpcPeeringConnection where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteVpcPeeringConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteVpcPeeringConnection where
  toQuery DeleteVpcPeeringConnection' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteVpcPeeringConnection" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "VpcPeeringConnectionId"
          Data.=: vpcPeeringConnectionId
      ]

-- | /See:/ 'newDeleteVpcPeeringConnectionResponse' smart constructor.
data DeleteVpcPeeringConnectionResponse = DeleteVpcPeeringConnectionResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcPeeringConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'deleteVpcPeeringConnectionResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'deleteVpcPeeringConnectionResponse_httpStatus' - The response's http status code.
newDeleteVpcPeeringConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteVpcPeeringConnectionResponse
newDeleteVpcPeeringConnectionResponse pHttpStatus_ =
  DeleteVpcPeeringConnectionResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
deleteVpcPeeringConnectionResponse_return :: Lens.Lens' DeleteVpcPeeringConnectionResponse (Prelude.Maybe Prelude.Bool)
deleteVpcPeeringConnectionResponse_return = Lens.lens (\DeleteVpcPeeringConnectionResponse' {return'} -> return') (\s@DeleteVpcPeeringConnectionResponse' {} a -> s {return' = a} :: DeleteVpcPeeringConnectionResponse)

-- | The response's http status code.
deleteVpcPeeringConnectionResponse_httpStatus :: Lens.Lens' DeleteVpcPeeringConnectionResponse Prelude.Int
deleteVpcPeeringConnectionResponse_httpStatus = Lens.lens (\DeleteVpcPeeringConnectionResponse' {httpStatus} -> httpStatus) (\s@DeleteVpcPeeringConnectionResponse' {} a -> s {httpStatus = a} :: DeleteVpcPeeringConnectionResponse)

instance
  Prelude.NFData
    DeleteVpcPeeringConnectionResponse
  where
  rnf DeleteVpcPeeringConnectionResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
