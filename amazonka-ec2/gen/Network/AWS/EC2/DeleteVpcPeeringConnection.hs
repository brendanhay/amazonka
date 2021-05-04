{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.DeleteVpcPeeringConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a VPC peering connection. Either the owner of the requester VPC
-- or the owner of the accepter VPC can delete the VPC peering connection
-- if it\'s in the @active@ state. The owner of the requester VPC can
-- delete a VPC peering connection in the @pending-acceptance@ state. You
-- cannot delete a VPC peering connection that\'s in the @failed@ state.
module Network.AWS.EC2.DeleteVpcPeeringConnection
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.AWSRequest
    DeleteVpcPeeringConnection
  where
  type
    Rs DeleteVpcPeeringConnection =
      DeleteVpcPeeringConnectionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteVpcPeeringConnectionResponse'
            Prelude.<$> (x Prelude..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteVpcPeeringConnection

instance Prelude.NFData DeleteVpcPeeringConnection

instance Prelude.ToHeaders DeleteVpcPeeringConnection where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteVpcPeeringConnection where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteVpcPeeringConnection where
  toQuery DeleteVpcPeeringConnection' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteVpcPeeringConnection" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "VpcPeeringConnectionId"
          Prelude.=: vpcPeeringConnectionId
      ]

-- | /See:/ 'newDeleteVpcPeeringConnectionResponse' smart constructor.
data DeleteVpcPeeringConnectionResponse = DeleteVpcPeeringConnectionResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
