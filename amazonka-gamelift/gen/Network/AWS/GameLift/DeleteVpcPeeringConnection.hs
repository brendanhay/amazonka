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
-- Module      : Network.AWS.GameLift.DeleteVpcPeeringConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a VPC peering connection. To delete the connection, you must
-- have a valid authorization for the VPC peering connection that you want
-- to delete. You can check for an authorization by calling
-- DescribeVpcPeeringAuthorizations or request a new one using
-- CreateVpcPeeringAuthorization.
--
-- Once a valid authorization exists, call this operation from the AWS
-- account that is used to manage the Amazon GameLift fleets. Identify the
-- connection to delete by the connection ID and fleet ID. If successful,
-- the connection is removed.
--
-- -   CreateVpcPeeringAuthorization
--
-- -   DescribeVpcPeeringAuthorizations
--
-- -   DeleteVpcPeeringAuthorization
--
-- -   CreateVpcPeeringConnection
--
-- -   DescribeVpcPeeringConnections
--
-- -   DeleteVpcPeeringConnection
module Network.AWS.GameLift.DeleteVpcPeeringConnection
  ( -- * Creating a Request
    DeleteVpcPeeringConnection (..),
    newDeleteVpcPeeringConnection,

    -- * Request Lenses
    deleteVpcPeeringConnection_fleetId,
    deleteVpcPeeringConnection_vpcPeeringConnectionId,

    -- * Destructuring the Response
    DeleteVpcPeeringConnectionResponse (..),
    newDeleteVpcPeeringConnectionResponse,

    -- * Response Lenses
    deleteVpcPeeringConnectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDeleteVpcPeeringConnection' smart constructor.
data DeleteVpcPeeringConnection = DeleteVpcPeeringConnection'
  { -- | A unique identifier for a fleet. This fleet specified must match the
    -- fleet referenced in the VPC peering connection record. You can use
    -- either the fleet ID or ARN value.
    fleetId :: Core.Text,
    -- | A unique identifier for a VPC peering connection. This value is included
    -- in the VpcPeeringConnection object, which can be retrieved by calling
    -- DescribeVpcPeeringConnections.
    vpcPeeringConnectionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteVpcPeeringConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'deleteVpcPeeringConnection_fleetId' - A unique identifier for a fleet. This fleet specified must match the
-- fleet referenced in the VPC peering connection record. You can use
-- either the fleet ID or ARN value.
--
-- 'vpcPeeringConnectionId', 'deleteVpcPeeringConnection_vpcPeeringConnectionId' - A unique identifier for a VPC peering connection. This value is included
-- in the VpcPeeringConnection object, which can be retrieved by calling
-- DescribeVpcPeeringConnections.
newDeleteVpcPeeringConnection ::
  -- | 'fleetId'
  Core.Text ->
  -- | 'vpcPeeringConnectionId'
  Core.Text ->
  DeleteVpcPeeringConnection
newDeleteVpcPeeringConnection
  pFleetId_
  pVpcPeeringConnectionId_ =
    DeleteVpcPeeringConnection'
      { fleetId = pFleetId_,
        vpcPeeringConnectionId =
          pVpcPeeringConnectionId_
      }

-- | A unique identifier for a fleet. This fleet specified must match the
-- fleet referenced in the VPC peering connection record. You can use
-- either the fleet ID or ARN value.
deleteVpcPeeringConnection_fleetId :: Lens.Lens' DeleteVpcPeeringConnection Core.Text
deleteVpcPeeringConnection_fleetId = Lens.lens (\DeleteVpcPeeringConnection' {fleetId} -> fleetId) (\s@DeleteVpcPeeringConnection' {} a -> s {fleetId = a} :: DeleteVpcPeeringConnection)

-- | A unique identifier for a VPC peering connection. This value is included
-- in the VpcPeeringConnection object, which can be retrieved by calling
-- DescribeVpcPeeringConnections.
deleteVpcPeeringConnection_vpcPeeringConnectionId :: Lens.Lens' DeleteVpcPeeringConnection Core.Text
deleteVpcPeeringConnection_vpcPeeringConnectionId = Lens.lens (\DeleteVpcPeeringConnection' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@DeleteVpcPeeringConnection' {} a -> s {vpcPeeringConnectionId = a} :: DeleteVpcPeeringConnection)

instance Core.AWSRequest DeleteVpcPeeringConnection where
  type
    AWSResponse DeleteVpcPeeringConnection =
      DeleteVpcPeeringConnectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteVpcPeeringConnectionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteVpcPeeringConnection

instance Core.NFData DeleteVpcPeeringConnection

instance Core.ToHeaders DeleteVpcPeeringConnection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DeleteVpcPeeringConnection" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteVpcPeeringConnection where
  toJSON DeleteVpcPeeringConnection' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FleetId" Core..= fleetId),
            Core.Just
              ( "VpcPeeringConnectionId"
                  Core..= vpcPeeringConnectionId
              )
          ]
      )

instance Core.ToPath DeleteVpcPeeringConnection where
  toPath = Core.const "/"

instance Core.ToQuery DeleteVpcPeeringConnection where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteVpcPeeringConnectionResponse' smart constructor.
data DeleteVpcPeeringConnectionResponse = DeleteVpcPeeringConnectionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteVpcPeeringConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteVpcPeeringConnectionResponse_httpStatus' - The response's http status code.
newDeleteVpcPeeringConnectionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteVpcPeeringConnectionResponse
newDeleteVpcPeeringConnectionResponse pHttpStatus_ =
  DeleteVpcPeeringConnectionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteVpcPeeringConnectionResponse_httpStatus :: Lens.Lens' DeleteVpcPeeringConnectionResponse Core.Int
deleteVpcPeeringConnectionResponse_httpStatus = Lens.lens (\DeleteVpcPeeringConnectionResponse' {httpStatus} -> httpStatus) (\s@DeleteVpcPeeringConnectionResponse' {} a -> s {httpStatus = a} :: DeleteVpcPeeringConnectionResponse)

instance
  Core.NFData
    DeleteVpcPeeringConnectionResponse
