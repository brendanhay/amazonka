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
-- Module      : Network.AWS.EC2.DeleteClientVpnRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a route from a Client VPN endpoint. You can only delete routes
-- that you manually added using the __CreateClientVpnRoute__ action. You
-- cannot delete routes that were automatically added when associating a
-- subnet. To remove routes that have been automatically added,
-- disassociate the target subnet from the Client VPN endpoint.
module Network.AWS.EC2.DeleteClientVpnRoute
  ( -- * Creating a Request
    DeleteClientVpnRoute (..),
    newDeleteClientVpnRoute,

    -- * Request Lenses
    deleteClientVpnRoute_dryRun,
    deleteClientVpnRoute_targetVpcSubnetId,
    deleteClientVpnRoute_clientVpnEndpointId,
    deleteClientVpnRoute_destinationCidrBlock,

    -- * Destructuring the Response
    DeleteClientVpnRouteResponse (..),
    newDeleteClientVpnRouteResponse,

    -- * Response Lenses
    deleteClientVpnRouteResponse_status,
    deleteClientVpnRouteResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteClientVpnRoute' smart constructor.
data DeleteClientVpnRoute = DeleteClientVpnRoute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the target subnet used by the route.
    targetVpcSubnetId :: Core.Maybe Core.Text,
    -- | The ID of the Client VPN endpoint from which the route is to be deleted.
    clientVpnEndpointId :: Core.Text,
    -- | The IPv4 address range, in CIDR notation, of the route to be deleted.
    destinationCidrBlock :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteClientVpnRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteClientVpnRoute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'targetVpcSubnetId', 'deleteClientVpnRoute_targetVpcSubnetId' - The ID of the target subnet used by the route.
--
-- 'clientVpnEndpointId', 'deleteClientVpnRoute_clientVpnEndpointId' - The ID of the Client VPN endpoint from which the route is to be deleted.
--
-- 'destinationCidrBlock', 'deleteClientVpnRoute_destinationCidrBlock' - The IPv4 address range, in CIDR notation, of the route to be deleted.
newDeleteClientVpnRoute ::
  -- | 'clientVpnEndpointId'
  Core.Text ->
  -- | 'destinationCidrBlock'
  Core.Text ->
  DeleteClientVpnRoute
newDeleteClientVpnRoute
  pClientVpnEndpointId_
  pDestinationCidrBlock_ =
    DeleteClientVpnRoute'
      { dryRun = Core.Nothing,
        targetVpcSubnetId = Core.Nothing,
        clientVpnEndpointId = pClientVpnEndpointId_,
        destinationCidrBlock = pDestinationCidrBlock_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteClientVpnRoute_dryRun :: Lens.Lens' DeleteClientVpnRoute (Core.Maybe Core.Bool)
deleteClientVpnRoute_dryRun = Lens.lens (\DeleteClientVpnRoute' {dryRun} -> dryRun) (\s@DeleteClientVpnRoute' {} a -> s {dryRun = a} :: DeleteClientVpnRoute)

-- | The ID of the target subnet used by the route.
deleteClientVpnRoute_targetVpcSubnetId :: Lens.Lens' DeleteClientVpnRoute (Core.Maybe Core.Text)
deleteClientVpnRoute_targetVpcSubnetId = Lens.lens (\DeleteClientVpnRoute' {targetVpcSubnetId} -> targetVpcSubnetId) (\s@DeleteClientVpnRoute' {} a -> s {targetVpcSubnetId = a} :: DeleteClientVpnRoute)

-- | The ID of the Client VPN endpoint from which the route is to be deleted.
deleteClientVpnRoute_clientVpnEndpointId :: Lens.Lens' DeleteClientVpnRoute Core.Text
deleteClientVpnRoute_clientVpnEndpointId = Lens.lens (\DeleteClientVpnRoute' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@DeleteClientVpnRoute' {} a -> s {clientVpnEndpointId = a} :: DeleteClientVpnRoute)

-- | The IPv4 address range, in CIDR notation, of the route to be deleted.
deleteClientVpnRoute_destinationCidrBlock :: Lens.Lens' DeleteClientVpnRoute Core.Text
deleteClientVpnRoute_destinationCidrBlock = Lens.lens (\DeleteClientVpnRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@DeleteClientVpnRoute' {} a -> s {destinationCidrBlock = a} :: DeleteClientVpnRoute)

instance Core.AWSRequest DeleteClientVpnRoute where
  type
    AWSResponse DeleteClientVpnRoute =
      DeleteClientVpnRouteResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteClientVpnRouteResponse'
            Core.<$> (x Core..@? "status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteClientVpnRoute

instance Core.NFData DeleteClientVpnRoute

instance Core.ToHeaders DeleteClientVpnRoute where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteClientVpnRoute where
  toPath = Core.const "/"

instance Core.ToQuery DeleteClientVpnRoute where
  toQuery DeleteClientVpnRoute' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteClientVpnRoute" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "TargetVpcSubnetId" Core.=: targetVpcSubnetId,
        "ClientVpnEndpointId" Core.=: clientVpnEndpointId,
        "DestinationCidrBlock" Core.=: destinationCidrBlock
      ]

-- | /See:/ 'newDeleteClientVpnRouteResponse' smart constructor.
data DeleteClientVpnRouteResponse = DeleteClientVpnRouteResponse'
  { -- | The current state of the route.
    status :: Core.Maybe ClientVpnRouteStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteClientVpnRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'deleteClientVpnRouteResponse_status' - The current state of the route.
--
-- 'httpStatus', 'deleteClientVpnRouteResponse_httpStatus' - The response's http status code.
newDeleteClientVpnRouteResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteClientVpnRouteResponse
newDeleteClientVpnRouteResponse pHttpStatus_ =
  DeleteClientVpnRouteResponse'
    { status =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current state of the route.
deleteClientVpnRouteResponse_status :: Lens.Lens' DeleteClientVpnRouteResponse (Core.Maybe ClientVpnRouteStatus)
deleteClientVpnRouteResponse_status = Lens.lens (\DeleteClientVpnRouteResponse' {status} -> status) (\s@DeleteClientVpnRouteResponse' {} a -> s {status = a} :: DeleteClientVpnRouteResponse)

-- | The response's http status code.
deleteClientVpnRouteResponse_httpStatus :: Lens.Lens' DeleteClientVpnRouteResponse Core.Int
deleteClientVpnRouteResponse_httpStatus = Lens.lens (\DeleteClientVpnRouteResponse' {httpStatus} -> httpStatus) (\s@DeleteClientVpnRouteResponse' {} a -> s {httpStatus = a} :: DeleteClientVpnRouteResponse)

instance Core.NFData DeleteClientVpnRouteResponse
