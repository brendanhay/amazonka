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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteClientVpnRoute' smart constructor.
data DeleteClientVpnRoute = DeleteClientVpnRoute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the target subnet used by the route.
    targetVpcSubnetId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Client VPN endpoint from which the route is to be deleted.
    clientVpnEndpointId :: Prelude.Text,
    -- | The IPv4 address range, in CIDR notation, of the route to be deleted.
    destinationCidrBlock :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'destinationCidrBlock'
  Prelude.Text ->
  DeleteClientVpnRoute
newDeleteClientVpnRoute
  pClientVpnEndpointId_
  pDestinationCidrBlock_ =
    DeleteClientVpnRoute'
      { dryRun = Prelude.Nothing,
        targetVpcSubnetId = Prelude.Nothing,
        clientVpnEndpointId = pClientVpnEndpointId_,
        destinationCidrBlock = pDestinationCidrBlock_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteClientVpnRoute_dryRun :: Lens.Lens' DeleteClientVpnRoute (Prelude.Maybe Prelude.Bool)
deleteClientVpnRoute_dryRun = Lens.lens (\DeleteClientVpnRoute' {dryRun} -> dryRun) (\s@DeleteClientVpnRoute' {} a -> s {dryRun = a} :: DeleteClientVpnRoute)

-- | The ID of the target subnet used by the route.
deleteClientVpnRoute_targetVpcSubnetId :: Lens.Lens' DeleteClientVpnRoute (Prelude.Maybe Prelude.Text)
deleteClientVpnRoute_targetVpcSubnetId = Lens.lens (\DeleteClientVpnRoute' {targetVpcSubnetId} -> targetVpcSubnetId) (\s@DeleteClientVpnRoute' {} a -> s {targetVpcSubnetId = a} :: DeleteClientVpnRoute)

-- | The ID of the Client VPN endpoint from which the route is to be deleted.
deleteClientVpnRoute_clientVpnEndpointId :: Lens.Lens' DeleteClientVpnRoute Prelude.Text
deleteClientVpnRoute_clientVpnEndpointId = Lens.lens (\DeleteClientVpnRoute' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@DeleteClientVpnRoute' {} a -> s {clientVpnEndpointId = a} :: DeleteClientVpnRoute)

-- | The IPv4 address range, in CIDR notation, of the route to be deleted.
deleteClientVpnRoute_destinationCidrBlock :: Lens.Lens' DeleteClientVpnRoute Prelude.Text
deleteClientVpnRoute_destinationCidrBlock = Lens.lens (\DeleteClientVpnRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@DeleteClientVpnRoute' {} a -> s {destinationCidrBlock = a} :: DeleteClientVpnRoute)

instance Prelude.AWSRequest DeleteClientVpnRoute where
  type
    Rs DeleteClientVpnRoute =
      DeleteClientVpnRouteResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteClientVpnRouteResponse'
            Prelude.<$> (x Prelude..@? "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteClientVpnRoute

instance Prelude.NFData DeleteClientVpnRoute

instance Prelude.ToHeaders DeleteClientVpnRoute where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteClientVpnRoute where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteClientVpnRoute where
  toQuery DeleteClientVpnRoute' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteClientVpnRoute" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "TargetVpcSubnetId" Prelude.=: targetVpcSubnetId,
        "ClientVpnEndpointId" Prelude.=: clientVpnEndpointId,
        "DestinationCidrBlock"
          Prelude.=: destinationCidrBlock
      ]

-- | /See:/ 'newDeleteClientVpnRouteResponse' smart constructor.
data DeleteClientVpnRouteResponse = DeleteClientVpnRouteResponse'
  { -- | The current state of the route.
    status :: Prelude.Maybe ClientVpnRouteStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteClientVpnRouteResponse
newDeleteClientVpnRouteResponse pHttpStatus_ =
  DeleteClientVpnRouteResponse'
    { status =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current state of the route.
deleteClientVpnRouteResponse_status :: Lens.Lens' DeleteClientVpnRouteResponse (Prelude.Maybe ClientVpnRouteStatus)
deleteClientVpnRouteResponse_status = Lens.lens (\DeleteClientVpnRouteResponse' {status} -> status) (\s@DeleteClientVpnRouteResponse' {} a -> s {status = a} :: DeleteClientVpnRouteResponse)

-- | The response's http status code.
deleteClientVpnRouteResponse_httpStatus :: Lens.Lens' DeleteClientVpnRouteResponse Prelude.Int
deleteClientVpnRouteResponse_httpStatus = Lens.lens (\DeleteClientVpnRouteResponse' {httpStatus} -> httpStatus) (\s@DeleteClientVpnRouteResponse' {} a -> s {httpStatus = a} :: DeleteClientVpnRouteResponse)

instance Prelude.NFData DeleteClientVpnRouteResponse
