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
-- Module      : Amazonka.EC2.DeleteClientVpnRoute
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.EC2.DeleteClientVpnRoute
  ( -- * Creating a Request
    DeleteClientVpnRoute (..),
    newDeleteClientVpnRoute,

    -- * Request Lenses
    deleteClientVpnRoute_targetVpcSubnetId,
    deleteClientVpnRoute_dryRun,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteClientVpnRoute' smart constructor.
data DeleteClientVpnRoute = DeleteClientVpnRoute'
  { -- | The ID of the target subnet used by the route.
    targetVpcSubnetId :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Client VPN endpoint from which the route is to be deleted.
    clientVpnEndpointId :: Prelude.Text,
    -- | The IPv4 address range, in CIDR notation, of the route to be deleted.
    destinationCidrBlock :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteClientVpnRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetVpcSubnetId', 'deleteClientVpnRoute_targetVpcSubnetId' - The ID of the target subnet used by the route.
--
-- 'dryRun', 'deleteClientVpnRoute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
      { targetVpcSubnetId =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        clientVpnEndpointId = pClientVpnEndpointId_,
        destinationCidrBlock = pDestinationCidrBlock_
      }

-- | The ID of the target subnet used by the route.
deleteClientVpnRoute_targetVpcSubnetId :: Lens.Lens' DeleteClientVpnRoute (Prelude.Maybe Prelude.Text)
deleteClientVpnRoute_targetVpcSubnetId = Lens.lens (\DeleteClientVpnRoute' {targetVpcSubnetId} -> targetVpcSubnetId) (\s@DeleteClientVpnRoute' {} a -> s {targetVpcSubnetId = a} :: DeleteClientVpnRoute)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteClientVpnRoute_dryRun :: Lens.Lens' DeleteClientVpnRoute (Prelude.Maybe Prelude.Bool)
deleteClientVpnRoute_dryRun = Lens.lens (\DeleteClientVpnRoute' {dryRun} -> dryRun) (\s@DeleteClientVpnRoute' {} a -> s {dryRun = a} :: DeleteClientVpnRoute)

-- | The ID of the Client VPN endpoint from which the route is to be deleted.
deleteClientVpnRoute_clientVpnEndpointId :: Lens.Lens' DeleteClientVpnRoute Prelude.Text
deleteClientVpnRoute_clientVpnEndpointId = Lens.lens (\DeleteClientVpnRoute' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@DeleteClientVpnRoute' {} a -> s {clientVpnEndpointId = a} :: DeleteClientVpnRoute)

-- | The IPv4 address range, in CIDR notation, of the route to be deleted.
deleteClientVpnRoute_destinationCidrBlock :: Lens.Lens' DeleteClientVpnRoute Prelude.Text
deleteClientVpnRoute_destinationCidrBlock = Lens.lens (\DeleteClientVpnRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@DeleteClientVpnRoute' {} a -> s {destinationCidrBlock = a} :: DeleteClientVpnRoute)

instance Core.AWSRequest DeleteClientVpnRoute where
  type
    AWSResponse DeleteClientVpnRoute =
      DeleteClientVpnRouteResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteClientVpnRouteResponse'
            Prelude.<$> (x Data..@? "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteClientVpnRoute where
  hashWithSalt _salt DeleteClientVpnRoute' {..} =
    _salt `Prelude.hashWithSalt` targetVpcSubnetId
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` clientVpnEndpointId
      `Prelude.hashWithSalt` destinationCidrBlock

instance Prelude.NFData DeleteClientVpnRoute where
  rnf DeleteClientVpnRoute' {..} =
    Prelude.rnf targetVpcSubnetId
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf clientVpnEndpointId
      `Prelude.seq` Prelude.rnf destinationCidrBlock

instance Data.ToHeaders DeleteClientVpnRoute where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteClientVpnRoute where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteClientVpnRoute where
  toQuery DeleteClientVpnRoute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteClientVpnRoute" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "TargetVpcSubnetId" Data.=: targetVpcSubnetId,
        "DryRun" Data.=: dryRun,
        "ClientVpnEndpointId" Data.=: clientVpnEndpointId,
        "DestinationCidrBlock" Data.=: destinationCidrBlock
      ]

-- | /See:/ 'newDeleteClientVpnRouteResponse' smart constructor.
data DeleteClientVpnRouteResponse = DeleteClientVpnRouteResponse'
  { -- | The current state of the route.
    status :: Prelude.Maybe ClientVpnRouteStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteClientVpnRouteResponse where
  rnf DeleteClientVpnRouteResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
