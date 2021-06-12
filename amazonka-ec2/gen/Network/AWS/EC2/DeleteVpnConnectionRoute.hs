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
-- Module      : Network.AWS.EC2.DeleteVpnConnectionRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified static route associated with a VPN connection
-- between an existing virtual private gateway and a VPN customer gateway.
-- The static route allows traffic to be routed from the virtual private
-- gateway to the VPN customer gateway.
module Network.AWS.EC2.DeleteVpnConnectionRoute
  ( -- * Creating a Request
    DeleteVpnConnectionRoute (..),
    newDeleteVpnConnectionRoute,

    -- * Request Lenses
    deleteVpnConnectionRoute_destinationCidrBlock,
    deleteVpnConnectionRoute_vpnConnectionId,

    -- * Destructuring the Response
    DeleteVpnConnectionRouteResponse (..),
    newDeleteVpnConnectionRouteResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteVpnConnectionRoute.
--
-- /See:/ 'newDeleteVpnConnectionRoute' smart constructor.
data DeleteVpnConnectionRoute = DeleteVpnConnectionRoute'
  { -- | The CIDR block associated with the local subnet of the customer network.
    destinationCidrBlock :: Core.Text,
    -- | The ID of the VPN connection.
    vpnConnectionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteVpnConnectionRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationCidrBlock', 'deleteVpnConnectionRoute_destinationCidrBlock' - The CIDR block associated with the local subnet of the customer network.
--
-- 'vpnConnectionId', 'deleteVpnConnectionRoute_vpnConnectionId' - The ID of the VPN connection.
newDeleteVpnConnectionRoute ::
  -- | 'destinationCidrBlock'
  Core.Text ->
  -- | 'vpnConnectionId'
  Core.Text ->
  DeleteVpnConnectionRoute
newDeleteVpnConnectionRoute
  pDestinationCidrBlock_
  pVpnConnectionId_ =
    DeleteVpnConnectionRoute'
      { destinationCidrBlock =
          pDestinationCidrBlock_,
        vpnConnectionId = pVpnConnectionId_
      }

-- | The CIDR block associated with the local subnet of the customer network.
deleteVpnConnectionRoute_destinationCidrBlock :: Lens.Lens' DeleteVpnConnectionRoute Core.Text
deleteVpnConnectionRoute_destinationCidrBlock = Lens.lens (\DeleteVpnConnectionRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@DeleteVpnConnectionRoute' {} a -> s {destinationCidrBlock = a} :: DeleteVpnConnectionRoute)

-- | The ID of the VPN connection.
deleteVpnConnectionRoute_vpnConnectionId :: Lens.Lens' DeleteVpnConnectionRoute Core.Text
deleteVpnConnectionRoute_vpnConnectionId = Lens.lens (\DeleteVpnConnectionRoute' {vpnConnectionId} -> vpnConnectionId) (\s@DeleteVpnConnectionRoute' {} a -> s {vpnConnectionId = a} :: DeleteVpnConnectionRoute)

instance Core.AWSRequest DeleteVpnConnectionRoute where
  type
    AWSResponse DeleteVpnConnectionRoute =
      DeleteVpnConnectionRouteResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteVpnConnectionRouteResponse'

instance Core.Hashable DeleteVpnConnectionRoute

instance Core.NFData DeleteVpnConnectionRoute

instance Core.ToHeaders DeleteVpnConnectionRoute where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteVpnConnectionRoute where
  toPath = Core.const "/"

instance Core.ToQuery DeleteVpnConnectionRoute where
  toQuery DeleteVpnConnectionRoute' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteVpnConnectionRoute" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DestinationCidrBlock" Core.=: destinationCidrBlock,
        "VpnConnectionId" Core.=: vpnConnectionId
      ]

-- | /See:/ 'newDeleteVpnConnectionRouteResponse' smart constructor.
data DeleteVpnConnectionRouteResponse = DeleteVpnConnectionRouteResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteVpnConnectionRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVpnConnectionRouteResponse ::
  DeleteVpnConnectionRouteResponse
newDeleteVpnConnectionRouteResponse =
  DeleteVpnConnectionRouteResponse'

instance Core.NFData DeleteVpnConnectionRouteResponse
