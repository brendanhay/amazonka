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
-- Module      : Network.AWS.EC2.ModifyVpnConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the customer gateway or the target gateway of an AWS
-- Site-to-Site VPN connection. To modify the target gateway, the following
-- migration options are available:
--
-- -   An existing virtual private gateway to a new virtual private gateway
--
-- -   An existing virtual private gateway to a transit gateway
--
-- -   An existing transit gateway to a new transit gateway
--
-- -   An existing transit gateway to a virtual private gateway
--
-- Before you perform the migration to the new gateway, you must configure
-- the new gateway. Use CreateVpnGateway to create a virtual private
-- gateway, or CreateTransitGateway to create a transit gateway.
--
-- This step is required when you migrate from a virtual private gateway
-- with static routes to a transit gateway.
--
-- You must delete the static routes before you migrate to the new gateway.
--
-- Keep a copy of the static route before you delete it. You will need to
-- add back these routes to the transit gateway after the VPN connection
-- migration is complete.
--
-- After you migrate to the new gateway, you might need to modify your VPC
-- route table. Use CreateRoute and DeleteRoute to make the changes
-- described in
-- <https://docs.aws.amazon.com/vpn/latest/s2svpn/modify-vpn-target.html#step-update-routing VPN Gateway Target Modification Required VPC Route Table Updates>
-- in the /AWS Site-to-Site VPN User Guide/.
--
-- When the new gateway is a transit gateway, modify the transit gateway
-- route table to allow traffic between the VPC and the AWS Site-to-Site
-- VPN connection. Use CreateTransitGatewayRoute to add the routes.
--
-- If you deleted VPN static routes, you must add the static routes to the
-- transit gateway route table.
--
-- After you perform this operation, the AWS VPN endpoint\'s IP addresses
-- on the AWS side and the tunnel options remain intact. Your AWS
-- Site-to-Site VPN connection will be temporarily unavailable for a brief
-- period while we provision the new endpoints.
module Network.AWS.EC2.ModifyVpnConnection
  ( -- * Creating a Request
    ModifyVpnConnection (..),
    newModifyVpnConnection,

    -- * Request Lenses
    modifyVpnConnection_dryRun,
    modifyVpnConnection_customerGatewayId,
    modifyVpnConnection_vpnGatewayId,
    modifyVpnConnection_transitGatewayId,
    modifyVpnConnection_vpnConnectionId,

    -- * Destructuring the Response
    ModifyVpnConnectionResponse (..),
    newModifyVpnConnectionResponse,

    -- * Response Lenses
    modifyVpnConnectionResponse_vpnConnection,
    modifyVpnConnectionResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyVpnConnection' smart constructor.
data ModifyVpnConnection = ModifyVpnConnection'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the customer gateway at your end of the VPN connection.
    customerGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual private gateway at the AWS side of the VPN
    -- connection.
    vpnGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPN connection.
    vpnConnectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpnConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyVpnConnection_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'customerGatewayId', 'modifyVpnConnection_customerGatewayId' - The ID of the customer gateway at your end of the VPN connection.
--
-- 'vpnGatewayId', 'modifyVpnConnection_vpnGatewayId' - The ID of the virtual private gateway at the AWS side of the VPN
-- connection.
--
-- 'transitGatewayId', 'modifyVpnConnection_transitGatewayId' - The ID of the transit gateway.
--
-- 'vpnConnectionId', 'modifyVpnConnection_vpnConnectionId' - The ID of the VPN connection.
newModifyVpnConnection ::
  -- | 'vpnConnectionId'
  Prelude.Text ->
  ModifyVpnConnection
newModifyVpnConnection pVpnConnectionId_ =
  ModifyVpnConnection'
    { dryRun = Prelude.Nothing,
      customerGatewayId = Prelude.Nothing,
      vpnGatewayId = Prelude.Nothing,
      transitGatewayId = Prelude.Nothing,
      vpnConnectionId = pVpnConnectionId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpnConnection_dryRun :: Lens.Lens' ModifyVpnConnection (Prelude.Maybe Prelude.Bool)
modifyVpnConnection_dryRun = Lens.lens (\ModifyVpnConnection' {dryRun} -> dryRun) (\s@ModifyVpnConnection' {} a -> s {dryRun = a} :: ModifyVpnConnection)

-- | The ID of the customer gateway at your end of the VPN connection.
modifyVpnConnection_customerGatewayId :: Lens.Lens' ModifyVpnConnection (Prelude.Maybe Prelude.Text)
modifyVpnConnection_customerGatewayId = Lens.lens (\ModifyVpnConnection' {customerGatewayId} -> customerGatewayId) (\s@ModifyVpnConnection' {} a -> s {customerGatewayId = a} :: ModifyVpnConnection)

-- | The ID of the virtual private gateway at the AWS side of the VPN
-- connection.
modifyVpnConnection_vpnGatewayId :: Lens.Lens' ModifyVpnConnection (Prelude.Maybe Prelude.Text)
modifyVpnConnection_vpnGatewayId = Lens.lens (\ModifyVpnConnection' {vpnGatewayId} -> vpnGatewayId) (\s@ModifyVpnConnection' {} a -> s {vpnGatewayId = a} :: ModifyVpnConnection)

-- | The ID of the transit gateway.
modifyVpnConnection_transitGatewayId :: Lens.Lens' ModifyVpnConnection (Prelude.Maybe Prelude.Text)
modifyVpnConnection_transitGatewayId = Lens.lens (\ModifyVpnConnection' {transitGatewayId} -> transitGatewayId) (\s@ModifyVpnConnection' {} a -> s {transitGatewayId = a} :: ModifyVpnConnection)

-- | The ID of the VPN connection.
modifyVpnConnection_vpnConnectionId :: Lens.Lens' ModifyVpnConnection Prelude.Text
modifyVpnConnection_vpnConnectionId = Lens.lens (\ModifyVpnConnection' {vpnConnectionId} -> vpnConnectionId) (\s@ModifyVpnConnection' {} a -> s {vpnConnectionId = a} :: ModifyVpnConnection)

instance Prelude.AWSRequest ModifyVpnConnection where
  type
    Rs ModifyVpnConnection =
      ModifyVpnConnectionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpnConnectionResponse'
            Prelude.<$> (x Prelude..@? "vpnConnection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyVpnConnection

instance Prelude.NFData ModifyVpnConnection

instance Prelude.ToHeaders ModifyVpnConnection where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ModifyVpnConnection where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyVpnConnection where
  toQuery ModifyVpnConnection' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ModifyVpnConnection" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "CustomerGatewayId" Prelude.=: customerGatewayId,
        "VpnGatewayId" Prelude.=: vpnGatewayId,
        "TransitGatewayId" Prelude.=: transitGatewayId,
        "VpnConnectionId" Prelude.=: vpnConnectionId
      ]

-- | /See:/ 'newModifyVpnConnectionResponse' smart constructor.
data ModifyVpnConnectionResponse = ModifyVpnConnectionResponse'
  { vpnConnection :: Prelude.Maybe VpnConnection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpnConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpnConnection', 'modifyVpnConnectionResponse_vpnConnection' - Undocumented member.
--
-- 'httpStatus', 'modifyVpnConnectionResponse_httpStatus' - The response's http status code.
newModifyVpnConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyVpnConnectionResponse
newModifyVpnConnectionResponse pHttpStatus_ =
  ModifyVpnConnectionResponse'
    { vpnConnection =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyVpnConnectionResponse_vpnConnection :: Lens.Lens' ModifyVpnConnectionResponse (Prelude.Maybe VpnConnection)
modifyVpnConnectionResponse_vpnConnection = Lens.lens (\ModifyVpnConnectionResponse' {vpnConnection} -> vpnConnection) (\s@ModifyVpnConnectionResponse' {} a -> s {vpnConnection = a} :: ModifyVpnConnectionResponse)

-- | The response's http status code.
modifyVpnConnectionResponse_httpStatus :: Lens.Lens' ModifyVpnConnectionResponse Prelude.Int
modifyVpnConnectionResponse_httpStatus = Lens.lens (\ModifyVpnConnectionResponse' {httpStatus} -> httpStatus) (\s@ModifyVpnConnectionResponse' {} a -> s {httpStatus = a} :: ModifyVpnConnectionResponse)

instance Prelude.NFData ModifyVpnConnectionResponse
