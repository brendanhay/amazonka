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
-- Module      : Amazonka.EC2.ModifyVpnConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the customer gateway or the target gateway of an Amazon Web
-- Services Site-to-Site VPN connection. To modify the target gateway, the
-- following migration options are available:
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
-- <https://docs.aws.amazon.com/vpn/latest/s2svpn/modify-vpn-target.html#step-update-routing Update VPC route tables>
-- in the /Amazon Web Services Site-to-Site VPN User Guide/.
--
-- When the new gateway is a transit gateway, modify the transit gateway
-- route table to allow traffic between the VPC and the Amazon Web Services
-- Site-to-Site VPN connection. Use CreateTransitGatewayRoute to add the
-- routes.
--
-- If you deleted VPN static routes, you must add the static routes to the
-- transit gateway route table.
--
-- After you perform this operation, the VPN endpoint\'s IP addresses on
-- the Amazon Web Services side and the tunnel options remain intact. Your
-- Amazon Web Services Site-to-Site VPN connection will be temporarily
-- unavailable for a brief period while we provision the new endpoints.
module Amazonka.EC2.ModifyVpnConnection
  ( -- * Creating a Request
    ModifyVpnConnection (..),
    newModifyVpnConnection,

    -- * Request Lenses
    modifyVpnConnection_customerGatewayId,
    modifyVpnConnection_dryRun,
    modifyVpnConnection_transitGatewayId,
    modifyVpnConnection_vpnGatewayId,
    modifyVpnConnection_vpnConnectionId,

    -- * Destructuring the Response
    ModifyVpnConnectionResponse (..),
    newModifyVpnConnectionResponse,

    -- * Response Lenses
    modifyVpnConnectionResponse_vpnConnection,
    modifyVpnConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyVpnConnection' smart constructor.
data ModifyVpnConnection = ModifyVpnConnection'
  { -- | The ID of the customer gateway at your end of the VPN connection.
    customerGatewayId :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual private gateway at the Amazon Web Services side of
    -- the VPN connection.
    vpnGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPN connection.
    vpnConnectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpnConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerGatewayId', 'modifyVpnConnection_customerGatewayId' - The ID of the customer gateway at your end of the VPN connection.
--
-- 'dryRun', 'modifyVpnConnection_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayId', 'modifyVpnConnection_transitGatewayId' - The ID of the transit gateway.
--
-- 'vpnGatewayId', 'modifyVpnConnection_vpnGatewayId' - The ID of the virtual private gateway at the Amazon Web Services side of
-- the VPN connection.
--
-- 'vpnConnectionId', 'modifyVpnConnection_vpnConnectionId' - The ID of the VPN connection.
newModifyVpnConnection ::
  -- | 'vpnConnectionId'
  Prelude.Text ->
  ModifyVpnConnection
newModifyVpnConnection pVpnConnectionId_ =
  ModifyVpnConnection'
    { customerGatewayId =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      transitGatewayId = Prelude.Nothing,
      vpnGatewayId = Prelude.Nothing,
      vpnConnectionId = pVpnConnectionId_
    }

-- | The ID of the customer gateway at your end of the VPN connection.
modifyVpnConnection_customerGatewayId :: Lens.Lens' ModifyVpnConnection (Prelude.Maybe Prelude.Text)
modifyVpnConnection_customerGatewayId = Lens.lens (\ModifyVpnConnection' {customerGatewayId} -> customerGatewayId) (\s@ModifyVpnConnection' {} a -> s {customerGatewayId = a} :: ModifyVpnConnection)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpnConnection_dryRun :: Lens.Lens' ModifyVpnConnection (Prelude.Maybe Prelude.Bool)
modifyVpnConnection_dryRun = Lens.lens (\ModifyVpnConnection' {dryRun} -> dryRun) (\s@ModifyVpnConnection' {} a -> s {dryRun = a} :: ModifyVpnConnection)

-- | The ID of the transit gateway.
modifyVpnConnection_transitGatewayId :: Lens.Lens' ModifyVpnConnection (Prelude.Maybe Prelude.Text)
modifyVpnConnection_transitGatewayId = Lens.lens (\ModifyVpnConnection' {transitGatewayId} -> transitGatewayId) (\s@ModifyVpnConnection' {} a -> s {transitGatewayId = a} :: ModifyVpnConnection)

-- | The ID of the virtual private gateway at the Amazon Web Services side of
-- the VPN connection.
modifyVpnConnection_vpnGatewayId :: Lens.Lens' ModifyVpnConnection (Prelude.Maybe Prelude.Text)
modifyVpnConnection_vpnGatewayId = Lens.lens (\ModifyVpnConnection' {vpnGatewayId} -> vpnGatewayId) (\s@ModifyVpnConnection' {} a -> s {vpnGatewayId = a} :: ModifyVpnConnection)

-- | The ID of the VPN connection.
modifyVpnConnection_vpnConnectionId :: Lens.Lens' ModifyVpnConnection Prelude.Text
modifyVpnConnection_vpnConnectionId = Lens.lens (\ModifyVpnConnection' {vpnConnectionId} -> vpnConnectionId) (\s@ModifyVpnConnection' {} a -> s {vpnConnectionId = a} :: ModifyVpnConnection)

instance Core.AWSRequest ModifyVpnConnection where
  type
    AWSResponse ModifyVpnConnection =
      ModifyVpnConnectionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpnConnectionResponse'
            Prelude.<$> (x Data..@? "vpnConnection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyVpnConnection where
  hashWithSalt _salt ModifyVpnConnection' {..} =
    _salt
      `Prelude.hashWithSalt` customerGatewayId
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` transitGatewayId
      `Prelude.hashWithSalt` vpnGatewayId
      `Prelude.hashWithSalt` vpnConnectionId

instance Prelude.NFData ModifyVpnConnection where
  rnf ModifyVpnConnection' {..} =
    Prelude.rnf customerGatewayId
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf transitGatewayId
      `Prelude.seq` Prelude.rnf vpnGatewayId
      `Prelude.seq` Prelude.rnf vpnConnectionId

instance Data.ToHeaders ModifyVpnConnection where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyVpnConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyVpnConnection where
  toQuery ModifyVpnConnection' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyVpnConnection" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "CustomerGatewayId" Data.=: customerGatewayId,
        "DryRun" Data.=: dryRun,
        "TransitGatewayId" Data.=: transitGatewayId,
        "VpnGatewayId" Data.=: vpnGatewayId,
        "VpnConnectionId" Data.=: vpnConnectionId
      ]

-- | /See:/ 'newModifyVpnConnectionResponse' smart constructor.
data ModifyVpnConnectionResponse = ModifyVpnConnectionResponse'
  { -- | Information about the VPN connection.
    vpnConnection :: Prelude.Maybe VpnConnection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpnConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpnConnection', 'modifyVpnConnectionResponse_vpnConnection' - Information about the VPN connection.
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

-- | Information about the VPN connection.
modifyVpnConnectionResponse_vpnConnection :: Lens.Lens' ModifyVpnConnectionResponse (Prelude.Maybe VpnConnection)
modifyVpnConnectionResponse_vpnConnection = Lens.lens (\ModifyVpnConnectionResponse' {vpnConnection} -> vpnConnection) (\s@ModifyVpnConnectionResponse' {} a -> s {vpnConnection = a} :: ModifyVpnConnectionResponse)

-- | The response's http status code.
modifyVpnConnectionResponse_httpStatus :: Lens.Lens' ModifyVpnConnectionResponse Prelude.Int
modifyVpnConnectionResponse_httpStatus = Lens.lens (\ModifyVpnConnectionResponse' {httpStatus} -> httpStatus) (\s@ModifyVpnConnectionResponse' {} a -> s {httpStatus = a} :: ModifyVpnConnectionResponse)

instance Prelude.NFData ModifyVpnConnectionResponse where
  rnf ModifyVpnConnectionResponse' {..} =
    Prelude.rnf vpnConnection
      `Prelude.seq` Prelude.rnf httpStatus
