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
-- Module      : Network.AWS.EC2.CreateVpnConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPN connection between an existing virtual private gateway or
-- transit gateway and a customer gateway. The supported connection type is
-- @ipsec.1@.
--
-- The response includes information that you need to give to your network
-- administrator to configure your customer gateway.
--
-- We strongly recommend that you use HTTPS when calling this operation
-- because the response contains sensitive cryptographic information for
-- configuring your customer gateway device.
--
-- If you decide to shut down your VPN connection for any reason and later
-- create a new VPN connection, you must reconfigure your customer gateway
-- with the new information returned from this call.
--
-- This is an idempotent operation. If you perform the operation more than
-- once, Amazon EC2 doesn\'t return an error.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html AWS Site-to-Site VPN>
-- in the /AWS Site-to-Site VPN User Guide/.
module Network.AWS.EC2.CreateVpnConnection
  ( -- * Creating a Request
    CreateVpnConnection (..),
    newCreateVpnConnection,

    -- * Request Lenses
    createVpnConnection_tagSpecifications,
    createVpnConnection_dryRun,
    createVpnConnection_options,
    createVpnConnection_vpnGatewayId,
    createVpnConnection_transitGatewayId,
    createVpnConnection_customerGatewayId,
    createVpnConnection_type,

    -- * Destructuring the Response
    CreateVpnConnectionResponse (..),
    newCreateVpnConnectionResponse,

    -- * Response Lenses
    createVpnConnectionResponse_vpnConnection,
    createVpnConnectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateVpnConnection.
--
-- /See:/ 'newCreateVpnConnection' smart constructor.
data CreateVpnConnection = CreateVpnConnection'
  { -- | The tags to apply to the VPN connection.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The options for the VPN connection.
    options :: Prelude.Maybe VpnConnectionOptionsSpecification,
    -- | The ID of the virtual private gateway. If you specify a virtual private
    -- gateway, you cannot specify a transit gateway.
    vpnGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway. If you specify a transit gateway, you
    -- cannot specify a virtual private gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the customer gateway.
    customerGatewayId :: Prelude.Text,
    -- | The type of VPN connection (@ipsec.1@).
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpnConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createVpnConnection_tagSpecifications' - The tags to apply to the VPN connection.
--
-- 'dryRun', 'createVpnConnection_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'options', 'createVpnConnection_options' - The options for the VPN connection.
--
-- 'vpnGatewayId', 'createVpnConnection_vpnGatewayId' - The ID of the virtual private gateway. If you specify a virtual private
-- gateway, you cannot specify a transit gateway.
--
-- 'transitGatewayId', 'createVpnConnection_transitGatewayId' - The ID of the transit gateway. If you specify a transit gateway, you
-- cannot specify a virtual private gateway.
--
-- 'customerGatewayId', 'createVpnConnection_customerGatewayId' - The ID of the customer gateway.
--
-- 'type'', 'createVpnConnection_type' - The type of VPN connection (@ipsec.1@).
newCreateVpnConnection ::
  -- | 'customerGatewayId'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  CreateVpnConnection
newCreateVpnConnection pCustomerGatewayId_ pType_ =
  CreateVpnConnection'
    { tagSpecifications =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      options = Prelude.Nothing,
      vpnGatewayId = Prelude.Nothing,
      transitGatewayId = Prelude.Nothing,
      customerGatewayId = pCustomerGatewayId_,
      type' = pType_
    }

-- | The tags to apply to the VPN connection.
createVpnConnection_tagSpecifications :: Lens.Lens' CreateVpnConnection (Prelude.Maybe [TagSpecification])
createVpnConnection_tagSpecifications = Lens.lens (\CreateVpnConnection' {tagSpecifications} -> tagSpecifications) (\s@CreateVpnConnection' {} a -> s {tagSpecifications = a} :: CreateVpnConnection) Prelude.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createVpnConnection_dryRun :: Lens.Lens' CreateVpnConnection (Prelude.Maybe Prelude.Bool)
createVpnConnection_dryRun = Lens.lens (\CreateVpnConnection' {dryRun} -> dryRun) (\s@CreateVpnConnection' {} a -> s {dryRun = a} :: CreateVpnConnection)

-- | The options for the VPN connection.
createVpnConnection_options :: Lens.Lens' CreateVpnConnection (Prelude.Maybe VpnConnectionOptionsSpecification)
createVpnConnection_options = Lens.lens (\CreateVpnConnection' {options} -> options) (\s@CreateVpnConnection' {} a -> s {options = a} :: CreateVpnConnection)

-- | The ID of the virtual private gateway. If you specify a virtual private
-- gateway, you cannot specify a transit gateway.
createVpnConnection_vpnGatewayId :: Lens.Lens' CreateVpnConnection (Prelude.Maybe Prelude.Text)
createVpnConnection_vpnGatewayId = Lens.lens (\CreateVpnConnection' {vpnGatewayId} -> vpnGatewayId) (\s@CreateVpnConnection' {} a -> s {vpnGatewayId = a} :: CreateVpnConnection)

-- | The ID of the transit gateway. If you specify a transit gateway, you
-- cannot specify a virtual private gateway.
createVpnConnection_transitGatewayId :: Lens.Lens' CreateVpnConnection (Prelude.Maybe Prelude.Text)
createVpnConnection_transitGatewayId = Lens.lens (\CreateVpnConnection' {transitGatewayId} -> transitGatewayId) (\s@CreateVpnConnection' {} a -> s {transitGatewayId = a} :: CreateVpnConnection)

-- | The ID of the customer gateway.
createVpnConnection_customerGatewayId :: Lens.Lens' CreateVpnConnection Prelude.Text
createVpnConnection_customerGatewayId = Lens.lens (\CreateVpnConnection' {customerGatewayId} -> customerGatewayId) (\s@CreateVpnConnection' {} a -> s {customerGatewayId = a} :: CreateVpnConnection)

-- | The type of VPN connection (@ipsec.1@).
createVpnConnection_type :: Lens.Lens' CreateVpnConnection Prelude.Text
createVpnConnection_type = Lens.lens (\CreateVpnConnection' {type'} -> type') (\s@CreateVpnConnection' {} a -> s {type' = a} :: CreateVpnConnection)

instance Core.AWSRequest CreateVpnConnection where
  type
    AWSResponse CreateVpnConnection =
      CreateVpnConnectionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateVpnConnectionResponse'
            Prelude.<$> (x Core..@? "vpnConnection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVpnConnection

instance Prelude.NFData CreateVpnConnection

instance Core.ToHeaders CreateVpnConnection where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateVpnConnection where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateVpnConnection where
  toQuery CreateVpnConnection' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateVpnConnection" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "Options" Core.=: options,
        "VpnGatewayId" Core.=: vpnGatewayId,
        "TransitGatewayId" Core.=: transitGatewayId,
        "CustomerGatewayId" Core.=: customerGatewayId,
        "Type" Core.=: type'
      ]

-- | Contains the output of CreateVpnConnection.
--
-- /See:/ 'newCreateVpnConnectionResponse' smart constructor.
data CreateVpnConnectionResponse = CreateVpnConnectionResponse'
  { -- | Information about the VPN connection.
    vpnConnection :: Prelude.Maybe VpnConnection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpnConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpnConnection', 'createVpnConnectionResponse_vpnConnection' - Information about the VPN connection.
--
-- 'httpStatus', 'createVpnConnectionResponse_httpStatus' - The response's http status code.
newCreateVpnConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVpnConnectionResponse
newCreateVpnConnectionResponse pHttpStatus_ =
  CreateVpnConnectionResponse'
    { vpnConnection =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the VPN connection.
createVpnConnectionResponse_vpnConnection :: Lens.Lens' CreateVpnConnectionResponse (Prelude.Maybe VpnConnection)
createVpnConnectionResponse_vpnConnection = Lens.lens (\CreateVpnConnectionResponse' {vpnConnection} -> vpnConnection) (\s@CreateVpnConnectionResponse' {} a -> s {vpnConnection = a} :: CreateVpnConnectionResponse)

-- | The response's http status code.
createVpnConnectionResponse_httpStatus :: Lens.Lens' CreateVpnConnectionResponse Prelude.Int
createVpnConnectionResponse_httpStatus = Lens.lens (\CreateVpnConnectionResponse' {httpStatus} -> httpStatus) (\s@CreateVpnConnectionResponse' {} a -> s {httpStatus = a} :: CreateVpnConnectionResponse)

instance Prelude.NFData CreateVpnConnectionResponse
