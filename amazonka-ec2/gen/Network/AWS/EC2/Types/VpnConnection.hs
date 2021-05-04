{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpnConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VpnConnection where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.GatewayType
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.VgwTelemetry
import Network.AWS.EC2.Types.VpnConnectionOptions
import Network.AWS.EC2.Types.VpnState
import Network.AWS.EC2.Types.VpnStaticRoute
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a VPN connection.
--
-- /See:/ 'newVpnConnection' smart constructor.
data VpnConnection = VpnConnection'
  { -- | The configuration information for the VPN connection\'s customer gateway
    -- (in the native XML format). This element is always present in the
    -- CreateVpnConnection response; however, it\'s present in the
    -- DescribeVpnConnections response only if the VPN connection is in the
    -- @pending@ or @available@ state.
    customerGatewayConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The VPN connection options.
    options :: Prelude.Maybe VpnConnectionOptions,
    -- | The category of the VPN connection. A value of @VPN@ indicates an AWS
    -- VPN connection. A value of @VPN-Classic@ indicates an AWS Classic VPN
    -- connection.
    category :: Prelude.Maybe Prelude.Text,
    -- | The static routes associated with the VPN connection.
    routes :: Prelude.Maybe [VpnStaticRoute],
    -- | Any tags assigned to the VPN connection.
    tags :: Prelude.Maybe [Tag],
    -- | Information about the VPN tunnel.
    vgwTelemetry :: Prelude.Maybe [VgwTelemetry],
    -- | The ID of the virtual private gateway at the AWS side of the VPN
    -- connection.
    vpnGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway associated with the VPN connection.
    transitGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPN connection.
    vpnConnectionId :: Prelude.Text,
    -- | The ID of the customer gateway at your end of the VPN connection.
    customerGatewayId :: Prelude.Text,
    -- | The current state of the VPN connection.
    state :: VpnState,
    -- | The type of VPN connection.
    type' :: GatewayType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VpnConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerGatewayConfiguration', 'vpnConnection_customerGatewayConfiguration' - The configuration information for the VPN connection\'s customer gateway
-- (in the native XML format). This element is always present in the
-- CreateVpnConnection response; however, it\'s present in the
-- DescribeVpnConnections response only if the VPN connection is in the
-- @pending@ or @available@ state.
--
-- 'options', 'vpnConnection_options' - The VPN connection options.
--
-- 'category', 'vpnConnection_category' - The category of the VPN connection. A value of @VPN@ indicates an AWS
-- VPN connection. A value of @VPN-Classic@ indicates an AWS Classic VPN
-- connection.
--
-- 'routes', 'vpnConnection_routes' - The static routes associated with the VPN connection.
--
-- 'tags', 'vpnConnection_tags' - Any tags assigned to the VPN connection.
--
-- 'vgwTelemetry', 'vpnConnection_vgwTelemetry' - Information about the VPN tunnel.
--
-- 'vpnGatewayId', 'vpnConnection_vpnGatewayId' - The ID of the virtual private gateway at the AWS side of the VPN
-- connection.
--
-- 'transitGatewayId', 'vpnConnection_transitGatewayId' - The ID of the transit gateway associated with the VPN connection.
--
-- 'vpnConnectionId', 'vpnConnection_vpnConnectionId' - The ID of the VPN connection.
--
-- 'customerGatewayId', 'vpnConnection_customerGatewayId' - The ID of the customer gateway at your end of the VPN connection.
--
-- 'state', 'vpnConnection_state' - The current state of the VPN connection.
--
-- 'type'', 'vpnConnection_type' - The type of VPN connection.
newVpnConnection ::
  -- | 'vpnConnectionId'
  Prelude.Text ->
  -- | 'customerGatewayId'
  Prelude.Text ->
  -- | 'state'
  VpnState ->
  -- | 'type''
  GatewayType ->
  VpnConnection
newVpnConnection
  pVpnConnectionId_
  pCustomerGatewayId_
  pState_
  pType_ =
    VpnConnection'
      { customerGatewayConfiguration =
          Prelude.Nothing,
        options = Prelude.Nothing,
        category = Prelude.Nothing,
        routes = Prelude.Nothing,
        tags = Prelude.Nothing,
        vgwTelemetry = Prelude.Nothing,
        vpnGatewayId = Prelude.Nothing,
        transitGatewayId = Prelude.Nothing,
        vpnConnectionId = pVpnConnectionId_,
        customerGatewayId = pCustomerGatewayId_,
        state = pState_,
        type' = pType_
      }

-- | The configuration information for the VPN connection\'s customer gateway
-- (in the native XML format). This element is always present in the
-- CreateVpnConnection response; however, it\'s present in the
-- DescribeVpnConnections response only if the VPN connection is in the
-- @pending@ or @available@ state.
vpnConnection_customerGatewayConfiguration :: Lens.Lens' VpnConnection (Prelude.Maybe Prelude.Text)
vpnConnection_customerGatewayConfiguration = Lens.lens (\VpnConnection' {customerGatewayConfiguration} -> customerGatewayConfiguration) (\s@VpnConnection' {} a -> s {customerGatewayConfiguration = a} :: VpnConnection)

-- | The VPN connection options.
vpnConnection_options :: Lens.Lens' VpnConnection (Prelude.Maybe VpnConnectionOptions)
vpnConnection_options = Lens.lens (\VpnConnection' {options} -> options) (\s@VpnConnection' {} a -> s {options = a} :: VpnConnection)

-- | The category of the VPN connection. A value of @VPN@ indicates an AWS
-- VPN connection. A value of @VPN-Classic@ indicates an AWS Classic VPN
-- connection.
vpnConnection_category :: Lens.Lens' VpnConnection (Prelude.Maybe Prelude.Text)
vpnConnection_category = Lens.lens (\VpnConnection' {category} -> category) (\s@VpnConnection' {} a -> s {category = a} :: VpnConnection)

-- | The static routes associated with the VPN connection.
vpnConnection_routes :: Lens.Lens' VpnConnection (Prelude.Maybe [VpnStaticRoute])
vpnConnection_routes = Lens.lens (\VpnConnection' {routes} -> routes) (\s@VpnConnection' {} a -> s {routes = a} :: VpnConnection) Prelude.. Lens.mapping Prelude._Coerce

-- | Any tags assigned to the VPN connection.
vpnConnection_tags :: Lens.Lens' VpnConnection (Prelude.Maybe [Tag])
vpnConnection_tags = Lens.lens (\VpnConnection' {tags} -> tags) (\s@VpnConnection' {} a -> s {tags = a} :: VpnConnection) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the VPN tunnel.
vpnConnection_vgwTelemetry :: Lens.Lens' VpnConnection (Prelude.Maybe [VgwTelemetry])
vpnConnection_vgwTelemetry = Lens.lens (\VpnConnection' {vgwTelemetry} -> vgwTelemetry) (\s@VpnConnection' {} a -> s {vgwTelemetry = a} :: VpnConnection) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the virtual private gateway at the AWS side of the VPN
-- connection.
vpnConnection_vpnGatewayId :: Lens.Lens' VpnConnection (Prelude.Maybe Prelude.Text)
vpnConnection_vpnGatewayId = Lens.lens (\VpnConnection' {vpnGatewayId} -> vpnGatewayId) (\s@VpnConnection' {} a -> s {vpnGatewayId = a} :: VpnConnection)

-- | The ID of the transit gateway associated with the VPN connection.
vpnConnection_transitGatewayId :: Lens.Lens' VpnConnection (Prelude.Maybe Prelude.Text)
vpnConnection_transitGatewayId = Lens.lens (\VpnConnection' {transitGatewayId} -> transitGatewayId) (\s@VpnConnection' {} a -> s {transitGatewayId = a} :: VpnConnection)

-- | The ID of the VPN connection.
vpnConnection_vpnConnectionId :: Lens.Lens' VpnConnection Prelude.Text
vpnConnection_vpnConnectionId = Lens.lens (\VpnConnection' {vpnConnectionId} -> vpnConnectionId) (\s@VpnConnection' {} a -> s {vpnConnectionId = a} :: VpnConnection)

-- | The ID of the customer gateway at your end of the VPN connection.
vpnConnection_customerGatewayId :: Lens.Lens' VpnConnection Prelude.Text
vpnConnection_customerGatewayId = Lens.lens (\VpnConnection' {customerGatewayId} -> customerGatewayId) (\s@VpnConnection' {} a -> s {customerGatewayId = a} :: VpnConnection)

-- | The current state of the VPN connection.
vpnConnection_state :: Lens.Lens' VpnConnection VpnState
vpnConnection_state = Lens.lens (\VpnConnection' {state} -> state) (\s@VpnConnection' {} a -> s {state = a} :: VpnConnection)

-- | The type of VPN connection.
vpnConnection_type :: Lens.Lens' VpnConnection GatewayType
vpnConnection_type = Lens.lens (\VpnConnection' {type'} -> type') (\s@VpnConnection' {} a -> s {type' = a} :: VpnConnection)

instance Prelude.FromXML VpnConnection where
  parseXML x =
    VpnConnection'
      Prelude.<$> (x Prelude..@? "customerGatewayConfiguration")
      Prelude.<*> (x Prelude..@? "options")
      Prelude.<*> (x Prelude..@? "category")
      Prelude.<*> ( x Prelude..@? "routes" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> ( x Prelude..@? "vgwTelemetry"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "vpnGatewayId")
      Prelude.<*> (x Prelude..@? "transitGatewayId")
      Prelude.<*> (x Prelude..@ "vpnConnectionId")
      Prelude.<*> (x Prelude..@ "customerGatewayId")
      Prelude.<*> (x Prelude..@ "state")
      Prelude.<*> (x Prelude..@ "type")

instance Prelude.Hashable VpnConnection

instance Prelude.NFData VpnConnection
