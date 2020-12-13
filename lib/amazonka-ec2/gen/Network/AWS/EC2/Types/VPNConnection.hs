{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPNConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPNConnection
  ( VPNConnection (..),

    -- * Smart constructor
    mkVPNConnection,

    -- * Lenses
    vcCustomerGatewayConfiguration,
    vcState,
    vcRoutes,
    vcVPNGatewayId,
    vcCategory,
    vcCustomerGatewayId,
    vcTransitGatewayId,
    vcType,
    vcOptions,
    vcVPNConnectionId,
    vcTags,
    vcVGWTelemetry,
  )
where

import Network.AWS.EC2.Types.GatewayType
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.VGWTelemetry
import Network.AWS.EC2.Types.VPNConnectionOptions
import Network.AWS.EC2.Types.VPNState
import Network.AWS.EC2.Types.VPNStaticRoute
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a VPN connection.
--
-- /See:/ 'mkVPNConnection' smart constructor.
data VPNConnection = VPNConnection'
  { -- | The configuration information for the VPN connection's customer gateway (in the native XML format). This element is always present in the 'CreateVpnConnection' response; however, it's present in the 'DescribeVpnConnections' response only if the VPN connection is in the @pending@ or @available@ state.
    customerGatewayConfiguration :: Lude.Maybe Lude.Text,
    -- | The current state of the VPN connection.
    state :: VPNState,
    -- | The static routes associated with the VPN connection.
    routes :: Lude.Maybe [VPNStaticRoute],
    -- | The ID of the virtual private gateway at the AWS side of the VPN connection.
    vpnGatewayId :: Lude.Maybe Lude.Text,
    -- | The category of the VPN connection. A value of @VPN@ indicates an AWS VPN connection. A value of @VPN-Classic@ indicates an AWS Classic VPN connection.
    category :: Lude.Maybe Lude.Text,
    -- | The ID of the customer gateway at your end of the VPN connection.
    customerGatewayId :: Lude.Text,
    -- | The ID of the transit gateway associated with the VPN connection.
    transitGatewayId :: Lude.Maybe Lude.Text,
    -- | The type of VPN connection.
    type' :: GatewayType,
    -- | The VPN connection options.
    options :: Lude.Maybe VPNConnectionOptions,
    -- | The ID of the VPN connection.
    vpnConnectionId :: Lude.Text,
    -- | Any tags assigned to the VPN connection.
    tags :: Lude.Maybe [Tag],
    -- | Information about the VPN tunnel.
    vgwTelemetry :: Lude.Maybe [VGWTelemetry]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPNConnection' with the minimum fields required to make a request.
--
-- * 'customerGatewayConfiguration' - The configuration information for the VPN connection's customer gateway (in the native XML format). This element is always present in the 'CreateVpnConnection' response; however, it's present in the 'DescribeVpnConnections' response only if the VPN connection is in the @pending@ or @available@ state.
-- * 'state' - The current state of the VPN connection.
-- * 'routes' - The static routes associated with the VPN connection.
-- * 'vpnGatewayId' - The ID of the virtual private gateway at the AWS side of the VPN connection.
-- * 'category' - The category of the VPN connection. A value of @VPN@ indicates an AWS VPN connection. A value of @VPN-Classic@ indicates an AWS Classic VPN connection.
-- * 'customerGatewayId' - The ID of the customer gateway at your end of the VPN connection.
-- * 'transitGatewayId' - The ID of the transit gateway associated with the VPN connection.
-- * 'type'' - The type of VPN connection.
-- * 'options' - The VPN connection options.
-- * 'vpnConnectionId' - The ID of the VPN connection.
-- * 'tags' - Any tags assigned to the VPN connection.
-- * 'vgwTelemetry' - Information about the VPN tunnel.
mkVPNConnection ::
  -- | 'state'
  VPNState ->
  -- | 'customerGatewayId'
  Lude.Text ->
  -- | 'type''
  GatewayType ->
  -- | 'vpnConnectionId'
  Lude.Text ->
  VPNConnection
mkVPNConnection
  pState_
  pCustomerGatewayId_
  pType_
  pVPNConnectionId_ =
    VPNConnection'
      { customerGatewayConfiguration = Lude.Nothing,
        state = pState_,
        routes = Lude.Nothing,
        vpnGatewayId = Lude.Nothing,
        category = Lude.Nothing,
        customerGatewayId = pCustomerGatewayId_,
        transitGatewayId = Lude.Nothing,
        type' = pType_,
        options = Lude.Nothing,
        vpnConnectionId = pVPNConnectionId_,
        tags = Lude.Nothing,
        vgwTelemetry = Lude.Nothing
      }

-- | The configuration information for the VPN connection's customer gateway (in the native XML format). This element is always present in the 'CreateVpnConnection' response; however, it's present in the 'DescribeVpnConnections' response only if the VPN connection is in the @pending@ or @available@ state.
--
-- /Note:/ Consider using 'customerGatewayConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcCustomerGatewayConfiguration :: Lens.Lens' VPNConnection (Lude.Maybe Lude.Text)
vcCustomerGatewayConfiguration = Lens.lens (customerGatewayConfiguration :: VPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {customerGatewayConfiguration = a} :: VPNConnection)
{-# DEPRECATED vcCustomerGatewayConfiguration "Use generic-lens or generic-optics with 'customerGatewayConfiguration' instead." #-}

-- | The current state of the VPN connection.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcState :: Lens.Lens' VPNConnection VPNState
vcState = Lens.lens (state :: VPNConnection -> VPNState) (\s a -> s {state = a} :: VPNConnection)
{-# DEPRECATED vcState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The static routes associated with the VPN connection.
--
-- /Note:/ Consider using 'routes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcRoutes :: Lens.Lens' VPNConnection (Lude.Maybe [VPNStaticRoute])
vcRoutes = Lens.lens (routes :: VPNConnection -> Lude.Maybe [VPNStaticRoute]) (\s a -> s {routes = a} :: VPNConnection)
{-# DEPRECATED vcRoutes "Use generic-lens or generic-optics with 'routes' instead." #-}

-- | The ID of the virtual private gateway at the AWS side of the VPN connection.
--
-- /Note:/ Consider using 'vpnGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcVPNGatewayId :: Lens.Lens' VPNConnection (Lude.Maybe Lude.Text)
vcVPNGatewayId = Lens.lens (vpnGatewayId :: VPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {vpnGatewayId = a} :: VPNConnection)
{-# DEPRECATED vcVPNGatewayId "Use generic-lens or generic-optics with 'vpnGatewayId' instead." #-}

-- | The category of the VPN connection. A value of @VPN@ indicates an AWS VPN connection. A value of @VPN-Classic@ indicates an AWS Classic VPN connection.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcCategory :: Lens.Lens' VPNConnection (Lude.Maybe Lude.Text)
vcCategory = Lens.lens (category :: VPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {category = a} :: VPNConnection)
{-# DEPRECATED vcCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | The ID of the customer gateway at your end of the VPN connection.
--
-- /Note:/ Consider using 'customerGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcCustomerGatewayId :: Lens.Lens' VPNConnection Lude.Text
vcCustomerGatewayId = Lens.lens (customerGatewayId :: VPNConnection -> Lude.Text) (\s a -> s {customerGatewayId = a} :: VPNConnection)
{-# DEPRECATED vcCustomerGatewayId "Use generic-lens or generic-optics with 'customerGatewayId' instead." #-}

-- | The ID of the transit gateway associated with the VPN connection.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcTransitGatewayId :: Lens.Lens' VPNConnection (Lude.Maybe Lude.Text)
vcTransitGatewayId = Lens.lens (transitGatewayId :: VPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayId = a} :: VPNConnection)
{-# DEPRECATED vcTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | The type of VPN connection.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcType :: Lens.Lens' VPNConnection GatewayType
vcType = Lens.lens (type' :: VPNConnection -> GatewayType) (\s a -> s {type' = a} :: VPNConnection)
{-# DEPRECATED vcType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The VPN connection options.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcOptions :: Lens.Lens' VPNConnection (Lude.Maybe VPNConnectionOptions)
vcOptions = Lens.lens (options :: VPNConnection -> Lude.Maybe VPNConnectionOptions) (\s a -> s {options = a} :: VPNConnection)
{-# DEPRECATED vcOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The ID of the VPN connection.
--
-- /Note:/ Consider using 'vpnConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcVPNConnectionId :: Lens.Lens' VPNConnection Lude.Text
vcVPNConnectionId = Lens.lens (vpnConnectionId :: VPNConnection -> Lude.Text) (\s a -> s {vpnConnectionId = a} :: VPNConnection)
{-# DEPRECATED vcVPNConnectionId "Use generic-lens or generic-optics with 'vpnConnectionId' instead." #-}

-- | Any tags assigned to the VPN connection.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcTags :: Lens.Lens' VPNConnection (Lude.Maybe [Tag])
vcTags = Lens.lens (tags :: VPNConnection -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: VPNConnection)
{-# DEPRECATED vcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Information about the VPN tunnel.
--
-- /Note:/ Consider using 'vgwTelemetry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcVGWTelemetry :: Lens.Lens' VPNConnection (Lude.Maybe [VGWTelemetry])
vcVGWTelemetry = Lens.lens (vgwTelemetry :: VPNConnection -> Lude.Maybe [VGWTelemetry]) (\s a -> s {vgwTelemetry = a} :: VPNConnection)
{-# DEPRECATED vcVGWTelemetry "Use generic-lens or generic-optics with 'vgwTelemetry' instead." #-}

instance Lude.FromXML VPNConnection where
  parseXML x =
    VPNConnection'
      Lude.<$> (x Lude..@? "customerGatewayConfiguration")
      Lude.<*> (x Lude..@ "state")
      Lude.<*> ( x Lude..@? "routes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "vpnGatewayId")
      Lude.<*> (x Lude..@? "category")
      Lude.<*> (x Lude..@ "customerGatewayId")
      Lude.<*> (x Lude..@? "transitGatewayId")
      Lude.<*> (x Lude..@ "type")
      Lude.<*> (x Lude..@? "options")
      Lude.<*> (x Lude..@ "vpnConnectionId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "vgwTelemetry" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
