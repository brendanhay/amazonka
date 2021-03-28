{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpnConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VpnConnection
  ( VpnConnection (..)
  -- * Smart constructor
  , mkVpnConnection
  -- * Lenses
  , vcCategory
  , vcCustomerGatewayConfiguration
  , vcCustomerGatewayId
  , vcOptions
  , vcRoutes
  , vcState
  , vcTags
  , vcTransitGatewayId
  , vcType
  , vcVgwTelemetry
  , vcVpnConnectionId
  , vcVpnGatewayId
  ) where

import qualified Network.AWS.EC2.Types.GatewayType as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.VgwTelemetry as Types
import qualified Network.AWS.EC2.Types.VpnConnectionOptions as Types
import qualified Network.AWS.EC2.Types.VpnState as Types
import qualified Network.AWS.EC2.Types.VpnStaticRoute as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a VPN connection.
--
-- /See:/ 'mkVpnConnection' smart constructor.
data VpnConnection = VpnConnection'
  { category :: Core.Maybe Core.Text
    -- ^ The category of the VPN connection. A value of @VPN@ indicates an AWS VPN connection. A value of @VPN-Classic@ indicates an AWS Classic VPN connection.
  , customerGatewayConfiguration :: Core.Maybe Core.Text
    -- ^ The configuration information for the VPN connection's customer gateway (in the native XML format). This element is always present in the 'CreateVpnConnection' response; however, it's present in the 'DescribeVpnConnections' response only if the VPN connection is in the @pending@ or @available@ state.
  , customerGatewayId :: Core.Text
    -- ^ The ID of the customer gateway at your end of the VPN connection.
  , options :: Core.Maybe Types.VpnConnectionOptions
    -- ^ The VPN connection options.
  , routes :: Core.Maybe [Types.VpnStaticRoute]
    -- ^ The static routes associated with the VPN connection.
  , state :: Types.VpnState
    -- ^ The current state of the VPN connection.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the VPN connection.
  , transitGatewayId :: Core.Maybe Core.Text
    -- ^ The ID of the transit gateway associated with the VPN connection.
  , type' :: Types.GatewayType
    -- ^ The type of VPN connection.
  , vgwTelemetry :: Core.Maybe [Types.VgwTelemetry]
    -- ^ Information about the VPN tunnel.
  , vpnConnectionId :: Core.Text
    -- ^ The ID of the VPN connection.
  , vpnGatewayId :: Core.Maybe Core.Text
    -- ^ The ID of the virtual private gateway at the AWS side of the VPN connection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'VpnConnection' value with any optional fields omitted.
mkVpnConnection
    :: Core.Text -- ^ 'customerGatewayId'
    -> Types.VpnState -- ^ 'state'
    -> Types.GatewayType -- ^ 'type\''
    -> Core.Text -- ^ 'vpnConnectionId'
    -> VpnConnection
mkVpnConnection customerGatewayId state type' vpnConnectionId
  = VpnConnection'{category = Core.Nothing,
                   customerGatewayConfiguration = Core.Nothing, customerGatewayId,
                   options = Core.Nothing, routes = Core.Nothing, state,
                   tags = Core.Nothing, transitGatewayId = Core.Nothing, type',
                   vgwTelemetry = Core.Nothing, vpnConnectionId,
                   vpnGatewayId = Core.Nothing}

-- | The category of the VPN connection. A value of @VPN@ indicates an AWS VPN connection. A value of @VPN-Classic@ indicates an AWS Classic VPN connection.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcCategory :: Lens.Lens' VpnConnection (Core.Maybe Core.Text)
vcCategory = Lens.field @"category"
{-# INLINEABLE vcCategory #-}
{-# DEPRECATED category "Use generic-lens or generic-optics with 'category' instead"  #-}

-- | The configuration information for the VPN connection's customer gateway (in the native XML format). This element is always present in the 'CreateVpnConnection' response; however, it's present in the 'DescribeVpnConnections' response only if the VPN connection is in the @pending@ or @available@ state.
--
-- /Note:/ Consider using 'customerGatewayConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcCustomerGatewayConfiguration :: Lens.Lens' VpnConnection (Core.Maybe Core.Text)
vcCustomerGatewayConfiguration = Lens.field @"customerGatewayConfiguration"
{-# INLINEABLE vcCustomerGatewayConfiguration #-}
{-# DEPRECATED customerGatewayConfiguration "Use generic-lens or generic-optics with 'customerGatewayConfiguration' instead"  #-}

-- | The ID of the customer gateway at your end of the VPN connection.
--
-- /Note:/ Consider using 'customerGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcCustomerGatewayId :: Lens.Lens' VpnConnection Core.Text
vcCustomerGatewayId = Lens.field @"customerGatewayId"
{-# INLINEABLE vcCustomerGatewayId #-}
{-# DEPRECATED customerGatewayId "Use generic-lens or generic-optics with 'customerGatewayId' instead"  #-}

-- | The VPN connection options.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcOptions :: Lens.Lens' VpnConnection (Core.Maybe Types.VpnConnectionOptions)
vcOptions = Lens.field @"options"
{-# INLINEABLE vcOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | The static routes associated with the VPN connection.
--
-- /Note:/ Consider using 'routes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcRoutes :: Lens.Lens' VpnConnection (Core.Maybe [Types.VpnStaticRoute])
vcRoutes = Lens.field @"routes"
{-# INLINEABLE vcRoutes #-}
{-# DEPRECATED routes "Use generic-lens or generic-optics with 'routes' instead"  #-}

-- | The current state of the VPN connection.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcState :: Lens.Lens' VpnConnection Types.VpnState
vcState = Lens.field @"state"
{-# INLINEABLE vcState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | Any tags assigned to the VPN connection.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcTags :: Lens.Lens' VpnConnection (Core.Maybe [Types.Tag])
vcTags = Lens.field @"tags"
{-# INLINEABLE vcTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ID of the transit gateway associated with the VPN connection.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcTransitGatewayId :: Lens.Lens' VpnConnection (Core.Maybe Core.Text)
vcTransitGatewayId = Lens.field @"transitGatewayId"
{-# INLINEABLE vcTransitGatewayId #-}
{-# DEPRECATED transitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead"  #-}

-- | The type of VPN connection.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcType :: Lens.Lens' VpnConnection Types.GatewayType
vcType = Lens.field @"type'"
{-# INLINEABLE vcType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | Information about the VPN tunnel.
--
-- /Note:/ Consider using 'vgwTelemetry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcVgwTelemetry :: Lens.Lens' VpnConnection (Core.Maybe [Types.VgwTelemetry])
vcVgwTelemetry = Lens.field @"vgwTelemetry"
{-# INLINEABLE vcVgwTelemetry #-}
{-# DEPRECATED vgwTelemetry "Use generic-lens or generic-optics with 'vgwTelemetry' instead"  #-}

-- | The ID of the VPN connection.
--
-- /Note:/ Consider using 'vpnConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcVpnConnectionId :: Lens.Lens' VpnConnection Core.Text
vcVpnConnectionId = Lens.field @"vpnConnectionId"
{-# INLINEABLE vcVpnConnectionId #-}
{-# DEPRECATED vpnConnectionId "Use generic-lens or generic-optics with 'vpnConnectionId' instead"  #-}

-- | The ID of the virtual private gateway at the AWS side of the VPN connection.
--
-- /Note:/ Consider using 'vpnGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcVpnGatewayId :: Lens.Lens' VpnConnection (Core.Maybe Core.Text)
vcVpnGatewayId = Lens.field @"vpnGatewayId"
{-# INLINEABLE vcVpnGatewayId #-}
{-# DEPRECATED vpnGatewayId "Use generic-lens or generic-optics with 'vpnGatewayId' instead"  #-}

instance Core.FromXML VpnConnection where
        parseXML x
          = VpnConnection' Core.<$>
              (x Core..@? "category") Core.<*>
                x Core..@? "customerGatewayConfiguration"
                Core.<*> x Core..@ "customerGatewayId"
                Core.<*> x Core..@? "options"
                Core.<*> x Core..@? "routes" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@ "state"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "transitGatewayId"
                Core.<*> x Core..@ "type"
                Core.<*>
                x Core..@? "vgwTelemetry" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@ "vpnConnectionId"
                Core.<*> x Core..@? "vpnGatewayId"
