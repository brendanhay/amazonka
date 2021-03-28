{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVpnConnectionRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a static route associated with a VPN connection between an existing virtual private gateway and a VPN customer gateway. The static route allows traffic to be routed from the virtual private gateway to the VPN customer gateway.
--
-- For more information, see <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html AWS Site-to-Site VPN> in the /AWS Site-to-Site VPN User Guide/ .
module Network.AWS.EC2.CreateVpnConnectionRoute
    (
    -- * Creating a request
      CreateVpnConnectionRoute (..)
    , mkCreateVpnConnectionRoute
    -- ** Request lenses
    , cvcrDestinationCidrBlock
    , cvcrVpnConnectionId

    -- * Destructuring the response
    , CreateVpnConnectionRouteResponse (..)
    , mkCreateVpnConnectionRouteResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateVpnConnectionRoute.
--
-- /See:/ 'mkCreateVpnConnectionRoute' smart constructor.
data CreateVpnConnectionRoute = CreateVpnConnectionRoute'
  { destinationCidrBlock :: Core.Text
    -- ^ The CIDR block associated with the local subnet of the customer network.
  , vpnConnectionId :: Types.VpnConnectionId
    -- ^ The ID of the VPN connection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVpnConnectionRoute' value with any optional fields omitted.
mkCreateVpnConnectionRoute
    :: Core.Text -- ^ 'destinationCidrBlock'
    -> Types.VpnConnectionId -- ^ 'vpnConnectionId'
    -> CreateVpnConnectionRoute
mkCreateVpnConnectionRoute destinationCidrBlock vpnConnectionId
  = CreateVpnConnectionRoute'{destinationCidrBlock, vpnConnectionId}

-- | The CIDR block associated with the local subnet of the customer network.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcrDestinationCidrBlock :: Lens.Lens' CreateVpnConnectionRoute Core.Text
cvcrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# INLINEABLE cvcrDestinationCidrBlock #-}
{-# DEPRECATED destinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead"  #-}

-- | The ID of the VPN connection.
--
-- /Note:/ Consider using 'vpnConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcrVpnConnectionId :: Lens.Lens' CreateVpnConnectionRoute Types.VpnConnectionId
cvcrVpnConnectionId = Lens.field @"vpnConnectionId"
{-# INLINEABLE cvcrVpnConnectionId #-}
{-# DEPRECATED vpnConnectionId "Use generic-lens or generic-optics with 'vpnConnectionId' instead"  #-}

instance Core.ToQuery CreateVpnConnectionRoute where
        toQuery CreateVpnConnectionRoute{..}
          = Core.toQueryPair "Action"
              ("CreateVpnConnectionRoute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "DestinationCidrBlock" destinationCidrBlock
              Core.<> Core.toQueryPair "VpnConnectionId" vpnConnectionId

instance Core.ToHeaders CreateVpnConnectionRoute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateVpnConnectionRoute where
        type Rs CreateVpnConnectionRoute = CreateVpnConnectionRouteResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull CreateVpnConnectionRouteResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateVpnConnectionRouteResponse' smart constructor.
data CreateVpnConnectionRouteResponse = CreateVpnConnectionRouteResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVpnConnectionRouteResponse' value with any optional fields omitted.
mkCreateVpnConnectionRouteResponse
    :: CreateVpnConnectionRouteResponse
mkCreateVpnConnectionRouteResponse
  = CreateVpnConnectionRouteResponse'
