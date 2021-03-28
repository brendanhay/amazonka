{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVpnConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the customer gateway or the target gateway of an AWS Site-to-Site VPN connection. To modify the target gateway, the following migration options are available:
--
--
--     * An existing virtual private gateway to a new virtual private gateway
--
--
--     * An existing virtual private gateway to a transit gateway
--
--
--     * An existing transit gateway to a new transit gateway
--
--
--     * An existing transit gateway to a virtual private gateway
--
--
-- Before you perform the migration to the new gateway, you must configure the new gateway. Use 'CreateVpnGateway' to create a virtual private gateway, or 'CreateTransitGateway' to create a transit gateway.
-- This step is required when you migrate from a virtual private gateway with static routes to a transit gateway. 
-- You must delete the static routes before you migrate to the new gateway.
-- Keep a copy of the static route before you delete it. You will need to add back these routes to the transit gateway after the VPN connection migration is complete.
-- After you migrate to the new gateway, you might need to modify your VPC route table. Use 'CreateRoute' and 'DeleteRoute' to make the changes described in <https://docs.aws.amazon.com/vpn/latest/s2svpn/modify-vpn-target.html#step-update-routing VPN Gateway Target Modification Required VPC Route Table Updates> in the /AWS Site-to-Site VPN User Guide/ .
-- When the new gateway is a transit gateway, modify the transit gateway route table to allow traffic between the VPC and the AWS Site-to-Site VPN connection. Use 'CreateTransitGatewayRoute' to add the routes.
-- If you deleted VPN static routes, you must add the static routes to the transit gateway route table.
-- After you perform this operation, the AWS VPN endpoint's IP addresses on the AWS side and the tunnel options remain intact. Your AWS Site-to-Site VPN connection will be temporarily unavailable for a brief period while we provision the new endpoints.
module Network.AWS.EC2.ModifyVpnConnection
    (
    -- * Creating a request
      ModifyVpnConnection (..)
    , mkModifyVpnConnection
    -- ** Request lenses
    , mvcVpnConnectionId
    , mvcCustomerGatewayId
    , mvcDryRun
    , mvcTransitGatewayId
    , mvcVpnGatewayId

    -- * Destructuring the response
    , ModifyVpnConnectionResponse (..)
    , mkModifyVpnConnectionResponse
    -- ** Response lenses
    , mvcrrsVpnConnection
    , mvcrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyVpnConnection' smart constructor.
data ModifyVpnConnection = ModifyVpnConnection'
  { vpnConnectionId :: Types.VpnConnectionId
    -- ^ The ID of the VPN connection.
  , customerGatewayId :: Core.Maybe Types.CustomerGatewayId
    -- ^ The ID of the customer gateway at your end of the VPN connection.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , transitGatewayId :: Core.Maybe Types.TransitGatewayId
    -- ^ The ID of the transit gateway.
  , vpnGatewayId :: Core.Maybe Types.VpnGatewayId
    -- ^ The ID of the virtual private gateway at the AWS side of the VPN connection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVpnConnection' value with any optional fields omitted.
mkModifyVpnConnection
    :: Types.VpnConnectionId -- ^ 'vpnConnectionId'
    -> ModifyVpnConnection
mkModifyVpnConnection vpnConnectionId
  = ModifyVpnConnection'{vpnConnectionId,
                         customerGatewayId = Core.Nothing, dryRun = Core.Nothing,
                         transitGatewayId = Core.Nothing, vpnGatewayId = Core.Nothing}

-- | The ID of the VPN connection.
--
-- /Note:/ Consider using 'vpnConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcVpnConnectionId :: Lens.Lens' ModifyVpnConnection Types.VpnConnectionId
mvcVpnConnectionId = Lens.field @"vpnConnectionId"
{-# INLINEABLE mvcVpnConnectionId #-}
{-# DEPRECATED vpnConnectionId "Use generic-lens or generic-optics with 'vpnConnectionId' instead"  #-}

-- | The ID of the customer gateway at your end of the VPN connection.
--
-- /Note:/ Consider using 'customerGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcCustomerGatewayId :: Lens.Lens' ModifyVpnConnection (Core.Maybe Types.CustomerGatewayId)
mvcCustomerGatewayId = Lens.field @"customerGatewayId"
{-# INLINEABLE mvcCustomerGatewayId #-}
{-# DEPRECATED customerGatewayId "Use generic-lens or generic-optics with 'customerGatewayId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcDryRun :: Lens.Lens' ModifyVpnConnection (Core.Maybe Core.Bool)
mvcDryRun = Lens.field @"dryRun"
{-# INLINEABLE mvcDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcTransitGatewayId :: Lens.Lens' ModifyVpnConnection (Core.Maybe Types.TransitGatewayId)
mvcTransitGatewayId = Lens.field @"transitGatewayId"
{-# INLINEABLE mvcTransitGatewayId #-}
{-# DEPRECATED transitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead"  #-}

-- | The ID of the virtual private gateway at the AWS side of the VPN connection.
--
-- /Note:/ Consider using 'vpnGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcVpnGatewayId :: Lens.Lens' ModifyVpnConnection (Core.Maybe Types.VpnGatewayId)
mvcVpnGatewayId = Lens.field @"vpnGatewayId"
{-# INLINEABLE mvcVpnGatewayId #-}
{-# DEPRECATED vpnGatewayId "Use generic-lens or generic-optics with 'vpnGatewayId' instead"  #-}

instance Core.ToQuery ModifyVpnConnection where
        toQuery ModifyVpnConnection{..}
          = Core.toQueryPair "Action" ("ModifyVpnConnection" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VpnConnectionId" vpnConnectionId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CustomerGatewayId")
                customerGatewayId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TransitGatewayId")
                transitGatewayId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VpnGatewayId")
                vpnGatewayId

instance Core.ToHeaders ModifyVpnConnection where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyVpnConnection where
        type Rs ModifyVpnConnection = ModifyVpnConnectionResponse
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
          = Response.receiveXML
              (\ s h x ->
                 ModifyVpnConnectionResponse' Core.<$>
                   (x Core..@? "vpnConnection") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyVpnConnectionResponse' smart constructor.
data ModifyVpnConnectionResponse = ModifyVpnConnectionResponse'
  { vpnConnection :: Core.Maybe Types.VpnConnection
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyVpnConnectionResponse' value with any optional fields omitted.
mkModifyVpnConnectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyVpnConnectionResponse
mkModifyVpnConnectionResponse responseStatus
  = ModifyVpnConnectionResponse'{vpnConnection = Core.Nothing,
                                 responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'vpnConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcrrsVpnConnection :: Lens.Lens' ModifyVpnConnectionResponse (Core.Maybe Types.VpnConnection)
mvcrrsVpnConnection = Lens.field @"vpnConnection"
{-# INLINEABLE mvcrrsVpnConnection #-}
{-# DEPRECATED vpnConnection "Use generic-lens or generic-optics with 'vpnConnection' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvcrrsResponseStatus :: Lens.Lens' ModifyVpnConnectionResponse Core.Int
mvcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mvcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
