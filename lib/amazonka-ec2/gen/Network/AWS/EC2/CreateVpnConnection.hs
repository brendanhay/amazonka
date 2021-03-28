{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVpnConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPN connection between an existing virtual private gateway or transit gateway and a customer gateway. The supported connection type is @ipsec.1@ .
--
-- The response includes information that you need to give to your network administrator to configure your customer gateway.
-- /Important:/ We strongly recommend that you use HTTPS when calling this operation because the response contains sensitive cryptographic information for configuring your customer gateway device.
-- If you decide to shut down your VPN connection for any reason and later create a new VPN connection, you must reconfigure your customer gateway with the new information returned from this call.
-- This is an idempotent operation. If you perform the operation more than once, Amazon EC2 doesn't return an error.
-- For more information, see <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html AWS Site-to-Site VPN> in the /AWS Site-to-Site VPN User Guide/ .
module Network.AWS.EC2.CreateVpnConnection
    (
    -- * Creating a request
      CreateVpnConnection (..)
    , mkCreateVpnConnection
    -- ** Request lenses
    , cvcCustomerGatewayId
    , cvcType
    , cvcDryRun
    , cvcOptions
    , cvcTagSpecifications
    , cvcTransitGatewayId
    , cvcVpnGatewayId

    -- * Destructuring the response
    , CreateVpnConnectionResponse (..)
    , mkCreateVpnConnectionResponse
    -- ** Response lenses
    , cvcrrsVpnConnection
    , cvcrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateVpnConnection.
--
-- /See:/ 'mkCreateVpnConnection' smart constructor.
data CreateVpnConnection = CreateVpnConnection'
  { customerGatewayId :: Types.CustomerGatewayId
    -- ^ The ID of the customer gateway.
  , type' :: Core.Text
    -- ^ The type of VPN connection (@ipsec.1@ ).
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , options :: Core.Maybe Types.VpnConnectionOptionsSpecification
    -- ^ The options for the VPN connection.
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the VPN connection.
  , transitGatewayId :: Core.Maybe Types.TransitGatewayId
    -- ^ The ID of the transit gateway. If you specify a transit gateway, you cannot specify a virtual private gateway.
  , vpnGatewayId :: Core.Maybe Types.VpnGatewayId
    -- ^ The ID of the virtual private gateway. If you specify a virtual private gateway, you cannot specify a transit gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVpnConnection' value with any optional fields omitted.
mkCreateVpnConnection
    :: Types.CustomerGatewayId -- ^ 'customerGatewayId'
    -> Core.Text -- ^ 'type\''
    -> CreateVpnConnection
mkCreateVpnConnection customerGatewayId type'
  = CreateVpnConnection'{customerGatewayId, type',
                         dryRun = Core.Nothing, options = Core.Nothing,
                         tagSpecifications = Core.Nothing, transitGatewayId = Core.Nothing,
                         vpnGatewayId = Core.Nothing}

-- | The ID of the customer gateway.
--
-- /Note:/ Consider using 'customerGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcCustomerGatewayId :: Lens.Lens' CreateVpnConnection Types.CustomerGatewayId
cvcCustomerGatewayId = Lens.field @"customerGatewayId"
{-# INLINEABLE cvcCustomerGatewayId #-}
{-# DEPRECATED customerGatewayId "Use generic-lens or generic-optics with 'customerGatewayId' instead"  #-}

-- | The type of VPN connection (@ipsec.1@ ).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcType :: Lens.Lens' CreateVpnConnection Core.Text
cvcType = Lens.field @"type'"
{-# INLINEABLE cvcType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcDryRun :: Lens.Lens' CreateVpnConnection (Core.Maybe Core.Bool)
cvcDryRun = Lens.field @"dryRun"
{-# INLINEABLE cvcDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The options for the VPN connection.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcOptions :: Lens.Lens' CreateVpnConnection (Core.Maybe Types.VpnConnectionOptionsSpecification)
cvcOptions = Lens.field @"options"
{-# INLINEABLE cvcOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | The tags to apply to the VPN connection.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcTagSpecifications :: Lens.Lens' CreateVpnConnection (Core.Maybe [Types.TagSpecification])
cvcTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE cvcTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

-- | The ID of the transit gateway. If you specify a transit gateway, you cannot specify a virtual private gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcTransitGatewayId :: Lens.Lens' CreateVpnConnection (Core.Maybe Types.TransitGatewayId)
cvcTransitGatewayId = Lens.field @"transitGatewayId"
{-# INLINEABLE cvcTransitGatewayId #-}
{-# DEPRECATED transitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead"  #-}

-- | The ID of the virtual private gateway. If you specify a virtual private gateway, you cannot specify a transit gateway.
--
-- /Note:/ Consider using 'vpnGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcVpnGatewayId :: Lens.Lens' CreateVpnConnection (Core.Maybe Types.VpnGatewayId)
cvcVpnGatewayId = Lens.field @"vpnGatewayId"
{-# INLINEABLE cvcVpnGatewayId #-}
{-# DEPRECATED vpnGatewayId "Use generic-lens or generic-optics with 'vpnGatewayId' instead"  #-}

instance Core.ToQuery CreateVpnConnection where
        toQuery CreateVpnConnection{..}
          = Core.toQueryPair "Action" ("CreateVpnConnection" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "CustomerGatewayId" customerGatewayId
              Core.<> Core.toQueryPair "Type" type'
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Options") options
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TransitGatewayId")
                transitGatewayId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VpnGatewayId")
                vpnGatewayId

instance Core.ToHeaders CreateVpnConnection where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateVpnConnection where
        type Rs CreateVpnConnection = CreateVpnConnectionResponse
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
                 CreateVpnConnectionResponse' Core.<$>
                   (x Core..@? "vpnConnection") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of CreateVpnConnection.
--
-- /See:/ 'mkCreateVpnConnectionResponse' smart constructor.
data CreateVpnConnectionResponse = CreateVpnConnectionResponse'
  { vpnConnection :: Core.Maybe Types.VpnConnection
    -- ^ Information about the VPN connection.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateVpnConnectionResponse' value with any optional fields omitted.
mkCreateVpnConnectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateVpnConnectionResponse
mkCreateVpnConnectionResponse responseStatus
  = CreateVpnConnectionResponse'{vpnConnection = Core.Nothing,
                                 responseStatus}

-- | Information about the VPN connection.
--
-- /Note:/ Consider using 'vpnConnection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcrrsVpnConnection :: Lens.Lens' CreateVpnConnectionResponse (Core.Maybe Types.VpnConnection)
cvcrrsVpnConnection = Lens.field @"vpnConnection"
{-# INLINEABLE cvcrrsVpnConnection #-}
{-# DEPRECATED vpnConnection "Use generic-lens or generic-optics with 'vpnConnection' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcrrsResponseStatus :: Lens.Lens' CreateVpnConnectionResponse Core.Int
cvcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cvcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
