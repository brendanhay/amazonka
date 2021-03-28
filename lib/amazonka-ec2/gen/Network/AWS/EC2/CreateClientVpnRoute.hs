{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateClientVpnRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a route to a network to a Client VPN endpoint. Each Client VPN endpoint has a route table that describes the available destination network routes. Each route in the route table specifies the path for traﬃc to speciﬁc resources or networks.
module Network.AWS.EC2.CreateClientVpnRoute
    (
    -- * Creating a request
      CreateClientVpnRoute (..)
    , mkCreateClientVpnRoute
    -- ** Request lenses
    , ccvrClientVpnEndpointId
    , ccvrDestinationCidrBlock
    , ccvrTargetVpcSubnetId
    , ccvrClientToken
    , ccvrDescription
    , ccvrDryRun

    -- * Destructuring the response
    , CreateClientVpnRouteResponse (..)
    , mkCreateClientVpnRouteResponse
    -- ** Response lenses
    , ccvrrrsStatus
    , ccvrrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateClientVpnRoute' smart constructor.
data CreateClientVpnRoute = CreateClientVpnRoute'
  { clientVpnEndpointId :: Types.ClientVpnEndpointId
    -- ^ The ID of the Client VPN endpoint to which to add the route.
  , destinationCidrBlock :: Core.Text
    -- ^ The IPv4 address range, in CIDR notation, of the route destination. For example:
--
--
--     * To add a route for Internet access, enter @0.0.0.0/0@ 
--
--
--     * To add a route for a peered VPC, enter the peered VPC's IPv4 CIDR range
--
--
--     * To add a route for an on-premises network, enter the AWS Site-to-Site VPN connection's IPv4 CIDR range
--
--
--     * To add a route for the local network, enter the client CIDR range
--
--
  , targetVpcSubnetId :: Types.TargetVpcSubnetId
    -- ^ The ID of the subnet through which you want to route traffic. The specified subnet must be an existing target network of the Client VPN endpoint.
--
-- Alternatively, if you're adding a route for the local network, specify @local@ .
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
  , description :: Core.Maybe Core.Text
    -- ^ A brief description of the route.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateClientVpnRoute' value with any optional fields omitted.
mkCreateClientVpnRoute
    :: Types.ClientVpnEndpointId -- ^ 'clientVpnEndpointId'
    -> Core.Text -- ^ 'destinationCidrBlock'
    -> Types.TargetVpcSubnetId -- ^ 'targetVpcSubnetId'
    -> CreateClientVpnRoute
mkCreateClientVpnRoute clientVpnEndpointId destinationCidrBlock
  targetVpcSubnetId
  = CreateClientVpnRoute'{clientVpnEndpointId, destinationCidrBlock,
                          targetVpcSubnetId, clientToken = Core.Nothing,
                          description = Core.Nothing, dryRun = Core.Nothing}

-- | The ID of the Client VPN endpoint to which to add the route.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvrClientVpnEndpointId :: Lens.Lens' CreateClientVpnRoute Types.ClientVpnEndpointId
ccvrClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE ccvrClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | The IPv4 address range, in CIDR notation, of the route destination. For example:
--
--
--     * To add a route for Internet access, enter @0.0.0.0/0@ 
--
--
--     * To add a route for a peered VPC, enter the peered VPC's IPv4 CIDR range
--
--
--     * To add a route for an on-premises network, enter the AWS Site-to-Site VPN connection's IPv4 CIDR range
--
--
--     * To add a route for the local network, enter the client CIDR range
--
--
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvrDestinationCidrBlock :: Lens.Lens' CreateClientVpnRoute Core.Text
ccvrDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# INLINEABLE ccvrDestinationCidrBlock #-}
{-# DEPRECATED destinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead"  #-}

-- | The ID of the subnet through which you want to route traffic. The specified subnet must be an existing target network of the Client VPN endpoint.
--
-- Alternatively, if you're adding a route for the local network, specify @local@ .
--
-- /Note:/ Consider using 'targetVpcSubnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvrTargetVpcSubnetId :: Lens.Lens' CreateClientVpnRoute Types.TargetVpcSubnetId
ccvrTargetVpcSubnetId = Lens.field @"targetVpcSubnetId"
{-# INLINEABLE ccvrTargetVpcSubnetId #-}
{-# DEPRECATED targetVpcSubnetId "Use generic-lens or generic-optics with 'targetVpcSubnetId' instead"  #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvrClientToken :: Lens.Lens' CreateClientVpnRoute (Core.Maybe Core.Text)
ccvrClientToken = Lens.field @"clientToken"
{-# INLINEABLE ccvrClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | A brief description of the route.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvrDescription :: Lens.Lens' CreateClientVpnRoute (Core.Maybe Core.Text)
ccvrDescription = Lens.field @"description"
{-# INLINEABLE ccvrDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvrDryRun :: Lens.Lens' CreateClientVpnRoute (Core.Maybe Core.Bool)
ccvrDryRun = Lens.field @"dryRun"
{-# INLINEABLE ccvrDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery CreateClientVpnRoute where
        toQuery CreateClientVpnRoute{..}
          = Core.toQueryPair "Action" ("CreateClientVpnRoute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ClientVpnEndpointId" clientVpnEndpointId
              Core.<>
              Core.toQueryPair "DestinationCidrBlock" destinationCidrBlock
              Core.<> Core.toQueryPair "TargetVpcSubnetId" targetVpcSubnetId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders CreateClientVpnRoute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateClientVpnRoute where
        type Rs CreateClientVpnRoute = CreateClientVpnRouteResponse
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
                 CreateClientVpnRouteResponse' Core.<$>
                   (x Core..@? "status") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateClientVpnRouteResponse' smart constructor.
data CreateClientVpnRouteResponse = CreateClientVpnRouteResponse'
  { status :: Core.Maybe Types.ClientVpnRouteStatus
    -- ^ The current state of the route.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateClientVpnRouteResponse' value with any optional fields omitted.
mkCreateClientVpnRouteResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateClientVpnRouteResponse
mkCreateClientVpnRouteResponse responseStatus
  = CreateClientVpnRouteResponse'{status = Core.Nothing,
                                  responseStatus}

-- | The current state of the route.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvrrrsStatus :: Lens.Lens' CreateClientVpnRouteResponse (Core.Maybe Types.ClientVpnRouteStatus)
ccvrrrsStatus = Lens.field @"status"
{-# INLINEABLE ccvrrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvrrrsResponseStatus :: Lens.Lens' CreateClientVpnRouteResponse Core.Int
ccvrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccvrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
