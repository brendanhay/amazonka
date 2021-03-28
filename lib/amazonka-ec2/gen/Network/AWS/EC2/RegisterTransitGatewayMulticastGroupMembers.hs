{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RegisterTransitGatewayMulticastGroupMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers members (network interfaces) with the transit gateway multicast group. A member is a network interface associated with a supported EC2 instance that receives multicast traffic. For information about supported instances, see <https://docs.aws.amazon.com/vpc/latest/tgw/transit-gateway-limits.html#multicast-limits Multicast Consideration> in /Amazon VPC Transit Gateways/ .
--
-- After you add the members, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SearchTransitGatewayMulticastGroups.html SearchTransitGatewayMulticastGroups> to verify that the members were added to the transit gateway multicast group.
module Network.AWS.EC2.RegisterTransitGatewayMulticastGroupMembers
    (
    -- * Creating a request
      RegisterTransitGatewayMulticastGroupMembers (..)
    , mkRegisterTransitGatewayMulticastGroupMembers
    -- ** Request lenses
    , rtgmgmDryRun
    , rtgmgmGroupIpAddress
    , rtgmgmNetworkInterfaceIds
    , rtgmgmTransitGatewayMulticastDomainId

    -- * Destructuring the response
    , RegisterTransitGatewayMulticastGroupMembersResponse (..)
    , mkRegisterTransitGatewayMulticastGroupMembersResponse
    -- ** Response lenses
    , rtgmgmrrsRegisteredMulticastGroupMembers
    , rtgmgmrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterTransitGatewayMulticastGroupMembers' smart constructor.
data RegisterTransitGatewayMulticastGroupMembers = RegisterTransitGatewayMulticastGroupMembers'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , groupIpAddress :: Core.Maybe Core.Text
    -- ^ The IP address assigned to the transit gateway multicast group.
  , networkInterfaceIds :: Core.Maybe [Types.NetworkInterfaceId]
    -- ^ The group members' network interface IDs to register with the transit gateway multicast group.
  , transitGatewayMulticastDomainId :: Core.Maybe Types.TransitGatewayMulticastDomainId
    -- ^ The ID of the transit gateway multicast domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTransitGatewayMulticastGroupMembers' value with any optional fields omitted.
mkRegisterTransitGatewayMulticastGroupMembers
    :: RegisterTransitGatewayMulticastGroupMembers
mkRegisterTransitGatewayMulticastGroupMembers
  = RegisterTransitGatewayMulticastGroupMembers'{dryRun =
                                                   Core.Nothing,
                                                 groupIpAddress = Core.Nothing,
                                                 networkInterfaceIds = Core.Nothing,
                                                 transitGatewayMulticastDomainId = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgmDryRun :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembers (Core.Maybe Core.Bool)
rtgmgmDryRun = Lens.field @"dryRun"
{-# INLINEABLE rtgmgmDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgmGroupIpAddress :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembers (Core.Maybe Core.Text)
rtgmgmGroupIpAddress = Lens.field @"groupIpAddress"
{-# INLINEABLE rtgmgmGroupIpAddress #-}
{-# DEPRECATED groupIpAddress "Use generic-lens or generic-optics with 'groupIpAddress' instead"  #-}

-- | The group members' network interface IDs to register with the transit gateway multicast group.
--
-- /Note:/ Consider using 'networkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgmNetworkInterfaceIds :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembers (Core.Maybe [Types.NetworkInterfaceId])
rtgmgmNetworkInterfaceIds = Lens.field @"networkInterfaceIds"
{-# INLINEABLE rtgmgmNetworkInterfaceIds #-}
{-# DEPRECATED networkInterfaceIds "Use generic-lens or generic-optics with 'networkInterfaceIds' instead"  #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgmTransitGatewayMulticastDomainId :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembers (Core.Maybe Types.TransitGatewayMulticastDomainId)
rtgmgmTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# INLINEABLE rtgmgmTransitGatewayMulticastDomainId #-}
{-# DEPRECATED transitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead"  #-}

instance Core.ToQuery RegisterTransitGatewayMulticastGroupMembers
         where
        toQuery RegisterTransitGatewayMulticastGroupMembers{..}
          = Core.toQueryPair "Action"
              ("RegisterTransitGatewayMulticastGroupMembers" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "GroupIpAddress")
                groupIpAddress
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "NetworkInterfaceIds")
                networkInterfaceIds
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "TransitGatewayMulticastDomainId")
                transitGatewayMulticastDomainId

instance Core.ToHeaders RegisterTransitGatewayMulticastGroupMembers
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest
           RegisterTransitGatewayMulticastGroupMembers
         where
        type Rs RegisterTransitGatewayMulticastGroupMembers =
             RegisterTransitGatewayMulticastGroupMembersResponse
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
                 RegisterTransitGatewayMulticastGroupMembersResponse' Core.<$>
                   (x Core..@? "registeredMulticastGroupMembers") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRegisterTransitGatewayMulticastGroupMembersResponse' smart constructor.
data RegisterTransitGatewayMulticastGroupMembersResponse = RegisterTransitGatewayMulticastGroupMembersResponse'
  { registeredMulticastGroupMembers :: Core.Maybe Types.TransitGatewayMulticastRegisteredGroupMembers
    -- ^ Information about the registered transit gateway multicast group members.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTransitGatewayMulticastGroupMembersResponse' value with any optional fields omitted.
mkRegisterTransitGatewayMulticastGroupMembersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RegisterTransitGatewayMulticastGroupMembersResponse
mkRegisterTransitGatewayMulticastGroupMembersResponse
  responseStatus
  = RegisterTransitGatewayMulticastGroupMembersResponse'{registeredMulticastGroupMembers
                                                           = Core.Nothing,
                                                         responseStatus}

-- | Information about the registered transit gateway multicast group members.
--
-- /Note:/ Consider using 'registeredMulticastGroupMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgmrrsRegisteredMulticastGroupMembers :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembersResponse (Core.Maybe Types.TransitGatewayMulticastRegisteredGroupMembers)
rtgmgmrrsRegisteredMulticastGroupMembers = Lens.field @"registeredMulticastGroupMembers"
{-# INLINEABLE rtgmgmrrsRegisteredMulticastGroupMembers #-}
{-# DEPRECATED registeredMulticastGroupMembers "Use generic-lens or generic-optics with 'registeredMulticastGroupMembers' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgmrrsResponseStatus :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembersResponse Core.Int
rtgmgmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtgmgmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
