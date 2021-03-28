{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RegisterTransitGatewayMulticastGroupSources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers sources (network interfaces) with the specified transit gateway multicast group.
--
-- A multicast source is a network interface attached to a supported instance that sends multicast traffic. For information about supported instances, see <https://docs.aws.amazon.com/vpc/latest/tgw/transit-gateway-limits.html#multicast-limits Multicast Considerations> in /Amazon VPC Transit Gateways/ .
-- After you add the source, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SearchTransitGatewayMulticastGroups.html SearchTransitGatewayMulticastGroups> to verify that the source was added to the multicast group.
module Network.AWS.EC2.RegisterTransitGatewayMulticastGroupSources
    (
    -- * Creating a request
      RegisterTransitGatewayMulticastGroupSources (..)
    , mkRegisterTransitGatewayMulticastGroupSources
    -- ** Request lenses
    , rtgmgsDryRun
    , rtgmgsGroupIpAddress
    , rtgmgsNetworkInterfaceIds
    , rtgmgsTransitGatewayMulticastDomainId

    -- * Destructuring the response
    , RegisterTransitGatewayMulticastGroupSourcesResponse (..)
    , mkRegisterTransitGatewayMulticastGroupSourcesResponse
    -- ** Response lenses
    , rtgmgsrrsRegisteredMulticastGroupSources
    , rtgmgsrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterTransitGatewayMulticastGroupSources' smart constructor.
data RegisterTransitGatewayMulticastGroupSources = RegisterTransitGatewayMulticastGroupSources'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , groupIpAddress :: Core.Maybe Core.Text
    -- ^ The IP address assigned to the transit gateway multicast group.
  , networkInterfaceIds :: Core.Maybe [Types.NetworkInterfaceId]
    -- ^ The group sources' network interface IDs to register with the transit gateway multicast group.
  , transitGatewayMulticastDomainId :: Core.Maybe Types.TransitGatewayMulticastDomainId
    -- ^ The ID of the transit gateway multicast domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTransitGatewayMulticastGroupSources' value with any optional fields omitted.
mkRegisterTransitGatewayMulticastGroupSources
    :: RegisterTransitGatewayMulticastGroupSources
mkRegisterTransitGatewayMulticastGroupSources
  = RegisterTransitGatewayMulticastGroupSources'{dryRun =
                                                   Core.Nothing,
                                                 groupIpAddress = Core.Nothing,
                                                 networkInterfaceIds = Core.Nothing,
                                                 transitGatewayMulticastDomainId = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgsDryRun :: Lens.Lens' RegisterTransitGatewayMulticastGroupSources (Core.Maybe Core.Bool)
rtgmgsDryRun = Lens.field @"dryRun"
{-# INLINEABLE rtgmgsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgsGroupIpAddress :: Lens.Lens' RegisterTransitGatewayMulticastGroupSources (Core.Maybe Core.Text)
rtgmgsGroupIpAddress = Lens.field @"groupIpAddress"
{-# INLINEABLE rtgmgsGroupIpAddress #-}
{-# DEPRECATED groupIpAddress "Use generic-lens or generic-optics with 'groupIpAddress' instead"  #-}

-- | The group sources' network interface IDs to register with the transit gateway multicast group.
--
-- /Note:/ Consider using 'networkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgsNetworkInterfaceIds :: Lens.Lens' RegisterTransitGatewayMulticastGroupSources (Core.Maybe [Types.NetworkInterfaceId])
rtgmgsNetworkInterfaceIds = Lens.field @"networkInterfaceIds"
{-# INLINEABLE rtgmgsNetworkInterfaceIds #-}
{-# DEPRECATED networkInterfaceIds "Use generic-lens or generic-optics with 'networkInterfaceIds' instead"  #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgsTransitGatewayMulticastDomainId :: Lens.Lens' RegisterTransitGatewayMulticastGroupSources (Core.Maybe Types.TransitGatewayMulticastDomainId)
rtgmgsTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# INLINEABLE rtgmgsTransitGatewayMulticastDomainId #-}
{-# DEPRECATED transitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead"  #-}

instance Core.ToQuery RegisterTransitGatewayMulticastGroupSources
         where
        toQuery RegisterTransitGatewayMulticastGroupSources{..}
          = Core.toQueryPair "Action"
              ("RegisterTransitGatewayMulticastGroupSources" :: Core.Text)
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

instance Core.ToHeaders RegisterTransitGatewayMulticastGroupSources
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest
           RegisterTransitGatewayMulticastGroupSources
         where
        type Rs RegisterTransitGatewayMulticastGroupSources =
             RegisterTransitGatewayMulticastGroupSourcesResponse
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
                 RegisterTransitGatewayMulticastGroupSourcesResponse' Core.<$>
                   (x Core..@? "registeredMulticastGroupSources") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRegisterTransitGatewayMulticastGroupSourcesResponse' smart constructor.
data RegisterTransitGatewayMulticastGroupSourcesResponse = RegisterTransitGatewayMulticastGroupSourcesResponse'
  { registeredMulticastGroupSources :: Core.Maybe Types.TransitGatewayMulticastRegisteredGroupSources
    -- ^ Information about the transit gateway multicast group sources.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTransitGatewayMulticastGroupSourcesResponse' value with any optional fields omitted.
mkRegisterTransitGatewayMulticastGroupSourcesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RegisterTransitGatewayMulticastGroupSourcesResponse
mkRegisterTransitGatewayMulticastGroupSourcesResponse
  responseStatus
  = RegisterTransitGatewayMulticastGroupSourcesResponse'{registeredMulticastGroupSources
                                                           = Core.Nothing,
                                                         responseStatus}

-- | Information about the transit gateway multicast group sources.
--
-- /Note:/ Consider using 'registeredMulticastGroupSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgsrrsRegisteredMulticastGroupSources :: Lens.Lens' RegisterTransitGatewayMulticastGroupSourcesResponse (Core.Maybe Types.TransitGatewayMulticastRegisteredGroupSources)
rtgmgsrrsRegisteredMulticastGroupSources = Lens.field @"registeredMulticastGroupSources"
{-# INLINEABLE rtgmgsrrsRegisteredMulticastGroupSources #-}
{-# DEPRECATED registeredMulticastGroupSources "Use generic-lens or generic-optics with 'registeredMulticastGroupSources' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgsrrsResponseStatus :: Lens.Lens' RegisterTransitGatewayMulticastGroupSourcesResponse Core.Int
rtgmgsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtgmgsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
