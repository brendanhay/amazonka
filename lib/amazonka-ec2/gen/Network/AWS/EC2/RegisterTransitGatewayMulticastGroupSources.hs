{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    RegisterTransitGatewayMulticastGroupSources (..),
    mkRegisterTransitGatewayMulticastGroupSources,

    -- ** Request lenses
    rtgmgsDryRun,
    rtgmgsGroupIpAddress,
    rtgmgsNetworkInterfaceIds,
    rtgmgsTransitGatewayMulticastDomainId,

    -- * Destructuring the response
    RegisterTransitGatewayMulticastGroupSourcesResponse (..),
    mkRegisterTransitGatewayMulticastGroupSourcesResponse,

    -- ** Response lenses
    rtgmgsrrsRegisteredMulticastGroupSources,
    rtgmgsrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterTransitGatewayMulticastGroupSources' smart constructor.
data RegisterTransitGatewayMulticastGroupSources = RegisterTransitGatewayMulticastGroupSources'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Core.Maybe Types.String,
    -- | The group sources' network interface IDs to register with the transit gateway multicast group.
    networkInterfaceIds :: Core.Maybe [Types.NetworkInterfaceId],
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Types.TransitGatewayMulticastDomainId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTransitGatewayMulticastGroupSources' value with any optional fields omitted.
mkRegisterTransitGatewayMulticastGroupSources ::
  RegisterTransitGatewayMulticastGroupSources
mkRegisterTransitGatewayMulticastGroupSources =
  RegisterTransitGatewayMulticastGroupSources'
    { dryRun =
        Core.Nothing,
      groupIpAddress = Core.Nothing,
      networkInterfaceIds = Core.Nothing,
      transitGatewayMulticastDomainId = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgsDryRun :: Lens.Lens' RegisterTransitGatewayMulticastGroupSources (Core.Maybe Core.Bool)
rtgmgsDryRun = Lens.field @"dryRun"
{-# DEPRECATED rtgmgsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgsGroupIpAddress :: Lens.Lens' RegisterTransitGatewayMulticastGroupSources (Core.Maybe Types.String)
rtgmgsGroupIpAddress = Lens.field @"groupIpAddress"
{-# DEPRECATED rtgmgsGroupIpAddress "Use generic-lens or generic-optics with 'groupIpAddress' instead." #-}

-- | The group sources' network interface IDs to register with the transit gateway multicast group.
--
-- /Note:/ Consider using 'networkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgsNetworkInterfaceIds :: Lens.Lens' RegisterTransitGatewayMulticastGroupSources (Core.Maybe [Types.NetworkInterfaceId])
rtgmgsNetworkInterfaceIds = Lens.field @"networkInterfaceIds"
{-# DEPRECATED rtgmgsNetworkInterfaceIds "Use generic-lens or generic-optics with 'networkInterfaceIds' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgsTransitGatewayMulticastDomainId :: Lens.Lens' RegisterTransitGatewayMulticastGroupSources (Core.Maybe Types.TransitGatewayMulticastDomainId)
rtgmgsTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# DEPRECATED rtgmgsTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

instance
  Core.AWSRequest
    RegisterTransitGatewayMulticastGroupSources
  where
  type
    Rs RegisterTransitGatewayMulticastGroupSources =
      RegisterTransitGatewayMulticastGroupSourcesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure
                ("Action", "RegisterTransitGatewayMulticastGroupSources")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "GroupIpAddress" Core.<$> groupIpAddress)
                Core.<> ( Core.toQueryList "NetworkInterfaceIds"
                            Core.<$> networkInterfaceIds
                        )
                Core.<> ( Core.toQueryValue "TransitGatewayMulticastDomainId"
                            Core.<$> transitGatewayMulticastDomainId
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          RegisterTransitGatewayMulticastGroupSourcesResponse'
            Core.<$> (x Core..@? "registeredMulticastGroupSources")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRegisterTransitGatewayMulticastGroupSourcesResponse' smart constructor.
data RegisterTransitGatewayMulticastGroupSourcesResponse = RegisterTransitGatewayMulticastGroupSourcesResponse'
  { -- | Information about the transit gateway multicast group sources.
    registeredMulticastGroupSources :: Core.Maybe Types.TransitGatewayMulticastRegisteredGroupSources,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTransitGatewayMulticastGroupSourcesResponse' value with any optional fields omitted.
mkRegisterTransitGatewayMulticastGroupSourcesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RegisterTransitGatewayMulticastGroupSourcesResponse
mkRegisterTransitGatewayMulticastGroupSourcesResponse
  responseStatus =
    RegisterTransitGatewayMulticastGroupSourcesResponse'
      { registeredMulticastGroupSources =
          Core.Nothing,
        responseStatus
      }

-- | Information about the transit gateway multicast group sources.
--
-- /Note:/ Consider using 'registeredMulticastGroupSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgsrrsRegisteredMulticastGroupSources :: Lens.Lens' RegisterTransitGatewayMulticastGroupSourcesResponse (Core.Maybe Types.TransitGatewayMulticastRegisteredGroupSources)
rtgmgsrrsRegisteredMulticastGroupSources = Lens.field @"registeredMulticastGroupSources"
{-# DEPRECATED rtgmgsrrsRegisteredMulticastGroupSources "Use generic-lens or generic-optics with 'registeredMulticastGroupSources' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgsrrsResponseStatus :: Lens.Lens' RegisterTransitGatewayMulticastGroupSourcesResponse Core.Int
rtgmgsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rtgmgsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
