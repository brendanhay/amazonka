{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    RegisterTransitGatewayMulticastGroupMembers (..),
    mkRegisterTransitGatewayMulticastGroupMembers,

    -- ** Request lenses
    rtgmgmDryRun,
    rtgmgmGroupIpAddress,
    rtgmgmNetworkInterfaceIds,
    rtgmgmTransitGatewayMulticastDomainId,

    -- * Destructuring the response
    RegisterTransitGatewayMulticastGroupMembersResponse (..),
    mkRegisterTransitGatewayMulticastGroupMembersResponse,

    -- ** Response lenses
    rtgmgmrrsRegisteredMulticastGroupMembers,
    rtgmgmrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterTransitGatewayMulticastGroupMembers' smart constructor.
data RegisterTransitGatewayMulticastGroupMembers = RegisterTransitGatewayMulticastGroupMembers'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Core.Maybe Types.String,
    -- | The group members' network interface IDs to register with the transit gateway multicast group.
    networkInterfaceIds :: Core.Maybe [Types.NetworkInterfaceId],
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Types.TransitGatewayMulticastDomainId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTransitGatewayMulticastGroupMembers' value with any optional fields omitted.
mkRegisterTransitGatewayMulticastGroupMembers ::
  RegisterTransitGatewayMulticastGroupMembers
mkRegisterTransitGatewayMulticastGroupMembers =
  RegisterTransitGatewayMulticastGroupMembers'
    { dryRun =
        Core.Nothing,
      groupIpAddress = Core.Nothing,
      networkInterfaceIds = Core.Nothing,
      transitGatewayMulticastDomainId = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgmDryRun :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembers (Core.Maybe Core.Bool)
rtgmgmDryRun = Lens.field @"dryRun"
{-# DEPRECATED rtgmgmDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgmGroupIpAddress :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembers (Core.Maybe Types.String)
rtgmgmGroupIpAddress = Lens.field @"groupIpAddress"
{-# DEPRECATED rtgmgmGroupIpAddress "Use generic-lens or generic-optics with 'groupIpAddress' instead." #-}

-- | The group members' network interface IDs to register with the transit gateway multicast group.
--
-- /Note:/ Consider using 'networkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgmNetworkInterfaceIds :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembers (Core.Maybe [Types.NetworkInterfaceId])
rtgmgmNetworkInterfaceIds = Lens.field @"networkInterfaceIds"
{-# DEPRECATED rtgmgmNetworkInterfaceIds "Use generic-lens or generic-optics with 'networkInterfaceIds' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgmTransitGatewayMulticastDomainId :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembers (Core.Maybe Types.TransitGatewayMulticastDomainId)
rtgmgmTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# DEPRECATED rtgmgmTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

instance
  Core.AWSRequest
    RegisterTransitGatewayMulticastGroupMembers
  where
  type
    Rs RegisterTransitGatewayMulticastGroupMembers =
      RegisterTransitGatewayMulticastGroupMembersResponse
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
                ("Action", "RegisterTransitGatewayMulticastGroupMembers")
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
          RegisterTransitGatewayMulticastGroupMembersResponse'
            Core.<$> (x Core..@? "registeredMulticastGroupMembers")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRegisterTransitGatewayMulticastGroupMembersResponse' smart constructor.
data RegisterTransitGatewayMulticastGroupMembersResponse = RegisterTransitGatewayMulticastGroupMembersResponse'
  { -- | Information about the registered transit gateway multicast group members.
    registeredMulticastGroupMembers :: Core.Maybe Types.TransitGatewayMulticastRegisteredGroupMembers,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTransitGatewayMulticastGroupMembersResponse' value with any optional fields omitted.
mkRegisterTransitGatewayMulticastGroupMembersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RegisterTransitGatewayMulticastGroupMembersResponse
mkRegisterTransitGatewayMulticastGroupMembersResponse
  responseStatus =
    RegisterTransitGatewayMulticastGroupMembersResponse'
      { registeredMulticastGroupMembers =
          Core.Nothing,
        responseStatus
      }

-- | Information about the registered transit gateway multicast group members.
--
-- /Note:/ Consider using 'registeredMulticastGroupMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgmrrsRegisteredMulticastGroupMembers :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembersResponse (Core.Maybe Types.TransitGatewayMulticastRegisteredGroupMembers)
rtgmgmrrsRegisteredMulticastGroupMembers = Lens.field @"registeredMulticastGroupMembers"
{-# DEPRECATED rtgmgmrrsRegisteredMulticastGroupMembers "Use generic-lens or generic-optics with 'registeredMulticastGroupMembers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgmgmrrsResponseStatus :: Lens.Lens' RegisterTransitGatewayMulticastGroupMembersResponse Core.Int
rtgmgmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rtgmgmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
