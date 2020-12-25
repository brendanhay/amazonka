{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeregisterTransitGatewayMulticastGroupMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified members (network interfaces) from the transit gateway multicast group.
module Network.AWS.EC2.DeregisterTransitGatewayMulticastGroupMembers
  ( -- * Creating a request
    DeregisterTransitGatewayMulticastGroupMembers (..),
    mkDeregisterTransitGatewayMulticastGroupMembers,

    -- ** Request lenses
    dtgmgmDryRun,
    dtgmgmGroupIpAddress,
    dtgmgmNetworkInterfaceIds,
    dtgmgmTransitGatewayMulticastDomainId,

    -- * Destructuring the response
    DeregisterTransitGatewayMulticastGroupMembersResponse (..),
    mkDeregisterTransitGatewayMulticastGroupMembersResponse,

    -- ** Response lenses
    dtgmgmrrsDeregisteredMulticastGroupMembers,
    dtgmgmrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterTransitGatewayMulticastGroupMembers' smart constructor.
data DeregisterTransitGatewayMulticastGroupMembers = DeregisterTransitGatewayMulticastGroupMembers'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Core.Maybe Types.String,
    -- | The IDs of the group members' network interfaces.
    networkInterfaceIds :: Core.Maybe [Types.NetworkInterfaceId],
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Types.TransitGatewayMulticastDomainId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterTransitGatewayMulticastGroupMembers' value with any optional fields omitted.
mkDeregisterTransitGatewayMulticastGroupMembers ::
  DeregisterTransitGatewayMulticastGroupMembers
mkDeregisterTransitGatewayMulticastGroupMembers =
  DeregisterTransitGatewayMulticastGroupMembers'
    { dryRun =
        Core.Nothing,
      groupIpAddress = Core.Nothing,
      networkInterfaceIds = Core.Nothing,
      transitGatewayMulticastDomainId = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgmDryRun :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembers (Core.Maybe Core.Bool)
dtgmgmDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtgmgmDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgmGroupIpAddress :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembers (Core.Maybe Types.String)
dtgmgmGroupIpAddress = Lens.field @"groupIpAddress"
{-# DEPRECATED dtgmgmGroupIpAddress "Use generic-lens or generic-optics with 'groupIpAddress' instead." #-}

-- | The IDs of the group members' network interfaces.
--
-- /Note:/ Consider using 'networkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgmNetworkInterfaceIds :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembers (Core.Maybe [Types.NetworkInterfaceId])
dtgmgmNetworkInterfaceIds = Lens.field @"networkInterfaceIds"
{-# DEPRECATED dtgmgmNetworkInterfaceIds "Use generic-lens or generic-optics with 'networkInterfaceIds' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgmTransitGatewayMulticastDomainId :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembers (Core.Maybe Types.TransitGatewayMulticastDomainId)
dtgmgmTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# DEPRECATED dtgmgmTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

instance
  Core.AWSRequest
    DeregisterTransitGatewayMulticastGroupMembers
  where
  type
    Rs DeregisterTransitGatewayMulticastGroupMembers =
      DeregisterTransitGatewayMulticastGroupMembersResponse
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
                ("Action", "DeregisterTransitGatewayMulticastGroupMembers")
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
          DeregisterTransitGatewayMulticastGroupMembersResponse'
            Core.<$> (x Core..@? "deregisteredMulticastGroupMembers")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeregisterTransitGatewayMulticastGroupMembersResponse' smart constructor.
data DeregisterTransitGatewayMulticastGroupMembersResponse = DeregisterTransitGatewayMulticastGroupMembersResponse'
  { -- | Information about the deregistered members.
    deregisteredMulticastGroupMembers :: Core.Maybe Types.TransitGatewayMulticastDeregisteredGroupMembers,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterTransitGatewayMulticastGroupMembersResponse' value with any optional fields omitted.
mkDeregisterTransitGatewayMulticastGroupMembersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeregisterTransitGatewayMulticastGroupMembersResponse
mkDeregisterTransitGatewayMulticastGroupMembersResponse
  responseStatus =
    DeregisterTransitGatewayMulticastGroupMembersResponse'
      { deregisteredMulticastGroupMembers =
          Core.Nothing,
        responseStatus
      }

-- | Information about the deregistered members.
--
-- /Note:/ Consider using 'deregisteredMulticastGroupMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgmrrsDeregisteredMulticastGroupMembers :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembersResponse (Core.Maybe Types.TransitGatewayMulticastDeregisteredGroupMembers)
dtgmgmrrsDeregisteredMulticastGroupMembers = Lens.field @"deregisteredMulticastGroupMembers"
{-# DEPRECATED dtgmgmrrsDeregisteredMulticastGroupMembers "Use generic-lens or generic-optics with 'deregisteredMulticastGroupMembers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgmrrsResponseStatus :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembersResponse Core.Int
dtgmgmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtgmgmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
