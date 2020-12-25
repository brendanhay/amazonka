{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeregisterTransitGatewayMulticastGroupSources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified sources (network interfaces) from the transit gateway multicast group.
module Network.AWS.EC2.DeregisterTransitGatewayMulticastGroupSources
  ( -- * Creating a request
    DeregisterTransitGatewayMulticastGroupSources (..),
    mkDeregisterTransitGatewayMulticastGroupSources,

    -- ** Request lenses
    dtgmgsDryRun,
    dtgmgsGroupIpAddress,
    dtgmgsNetworkInterfaceIds,
    dtgmgsTransitGatewayMulticastDomainId,

    -- * Destructuring the response
    DeregisterTransitGatewayMulticastGroupSourcesResponse (..),
    mkDeregisterTransitGatewayMulticastGroupSourcesResponse,

    -- ** Response lenses
    dtgmgsrrsDeregisteredMulticastGroupSources,
    dtgmgsrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterTransitGatewayMulticastGroupSources' smart constructor.
data DeregisterTransitGatewayMulticastGroupSources = DeregisterTransitGatewayMulticastGroupSources'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Core.Maybe Types.String,
    -- | The IDs of the group sources' network interfaces.
    networkInterfaceIds :: Core.Maybe [Types.NetworkInterfaceId],
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Types.TransitGatewayMulticastDomainId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterTransitGatewayMulticastGroupSources' value with any optional fields omitted.
mkDeregisterTransitGatewayMulticastGroupSources ::
  DeregisterTransitGatewayMulticastGroupSources
mkDeregisterTransitGatewayMulticastGroupSources =
  DeregisterTransitGatewayMulticastGroupSources'
    { dryRun =
        Core.Nothing,
      groupIpAddress = Core.Nothing,
      networkInterfaceIds = Core.Nothing,
      transitGatewayMulticastDomainId = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgsDryRun :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSources (Core.Maybe Core.Bool)
dtgmgsDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtgmgsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgsGroupIpAddress :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSources (Core.Maybe Types.String)
dtgmgsGroupIpAddress = Lens.field @"groupIpAddress"
{-# DEPRECATED dtgmgsGroupIpAddress "Use generic-lens or generic-optics with 'groupIpAddress' instead." #-}

-- | The IDs of the group sources' network interfaces.
--
-- /Note:/ Consider using 'networkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgsNetworkInterfaceIds :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSources (Core.Maybe [Types.NetworkInterfaceId])
dtgmgsNetworkInterfaceIds = Lens.field @"networkInterfaceIds"
{-# DEPRECATED dtgmgsNetworkInterfaceIds "Use generic-lens or generic-optics with 'networkInterfaceIds' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgsTransitGatewayMulticastDomainId :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSources (Core.Maybe Types.TransitGatewayMulticastDomainId)
dtgmgsTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# DEPRECATED dtgmgsTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

instance
  Core.AWSRequest
    DeregisterTransitGatewayMulticastGroupSources
  where
  type
    Rs DeregisterTransitGatewayMulticastGroupSources =
      DeregisterTransitGatewayMulticastGroupSourcesResponse
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
                ("Action", "DeregisterTransitGatewayMulticastGroupSources")
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
          DeregisterTransitGatewayMulticastGroupSourcesResponse'
            Core.<$> (x Core..@? "deregisteredMulticastGroupSources")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeregisterTransitGatewayMulticastGroupSourcesResponse' smart constructor.
data DeregisterTransitGatewayMulticastGroupSourcesResponse = DeregisterTransitGatewayMulticastGroupSourcesResponse'
  { -- | Information about the deregistered group sources.
    deregisteredMulticastGroupSources :: Core.Maybe Types.TransitGatewayMulticastDeregisteredGroupSources,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterTransitGatewayMulticastGroupSourcesResponse' value with any optional fields omitted.
mkDeregisterTransitGatewayMulticastGroupSourcesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeregisterTransitGatewayMulticastGroupSourcesResponse
mkDeregisterTransitGatewayMulticastGroupSourcesResponse
  responseStatus =
    DeregisterTransitGatewayMulticastGroupSourcesResponse'
      { deregisteredMulticastGroupSources =
          Core.Nothing,
        responseStatus
      }

-- | Information about the deregistered group sources.
--
-- /Note:/ Consider using 'deregisteredMulticastGroupSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgsrrsDeregisteredMulticastGroupSources :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSourcesResponse (Core.Maybe Types.TransitGatewayMulticastDeregisteredGroupSources)
dtgmgsrrsDeregisteredMulticastGroupSources = Lens.field @"deregisteredMulticastGroupSources"
{-# DEPRECATED dtgmgsrrsDeregisteredMulticastGroupSources "Use generic-lens or generic-optics with 'deregisteredMulticastGroupSources' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgsrrsResponseStatus :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSourcesResponse Core.Int
dtgmgsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtgmgsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
