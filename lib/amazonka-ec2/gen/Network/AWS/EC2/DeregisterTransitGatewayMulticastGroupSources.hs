{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeregisterTransitGatewayMulticastGroupSources (..)
    , mkDeregisterTransitGatewayMulticastGroupSources
    -- ** Request lenses
    , dtgmgsDryRun
    , dtgmgsGroupIpAddress
    , dtgmgsNetworkInterfaceIds
    , dtgmgsTransitGatewayMulticastDomainId

    -- * Destructuring the response
    , DeregisterTransitGatewayMulticastGroupSourcesResponse (..)
    , mkDeregisterTransitGatewayMulticastGroupSourcesResponse
    -- ** Response lenses
    , dtgmgsrrsDeregisteredMulticastGroupSources
    , dtgmgsrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterTransitGatewayMulticastGroupSources' smart constructor.
data DeregisterTransitGatewayMulticastGroupSources = DeregisterTransitGatewayMulticastGroupSources'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , groupIpAddress :: Core.Maybe Core.Text
    -- ^ The IP address assigned to the transit gateway multicast group.
  , networkInterfaceIds :: Core.Maybe [Types.NetworkInterfaceId]
    -- ^ The IDs of the group sources' network interfaces.
  , transitGatewayMulticastDomainId :: Core.Maybe Types.TransitGatewayMulticastDomainId
    -- ^ The ID of the transit gateway multicast domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterTransitGatewayMulticastGroupSources' value with any optional fields omitted.
mkDeregisterTransitGatewayMulticastGroupSources
    :: DeregisterTransitGatewayMulticastGroupSources
mkDeregisterTransitGatewayMulticastGroupSources
  = DeregisterTransitGatewayMulticastGroupSources'{dryRun =
                                                     Core.Nothing,
                                                   groupIpAddress = Core.Nothing,
                                                   networkInterfaceIds = Core.Nothing,
                                                   transitGatewayMulticastDomainId = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgsDryRun :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSources (Core.Maybe Core.Bool)
dtgmgsDryRun = Lens.field @"dryRun"
{-# INLINEABLE dtgmgsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgsGroupIpAddress :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSources (Core.Maybe Core.Text)
dtgmgsGroupIpAddress = Lens.field @"groupIpAddress"
{-# INLINEABLE dtgmgsGroupIpAddress #-}
{-# DEPRECATED groupIpAddress "Use generic-lens or generic-optics with 'groupIpAddress' instead"  #-}

-- | The IDs of the group sources' network interfaces.
--
-- /Note:/ Consider using 'networkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgsNetworkInterfaceIds :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSources (Core.Maybe [Types.NetworkInterfaceId])
dtgmgsNetworkInterfaceIds = Lens.field @"networkInterfaceIds"
{-# INLINEABLE dtgmgsNetworkInterfaceIds #-}
{-# DEPRECATED networkInterfaceIds "Use generic-lens or generic-optics with 'networkInterfaceIds' instead"  #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgsTransitGatewayMulticastDomainId :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSources (Core.Maybe Types.TransitGatewayMulticastDomainId)
dtgmgsTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# INLINEABLE dtgmgsTransitGatewayMulticastDomainId #-}
{-# DEPRECATED transitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead"  #-}

instance Core.ToQuery DeregisterTransitGatewayMulticastGroupSources
         where
        toQuery DeregisterTransitGatewayMulticastGroupSources{..}
          = Core.toQueryPair "Action"
              ("DeregisterTransitGatewayMulticastGroupSources" :: Core.Text)
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

instance Core.ToHeaders
           DeregisterTransitGatewayMulticastGroupSources
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest
           DeregisterTransitGatewayMulticastGroupSources
         where
        type Rs DeregisterTransitGatewayMulticastGroupSources =
             DeregisterTransitGatewayMulticastGroupSourcesResponse
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
                 DeregisterTransitGatewayMulticastGroupSourcesResponse' Core.<$>
                   (x Core..@? "deregisteredMulticastGroupSources") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeregisterTransitGatewayMulticastGroupSourcesResponse' smart constructor.
data DeregisterTransitGatewayMulticastGroupSourcesResponse = DeregisterTransitGatewayMulticastGroupSourcesResponse'
  { deregisteredMulticastGroupSources :: Core.Maybe Types.TransitGatewayMulticastDeregisteredGroupSources
    -- ^ Information about the deregistered group sources.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterTransitGatewayMulticastGroupSourcesResponse' value with any optional fields omitted.
mkDeregisterTransitGatewayMulticastGroupSourcesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeregisterTransitGatewayMulticastGroupSourcesResponse
mkDeregisterTransitGatewayMulticastGroupSourcesResponse
  responseStatus
  = DeregisterTransitGatewayMulticastGroupSourcesResponse'{deregisteredMulticastGroupSources
                                                             = Core.Nothing,
                                                           responseStatus}

-- | Information about the deregistered group sources.
--
-- /Note:/ Consider using 'deregisteredMulticastGroupSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgsrrsDeregisteredMulticastGroupSources :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSourcesResponse (Core.Maybe Types.TransitGatewayMulticastDeregisteredGroupSources)
dtgmgsrrsDeregisteredMulticastGroupSources = Lens.field @"deregisteredMulticastGroupSources"
{-# INLINEABLE dtgmgsrrsDeregisteredMulticastGroupSources #-}
{-# DEPRECATED deregisteredMulticastGroupSources "Use generic-lens or generic-optics with 'deregisteredMulticastGroupSources' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgsrrsResponseStatus :: Lens.Lens' DeregisterTransitGatewayMulticastGroupSourcesResponse Core.Int
dtgmgsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtgmgsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
