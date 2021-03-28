{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeregisterTransitGatewayMulticastGroupMembers (..)
    , mkDeregisterTransitGatewayMulticastGroupMembers
    -- ** Request lenses
    , dtgmgmDryRun
    , dtgmgmGroupIpAddress
    , dtgmgmNetworkInterfaceIds
    , dtgmgmTransitGatewayMulticastDomainId

    -- * Destructuring the response
    , DeregisterTransitGatewayMulticastGroupMembersResponse (..)
    , mkDeregisterTransitGatewayMulticastGroupMembersResponse
    -- ** Response lenses
    , dtgmgmrrsDeregisteredMulticastGroupMembers
    , dtgmgmrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterTransitGatewayMulticastGroupMembers' smart constructor.
data DeregisterTransitGatewayMulticastGroupMembers = DeregisterTransitGatewayMulticastGroupMembers'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , groupIpAddress :: Core.Maybe Core.Text
    -- ^ The IP address assigned to the transit gateway multicast group.
  , networkInterfaceIds :: Core.Maybe [Types.NetworkInterfaceId]
    -- ^ The IDs of the group members' network interfaces.
  , transitGatewayMulticastDomainId :: Core.Maybe Types.TransitGatewayMulticastDomainId
    -- ^ The ID of the transit gateway multicast domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterTransitGatewayMulticastGroupMembers' value with any optional fields omitted.
mkDeregisterTransitGatewayMulticastGroupMembers
    :: DeregisterTransitGatewayMulticastGroupMembers
mkDeregisterTransitGatewayMulticastGroupMembers
  = DeregisterTransitGatewayMulticastGroupMembers'{dryRun =
                                                     Core.Nothing,
                                                   groupIpAddress = Core.Nothing,
                                                   networkInterfaceIds = Core.Nothing,
                                                   transitGatewayMulticastDomainId = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgmDryRun :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembers (Core.Maybe Core.Bool)
dtgmgmDryRun = Lens.field @"dryRun"
{-# INLINEABLE dtgmgmDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgmGroupIpAddress :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembers (Core.Maybe Core.Text)
dtgmgmGroupIpAddress = Lens.field @"groupIpAddress"
{-# INLINEABLE dtgmgmGroupIpAddress #-}
{-# DEPRECATED groupIpAddress "Use generic-lens or generic-optics with 'groupIpAddress' instead"  #-}

-- | The IDs of the group members' network interfaces.
--
-- /Note:/ Consider using 'networkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgmNetworkInterfaceIds :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembers (Core.Maybe [Types.NetworkInterfaceId])
dtgmgmNetworkInterfaceIds = Lens.field @"networkInterfaceIds"
{-# INLINEABLE dtgmgmNetworkInterfaceIds #-}
{-# DEPRECATED networkInterfaceIds "Use generic-lens or generic-optics with 'networkInterfaceIds' instead"  #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgmTransitGatewayMulticastDomainId :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembers (Core.Maybe Types.TransitGatewayMulticastDomainId)
dtgmgmTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# INLINEABLE dtgmgmTransitGatewayMulticastDomainId #-}
{-# DEPRECATED transitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead"  #-}

instance Core.ToQuery DeregisterTransitGatewayMulticastGroupMembers
         where
        toQuery DeregisterTransitGatewayMulticastGroupMembers{..}
          = Core.toQueryPair "Action"
              ("DeregisterTransitGatewayMulticastGroupMembers" :: Core.Text)
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
           DeregisterTransitGatewayMulticastGroupMembers
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest
           DeregisterTransitGatewayMulticastGroupMembers
         where
        type Rs DeregisterTransitGatewayMulticastGroupMembers =
             DeregisterTransitGatewayMulticastGroupMembersResponse
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
                 DeregisterTransitGatewayMulticastGroupMembersResponse' Core.<$>
                   (x Core..@? "deregisteredMulticastGroupMembers") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeregisterTransitGatewayMulticastGroupMembersResponse' smart constructor.
data DeregisterTransitGatewayMulticastGroupMembersResponse = DeregisterTransitGatewayMulticastGroupMembersResponse'
  { deregisteredMulticastGroupMembers :: Core.Maybe Types.TransitGatewayMulticastDeregisteredGroupMembers
    -- ^ Information about the deregistered members.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterTransitGatewayMulticastGroupMembersResponse' value with any optional fields omitted.
mkDeregisterTransitGatewayMulticastGroupMembersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeregisterTransitGatewayMulticastGroupMembersResponse
mkDeregisterTransitGatewayMulticastGroupMembersResponse
  responseStatus
  = DeregisterTransitGatewayMulticastGroupMembersResponse'{deregisteredMulticastGroupMembers
                                                             = Core.Nothing,
                                                           responseStatus}

-- | Information about the deregistered members.
--
-- /Note:/ Consider using 'deregisteredMulticastGroupMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgmrrsDeregisteredMulticastGroupMembers :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembersResponse (Core.Maybe Types.TransitGatewayMulticastDeregisteredGroupMembers)
dtgmgmrrsDeregisteredMulticastGroupMembers = Lens.field @"deregisteredMulticastGroupMembers"
{-# INLINEABLE dtgmgmrrsDeregisteredMulticastGroupMembers #-}
{-# DEPRECATED deregisteredMulticastGroupMembers "Use generic-lens or generic-optics with 'deregisteredMulticastGroupMembers' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmgmrrsResponseStatus :: Lens.Lens' DeregisterTransitGatewayMulticastGroupMembersResponse Core.Int
dtgmgmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtgmgmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
