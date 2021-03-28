{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyTrafficMirrorSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a Traffic Mirror session.
module Network.AWS.EC2.ModifyTrafficMirrorSession
    (
    -- * Creating a request
      ModifyTrafficMirrorSession (..)
    , mkModifyTrafficMirrorSession
    -- ** Request lenses
    , mtmsTrafficMirrorSessionId
    , mtmsDescription
    , mtmsDryRun
    , mtmsPacketLength
    , mtmsRemoveFields
    , mtmsSessionNumber
    , mtmsTrafficMirrorFilterId
    , mtmsTrafficMirrorTargetId
    , mtmsVirtualNetworkId

    -- * Destructuring the response
    , ModifyTrafficMirrorSessionResponse (..)
    , mkModifyTrafficMirrorSessionResponse
    -- ** Response lenses
    , mtmsrrsTrafficMirrorSession
    , mtmsrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyTrafficMirrorSession' smart constructor.
data ModifyTrafficMirrorSession = ModifyTrafficMirrorSession'
  { trafficMirrorSessionId :: Types.TrafficMirrorSessionId
    -- ^ The ID of the Traffic Mirror session.
  , description :: Core.Maybe Core.Text
    -- ^ The description to assign to the Traffic Mirror session.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , packetLength :: Core.Maybe Core.Int
    -- ^ The number of bytes in each packet to mirror. These are bytes after the VXLAN header. To mirror a subset, set this to the length (in bytes) to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target. Do not specify this parameter when you want to mirror the entire packet.
  , removeFields :: Core.Maybe [Types.TrafficMirrorSessionField]
    -- ^ The properties that you want to remove from the Traffic Mirror session.
--
-- When you remove a property from a Traffic Mirror session, the property is set to the default.
  , sessionNumber :: Core.Maybe Core.Int
    -- ^ The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
  , trafficMirrorFilterId :: Core.Maybe Types.TrafficMirrorFilterId
    -- ^ The ID of the Traffic Mirror filter.
  , trafficMirrorTargetId :: Core.Maybe Types.TrafficMirrorTargetId
    -- ^ The Traffic Mirror target. The target must be in the same VPC as the source, or have a VPC peering connection with the source.
  , virtualNetworkId :: Core.Maybe Core.Int
    -- ^ The virtual network ID of the Traffic Mirror session.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTrafficMirrorSession' value with any optional fields omitted.
mkModifyTrafficMirrorSession
    :: Types.TrafficMirrorSessionId -- ^ 'trafficMirrorSessionId'
    -> ModifyTrafficMirrorSession
mkModifyTrafficMirrorSession trafficMirrorSessionId
  = ModifyTrafficMirrorSession'{trafficMirrorSessionId,
                                description = Core.Nothing, dryRun = Core.Nothing,
                                packetLength = Core.Nothing, removeFields = Core.Nothing,
                                sessionNumber = Core.Nothing, trafficMirrorFilterId = Core.Nothing,
                                trafficMirrorTargetId = Core.Nothing,
                                virtualNetworkId = Core.Nothing}

-- | The ID of the Traffic Mirror session.
--
-- /Note:/ Consider using 'trafficMirrorSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsTrafficMirrorSessionId :: Lens.Lens' ModifyTrafficMirrorSession Types.TrafficMirrorSessionId
mtmsTrafficMirrorSessionId = Lens.field @"trafficMirrorSessionId"
{-# INLINEABLE mtmsTrafficMirrorSessionId #-}
{-# DEPRECATED trafficMirrorSessionId "Use generic-lens or generic-optics with 'trafficMirrorSessionId' instead"  #-}

-- | The description to assign to the Traffic Mirror session.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsDescription :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Core.Text)
mtmsDescription = Lens.field @"description"
{-# INLINEABLE mtmsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsDryRun :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Core.Bool)
mtmsDryRun = Lens.field @"dryRun"
{-# INLINEABLE mtmsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The number of bytes in each packet to mirror. These are bytes after the VXLAN header. To mirror a subset, set this to the length (in bytes) to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target. Do not specify this parameter when you want to mirror the entire packet.
--
-- /Note:/ Consider using 'packetLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsPacketLength :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Core.Int)
mtmsPacketLength = Lens.field @"packetLength"
{-# INLINEABLE mtmsPacketLength #-}
{-# DEPRECATED packetLength "Use generic-lens or generic-optics with 'packetLength' instead"  #-}

-- | The properties that you want to remove from the Traffic Mirror session.
--
-- When you remove a property from a Traffic Mirror session, the property is set to the default.
--
-- /Note:/ Consider using 'removeFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsRemoveFields :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe [Types.TrafficMirrorSessionField])
mtmsRemoveFields = Lens.field @"removeFields"
{-# INLINEABLE mtmsRemoveFields #-}
{-# DEPRECATED removeFields "Use generic-lens or generic-optics with 'removeFields' instead"  #-}

-- | The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
--
-- /Note:/ Consider using 'sessionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsSessionNumber :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Core.Int)
mtmsSessionNumber = Lens.field @"sessionNumber"
{-# INLINEABLE mtmsSessionNumber #-}
{-# DEPRECATED sessionNumber "Use generic-lens or generic-optics with 'sessionNumber' instead"  #-}

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsTrafficMirrorFilterId :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Types.TrafficMirrorFilterId)
mtmsTrafficMirrorFilterId = Lens.field @"trafficMirrorFilterId"
{-# INLINEABLE mtmsTrafficMirrorFilterId #-}
{-# DEPRECATED trafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead"  #-}

-- | The Traffic Mirror target. The target must be in the same VPC as the source, or have a VPC peering connection with the source.
--
-- /Note:/ Consider using 'trafficMirrorTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsTrafficMirrorTargetId :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Types.TrafficMirrorTargetId)
mtmsTrafficMirrorTargetId = Lens.field @"trafficMirrorTargetId"
{-# INLINEABLE mtmsTrafficMirrorTargetId #-}
{-# DEPRECATED trafficMirrorTargetId "Use generic-lens or generic-optics with 'trafficMirrorTargetId' instead"  #-}

-- | The virtual network ID of the Traffic Mirror session.
--
-- /Note:/ Consider using 'virtualNetworkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsVirtualNetworkId :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Core.Int)
mtmsVirtualNetworkId = Lens.field @"virtualNetworkId"
{-# INLINEABLE mtmsVirtualNetworkId #-}
{-# DEPRECATED virtualNetworkId "Use generic-lens or generic-optics with 'virtualNetworkId' instead"  #-}

instance Core.ToQuery ModifyTrafficMirrorSession where
        toQuery ModifyTrafficMirrorSession{..}
          = Core.toQueryPair "Action"
              ("ModifyTrafficMirrorSession" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TrafficMirrorSessionId" trafficMirrorSessionId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PacketLength")
                packetLength
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "RemoveField")
                removeFields
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SessionNumber")
                sessionNumber
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TrafficMirrorFilterId")
                trafficMirrorFilterId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TrafficMirrorTargetId")
                trafficMirrorTargetId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VirtualNetworkId")
                virtualNetworkId

instance Core.ToHeaders ModifyTrafficMirrorSession where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyTrafficMirrorSession where
        type Rs ModifyTrafficMirrorSession =
             ModifyTrafficMirrorSessionResponse
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
                 ModifyTrafficMirrorSessionResponse' Core.<$>
                   (x Core..@? "trafficMirrorSession") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyTrafficMirrorSessionResponse' smart constructor.
data ModifyTrafficMirrorSessionResponse = ModifyTrafficMirrorSessionResponse'
  { trafficMirrorSession :: Core.Maybe Types.TrafficMirrorSession
    -- ^ Information about the Traffic Mirror session.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTrafficMirrorSessionResponse' value with any optional fields omitted.
mkModifyTrafficMirrorSessionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyTrafficMirrorSessionResponse
mkModifyTrafficMirrorSessionResponse responseStatus
  = ModifyTrafficMirrorSessionResponse'{trafficMirrorSession =
                                          Core.Nothing,
                                        responseStatus}

-- | Information about the Traffic Mirror session.
--
-- /Note:/ Consider using 'trafficMirrorSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsrrsTrafficMirrorSession :: Lens.Lens' ModifyTrafficMirrorSessionResponse (Core.Maybe Types.TrafficMirrorSession)
mtmsrrsTrafficMirrorSession = Lens.field @"trafficMirrorSession"
{-# INLINEABLE mtmsrrsTrafficMirrorSession #-}
{-# DEPRECATED trafficMirrorSession "Use generic-lens or generic-optics with 'trafficMirrorSession' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsrrsResponseStatus :: Lens.Lens' ModifyTrafficMirrorSessionResponse Core.Int
mtmsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mtmsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
