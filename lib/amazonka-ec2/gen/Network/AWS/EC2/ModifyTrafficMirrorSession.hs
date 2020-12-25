{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ModifyTrafficMirrorSession (..),
    mkModifyTrafficMirrorSession,

    -- ** Request lenses
    mtmsTrafficMirrorSessionId,
    mtmsDescription,
    mtmsDryRun,
    mtmsPacketLength,
    mtmsRemoveFields,
    mtmsSessionNumber,
    mtmsTrafficMirrorFilterId,
    mtmsTrafficMirrorTargetId,
    mtmsVirtualNetworkId,

    -- * Destructuring the response
    ModifyTrafficMirrorSessionResponse (..),
    mkModifyTrafficMirrorSessionResponse,

    -- ** Response lenses
    mtmsrrsTrafficMirrorSession,
    mtmsrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyTrafficMirrorSession' smart constructor.
data ModifyTrafficMirrorSession = ModifyTrafficMirrorSession'
  { -- | The ID of the Traffic Mirror session.
    trafficMirrorSessionId :: Types.TrafficMirrorSessionId,
    -- | The description to assign to the Traffic Mirror session.
    description :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The number of bytes in each packet to mirror. These are bytes after the VXLAN header. To mirror a subset, set this to the length (in bytes) to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target. Do not specify this parameter when you want to mirror the entire packet.
    packetLength :: Core.Maybe Core.Int,
    -- | The properties that you want to remove from the Traffic Mirror session.
    --
    -- When you remove a property from a Traffic Mirror session, the property is set to the default.
    removeFields :: Core.Maybe [Types.TrafficMirrorSessionField],
    -- | The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets.
    --
    -- Valid values are 1-32766.
    sessionNumber :: Core.Maybe Core.Int,
    -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Core.Maybe Types.TrafficMirrorFilterId,
    -- | The Traffic Mirror target. The target must be in the same VPC as the source, or have a VPC peering connection with the source.
    trafficMirrorTargetId :: Core.Maybe Types.TrafficMirrorTargetId,
    -- | The virtual network ID of the Traffic Mirror session.
    virtualNetworkId :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTrafficMirrorSession' value with any optional fields omitted.
mkModifyTrafficMirrorSession ::
  -- | 'trafficMirrorSessionId'
  Types.TrafficMirrorSessionId ->
  ModifyTrafficMirrorSession
mkModifyTrafficMirrorSession trafficMirrorSessionId =
  ModifyTrafficMirrorSession'
    { trafficMirrorSessionId,
      description = Core.Nothing,
      dryRun = Core.Nothing,
      packetLength = Core.Nothing,
      removeFields = Core.Nothing,
      sessionNumber = Core.Nothing,
      trafficMirrorFilterId = Core.Nothing,
      trafficMirrorTargetId = Core.Nothing,
      virtualNetworkId = Core.Nothing
    }

-- | The ID of the Traffic Mirror session.
--
-- /Note:/ Consider using 'trafficMirrorSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsTrafficMirrorSessionId :: Lens.Lens' ModifyTrafficMirrorSession Types.TrafficMirrorSessionId
mtmsTrafficMirrorSessionId = Lens.field @"trafficMirrorSessionId"
{-# DEPRECATED mtmsTrafficMirrorSessionId "Use generic-lens or generic-optics with 'trafficMirrorSessionId' instead." #-}

-- | The description to assign to the Traffic Mirror session.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsDescription :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Types.String)
mtmsDescription = Lens.field @"description"
{-# DEPRECATED mtmsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsDryRun :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Core.Bool)
mtmsDryRun = Lens.field @"dryRun"
{-# DEPRECATED mtmsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The number of bytes in each packet to mirror. These are bytes after the VXLAN header. To mirror a subset, set this to the length (in bytes) to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target. Do not specify this parameter when you want to mirror the entire packet.
--
-- /Note:/ Consider using 'packetLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsPacketLength :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Core.Int)
mtmsPacketLength = Lens.field @"packetLength"
{-# DEPRECATED mtmsPacketLength "Use generic-lens or generic-optics with 'packetLength' instead." #-}

-- | The properties that you want to remove from the Traffic Mirror session.
--
-- When you remove a property from a Traffic Mirror session, the property is set to the default.
--
-- /Note:/ Consider using 'removeFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsRemoveFields :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe [Types.TrafficMirrorSessionField])
mtmsRemoveFields = Lens.field @"removeFields"
{-# DEPRECATED mtmsRemoveFields "Use generic-lens or generic-optics with 'removeFields' instead." #-}

-- | The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
--
-- /Note:/ Consider using 'sessionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsSessionNumber :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Core.Int)
mtmsSessionNumber = Lens.field @"sessionNumber"
{-# DEPRECATED mtmsSessionNumber "Use generic-lens or generic-optics with 'sessionNumber' instead." #-}

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsTrafficMirrorFilterId :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Types.TrafficMirrorFilterId)
mtmsTrafficMirrorFilterId = Lens.field @"trafficMirrorFilterId"
{-# DEPRECATED mtmsTrafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead." #-}

-- | The Traffic Mirror target. The target must be in the same VPC as the source, or have a VPC peering connection with the source.
--
-- /Note:/ Consider using 'trafficMirrorTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsTrafficMirrorTargetId :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Types.TrafficMirrorTargetId)
mtmsTrafficMirrorTargetId = Lens.field @"trafficMirrorTargetId"
{-# DEPRECATED mtmsTrafficMirrorTargetId "Use generic-lens or generic-optics with 'trafficMirrorTargetId' instead." #-}

-- | The virtual network ID of the Traffic Mirror session.
--
-- /Note:/ Consider using 'virtualNetworkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsVirtualNetworkId :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Core.Int)
mtmsVirtualNetworkId = Lens.field @"virtualNetworkId"
{-# DEPRECATED mtmsVirtualNetworkId "Use generic-lens or generic-optics with 'virtualNetworkId' instead." #-}

instance Core.AWSRequest ModifyTrafficMirrorSession where
  type
    Rs ModifyTrafficMirrorSession =
      ModifyTrafficMirrorSessionResponse
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
            ( Core.pure ("Action", "ModifyTrafficMirrorSession")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "TrafficMirrorSessionId" trafficMirrorSessionId)
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "PacketLength" Core.<$> packetLength)
                Core.<> (Core.toQueryList "RemoveField" Core.<$> removeFields)
                Core.<> (Core.toQueryValue "SessionNumber" Core.<$> sessionNumber)
                Core.<> ( Core.toQueryValue "TrafficMirrorFilterId"
                            Core.<$> trafficMirrorFilterId
                        )
                Core.<> ( Core.toQueryValue "TrafficMirrorTargetId"
                            Core.<$> trafficMirrorTargetId
                        )
                Core.<> (Core.toQueryValue "VirtualNetworkId" Core.<$> virtualNetworkId)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyTrafficMirrorSessionResponse'
            Core.<$> (x Core..@? "trafficMirrorSession")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyTrafficMirrorSessionResponse' smart constructor.
data ModifyTrafficMirrorSessionResponse = ModifyTrafficMirrorSessionResponse'
  { -- | Information about the Traffic Mirror session.
    trafficMirrorSession :: Core.Maybe Types.TrafficMirrorSession,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTrafficMirrorSessionResponse' value with any optional fields omitted.
mkModifyTrafficMirrorSessionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyTrafficMirrorSessionResponse
mkModifyTrafficMirrorSessionResponse responseStatus =
  ModifyTrafficMirrorSessionResponse'
    { trafficMirrorSession =
        Core.Nothing,
      responseStatus
    }

-- | Information about the Traffic Mirror session.
--
-- /Note:/ Consider using 'trafficMirrorSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsrrsTrafficMirrorSession :: Lens.Lens' ModifyTrafficMirrorSessionResponse (Core.Maybe Types.TrafficMirrorSession)
mtmsrrsTrafficMirrorSession = Lens.field @"trafficMirrorSession"
{-# DEPRECATED mtmsrrsTrafficMirrorSession "Use generic-lens or generic-optics with 'trafficMirrorSession' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsrrsResponseStatus :: Lens.Lens' ModifyTrafficMirrorSessionResponse Core.Int
mtmsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mtmsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
