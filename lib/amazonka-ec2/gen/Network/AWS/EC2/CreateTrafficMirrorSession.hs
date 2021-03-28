{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTrafficMirrorSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Traffic Mirror session.
--
-- A Traffic Mirror session actively copies packets from a Traffic Mirror source to a Traffic Mirror target. Create a filter, and then assign it to the session to define a subset of the traffic to mirror, for example all TCP traffic.
-- The Traffic Mirror source and the Traffic Mirror target (monitoring appliances) can be in the same VPC, or in a different VPC connected via VPC peering or a transit gateway. 
-- By default, no traffic is mirrored. Use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTrafficMirrorFilter.htm CreateTrafficMirrorFilter> to create filter rules that specify the traffic to mirror.
module Network.AWS.EC2.CreateTrafficMirrorSession
    (
    -- * Creating a request
      CreateTrafficMirrorSession (..)
    , mkCreateTrafficMirrorSession
    -- ** Request lenses
    , ctmsNetworkInterfaceId
    , ctmsTrafficMirrorTargetId
    , ctmsTrafficMirrorFilterId
    , ctmsSessionNumber
    , ctmsClientToken
    , ctmsDescription
    , ctmsDryRun
    , ctmsPacketLength
    , ctmsTagSpecifications
    , ctmsVirtualNetworkId

    -- * Destructuring the response
    , CreateTrafficMirrorSessionResponse (..)
    , mkCreateTrafficMirrorSessionResponse
    -- ** Response lenses
    , ctmsrrsClientToken
    , ctmsrrsTrafficMirrorSession
    , ctmsrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTrafficMirrorSession' smart constructor.
data CreateTrafficMirrorSession = CreateTrafficMirrorSession'
  { networkInterfaceId :: Types.NetworkInterfaceId
    -- ^ The ID of the source network interface.
  , trafficMirrorTargetId :: Types.TrafficMirrorTargetId
    -- ^ The ID of the Traffic Mirror target.
  , trafficMirrorFilterId :: Types.TrafficMirrorFilterId
    -- ^ The ID of the Traffic Mirror filter.
  , sessionNumber :: Core.Int
    -- ^ The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
  , description :: Core.Maybe Core.Text
    -- ^ The description of the Traffic Mirror session.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , packetLength :: Core.Maybe Core.Int
    -- ^ The number of bytes in each packet to mirror. These are bytes after the VXLAN header. Do not specify this parameter when you want to mirror the entire packet. To mirror a subset of the packet, set this to the length (in bytes) that you want to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target.
--
-- If you do not want to mirror the entire packet, use the @PacketLength@ parameter to specify the number of bytes in each packet to mirror.
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to assign to a Traffic Mirror session.
  , virtualNetworkId :: Core.Maybe Core.Int
    -- ^ The VXLAN ID for the Traffic Mirror session. For more information about the VXLAN protocol, see <https://tools.ietf.org/html/rfc7348 RFC 7348> . If you do not specify a @VirtualNetworkId@ , an account-wide unique id is chosen at random.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrafficMirrorSession' value with any optional fields omitted.
mkCreateTrafficMirrorSession
    :: Types.NetworkInterfaceId -- ^ 'networkInterfaceId'
    -> Types.TrafficMirrorTargetId -- ^ 'trafficMirrorTargetId'
    -> Types.TrafficMirrorFilterId -- ^ 'trafficMirrorFilterId'
    -> Core.Int -- ^ 'sessionNumber'
    -> CreateTrafficMirrorSession
mkCreateTrafficMirrorSession networkInterfaceId
  trafficMirrorTargetId trafficMirrorFilterId sessionNumber
  = CreateTrafficMirrorSession'{networkInterfaceId,
                                trafficMirrorTargetId, trafficMirrorFilterId, sessionNumber,
                                clientToken = Core.Nothing, description = Core.Nothing,
                                dryRun = Core.Nothing, packetLength = Core.Nothing,
                                tagSpecifications = Core.Nothing, virtualNetworkId = Core.Nothing}

-- | The ID of the source network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsNetworkInterfaceId :: Lens.Lens' CreateTrafficMirrorSession Types.NetworkInterfaceId
ctmsNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE ctmsNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The ID of the Traffic Mirror target.
--
-- /Note:/ Consider using 'trafficMirrorTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsTrafficMirrorTargetId :: Lens.Lens' CreateTrafficMirrorSession Types.TrafficMirrorTargetId
ctmsTrafficMirrorTargetId = Lens.field @"trafficMirrorTargetId"
{-# INLINEABLE ctmsTrafficMirrorTargetId #-}
{-# DEPRECATED trafficMirrorTargetId "Use generic-lens or generic-optics with 'trafficMirrorTargetId' instead"  #-}

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsTrafficMirrorFilterId :: Lens.Lens' CreateTrafficMirrorSession Types.TrafficMirrorFilterId
ctmsTrafficMirrorFilterId = Lens.field @"trafficMirrorFilterId"
{-# INLINEABLE ctmsTrafficMirrorFilterId #-}
{-# DEPRECATED trafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead"  #-}

-- | The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
--
-- /Note:/ Consider using 'sessionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsSessionNumber :: Lens.Lens' CreateTrafficMirrorSession Core.Int
ctmsSessionNumber = Lens.field @"sessionNumber"
{-# INLINEABLE ctmsSessionNumber #-}
{-# DEPRECATED sessionNumber "Use generic-lens or generic-optics with 'sessionNumber' instead"  #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsClientToken :: Lens.Lens' CreateTrafficMirrorSession (Core.Maybe Core.Text)
ctmsClientToken = Lens.field @"clientToken"
{-# INLINEABLE ctmsClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The description of the Traffic Mirror session.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsDescription :: Lens.Lens' CreateTrafficMirrorSession (Core.Maybe Core.Text)
ctmsDescription = Lens.field @"description"
{-# INLINEABLE ctmsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsDryRun :: Lens.Lens' CreateTrafficMirrorSession (Core.Maybe Core.Bool)
ctmsDryRun = Lens.field @"dryRun"
{-# INLINEABLE ctmsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The number of bytes in each packet to mirror. These are bytes after the VXLAN header. Do not specify this parameter when you want to mirror the entire packet. To mirror a subset of the packet, set this to the length (in bytes) that you want to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target.
--
-- If you do not want to mirror the entire packet, use the @PacketLength@ parameter to specify the number of bytes in each packet to mirror.
--
-- /Note:/ Consider using 'packetLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsPacketLength :: Lens.Lens' CreateTrafficMirrorSession (Core.Maybe Core.Int)
ctmsPacketLength = Lens.field @"packetLength"
{-# INLINEABLE ctmsPacketLength #-}
{-# DEPRECATED packetLength "Use generic-lens or generic-optics with 'packetLength' instead"  #-}

-- | The tags to assign to a Traffic Mirror session.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsTagSpecifications :: Lens.Lens' CreateTrafficMirrorSession (Core.Maybe [Types.TagSpecification])
ctmsTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE ctmsTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

-- | The VXLAN ID for the Traffic Mirror session. For more information about the VXLAN protocol, see <https://tools.ietf.org/html/rfc7348 RFC 7348> . If you do not specify a @VirtualNetworkId@ , an account-wide unique id is chosen at random.
--
-- /Note:/ Consider using 'virtualNetworkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsVirtualNetworkId :: Lens.Lens' CreateTrafficMirrorSession (Core.Maybe Core.Int)
ctmsVirtualNetworkId = Lens.field @"virtualNetworkId"
{-# INLINEABLE ctmsVirtualNetworkId #-}
{-# DEPRECATED virtualNetworkId "Use generic-lens or generic-optics with 'virtualNetworkId' instead"  #-}

instance Core.ToQuery CreateTrafficMirrorSession where
        toQuery CreateTrafficMirrorSession{..}
          = Core.toQueryPair "Action"
              ("CreateTrafficMirrorSession" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "NetworkInterfaceId" networkInterfaceId
              Core.<>
              Core.toQueryPair "TrafficMirrorTargetId" trafficMirrorTargetId
              Core.<>
              Core.toQueryPair "TrafficMirrorFilterId" trafficMirrorFilterId
              Core.<> Core.toQueryPair "SessionNumber" sessionNumber
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PacketLength")
                packetLength
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VirtualNetworkId")
                virtualNetworkId

instance Core.ToHeaders CreateTrafficMirrorSession where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateTrafficMirrorSession where
        type Rs CreateTrafficMirrorSession =
             CreateTrafficMirrorSessionResponse
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
                 CreateTrafficMirrorSessionResponse' Core.<$>
                   (x Core..@? "clientToken") Core.<*>
                     x Core..@? "trafficMirrorSession"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTrafficMirrorSessionResponse' smart constructor.
data CreateTrafficMirrorSessionResponse = CreateTrafficMirrorSessionResponse'
  { clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
  , trafficMirrorSession :: Core.Maybe Types.TrafficMirrorSession
    -- ^ Information about the Traffic Mirror session.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrafficMirrorSessionResponse' value with any optional fields omitted.
mkCreateTrafficMirrorSessionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTrafficMirrorSessionResponse
mkCreateTrafficMirrorSessionResponse responseStatus
  = CreateTrafficMirrorSessionResponse'{clientToken = Core.Nothing,
                                        trafficMirrorSession = Core.Nothing, responseStatus}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsrrsClientToken :: Lens.Lens' CreateTrafficMirrorSessionResponse (Core.Maybe Core.Text)
ctmsrrsClientToken = Lens.field @"clientToken"
{-# INLINEABLE ctmsrrsClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Information about the Traffic Mirror session.
--
-- /Note:/ Consider using 'trafficMirrorSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsrrsTrafficMirrorSession :: Lens.Lens' CreateTrafficMirrorSessionResponse (Core.Maybe Types.TrafficMirrorSession)
ctmsrrsTrafficMirrorSession = Lens.field @"trafficMirrorSession"
{-# INLINEABLE ctmsrrsTrafficMirrorSession #-}
{-# DEPRECATED trafficMirrorSession "Use generic-lens or generic-optics with 'trafficMirrorSession' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsrrsResponseStatus :: Lens.Lens' CreateTrafficMirrorSessionResponse Core.Int
ctmsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctmsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
