{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorSession
  ( TrafficMirrorSession (..),

    -- * Smart constructor
    mkTrafficMirrorSession,

    -- * Lenses
    tmsDescription,
    tmsNetworkInterfaceId,
    tmsOwnerId,
    tmsPacketLength,
    tmsSessionNumber,
    tmsTags,
    tmsTrafficMirrorFilterId,
    tmsTrafficMirrorSessionId,
    tmsTrafficMirrorTargetId,
    tmsVirtualNetworkId,
  )
where

import qualified Network.AWS.EC2.Types.Description as Types
import qualified Network.AWS.EC2.Types.NetworkInterfaceId as Types
import qualified Network.AWS.EC2.Types.OwnerId as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.TrafficMirrorFilterId as Types
import qualified Network.AWS.EC2.Types.TrafficMirrorSessionId as Types
import qualified Network.AWS.EC2.Types.TrafficMirrorTargetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Traffic Mirror session.
--
-- /See:/ 'mkTrafficMirrorSession' smart constructor.
data TrafficMirrorSession = TrafficMirrorSession'
  { -- | The description of the Traffic Mirror session.
    description :: Core.Maybe Types.Description,
    -- | The ID of the Traffic Mirror session's network interface.
    networkInterfaceId :: Core.Maybe Types.NetworkInterfaceId,
    -- | The ID of the account that owns the Traffic Mirror session.
    ownerId :: Core.Maybe Types.OwnerId,
    -- | The number of bytes in each packet to mirror. These are the bytes after the VXLAN header. To mirror a subset, set this to the length (in bytes) to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target. Do not specify this parameter when you want to mirror the entire packet
    packetLength :: Core.Maybe Core.Int,
    -- | The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets.
    --
    -- Valid values are 1-32766.
    sessionNumber :: Core.Maybe Core.Int,
    -- | The tags assigned to the Traffic Mirror session.
    tags :: Core.Maybe [Types.Tag],
    -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Core.Maybe Types.TrafficMirrorFilterId,
    -- | The ID for the Traffic Mirror session.
    trafficMirrorSessionId :: Core.Maybe Types.TrafficMirrorSessionId,
    -- | The ID of the Traffic Mirror target.
    trafficMirrorTargetId :: Core.Maybe Types.TrafficMirrorTargetId,
    -- | The virtual network ID associated with the Traffic Mirror session.
    virtualNetworkId :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrafficMirrorSession' value with any optional fields omitted.
mkTrafficMirrorSession ::
  TrafficMirrorSession
mkTrafficMirrorSession =
  TrafficMirrorSession'
    { description = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      ownerId = Core.Nothing,
      packetLength = Core.Nothing,
      sessionNumber = Core.Nothing,
      tags = Core.Nothing,
      trafficMirrorFilterId = Core.Nothing,
      trafficMirrorSessionId = Core.Nothing,
      trafficMirrorTargetId = Core.Nothing,
      virtualNetworkId = Core.Nothing
    }

-- | The description of the Traffic Mirror session.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmsDescription :: Lens.Lens' TrafficMirrorSession (Core.Maybe Types.Description)
tmsDescription = Lens.field @"description"
{-# DEPRECATED tmsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the Traffic Mirror session's network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmsNetworkInterfaceId :: Lens.Lens' TrafficMirrorSession (Core.Maybe Types.NetworkInterfaceId)
tmsNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED tmsNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The ID of the account that owns the Traffic Mirror session.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmsOwnerId :: Lens.Lens' TrafficMirrorSession (Core.Maybe Types.OwnerId)
tmsOwnerId = Lens.field @"ownerId"
{-# DEPRECATED tmsOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The number of bytes in each packet to mirror. These are the bytes after the VXLAN header. To mirror a subset, set this to the length (in bytes) to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target. Do not specify this parameter when you want to mirror the entire packet
--
-- /Note:/ Consider using 'packetLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmsPacketLength :: Lens.Lens' TrafficMirrorSession (Core.Maybe Core.Int)
tmsPacketLength = Lens.field @"packetLength"
{-# DEPRECATED tmsPacketLength "Use generic-lens or generic-optics with 'packetLength' instead." #-}

-- | The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
--
-- /Note:/ Consider using 'sessionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmsSessionNumber :: Lens.Lens' TrafficMirrorSession (Core.Maybe Core.Int)
tmsSessionNumber = Lens.field @"sessionNumber"
{-# DEPRECATED tmsSessionNumber "Use generic-lens or generic-optics with 'sessionNumber' instead." #-}

-- | The tags assigned to the Traffic Mirror session.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmsTags :: Lens.Lens' TrafficMirrorSession (Core.Maybe [Types.Tag])
tmsTags = Lens.field @"tags"
{-# DEPRECATED tmsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmsTrafficMirrorFilterId :: Lens.Lens' TrafficMirrorSession (Core.Maybe Types.TrafficMirrorFilterId)
tmsTrafficMirrorFilterId = Lens.field @"trafficMirrorFilterId"
{-# DEPRECATED tmsTrafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead." #-}

-- | The ID for the Traffic Mirror session.
--
-- /Note:/ Consider using 'trafficMirrorSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmsTrafficMirrorSessionId :: Lens.Lens' TrafficMirrorSession (Core.Maybe Types.TrafficMirrorSessionId)
tmsTrafficMirrorSessionId = Lens.field @"trafficMirrorSessionId"
{-# DEPRECATED tmsTrafficMirrorSessionId "Use generic-lens or generic-optics with 'trafficMirrorSessionId' instead." #-}

-- | The ID of the Traffic Mirror target.
--
-- /Note:/ Consider using 'trafficMirrorTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmsTrafficMirrorTargetId :: Lens.Lens' TrafficMirrorSession (Core.Maybe Types.TrafficMirrorTargetId)
tmsTrafficMirrorTargetId = Lens.field @"trafficMirrorTargetId"
{-# DEPRECATED tmsTrafficMirrorTargetId "Use generic-lens or generic-optics with 'trafficMirrorTargetId' instead." #-}

-- | The virtual network ID associated with the Traffic Mirror session.
--
-- /Note:/ Consider using 'virtualNetworkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmsVirtualNetworkId :: Lens.Lens' TrafficMirrorSession (Core.Maybe Core.Int)
tmsVirtualNetworkId = Lens.field @"virtualNetworkId"
{-# DEPRECATED tmsVirtualNetworkId "Use generic-lens or generic-optics with 'virtualNetworkId' instead." #-}

instance Core.FromXML TrafficMirrorSession where
  parseXML x =
    TrafficMirrorSession'
      Core.<$> (x Core..@? "description")
      Core.<*> (x Core..@? "networkInterfaceId")
      Core.<*> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "packetLength")
      Core.<*> (x Core..@? "sessionNumber")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "trafficMirrorFilterId")
      Core.<*> (x Core..@? "trafficMirrorSessionId")
      Core.<*> (x Core..@? "trafficMirrorTargetId")
      Core.<*> (x Core..@? "virtualNetworkId")
