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
    tmsTrafficMirrorTargetId,
    tmsNetworkInterfaceId,
    tmsTrafficMirrorFilterId,
    tmsPacketLength,
    tmsOwnerId,
    tmsTrafficMirrorSessionId,
    tmsVirtualNetworkId,
    tmsSessionNumber,
    tmsDescription,
    tmsTags,
  )
where

import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Traffic Mirror session.
--
-- /See:/ 'mkTrafficMirrorSession' smart constructor.
data TrafficMirrorSession = TrafficMirrorSession'
  { trafficMirrorTargetId ::
      Lude.Maybe Lude.Text,
    networkInterfaceId :: Lude.Maybe Lude.Text,
    trafficMirrorFilterId :: Lude.Maybe Lude.Text,
    packetLength :: Lude.Maybe Lude.Int,
    ownerId :: Lude.Maybe Lude.Text,
    trafficMirrorSessionId :: Lude.Maybe Lude.Text,
    virtualNetworkId :: Lude.Maybe Lude.Int,
    sessionNumber :: Lude.Maybe Lude.Int,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrafficMirrorSession' with the minimum fields required to make a request.
--
-- * 'description' - The description of the Traffic Mirror session.
-- * 'networkInterfaceId' - The ID of the Traffic Mirror session's network interface.
-- * 'ownerId' - The ID of the account that owns the Traffic Mirror session.
-- * 'packetLength' - The number of bytes in each packet to mirror. These are the bytes after the VXLAN header. To mirror a subset, set this to the length (in bytes) to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target. Do not specify this parameter when you want to mirror the entire packet
-- * 'sessionNumber' - The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
-- * 'tags' - The tags assigned to the Traffic Mirror session.
-- * 'trafficMirrorFilterId' - The ID of the Traffic Mirror filter.
-- * 'trafficMirrorSessionId' - The ID for the Traffic Mirror session.
-- * 'trafficMirrorTargetId' - The ID of the Traffic Mirror target.
-- * 'virtualNetworkId' - The virtual network ID associated with the Traffic Mirror session.
mkTrafficMirrorSession ::
  TrafficMirrorSession
mkTrafficMirrorSession =
  TrafficMirrorSession'
    { trafficMirrorTargetId = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      trafficMirrorFilterId = Lude.Nothing,
      packetLength = Lude.Nothing,
      ownerId = Lude.Nothing,
      trafficMirrorSessionId = Lude.Nothing,
      virtualNetworkId = Lude.Nothing,
      sessionNumber = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The ID of the Traffic Mirror target.
--
-- /Note:/ Consider using 'trafficMirrorTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmsTrafficMirrorTargetId :: Lens.Lens' TrafficMirrorSession (Lude.Maybe Lude.Text)
tmsTrafficMirrorTargetId = Lens.lens (trafficMirrorTargetId :: TrafficMirrorSession -> Lude.Maybe Lude.Text) (\s a -> s {trafficMirrorTargetId = a} :: TrafficMirrorSession)
{-# DEPRECATED tmsTrafficMirrorTargetId "Use generic-lens or generic-optics with 'trafficMirrorTargetId' instead." #-}

-- | The ID of the Traffic Mirror session's network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmsNetworkInterfaceId :: Lens.Lens' TrafficMirrorSession (Lude.Maybe Lude.Text)
tmsNetworkInterfaceId = Lens.lens (networkInterfaceId :: TrafficMirrorSession -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: TrafficMirrorSession)
{-# DEPRECATED tmsNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmsTrafficMirrorFilterId :: Lens.Lens' TrafficMirrorSession (Lude.Maybe Lude.Text)
tmsTrafficMirrorFilterId = Lens.lens (trafficMirrorFilterId :: TrafficMirrorSession -> Lude.Maybe Lude.Text) (\s a -> s {trafficMirrorFilterId = a} :: TrafficMirrorSession)
{-# DEPRECATED tmsTrafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead." #-}

-- | The number of bytes in each packet to mirror. These are the bytes after the VXLAN header. To mirror a subset, set this to the length (in bytes) to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target. Do not specify this parameter when you want to mirror the entire packet
--
-- /Note:/ Consider using 'packetLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmsPacketLength :: Lens.Lens' TrafficMirrorSession (Lude.Maybe Lude.Int)
tmsPacketLength = Lens.lens (packetLength :: TrafficMirrorSession -> Lude.Maybe Lude.Int) (\s a -> s {packetLength = a} :: TrafficMirrorSession)
{-# DEPRECATED tmsPacketLength "Use generic-lens or generic-optics with 'packetLength' instead." #-}

-- | The ID of the account that owns the Traffic Mirror session.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmsOwnerId :: Lens.Lens' TrafficMirrorSession (Lude.Maybe Lude.Text)
tmsOwnerId = Lens.lens (ownerId :: TrafficMirrorSession -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: TrafficMirrorSession)
{-# DEPRECATED tmsOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The ID for the Traffic Mirror session.
--
-- /Note:/ Consider using 'trafficMirrorSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmsTrafficMirrorSessionId :: Lens.Lens' TrafficMirrorSession (Lude.Maybe Lude.Text)
tmsTrafficMirrorSessionId = Lens.lens (trafficMirrorSessionId :: TrafficMirrorSession -> Lude.Maybe Lude.Text) (\s a -> s {trafficMirrorSessionId = a} :: TrafficMirrorSession)
{-# DEPRECATED tmsTrafficMirrorSessionId "Use generic-lens or generic-optics with 'trafficMirrorSessionId' instead." #-}

-- | The virtual network ID associated with the Traffic Mirror session.
--
-- /Note:/ Consider using 'virtualNetworkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmsVirtualNetworkId :: Lens.Lens' TrafficMirrorSession (Lude.Maybe Lude.Int)
tmsVirtualNetworkId = Lens.lens (virtualNetworkId :: TrafficMirrorSession -> Lude.Maybe Lude.Int) (\s a -> s {virtualNetworkId = a} :: TrafficMirrorSession)
{-# DEPRECATED tmsVirtualNetworkId "Use generic-lens or generic-optics with 'virtualNetworkId' instead." #-}

-- | The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
--
-- /Note:/ Consider using 'sessionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmsSessionNumber :: Lens.Lens' TrafficMirrorSession (Lude.Maybe Lude.Int)
tmsSessionNumber = Lens.lens (sessionNumber :: TrafficMirrorSession -> Lude.Maybe Lude.Int) (\s a -> s {sessionNumber = a} :: TrafficMirrorSession)
{-# DEPRECATED tmsSessionNumber "Use generic-lens or generic-optics with 'sessionNumber' instead." #-}

-- | The description of the Traffic Mirror session.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmsDescription :: Lens.Lens' TrafficMirrorSession (Lude.Maybe Lude.Text)
tmsDescription = Lens.lens (description :: TrafficMirrorSession -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: TrafficMirrorSession)
{-# DEPRECATED tmsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags assigned to the Traffic Mirror session.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmsTags :: Lens.Lens' TrafficMirrorSession (Lude.Maybe [Tag])
tmsTags = Lens.lens (tags :: TrafficMirrorSession -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: TrafficMirrorSession)
{-# DEPRECATED tmsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML TrafficMirrorSession where
  parseXML x =
    TrafficMirrorSession'
      Lude.<$> (x Lude..@? "trafficMirrorTargetId")
      Lude.<*> (x Lude..@? "networkInterfaceId")
      Lude.<*> (x Lude..@? "trafficMirrorFilterId")
      Lude.<*> (x Lude..@? "packetLength")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "trafficMirrorSessionId")
      Lude.<*> (x Lude..@? "virtualNetworkId")
      Lude.<*> (x Lude..@? "sessionNumber")
      Lude.<*> (x Lude..@? "description")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
