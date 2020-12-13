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
    mtmsRemoveFields,
    mtmsTrafficMirrorTargetId,
    mtmsTrafficMirrorFilterId,
    mtmsPacketLength,
    mtmsTrafficMirrorSessionId,
    mtmsVirtualNetworkId,
    mtmsSessionNumber,
    mtmsDescription,
    mtmsDryRun,

    -- * Destructuring the response
    ModifyTrafficMirrorSessionResponse (..),
    mkModifyTrafficMirrorSessionResponse,

    -- ** Response lenses
    mtmsrsTrafficMirrorSession,
    mtmsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyTrafficMirrorSession' smart constructor.
data ModifyTrafficMirrorSession = ModifyTrafficMirrorSession'
  { -- | The properties that you want to remove from the Traffic Mirror session.
    --
    -- When you remove a property from a Traffic Mirror session, the property is set to the default.
    removeFields :: Lude.Maybe [TrafficMirrorSessionField],
    -- | The Traffic Mirror target. The target must be in the same VPC as the source, or have a VPC peering connection with the source.
    trafficMirrorTargetId :: Lude.Maybe Lude.Text,
    -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Lude.Maybe Lude.Text,
    -- | The number of bytes in each packet to mirror. These are bytes after the VXLAN header. To mirror a subset, set this to the length (in bytes) to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target. Do not specify this parameter when you want to mirror the entire packet.
    packetLength :: Lude.Maybe Lude.Int,
    -- | The ID of the Traffic Mirror session.
    trafficMirrorSessionId :: Lude.Text,
    -- | The virtual network ID of the Traffic Mirror session.
    virtualNetworkId :: Lude.Maybe Lude.Int,
    -- | The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets.
    --
    -- Valid values are 1-32766.
    sessionNumber :: Lude.Maybe Lude.Int,
    -- | The description to assign to the Traffic Mirror session.
    description :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyTrafficMirrorSession' with the minimum fields required to make a request.
--
-- * 'removeFields' - The properties that you want to remove from the Traffic Mirror session.
--
-- When you remove a property from a Traffic Mirror session, the property is set to the default.
-- * 'trafficMirrorTargetId' - The Traffic Mirror target. The target must be in the same VPC as the source, or have a VPC peering connection with the source.
-- * 'trafficMirrorFilterId' - The ID of the Traffic Mirror filter.
-- * 'packetLength' - The number of bytes in each packet to mirror. These are bytes after the VXLAN header. To mirror a subset, set this to the length (in bytes) to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target. Do not specify this parameter when you want to mirror the entire packet.
-- * 'trafficMirrorSessionId' - The ID of the Traffic Mirror session.
-- * 'virtualNetworkId' - The virtual network ID of the Traffic Mirror session.
-- * 'sessionNumber' - The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
-- * 'description' - The description to assign to the Traffic Mirror session.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkModifyTrafficMirrorSession ::
  -- | 'trafficMirrorSessionId'
  Lude.Text ->
  ModifyTrafficMirrorSession
mkModifyTrafficMirrorSession pTrafficMirrorSessionId_ =
  ModifyTrafficMirrorSession'
    { removeFields = Lude.Nothing,
      trafficMirrorTargetId = Lude.Nothing,
      trafficMirrorFilterId = Lude.Nothing,
      packetLength = Lude.Nothing,
      trafficMirrorSessionId = pTrafficMirrorSessionId_,
      virtualNetworkId = Lude.Nothing,
      sessionNumber = Lude.Nothing,
      description = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The properties that you want to remove from the Traffic Mirror session.
--
-- When you remove a property from a Traffic Mirror session, the property is set to the default.
--
-- /Note:/ Consider using 'removeFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsRemoveFields :: Lens.Lens' ModifyTrafficMirrorSession (Lude.Maybe [TrafficMirrorSessionField])
mtmsRemoveFields = Lens.lens (removeFields :: ModifyTrafficMirrorSession -> Lude.Maybe [TrafficMirrorSessionField]) (\s a -> s {removeFields = a} :: ModifyTrafficMirrorSession)
{-# DEPRECATED mtmsRemoveFields "Use generic-lens or generic-optics with 'removeFields' instead." #-}

-- | The Traffic Mirror target. The target must be in the same VPC as the source, or have a VPC peering connection with the source.
--
-- /Note:/ Consider using 'trafficMirrorTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsTrafficMirrorTargetId :: Lens.Lens' ModifyTrafficMirrorSession (Lude.Maybe Lude.Text)
mtmsTrafficMirrorTargetId = Lens.lens (trafficMirrorTargetId :: ModifyTrafficMirrorSession -> Lude.Maybe Lude.Text) (\s a -> s {trafficMirrorTargetId = a} :: ModifyTrafficMirrorSession)
{-# DEPRECATED mtmsTrafficMirrorTargetId "Use generic-lens or generic-optics with 'trafficMirrorTargetId' instead." #-}

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsTrafficMirrorFilterId :: Lens.Lens' ModifyTrafficMirrorSession (Lude.Maybe Lude.Text)
mtmsTrafficMirrorFilterId = Lens.lens (trafficMirrorFilterId :: ModifyTrafficMirrorSession -> Lude.Maybe Lude.Text) (\s a -> s {trafficMirrorFilterId = a} :: ModifyTrafficMirrorSession)
{-# DEPRECATED mtmsTrafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead." #-}

-- | The number of bytes in each packet to mirror. These are bytes after the VXLAN header. To mirror a subset, set this to the length (in bytes) to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target. Do not specify this parameter when you want to mirror the entire packet.
--
-- /Note:/ Consider using 'packetLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsPacketLength :: Lens.Lens' ModifyTrafficMirrorSession (Lude.Maybe Lude.Int)
mtmsPacketLength = Lens.lens (packetLength :: ModifyTrafficMirrorSession -> Lude.Maybe Lude.Int) (\s a -> s {packetLength = a} :: ModifyTrafficMirrorSession)
{-# DEPRECATED mtmsPacketLength "Use generic-lens or generic-optics with 'packetLength' instead." #-}

-- | The ID of the Traffic Mirror session.
--
-- /Note:/ Consider using 'trafficMirrorSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsTrafficMirrorSessionId :: Lens.Lens' ModifyTrafficMirrorSession Lude.Text
mtmsTrafficMirrorSessionId = Lens.lens (trafficMirrorSessionId :: ModifyTrafficMirrorSession -> Lude.Text) (\s a -> s {trafficMirrorSessionId = a} :: ModifyTrafficMirrorSession)
{-# DEPRECATED mtmsTrafficMirrorSessionId "Use generic-lens or generic-optics with 'trafficMirrorSessionId' instead." #-}

-- | The virtual network ID of the Traffic Mirror session.
--
-- /Note:/ Consider using 'virtualNetworkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsVirtualNetworkId :: Lens.Lens' ModifyTrafficMirrorSession (Lude.Maybe Lude.Int)
mtmsVirtualNetworkId = Lens.lens (virtualNetworkId :: ModifyTrafficMirrorSession -> Lude.Maybe Lude.Int) (\s a -> s {virtualNetworkId = a} :: ModifyTrafficMirrorSession)
{-# DEPRECATED mtmsVirtualNetworkId "Use generic-lens or generic-optics with 'virtualNetworkId' instead." #-}

-- | The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
--
-- /Note:/ Consider using 'sessionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsSessionNumber :: Lens.Lens' ModifyTrafficMirrorSession (Lude.Maybe Lude.Int)
mtmsSessionNumber = Lens.lens (sessionNumber :: ModifyTrafficMirrorSession -> Lude.Maybe Lude.Int) (\s a -> s {sessionNumber = a} :: ModifyTrafficMirrorSession)
{-# DEPRECATED mtmsSessionNumber "Use generic-lens or generic-optics with 'sessionNumber' instead." #-}

-- | The description to assign to the Traffic Mirror session.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsDescription :: Lens.Lens' ModifyTrafficMirrorSession (Lude.Maybe Lude.Text)
mtmsDescription = Lens.lens (description :: ModifyTrafficMirrorSession -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ModifyTrafficMirrorSession)
{-# DEPRECATED mtmsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsDryRun :: Lens.Lens' ModifyTrafficMirrorSession (Lude.Maybe Lude.Bool)
mtmsDryRun = Lens.lens (dryRun :: ModifyTrafficMirrorSession -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyTrafficMirrorSession)
{-# DEPRECATED mtmsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ModifyTrafficMirrorSession where
  type
    Rs ModifyTrafficMirrorSession =
      ModifyTrafficMirrorSessionResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyTrafficMirrorSessionResponse'
            Lude.<$> (x Lude..@? "trafficMirrorSession")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyTrafficMirrorSession where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyTrafficMirrorSession where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyTrafficMirrorSession where
  toQuery ModifyTrafficMirrorSession' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyTrafficMirrorSession" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "RemoveField" Lude.<$> removeFields),
        "TrafficMirrorTargetId" Lude.=: trafficMirrorTargetId,
        "TrafficMirrorFilterId" Lude.=: trafficMirrorFilterId,
        "PacketLength" Lude.=: packetLength,
        "TrafficMirrorSessionId" Lude.=: trafficMirrorSessionId,
        "VirtualNetworkId" Lude.=: virtualNetworkId,
        "SessionNumber" Lude.=: sessionNumber,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkModifyTrafficMirrorSessionResponse' smart constructor.
data ModifyTrafficMirrorSessionResponse = ModifyTrafficMirrorSessionResponse'
  { -- | Information about the Traffic Mirror session.
    trafficMirrorSession :: Lude.Maybe TrafficMirrorSession,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyTrafficMirrorSessionResponse' with the minimum fields required to make a request.
--
-- * 'trafficMirrorSession' - Information about the Traffic Mirror session.
-- * 'responseStatus' - The response status code.
mkModifyTrafficMirrorSessionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyTrafficMirrorSessionResponse
mkModifyTrafficMirrorSessionResponse pResponseStatus_ =
  ModifyTrafficMirrorSessionResponse'
    { trafficMirrorSession =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the Traffic Mirror session.
--
-- /Note:/ Consider using 'trafficMirrorSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsrsTrafficMirrorSession :: Lens.Lens' ModifyTrafficMirrorSessionResponse (Lude.Maybe TrafficMirrorSession)
mtmsrsTrafficMirrorSession = Lens.lens (trafficMirrorSession :: ModifyTrafficMirrorSessionResponse -> Lude.Maybe TrafficMirrorSession) (\s a -> s {trafficMirrorSession = a} :: ModifyTrafficMirrorSessionResponse)
{-# DEPRECATED mtmsrsTrafficMirrorSession "Use generic-lens or generic-optics with 'trafficMirrorSession' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtmsrsResponseStatus :: Lens.Lens' ModifyTrafficMirrorSessionResponse Lude.Int
mtmsrsResponseStatus = Lens.lens (responseStatus :: ModifyTrafficMirrorSessionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyTrafficMirrorSessionResponse)
{-# DEPRECATED mtmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
