{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateTrafficMirrorSession (..),
    mkCreateTrafficMirrorSession,

    -- ** Request lenses
    ctmsClientToken,
    ctmsPacketLength,
    ctmsTagSpecifications,
    ctmsVirtualNetworkId,
    ctmsDescription,
    ctmsDryRun,
    ctmsNetworkInterfaceId,
    ctmsTrafficMirrorTargetId,
    ctmsTrafficMirrorFilterId,
    ctmsSessionNumber,

    -- * Destructuring the response
    CreateTrafficMirrorSessionResponse (..),
    mkCreateTrafficMirrorSessionResponse,

    -- ** Response lenses
    ctmsrsTrafficMirrorSession,
    ctmsrsClientToken,
    ctmsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTrafficMirrorSession' smart constructor.
data CreateTrafficMirrorSession = CreateTrafficMirrorSession'
  { clientToken ::
      Lude.Maybe Lude.Text,
    packetLength :: Lude.Maybe Lude.Int,
    tagSpecifications ::
      Lude.Maybe [TagSpecification],
    virtualNetworkId ::
      Lude.Maybe Lude.Int,
    description :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    networkInterfaceId :: Lude.Text,
    trafficMirrorTargetId :: Lude.Text,
    trafficMirrorFilterId :: Lude.Text,
    sessionNumber :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTrafficMirrorSession' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'description' - The description of the Traffic Mirror session.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'networkInterfaceId' - The ID of the source network interface.
-- * 'packetLength' - The number of bytes in each packet to mirror. These are bytes after the VXLAN header. Do not specify this parameter when you want to mirror the entire packet. To mirror a subset of the packet, set this to the length (in bytes) that you want to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target.
--
-- If you do not want to mirror the entire packet, use the @PacketLength@ parameter to specify the number of bytes in each packet to mirror.
-- * 'sessionNumber' - The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
-- * 'tagSpecifications' - The tags to assign to a Traffic Mirror session.
-- * 'trafficMirrorFilterId' - The ID of the Traffic Mirror filter.
-- * 'trafficMirrorTargetId' - The ID of the Traffic Mirror target.
-- * 'virtualNetworkId' - The VXLAN ID for the Traffic Mirror session. For more information about the VXLAN protocol, see <https://tools.ietf.org/html/rfc7348 RFC 7348> . If you do not specify a @VirtualNetworkId@ , an account-wide unique id is chosen at random.
mkCreateTrafficMirrorSession ::
  -- | 'networkInterfaceId'
  Lude.Text ->
  -- | 'trafficMirrorTargetId'
  Lude.Text ->
  -- | 'trafficMirrorFilterId'
  Lude.Text ->
  -- | 'sessionNumber'
  Lude.Int ->
  CreateTrafficMirrorSession
mkCreateTrafficMirrorSession
  pNetworkInterfaceId_
  pTrafficMirrorTargetId_
  pTrafficMirrorFilterId_
  pSessionNumber_ =
    CreateTrafficMirrorSession'
      { clientToken = Lude.Nothing,
        packetLength = Lude.Nothing,
        tagSpecifications = Lude.Nothing,
        virtualNetworkId = Lude.Nothing,
        description = Lude.Nothing,
        dryRun = Lude.Nothing,
        networkInterfaceId = pNetworkInterfaceId_,
        trafficMirrorTargetId = pTrafficMirrorTargetId_,
        trafficMirrorFilterId = pTrafficMirrorFilterId_,
        sessionNumber = pSessionNumber_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsClientToken :: Lens.Lens' CreateTrafficMirrorSession (Lude.Maybe Lude.Text)
ctmsClientToken = Lens.lens (clientToken :: CreateTrafficMirrorSession -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateTrafficMirrorSession)
{-# DEPRECATED ctmsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The number of bytes in each packet to mirror. These are bytes after the VXLAN header. Do not specify this parameter when you want to mirror the entire packet. To mirror a subset of the packet, set this to the length (in bytes) that you want to mirror. For example, if you set this value to 100, then the first 100 bytes that meet the filter criteria are copied to the target.
--
-- If you do not want to mirror the entire packet, use the @PacketLength@ parameter to specify the number of bytes in each packet to mirror.
--
-- /Note:/ Consider using 'packetLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsPacketLength :: Lens.Lens' CreateTrafficMirrorSession (Lude.Maybe Lude.Int)
ctmsPacketLength = Lens.lens (packetLength :: CreateTrafficMirrorSession -> Lude.Maybe Lude.Int) (\s a -> s {packetLength = a} :: CreateTrafficMirrorSession)
{-# DEPRECATED ctmsPacketLength "Use generic-lens or generic-optics with 'packetLength' instead." #-}

-- | The tags to assign to a Traffic Mirror session.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsTagSpecifications :: Lens.Lens' CreateTrafficMirrorSession (Lude.Maybe [TagSpecification])
ctmsTagSpecifications = Lens.lens (tagSpecifications :: CreateTrafficMirrorSession -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateTrafficMirrorSession)
{-# DEPRECATED ctmsTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The VXLAN ID for the Traffic Mirror session. For more information about the VXLAN protocol, see <https://tools.ietf.org/html/rfc7348 RFC 7348> . If you do not specify a @VirtualNetworkId@ , an account-wide unique id is chosen at random.
--
-- /Note:/ Consider using 'virtualNetworkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsVirtualNetworkId :: Lens.Lens' CreateTrafficMirrorSession (Lude.Maybe Lude.Int)
ctmsVirtualNetworkId = Lens.lens (virtualNetworkId :: CreateTrafficMirrorSession -> Lude.Maybe Lude.Int) (\s a -> s {virtualNetworkId = a} :: CreateTrafficMirrorSession)
{-# DEPRECATED ctmsVirtualNetworkId "Use generic-lens or generic-optics with 'virtualNetworkId' instead." #-}

-- | The description of the Traffic Mirror session.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsDescription :: Lens.Lens' CreateTrafficMirrorSession (Lude.Maybe Lude.Text)
ctmsDescription = Lens.lens (description :: CreateTrafficMirrorSession -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateTrafficMirrorSession)
{-# DEPRECATED ctmsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsDryRun :: Lens.Lens' CreateTrafficMirrorSession (Lude.Maybe Lude.Bool)
ctmsDryRun = Lens.lens (dryRun :: CreateTrafficMirrorSession -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateTrafficMirrorSession)
{-# DEPRECATED ctmsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the source network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsNetworkInterfaceId :: Lens.Lens' CreateTrafficMirrorSession Lude.Text
ctmsNetworkInterfaceId = Lens.lens (networkInterfaceId :: CreateTrafficMirrorSession -> Lude.Text) (\s a -> s {networkInterfaceId = a} :: CreateTrafficMirrorSession)
{-# DEPRECATED ctmsNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The ID of the Traffic Mirror target.
--
-- /Note:/ Consider using 'trafficMirrorTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsTrafficMirrorTargetId :: Lens.Lens' CreateTrafficMirrorSession Lude.Text
ctmsTrafficMirrorTargetId = Lens.lens (trafficMirrorTargetId :: CreateTrafficMirrorSession -> Lude.Text) (\s a -> s {trafficMirrorTargetId = a} :: CreateTrafficMirrorSession)
{-# DEPRECATED ctmsTrafficMirrorTargetId "Use generic-lens or generic-optics with 'trafficMirrorTargetId' instead." #-}

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsTrafficMirrorFilterId :: Lens.Lens' CreateTrafficMirrorSession Lude.Text
ctmsTrafficMirrorFilterId = Lens.lens (trafficMirrorFilterId :: CreateTrafficMirrorSession -> Lude.Text) (\s a -> s {trafficMirrorFilterId = a} :: CreateTrafficMirrorSession)
{-# DEPRECATED ctmsTrafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead." #-}

-- | The session number determines the order in which sessions are evaluated when an interface is used by multiple sessions. The first session with a matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
--
-- /Note:/ Consider using 'sessionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsSessionNumber :: Lens.Lens' CreateTrafficMirrorSession Lude.Int
ctmsSessionNumber = Lens.lens (sessionNumber :: CreateTrafficMirrorSession -> Lude.Int) (\s a -> s {sessionNumber = a} :: CreateTrafficMirrorSession)
{-# DEPRECATED ctmsSessionNumber "Use generic-lens or generic-optics with 'sessionNumber' instead." #-}

instance Lude.AWSRequest CreateTrafficMirrorSession where
  type
    Rs CreateTrafficMirrorSession =
      CreateTrafficMirrorSessionResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateTrafficMirrorSessionResponse'
            Lude.<$> (x Lude..@? "trafficMirrorSession")
            Lude.<*> (x Lude..@? "clientToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTrafficMirrorSession where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateTrafficMirrorSession where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTrafficMirrorSession where
  toQuery CreateTrafficMirrorSession' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateTrafficMirrorSession" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        "PacketLength" Lude.=: packetLength,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "VirtualNetworkId" Lude.=: virtualNetworkId,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun,
        "NetworkInterfaceId" Lude.=: networkInterfaceId,
        "TrafficMirrorTargetId" Lude.=: trafficMirrorTargetId,
        "TrafficMirrorFilterId" Lude.=: trafficMirrorFilterId,
        "SessionNumber" Lude.=: sessionNumber
      ]

-- | /See:/ 'mkCreateTrafficMirrorSessionResponse' smart constructor.
data CreateTrafficMirrorSessionResponse = CreateTrafficMirrorSessionResponse'
  { trafficMirrorSession ::
      Lude.Maybe
        TrafficMirrorSession,
    clientToken ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTrafficMirrorSessionResponse' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'responseStatus' - The response status code.
-- * 'trafficMirrorSession' - Information about the Traffic Mirror session.
mkCreateTrafficMirrorSessionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTrafficMirrorSessionResponse
mkCreateTrafficMirrorSessionResponse pResponseStatus_ =
  CreateTrafficMirrorSessionResponse'
    { trafficMirrorSession =
        Lude.Nothing,
      clientToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the Traffic Mirror session.
--
-- /Note:/ Consider using 'trafficMirrorSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsrsTrafficMirrorSession :: Lens.Lens' CreateTrafficMirrorSessionResponse (Lude.Maybe TrafficMirrorSession)
ctmsrsTrafficMirrorSession = Lens.lens (trafficMirrorSession :: CreateTrafficMirrorSessionResponse -> Lude.Maybe TrafficMirrorSession) (\s a -> s {trafficMirrorSession = a} :: CreateTrafficMirrorSessionResponse)
{-# DEPRECATED ctmsrsTrafficMirrorSession "Use generic-lens or generic-optics with 'trafficMirrorSession' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsrsClientToken :: Lens.Lens' CreateTrafficMirrorSessionResponse (Lude.Maybe Lude.Text)
ctmsrsClientToken = Lens.lens (clientToken :: CreateTrafficMirrorSessionResponse -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateTrafficMirrorSessionResponse)
{-# DEPRECATED ctmsrsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmsrsResponseStatus :: Lens.Lens' CreateTrafficMirrorSessionResponse Lude.Int
ctmsrsResponseStatus = Lens.lens (responseStatus :: CreateTrafficMirrorSessionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTrafficMirrorSessionResponse)
{-# DEPRECATED ctmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
