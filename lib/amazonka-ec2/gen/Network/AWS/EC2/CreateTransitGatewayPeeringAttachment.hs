{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a transit gateway peering attachment between the specified transit gateway (requester) and a peer transit gateway (accepter). The transit gateways must be in different Regions. The peer transit gateway can be in your account or a different AWS account.
--
-- After you create the peering attachment, the owner of the accepter transit gateway must accept the attachment request.
module Network.AWS.EC2.CreateTransitGatewayPeeringAttachment
  ( -- * Creating a request
    CreateTransitGatewayPeeringAttachment (..),
    mkCreateTransitGatewayPeeringAttachment,

    -- ** Request lenses
    ctgpaPeerAccountId,
    ctgpaTagSpecifications,
    ctgpaPeerRegion,
    ctgpaTransitGatewayId,
    ctgpaPeerTransitGatewayId,
    ctgpaDryRun,

    -- * Destructuring the response
    CreateTransitGatewayPeeringAttachmentResponse (..),
    mkCreateTransitGatewayPeeringAttachmentResponse,

    -- ** Response lenses
    ctgparsTransitGatewayPeeringAttachment,
    ctgparsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTransitGatewayPeeringAttachment' smart constructor.
data CreateTransitGatewayPeeringAttachment = CreateTransitGatewayPeeringAttachment'
  { -- | The AWS account ID of the owner of the peer transit gateway.
    peerAccountId :: Lude.Text,
    -- | The tags to apply to the transit gateway peering attachment.
    tagSpecifications :: Lude.Maybe [TagSpecification],
    -- | The Region where the peer transit gateway is located.
    peerRegion :: Lude.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Lude.Text,
    -- | The ID of the peer transit gateway with which to create the peering attachment.
    peerTransitGatewayId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTransitGatewayPeeringAttachment' with the minimum fields required to make a request.
--
-- * 'peerAccountId' - The AWS account ID of the owner of the peer transit gateway.
-- * 'tagSpecifications' - The tags to apply to the transit gateway peering attachment.
-- * 'peerRegion' - The Region where the peer transit gateway is located.
-- * 'transitGatewayId' - The ID of the transit gateway.
-- * 'peerTransitGatewayId' - The ID of the peer transit gateway with which to create the peering attachment.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCreateTransitGatewayPeeringAttachment ::
  -- | 'peerAccountId'
  Lude.Text ->
  -- | 'peerRegion'
  Lude.Text ->
  -- | 'transitGatewayId'
  Lude.Text ->
  -- | 'peerTransitGatewayId'
  Lude.Text ->
  CreateTransitGatewayPeeringAttachment
mkCreateTransitGatewayPeeringAttachment
  pPeerAccountId_
  pPeerRegion_
  pTransitGatewayId_
  pPeerTransitGatewayId_ =
    CreateTransitGatewayPeeringAttachment'
      { peerAccountId =
          pPeerAccountId_,
        tagSpecifications = Lude.Nothing,
        peerRegion = pPeerRegion_,
        transitGatewayId = pTransitGatewayId_,
        peerTransitGatewayId = pPeerTransitGatewayId_,
        dryRun = Lude.Nothing
      }

-- | The AWS account ID of the owner of the peer transit gateway.
--
-- /Note:/ Consider using 'peerAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaPeerAccountId :: Lens.Lens' CreateTransitGatewayPeeringAttachment Lude.Text
ctgpaPeerAccountId = Lens.lens (peerAccountId :: CreateTransitGatewayPeeringAttachment -> Lude.Text) (\s a -> s {peerAccountId = a} :: CreateTransitGatewayPeeringAttachment)
{-# DEPRECATED ctgpaPeerAccountId "Use generic-lens or generic-optics with 'peerAccountId' instead." #-}

-- | The tags to apply to the transit gateway peering attachment.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaTagSpecifications :: Lens.Lens' CreateTransitGatewayPeeringAttachment (Lude.Maybe [TagSpecification])
ctgpaTagSpecifications = Lens.lens (tagSpecifications :: CreateTransitGatewayPeeringAttachment -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateTransitGatewayPeeringAttachment)
{-# DEPRECATED ctgpaTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The Region where the peer transit gateway is located.
--
-- /Note:/ Consider using 'peerRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaPeerRegion :: Lens.Lens' CreateTransitGatewayPeeringAttachment Lude.Text
ctgpaPeerRegion = Lens.lens (peerRegion :: CreateTransitGatewayPeeringAttachment -> Lude.Text) (\s a -> s {peerRegion = a} :: CreateTransitGatewayPeeringAttachment)
{-# DEPRECATED ctgpaPeerRegion "Use generic-lens or generic-optics with 'peerRegion' instead." #-}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaTransitGatewayId :: Lens.Lens' CreateTransitGatewayPeeringAttachment Lude.Text
ctgpaTransitGatewayId = Lens.lens (transitGatewayId :: CreateTransitGatewayPeeringAttachment -> Lude.Text) (\s a -> s {transitGatewayId = a} :: CreateTransitGatewayPeeringAttachment)
{-# DEPRECATED ctgpaTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | The ID of the peer transit gateway with which to create the peering attachment.
--
-- /Note:/ Consider using 'peerTransitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaPeerTransitGatewayId :: Lens.Lens' CreateTransitGatewayPeeringAttachment Lude.Text
ctgpaPeerTransitGatewayId = Lens.lens (peerTransitGatewayId :: CreateTransitGatewayPeeringAttachment -> Lude.Text) (\s a -> s {peerTransitGatewayId = a} :: CreateTransitGatewayPeeringAttachment)
{-# DEPRECATED ctgpaPeerTransitGatewayId "Use generic-lens or generic-optics with 'peerTransitGatewayId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaDryRun :: Lens.Lens' CreateTransitGatewayPeeringAttachment (Lude.Maybe Lude.Bool)
ctgpaDryRun = Lens.lens (dryRun :: CreateTransitGatewayPeeringAttachment -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateTransitGatewayPeeringAttachment)
{-# DEPRECATED ctgpaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateTransitGatewayPeeringAttachment where
  type
    Rs CreateTransitGatewayPeeringAttachment =
      CreateTransitGatewayPeeringAttachmentResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateTransitGatewayPeeringAttachmentResponse'
            Lude.<$> (x Lude..@? "transitGatewayPeeringAttachment")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTransitGatewayPeeringAttachment where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateTransitGatewayPeeringAttachment where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTransitGatewayPeeringAttachment where
  toQuery CreateTransitGatewayPeeringAttachment' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateTransitGatewayPeeringAttachment" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "PeerAccountId" Lude.=: peerAccountId,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "PeerRegion" Lude.=: peerRegion,
        "TransitGatewayId" Lude.=: transitGatewayId,
        "PeerTransitGatewayId" Lude.=: peerTransitGatewayId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateTransitGatewayPeeringAttachmentResponse' smart constructor.
data CreateTransitGatewayPeeringAttachmentResponse = CreateTransitGatewayPeeringAttachmentResponse'
  { -- | The transit gateway peering attachment.
    transitGatewayPeeringAttachment :: Lude.Maybe TransitGatewayPeeringAttachment,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTransitGatewayPeeringAttachmentResponse' with the minimum fields required to make a request.
--
-- * 'transitGatewayPeeringAttachment' - The transit gateway peering attachment.
-- * 'responseStatus' - The response status code.
mkCreateTransitGatewayPeeringAttachmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTransitGatewayPeeringAttachmentResponse
mkCreateTransitGatewayPeeringAttachmentResponse pResponseStatus_ =
  CreateTransitGatewayPeeringAttachmentResponse'
    { transitGatewayPeeringAttachment =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The transit gateway peering attachment.
--
-- /Note:/ Consider using 'transitGatewayPeeringAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgparsTransitGatewayPeeringAttachment :: Lens.Lens' CreateTransitGatewayPeeringAttachmentResponse (Lude.Maybe TransitGatewayPeeringAttachment)
ctgparsTransitGatewayPeeringAttachment = Lens.lens (transitGatewayPeeringAttachment :: CreateTransitGatewayPeeringAttachmentResponse -> Lude.Maybe TransitGatewayPeeringAttachment) (\s a -> s {transitGatewayPeeringAttachment = a} :: CreateTransitGatewayPeeringAttachmentResponse)
{-# DEPRECATED ctgparsTransitGatewayPeeringAttachment "Use generic-lens or generic-optics with 'transitGatewayPeeringAttachment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgparsResponseStatus :: Lens.Lens' CreateTransitGatewayPeeringAttachmentResponse Lude.Int
ctgparsResponseStatus = Lens.lens (responseStatus :: CreateTransitGatewayPeeringAttachmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTransitGatewayPeeringAttachmentResponse)
{-# DEPRECATED ctgparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
