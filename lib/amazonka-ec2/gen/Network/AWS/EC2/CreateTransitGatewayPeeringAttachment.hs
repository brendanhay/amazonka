{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ctgpaTagSpecifications,
    ctgpaDryRun,
    ctgpaTransitGatewayId,
    ctgpaPeerTransitGatewayId,
    ctgpaPeerAccountId,
    ctgpaPeerRegion,

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
  { tagSpecifications ::
      Lude.Maybe
        [TagSpecification],
    dryRun ::
      Lude.Maybe
        Lude.Bool,
    transitGatewayId ::
      Lude.Text,
    peerTransitGatewayId ::
      Lude.Text,
    peerAccountId ::
      Lude.Text,
    peerRegion ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTransitGatewayPeeringAttachment' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'peerAccountId' - The AWS account ID of the owner of the peer transit gateway.
-- * 'peerRegion' - The Region where the peer transit gateway is located.
-- * 'peerTransitGatewayId' - The ID of the peer transit gateway with which to create the peering attachment.
-- * 'tagSpecifications' - The tags to apply to the transit gateway peering attachment.
-- * 'transitGatewayId' - The ID of the transit gateway.
mkCreateTransitGatewayPeeringAttachment ::
  -- | 'transitGatewayId'
  Lude.Text ->
  -- | 'peerTransitGatewayId'
  Lude.Text ->
  -- | 'peerAccountId'
  Lude.Text ->
  -- | 'peerRegion'
  Lude.Text ->
  CreateTransitGatewayPeeringAttachment
mkCreateTransitGatewayPeeringAttachment
  pTransitGatewayId_
  pPeerTransitGatewayId_
  pPeerAccountId_
  pPeerRegion_ =
    CreateTransitGatewayPeeringAttachment'
      { tagSpecifications =
          Lude.Nothing,
        dryRun = Lude.Nothing,
        transitGatewayId = pTransitGatewayId_,
        peerTransitGatewayId = pPeerTransitGatewayId_,
        peerAccountId = pPeerAccountId_,
        peerRegion = pPeerRegion_
      }

-- | The tags to apply to the transit gateway peering attachment.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaTagSpecifications :: Lens.Lens' CreateTransitGatewayPeeringAttachment (Lude.Maybe [TagSpecification])
ctgpaTagSpecifications = Lens.lens (tagSpecifications :: CreateTransitGatewayPeeringAttachment -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateTransitGatewayPeeringAttachment)
{-# DEPRECATED ctgpaTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaDryRun :: Lens.Lens' CreateTransitGatewayPeeringAttachment (Lude.Maybe Lude.Bool)
ctgpaDryRun = Lens.lens (dryRun :: CreateTransitGatewayPeeringAttachment -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateTransitGatewayPeeringAttachment)
{-# DEPRECATED ctgpaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

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

-- | The AWS account ID of the owner of the peer transit gateway.
--
-- /Note:/ Consider using 'peerAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaPeerAccountId :: Lens.Lens' CreateTransitGatewayPeeringAttachment Lude.Text
ctgpaPeerAccountId = Lens.lens (peerAccountId :: CreateTransitGatewayPeeringAttachment -> Lude.Text) (\s a -> s {peerAccountId = a} :: CreateTransitGatewayPeeringAttachment)
{-# DEPRECATED ctgpaPeerAccountId "Use generic-lens or generic-optics with 'peerAccountId' instead." #-}

-- | The Region where the peer transit gateway is located.
--
-- /Note:/ Consider using 'peerRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgpaPeerRegion :: Lens.Lens' CreateTransitGatewayPeeringAttachment Lude.Text
ctgpaPeerRegion = Lens.lens (peerRegion :: CreateTransitGatewayPeeringAttachment -> Lude.Text) (\s a -> s {peerRegion = a} :: CreateTransitGatewayPeeringAttachment)
{-# DEPRECATED ctgpaPeerRegion "Use generic-lens or generic-optics with 'peerRegion' instead." #-}

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
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "DryRun" Lude.=: dryRun,
        "TransitGatewayId" Lude.=: transitGatewayId,
        "PeerTransitGatewayId" Lude.=: peerTransitGatewayId,
        "PeerAccountId" Lude.=: peerAccountId,
        "PeerRegion" Lude.=: peerRegion
      ]

-- | /See:/ 'mkCreateTransitGatewayPeeringAttachmentResponse' smart constructor.
data CreateTransitGatewayPeeringAttachmentResponse = CreateTransitGatewayPeeringAttachmentResponse'
  { transitGatewayPeeringAttachment ::
      Lude.Maybe
        TransitGatewayPeeringAttachment,
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'CreateTransitGatewayPeeringAttachmentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'transitGatewayPeeringAttachment' - The transit gateway peering attachment.
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
