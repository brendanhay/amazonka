{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RejectTransitGatewayVPCAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a request to attach a VPC to a transit gateway.
--
-- The VPC attachment must be in the @pendingAcceptance@ state. Use 'DescribeTransitGatewayVpcAttachments' to view your pending VPC attachment requests. Use 'AcceptTransitGatewayVpcAttachment' to accept a VPC attachment request.
module Network.AWS.EC2.RejectTransitGatewayVPCAttachment
  ( -- * Creating a request
    RejectTransitGatewayVPCAttachment (..),
    mkRejectTransitGatewayVPCAttachment,

    -- ** Request lenses
    rtgvaTransitGatewayAttachmentId,
    rtgvaDryRun,

    -- * Destructuring the response
    RejectTransitGatewayVPCAttachmentResponse (..),
    mkRejectTransitGatewayVPCAttachmentResponse,

    -- ** Response lenses
    rtgvarsTransitGatewayVPCAttachment,
    rtgvarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRejectTransitGatewayVPCAttachment' smart constructor.
data RejectTransitGatewayVPCAttachment = RejectTransitGatewayVPCAttachment'
  { -- | The ID of the attachment.
    transitGatewayAttachmentId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RejectTransitGatewayVPCAttachment' with the minimum fields required to make a request.
--
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkRejectTransitGatewayVPCAttachment ::
  -- | 'transitGatewayAttachmentId'
  Lude.Text ->
  RejectTransitGatewayVPCAttachment
mkRejectTransitGatewayVPCAttachment pTransitGatewayAttachmentId_ =
  RejectTransitGatewayVPCAttachment'
    { transitGatewayAttachmentId =
        pTransitGatewayAttachmentId_,
      dryRun = Lude.Nothing
    }

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgvaTransitGatewayAttachmentId :: Lens.Lens' RejectTransitGatewayVPCAttachment Lude.Text
rtgvaTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: RejectTransitGatewayVPCAttachment -> Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: RejectTransitGatewayVPCAttachment)
{-# DEPRECATED rtgvaTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgvaDryRun :: Lens.Lens' RejectTransitGatewayVPCAttachment (Lude.Maybe Lude.Bool)
rtgvaDryRun = Lens.lens (dryRun :: RejectTransitGatewayVPCAttachment -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: RejectTransitGatewayVPCAttachment)
{-# DEPRECATED rtgvaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest RejectTransitGatewayVPCAttachment where
  type
    Rs RejectTransitGatewayVPCAttachment =
      RejectTransitGatewayVPCAttachmentResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          RejectTransitGatewayVPCAttachmentResponse'
            Lude.<$> (x Lude..@? "transitGatewayVpcAttachment")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RejectTransitGatewayVPCAttachment where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RejectTransitGatewayVPCAttachment where
  toPath = Lude.const "/"

instance Lude.ToQuery RejectTransitGatewayVPCAttachment where
  toQuery RejectTransitGatewayVPCAttachment' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RejectTransitGatewayVpcAttachment" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "TransitGatewayAttachmentId" Lude.=: transitGatewayAttachmentId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkRejectTransitGatewayVPCAttachmentResponse' smart constructor.
data RejectTransitGatewayVPCAttachmentResponse = RejectTransitGatewayVPCAttachmentResponse'
  { -- | Information about the attachment.
    transitGatewayVPCAttachment :: Lude.Maybe TransitGatewayVPCAttachment,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RejectTransitGatewayVPCAttachmentResponse' with the minimum fields required to make a request.
--
-- * 'transitGatewayVPCAttachment' - Information about the attachment.
-- * 'responseStatus' - The response status code.
mkRejectTransitGatewayVPCAttachmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RejectTransitGatewayVPCAttachmentResponse
mkRejectTransitGatewayVPCAttachmentResponse pResponseStatus_ =
  RejectTransitGatewayVPCAttachmentResponse'
    { transitGatewayVPCAttachment =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the attachment.
--
-- /Note:/ Consider using 'transitGatewayVPCAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgvarsTransitGatewayVPCAttachment :: Lens.Lens' RejectTransitGatewayVPCAttachmentResponse (Lude.Maybe TransitGatewayVPCAttachment)
rtgvarsTransitGatewayVPCAttachment = Lens.lens (transitGatewayVPCAttachment :: RejectTransitGatewayVPCAttachmentResponse -> Lude.Maybe TransitGatewayVPCAttachment) (\s a -> s {transitGatewayVPCAttachment = a} :: RejectTransitGatewayVPCAttachmentResponse)
{-# DEPRECATED rtgvarsTransitGatewayVPCAttachment "Use generic-lens or generic-optics with 'transitGatewayVPCAttachment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgvarsResponseStatus :: Lens.Lens' RejectTransitGatewayVPCAttachmentResponse Lude.Int
rtgvarsResponseStatus = Lens.lens (responseStatus :: RejectTransitGatewayVPCAttachmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RejectTransitGatewayVPCAttachmentResponse)
{-# DEPRECATED rtgvarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
