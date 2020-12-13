{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AcceptTransitGatewayVPCAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a request to attach a VPC to a transit gateway.
--
-- The VPC attachment must be in the @pendingAcceptance@ state. Use 'DescribeTransitGatewayVpcAttachments' to view your pending VPC attachment requests. Use 'RejectTransitGatewayVpcAttachment' to reject a VPC attachment request.
module Network.AWS.EC2.AcceptTransitGatewayVPCAttachment
  ( -- * Creating a request
    AcceptTransitGatewayVPCAttachment (..),
    mkAcceptTransitGatewayVPCAttachment,

    -- ** Request lenses
    atgvaTransitGatewayAttachmentId,
    atgvaDryRun,

    -- * Destructuring the response
    AcceptTransitGatewayVPCAttachmentResponse (..),
    mkAcceptTransitGatewayVPCAttachmentResponse,

    -- ** Response lenses
    atgvarsTransitGatewayVPCAttachment,
    atgvarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAcceptTransitGatewayVPCAttachment' smart constructor.
data AcceptTransitGatewayVPCAttachment = AcceptTransitGatewayVPCAttachment'
  { -- | The ID of the attachment.
    transitGatewayAttachmentId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptTransitGatewayVPCAttachment' with the minimum fields required to make a request.
--
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkAcceptTransitGatewayVPCAttachment ::
  -- | 'transitGatewayAttachmentId'
  Lude.Text ->
  AcceptTransitGatewayVPCAttachment
mkAcceptTransitGatewayVPCAttachment pTransitGatewayAttachmentId_ =
  AcceptTransitGatewayVPCAttachment'
    { transitGatewayAttachmentId =
        pTransitGatewayAttachmentId_,
      dryRun = Lude.Nothing
    }

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgvaTransitGatewayAttachmentId :: Lens.Lens' AcceptTransitGatewayVPCAttachment Lude.Text
atgvaTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: AcceptTransitGatewayVPCAttachment -> Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: AcceptTransitGatewayVPCAttachment)
{-# DEPRECATED atgvaTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgvaDryRun :: Lens.Lens' AcceptTransitGatewayVPCAttachment (Lude.Maybe Lude.Bool)
atgvaDryRun = Lens.lens (dryRun :: AcceptTransitGatewayVPCAttachment -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AcceptTransitGatewayVPCAttachment)
{-# DEPRECATED atgvaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest AcceptTransitGatewayVPCAttachment where
  type
    Rs AcceptTransitGatewayVPCAttachment =
      AcceptTransitGatewayVPCAttachmentResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AcceptTransitGatewayVPCAttachmentResponse'
            Lude.<$> (x Lude..@? "transitGatewayVpcAttachment")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AcceptTransitGatewayVPCAttachment where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AcceptTransitGatewayVPCAttachment where
  toPath = Lude.const "/"

instance Lude.ToQuery AcceptTransitGatewayVPCAttachment where
  toQuery AcceptTransitGatewayVPCAttachment' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AcceptTransitGatewayVpcAttachment" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "TransitGatewayAttachmentId" Lude.=: transitGatewayAttachmentId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkAcceptTransitGatewayVPCAttachmentResponse' smart constructor.
data AcceptTransitGatewayVPCAttachmentResponse = AcceptTransitGatewayVPCAttachmentResponse'
  { -- | The VPC attachment.
    transitGatewayVPCAttachment :: Lude.Maybe TransitGatewayVPCAttachment,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptTransitGatewayVPCAttachmentResponse' with the minimum fields required to make a request.
--
-- * 'transitGatewayVPCAttachment' - The VPC attachment.
-- * 'responseStatus' - The response status code.
mkAcceptTransitGatewayVPCAttachmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AcceptTransitGatewayVPCAttachmentResponse
mkAcceptTransitGatewayVPCAttachmentResponse pResponseStatus_ =
  AcceptTransitGatewayVPCAttachmentResponse'
    { transitGatewayVPCAttachment =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The VPC attachment.
--
-- /Note:/ Consider using 'transitGatewayVPCAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgvarsTransitGatewayVPCAttachment :: Lens.Lens' AcceptTransitGatewayVPCAttachmentResponse (Lude.Maybe TransitGatewayVPCAttachment)
atgvarsTransitGatewayVPCAttachment = Lens.lens (transitGatewayVPCAttachment :: AcceptTransitGatewayVPCAttachmentResponse -> Lude.Maybe TransitGatewayVPCAttachment) (\s a -> s {transitGatewayVPCAttachment = a} :: AcceptTransitGatewayVPCAttachmentResponse)
{-# DEPRECATED atgvarsTransitGatewayVPCAttachment "Use generic-lens or generic-optics with 'transitGatewayVPCAttachment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgvarsResponseStatus :: Lens.Lens' AcceptTransitGatewayVPCAttachmentResponse Lude.Int
atgvarsResponseStatus = Lens.lens (responseStatus :: AcceptTransitGatewayVPCAttachmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AcceptTransitGatewayVPCAttachmentResponse)
{-# DEPRECATED atgvarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
