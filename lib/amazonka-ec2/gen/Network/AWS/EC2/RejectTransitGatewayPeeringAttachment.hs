{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RejectTransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a transit gateway peering attachment request.
module Network.AWS.EC2.RejectTransitGatewayPeeringAttachment
  ( -- * Creating a request
    RejectTransitGatewayPeeringAttachment (..),
    mkRejectTransitGatewayPeeringAttachment,

    -- ** Request lenses
    rtgpaDryRun,
    rtgpaTransitGatewayAttachmentId,

    -- * Destructuring the response
    RejectTransitGatewayPeeringAttachmentResponse (..),
    mkRejectTransitGatewayPeeringAttachmentResponse,

    -- ** Response lenses
    rtgparsTransitGatewayPeeringAttachment,
    rtgparsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRejectTransitGatewayPeeringAttachment' smart constructor.
data RejectTransitGatewayPeeringAttachment = RejectTransitGatewayPeeringAttachment'
  { dryRun ::
      Lude.Maybe
        Lude.Bool,
    transitGatewayAttachmentId ::
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

-- | Creates a value of 'RejectTransitGatewayPeeringAttachment' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'transitGatewayAttachmentId' - The ID of the transit gateway peering attachment.
mkRejectTransitGatewayPeeringAttachment ::
  -- | 'transitGatewayAttachmentId'
  Lude.Text ->
  RejectTransitGatewayPeeringAttachment
mkRejectTransitGatewayPeeringAttachment
  pTransitGatewayAttachmentId_ =
    RejectTransitGatewayPeeringAttachment'
      { dryRun = Lude.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgpaDryRun :: Lens.Lens' RejectTransitGatewayPeeringAttachment (Lude.Maybe Lude.Bool)
rtgpaDryRun = Lens.lens (dryRun :: RejectTransitGatewayPeeringAttachment -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: RejectTransitGatewayPeeringAttachment)
{-# DEPRECATED rtgpaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the transit gateway peering attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgpaTransitGatewayAttachmentId :: Lens.Lens' RejectTransitGatewayPeeringAttachment Lude.Text
rtgpaTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: RejectTransitGatewayPeeringAttachment -> Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: RejectTransitGatewayPeeringAttachment)
{-# DEPRECATED rtgpaTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Lude.AWSRequest RejectTransitGatewayPeeringAttachment where
  type
    Rs RejectTransitGatewayPeeringAttachment =
      RejectTransitGatewayPeeringAttachmentResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          RejectTransitGatewayPeeringAttachmentResponse'
            Lude.<$> (x Lude..@? "transitGatewayPeeringAttachment")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RejectTransitGatewayPeeringAttachment where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RejectTransitGatewayPeeringAttachment where
  toPath = Lude.const "/"

instance Lude.ToQuery RejectTransitGatewayPeeringAttachment where
  toQuery RejectTransitGatewayPeeringAttachment' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RejectTransitGatewayPeeringAttachment" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "TransitGatewayAttachmentId" Lude.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'mkRejectTransitGatewayPeeringAttachmentResponse' smart constructor.
data RejectTransitGatewayPeeringAttachmentResponse = RejectTransitGatewayPeeringAttachmentResponse'
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

-- | Creates a value of 'RejectTransitGatewayPeeringAttachmentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'transitGatewayPeeringAttachment' - The transit gateway peering attachment.
mkRejectTransitGatewayPeeringAttachmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RejectTransitGatewayPeeringAttachmentResponse
mkRejectTransitGatewayPeeringAttachmentResponse pResponseStatus_ =
  RejectTransitGatewayPeeringAttachmentResponse'
    { transitGatewayPeeringAttachment =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The transit gateway peering attachment.
--
-- /Note:/ Consider using 'transitGatewayPeeringAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgparsTransitGatewayPeeringAttachment :: Lens.Lens' RejectTransitGatewayPeeringAttachmentResponse (Lude.Maybe TransitGatewayPeeringAttachment)
rtgparsTransitGatewayPeeringAttachment = Lens.lens (transitGatewayPeeringAttachment :: RejectTransitGatewayPeeringAttachmentResponse -> Lude.Maybe TransitGatewayPeeringAttachment) (\s a -> s {transitGatewayPeeringAttachment = a} :: RejectTransitGatewayPeeringAttachmentResponse)
{-# DEPRECATED rtgparsTransitGatewayPeeringAttachment "Use generic-lens or generic-optics with 'transitGatewayPeeringAttachment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtgparsResponseStatus :: Lens.Lens' RejectTransitGatewayPeeringAttachmentResponse Lude.Int
rtgparsResponseStatus = Lens.lens (responseStatus :: RejectTransitGatewayPeeringAttachmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RejectTransitGatewayPeeringAttachmentResponse)
{-# DEPRECATED rtgparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
