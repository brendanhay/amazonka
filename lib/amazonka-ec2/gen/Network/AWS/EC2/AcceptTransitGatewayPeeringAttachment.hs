{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AcceptTransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a transit gateway peering attachment request. The peering attachment must be in the @pendingAcceptance@ state.
module Network.AWS.EC2.AcceptTransitGatewayPeeringAttachment
  ( -- * Creating a request
    AcceptTransitGatewayPeeringAttachment (..),
    mkAcceptTransitGatewayPeeringAttachment,

    -- ** Request lenses
    atgpaDryRun,
    atgpaTransitGatewayAttachmentId,

    -- * Destructuring the response
    AcceptTransitGatewayPeeringAttachmentResponse (..),
    mkAcceptTransitGatewayPeeringAttachmentResponse,

    -- ** Response lenses
    atgparsTransitGatewayPeeringAttachment,
    atgparsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAcceptTransitGatewayPeeringAttachment' smart constructor.
data AcceptTransitGatewayPeeringAttachment = AcceptTransitGatewayPeeringAttachment'
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

-- | Creates a value of 'AcceptTransitGatewayPeeringAttachment' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'transitGatewayAttachmentId' - The ID of the transit gateway attachment.
mkAcceptTransitGatewayPeeringAttachment ::
  -- | 'transitGatewayAttachmentId'
  Lude.Text ->
  AcceptTransitGatewayPeeringAttachment
mkAcceptTransitGatewayPeeringAttachment
  pTransitGatewayAttachmentId_ =
    AcceptTransitGatewayPeeringAttachment'
      { dryRun = Lude.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgpaDryRun :: Lens.Lens' AcceptTransitGatewayPeeringAttachment (Lude.Maybe Lude.Bool)
atgpaDryRun = Lens.lens (dryRun :: AcceptTransitGatewayPeeringAttachment -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AcceptTransitGatewayPeeringAttachment)
{-# DEPRECATED atgpaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the transit gateway attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgpaTransitGatewayAttachmentId :: Lens.Lens' AcceptTransitGatewayPeeringAttachment Lude.Text
atgpaTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: AcceptTransitGatewayPeeringAttachment -> Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: AcceptTransitGatewayPeeringAttachment)
{-# DEPRECATED atgpaTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Lude.AWSRequest AcceptTransitGatewayPeeringAttachment where
  type
    Rs AcceptTransitGatewayPeeringAttachment =
      AcceptTransitGatewayPeeringAttachmentResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AcceptTransitGatewayPeeringAttachmentResponse'
            Lude.<$> (x Lude..@? "transitGatewayPeeringAttachment")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AcceptTransitGatewayPeeringAttachment where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AcceptTransitGatewayPeeringAttachment where
  toPath = Lude.const "/"

instance Lude.ToQuery AcceptTransitGatewayPeeringAttachment where
  toQuery AcceptTransitGatewayPeeringAttachment' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AcceptTransitGatewayPeeringAttachment" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "TransitGatewayAttachmentId" Lude.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'mkAcceptTransitGatewayPeeringAttachmentResponse' smart constructor.
data AcceptTransitGatewayPeeringAttachmentResponse = AcceptTransitGatewayPeeringAttachmentResponse'
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

-- | Creates a value of 'AcceptTransitGatewayPeeringAttachmentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'transitGatewayPeeringAttachment' - The transit gateway peering attachment.
mkAcceptTransitGatewayPeeringAttachmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AcceptTransitGatewayPeeringAttachmentResponse
mkAcceptTransitGatewayPeeringAttachmentResponse pResponseStatus_ =
  AcceptTransitGatewayPeeringAttachmentResponse'
    { transitGatewayPeeringAttachment =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The transit gateway peering attachment.
--
-- /Note:/ Consider using 'transitGatewayPeeringAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgparsTransitGatewayPeeringAttachment :: Lens.Lens' AcceptTransitGatewayPeeringAttachmentResponse (Lude.Maybe TransitGatewayPeeringAttachment)
atgparsTransitGatewayPeeringAttachment = Lens.lens (transitGatewayPeeringAttachment :: AcceptTransitGatewayPeeringAttachmentResponse -> Lude.Maybe TransitGatewayPeeringAttachment) (\s a -> s {transitGatewayPeeringAttachment = a} :: AcceptTransitGatewayPeeringAttachmentResponse)
{-# DEPRECATED atgparsTransitGatewayPeeringAttachment "Use generic-lens or generic-optics with 'transitGatewayPeeringAttachment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgparsResponseStatus :: Lens.Lens' AcceptTransitGatewayPeeringAttachmentResponse Lude.Int
atgparsResponseStatus = Lens.lens (responseStatus :: AcceptTransitGatewayPeeringAttachmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AcceptTransitGatewayPeeringAttachmentResponse)
{-# DEPRECATED atgparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
