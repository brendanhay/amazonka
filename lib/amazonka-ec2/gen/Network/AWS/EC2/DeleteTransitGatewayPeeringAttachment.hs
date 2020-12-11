{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a transit gateway peering attachment.
module Network.AWS.EC2.DeleteTransitGatewayPeeringAttachment
  ( -- * Creating a request
    DeleteTransitGatewayPeeringAttachment (..),
    mkDeleteTransitGatewayPeeringAttachment,

    -- ** Request lenses
    dtgpatDryRun,
    dtgpatTransitGatewayAttachmentId,

    -- * Destructuring the response
    DeleteTransitGatewayPeeringAttachmentResponse (..),
    mkDeleteTransitGatewayPeeringAttachmentResponse,

    -- ** Response lenses
    dtgparsTransitGatewayPeeringAttachment,
    dtgparsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTransitGatewayPeeringAttachment' smart constructor.
data DeleteTransitGatewayPeeringAttachment = DeleteTransitGatewayPeeringAttachment'
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

-- | Creates a value of 'DeleteTransitGatewayPeeringAttachment' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'transitGatewayAttachmentId' - The ID of the transit gateway peering attachment.
mkDeleteTransitGatewayPeeringAttachment ::
  -- | 'transitGatewayAttachmentId'
  Lude.Text ->
  DeleteTransitGatewayPeeringAttachment
mkDeleteTransitGatewayPeeringAttachment
  pTransitGatewayAttachmentId_ =
    DeleteTransitGatewayPeeringAttachment'
      { dryRun = Lude.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgpatDryRun :: Lens.Lens' DeleteTransitGatewayPeeringAttachment (Lude.Maybe Lude.Bool)
dtgpatDryRun = Lens.lens (dryRun :: DeleteTransitGatewayPeeringAttachment -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteTransitGatewayPeeringAttachment)
{-# DEPRECATED dtgpatDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the transit gateway peering attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgpatTransitGatewayAttachmentId :: Lens.Lens' DeleteTransitGatewayPeeringAttachment Lude.Text
dtgpatTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: DeleteTransitGatewayPeeringAttachment -> Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: DeleteTransitGatewayPeeringAttachment)
{-# DEPRECATED dtgpatTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Lude.AWSRequest DeleteTransitGatewayPeeringAttachment where
  type
    Rs DeleteTransitGatewayPeeringAttachment =
      DeleteTransitGatewayPeeringAttachmentResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteTransitGatewayPeeringAttachmentResponse'
            Lude.<$> (x Lude..@? "transitGatewayPeeringAttachment")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTransitGatewayPeeringAttachment where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteTransitGatewayPeeringAttachment where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTransitGatewayPeeringAttachment where
  toQuery DeleteTransitGatewayPeeringAttachment' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteTransitGatewayPeeringAttachment" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "TransitGatewayAttachmentId" Lude.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'mkDeleteTransitGatewayPeeringAttachmentResponse' smart constructor.
data DeleteTransitGatewayPeeringAttachmentResponse = DeleteTransitGatewayPeeringAttachmentResponse'
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

-- | Creates a value of 'DeleteTransitGatewayPeeringAttachmentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'transitGatewayPeeringAttachment' - The transit gateway peering attachment.
mkDeleteTransitGatewayPeeringAttachmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTransitGatewayPeeringAttachmentResponse
mkDeleteTransitGatewayPeeringAttachmentResponse pResponseStatus_ =
  DeleteTransitGatewayPeeringAttachmentResponse'
    { transitGatewayPeeringAttachment =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The transit gateway peering attachment.
--
-- /Note:/ Consider using 'transitGatewayPeeringAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgparsTransitGatewayPeeringAttachment :: Lens.Lens' DeleteTransitGatewayPeeringAttachmentResponse (Lude.Maybe TransitGatewayPeeringAttachment)
dtgparsTransitGatewayPeeringAttachment = Lens.lens (transitGatewayPeeringAttachment :: DeleteTransitGatewayPeeringAttachmentResponse -> Lude.Maybe TransitGatewayPeeringAttachment) (\s a -> s {transitGatewayPeeringAttachment = a} :: DeleteTransitGatewayPeeringAttachmentResponse)
{-# DEPRECATED dtgparsTransitGatewayPeeringAttachment "Use generic-lens or generic-optics with 'transitGatewayPeeringAttachment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgparsResponseStatus :: Lens.Lens' DeleteTransitGatewayPeeringAttachmentResponse Lude.Int
dtgparsResponseStatus = Lens.lens (responseStatus :: DeleteTransitGatewayPeeringAttachmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTransitGatewayPeeringAttachmentResponse)
{-# DEPRECATED dtgparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
