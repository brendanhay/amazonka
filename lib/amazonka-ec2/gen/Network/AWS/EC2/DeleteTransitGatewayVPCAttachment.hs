{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTransitGatewayVPCAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified VPC attachment.
module Network.AWS.EC2.DeleteTransitGatewayVPCAttachment
  ( -- * Creating a request
    DeleteTransitGatewayVPCAttachment (..),
    mkDeleteTransitGatewayVPCAttachment,

    -- ** Request lenses
    dtgvaDryRun,
    dtgvaTransitGatewayAttachmentId,

    -- * Destructuring the response
    DeleteTransitGatewayVPCAttachmentResponse (..),
    mkDeleteTransitGatewayVPCAttachmentResponse,

    -- ** Response lenses
    dtgvpcarsTransitGatewayVPCAttachment,
    dtgvpcarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTransitGatewayVPCAttachment' smart constructor.
data DeleteTransitGatewayVPCAttachment = DeleteTransitGatewayVPCAttachment'
  { dryRun ::
      Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'DeleteTransitGatewayVPCAttachment' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
mkDeleteTransitGatewayVPCAttachment ::
  -- | 'transitGatewayAttachmentId'
  Lude.Text ->
  DeleteTransitGatewayVPCAttachment
mkDeleteTransitGatewayVPCAttachment pTransitGatewayAttachmentId_ =
  DeleteTransitGatewayVPCAttachment'
    { dryRun = Lude.Nothing,
      transitGatewayAttachmentId = pTransitGatewayAttachmentId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvaDryRun :: Lens.Lens' DeleteTransitGatewayVPCAttachment (Lude.Maybe Lude.Bool)
dtgvaDryRun = Lens.lens (dryRun :: DeleteTransitGatewayVPCAttachment -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteTransitGatewayVPCAttachment)
{-# DEPRECATED dtgvaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvaTransitGatewayAttachmentId :: Lens.Lens' DeleteTransitGatewayVPCAttachment Lude.Text
dtgvaTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: DeleteTransitGatewayVPCAttachment -> Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: DeleteTransitGatewayVPCAttachment)
{-# DEPRECATED dtgvaTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Lude.AWSRequest DeleteTransitGatewayVPCAttachment where
  type
    Rs DeleteTransitGatewayVPCAttachment =
      DeleteTransitGatewayVPCAttachmentResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteTransitGatewayVPCAttachmentResponse'
            Lude.<$> (x Lude..@? "transitGatewayVpcAttachment")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTransitGatewayVPCAttachment where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteTransitGatewayVPCAttachment where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTransitGatewayVPCAttachment where
  toQuery DeleteTransitGatewayVPCAttachment' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteTransitGatewayVpcAttachment" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "TransitGatewayAttachmentId" Lude.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'mkDeleteTransitGatewayVPCAttachmentResponse' smart constructor.
data DeleteTransitGatewayVPCAttachmentResponse = DeleteTransitGatewayVPCAttachmentResponse'
  { transitGatewayVPCAttachment ::
      Lude.Maybe
        TransitGatewayVPCAttachment,
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

-- | Creates a value of 'DeleteTransitGatewayVPCAttachmentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'transitGatewayVPCAttachment' - Information about the deleted VPC attachment.
mkDeleteTransitGatewayVPCAttachmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTransitGatewayVPCAttachmentResponse
mkDeleteTransitGatewayVPCAttachmentResponse pResponseStatus_ =
  DeleteTransitGatewayVPCAttachmentResponse'
    { transitGatewayVPCAttachment =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the deleted VPC attachment.
--
-- /Note:/ Consider using 'transitGatewayVPCAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvpcarsTransitGatewayVPCAttachment :: Lens.Lens' DeleteTransitGatewayVPCAttachmentResponse (Lude.Maybe TransitGatewayVPCAttachment)
dtgvpcarsTransitGatewayVPCAttachment = Lens.lens (transitGatewayVPCAttachment :: DeleteTransitGatewayVPCAttachmentResponse -> Lude.Maybe TransitGatewayVPCAttachment) (\s a -> s {transitGatewayVPCAttachment = a} :: DeleteTransitGatewayVPCAttachmentResponse)
{-# DEPRECATED dtgvpcarsTransitGatewayVPCAttachment "Use generic-lens or generic-optics with 'transitGatewayVPCAttachment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvpcarsResponseStatus :: Lens.Lens' DeleteTransitGatewayVPCAttachmentResponse Lude.Int
dtgvpcarsResponseStatus = Lens.lens (responseStatus :: DeleteTransitGatewayVPCAttachmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTransitGatewayVPCAttachmentResponse)
{-# DEPRECATED dtgvpcarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
