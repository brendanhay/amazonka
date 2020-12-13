{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dtgvpcaTransitGatewayAttachmentId,
    dtgvpcaDryRun,

    -- * Destructuring the response
    DeleteTransitGatewayVPCAttachmentResponse (..),
    mkDeleteTransitGatewayVPCAttachmentResponse,

    -- ** Response lenses
    dtgvarsTransitGatewayVPCAttachment,
    dtgvarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTransitGatewayVPCAttachment' smart constructor.
data DeleteTransitGatewayVPCAttachment = DeleteTransitGatewayVPCAttachment'
  { -- | The ID of the attachment.
    transitGatewayAttachmentId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTransitGatewayVPCAttachment' with the minimum fields required to make a request.
--
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDeleteTransitGatewayVPCAttachment ::
  -- | 'transitGatewayAttachmentId'
  Lude.Text ->
  DeleteTransitGatewayVPCAttachment
mkDeleteTransitGatewayVPCAttachment pTransitGatewayAttachmentId_ =
  DeleteTransitGatewayVPCAttachment'
    { transitGatewayAttachmentId =
        pTransitGatewayAttachmentId_,
      dryRun = Lude.Nothing
    }

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvpcaTransitGatewayAttachmentId :: Lens.Lens' DeleteTransitGatewayVPCAttachment Lude.Text
dtgvpcaTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: DeleteTransitGatewayVPCAttachment -> Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: DeleteTransitGatewayVPCAttachment)
{-# DEPRECATED dtgvpcaTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvpcaDryRun :: Lens.Lens' DeleteTransitGatewayVPCAttachment (Lude.Maybe Lude.Bool)
dtgvpcaDryRun = Lens.lens (dryRun :: DeleteTransitGatewayVPCAttachment -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteTransitGatewayVPCAttachment)
{-# DEPRECATED dtgvpcaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

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
        "TransitGatewayAttachmentId" Lude.=: transitGatewayAttachmentId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDeleteTransitGatewayVPCAttachmentResponse' smart constructor.
data DeleteTransitGatewayVPCAttachmentResponse = DeleteTransitGatewayVPCAttachmentResponse'
  { -- | Information about the deleted VPC attachment.
    transitGatewayVPCAttachment :: Lude.Maybe TransitGatewayVPCAttachment,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTransitGatewayVPCAttachmentResponse' with the minimum fields required to make a request.
--
-- * 'transitGatewayVPCAttachment' - Information about the deleted VPC attachment.
-- * 'responseStatus' - The response status code.
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
dtgvarsTransitGatewayVPCAttachment :: Lens.Lens' DeleteTransitGatewayVPCAttachmentResponse (Lude.Maybe TransitGatewayVPCAttachment)
dtgvarsTransitGatewayVPCAttachment = Lens.lens (transitGatewayVPCAttachment :: DeleteTransitGatewayVPCAttachmentResponse -> Lude.Maybe TransitGatewayVPCAttachment) (\s a -> s {transitGatewayVPCAttachment = a} :: DeleteTransitGatewayVPCAttachmentResponse)
{-# DEPRECATED dtgvarsTransitGatewayVPCAttachment "Use generic-lens or generic-optics with 'transitGatewayVPCAttachment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvarsResponseStatus :: Lens.Lens' DeleteTransitGatewayVPCAttachmentResponse Lude.Int
dtgvarsResponseStatus = Lens.lens (responseStatus :: DeleteTransitGatewayVPCAttachmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTransitGatewayVPCAttachmentResponse)
{-# DEPRECATED dtgvarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
