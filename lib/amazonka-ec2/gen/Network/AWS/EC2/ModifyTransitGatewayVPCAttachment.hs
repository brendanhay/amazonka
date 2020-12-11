{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyTransitGatewayVPCAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified VPC attachment.
module Network.AWS.EC2.ModifyTransitGatewayVPCAttachment
  ( -- * Creating a request
    ModifyTransitGatewayVPCAttachment (..),
    mkModifyTransitGatewayVPCAttachment,

    -- ** Request lenses
    mtgvaAddSubnetIds,
    mtgvaOptions,
    mtgvaRemoveSubnetIds,
    mtgvaDryRun,
    mtgvaTransitGatewayAttachmentId,

    -- * Destructuring the response
    ModifyTransitGatewayVPCAttachmentResponse (..),
    mkModifyTransitGatewayVPCAttachmentResponse,

    -- ** Response lenses
    mtgvarsTransitGatewayVPCAttachment,
    mtgvarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyTransitGatewayVPCAttachment' smart constructor.
data ModifyTransitGatewayVPCAttachment = ModifyTransitGatewayVPCAttachment'
  { addSubnetIds ::
      Lude.Maybe [Lude.Text],
    options ::
      Lude.Maybe
        ModifyTransitGatewayVPCAttachmentRequestOptions,
    removeSubnetIds ::
      Lude.Maybe [Lude.Text],
    dryRun ::
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

-- | Creates a value of 'ModifyTransitGatewayVPCAttachment' with the minimum fields required to make a request.
--
-- * 'addSubnetIds' - The IDs of one or more subnets to add. You can specify at most one subnet per Availability Zone.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'options' - The new VPC attachment options.
-- * 'removeSubnetIds' - The IDs of one or more subnets to remove.
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
mkModifyTransitGatewayVPCAttachment ::
  -- | 'transitGatewayAttachmentId'
  Lude.Text ->
  ModifyTransitGatewayVPCAttachment
mkModifyTransitGatewayVPCAttachment pTransitGatewayAttachmentId_ =
  ModifyTransitGatewayVPCAttachment'
    { addSubnetIds = Lude.Nothing,
      options = Lude.Nothing,
      removeSubnetIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      transitGatewayAttachmentId = pTransitGatewayAttachmentId_
    }

-- | The IDs of one or more subnets to add. You can specify at most one subnet per Availability Zone.
--
-- /Note:/ Consider using 'addSubnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgvaAddSubnetIds :: Lens.Lens' ModifyTransitGatewayVPCAttachment (Lude.Maybe [Lude.Text])
mtgvaAddSubnetIds = Lens.lens (addSubnetIds :: ModifyTransitGatewayVPCAttachment -> Lude.Maybe [Lude.Text]) (\s a -> s {addSubnetIds = a} :: ModifyTransitGatewayVPCAttachment)
{-# DEPRECATED mtgvaAddSubnetIds "Use generic-lens or generic-optics with 'addSubnetIds' instead." #-}

-- | The new VPC attachment options.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgvaOptions :: Lens.Lens' ModifyTransitGatewayVPCAttachment (Lude.Maybe ModifyTransitGatewayVPCAttachmentRequestOptions)
mtgvaOptions = Lens.lens (options :: ModifyTransitGatewayVPCAttachment -> Lude.Maybe ModifyTransitGatewayVPCAttachmentRequestOptions) (\s a -> s {options = a} :: ModifyTransitGatewayVPCAttachment)
{-# DEPRECATED mtgvaOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The IDs of one or more subnets to remove.
--
-- /Note:/ Consider using 'removeSubnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgvaRemoveSubnetIds :: Lens.Lens' ModifyTransitGatewayVPCAttachment (Lude.Maybe [Lude.Text])
mtgvaRemoveSubnetIds = Lens.lens (removeSubnetIds :: ModifyTransitGatewayVPCAttachment -> Lude.Maybe [Lude.Text]) (\s a -> s {removeSubnetIds = a} :: ModifyTransitGatewayVPCAttachment)
{-# DEPRECATED mtgvaRemoveSubnetIds "Use generic-lens or generic-optics with 'removeSubnetIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgvaDryRun :: Lens.Lens' ModifyTransitGatewayVPCAttachment (Lude.Maybe Lude.Bool)
mtgvaDryRun = Lens.lens (dryRun :: ModifyTransitGatewayVPCAttachment -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyTransitGatewayVPCAttachment)
{-# DEPRECATED mtgvaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgvaTransitGatewayAttachmentId :: Lens.Lens' ModifyTransitGatewayVPCAttachment Lude.Text
mtgvaTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: ModifyTransitGatewayVPCAttachment -> Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: ModifyTransitGatewayVPCAttachment)
{-# DEPRECATED mtgvaTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Lude.AWSRequest ModifyTransitGatewayVPCAttachment where
  type
    Rs ModifyTransitGatewayVPCAttachment =
      ModifyTransitGatewayVPCAttachmentResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyTransitGatewayVPCAttachmentResponse'
            Lude.<$> (x Lude..@? "transitGatewayVpcAttachment")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyTransitGatewayVPCAttachment where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyTransitGatewayVPCAttachment where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyTransitGatewayVPCAttachment where
  toQuery ModifyTransitGatewayVPCAttachment' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyTransitGatewayVpcAttachment" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "AddSubnetIds" Lude.<$> addSubnetIds),
        "Options" Lude.=: options,
        Lude.toQuery
          (Lude.toQueryList "RemoveSubnetIds" Lude.<$> removeSubnetIds),
        "DryRun" Lude.=: dryRun,
        "TransitGatewayAttachmentId" Lude.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'mkModifyTransitGatewayVPCAttachmentResponse' smart constructor.
data ModifyTransitGatewayVPCAttachmentResponse = ModifyTransitGatewayVPCAttachmentResponse'
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

-- | Creates a value of 'ModifyTransitGatewayVPCAttachmentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'transitGatewayVPCAttachment' - Information about the modified attachment.
mkModifyTransitGatewayVPCAttachmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyTransitGatewayVPCAttachmentResponse
mkModifyTransitGatewayVPCAttachmentResponse pResponseStatus_ =
  ModifyTransitGatewayVPCAttachmentResponse'
    { transitGatewayVPCAttachment =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the modified attachment.
--
-- /Note:/ Consider using 'transitGatewayVPCAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgvarsTransitGatewayVPCAttachment :: Lens.Lens' ModifyTransitGatewayVPCAttachmentResponse (Lude.Maybe TransitGatewayVPCAttachment)
mtgvarsTransitGatewayVPCAttachment = Lens.lens (transitGatewayVPCAttachment :: ModifyTransitGatewayVPCAttachmentResponse -> Lude.Maybe TransitGatewayVPCAttachment) (\s a -> s {transitGatewayVPCAttachment = a} :: ModifyTransitGatewayVPCAttachmentResponse)
{-# DEPRECATED mtgvarsTransitGatewayVPCAttachment "Use generic-lens or generic-optics with 'transitGatewayVPCAttachment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgvarsResponseStatus :: Lens.Lens' ModifyTransitGatewayVPCAttachmentResponse Lude.Int
mtgvarsResponseStatus = Lens.lens (responseStatus :: ModifyTransitGatewayVPCAttachmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyTransitGatewayVPCAttachmentResponse)
{-# DEPRECATED mtgvarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
