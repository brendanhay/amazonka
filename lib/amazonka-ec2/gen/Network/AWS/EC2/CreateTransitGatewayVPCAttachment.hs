{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTransitGatewayVPCAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified VPC to the specified transit gateway.
--
-- If you attach a VPC with a CIDR range that overlaps the CIDR range of a VPC that is already attached, the new VPC CIDR range is not propagated to the default propagation route table.
-- To send VPC traffic to an attached transit gateway, add a route to the VPC route table using 'CreateRoute' .
module Network.AWS.EC2.CreateTransitGatewayVPCAttachment
  ( -- * Creating a request
    CreateTransitGatewayVPCAttachment (..),
    mkCreateTransitGatewayVPCAttachment,

    -- ** Request lenses
    ctgvaSubnetIds,
    ctgvaVPCId,
    ctgvaTagSpecifications,
    ctgvaTransitGatewayId,
    ctgvaOptions,
    ctgvaDryRun,

    -- * Destructuring the response
    CreateTransitGatewayVPCAttachmentResponse (..),
    mkCreateTransitGatewayVPCAttachmentResponse,

    -- ** Response lenses
    ctgvarsTransitGatewayVPCAttachment,
    ctgvarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTransitGatewayVPCAttachment' smart constructor.
data CreateTransitGatewayVPCAttachment = CreateTransitGatewayVPCAttachment'
  { -- | The IDs of one or more subnets. You can specify only one subnet per Availability Zone. You must specify at least one subnet, but we recommend that you specify two subnets for better availability. The transit gateway uses one IP address from each specified subnet.
    subnetIds :: [Lude.Text],
    -- | The ID of the VPC.
    vpcId :: Lude.Text,
    -- | The tags to apply to the VPC attachment.
    tagSpecifications :: Lude.Maybe [TagSpecification],
    -- | The ID of the transit gateway.
    transitGatewayId :: Lude.Text,
    -- | The VPC attachment options.
    options :: Lude.Maybe CreateTransitGatewayVPCAttachmentRequestOptions,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTransitGatewayVPCAttachment' with the minimum fields required to make a request.
--
-- * 'subnetIds' - The IDs of one or more subnets. You can specify only one subnet per Availability Zone. You must specify at least one subnet, but we recommend that you specify two subnets for better availability. The transit gateway uses one IP address from each specified subnet.
-- * 'vpcId' - The ID of the VPC.
-- * 'tagSpecifications' - The tags to apply to the VPC attachment.
-- * 'transitGatewayId' - The ID of the transit gateway.
-- * 'options' - The VPC attachment options.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCreateTransitGatewayVPCAttachment ::
  -- | 'vpcId'
  Lude.Text ->
  -- | 'transitGatewayId'
  Lude.Text ->
  CreateTransitGatewayVPCAttachment
mkCreateTransitGatewayVPCAttachment pVPCId_ pTransitGatewayId_ =
  CreateTransitGatewayVPCAttachment'
    { subnetIds = Lude.mempty,
      vpcId = pVPCId_,
      tagSpecifications = Lude.Nothing,
      transitGatewayId = pTransitGatewayId_,
      options = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The IDs of one or more subnets. You can specify only one subnet per Availability Zone. You must specify at least one subnet, but we recommend that you specify two subnets for better availability. The transit gateway uses one IP address from each specified subnet.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgvaSubnetIds :: Lens.Lens' CreateTransitGatewayVPCAttachment [Lude.Text]
ctgvaSubnetIds = Lens.lens (subnetIds :: CreateTransitGatewayVPCAttachment -> [Lude.Text]) (\s a -> s {subnetIds = a} :: CreateTransitGatewayVPCAttachment)
{-# DEPRECATED ctgvaSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgvaVPCId :: Lens.Lens' CreateTransitGatewayVPCAttachment Lude.Text
ctgvaVPCId = Lens.lens (vpcId :: CreateTransitGatewayVPCAttachment -> Lude.Text) (\s a -> s {vpcId = a} :: CreateTransitGatewayVPCAttachment)
{-# DEPRECATED ctgvaVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The tags to apply to the VPC attachment.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgvaTagSpecifications :: Lens.Lens' CreateTransitGatewayVPCAttachment (Lude.Maybe [TagSpecification])
ctgvaTagSpecifications = Lens.lens (tagSpecifications :: CreateTransitGatewayVPCAttachment -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateTransitGatewayVPCAttachment)
{-# DEPRECATED ctgvaTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgvaTransitGatewayId :: Lens.Lens' CreateTransitGatewayVPCAttachment Lude.Text
ctgvaTransitGatewayId = Lens.lens (transitGatewayId :: CreateTransitGatewayVPCAttachment -> Lude.Text) (\s a -> s {transitGatewayId = a} :: CreateTransitGatewayVPCAttachment)
{-# DEPRECATED ctgvaTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | The VPC attachment options.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgvaOptions :: Lens.Lens' CreateTransitGatewayVPCAttachment (Lude.Maybe CreateTransitGatewayVPCAttachmentRequestOptions)
ctgvaOptions = Lens.lens (options :: CreateTransitGatewayVPCAttachment -> Lude.Maybe CreateTransitGatewayVPCAttachmentRequestOptions) (\s a -> s {options = a} :: CreateTransitGatewayVPCAttachment)
{-# DEPRECATED ctgvaOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgvaDryRun :: Lens.Lens' CreateTransitGatewayVPCAttachment (Lude.Maybe Lude.Bool)
ctgvaDryRun = Lens.lens (dryRun :: CreateTransitGatewayVPCAttachment -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateTransitGatewayVPCAttachment)
{-# DEPRECATED ctgvaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateTransitGatewayVPCAttachment where
  type
    Rs CreateTransitGatewayVPCAttachment =
      CreateTransitGatewayVPCAttachmentResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateTransitGatewayVPCAttachmentResponse'
            Lude.<$> (x Lude..@? "transitGatewayVpcAttachment")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTransitGatewayVPCAttachment where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateTransitGatewayVPCAttachment where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTransitGatewayVPCAttachment where
  toQuery CreateTransitGatewayVPCAttachment' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateTransitGatewayVpcAttachment" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQueryList "SubnetIds" subnetIds,
        "VpcId" Lude.=: vpcId,
        Lude.toQuery
          (Lude.toQueryList "TagSpecifications" Lude.<$> tagSpecifications),
        "TransitGatewayId" Lude.=: transitGatewayId,
        "Options" Lude.=: options,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateTransitGatewayVPCAttachmentResponse' smart constructor.
data CreateTransitGatewayVPCAttachmentResponse = CreateTransitGatewayVPCAttachmentResponse'
  { -- | Information about the VPC attachment.
    transitGatewayVPCAttachment :: Lude.Maybe TransitGatewayVPCAttachment,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTransitGatewayVPCAttachmentResponse' with the minimum fields required to make a request.
--
-- * 'transitGatewayVPCAttachment' - Information about the VPC attachment.
-- * 'responseStatus' - The response status code.
mkCreateTransitGatewayVPCAttachmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTransitGatewayVPCAttachmentResponse
mkCreateTransitGatewayVPCAttachmentResponse pResponseStatus_ =
  CreateTransitGatewayVPCAttachmentResponse'
    { transitGatewayVPCAttachment =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the VPC attachment.
--
-- /Note:/ Consider using 'transitGatewayVPCAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgvarsTransitGatewayVPCAttachment :: Lens.Lens' CreateTransitGatewayVPCAttachmentResponse (Lude.Maybe TransitGatewayVPCAttachment)
ctgvarsTransitGatewayVPCAttachment = Lens.lens (transitGatewayVPCAttachment :: CreateTransitGatewayVPCAttachmentResponse -> Lude.Maybe TransitGatewayVPCAttachment) (\s a -> s {transitGatewayVPCAttachment = a} :: CreateTransitGatewayVPCAttachmentResponse)
{-# DEPRECATED ctgvarsTransitGatewayVPCAttachment "Use generic-lens or generic-optics with 'transitGatewayVPCAttachment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgvarsResponseStatus :: Lens.Lens' CreateTransitGatewayVPCAttachmentResponse Lude.Int
ctgvarsResponseStatus = Lens.lens (responseStatus :: CreateTransitGatewayVPCAttachmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTransitGatewayVPCAttachmentResponse)
{-# DEPRECATED ctgvarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
