-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayVPCAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayVPCAttachment
  ( TransitGatewayVPCAttachment (..),

    -- * Smart constructor
    mkTransitGatewayVPCAttachment,

    -- * Lenses
    tgvaCreationTime,
    tgvaState,
    tgvaSubnetIds,
    tgvaVPCId,
    tgvaTransitGatewayId,
    tgvaOptions,
    tgvaTransitGatewayAttachmentId,
    tgvaTags,
    tgvaVPCOwnerId,
  )
where

import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayAttachmentState
import Network.AWS.EC2.Types.TransitGatewayVPCAttachmentOptions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a VPC attachment.
--
-- /See:/ 'mkTransitGatewayVPCAttachment' smart constructor.
data TransitGatewayVPCAttachment = TransitGatewayVPCAttachment'
  { creationTime ::
      Lude.Maybe Lude.ISO8601,
    state ::
      Lude.Maybe
        TransitGatewayAttachmentState,
    subnetIds :: Lude.Maybe [Lude.Text],
    vpcId :: Lude.Maybe Lude.Text,
    transitGatewayId ::
      Lude.Maybe Lude.Text,
    options ::
      Lude.Maybe
        TransitGatewayVPCAttachmentOptions,
    transitGatewayAttachmentId ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    vpcOwnerId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitGatewayVPCAttachment' with the minimum fields required to make a request.
--
-- * 'creationTime' - The creation time.
-- * 'options' - The VPC attachment options.
-- * 'state' - The state of the VPC attachment. Note that the @initiating@ state has been deprecated.
-- * 'subnetIds' - The IDs of the subnets.
-- * 'tags' - The tags for the VPC attachment.
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
-- * 'transitGatewayId' - The ID of the transit gateway.
-- * 'vpcId' - The ID of the VPC.
-- * 'vpcOwnerId' - The ID of the AWS account that owns the VPC.
mkTransitGatewayVPCAttachment ::
  TransitGatewayVPCAttachment
mkTransitGatewayVPCAttachment =
  TransitGatewayVPCAttachment'
    { creationTime = Lude.Nothing,
      state = Lude.Nothing,
      subnetIds = Lude.Nothing,
      vpcId = Lude.Nothing,
      transitGatewayId = Lude.Nothing,
      options = Lude.Nothing,
      transitGatewayAttachmentId = Lude.Nothing,
      tags = Lude.Nothing,
      vpcOwnerId = Lude.Nothing
    }

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaCreationTime :: Lens.Lens' TransitGatewayVPCAttachment (Lude.Maybe Lude.ISO8601)
tgvaCreationTime = Lens.lens (creationTime :: TransitGatewayVPCAttachment -> Lude.Maybe Lude.ISO8601) (\s a -> s {creationTime = a} :: TransitGatewayVPCAttachment)
{-# DEPRECATED tgvaCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The state of the VPC attachment. Note that the @initiating@ state has been deprecated.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaState :: Lens.Lens' TransitGatewayVPCAttachment (Lude.Maybe TransitGatewayAttachmentState)
tgvaState = Lens.lens (state :: TransitGatewayVPCAttachment -> Lude.Maybe TransitGatewayAttachmentState) (\s a -> s {state = a} :: TransitGatewayVPCAttachment)
{-# DEPRECATED tgvaState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The IDs of the subnets.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaSubnetIds :: Lens.Lens' TransitGatewayVPCAttachment (Lude.Maybe [Lude.Text])
tgvaSubnetIds = Lens.lens (subnetIds :: TransitGatewayVPCAttachment -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: TransitGatewayVPCAttachment)
{-# DEPRECATED tgvaSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaVPCId :: Lens.Lens' TransitGatewayVPCAttachment (Lude.Maybe Lude.Text)
tgvaVPCId = Lens.lens (vpcId :: TransitGatewayVPCAttachment -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: TransitGatewayVPCAttachment)
{-# DEPRECATED tgvaVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaTransitGatewayId :: Lens.Lens' TransitGatewayVPCAttachment (Lude.Maybe Lude.Text)
tgvaTransitGatewayId = Lens.lens (transitGatewayId :: TransitGatewayVPCAttachment -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayId = a} :: TransitGatewayVPCAttachment)
{-# DEPRECATED tgvaTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | The VPC attachment options.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaOptions :: Lens.Lens' TransitGatewayVPCAttachment (Lude.Maybe TransitGatewayVPCAttachmentOptions)
tgvaOptions = Lens.lens (options :: TransitGatewayVPCAttachment -> Lude.Maybe TransitGatewayVPCAttachmentOptions) (\s a -> s {options = a} :: TransitGatewayVPCAttachment)
{-# DEPRECATED tgvaOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayVPCAttachment (Lude.Maybe Lude.Text)
tgvaTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: TransitGatewayVPCAttachment -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: TransitGatewayVPCAttachment)
{-# DEPRECATED tgvaTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | The tags for the VPC attachment.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaTags :: Lens.Lens' TransitGatewayVPCAttachment (Lude.Maybe [Tag])
tgvaTags = Lens.lens (tags :: TransitGatewayVPCAttachment -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: TransitGatewayVPCAttachment)
{-# DEPRECATED tgvaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the AWS account that owns the VPC.
--
-- /Note:/ Consider using 'vpcOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaVPCOwnerId :: Lens.Lens' TransitGatewayVPCAttachment (Lude.Maybe Lude.Text)
tgvaVPCOwnerId = Lens.lens (vpcOwnerId :: TransitGatewayVPCAttachment -> Lude.Maybe Lude.Text) (\s a -> s {vpcOwnerId = a} :: TransitGatewayVPCAttachment)
{-# DEPRECATED tgvaVPCOwnerId "Use generic-lens or generic-optics with 'vpcOwnerId' instead." #-}

instance Lude.FromXML TransitGatewayVPCAttachment where
  parseXML x =
    TransitGatewayVPCAttachment'
      Lude.<$> (x Lude..@? "creationTime")
      Lude.<*> (x Lude..@? "state")
      Lude.<*> ( x Lude..@? "subnetIds" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "vpcId")
      Lude.<*> (x Lude..@? "transitGatewayId")
      Lude.<*> (x Lude..@? "options")
      Lude.<*> (x Lude..@? "transitGatewayAttachmentId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "vpcOwnerId")
