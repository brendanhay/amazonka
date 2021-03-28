{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayVpcAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayVpcAttachment
  ( TransitGatewayVpcAttachment (..)
  -- * Smart constructor
  , mkTransitGatewayVpcAttachment
  -- * Lenses
  , tgvaCreationTime
  , tgvaOptions
  , tgvaState
  , tgvaSubnetIds
  , tgvaTags
  , tgvaTransitGatewayAttachmentId
  , tgvaTransitGatewayId
  , tgvaVpcId
  , tgvaVpcOwnerId
  ) where

import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.TransitGatewayAttachmentState as Types
import qualified Network.AWS.EC2.Types.TransitGatewayVpcAttachmentOptions as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a VPC attachment.
--
-- /See:/ 'mkTransitGatewayVpcAttachment' smart constructor.
data TransitGatewayVpcAttachment = TransitGatewayVpcAttachment'
  { creationTime :: Core.Maybe Core.UTCTime
    -- ^ The creation time.
  , options :: Core.Maybe Types.TransitGatewayVpcAttachmentOptions
    -- ^ The VPC attachment options.
  , state :: Core.Maybe Types.TransitGatewayAttachmentState
    -- ^ The state of the VPC attachment. Note that the @initiating@ state has been deprecated.
  , subnetIds :: Core.Maybe [Core.Text]
    -- ^ The IDs of the subnets.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags for the VPC attachment.
  , transitGatewayAttachmentId :: Core.Maybe Core.Text
    -- ^ The ID of the attachment.
  , transitGatewayId :: Core.Maybe Core.Text
    -- ^ The ID of the transit gateway.
  , vpcId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC.
  , vpcOwnerId :: Core.Maybe Core.Text
    -- ^ The ID of the AWS account that owns the VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TransitGatewayVpcAttachment' value with any optional fields omitted.
mkTransitGatewayVpcAttachment
    :: TransitGatewayVpcAttachment
mkTransitGatewayVpcAttachment
  = TransitGatewayVpcAttachment'{creationTime = Core.Nothing,
                                 options = Core.Nothing, state = Core.Nothing,
                                 subnetIds = Core.Nothing, tags = Core.Nothing,
                                 transitGatewayAttachmentId = Core.Nothing,
                                 transitGatewayId = Core.Nothing, vpcId = Core.Nothing,
                                 vpcOwnerId = Core.Nothing}

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaCreationTime :: Lens.Lens' TransitGatewayVpcAttachment (Core.Maybe Core.UTCTime)
tgvaCreationTime = Lens.field @"creationTime"
{-# INLINEABLE tgvaCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The VPC attachment options.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaOptions :: Lens.Lens' TransitGatewayVpcAttachment (Core.Maybe Types.TransitGatewayVpcAttachmentOptions)
tgvaOptions = Lens.field @"options"
{-# INLINEABLE tgvaOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | The state of the VPC attachment. Note that the @initiating@ state has been deprecated.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaState :: Lens.Lens' TransitGatewayVpcAttachment (Core.Maybe Types.TransitGatewayAttachmentState)
tgvaState = Lens.field @"state"
{-# INLINEABLE tgvaState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The IDs of the subnets.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaSubnetIds :: Lens.Lens' TransitGatewayVpcAttachment (Core.Maybe [Core.Text])
tgvaSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE tgvaSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | The tags for the VPC attachment.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaTags :: Lens.Lens' TransitGatewayVpcAttachment (Core.Maybe [Types.Tag])
tgvaTags = Lens.field @"tags"
{-# INLINEABLE tgvaTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayVpcAttachment (Core.Maybe Core.Text)
tgvaTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE tgvaTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaTransitGatewayId :: Lens.Lens' TransitGatewayVpcAttachment (Core.Maybe Core.Text)
tgvaTransitGatewayId = Lens.field @"transitGatewayId"
{-# INLINEABLE tgvaTransitGatewayId #-}
{-# DEPRECATED transitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaVpcId :: Lens.Lens' TransitGatewayVpcAttachment (Core.Maybe Core.Text)
tgvaVpcId = Lens.field @"vpcId"
{-# INLINEABLE tgvaVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | The ID of the AWS account that owns the VPC.
--
-- /Note:/ Consider using 'vpcOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgvaVpcOwnerId :: Lens.Lens' TransitGatewayVpcAttachment (Core.Maybe Core.Text)
tgvaVpcOwnerId = Lens.field @"vpcOwnerId"
{-# INLINEABLE tgvaVpcOwnerId #-}
{-# DEPRECATED vpcOwnerId "Use generic-lens or generic-optics with 'vpcOwnerId' instead"  #-}

instance Core.FromXML TransitGatewayVpcAttachment where
        parseXML x
          = TransitGatewayVpcAttachment' Core.<$>
              (x Core..@? "creationTime") Core.<*> x Core..@? "options" Core.<*>
                x Core..@? "state"
                Core.<*> x Core..@? "subnetIds" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "transitGatewayAttachmentId"
                Core.<*> x Core..@? "transitGatewayId"
                Core.<*> x Core..@? "vpcId"
                Core.<*> x Core..@? "vpcOwnerId"
