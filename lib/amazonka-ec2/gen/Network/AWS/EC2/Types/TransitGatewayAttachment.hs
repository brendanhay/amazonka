{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayAttachment
  ( TransitGatewayAttachment (..)
  -- * Smart constructor
  , mkTransitGatewayAttachment
  -- * Lenses
  , tgaAssociation
  , tgaCreationTime
  , tgaResourceId
  , tgaResourceOwnerId
  , tgaResourceType
  , tgaState
  , tgaTags
  , tgaTransitGatewayAttachmentId
  , tgaTransitGatewayId
  , tgaTransitGatewayOwnerId
  ) where

import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.TransitGatewayAttachmentAssociation as Types
import qualified Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType as Types
import qualified Network.AWS.EC2.Types.TransitGatewayAttachmentState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an attachment between a resource and a transit gateway.
--
-- /See:/ 'mkTransitGatewayAttachment' smart constructor.
data TransitGatewayAttachment = TransitGatewayAttachment'
  { association :: Core.Maybe Types.TransitGatewayAttachmentAssociation
    -- ^ The association.
  , creationTime :: Core.Maybe Core.UTCTime
    -- ^ The creation time.
  , resourceId :: Core.Maybe Core.Text
    -- ^ The ID of the resource.
  , resourceOwnerId :: Core.Maybe Core.Text
    -- ^ The ID of the AWS account that owns the resource.
  , resourceType :: Core.Maybe Types.TransitGatewayAttachmentResourceType
    -- ^ The resource type. Note that the @tgw-peering@ resource type has been deprecated.
  , state :: Core.Maybe Types.TransitGatewayAttachmentState
    -- ^ The attachment state. Note that the @initiating@ state has been deprecated.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags for the attachment.
  , transitGatewayAttachmentId :: Core.Maybe Core.Text
    -- ^ The ID of the attachment.
  , transitGatewayId :: Core.Maybe Core.Text
    -- ^ The ID of the transit gateway.
  , transitGatewayOwnerId :: Core.Maybe Core.Text
    -- ^ The ID of the AWS account that owns the transit gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TransitGatewayAttachment' value with any optional fields omitted.
mkTransitGatewayAttachment
    :: TransitGatewayAttachment
mkTransitGatewayAttachment
  = TransitGatewayAttachment'{association = Core.Nothing,
                              creationTime = Core.Nothing, resourceId = Core.Nothing,
                              resourceOwnerId = Core.Nothing, resourceType = Core.Nothing,
                              state = Core.Nothing, tags = Core.Nothing,
                              transitGatewayAttachmentId = Core.Nothing,
                              transitGatewayId = Core.Nothing,
                              transitGatewayOwnerId = Core.Nothing}

-- | The association.
--
-- /Note:/ Consider using 'association' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaAssociation :: Lens.Lens' TransitGatewayAttachment (Core.Maybe Types.TransitGatewayAttachmentAssociation)
tgaAssociation = Lens.field @"association"
{-# INLINEABLE tgaAssociation #-}
{-# DEPRECATED association "Use generic-lens or generic-optics with 'association' instead"  #-}

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaCreationTime :: Lens.Lens' TransitGatewayAttachment (Core.Maybe Core.UTCTime)
tgaCreationTime = Lens.field @"creationTime"
{-# INLINEABLE tgaCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaResourceId :: Lens.Lens' TransitGatewayAttachment (Core.Maybe Core.Text)
tgaResourceId = Lens.field @"resourceId"
{-# INLINEABLE tgaResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The ID of the AWS account that owns the resource.
--
-- /Note:/ Consider using 'resourceOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaResourceOwnerId :: Lens.Lens' TransitGatewayAttachment (Core.Maybe Core.Text)
tgaResourceOwnerId = Lens.field @"resourceOwnerId"
{-# INLINEABLE tgaResourceOwnerId #-}
{-# DEPRECATED resourceOwnerId "Use generic-lens or generic-optics with 'resourceOwnerId' instead"  #-}

-- | The resource type. Note that the @tgw-peering@ resource type has been deprecated.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaResourceType :: Lens.Lens' TransitGatewayAttachment (Core.Maybe Types.TransitGatewayAttachmentResourceType)
tgaResourceType = Lens.field @"resourceType"
{-# INLINEABLE tgaResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The attachment state. Note that the @initiating@ state has been deprecated.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaState :: Lens.Lens' TransitGatewayAttachment (Core.Maybe Types.TransitGatewayAttachmentState)
tgaState = Lens.field @"state"
{-# INLINEABLE tgaState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The tags for the attachment.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaTags :: Lens.Lens' TransitGatewayAttachment (Core.Maybe [Types.Tag])
tgaTags = Lens.field @"tags"
{-# INLINEABLE tgaTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayAttachment (Core.Maybe Core.Text)
tgaTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE tgaTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaTransitGatewayId :: Lens.Lens' TransitGatewayAttachment (Core.Maybe Core.Text)
tgaTransitGatewayId = Lens.field @"transitGatewayId"
{-# INLINEABLE tgaTransitGatewayId #-}
{-# DEPRECATED transitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead"  #-}

-- | The ID of the AWS account that owns the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgaTransitGatewayOwnerId :: Lens.Lens' TransitGatewayAttachment (Core.Maybe Core.Text)
tgaTransitGatewayOwnerId = Lens.field @"transitGatewayOwnerId"
{-# INLINEABLE tgaTransitGatewayOwnerId #-}
{-# DEPRECATED transitGatewayOwnerId "Use generic-lens or generic-optics with 'transitGatewayOwnerId' instead"  #-}

instance Core.FromXML TransitGatewayAttachment where
        parseXML x
          = TransitGatewayAttachment' Core.<$>
              (x Core..@? "association") Core.<*> x Core..@? "creationTime"
                Core.<*> x Core..@? "resourceId"
                Core.<*> x Core..@? "resourceOwnerId"
                Core.<*> x Core..@? "resourceType"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "transitGatewayAttachmentId"
                Core.<*> x Core..@? "transitGatewayId"
                Core.<*> x Core..@? "transitGatewayOwnerId"
