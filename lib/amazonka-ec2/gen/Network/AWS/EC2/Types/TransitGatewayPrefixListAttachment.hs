{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayPrefixListAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayPrefixListAttachment
  ( TransitGatewayPrefixListAttachment (..)
  -- * Smart constructor
  , mkTransitGatewayPrefixListAttachment
  -- * Lenses
  , tgplaResourceId
  , tgplaResourceType
  , tgplaTransitGatewayAttachmentId
  ) where

import qualified Network.AWS.EC2.Types.TransitGatewayAttachmentId as Types
import qualified Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a transit gateway prefix list attachment.
--
-- /See:/ 'mkTransitGatewayPrefixListAttachment' smart constructor.
data TransitGatewayPrefixListAttachment = TransitGatewayPrefixListAttachment'
  { resourceId :: Core.Maybe Core.Text
    -- ^ The ID of the resource.
  , resourceType :: Core.Maybe Types.TransitGatewayAttachmentResourceType
    -- ^ The resource type. Note that the @tgw-peering@ resource type has been deprecated.
  , transitGatewayAttachmentId :: Core.Maybe Types.TransitGatewayAttachmentId
    -- ^ The ID of the attachment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayPrefixListAttachment' value with any optional fields omitted.
mkTransitGatewayPrefixListAttachment
    :: TransitGatewayPrefixListAttachment
mkTransitGatewayPrefixListAttachment
  = TransitGatewayPrefixListAttachment'{resourceId = Core.Nothing,
                                        resourceType = Core.Nothing,
                                        transitGatewayAttachmentId = Core.Nothing}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgplaResourceId :: Lens.Lens' TransitGatewayPrefixListAttachment (Core.Maybe Core.Text)
tgplaResourceId = Lens.field @"resourceId"
{-# INLINEABLE tgplaResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The resource type. Note that the @tgw-peering@ resource type has been deprecated.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgplaResourceType :: Lens.Lens' TransitGatewayPrefixListAttachment (Core.Maybe Types.TransitGatewayAttachmentResourceType)
tgplaResourceType = Lens.field @"resourceType"
{-# INLINEABLE tgplaResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgplaTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayPrefixListAttachment (Core.Maybe Types.TransitGatewayAttachmentId)
tgplaTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE tgplaTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

instance Core.FromXML TransitGatewayPrefixListAttachment where
        parseXML x
          = TransitGatewayPrefixListAttachment' Core.<$>
              (x Core..@? "resourceId") Core.<*> x Core..@? "resourceType"
                Core.<*> x Core..@? "transitGatewayAttachmentId"
