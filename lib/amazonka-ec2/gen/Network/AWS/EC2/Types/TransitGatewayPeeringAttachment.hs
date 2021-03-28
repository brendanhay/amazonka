{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayPeeringAttachment
  ( TransitGatewayPeeringAttachment (..)
  -- * Smart constructor
  , mkTransitGatewayPeeringAttachment
  -- * Lenses
  , tgpaAccepterTgwInfo
  , tgpaCreationTime
  , tgpaRequesterTgwInfo
  , tgpaState
  , tgpaStatus
  , tgpaTags
  , tgpaTransitGatewayAttachmentId
  ) where

import qualified Network.AWS.EC2.Types.PeeringAttachmentStatus as Types
import qualified Network.AWS.EC2.Types.PeeringTgwInfo as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.TransitGatewayAttachmentState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the transit gateway peering attachment.
--
-- /See:/ 'mkTransitGatewayPeeringAttachment' smart constructor.
data TransitGatewayPeeringAttachment = TransitGatewayPeeringAttachment'
  { accepterTgwInfo :: Core.Maybe Types.PeeringTgwInfo
    -- ^ Information about the accepter transit gateway.
  , creationTime :: Core.Maybe Core.UTCTime
    -- ^ The time the transit gateway peering attachment was created.
  , requesterTgwInfo :: Core.Maybe Types.PeeringTgwInfo
    -- ^ Information about the requester transit gateway.
  , state :: Core.Maybe Types.TransitGatewayAttachmentState
    -- ^ The state of the transit gateway peering attachment. Note that the @initiating@ state has been deprecated.
  , status :: Core.Maybe Types.PeeringAttachmentStatus
    -- ^ The status of the transit gateway peering attachment.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags for the transit gateway peering attachment.
  , transitGatewayAttachmentId :: Core.Maybe Core.Text
    -- ^ The ID of the transit gateway peering attachment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TransitGatewayPeeringAttachment' value with any optional fields omitted.
mkTransitGatewayPeeringAttachment
    :: TransitGatewayPeeringAttachment
mkTransitGatewayPeeringAttachment
  = TransitGatewayPeeringAttachment'{accepterTgwInfo = Core.Nothing,
                                     creationTime = Core.Nothing, requesterTgwInfo = Core.Nothing,
                                     state = Core.Nothing, status = Core.Nothing,
                                     tags = Core.Nothing, transitGatewayAttachmentId = Core.Nothing}

-- | Information about the accepter transit gateway.
--
-- /Note:/ Consider using 'accepterTgwInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpaAccepterTgwInfo :: Lens.Lens' TransitGatewayPeeringAttachment (Core.Maybe Types.PeeringTgwInfo)
tgpaAccepterTgwInfo = Lens.field @"accepterTgwInfo"
{-# INLINEABLE tgpaAccepterTgwInfo #-}
{-# DEPRECATED accepterTgwInfo "Use generic-lens or generic-optics with 'accepterTgwInfo' instead"  #-}

-- | The time the transit gateway peering attachment was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpaCreationTime :: Lens.Lens' TransitGatewayPeeringAttachment (Core.Maybe Core.UTCTime)
tgpaCreationTime = Lens.field @"creationTime"
{-# INLINEABLE tgpaCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | Information about the requester transit gateway.
--
-- /Note:/ Consider using 'requesterTgwInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpaRequesterTgwInfo :: Lens.Lens' TransitGatewayPeeringAttachment (Core.Maybe Types.PeeringTgwInfo)
tgpaRequesterTgwInfo = Lens.field @"requesterTgwInfo"
{-# INLINEABLE tgpaRequesterTgwInfo #-}
{-# DEPRECATED requesterTgwInfo "Use generic-lens or generic-optics with 'requesterTgwInfo' instead"  #-}

-- | The state of the transit gateway peering attachment. Note that the @initiating@ state has been deprecated.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpaState :: Lens.Lens' TransitGatewayPeeringAttachment (Core.Maybe Types.TransitGatewayAttachmentState)
tgpaState = Lens.field @"state"
{-# INLINEABLE tgpaState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The status of the transit gateway peering attachment.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpaStatus :: Lens.Lens' TransitGatewayPeeringAttachment (Core.Maybe Types.PeeringAttachmentStatus)
tgpaStatus = Lens.field @"status"
{-# INLINEABLE tgpaStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The tags for the transit gateway peering attachment.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpaTags :: Lens.Lens' TransitGatewayPeeringAttachment (Core.Maybe [Types.Tag])
tgpaTags = Lens.field @"tags"
{-# INLINEABLE tgpaTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ID of the transit gateway peering attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpaTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayPeeringAttachment (Core.Maybe Core.Text)
tgpaTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE tgpaTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

instance Core.FromXML TransitGatewayPeeringAttachment where
        parseXML x
          = TransitGatewayPeeringAttachment' Core.<$>
              (x Core..@? "accepterTgwInfo") Core.<*> x Core..@? "creationTime"
                Core.<*> x Core..@? "requesterTgwInfo"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "status"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "transitGatewayAttachmentId"
