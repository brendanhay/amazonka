{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGateway
  ( TransitGateway (..),

    -- * Smart constructor
    mkTransitGateway,

    -- * Lenses
    tgCreationTime,
    tgDescription,
    tgOptions,
    tgOwnerId,
    tgState,
    tgTags,
    tgTransitGatewayArn,
    tgTransitGatewayId,
  )
where

import qualified Network.AWS.EC2.Types.Description as Types
import qualified Network.AWS.EC2.Types.OwnerId as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.TransitGatewayArn as Types
import qualified Network.AWS.EC2.Types.TransitGatewayId as Types
import qualified Network.AWS.EC2.Types.TransitGatewayOptions as Types
import qualified Network.AWS.EC2.Types.TransitGatewayState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a transit gateway.
--
-- /See:/ 'mkTransitGateway' smart constructor.
data TransitGateway = TransitGateway'
  { -- | The creation time.
    creationTime :: Core.Maybe Core.UTCTime,
    -- | The description of the transit gateway.
    description :: Core.Maybe Types.Description,
    -- | The transit gateway options.
    options :: Core.Maybe Types.TransitGatewayOptions,
    -- | The ID of the AWS account ID that owns the transit gateway.
    ownerId :: Core.Maybe Types.OwnerId,
    -- | The state of the transit gateway.
    state :: Core.Maybe Types.TransitGatewayState,
    -- | The tags for the transit gateway.
    tags :: Core.Maybe [Types.Tag],
    -- | The Amazon Resource Name (ARN) of the transit gateway.
    transitGatewayArn :: Core.Maybe Types.TransitGatewayArn,
    -- | The ID of the transit gateway.
    transitGatewayId :: Core.Maybe Types.TransitGatewayId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TransitGateway' value with any optional fields omitted.
mkTransitGateway ::
  TransitGateway
mkTransitGateway =
  TransitGateway'
    { creationTime = Core.Nothing,
      description = Core.Nothing,
      options = Core.Nothing,
      ownerId = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      transitGatewayArn = Core.Nothing,
      transitGatewayId = Core.Nothing
    }

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgCreationTime :: Lens.Lens' TransitGateway (Core.Maybe Core.UTCTime)
tgCreationTime = Lens.field @"creationTime"
{-# DEPRECATED tgCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The description of the transit gateway.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgDescription :: Lens.Lens' TransitGateway (Core.Maybe Types.Description)
tgDescription = Lens.field @"description"
{-# DEPRECATED tgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The transit gateway options.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgOptions :: Lens.Lens' TransitGateway (Core.Maybe Types.TransitGatewayOptions)
tgOptions = Lens.field @"options"
{-# DEPRECATED tgOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The ID of the AWS account ID that owns the transit gateway.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgOwnerId :: Lens.Lens' TransitGateway (Core.Maybe Types.OwnerId)
tgOwnerId = Lens.field @"ownerId"
{-# DEPRECATED tgOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The state of the transit gateway.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgState :: Lens.Lens' TransitGateway (Core.Maybe Types.TransitGatewayState)
tgState = Lens.field @"state"
{-# DEPRECATED tgState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The tags for the transit gateway.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgTags :: Lens.Lens' TransitGateway (Core.Maybe [Types.Tag])
tgTags = Lens.field @"tags"
{-# DEPRECATED tgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Name (ARN) of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgTransitGatewayArn :: Lens.Lens' TransitGateway (Core.Maybe Types.TransitGatewayArn)
tgTransitGatewayArn = Lens.field @"transitGatewayArn"
{-# DEPRECATED tgTransitGatewayArn "Use generic-lens or generic-optics with 'transitGatewayArn' instead." #-}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgTransitGatewayId :: Lens.Lens' TransitGateway (Core.Maybe Types.TransitGatewayId)
tgTransitGatewayId = Lens.field @"transitGatewayId"
{-# DEPRECATED tgTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

instance Core.FromXML TransitGateway where
  parseXML x =
    TransitGateway'
      Core.<$> (x Core..@? "creationTime")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "options")
      Core.<*> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "transitGatewayArn")
      Core.<*> (x Core..@? "transitGatewayId")
