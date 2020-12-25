{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayPrefixListReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayPrefixListReference
  ( TransitGatewayPrefixListReference (..),

    -- * Smart constructor
    mkTransitGatewayPrefixListReference,

    -- * Lenses
    tgplrBlackhole,
    tgplrPrefixListId,
    tgplrPrefixListOwnerId,
    tgplrState,
    tgplrTransitGatewayAttachment,
    tgplrTransitGatewayRouteTableId,
  )
where

import qualified Network.AWS.EC2.Types.PrefixListResourceId as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.TransitGatewayPrefixListAttachment as Types
import qualified Network.AWS.EC2.Types.TransitGatewayPrefixListReferenceState as Types
import qualified Network.AWS.EC2.Types.TransitGatewayRouteTableId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a prefix list reference.
--
-- /See:/ 'mkTransitGatewayPrefixListReference' smart constructor.
data TransitGatewayPrefixListReference = TransitGatewayPrefixListReference'
  { -- | Indicates whether traffic that matches this route is dropped.
    blackhole :: Core.Maybe Core.Bool,
    -- | The ID of the prefix list.
    prefixListId :: Core.Maybe Types.PrefixListResourceId,
    -- | The ID of the prefix list owner.
    prefixListOwnerId :: Core.Maybe Types.String,
    -- | The state of the prefix list reference.
    state :: Core.Maybe Types.TransitGatewayPrefixListReferenceState,
    -- | Information about the transit gateway attachment.
    transitGatewayAttachment :: Core.Maybe Types.TransitGatewayPrefixListAttachment,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Core.Maybe Types.TransitGatewayRouteTableId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayPrefixListReference' value with any optional fields omitted.
mkTransitGatewayPrefixListReference ::
  TransitGatewayPrefixListReference
mkTransitGatewayPrefixListReference =
  TransitGatewayPrefixListReference'
    { blackhole = Core.Nothing,
      prefixListId = Core.Nothing,
      prefixListOwnerId = Core.Nothing,
      state = Core.Nothing,
      transitGatewayAttachment = Core.Nothing,
      transitGatewayRouteTableId = Core.Nothing
    }

-- | Indicates whether traffic that matches this route is dropped.
--
-- /Note:/ Consider using 'blackhole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgplrBlackhole :: Lens.Lens' TransitGatewayPrefixListReference (Core.Maybe Core.Bool)
tgplrBlackhole = Lens.field @"blackhole"
{-# DEPRECATED tgplrBlackhole "Use generic-lens or generic-optics with 'blackhole' instead." #-}

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgplrPrefixListId :: Lens.Lens' TransitGatewayPrefixListReference (Core.Maybe Types.PrefixListResourceId)
tgplrPrefixListId = Lens.field @"prefixListId"
{-# DEPRECATED tgplrPrefixListId "Use generic-lens or generic-optics with 'prefixListId' instead." #-}

-- | The ID of the prefix list owner.
--
-- /Note:/ Consider using 'prefixListOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgplrPrefixListOwnerId :: Lens.Lens' TransitGatewayPrefixListReference (Core.Maybe Types.String)
tgplrPrefixListOwnerId = Lens.field @"prefixListOwnerId"
{-# DEPRECATED tgplrPrefixListOwnerId "Use generic-lens or generic-optics with 'prefixListOwnerId' instead." #-}

-- | The state of the prefix list reference.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgplrState :: Lens.Lens' TransitGatewayPrefixListReference (Core.Maybe Types.TransitGatewayPrefixListReferenceState)
tgplrState = Lens.field @"state"
{-# DEPRECATED tgplrState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Information about the transit gateway attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgplrTransitGatewayAttachment :: Lens.Lens' TransitGatewayPrefixListReference (Core.Maybe Types.TransitGatewayPrefixListAttachment)
tgplrTransitGatewayAttachment = Lens.field @"transitGatewayAttachment"
{-# DEPRECATED tgplrTransitGatewayAttachment "Use generic-lens or generic-optics with 'transitGatewayAttachment' instead." #-}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgplrTransitGatewayRouteTableId :: Lens.Lens' TransitGatewayPrefixListReference (Core.Maybe Types.TransitGatewayRouteTableId)
tgplrTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# DEPRECATED tgplrTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

instance Core.FromXML TransitGatewayPrefixListReference where
  parseXML x =
    TransitGatewayPrefixListReference'
      Core.<$> (x Core..@? "blackhole")
      Core.<*> (x Core..@? "prefixListId")
      Core.<*> (x Core..@? "prefixListOwnerId")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "transitGatewayAttachment")
      Core.<*> (x Core..@? "transitGatewayRouteTableId")
