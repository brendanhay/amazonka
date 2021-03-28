{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayRouteTable
  ( TransitGatewayRouteTable (..)
  -- * Smart constructor
  , mkTransitGatewayRouteTable
  -- * Lenses
  , tgrtCreationTime
  , tgrtDefaultAssociationRouteTable
  , tgrtDefaultPropagationRouteTable
  , tgrtState
  , tgrtTags
  , tgrtTransitGatewayId
  , tgrtTransitGatewayRouteTableId
  ) where

import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.TransitGatewayRouteTableState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a transit gateway route table.
--
-- /See:/ 'mkTransitGatewayRouteTable' smart constructor.
data TransitGatewayRouteTable = TransitGatewayRouteTable'
  { creationTime :: Core.Maybe Core.UTCTime
    -- ^ The creation time.
  , defaultAssociationRouteTable :: Core.Maybe Core.Bool
    -- ^ Indicates whether this is the default association route table for the transit gateway.
  , defaultPropagationRouteTable :: Core.Maybe Core.Bool
    -- ^ Indicates whether this is the default propagation route table for the transit gateway.
  , state :: Core.Maybe Types.TransitGatewayRouteTableState
    -- ^ The state of the transit gateway route table.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the route table.
  , transitGatewayId :: Core.Maybe Core.Text
    -- ^ The ID of the transit gateway.
  , transitGatewayRouteTableId :: Core.Maybe Core.Text
    -- ^ The ID of the transit gateway route table.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TransitGatewayRouteTable' value with any optional fields omitted.
mkTransitGatewayRouteTable
    :: TransitGatewayRouteTable
mkTransitGatewayRouteTable
  = TransitGatewayRouteTable'{creationTime = Core.Nothing,
                              defaultAssociationRouteTable = Core.Nothing,
                              defaultPropagationRouteTable = Core.Nothing, state = Core.Nothing,
                              tags = Core.Nothing, transitGatewayId = Core.Nothing,
                              transitGatewayRouteTableId = Core.Nothing}

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtCreationTime :: Lens.Lens' TransitGatewayRouteTable (Core.Maybe Core.UTCTime)
tgrtCreationTime = Lens.field @"creationTime"
{-# INLINEABLE tgrtCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | Indicates whether this is the default association route table for the transit gateway.
--
-- /Note:/ Consider using 'defaultAssociationRouteTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtDefaultAssociationRouteTable :: Lens.Lens' TransitGatewayRouteTable (Core.Maybe Core.Bool)
tgrtDefaultAssociationRouteTable = Lens.field @"defaultAssociationRouteTable"
{-# INLINEABLE tgrtDefaultAssociationRouteTable #-}
{-# DEPRECATED defaultAssociationRouteTable "Use generic-lens or generic-optics with 'defaultAssociationRouteTable' instead"  #-}

-- | Indicates whether this is the default propagation route table for the transit gateway.
--
-- /Note:/ Consider using 'defaultPropagationRouteTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtDefaultPropagationRouteTable :: Lens.Lens' TransitGatewayRouteTable (Core.Maybe Core.Bool)
tgrtDefaultPropagationRouteTable = Lens.field @"defaultPropagationRouteTable"
{-# INLINEABLE tgrtDefaultPropagationRouteTable #-}
{-# DEPRECATED defaultPropagationRouteTable "Use generic-lens or generic-optics with 'defaultPropagationRouteTable' instead"  #-}

-- | The state of the transit gateway route table.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtState :: Lens.Lens' TransitGatewayRouteTable (Core.Maybe Types.TransitGatewayRouteTableState)
tgrtState = Lens.field @"state"
{-# INLINEABLE tgrtState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | Any tags assigned to the route table.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtTags :: Lens.Lens' TransitGatewayRouteTable (Core.Maybe [Types.Tag])
tgrtTags = Lens.field @"tags"
{-# INLINEABLE tgrtTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtTransitGatewayId :: Lens.Lens' TransitGatewayRouteTable (Core.Maybe Core.Text)
tgrtTransitGatewayId = Lens.field @"transitGatewayId"
{-# INLINEABLE tgrtTransitGatewayId #-}
{-# DEPRECATED transitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead"  #-}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgrtTransitGatewayRouteTableId :: Lens.Lens' TransitGatewayRouteTable (Core.Maybe Core.Text)
tgrtTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# INLINEABLE tgrtTransitGatewayRouteTableId #-}
{-# DEPRECATED transitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead"  #-}

instance Core.FromXML TransitGatewayRouteTable where
        parseXML x
          = TransitGatewayRouteTable' Core.<$>
              (x Core..@? "creationTime") Core.<*>
                x Core..@? "defaultAssociationRouteTable"
                Core.<*> x Core..@? "defaultPropagationRouteTable"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "transitGatewayId"
                Core.<*> x Core..@? "transitGatewayRouteTableId"
