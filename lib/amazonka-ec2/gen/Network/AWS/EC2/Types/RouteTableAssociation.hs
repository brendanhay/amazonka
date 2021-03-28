{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RouteTableAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.RouteTableAssociation
  ( RouteTableAssociation (..)
  -- * Smart constructor
  , mkRouteTableAssociation
  -- * Lenses
  , rtaAssociationState
  , rtaGatewayId
  , rtaMain
  , rtaRouteTableAssociationId
  , rtaRouteTableId
  , rtaSubnetId
  ) where

import qualified Network.AWS.EC2.Types.RouteTableAssociationState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an association between a route table and a subnet or gateway.
--
-- /See:/ 'mkRouteTableAssociation' smart constructor.
data RouteTableAssociation = RouteTableAssociation'
  { associationState :: Core.Maybe Types.RouteTableAssociationState
    -- ^ The state of the association.
  , gatewayId :: Core.Maybe Core.Text
    -- ^ The ID of the internet gateway or virtual private gateway.
  , main :: Core.Maybe Core.Bool
    -- ^ Indicates whether this is the main route table.
  , routeTableAssociationId :: Core.Maybe Core.Text
    -- ^ The ID of the association.
  , routeTableId :: Core.Maybe Core.Text
    -- ^ The ID of the route table.
  , subnetId :: Core.Maybe Core.Text
    -- ^ The ID of the subnet. A subnet ID is not returned for an implicit association.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RouteTableAssociation' value with any optional fields omitted.
mkRouteTableAssociation
    :: RouteTableAssociation
mkRouteTableAssociation
  = RouteTableAssociation'{associationState = Core.Nothing,
                           gatewayId = Core.Nothing, main = Core.Nothing,
                           routeTableAssociationId = Core.Nothing,
                           routeTableId = Core.Nothing, subnetId = Core.Nothing}

-- | The state of the association.
--
-- /Note:/ Consider using 'associationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaAssociationState :: Lens.Lens' RouteTableAssociation (Core.Maybe Types.RouteTableAssociationState)
rtaAssociationState = Lens.field @"associationState"
{-# INLINEABLE rtaAssociationState #-}
{-# DEPRECATED associationState "Use generic-lens or generic-optics with 'associationState' instead"  #-}

-- | The ID of the internet gateway or virtual private gateway.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaGatewayId :: Lens.Lens' RouteTableAssociation (Core.Maybe Core.Text)
rtaGatewayId = Lens.field @"gatewayId"
{-# INLINEABLE rtaGatewayId #-}
{-# DEPRECATED gatewayId "Use generic-lens or generic-optics with 'gatewayId' instead"  #-}

-- | Indicates whether this is the main route table.
--
-- /Note:/ Consider using 'main' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaMain :: Lens.Lens' RouteTableAssociation (Core.Maybe Core.Bool)
rtaMain = Lens.field @"main"
{-# INLINEABLE rtaMain #-}
{-# DEPRECATED main "Use generic-lens or generic-optics with 'main' instead"  #-}

-- | The ID of the association.
--
-- /Note:/ Consider using 'routeTableAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaRouteTableAssociationId :: Lens.Lens' RouteTableAssociation (Core.Maybe Core.Text)
rtaRouteTableAssociationId = Lens.field @"routeTableAssociationId"
{-# INLINEABLE rtaRouteTableAssociationId #-}
{-# DEPRECATED routeTableAssociationId "Use generic-lens or generic-optics with 'routeTableAssociationId' instead"  #-}

-- | The ID of the route table.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaRouteTableId :: Lens.Lens' RouteTableAssociation (Core.Maybe Core.Text)
rtaRouteTableId = Lens.field @"routeTableId"
{-# INLINEABLE rtaRouteTableId #-}
{-# DEPRECATED routeTableId "Use generic-lens or generic-optics with 'routeTableId' instead"  #-}

-- | The ID of the subnet. A subnet ID is not returned for an implicit association.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaSubnetId :: Lens.Lens' RouteTableAssociation (Core.Maybe Core.Text)
rtaSubnetId = Lens.field @"subnetId"
{-# INLINEABLE rtaSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

instance Core.FromXML RouteTableAssociation where
        parseXML x
          = RouteTableAssociation' Core.<$>
              (x Core..@? "associationState") Core.<*> x Core..@? "gatewayId"
                Core.<*> x Core..@? "main"
                Core.<*> x Core..@? "routeTableAssociationId"
                Core.<*> x Core..@? "routeTableId"
                Core.<*> x Core..@? "subnetId"
