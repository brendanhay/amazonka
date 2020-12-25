{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RouteTableAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RouteTableAssociation
  ( RouteTableAssociation (..),

    -- * Smart constructor
    mkRouteTableAssociation,

    -- * Lenses
    rtaAssociationState,
    rtaGatewayId,
    rtaMain,
    rtaRouteTableAssociationId,
    rtaRouteTableId,
    rtaSubnetId,
  )
where

import qualified Network.AWS.EC2.Types.RouteTableAssociationState as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an association between a route table and a subnet or gateway.
--
-- /See:/ 'mkRouteTableAssociation' smart constructor.
data RouteTableAssociation = RouteTableAssociation'
  { -- | The state of the association.
    associationState :: Core.Maybe Types.RouteTableAssociationState,
    -- | The ID of the internet gateway or virtual private gateway.
    gatewayId :: Core.Maybe Types.String,
    -- | Indicates whether this is the main route table.
    main :: Core.Maybe Core.Bool,
    -- | The ID of the association.
    routeTableAssociationId :: Core.Maybe Types.String,
    -- | The ID of the route table.
    routeTableId :: Core.Maybe Types.String,
    -- | The ID of the subnet. A subnet ID is not returned for an implicit association.
    subnetId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RouteTableAssociation' value with any optional fields omitted.
mkRouteTableAssociation ::
  RouteTableAssociation
mkRouteTableAssociation =
  RouteTableAssociation'
    { associationState = Core.Nothing,
      gatewayId = Core.Nothing,
      main = Core.Nothing,
      routeTableAssociationId = Core.Nothing,
      routeTableId = Core.Nothing,
      subnetId = Core.Nothing
    }

-- | The state of the association.
--
-- /Note:/ Consider using 'associationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaAssociationState :: Lens.Lens' RouteTableAssociation (Core.Maybe Types.RouteTableAssociationState)
rtaAssociationState = Lens.field @"associationState"
{-# DEPRECATED rtaAssociationState "Use generic-lens or generic-optics with 'associationState' instead." #-}

-- | The ID of the internet gateway or virtual private gateway.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaGatewayId :: Lens.Lens' RouteTableAssociation (Core.Maybe Types.String)
rtaGatewayId = Lens.field @"gatewayId"
{-# DEPRECATED rtaGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

-- | Indicates whether this is the main route table.
--
-- /Note:/ Consider using 'main' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaMain :: Lens.Lens' RouteTableAssociation (Core.Maybe Core.Bool)
rtaMain = Lens.field @"main"
{-# DEPRECATED rtaMain "Use generic-lens or generic-optics with 'main' instead." #-}

-- | The ID of the association.
--
-- /Note:/ Consider using 'routeTableAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaRouteTableAssociationId :: Lens.Lens' RouteTableAssociation (Core.Maybe Types.String)
rtaRouteTableAssociationId = Lens.field @"routeTableAssociationId"
{-# DEPRECATED rtaRouteTableAssociationId "Use generic-lens or generic-optics with 'routeTableAssociationId' instead." #-}

-- | The ID of the route table.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaRouteTableId :: Lens.Lens' RouteTableAssociation (Core.Maybe Types.String)
rtaRouteTableId = Lens.field @"routeTableId"
{-# DEPRECATED rtaRouteTableId "Use generic-lens or generic-optics with 'routeTableId' instead." #-}

-- | The ID of the subnet. A subnet ID is not returned for an implicit association.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtaSubnetId :: Lens.Lens' RouteTableAssociation (Core.Maybe Types.String)
rtaSubnetId = Lens.field @"subnetId"
{-# DEPRECATED rtaSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

instance Core.FromXML RouteTableAssociation where
  parseXML x =
    RouteTableAssociation'
      Core.<$> (x Core..@? "associationState")
      Core.<*> (x Core..@? "gatewayId")
      Core.<*> (x Core..@? "main")
      Core.<*> (x Core..@? "routeTableAssociationId")
      Core.<*> (x Core..@? "routeTableId")
      Core.<*> (x Core..@? "subnetId")
