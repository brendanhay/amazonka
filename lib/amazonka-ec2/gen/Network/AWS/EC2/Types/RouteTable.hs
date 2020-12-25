{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RouteTable
  ( RouteTable (..),

    -- * Smart constructor
    mkRouteTable,

    -- * Lenses
    rtAssociations,
    rtOwnerId,
    rtPropagatingVgws,
    rtRouteTableId,
    rtRoutes,
    rtTags,
    rtVpcId,
  )
where

import qualified Network.AWS.EC2.Types.PropagatingVgw as Types
import qualified Network.AWS.EC2.Types.Route as Types
import qualified Network.AWS.EC2.Types.RouteTableAssociation as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a route table.
--
-- /See:/ 'mkRouteTable' smart constructor.
data RouteTable = RouteTable'
  { -- | The associations between the route table and one or more subnets or a gateway.
    associations :: Core.Maybe [Types.RouteTableAssociation],
    -- | The ID of the AWS account that owns the route table.
    ownerId :: Core.Maybe Types.String,
    -- | Any virtual private gateway (VGW) propagating routes.
    propagatingVgws :: Core.Maybe [Types.PropagatingVgw],
    -- | The ID of the route table.
    routeTableId :: Core.Maybe Types.String,
    -- | The routes in the route table.
    routes :: Core.Maybe [Types.Route],
    -- | Any tags assigned to the route table.
    tags :: Core.Maybe [Types.Tag],
    -- | The ID of the VPC.
    vpcId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RouteTable' value with any optional fields omitted.
mkRouteTable ::
  RouteTable
mkRouteTable =
  RouteTable'
    { associations = Core.Nothing,
      ownerId = Core.Nothing,
      propagatingVgws = Core.Nothing,
      routeTableId = Core.Nothing,
      routes = Core.Nothing,
      tags = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The associations between the route table and one or more subnets or a gateway.
--
-- /Note:/ Consider using 'associations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtAssociations :: Lens.Lens' RouteTable (Core.Maybe [Types.RouteTableAssociation])
rtAssociations = Lens.field @"associations"
{-# DEPRECATED rtAssociations "Use generic-lens or generic-optics with 'associations' instead." #-}

-- | The ID of the AWS account that owns the route table.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtOwnerId :: Lens.Lens' RouteTable (Core.Maybe Types.String)
rtOwnerId = Lens.field @"ownerId"
{-# DEPRECATED rtOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | Any virtual private gateway (VGW) propagating routes.
--
-- /Note:/ Consider using 'propagatingVgws' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtPropagatingVgws :: Lens.Lens' RouteTable (Core.Maybe [Types.PropagatingVgw])
rtPropagatingVgws = Lens.field @"propagatingVgws"
{-# DEPRECATED rtPropagatingVgws "Use generic-lens or generic-optics with 'propagatingVgws' instead." #-}

-- | The ID of the route table.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtRouteTableId :: Lens.Lens' RouteTable (Core.Maybe Types.String)
rtRouteTableId = Lens.field @"routeTableId"
{-# DEPRECATED rtRouteTableId "Use generic-lens or generic-optics with 'routeTableId' instead." #-}

-- | The routes in the route table.
--
-- /Note:/ Consider using 'routes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtRoutes :: Lens.Lens' RouteTable (Core.Maybe [Types.Route])
rtRoutes = Lens.field @"routes"
{-# DEPRECATED rtRoutes "Use generic-lens or generic-optics with 'routes' instead." #-}

-- | Any tags assigned to the route table.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTags :: Lens.Lens' RouteTable (Core.Maybe [Types.Tag])
rtTags = Lens.field @"tags"
{-# DEPRECATED rtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtVpcId :: Lens.Lens' RouteTable (Core.Maybe Types.String)
rtVpcId = Lens.field @"vpcId"
{-# DEPRECATED rtVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromXML RouteTable where
  parseXML x =
    RouteTable'
      Core.<$> (x Core..@? "associationSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "propagatingVgwSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "routeTableId")
      Core.<*> (x Core..@? "routeSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "vpcId")
