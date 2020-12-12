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
    rtRouteTableId,
    rtRoutes,
    rtVPCId,
    rtPropagatingVGWs,
    rtOwnerId,
    rtAssociations,
    rtTags,
  )
where

import Network.AWS.EC2.Types.PropagatingVGW
import Network.AWS.EC2.Types.Route
import Network.AWS.EC2.Types.RouteTableAssociation
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a route table.
--
-- /See:/ 'mkRouteTable' smart constructor.
data RouteTable = RouteTable'
  { routeTableId :: Lude.Maybe Lude.Text,
    routes :: Lude.Maybe [Route],
    vpcId :: Lude.Maybe Lude.Text,
    propagatingVGWs :: Lude.Maybe [PropagatingVGW],
    ownerId :: Lude.Maybe Lude.Text,
    associations :: Lude.Maybe [RouteTableAssociation],
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RouteTable' with the minimum fields required to make a request.
--
-- * 'associations' - The associations between the route table and one or more subnets or a gateway.
-- * 'ownerId' - The ID of the AWS account that owns the route table.
-- * 'propagatingVGWs' - Any virtual private gateway (VGW) propagating routes.
-- * 'routeTableId' - The ID of the route table.
-- * 'routes' - The routes in the route table.
-- * 'tags' - Any tags assigned to the route table.
-- * 'vpcId' - The ID of the VPC.
mkRouteTable ::
  RouteTable
mkRouteTable =
  RouteTable'
    { routeTableId = Lude.Nothing,
      routes = Lude.Nothing,
      vpcId = Lude.Nothing,
      propagatingVGWs = Lude.Nothing,
      ownerId = Lude.Nothing,
      associations = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The ID of the route table.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtRouteTableId :: Lens.Lens' RouteTable (Lude.Maybe Lude.Text)
rtRouteTableId = Lens.lens (routeTableId :: RouteTable -> Lude.Maybe Lude.Text) (\s a -> s {routeTableId = a} :: RouteTable)
{-# DEPRECATED rtRouteTableId "Use generic-lens or generic-optics with 'routeTableId' instead." #-}

-- | The routes in the route table.
--
-- /Note:/ Consider using 'routes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtRoutes :: Lens.Lens' RouteTable (Lude.Maybe [Route])
rtRoutes = Lens.lens (routes :: RouteTable -> Lude.Maybe [Route]) (\s a -> s {routes = a} :: RouteTable)
{-# DEPRECATED rtRoutes "Use generic-lens or generic-optics with 'routes' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtVPCId :: Lens.Lens' RouteTable (Lude.Maybe Lude.Text)
rtVPCId = Lens.lens (vpcId :: RouteTable -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: RouteTable)
{-# DEPRECATED rtVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Any virtual private gateway (VGW) propagating routes.
--
-- /Note:/ Consider using 'propagatingVGWs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtPropagatingVGWs :: Lens.Lens' RouteTable (Lude.Maybe [PropagatingVGW])
rtPropagatingVGWs = Lens.lens (propagatingVGWs :: RouteTable -> Lude.Maybe [PropagatingVGW]) (\s a -> s {propagatingVGWs = a} :: RouteTable)
{-# DEPRECATED rtPropagatingVGWs "Use generic-lens or generic-optics with 'propagatingVGWs' instead." #-}

-- | The ID of the AWS account that owns the route table.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtOwnerId :: Lens.Lens' RouteTable (Lude.Maybe Lude.Text)
rtOwnerId = Lens.lens (ownerId :: RouteTable -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: RouteTable)
{-# DEPRECATED rtOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The associations between the route table and one or more subnets or a gateway.
--
-- /Note:/ Consider using 'associations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtAssociations :: Lens.Lens' RouteTable (Lude.Maybe [RouteTableAssociation])
rtAssociations = Lens.lens (associations :: RouteTable -> Lude.Maybe [RouteTableAssociation]) (\s a -> s {associations = a} :: RouteTable)
{-# DEPRECATED rtAssociations "Use generic-lens or generic-optics with 'associations' instead." #-}

-- | Any tags assigned to the route table.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTags :: Lens.Lens' RouteTable (Lude.Maybe [Tag])
rtTags = Lens.lens (tags :: RouteTable -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: RouteTable)
{-# DEPRECATED rtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML RouteTable where
  parseXML x =
    RouteTable'
      Lude.<$> (x Lude..@? "routeTableId")
      Lude.<*> ( x Lude..@? "routeSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "vpcId")
      Lude.<*> ( x Lude..@? "propagatingVgwSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> ( x Lude..@? "associationSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
