{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RouteTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RouteTable where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PropagatingVgw
import Network.AWS.EC2.Types.Route
import Network.AWS.EC2.Types.RouteTableAssociation
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes a route table.
--
-- /See:/ 'newRouteTable' smart constructor.
data RouteTable = RouteTable'
  { -- | The ID of the AWS account that owns the route table.
    ownerId :: Core.Maybe Core.Text,
    -- | The ID of the route table.
    routeTableId :: Core.Maybe Core.Text,
    -- | The routes in the route table.
    routes :: Core.Maybe [Route],
    -- | Any tags assigned to the route table.
    tags :: Core.Maybe [Tag],
    -- | Any virtual private gateway (VGW) propagating routes.
    propagatingVgws :: Core.Maybe [PropagatingVgw],
    -- | The ID of the VPC.
    vpcId :: Core.Maybe Core.Text,
    -- | The associations between the route table and one or more subnets or a
    -- gateway.
    associations :: Core.Maybe [RouteTableAssociation]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RouteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'routeTable_ownerId' - The ID of the AWS account that owns the route table.
--
-- 'routeTableId', 'routeTable_routeTableId' - The ID of the route table.
--
-- 'routes', 'routeTable_routes' - The routes in the route table.
--
-- 'tags', 'routeTable_tags' - Any tags assigned to the route table.
--
-- 'propagatingVgws', 'routeTable_propagatingVgws' - Any virtual private gateway (VGW) propagating routes.
--
-- 'vpcId', 'routeTable_vpcId' - The ID of the VPC.
--
-- 'associations', 'routeTable_associations' - The associations between the route table and one or more subnets or a
-- gateway.
newRouteTable ::
  RouteTable
newRouteTable =
  RouteTable'
    { ownerId = Core.Nothing,
      routeTableId = Core.Nothing,
      routes = Core.Nothing,
      tags = Core.Nothing,
      propagatingVgws = Core.Nothing,
      vpcId = Core.Nothing,
      associations = Core.Nothing
    }

-- | The ID of the AWS account that owns the route table.
routeTable_ownerId :: Lens.Lens' RouteTable (Core.Maybe Core.Text)
routeTable_ownerId = Lens.lens (\RouteTable' {ownerId} -> ownerId) (\s@RouteTable' {} a -> s {ownerId = a} :: RouteTable)

-- | The ID of the route table.
routeTable_routeTableId :: Lens.Lens' RouteTable (Core.Maybe Core.Text)
routeTable_routeTableId = Lens.lens (\RouteTable' {routeTableId} -> routeTableId) (\s@RouteTable' {} a -> s {routeTableId = a} :: RouteTable)

-- | The routes in the route table.
routeTable_routes :: Lens.Lens' RouteTable (Core.Maybe [Route])
routeTable_routes = Lens.lens (\RouteTable' {routes} -> routes) (\s@RouteTable' {} a -> s {routes = a} :: RouteTable) Core.. Lens.mapping Lens._Coerce

-- | Any tags assigned to the route table.
routeTable_tags :: Lens.Lens' RouteTable (Core.Maybe [Tag])
routeTable_tags = Lens.lens (\RouteTable' {tags} -> tags) (\s@RouteTable' {} a -> s {tags = a} :: RouteTable) Core.. Lens.mapping Lens._Coerce

-- | Any virtual private gateway (VGW) propagating routes.
routeTable_propagatingVgws :: Lens.Lens' RouteTable (Core.Maybe [PropagatingVgw])
routeTable_propagatingVgws = Lens.lens (\RouteTable' {propagatingVgws} -> propagatingVgws) (\s@RouteTable' {} a -> s {propagatingVgws = a} :: RouteTable) Core.. Lens.mapping Lens._Coerce

-- | The ID of the VPC.
routeTable_vpcId :: Lens.Lens' RouteTable (Core.Maybe Core.Text)
routeTable_vpcId = Lens.lens (\RouteTable' {vpcId} -> vpcId) (\s@RouteTable' {} a -> s {vpcId = a} :: RouteTable)

-- | The associations between the route table and one or more subnets or a
-- gateway.
routeTable_associations :: Lens.Lens' RouteTable (Core.Maybe [RouteTableAssociation])
routeTable_associations = Lens.lens (\RouteTable' {associations} -> associations) (\s@RouteTable' {} a -> s {associations = a} :: RouteTable) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML RouteTable where
  parseXML x =
    RouteTable'
      Core.<$> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "routeTableId")
      Core.<*> ( x Core..@? "routeSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "propagatingVgwSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "vpcId")
      Core.<*> ( x Core..@? "associationSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable RouteTable

instance Core.NFData RouteTable
