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
import qualified Network.AWS.Prelude as Prelude

-- | Describes a route table.
--
-- /See:/ 'newRouteTable' smart constructor.
data RouteTable = RouteTable'
  { -- | The ID of the route table.
    routeTableId :: Prelude.Maybe Prelude.Text,
    -- | The routes in the route table.
    routes :: Prelude.Maybe [Route],
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | Any virtual private gateway (VGW) propagating routes.
    propagatingVgws :: Prelude.Maybe [PropagatingVgw],
    -- | The ID of the Amazon Web Services account that owns the route table.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The associations between the route table and one or more subnets or a
    -- gateway.
    associations :: Prelude.Maybe [RouteTableAssociation],
    -- | Any tags assigned to the route table.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routeTableId', 'routeTable_routeTableId' - The ID of the route table.
--
-- 'routes', 'routeTable_routes' - The routes in the route table.
--
-- 'vpcId', 'routeTable_vpcId' - The ID of the VPC.
--
-- 'propagatingVgws', 'routeTable_propagatingVgws' - Any virtual private gateway (VGW) propagating routes.
--
-- 'ownerId', 'routeTable_ownerId' - The ID of the Amazon Web Services account that owns the route table.
--
-- 'associations', 'routeTable_associations' - The associations between the route table and one or more subnets or a
-- gateway.
--
-- 'tags', 'routeTable_tags' - Any tags assigned to the route table.
newRouteTable ::
  RouteTable
newRouteTable =
  RouteTable'
    { routeTableId = Prelude.Nothing,
      routes = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      propagatingVgws = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      associations = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ID of the route table.
routeTable_routeTableId :: Lens.Lens' RouteTable (Prelude.Maybe Prelude.Text)
routeTable_routeTableId = Lens.lens (\RouteTable' {routeTableId} -> routeTableId) (\s@RouteTable' {} a -> s {routeTableId = a} :: RouteTable)

-- | The routes in the route table.
routeTable_routes :: Lens.Lens' RouteTable (Prelude.Maybe [Route])
routeTable_routes = Lens.lens (\RouteTable' {routes} -> routes) (\s@RouteTable' {} a -> s {routes = a} :: RouteTable) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC.
routeTable_vpcId :: Lens.Lens' RouteTable (Prelude.Maybe Prelude.Text)
routeTable_vpcId = Lens.lens (\RouteTable' {vpcId} -> vpcId) (\s@RouteTable' {} a -> s {vpcId = a} :: RouteTable)

-- | Any virtual private gateway (VGW) propagating routes.
routeTable_propagatingVgws :: Lens.Lens' RouteTable (Prelude.Maybe [PropagatingVgw])
routeTable_propagatingVgws = Lens.lens (\RouteTable' {propagatingVgws} -> propagatingVgws) (\s@RouteTable' {} a -> s {propagatingVgws = a} :: RouteTable) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services account that owns the route table.
routeTable_ownerId :: Lens.Lens' RouteTable (Prelude.Maybe Prelude.Text)
routeTable_ownerId = Lens.lens (\RouteTable' {ownerId} -> ownerId) (\s@RouteTable' {} a -> s {ownerId = a} :: RouteTable)

-- | The associations between the route table and one or more subnets or a
-- gateway.
routeTable_associations :: Lens.Lens' RouteTable (Prelude.Maybe [RouteTableAssociation])
routeTable_associations = Lens.lens (\RouteTable' {associations} -> associations) (\s@RouteTable' {} a -> s {associations = a} :: RouteTable) Prelude.. Lens.mapping Lens.coerced

-- | Any tags assigned to the route table.
routeTable_tags :: Lens.Lens' RouteTable (Prelude.Maybe [Tag])
routeTable_tags = Lens.lens (\RouteTable' {tags} -> tags) (\s@RouteTable' {} a -> s {tags = a} :: RouteTable) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML RouteTable where
  parseXML x =
    RouteTable'
      Prelude.<$> (x Core..@? "routeTableId")
      Prelude.<*> ( x Core..@? "routeSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "vpcId")
      Prelude.<*> ( x Core..@? "propagatingVgwSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "ownerId")
      Prelude.<*> ( x Core..@? "associationSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable RouteTable

instance Prelude.NFData RouteTable
