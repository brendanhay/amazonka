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
-- Module      : Amazonka.EC2.Types.RouteTable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.RouteTable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PropagatingVgw
import Amazonka.EC2.Types.Route
import Amazonka.EC2.Types.RouteTableAssociation
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a route table.
--
-- /See:/ 'newRouteTable' smart constructor.
data RouteTable = RouteTable'
  { -- | The associations between the route table and one or more subnets or a
    -- gateway.
    associations :: Prelude.Maybe [RouteTableAssociation],
    -- | The ID of the Amazon Web Services account that owns the route table.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Any virtual private gateway (VGW) propagating routes.
    propagatingVgws :: Prelude.Maybe [PropagatingVgw],
    -- | The ID of the route table.
    routeTableId :: Prelude.Maybe Prelude.Text,
    -- | The routes in the route table.
    routes :: Prelude.Maybe [Route],
    -- | Any tags assigned to the route table.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text
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
-- 'associations', 'routeTable_associations' - The associations between the route table and one or more subnets or a
-- gateway.
--
-- 'ownerId', 'routeTable_ownerId' - The ID of the Amazon Web Services account that owns the route table.
--
-- 'propagatingVgws', 'routeTable_propagatingVgws' - Any virtual private gateway (VGW) propagating routes.
--
-- 'routeTableId', 'routeTable_routeTableId' - The ID of the route table.
--
-- 'routes', 'routeTable_routes' - The routes in the route table.
--
-- 'tags', 'routeTable_tags' - Any tags assigned to the route table.
--
-- 'vpcId', 'routeTable_vpcId' - The ID of the VPC.
newRouteTable ::
  RouteTable
newRouteTable =
  RouteTable'
    { associations = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      propagatingVgws = Prelude.Nothing,
      routeTableId = Prelude.Nothing,
      routes = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The associations between the route table and one or more subnets or a
-- gateway.
routeTable_associations :: Lens.Lens' RouteTable (Prelude.Maybe [RouteTableAssociation])
routeTable_associations = Lens.lens (\RouteTable' {associations} -> associations) (\s@RouteTable' {} a -> s {associations = a} :: RouteTable) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services account that owns the route table.
routeTable_ownerId :: Lens.Lens' RouteTable (Prelude.Maybe Prelude.Text)
routeTable_ownerId = Lens.lens (\RouteTable' {ownerId} -> ownerId) (\s@RouteTable' {} a -> s {ownerId = a} :: RouteTable)

-- | Any virtual private gateway (VGW) propagating routes.
routeTable_propagatingVgws :: Lens.Lens' RouteTable (Prelude.Maybe [PropagatingVgw])
routeTable_propagatingVgws = Lens.lens (\RouteTable' {propagatingVgws} -> propagatingVgws) (\s@RouteTable' {} a -> s {propagatingVgws = a} :: RouteTable) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the route table.
routeTable_routeTableId :: Lens.Lens' RouteTable (Prelude.Maybe Prelude.Text)
routeTable_routeTableId = Lens.lens (\RouteTable' {routeTableId} -> routeTableId) (\s@RouteTable' {} a -> s {routeTableId = a} :: RouteTable)

-- | The routes in the route table.
routeTable_routes :: Lens.Lens' RouteTable (Prelude.Maybe [Route])
routeTable_routes = Lens.lens (\RouteTable' {routes} -> routes) (\s@RouteTable' {} a -> s {routes = a} :: RouteTable) Prelude.. Lens.mapping Lens.coerced

-- | Any tags assigned to the route table.
routeTable_tags :: Lens.Lens' RouteTable (Prelude.Maybe [Tag])
routeTable_tags = Lens.lens (\RouteTable' {tags} -> tags) (\s@RouteTable' {} a -> s {tags = a} :: RouteTable) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC.
routeTable_vpcId :: Lens.Lens' RouteTable (Prelude.Maybe Prelude.Text)
routeTable_vpcId = Lens.lens (\RouteTable' {vpcId} -> vpcId) (\s@RouteTable' {} a -> s {vpcId = a} :: RouteTable)

instance Data.FromXML RouteTable where
  parseXML x =
    RouteTable'
      Prelude.<$> ( x Data..@? "associationSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> ( x Data..@? "propagatingVgwSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "routeTableId")
      Prelude.<*> ( x Data..@? "routeSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "vpcId")

instance Prelude.Hashable RouteTable where
  hashWithSalt _salt RouteTable' {..} =
    _salt `Prelude.hashWithSalt` associations
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` propagatingVgws
      `Prelude.hashWithSalt` routeTableId
      `Prelude.hashWithSalt` routes
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData RouteTable where
  rnf RouteTable' {..} =
    Prelude.rnf associations
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf propagatingVgws
      `Prelude.seq` Prelude.rnf routeTableId
      `Prelude.seq` Prelude.rnf routes
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcId
