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
-- Module      : Amazonka.EC2.Types.RouteTableAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.RouteTableAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.RouteTableAssociationState
import qualified Amazonka.Prelude as Prelude

-- | Describes an association between a route table and a subnet or gateway.
--
-- /See:/ 'newRouteTableAssociation' smart constructor.
data RouteTableAssociation = RouteTableAssociation'
  { -- | The state of the association.
    associationState :: Prelude.Maybe RouteTableAssociationState,
    -- | The ID of the internet gateway or virtual private gateway.
    gatewayId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether this is the main route table.
    main :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the association.
    routeTableAssociationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the route table.
    routeTableId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet. A subnet ID is not returned for an implicit
    -- association.
    subnetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouteTableAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationState', 'routeTableAssociation_associationState' - The state of the association.
--
-- 'gatewayId', 'routeTableAssociation_gatewayId' - The ID of the internet gateway or virtual private gateway.
--
-- 'main', 'routeTableAssociation_main' - Indicates whether this is the main route table.
--
-- 'routeTableAssociationId', 'routeTableAssociation_routeTableAssociationId' - The ID of the association.
--
-- 'routeTableId', 'routeTableAssociation_routeTableId' - The ID of the route table.
--
-- 'subnetId', 'routeTableAssociation_subnetId' - The ID of the subnet. A subnet ID is not returned for an implicit
-- association.
newRouteTableAssociation ::
  RouteTableAssociation
newRouteTableAssociation =
  RouteTableAssociation'
    { associationState =
        Prelude.Nothing,
      gatewayId = Prelude.Nothing,
      main = Prelude.Nothing,
      routeTableAssociationId = Prelude.Nothing,
      routeTableId = Prelude.Nothing,
      subnetId = Prelude.Nothing
    }

-- | The state of the association.
routeTableAssociation_associationState :: Lens.Lens' RouteTableAssociation (Prelude.Maybe RouteTableAssociationState)
routeTableAssociation_associationState = Lens.lens (\RouteTableAssociation' {associationState} -> associationState) (\s@RouteTableAssociation' {} a -> s {associationState = a} :: RouteTableAssociation)

-- | The ID of the internet gateway or virtual private gateway.
routeTableAssociation_gatewayId :: Lens.Lens' RouteTableAssociation (Prelude.Maybe Prelude.Text)
routeTableAssociation_gatewayId = Lens.lens (\RouteTableAssociation' {gatewayId} -> gatewayId) (\s@RouteTableAssociation' {} a -> s {gatewayId = a} :: RouteTableAssociation)

-- | Indicates whether this is the main route table.
routeTableAssociation_main :: Lens.Lens' RouteTableAssociation (Prelude.Maybe Prelude.Bool)
routeTableAssociation_main = Lens.lens (\RouteTableAssociation' {main} -> main) (\s@RouteTableAssociation' {} a -> s {main = a} :: RouteTableAssociation)

-- | The ID of the association.
routeTableAssociation_routeTableAssociationId :: Lens.Lens' RouteTableAssociation (Prelude.Maybe Prelude.Text)
routeTableAssociation_routeTableAssociationId = Lens.lens (\RouteTableAssociation' {routeTableAssociationId} -> routeTableAssociationId) (\s@RouteTableAssociation' {} a -> s {routeTableAssociationId = a} :: RouteTableAssociation)

-- | The ID of the route table.
routeTableAssociation_routeTableId :: Lens.Lens' RouteTableAssociation (Prelude.Maybe Prelude.Text)
routeTableAssociation_routeTableId = Lens.lens (\RouteTableAssociation' {routeTableId} -> routeTableId) (\s@RouteTableAssociation' {} a -> s {routeTableId = a} :: RouteTableAssociation)

-- | The ID of the subnet. A subnet ID is not returned for an implicit
-- association.
routeTableAssociation_subnetId :: Lens.Lens' RouteTableAssociation (Prelude.Maybe Prelude.Text)
routeTableAssociation_subnetId = Lens.lens (\RouteTableAssociation' {subnetId} -> subnetId) (\s@RouteTableAssociation' {} a -> s {subnetId = a} :: RouteTableAssociation)

instance Data.FromXML RouteTableAssociation where
  parseXML x =
    RouteTableAssociation'
      Prelude.<$> (x Data..@? "associationState")
      Prelude.<*> (x Data..@? "gatewayId")
      Prelude.<*> (x Data..@? "main")
      Prelude.<*> (x Data..@? "routeTableAssociationId")
      Prelude.<*> (x Data..@? "routeTableId")
      Prelude.<*> (x Data..@? "subnetId")

instance Prelude.Hashable RouteTableAssociation where
  hashWithSalt _salt RouteTableAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` associationState
      `Prelude.hashWithSalt` gatewayId
      `Prelude.hashWithSalt` main
      `Prelude.hashWithSalt` routeTableAssociationId
      `Prelude.hashWithSalt` routeTableId
      `Prelude.hashWithSalt` subnetId

instance Prelude.NFData RouteTableAssociation where
  rnf RouteTableAssociation' {..} =
    Prelude.rnf associationState
      `Prelude.seq` Prelude.rnf gatewayId
      `Prelude.seq` Prelude.rnf main
      `Prelude.seq` Prelude.rnf routeTableAssociationId
      `Prelude.seq` Prelude.rnf routeTableId
      `Prelude.seq` Prelude.rnf subnetId
