{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.RouteTableAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RouteTableAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.RouteTableAssociationState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an association between a route table and a subnet or gateway.
--
-- /See:/ 'newRouteTableAssociation' smart constructor.
data RouteTableAssociation = RouteTableAssociation'
  { -- | The state of the association.
    associationState :: Prelude.Maybe RouteTableAssociationState,
    -- | Indicates whether this is the main route table.
    main :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the route table.
    routeTableId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet. A subnet ID is not returned for an implicit
    -- association.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the association.
    routeTableAssociationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the internet gateway or virtual private gateway.
    gatewayId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'main', 'routeTableAssociation_main' - Indicates whether this is the main route table.
--
-- 'routeTableId', 'routeTableAssociation_routeTableId' - The ID of the route table.
--
-- 'subnetId', 'routeTableAssociation_subnetId' - The ID of the subnet. A subnet ID is not returned for an implicit
-- association.
--
-- 'routeTableAssociationId', 'routeTableAssociation_routeTableAssociationId' - The ID of the association.
--
-- 'gatewayId', 'routeTableAssociation_gatewayId' - The ID of the internet gateway or virtual private gateway.
newRouteTableAssociation ::
  RouteTableAssociation
newRouteTableAssociation =
  RouteTableAssociation'
    { associationState =
        Prelude.Nothing,
      main = Prelude.Nothing,
      routeTableId = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      routeTableAssociationId = Prelude.Nothing,
      gatewayId = Prelude.Nothing
    }

-- | The state of the association.
routeTableAssociation_associationState :: Lens.Lens' RouteTableAssociation (Prelude.Maybe RouteTableAssociationState)
routeTableAssociation_associationState = Lens.lens (\RouteTableAssociation' {associationState} -> associationState) (\s@RouteTableAssociation' {} a -> s {associationState = a} :: RouteTableAssociation)

-- | Indicates whether this is the main route table.
routeTableAssociation_main :: Lens.Lens' RouteTableAssociation (Prelude.Maybe Prelude.Bool)
routeTableAssociation_main = Lens.lens (\RouteTableAssociation' {main} -> main) (\s@RouteTableAssociation' {} a -> s {main = a} :: RouteTableAssociation)

-- | The ID of the route table.
routeTableAssociation_routeTableId :: Lens.Lens' RouteTableAssociation (Prelude.Maybe Prelude.Text)
routeTableAssociation_routeTableId = Lens.lens (\RouteTableAssociation' {routeTableId} -> routeTableId) (\s@RouteTableAssociation' {} a -> s {routeTableId = a} :: RouteTableAssociation)

-- | The ID of the subnet. A subnet ID is not returned for an implicit
-- association.
routeTableAssociation_subnetId :: Lens.Lens' RouteTableAssociation (Prelude.Maybe Prelude.Text)
routeTableAssociation_subnetId = Lens.lens (\RouteTableAssociation' {subnetId} -> subnetId) (\s@RouteTableAssociation' {} a -> s {subnetId = a} :: RouteTableAssociation)

-- | The ID of the association.
routeTableAssociation_routeTableAssociationId :: Lens.Lens' RouteTableAssociation (Prelude.Maybe Prelude.Text)
routeTableAssociation_routeTableAssociationId = Lens.lens (\RouteTableAssociation' {routeTableAssociationId} -> routeTableAssociationId) (\s@RouteTableAssociation' {} a -> s {routeTableAssociationId = a} :: RouteTableAssociation)

-- | The ID of the internet gateway or virtual private gateway.
routeTableAssociation_gatewayId :: Lens.Lens' RouteTableAssociation (Prelude.Maybe Prelude.Text)
routeTableAssociation_gatewayId = Lens.lens (\RouteTableAssociation' {gatewayId} -> gatewayId) (\s@RouteTableAssociation' {} a -> s {gatewayId = a} :: RouteTableAssociation)

instance Prelude.FromXML RouteTableAssociation where
  parseXML x =
    RouteTableAssociation'
      Prelude.<$> (x Prelude..@? "associationState")
      Prelude.<*> (x Prelude..@? "main")
      Prelude.<*> (x Prelude..@? "routeTableId")
      Prelude.<*> (x Prelude..@? "subnetId")
      Prelude.<*> (x Prelude..@? "routeTableAssociationId")
      Prelude.<*> (x Prelude..@? "gatewayId")

instance Prelude.Hashable RouteTableAssociation

instance Prelude.NFData RouteTableAssociation
