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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.RouteTableAssociationState
import qualified Network.AWS.Lens as Lens

-- | Describes an association between a route table and a subnet or gateway.
--
-- /See:/ 'newRouteTableAssociation' smart constructor.
data RouteTableAssociation = RouteTableAssociation'
  { -- | The state of the association.
    associationState :: Core.Maybe RouteTableAssociationState,
    -- | Indicates whether this is the main route table.
    main :: Core.Maybe Core.Bool,
    -- | The ID of the route table.
    routeTableId :: Core.Maybe Core.Text,
    -- | The ID of the subnet. A subnet ID is not returned for an implicit
    -- association.
    subnetId :: Core.Maybe Core.Text,
    -- | The ID of the association.
    routeTableAssociationId :: Core.Maybe Core.Text,
    -- | The ID of the internet gateway or virtual private gateway.
    gatewayId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      main = Core.Nothing,
      routeTableId = Core.Nothing,
      subnetId = Core.Nothing,
      routeTableAssociationId = Core.Nothing,
      gatewayId = Core.Nothing
    }

-- | The state of the association.
routeTableAssociation_associationState :: Lens.Lens' RouteTableAssociation (Core.Maybe RouteTableAssociationState)
routeTableAssociation_associationState = Lens.lens (\RouteTableAssociation' {associationState} -> associationState) (\s@RouteTableAssociation' {} a -> s {associationState = a} :: RouteTableAssociation)

-- | Indicates whether this is the main route table.
routeTableAssociation_main :: Lens.Lens' RouteTableAssociation (Core.Maybe Core.Bool)
routeTableAssociation_main = Lens.lens (\RouteTableAssociation' {main} -> main) (\s@RouteTableAssociation' {} a -> s {main = a} :: RouteTableAssociation)

-- | The ID of the route table.
routeTableAssociation_routeTableId :: Lens.Lens' RouteTableAssociation (Core.Maybe Core.Text)
routeTableAssociation_routeTableId = Lens.lens (\RouteTableAssociation' {routeTableId} -> routeTableId) (\s@RouteTableAssociation' {} a -> s {routeTableId = a} :: RouteTableAssociation)

-- | The ID of the subnet. A subnet ID is not returned for an implicit
-- association.
routeTableAssociation_subnetId :: Lens.Lens' RouteTableAssociation (Core.Maybe Core.Text)
routeTableAssociation_subnetId = Lens.lens (\RouteTableAssociation' {subnetId} -> subnetId) (\s@RouteTableAssociation' {} a -> s {subnetId = a} :: RouteTableAssociation)

-- | The ID of the association.
routeTableAssociation_routeTableAssociationId :: Lens.Lens' RouteTableAssociation (Core.Maybe Core.Text)
routeTableAssociation_routeTableAssociationId = Lens.lens (\RouteTableAssociation' {routeTableAssociationId} -> routeTableAssociationId) (\s@RouteTableAssociation' {} a -> s {routeTableAssociationId = a} :: RouteTableAssociation)

-- | The ID of the internet gateway or virtual private gateway.
routeTableAssociation_gatewayId :: Lens.Lens' RouteTableAssociation (Core.Maybe Core.Text)
routeTableAssociation_gatewayId = Lens.lens (\RouteTableAssociation' {gatewayId} -> gatewayId) (\s@RouteTableAssociation' {} a -> s {gatewayId = a} :: RouteTableAssociation)

instance Core.FromXML RouteTableAssociation where
  parseXML x =
    RouteTableAssociation'
      Core.<$> (x Core..@? "associationState")
      Core.<*> (x Core..@? "main")
      Core.<*> (x Core..@? "routeTableId")
      Core.<*> (x Core..@? "subnetId")
      Core.<*> (x Core..@? "routeTableAssociationId")
      Core.<*> (x Core..@? "gatewayId")

instance Core.Hashable RouteTableAssociation

instance Core.NFData RouteTableAssociation
