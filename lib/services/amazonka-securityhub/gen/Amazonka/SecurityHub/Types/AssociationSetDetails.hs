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
-- Module      : Amazonka.SecurityHub.Types.AssociationSetDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AssociationSetDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AssociationStateDetails

-- | The associations between a route table and one or more subnets or a
-- gateway.
--
-- /See:/ 'newAssociationSetDetails' smart constructor.
data AssociationSetDetails = AssociationSetDetails'
  { -- | The state of the association between a route table and a subnet or
    -- gateway.
    associationState :: Prelude.Maybe AssociationStateDetails,
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
-- Create a value of 'AssociationSetDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationState', 'associationSetDetails_associationState' - The state of the association between a route table and a subnet or
-- gateway.
--
-- 'gatewayId', 'associationSetDetails_gatewayId' - The ID of the internet gateway or virtual private gateway.
--
-- 'main', 'associationSetDetails_main' - Indicates whether this is the main route table.
--
-- 'routeTableAssociationId', 'associationSetDetails_routeTableAssociationId' - The ID of the association.
--
-- 'routeTableId', 'associationSetDetails_routeTableId' - The ID of the route table.
--
-- 'subnetId', 'associationSetDetails_subnetId' - The ID of the subnet. A subnet ID is not returned for an implicit
-- association.
newAssociationSetDetails ::
  AssociationSetDetails
newAssociationSetDetails =
  AssociationSetDetails'
    { associationState =
        Prelude.Nothing,
      gatewayId = Prelude.Nothing,
      main = Prelude.Nothing,
      routeTableAssociationId = Prelude.Nothing,
      routeTableId = Prelude.Nothing,
      subnetId = Prelude.Nothing
    }

-- | The state of the association between a route table and a subnet or
-- gateway.
associationSetDetails_associationState :: Lens.Lens' AssociationSetDetails (Prelude.Maybe AssociationStateDetails)
associationSetDetails_associationState = Lens.lens (\AssociationSetDetails' {associationState} -> associationState) (\s@AssociationSetDetails' {} a -> s {associationState = a} :: AssociationSetDetails)

-- | The ID of the internet gateway or virtual private gateway.
associationSetDetails_gatewayId :: Lens.Lens' AssociationSetDetails (Prelude.Maybe Prelude.Text)
associationSetDetails_gatewayId = Lens.lens (\AssociationSetDetails' {gatewayId} -> gatewayId) (\s@AssociationSetDetails' {} a -> s {gatewayId = a} :: AssociationSetDetails)

-- | Indicates whether this is the main route table.
associationSetDetails_main :: Lens.Lens' AssociationSetDetails (Prelude.Maybe Prelude.Bool)
associationSetDetails_main = Lens.lens (\AssociationSetDetails' {main} -> main) (\s@AssociationSetDetails' {} a -> s {main = a} :: AssociationSetDetails)

-- | The ID of the association.
associationSetDetails_routeTableAssociationId :: Lens.Lens' AssociationSetDetails (Prelude.Maybe Prelude.Text)
associationSetDetails_routeTableAssociationId = Lens.lens (\AssociationSetDetails' {routeTableAssociationId} -> routeTableAssociationId) (\s@AssociationSetDetails' {} a -> s {routeTableAssociationId = a} :: AssociationSetDetails)

-- | The ID of the route table.
associationSetDetails_routeTableId :: Lens.Lens' AssociationSetDetails (Prelude.Maybe Prelude.Text)
associationSetDetails_routeTableId = Lens.lens (\AssociationSetDetails' {routeTableId} -> routeTableId) (\s@AssociationSetDetails' {} a -> s {routeTableId = a} :: AssociationSetDetails)

-- | The ID of the subnet. A subnet ID is not returned for an implicit
-- association.
associationSetDetails_subnetId :: Lens.Lens' AssociationSetDetails (Prelude.Maybe Prelude.Text)
associationSetDetails_subnetId = Lens.lens (\AssociationSetDetails' {subnetId} -> subnetId) (\s@AssociationSetDetails' {} a -> s {subnetId = a} :: AssociationSetDetails)

instance Data.FromJSON AssociationSetDetails where
  parseJSON =
    Data.withObject
      "AssociationSetDetails"
      ( \x ->
          AssociationSetDetails'
            Prelude.<$> (x Data..:? "AssociationState")
            Prelude.<*> (x Data..:? "GatewayId")
            Prelude.<*> (x Data..:? "Main")
            Prelude.<*> (x Data..:? "RouteTableAssociationId")
            Prelude.<*> (x Data..:? "RouteTableId")
            Prelude.<*> (x Data..:? "SubnetId")
      )

instance Prelude.Hashable AssociationSetDetails where
  hashWithSalt _salt AssociationSetDetails' {..} =
    _salt
      `Prelude.hashWithSalt` associationState
      `Prelude.hashWithSalt` gatewayId
      `Prelude.hashWithSalt` main
      `Prelude.hashWithSalt` routeTableAssociationId
      `Prelude.hashWithSalt` routeTableId
      `Prelude.hashWithSalt` subnetId

instance Prelude.NFData AssociationSetDetails where
  rnf AssociationSetDetails' {..} =
    Prelude.rnf associationState
      `Prelude.seq` Prelude.rnf gatewayId
      `Prelude.seq` Prelude.rnf main
      `Prelude.seq` Prelude.rnf routeTableAssociationId
      `Prelude.seq` Prelude.rnf routeTableId
      `Prelude.seq` Prelude.rnf subnetId

instance Data.ToJSON AssociationSetDetails where
  toJSON AssociationSetDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AssociationState" Data..=)
              Prelude.<$> associationState,
            ("GatewayId" Data..=) Prelude.<$> gatewayId,
            ("Main" Data..=) Prelude.<$> main,
            ("RouteTableAssociationId" Data..=)
              Prelude.<$> routeTableAssociationId,
            ("RouteTableId" Data..=) Prelude.<$> routeTableId,
            ("SubnetId" Data..=) Prelude.<$> subnetId
          ]
      )
