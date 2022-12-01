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
-- Module      : Amazonka.FMS.Types.EC2ReplaceRouteTableAssociationAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.EC2ReplaceRouteTableAssociationAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FMS.Types.ActionTarget
import qualified Amazonka.Prelude as Prelude

-- | Information about the ReplaceRouteTableAssociation action in Amazon EC2.
--
-- /See:/ 'newEC2ReplaceRouteTableAssociationAction' smart constructor.
data EC2ReplaceRouteTableAssociationAction = EC2ReplaceRouteTableAssociationAction'
  { -- | A description of the ReplaceRouteTableAssociation action in Amazon EC2.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information about the association ID.
    associationId :: ActionTarget,
    -- | Information about the ID of the new route table to associate with the
    -- subnet.
    routeTableId :: ActionTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EC2ReplaceRouteTableAssociationAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'eC2ReplaceRouteTableAssociationAction_description' - A description of the ReplaceRouteTableAssociation action in Amazon EC2.
--
-- 'associationId', 'eC2ReplaceRouteTableAssociationAction_associationId' - Information about the association ID.
--
-- 'routeTableId', 'eC2ReplaceRouteTableAssociationAction_routeTableId' - Information about the ID of the new route table to associate with the
-- subnet.
newEC2ReplaceRouteTableAssociationAction ::
  -- | 'associationId'
  ActionTarget ->
  -- | 'routeTableId'
  ActionTarget ->
  EC2ReplaceRouteTableAssociationAction
newEC2ReplaceRouteTableAssociationAction
  pAssociationId_
  pRouteTableId_ =
    EC2ReplaceRouteTableAssociationAction'
      { description =
          Prelude.Nothing,
        associationId = pAssociationId_,
        routeTableId = pRouteTableId_
      }

-- | A description of the ReplaceRouteTableAssociation action in Amazon EC2.
eC2ReplaceRouteTableAssociationAction_description :: Lens.Lens' EC2ReplaceRouteTableAssociationAction (Prelude.Maybe Prelude.Text)
eC2ReplaceRouteTableAssociationAction_description = Lens.lens (\EC2ReplaceRouteTableAssociationAction' {description} -> description) (\s@EC2ReplaceRouteTableAssociationAction' {} a -> s {description = a} :: EC2ReplaceRouteTableAssociationAction)

-- | Information about the association ID.
eC2ReplaceRouteTableAssociationAction_associationId :: Lens.Lens' EC2ReplaceRouteTableAssociationAction ActionTarget
eC2ReplaceRouteTableAssociationAction_associationId = Lens.lens (\EC2ReplaceRouteTableAssociationAction' {associationId} -> associationId) (\s@EC2ReplaceRouteTableAssociationAction' {} a -> s {associationId = a} :: EC2ReplaceRouteTableAssociationAction)

-- | Information about the ID of the new route table to associate with the
-- subnet.
eC2ReplaceRouteTableAssociationAction_routeTableId :: Lens.Lens' EC2ReplaceRouteTableAssociationAction ActionTarget
eC2ReplaceRouteTableAssociationAction_routeTableId = Lens.lens (\EC2ReplaceRouteTableAssociationAction' {routeTableId} -> routeTableId) (\s@EC2ReplaceRouteTableAssociationAction' {} a -> s {routeTableId = a} :: EC2ReplaceRouteTableAssociationAction)

instance
  Core.FromJSON
    EC2ReplaceRouteTableAssociationAction
  where
  parseJSON =
    Core.withObject
      "EC2ReplaceRouteTableAssociationAction"
      ( \x ->
          EC2ReplaceRouteTableAssociationAction'
            Prelude.<$> (x Core..:? "Description")
            Prelude.<*> (x Core..: "AssociationId")
            Prelude.<*> (x Core..: "RouteTableId")
      )

instance
  Prelude.Hashable
    EC2ReplaceRouteTableAssociationAction
  where
  hashWithSalt
    _salt
    EC2ReplaceRouteTableAssociationAction' {..} =
      _salt `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` associationId
        `Prelude.hashWithSalt` routeTableId

instance
  Prelude.NFData
    EC2ReplaceRouteTableAssociationAction
  where
  rnf EC2ReplaceRouteTableAssociationAction' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf routeTableId
