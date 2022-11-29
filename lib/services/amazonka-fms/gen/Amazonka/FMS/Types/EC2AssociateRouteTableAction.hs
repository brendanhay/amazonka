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
-- Module      : Amazonka.FMS.Types.EC2AssociateRouteTableAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.EC2AssociateRouteTableAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FMS.Types.ActionTarget
import qualified Amazonka.Prelude as Prelude

-- | The action of associating an EC2 resource, such as a subnet or internet
-- gateway, with a route table.
--
-- /See:/ 'newEC2AssociateRouteTableAction' smart constructor.
data EC2AssociateRouteTableAction = EC2AssociateRouteTableAction'
  { -- | The ID of the subnet for the EC2 route table that is associated with the
    -- remediation action.
    subnetId :: Prelude.Maybe ActionTarget,
    -- | A description of the EC2 route table that is associated with the
    -- remediation action.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the gateway to be used with the EC2 route table that is
    -- associated with the remediation action.
    gatewayId :: Prelude.Maybe ActionTarget,
    -- | The ID of the EC2 route table that is associated with the remediation
    -- action.
    routeTableId :: ActionTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EC2AssociateRouteTableAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetId', 'eC2AssociateRouteTableAction_subnetId' - The ID of the subnet for the EC2 route table that is associated with the
-- remediation action.
--
-- 'description', 'eC2AssociateRouteTableAction_description' - A description of the EC2 route table that is associated with the
-- remediation action.
--
-- 'gatewayId', 'eC2AssociateRouteTableAction_gatewayId' - The ID of the gateway to be used with the EC2 route table that is
-- associated with the remediation action.
--
-- 'routeTableId', 'eC2AssociateRouteTableAction_routeTableId' - The ID of the EC2 route table that is associated with the remediation
-- action.
newEC2AssociateRouteTableAction ::
  -- | 'routeTableId'
  ActionTarget ->
  EC2AssociateRouteTableAction
newEC2AssociateRouteTableAction pRouteTableId_ =
  EC2AssociateRouteTableAction'
    { subnetId =
        Prelude.Nothing,
      description = Prelude.Nothing,
      gatewayId = Prelude.Nothing,
      routeTableId = pRouteTableId_
    }

-- | The ID of the subnet for the EC2 route table that is associated with the
-- remediation action.
eC2AssociateRouteTableAction_subnetId :: Lens.Lens' EC2AssociateRouteTableAction (Prelude.Maybe ActionTarget)
eC2AssociateRouteTableAction_subnetId = Lens.lens (\EC2AssociateRouteTableAction' {subnetId} -> subnetId) (\s@EC2AssociateRouteTableAction' {} a -> s {subnetId = a} :: EC2AssociateRouteTableAction)

-- | A description of the EC2 route table that is associated with the
-- remediation action.
eC2AssociateRouteTableAction_description :: Lens.Lens' EC2AssociateRouteTableAction (Prelude.Maybe Prelude.Text)
eC2AssociateRouteTableAction_description = Lens.lens (\EC2AssociateRouteTableAction' {description} -> description) (\s@EC2AssociateRouteTableAction' {} a -> s {description = a} :: EC2AssociateRouteTableAction)

-- | The ID of the gateway to be used with the EC2 route table that is
-- associated with the remediation action.
eC2AssociateRouteTableAction_gatewayId :: Lens.Lens' EC2AssociateRouteTableAction (Prelude.Maybe ActionTarget)
eC2AssociateRouteTableAction_gatewayId = Lens.lens (\EC2AssociateRouteTableAction' {gatewayId} -> gatewayId) (\s@EC2AssociateRouteTableAction' {} a -> s {gatewayId = a} :: EC2AssociateRouteTableAction)

-- | The ID of the EC2 route table that is associated with the remediation
-- action.
eC2AssociateRouteTableAction_routeTableId :: Lens.Lens' EC2AssociateRouteTableAction ActionTarget
eC2AssociateRouteTableAction_routeTableId = Lens.lens (\EC2AssociateRouteTableAction' {routeTableId} -> routeTableId) (\s@EC2AssociateRouteTableAction' {} a -> s {routeTableId = a} :: EC2AssociateRouteTableAction)

instance Core.FromJSON EC2AssociateRouteTableAction where
  parseJSON =
    Core.withObject
      "EC2AssociateRouteTableAction"
      ( \x ->
          EC2AssociateRouteTableAction'
            Prelude.<$> (x Core..:? "SubnetId")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "GatewayId")
            Prelude.<*> (x Core..: "RouteTableId")
      )

instance
  Prelude.Hashable
    EC2AssociateRouteTableAction
  where
  hashWithSalt _salt EC2AssociateRouteTableAction' {..} =
    _salt `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` gatewayId
      `Prelude.hashWithSalt` routeTableId

instance Prelude.NFData EC2AssociateRouteTableAction where
  rnf EC2AssociateRouteTableAction' {..} =
    Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf gatewayId
      `Prelude.seq` Prelude.rnf routeTableId
