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
-- Module      : Amazonka.FMS.Types.EC2CopyRouteTableAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.EC2CopyRouteTableAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.ActionTarget
import qualified Amazonka.Prelude as Prelude

-- | An action that copies the EC2 route table for use in remediation.
--
-- /See:/ 'newEC2CopyRouteTableAction' smart constructor.
data EC2CopyRouteTableAction = EC2CopyRouteTableAction'
  { -- | A description of the copied EC2 route table that is associated with the
    -- remediation action.
    description :: Prelude.Maybe Prelude.Text,
    -- | The VPC ID of the copied EC2 route table that is associated with the
    -- remediation action.
    vpcId :: ActionTarget,
    -- | The ID of the copied EC2 route table that is associated with the
    -- remediation action.
    routeTableId :: ActionTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EC2CopyRouteTableAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'eC2CopyRouteTableAction_description' - A description of the copied EC2 route table that is associated with the
-- remediation action.
--
-- 'vpcId', 'eC2CopyRouteTableAction_vpcId' - The VPC ID of the copied EC2 route table that is associated with the
-- remediation action.
--
-- 'routeTableId', 'eC2CopyRouteTableAction_routeTableId' - The ID of the copied EC2 route table that is associated with the
-- remediation action.
newEC2CopyRouteTableAction ::
  -- | 'vpcId'
  ActionTarget ->
  -- | 'routeTableId'
  ActionTarget ->
  EC2CopyRouteTableAction
newEC2CopyRouteTableAction pVpcId_ pRouteTableId_ =
  EC2CopyRouteTableAction'
    { description =
        Prelude.Nothing,
      vpcId = pVpcId_,
      routeTableId = pRouteTableId_
    }

-- | A description of the copied EC2 route table that is associated with the
-- remediation action.
eC2CopyRouteTableAction_description :: Lens.Lens' EC2CopyRouteTableAction (Prelude.Maybe Prelude.Text)
eC2CopyRouteTableAction_description = Lens.lens (\EC2CopyRouteTableAction' {description} -> description) (\s@EC2CopyRouteTableAction' {} a -> s {description = a} :: EC2CopyRouteTableAction)

-- | The VPC ID of the copied EC2 route table that is associated with the
-- remediation action.
eC2CopyRouteTableAction_vpcId :: Lens.Lens' EC2CopyRouteTableAction ActionTarget
eC2CopyRouteTableAction_vpcId = Lens.lens (\EC2CopyRouteTableAction' {vpcId} -> vpcId) (\s@EC2CopyRouteTableAction' {} a -> s {vpcId = a} :: EC2CopyRouteTableAction)

-- | The ID of the copied EC2 route table that is associated with the
-- remediation action.
eC2CopyRouteTableAction_routeTableId :: Lens.Lens' EC2CopyRouteTableAction ActionTarget
eC2CopyRouteTableAction_routeTableId = Lens.lens (\EC2CopyRouteTableAction' {routeTableId} -> routeTableId) (\s@EC2CopyRouteTableAction' {} a -> s {routeTableId = a} :: EC2CopyRouteTableAction)

instance Data.FromJSON EC2CopyRouteTableAction where
  parseJSON =
    Data.withObject
      "EC2CopyRouteTableAction"
      ( \x ->
          EC2CopyRouteTableAction'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..: "VpcId")
            Prelude.<*> (x Data..: "RouteTableId")
      )

instance Prelude.Hashable EC2CopyRouteTableAction where
  hashWithSalt _salt EC2CopyRouteTableAction' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` routeTableId

instance Prelude.NFData EC2CopyRouteTableAction where
  rnf EC2CopyRouteTableAction' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf routeTableId
