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
-- Module      : Amazonka.CodeDeploy.Types.GreenFleetProvisioningOption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.GreenFleetProvisioningOption where

import Amazonka.CodeDeploy.Types.GreenFleetProvisioningAction
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the instances that belong to the replacement
-- environment in a blue\/green deployment.
--
-- /See:/ 'newGreenFleetProvisioningOption' smart constructor.
data GreenFleetProvisioningOption = GreenFleetProvisioningOption'
  { -- | The method used to add instances to a replacement environment.
    --
    -- -   @DISCOVER_EXISTING@: Use instances that already exist or will be
    --     created manually.
    --
    -- -   @COPY_AUTO_SCALING_GROUP@: Use settings from a specified Auto
    --     Scaling group to define and create instances in a new Auto Scaling
    --     group.
    action :: Prelude.Maybe GreenFleetProvisioningAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GreenFleetProvisioningOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'greenFleetProvisioningOption_action' - The method used to add instances to a replacement environment.
--
-- -   @DISCOVER_EXISTING@: Use instances that already exist or will be
--     created manually.
--
-- -   @COPY_AUTO_SCALING_GROUP@: Use settings from a specified Auto
--     Scaling group to define and create instances in a new Auto Scaling
--     group.
newGreenFleetProvisioningOption ::
  GreenFleetProvisioningOption
newGreenFleetProvisioningOption =
  GreenFleetProvisioningOption'
    { action =
        Prelude.Nothing
    }

-- | The method used to add instances to a replacement environment.
--
-- -   @DISCOVER_EXISTING@: Use instances that already exist or will be
--     created manually.
--
-- -   @COPY_AUTO_SCALING_GROUP@: Use settings from a specified Auto
--     Scaling group to define and create instances in a new Auto Scaling
--     group.
greenFleetProvisioningOption_action :: Lens.Lens' GreenFleetProvisioningOption (Prelude.Maybe GreenFleetProvisioningAction)
greenFleetProvisioningOption_action = Lens.lens (\GreenFleetProvisioningOption' {action} -> action) (\s@GreenFleetProvisioningOption' {} a -> s {action = a} :: GreenFleetProvisioningOption)

instance Data.FromJSON GreenFleetProvisioningOption where
  parseJSON =
    Data.withObject
      "GreenFleetProvisioningOption"
      ( \x ->
          GreenFleetProvisioningOption'
            Prelude.<$> (x Data..:? "action")
      )

instance
  Prelude.Hashable
    GreenFleetProvisioningOption
  where
  hashWithSalt _salt GreenFleetProvisioningOption' {..} =
    _salt `Prelude.hashWithSalt` action

instance Prelude.NFData GreenFleetProvisioningOption where
  rnf GreenFleetProvisioningOption' {..} =
    Prelude.rnf action

instance Data.ToJSON GreenFleetProvisioningOption where
  toJSON GreenFleetProvisioningOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [("action" Data..=) Prelude.<$> action]
      )
