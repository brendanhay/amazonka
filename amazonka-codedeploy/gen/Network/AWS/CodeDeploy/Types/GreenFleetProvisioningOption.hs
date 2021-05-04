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
-- Module      : Network.AWS.CodeDeploy.Types.GreenFleetProvisioningOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.GreenFleetProvisioningOption where

import Network.AWS.CodeDeploy.Types.GreenFleetProvisioningAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.FromJSON
    GreenFleetProvisioningOption
  where
  parseJSON =
    Prelude.withObject
      "GreenFleetProvisioningOption"
      ( \x ->
          GreenFleetProvisioningOption'
            Prelude.<$> (x Prelude..:? "action")
      )

instance
  Prelude.Hashable
    GreenFleetProvisioningOption

instance Prelude.NFData GreenFleetProvisioningOption

instance Prelude.ToJSON GreenFleetProvisioningOption where
  toJSON GreenFleetProvisioningOption' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("action" Prelude..=) Prelude.<$> action]
      )
