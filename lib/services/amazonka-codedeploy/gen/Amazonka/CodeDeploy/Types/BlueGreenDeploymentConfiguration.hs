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
-- Module      : Amazonka.CodeDeploy.Types.BlueGreenDeploymentConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.BlueGreenDeploymentConfiguration where

import Amazonka.CodeDeploy.Types.BlueInstanceTerminationOption
import Amazonka.CodeDeploy.Types.DeploymentReadyOption
import Amazonka.CodeDeploy.Types.GreenFleetProvisioningOption
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about blue\/green deployment options for a deployment group.
--
-- /See:/ 'newBlueGreenDeploymentConfiguration' smart constructor.
data BlueGreenDeploymentConfiguration = BlueGreenDeploymentConfiguration'
  { -- | Information about how instances are provisioned for a replacement
    -- environment in a blue\/green deployment.
    greenFleetProvisioningOption :: Prelude.Maybe GreenFleetProvisioningOption,
    -- | Information about whether to terminate instances in the original fleet
    -- during a blue\/green deployment.
    terminateBlueInstancesOnDeploymentSuccess :: Prelude.Maybe BlueInstanceTerminationOption,
    -- | Information about the action to take when newly provisioned instances
    -- are ready to receive traffic in a blue\/green deployment.
    deploymentReadyOption :: Prelude.Maybe DeploymentReadyOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BlueGreenDeploymentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'greenFleetProvisioningOption', 'blueGreenDeploymentConfiguration_greenFleetProvisioningOption' - Information about how instances are provisioned for a replacement
-- environment in a blue\/green deployment.
--
-- 'terminateBlueInstancesOnDeploymentSuccess', 'blueGreenDeploymentConfiguration_terminateBlueInstancesOnDeploymentSuccess' - Information about whether to terminate instances in the original fleet
-- during a blue\/green deployment.
--
-- 'deploymentReadyOption', 'blueGreenDeploymentConfiguration_deploymentReadyOption' - Information about the action to take when newly provisioned instances
-- are ready to receive traffic in a blue\/green deployment.
newBlueGreenDeploymentConfiguration ::
  BlueGreenDeploymentConfiguration
newBlueGreenDeploymentConfiguration =
  BlueGreenDeploymentConfiguration'
    { greenFleetProvisioningOption =
        Prelude.Nothing,
      terminateBlueInstancesOnDeploymentSuccess =
        Prelude.Nothing,
      deploymentReadyOption = Prelude.Nothing
    }

-- | Information about how instances are provisioned for a replacement
-- environment in a blue\/green deployment.
blueGreenDeploymentConfiguration_greenFleetProvisioningOption :: Lens.Lens' BlueGreenDeploymentConfiguration (Prelude.Maybe GreenFleetProvisioningOption)
blueGreenDeploymentConfiguration_greenFleetProvisioningOption = Lens.lens (\BlueGreenDeploymentConfiguration' {greenFleetProvisioningOption} -> greenFleetProvisioningOption) (\s@BlueGreenDeploymentConfiguration' {} a -> s {greenFleetProvisioningOption = a} :: BlueGreenDeploymentConfiguration)

-- | Information about whether to terminate instances in the original fleet
-- during a blue\/green deployment.
blueGreenDeploymentConfiguration_terminateBlueInstancesOnDeploymentSuccess :: Lens.Lens' BlueGreenDeploymentConfiguration (Prelude.Maybe BlueInstanceTerminationOption)
blueGreenDeploymentConfiguration_terminateBlueInstancesOnDeploymentSuccess = Lens.lens (\BlueGreenDeploymentConfiguration' {terminateBlueInstancesOnDeploymentSuccess} -> terminateBlueInstancesOnDeploymentSuccess) (\s@BlueGreenDeploymentConfiguration' {} a -> s {terminateBlueInstancesOnDeploymentSuccess = a} :: BlueGreenDeploymentConfiguration)

-- | Information about the action to take when newly provisioned instances
-- are ready to receive traffic in a blue\/green deployment.
blueGreenDeploymentConfiguration_deploymentReadyOption :: Lens.Lens' BlueGreenDeploymentConfiguration (Prelude.Maybe DeploymentReadyOption)
blueGreenDeploymentConfiguration_deploymentReadyOption = Lens.lens (\BlueGreenDeploymentConfiguration' {deploymentReadyOption} -> deploymentReadyOption) (\s@BlueGreenDeploymentConfiguration' {} a -> s {deploymentReadyOption = a} :: BlueGreenDeploymentConfiguration)

instance
  Core.FromJSON
    BlueGreenDeploymentConfiguration
  where
  parseJSON =
    Core.withObject
      "BlueGreenDeploymentConfiguration"
      ( \x ->
          BlueGreenDeploymentConfiguration'
            Prelude.<$> (x Core..:? "greenFleetProvisioningOption")
            Prelude.<*> ( x
                            Core..:? "terminateBlueInstancesOnDeploymentSuccess"
                        )
            Prelude.<*> (x Core..:? "deploymentReadyOption")
      )

instance
  Prelude.Hashable
    BlueGreenDeploymentConfiguration
  where
  hashWithSalt
    _salt
    BlueGreenDeploymentConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` greenFleetProvisioningOption
        `Prelude.hashWithSalt` terminateBlueInstancesOnDeploymentSuccess
        `Prelude.hashWithSalt` deploymentReadyOption

instance
  Prelude.NFData
    BlueGreenDeploymentConfiguration
  where
  rnf BlueGreenDeploymentConfiguration' {..} =
    Prelude.rnf greenFleetProvisioningOption
      `Prelude.seq` Prelude.rnf terminateBlueInstancesOnDeploymentSuccess
      `Prelude.seq` Prelude.rnf deploymentReadyOption

instance Core.ToJSON BlueGreenDeploymentConfiguration where
  toJSON BlueGreenDeploymentConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("greenFleetProvisioningOption" Core..=)
              Prelude.<$> greenFleetProvisioningOption,
            ("terminateBlueInstancesOnDeploymentSuccess" Core..=)
              Prelude.<$> terminateBlueInstancesOnDeploymentSuccess,
            ("deploymentReadyOption" Core..=)
              Prelude.<$> deploymentReadyOption
          ]
      )
