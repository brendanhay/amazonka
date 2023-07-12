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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.BlueGreenDeploymentConfiguration where

import Amazonka.CodeDeploy.Types.BlueInstanceTerminationOption
import Amazonka.CodeDeploy.Types.DeploymentReadyOption
import Amazonka.CodeDeploy.Types.GreenFleetProvisioningOption
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about blue\/green deployment options for a deployment group.
--
-- /See:/ 'newBlueGreenDeploymentConfiguration' smart constructor.
data BlueGreenDeploymentConfiguration = BlueGreenDeploymentConfiguration'
  { -- | Information about the action to take when newly provisioned instances
    -- are ready to receive traffic in a blue\/green deployment.
    deploymentReadyOption :: Prelude.Maybe DeploymentReadyOption,
    -- | Information about how instances are provisioned for a replacement
    -- environment in a blue\/green deployment.
    greenFleetProvisioningOption :: Prelude.Maybe GreenFleetProvisioningOption,
    -- | Information about whether to terminate instances in the original fleet
    -- during a blue\/green deployment.
    terminateBlueInstancesOnDeploymentSuccess :: Prelude.Maybe BlueInstanceTerminationOption
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
-- 'deploymentReadyOption', 'blueGreenDeploymentConfiguration_deploymentReadyOption' - Information about the action to take when newly provisioned instances
-- are ready to receive traffic in a blue\/green deployment.
--
-- 'greenFleetProvisioningOption', 'blueGreenDeploymentConfiguration_greenFleetProvisioningOption' - Information about how instances are provisioned for a replacement
-- environment in a blue\/green deployment.
--
-- 'terminateBlueInstancesOnDeploymentSuccess', 'blueGreenDeploymentConfiguration_terminateBlueInstancesOnDeploymentSuccess' - Information about whether to terminate instances in the original fleet
-- during a blue\/green deployment.
newBlueGreenDeploymentConfiguration ::
  BlueGreenDeploymentConfiguration
newBlueGreenDeploymentConfiguration =
  BlueGreenDeploymentConfiguration'
    { deploymentReadyOption =
        Prelude.Nothing,
      greenFleetProvisioningOption =
        Prelude.Nothing,
      terminateBlueInstancesOnDeploymentSuccess =
        Prelude.Nothing
    }

-- | Information about the action to take when newly provisioned instances
-- are ready to receive traffic in a blue\/green deployment.
blueGreenDeploymentConfiguration_deploymentReadyOption :: Lens.Lens' BlueGreenDeploymentConfiguration (Prelude.Maybe DeploymentReadyOption)
blueGreenDeploymentConfiguration_deploymentReadyOption = Lens.lens (\BlueGreenDeploymentConfiguration' {deploymentReadyOption} -> deploymentReadyOption) (\s@BlueGreenDeploymentConfiguration' {} a -> s {deploymentReadyOption = a} :: BlueGreenDeploymentConfiguration)

-- | Information about how instances are provisioned for a replacement
-- environment in a blue\/green deployment.
blueGreenDeploymentConfiguration_greenFleetProvisioningOption :: Lens.Lens' BlueGreenDeploymentConfiguration (Prelude.Maybe GreenFleetProvisioningOption)
blueGreenDeploymentConfiguration_greenFleetProvisioningOption = Lens.lens (\BlueGreenDeploymentConfiguration' {greenFleetProvisioningOption} -> greenFleetProvisioningOption) (\s@BlueGreenDeploymentConfiguration' {} a -> s {greenFleetProvisioningOption = a} :: BlueGreenDeploymentConfiguration)

-- | Information about whether to terminate instances in the original fleet
-- during a blue\/green deployment.
blueGreenDeploymentConfiguration_terminateBlueInstancesOnDeploymentSuccess :: Lens.Lens' BlueGreenDeploymentConfiguration (Prelude.Maybe BlueInstanceTerminationOption)
blueGreenDeploymentConfiguration_terminateBlueInstancesOnDeploymentSuccess = Lens.lens (\BlueGreenDeploymentConfiguration' {terminateBlueInstancesOnDeploymentSuccess} -> terminateBlueInstancesOnDeploymentSuccess) (\s@BlueGreenDeploymentConfiguration' {} a -> s {terminateBlueInstancesOnDeploymentSuccess = a} :: BlueGreenDeploymentConfiguration)

instance
  Data.FromJSON
    BlueGreenDeploymentConfiguration
  where
  parseJSON =
    Data.withObject
      "BlueGreenDeploymentConfiguration"
      ( \x ->
          BlueGreenDeploymentConfiguration'
            Prelude.<$> (x Data..:? "deploymentReadyOption")
            Prelude.<*> (x Data..:? "greenFleetProvisioningOption")
            Prelude.<*> ( x
                            Data..:? "terminateBlueInstancesOnDeploymentSuccess"
                        )
      )

instance
  Prelude.Hashable
    BlueGreenDeploymentConfiguration
  where
  hashWithSalt
    _salt
    BlueGreenDeploymentConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` deploymentReadyOption
        `Prelude.hashWithSalt` greenFleetProvisioningOption
        `Prelude.hashWithSalt` terminateBlueInstancesOnDeploymentSuccess

instance
  Prelude.NFData
    BlueGreenDeploymentConfiguration
  where
  rnf BlueGreenDeploymentConfiguration' {..} =
    Prelude.rnf deploymentReadyOption
      `Prelude.seq` Prelude.rnf greenFleetProvisioningOption
      `Prelude.seq` Prelude.rnf terminateBlueInstancesOnDeploymentSuccess

instance Data.ToJSON BlueGreenDeploymentConfiguration where
  toJSON BlueGreenDeploymentConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("deploymentReadyOption" Data..=)
              Prelude.<$> deploymentReadyOption,
            ("greenFleetProvisioningOption" Data..=)
              Prelude.<$> greenFleetProvisioningOption,
            ("terminateBlueInstancesOnDeploymentSuccess" Data..=)
              Prelude.<$> terminateBlueInstancesOnDeploymentSuccess
          ]
      )
