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
-- Module      : Network.AWS.CodeDeploy.Types.BlueGreenDeploymentConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.BlueGreenDeploymentConfiguration where

import Network.AWS.CodeDeploy.Types.BlueInstanceTerminationOption
import Network.AWS.CodeDeploy.Types.DeploymentReadyOption
import Network.AWS.CodeDeploy.Types.GreenFleetProvisioningOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about blue\/green deployment options for a deployment group.
--
-- /See:/ 'newBlueGreenDeploymentConfiguration' smart constructor.
data BlueGreenDeploymentConfiguration = BlueGreenDeploymentConfiguration'
  { -- | Information about how instances are provisioned for a replacement
    -- environment in a blue\/green deployment.
    greenFleetProvisioningOption :: Prelude.Maybe GreenFleetProvisioningOption,
    -- | Information about the action to take when newly provisioned instances
    -- are ready to receive traffic in a blue\/green deployment.
    deploymentReadyOption :: Prelude.Maybe DeploymentReadyOption,
    -- | Information about whether to terminate instances in the original fleet
    -- during a blue\/green deployment.
    terminateBlueInstancesOnDeploymentSuccess :: Prelude.Maybe BlueInstanceTerminationOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'deploymentReadyOption', 'blueGreenDeploymentConfiguration_deploymentReadyOption' - Information about the action to take when newly provisioned instances
-- are ready to receive traffic in a blue\/green deployment.
--
-- 'terminateBlueInstancesOnDeploymentSuccess', 'blueGreenDeploymentConfiguration_terminateBlueInstancesOnDeploymentSuccess' - Information about whether to terminate instances in the original fleet
-- during a blue\/green deployment.
newBlueGreenDeploymentConfiguration ::
  BlueGreenDeploymentConfiguration
newBlueGreenDeploymentConfiguration =
  BlueGreenDeploymentConfiguration'
    { greenFleetProvisioningOption =
        Prelude.Nothing,
      deploymentReadyOption = Prelude.Nothing,
      terminateBlueInstancesOnDeploymentSuccess =
        Prelude.Nothing
    }

-- | Information about how instances are provisioned for a replacement
-- environment in a blue\/green deployment.
blueGreenDeploymentConfiguration_greenFleetProvisioningOption :: Lens.Lens' BlueGreenDeploymentConfiguration (Prelude.Maybe GreenFleetProvisioningOption)
blueGreenDeploymentConfiguration_greenFleetProvisioningOption = Lens.lens (\BlueGreenDeploymentConfiguration' {greenFleetProvisioningOption} -> greenFleetProvisioningOption) (\s@BlueGreenDeploymentConfiguration' {} a -> s {greenFleetProvisioningOption = a} :: BlueGreenDeploymentConfiguration)

-- | Information about the action to take when newly provisioned instances
-- are ready to receive traffic in a blue\/green deployment.
blueGreenDeploymentConfiguration_deploymentReadyOption :: Lens.Lens' BlueGreenDeploymentConfiguration (Prelude.Maybe DeploymentReadyOption)
blueGreenDeploymentConfiguration_deploymentReadyOption = Lens.lens (\BlueGreenDeploymentConfiguration' {deploymentReadyOption} -> deploymentReadyOption) (\s@BlueGreenDeploymentConfiguration' {} a -> s {deploymentReadyOption = a} :: BlueGreenDeploymentConfiguration)

-- | Information about whether to terminate instances in the original fleet
-- during a blue\/green deployment.
blueGreenDeploymentConfiguration_terminateBlueInstancesOnDeploymentSuccess :: Lens.Lens' BlueGreenDeploymentConfiguration (Prelude.Maybe BlueInstanceTerminationOption)
blueGreenDeploymentConfiguration_terminateBlueInstancesOnDeploymentSuccess = Lens.lens (\BlueGreenDeploymentConfiguration' {terminateBlueInstancesOnDeploymentSuccess} -> terminateBlueInstancesOnDeploymentSuccess) (\s@BlueGreenDeploymentConfiguration' {} a -> s {terminateBlueInstancesOnDeploymentSuccess = a} :: BlueGreenDeploymentConfiguration)

instance
  Prelude.FromJSON
    BlueGreenDeploymentConfiguration
  where
  parseJSON =
    Prelude.withObject
      "BlueGreenDeploymentConfiguration"
      ( \x ->
          BlueGreenDeploymentConfiguration'
            Prelude.<$> (x Prelude..:? "greenFleetProvisioningOption")
            Prelude.<*> (x Prelude..:? "deploymentReadyOption")
            Prelude.<*> ( x
                            Prelude..:? "terminateBlueInstancesOnDeploymentSuccess"
                        )
      )

instance
  Prelude.Hashable
    BlueGreenDeploymentConfiguration

instance
  Prelude.NFData
    BlueGreenDeploymentConfiguration

instance
  Prelude.ToJSON
    BlueGreenDeploymentConfiguration
  where
  toJSON BlueGreenDeploymentConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("greenFleetProvisioningOption" Prelude..=)
              Prelude.<$> greenFleetProvisioningOption,
            ("deploymentReadyOption" Prelude..=)
              Prelude.<$> deploymentReadyOption,
            ( "terminateBlueInstancesOnDeploymentSuccess"
                Prelude..=
            )
              Prelude.<$> terminateBlueInstancesOnDeploymentSuccess
          ]
      )
