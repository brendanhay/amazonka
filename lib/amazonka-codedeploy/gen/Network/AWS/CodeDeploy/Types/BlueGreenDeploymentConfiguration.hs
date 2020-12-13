{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.BlueGreenDeploymentConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.BlueGreenDeploymentConfiguration
  ( BlueGreenDeploymentConfiguration (..),

    -- * Smart constructor
    mkBlueGreenDeploymentConfiguration,

    -- * Lenses
    bgdcDeploymentReadyOption,
    bgdcGreenFleetProvisioningOption,
    bgdcTerminateBlueInstancesOnDeploymentSuccess,
  )
where

import Network.AWS.CodeDeploy.Types.BlueInstanceTerminationOption
import Network.AWS.CodeDeploy.Types.DeploymentReadyOption
import Network.AWS.CodeDeploy.Types.GreenFleetProvisioningOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about blue/green deployment options for a deployment group.
--
-- /See:/ 'mkBlueGreenDeploymentConfiguration' smart constructor.
data BlueGreenDeploymentConfiguration = BlueGreenDeploymentConfiguration'
  { -- | Information about the action to take when newly provisioned instances are ready to receive traffic in a blue/green deployment.
    deploymentReadyOption :: Lude.Maybe DeploymentReadyOption,
    -- | Information about how instances are provisioned for a replacement environment in a blue/green deployment.
    greenFleetProvisioningOption :: Lude.Maybe GreenFleetProvisioningOption,
    -- | Information about whether to terminate instances in the original fleet during a blue/green deployment.
    terminateBlueInstancesOnDeploymentSuccess :: Lude.Maybe BlueInstanceTerminationOption
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BlueGreenDeploymentConfiguration' with the minimum fields required to make a request.
--
-- * 'deploymentReadyOption' - Information about the action to take when newly provisioned instances are ready to receive traffic in a blue/green deployment.
-- * 'greenFleetProvisioningOption' - Information about how instances are provisioned for a replacement environment in a blue/green deployment.
-- * 'terminateBlueInstancesOnDeploymentSuccess' - Information about whether to terminate instances in the original fleet during a blue/green deployment.
mkBlueGreenDeploymentConfiguration ::
  BlueGreenDeploymentConfiguration
mkBlueGreenDeploymentConfiguration =
  BlueGreenDeploymentConfiguration'
    { deploymentReadyOption =
        Lude.Nothing,
      greenFleetProvisioningOption = Lude.Nothing,
      terminateBlueInstancesOnDeploymentSuccess = Lude.Nothing
    }

-- | Information about the action to take when newly provisioned instances are ready to receive traffic in a blue/green deployment.
--
-- /Note:/ Consider using 'deploymentReadyOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdcDeploymentReadyOption :: Lens.Lens' BlueGreenDeploymentConfiguration (Lude.Maybe DeploymentReadyOption)
bgdcDeploymentReadyOption = Lens.lens (deploymentReadyOption :: BlueGreenDeploymentConfiguration -> Lude.Maybe DeploymentReadyOption) (\s a -> s {deploymentReadyOption = a} :: BlueGreenDeploymentConfiguration)
{-# DEPRECATED bgdcDeploymentReadyOption "Use generic-lens or generic-optics with 'deploymentReadyOption' instead." #-}

-- | Information about how instances are provisioned for a replacement environment in a blue/green deployment.
--
-- /Note:/ Consider using 'greenFleetProvisioningOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdcGreenFleetProvisioningOption :: Lens.Lens' BlueGreenDeploymentConfiguration (Lude.Maybe GreenFleetProvisioningOption)
bgdcGreenFleetProvisioningOption = Lens.lens (greenFleetProvisioningOption :: BlueGreenDeploymentConfiguration -> Lude.Maybe GreenFleetProvisioningOption) (\s a -> s {greenFleetProvisioningOption = a} :: BlueGreenDeploymentConfiguration)
{-# DEPRECATED bgdcGreenFleetProvisioningOption "Use generic-lens or generic-optics with 'greenFleetProvisioningOption' instead." #-}

-- | Information about whether to terminate instances in the original fleet during a blue/green deployment.
--
-- /Note:/ Consider using 'terminateBlueInstancesOnDeploymentSuccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdcTerminateBlueInstancesOnDeploymentSuccess :: Lens.Lens' BlueGreenDeploymentConfiguration (Lude.Maybe BlueInstanceTerminationOption)
bgdcTerminateBlueInstancesOnDeploymentSuccess = Lens.lens (terminateBlueInstancesOnDeploymentSuccess :: BlueGreenDeploymentConfiguration -> Lude.Maybe BlueInstanceTerminationOption) (\s a -> s {terminateBlueInstancesOnDeploymentSuccess = a} :: BlueGreenDeploymentConfiguration)
{-# DEPRECATED bgdcTerminateBlueInstancesOnDeploymentSuccess "Use generic-lens or generic-optics with 'terminateBlueInstancesOnDeploymentSuccess' instead." #-}

instance Lude.FromJSON BlueGreenDeploymentConfiguration where
  parseJSON =
    Lude.withObject
      "BlueGreenDeploymentConfiguration"
      ( \x ->
          BlueGreenDeploymentConfiguration'
            Lude.<$> (x Lude..:? "deploymentReadyOption")
            Lude.<*> (x Lude..:? "greenFleetProvisioningOption")
            Lude.<*> (x Lude..:? "terminateBlueInstancesOnDeploymentSuccess")
      )

instance Lude.ToJSON BlueGreenDeploymentConfiguration where
  toJSON BlueGreenDeploymentConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("deploymentReadyOption" Lude..=) Lude.<$> deploymentReadyOption,
            ("greenFleetProvisioningOption" Lude..=)
              Lude.<$> greenFleetProvisioningOption,
            ("terminateBlueInstancesOnDeploymentSuccess" Lude..=)
              Lude.<$> terminateBlueInstancesOnDeploymentSuccess
          ]
      )
