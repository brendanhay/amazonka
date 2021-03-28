{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.BlueGreenDeploymentConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.BlueGreenDeploymentConfiguration
  ( BlueGreenDeploymentConfiguration (..)
  -- * Smart constructor
  , mkBlueGreenDeploymentConfiguration
  -- * Lenses
  , bgdcDeploymentReadyOption
  , bgdcGreenFleetProvisioningOption
  , bgdcTerminateBlueInstancesOnDeploymentSuccess
  ) where

import qualified Network.AWS.CodeDeploy.Types.BlueInstanceTerminationOption as Types
import qualified Network.AWS.CodeDeploy.Types.DeploymentReadyOption as Types
import qualified Network.AWS.CodeDeploy.Types.GreenFleetProvisioningOption as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about blue/green deployment options for a deployment group.
--
-- /See:/ 'mkBlueGreenDeploymentConfiguration' smart constructor.
data BlueGreenDeploymentConfiguration = BlueGreenDeploymentConfiguration'
  { deploymentReadyOption :: Core.Maybe Types.DeploymentReadyOption
    -- ^ Information about the action to take when newly provisioned instances are ready to receive traffic in a blue/green deployment.
  , greenFleetProvisioningOption :: Core.Maybe Types.GreenFleetProvisioningOption
    -- ^ Information about how instances are provisioned for a replacement environment in a blue/green deployment.
  , terminateBlueInstancesOnDeploymentSuccess :: Core.Maybe Types.BlueInstanceTerminationOption
    -- ^ Information about whether to terminate instances in the original fleet during a blue/green deployment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BlueGreenDeploymentConfiguration' value with any optional fields omitted.
mkBlueGreenDeploymentConfiguration
    :: BlueGreenDeploymentConfiguration
mkBlueGreenDeploymentConfiguration
  = BlueGreenDeploymentConfiguration'{deploymentReadyOption =
                                        Core.Nothing,
                                      greenFleetProvisioningOption = Core.Nothing,
                                      terminateBlueInstancesOnDeploymentSuccess = Core.Nothing}

-- | Information about the action to take when newly provisioned instances are ready to receive traffic in a blue/green deployment.
--
-- /Note:/ Consider using 'deploymentReadyOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdcDeploymentReadyOption :: Lens.Lens' BlueGreenDeploymentConfiguration (Core.Maybe Types.DeploymentReadyOption)
bgdcDeploymentReadyOption = Lens.field @"deploymentReadyOption"
{-# INLINEABLE bgdcDeploymentReadyOption #-}
{-# DEPRECATED deploymentReadyOption "Use generic-lens or generic-optics with 'deploymentReadyOption' instead"  #-}

-- | Information about how instances are provisioned for a replacement environment in a blue/green deployment.
--
-- /Note:/ Consider using 'greenFleetProvisioningOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdcGreenFleetProvisioningOption :: Lens.Lens' BlueGreenDeploymentConfiguration (Core.Maybe Types.GreenFleetProvisioningOption)
bgdcGreenFleetProvisioningOption = Lens.field @"greenFleetProvisioningOption"
{-# INLINEABLE bgdcGreenFleetProvisioningOption #-}
{-# DEPRECATED greenFleetProvisioningOption "Use generic-lens or generic-optics with 'greenFleetProvisioningOption' instead"  #-}

-- | Information about whether to terminate instances in the original fleet during a blue/green deployment.
--
-- /Note:/ Consider using 'terminateBlueInstancesOnDeploymentSuccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdcTerminateBlueInstancesOnDeploymentSuccess :: Lens.Lens' BlueGreenDeploymentConfiguration (Core.Maybe Types.BlueInstanceTerminationOption)
bgdcTerminateBlueInstancesOnDeploymentSuccess = Lens.field @"terminateBlueInstancesOnDeploymentSuccess"
{-# INLINEABLE bgdcTerminateBlueInstancesOnDeploymentSuccess #-}
{-# DEPRECATED terminateBlueInstancesOnDeploymentSuccess "Use generic-lens or generic-optics with 'terminateBlueInstancesOnDeploymentSuccess' instead"  #-}

instance Core.FromJSON BlueGreenDeploymentConfiguration where
        toJSON BlueGreenDeploymentConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("deploymentReadyOption" Core..=) Core.<$> deploymentReadyOption,
                  ("greenFleetProvisioningOption" Core..=) Core.<$>
                    greenFleetProvisioningOption,
                  ("terminateBlueInstancesOnDeploymentSuccess" Core..=) Core.<$>
                    terminateBlueInstancesOnDeploymentSuccess])

instance Core.FromJSON BlueGreenDeploymentConfiguration where
        parseJSON
          = Core.withObject "BlueGreenDeploymentConfiguration" Core.$
              \ x ->
                BlueGreenDeploymentConfiguration' Core.<$>
                  (x Core..:? "deploymentReadyOption") Core.<*>
                    x Core..:? "greenFleetProvisioningOption"
                    Core.<*> x Core..:? "terminateBlueInstancesOnDeploymentSuccess"
