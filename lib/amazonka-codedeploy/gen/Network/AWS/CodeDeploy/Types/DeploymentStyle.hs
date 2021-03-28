{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentStyle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.DeploymentStyle
  ( DeploymentStyle (..)
  -- * Smart constructor
  , mkDeploymentStyle
  -- * Lenses
  , dsDeploymentOption
  , dsDeploymentType
  ) where

import qualified Network.AWS.CodeDeploy.Types.DeploymentOption as Types
import qualified Network.AWS.CodeDeploy.Types.DeploymentType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.
--
-- /See:/ 'mkDeploymentStyle' smart constructor.
data DeploymentStyle = DeploymentStyle'
  { deploymentOption :: Core.Maybe Types.DeploymentOption
    -- ^ Indicates whether to route deployment traffic behind a load balancer.
  , deploymentType :: Core.Maybe Types.DeploymentType
    -- ^ Indicates whether to run an in-place deployment or a blue/green deployment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeploymentStyle' value with any optional fields omitted.
mkDeploymentStyle
    :: DeploymentStyle
mkDeploymentStyle
  = DeploymentStyle'{deploymentOption = Core.Nothing,
                     deploymentType = Core.Nothing}

-- | Indicates whether to route deployment traffic behind a load balancer.
--
-- /Note:/ Consider using 'deploymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDeploymentOption :: Lens.Lens' DeploymentStyle (Core.Maybe Types.DeploymentOption)
dsDeploymentOption = Lens.field @"deploymentOption"
{-# INLINEABLE dsDeploymentOption #-}
{-# DEPRECATED deploymentOption "Use generic-lens or generic-optics with 'deploymentOption' instead"  #-}

-- | Indicates whether to run an in-place deployment or a blue/green deployment.
--
-- /Note:/ Consider using 'deploymentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDeploymentType :: Lens.Lens' DeploymentStyle (Core.Maybe Types.DeploymentType)
dsDeploymentType = Lens.field @"deploymentType"
{-# INLINEABLE dsDeploymentType #-}
{-# DEPRECATED deploymentType "Use generic-lens or generic-optics with 'deploymentType' instead"  #-}

instance Core.FromJSON DeploymentStyle where
        toJSON DeploymentStyle{..}
          = Core.object
              (Core.catMaybes
                 [("deploymentOption" Core..=) Core.<$> deploymentOption,
                  ("deploymentType" Core..=) Core.<$> deploymentType])

instance Core.FromJSON DeploymentStyle where
        parseJSON
          = Core.withObject "DeploymentStyle" Core.$
              \ x ->
                DeploymentStyle' Core.<$>
                  (x Core..:? "deploymentOption") Core.<*>
                    x Core..:? "deploymentType"
