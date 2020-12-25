{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentReadyOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentReadyOption
  ( DeploymentReadyOption (..),

    -- * Smart constructor
    mkDeploymentReadyOption,

    -- * Lenses
    droActionOnTimeout,
    droWaitTimeInMinutes,
  )
where

import qualified Network.AWS.CodeDeploy.Types.DeploymentReadyAction as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about how traffic is rerouted to instances in a replacement environment in a blue/green deployment.
--
-- /See:/ 'mkDeploymentReadyOption' smart constructor.
data DeploymentReadyOption = DeploymentReadyOption'
  { -- | Information about when to reroute traffic from an original environment to a replacement environment in a blue/green deployment.
    --
    --
    --     * CONTINUE_DEPLOYMENT: Register new instances with the load balancer immediately after the new application revision is installed on the instances in the replacement environment.
    --
    --
    --     * STOP_DEPLOYMENT: Do not register new instances with a load balancer unless traffic rerouting is started using 'ContinueDeployment' . If traffic rerouting is not started before the end of the specified wait period, the deployment status is changed to Stopped.
    actionOnTimeout :: Core.Maybe Types.DeploymentReadyAction,
    -- | The number of minutes to wait before the status of a blue/green deployment is changed to Stopped if rerouting is not started manually. Applies only to the @STOP_DEPLOYMENT@ option for @actionOnTimeout@ .
    waitTimeInMinutes :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeploymentReadyOption' value with any optional fields omitted.
mkDeploymentReadyOption ::
  DeploymentReadyOption
mkDeploymentReadyOption =
  DeploymentReadyOption'
    { actionOnTimeout = Core.Nothing,
      waitTimeInMinutes = Core.Nothing
    }

-- | Information about when to reroute traffic from an original environment to a replacement environment in a blue/green deployment.
--
--
--     * CONTINUE_DEPLOYMENT: Register new instances with the load balancer immediately after the new application revision is installed on the instances in the replacement environment.
--
--
--     * STOP_DEPLOYMENT: Do not register new instances with a load balancer unless traffic rerouting is started using 'ContinueDeployment' . If traffic rerouting is not started before the end of the specified wait period, the deployment status is changed to Stopped.
--
--
--
-- /Note:/ Consider using 'actionOnTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
droActionOnTimeout :: Lens.Lens' DeploymentReadyOption (Core.Maybe Types.DeploymentReadyAction)
droActionOnTimeout = Lens.field @"actionOnTimeout"
{-# DEPRECATED droActionOnTimeout "Use generic-lens or generic-optics with 'actionOnTimeout' instead." #-}

-- | The number of minutes to wait before the status of a blue/green deployment is changed to Stopped if rerouting is not started manually. Applies only to the @STOP_DEPLOYMENT@ option for @actionOnTimeout@ .
--
-- /Note:/ Consider using 'waitTimeInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
droWaitTimeInMinutes :: Lens.Lens' DeploymentReadyOption (Core.Maybe Core.Int)
droWaitTimeInMinutes = Lens.field @"waitTimeInMinutes"
{-# DEPRECATED droWaitTimeInMinutes "Use generic-lens or generic-optics with 'waitTimeInMinutes' instead." #-}

instance Core.FromJSON DeploymentReadyOption where
  toJSON DeploymentReadyOption {..} =
    Core.object
      ( Core.catMaybes
          [ ("actionOnTimeout" Core..=) Core.<$> actionOnTimeout,
            ("waitTimeInMinutes" Core..=) Core.<$> waitTimeInMinutes
          ]
      )

instance Core.FromJSON DeploymentReadyOption where
  parseJSON =
    Core.withObject "DeploymentReadyOption" Core.$
      \x ->
        DeploymentReadyOption'
          Core.<$> (x Core..:? "actionOnTimeout")
          Core.<*> (x Core..:? "waitTimeInMinutes")
