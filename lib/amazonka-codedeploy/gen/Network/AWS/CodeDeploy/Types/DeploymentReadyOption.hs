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

import Network.AWS.CodeDeploy.Types.DeploymentReadyAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
    actionOnTimeout :: Lude.Maybe DeploymentReadyAction,
    -- | The number of minutes to wait before the status of a blue/green deployment is changed to Stopped if rerouting is not started manually. Applies only to the @STOP_DEPLOYMENT@ option for @actionOnTimeout@ .
    waitTimeInMinutes :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeploymentReadyOption' with the minimum fields required to make a request.
--
-- * 'actionOnTimeout' - Information about when to reroute traffic from an original environment to a replacement environment in a blue/green deployment.
--
--
--     * CONTINUE_DEPLOYMENT: Register new instances with the load balancer immediately after the new application revision is installed on the instances in the replacement environment.
--
--
--     * STOP_DEPLOYMENT: Do not register new instances with a load balancer unless traffic rerouting is started using 'ContinueDeployment' . If traffic rerouting is not started before the end of the specified wait period, the deployment status is changed to Stopped.
--
--
-- * 'waitTimeInMinutes' - The number of minutes to wait before the status of a blue/green deployment is changed to Stopped if rerouting is not started manually. Applies only to the @STOP_DEPLOYMENT@ option for @actionOnTimeout@ .
mkDeploymentReadyOption ::
  DeploymentReadyOption
mkDeploymentReadyOption =
  DeploymentReadyOption'
    { actionOnTimeout = Lude.Nothing,
      waitTimeInMinutes = Lude.Nothing
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
droActionOnTimeout :: Lens.Lens' DeploymentReadyOption (Lude.Maybe DeploymentReadyAction)
droActionOnTimeout = Lens.lens (actionOnTimeout :: DeploymentReadyOption -> Lude.Maybe DeploymentReadyAction) (\s a -> s {actionOnTimeout = a} :: DeploymentReadyOption)
{-# DEPRECATED droActionOnTimeout "Use generic-lens or generic-optics with 'actionOnTimeout' instead." #-}

-- | The number of minutes to wait before the status of a blue/green deployment is changed to Stopped if rerouting is not started manually. Applies only to the @STOP_DEPLOYMENT@ option for @actionOnTimeout@ .
--
-- /Note:/ Consider using 'waitTimeInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
droWaitTimeInMinutes :: Lens.Lens' DeploymentReadyOption (Lude.Maybe Lude.Int)
droWaitTimeInMinutes = Lens.lens (waitTimeInMinutes :: DeploymentReadyOption -> Lude.Maybe Lude.Int) (\s a -> s {waitTimeInMinutes = a} :: DeploymentReadyOption)
{-# DEPRECATED droWaitTimeInMinutes "Use generic-lens or generic-optics with 'waitTimeInMinutes' instead." #-}

instance Lude.FromJSON DeploymentReadyOption where
  parseJSON =
    Lude.withObject
      "DeploymentReadyOption"
      ( \x ->
          DeploymentReadyOption'
            Lude.<$> (x Lude..:? "actionOnTimeout")
            Lude.<*> (x Lude..:? "waitTimeInMinutes")
      )

instance Lude.ToJSON DeploymentReadyOption where
  toJSON DeploymentReadyOption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("actionOnTimeout" Lude..=) Lude.<$> actionOnTimeout,
            ("waitTimeInMinutes" Lude..=) Lude.<$> waitTimeInMinutes
          ]
      )
