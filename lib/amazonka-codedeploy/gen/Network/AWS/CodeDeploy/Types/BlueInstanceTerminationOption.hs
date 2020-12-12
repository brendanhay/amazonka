{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.BlueInstanceTerminationOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.BlueInstanceTerminationOption
  ( BlueInstanceTerminationOption (..),

    -- * Smart constructor
    mkBlueInstanceTerminationOption,

    -- * Lenses
    bitoAction,
    bitoTerminationWaitTimeInMinutes,
  )
where

import Network.AWS.CodeDeploy.Types.InstanceAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about whether instances in the original environment are terminated when a blue/green deployment is successful. @BlueInstanceTerminationOption@ does not apply to Lambda deployments.
--
-- /See:/ 'mkBlueInstanceTerminationOption' smart constructor.
data BlueInstanceTerminationOption = BlueInstanceTerminationOption'
  { action ::
      Lude.Maybe InstanceAction,
    terminationWaitTimeInMinutes ::
      Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BlueInstanceTerminationOption' with the minimum fields required to make a request.
--
-- * 'action' - The action to take on instances in the original environment after a successful blue/green deployment.
--
--
--     * @TERMINATE@ : Instances are terminated after a specified wait time.
--
--
--     * @KEEP_ALIVE@ : Instances are left running after they are deregistered from the load balancer and removed from the deployment group.
--
--
-- * 'terminationWaitTimeInMinutes' - For an Amazon EC2 deployment, the number of minutes to wait after a successful blue/green deployment before terminating instances from the original environment.
--
-- For an Amazon ECS deployment, the number of minutes before deleting the original (blue) task set. During an Amazon ECS deployment, CodeDeploy shifts traffic from the original (blue) task set to a replacement (green) task set.
-- The maximum setting is 2880 minutes (2 days).
mkBlueInstanceTerminationOption ::
  BlueInstanceTerminationOption
mkBlueInstanceTerminationOption =
  BlueInstanceTerminationOption'
    { action = Lude.Nothing,
      terminationWaitTimeInMinutes = Lude.Nothing
    }

-- | The action to take on instances in the original environment after a successful blue/green deployment.
--
--
--     * @TERMINATE@ : Instances are terminated after a specified wait time.
--
--
--     * @KEEP_ALIVE@ : Instances are left running after they are deregistered from the load balancer and removed from the deployment group.
--
--
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bitoAction :: Lens.Lens' BlueInstanceTerminationOption (Lude.Maybe InstanceAction)
bitoAction = Lens.lens (action :: BlueInstanceTerminationOption -> Lude.Maybe InstanceAction) (\s a -> s {action = a} :: BlueInstanceTerminationOption)
{-# DEPRECATED bitoAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | For an Amazon EC2 deployment, the number of minutes to wait after a successful blue/green deployment before terminating instances from the original environment.
--
-- For an Amazon ECS deployment, the number of minutes before deleting the original (blue) task set. During an Amazon ECS deployment, CodeDeploy shifts traffic from the original (blue) task set to a replacement (green) task set.
-- The maximum setting is 2880 minutes (2 days).
--
-- /Note:/ Consider using 'terminationWaitTimeInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bitoTerminationWaitTimeInMinutes :: Lens.Lens' BlueInstanceTerminationOption (Lude.Maybe Lude.Int)
bitoTerminationWaitTimeInMinutes = Lens.lens (terminationWaitTimeInMinutes :: BlueInstanceTerminationOption -> Lude.Maybe Lude.Int) (\s a -> s {terminationWaitTimeInMinutes = a} :: BlueInstanceTerminationOption)
{-# DEPRECATED bitoTerminationWaitTimeInMinutes "Use generic-lens or generic-optics with 'terminationWaitTimeInMinutes' instead." #-}

instance Lude.FromJSON BlueInstanceTerminationOption where
  parseJSON =
    Lude.withObject
      "BlueInstanceTerminationOption"
      ( \x ->
          BlueInstanceTerminationOption'
            Lude.<$> (x Lude..:? "action")
            Lude.<*> (x Lude..:? "terminationWaitTimeInMinutes")
      )

instance Lude.ToJSON BlueInstanceTerminationOption where
  toJSON BlueInstanceTerminationOption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("action" Lude..=) Lude.<$> action,
            ("terminationWaitTimeInMinutes" Lude..=)
              Lude.<$> terminationWaitTimeInMinutes
          ]
      )
