{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.BlueInstanceTerminationOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.BlueInstanceTerminationOption
  ( BlueInstanceTerminationOption (..)
  -- * Smart constructor
  , mkBlueInstanceTerminationOption
  -- * Lenses
  , bitoAction
  , bitoTerminationWaitTimeInMinutes
  ) where

import qualified Network.AWS.CodeDeploy.Types.InstanceAction as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about whether instances in the original environment are terminated when a blue/green deployment is successful. @BlueInstanceTerminationOption@ does not apply to Lambda deployments. 
--
-- /See:/ 'mkBlueInstanceTerminationOption' smart constructor.
data BlueInstanceTerminationOption = BlueInstanceTerminationOption'
  { action :: Core.Maybe Types.InstanceAction
    -- ^ The action to take on instances in the original environment after a successful blue/green deployment.
--
--
--     * @TERMINATE@ : Instances are terminated after a specified wait time.
--
--
--     * @KEEP_ALIVE@ : Instances are left running after they are deregistered from the load balancer and removed from the deployment group.
--
--
  , terminationWaitTimeInMinutes :: Core.Maybe Core.Int
    -- ^ For an Amazon EC2 deployment, the number of minutes to wait after a successful blue/green deployment before terminating instances from the original environment.
--
-- For an Amazon ECS deployment, the number of minutes before deleting the original (blue) task set. During an Amazon ECS deployment, CodeDeploy shifts traffic from the original (blue) task set to a replacement (green) task set. 
-- The maximum setting is 2880 minutes (2 days). 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BlueInstanceTerminationOption' value with any optional fields omitted.
mkBlueInstanceTerminationOption
    :: BlueInstanceTerminationOption
mkBlueInstanceTerminationOption
  = BlueInstanceTerminationOption'{action = Core.Nothing,
                                   terminationWaitTimeInMinutes = Core.Nothing}

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
bitoAction :: Lens.Lens' BlueInstanceTerminationOption (Core.Maybe Types.InstanceAction)
bitoAction = Lens.field @"action"
{-# INLINEABLE bitoAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | For an Amazon EC2 deployment, the number of minutes to wait after a successful blue/green deployment before terminating instances from the original environment.
--
-- For an Amazon ECS deployment, the number of minutes before deleting the original (blue) task set. During an Amazon ECS deployment, CodeDeploy shifts traffic from the original (blue) task set to a replacement (green) task set. 
-- The maximum setting is 2880 minutes (2 days). 
--
-- /Note:/ Consider using 'terminationWaitTimeInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bitoTerminationWaitTimeInMinutes :: Lens.Lens' BlueInstanceTerminationOption (Core.Maybe Core.Int)
bitoTerminationWaitTimeInMinutes = Lens.field @"terminationWaitTimeInMinutes"
{-# INLINEABLE bitoTerminationWaitTimeInMinutes #-}
{-# DEPRECATED terminationWaitTimeInMinutes "Use generic-lens or generic-optics with 'terminationWaitTimeInMinutes' instead"  #-}

instance Core.FromJSON BlueInstanceTerminationOption where
        toJSON BlueInstanceTerminationOption{..}
          = Core.object
              (Core.catMaybes
                 [("action" Core..=) Core.<$> action,
                  ("terminationWaitTimeInMinutes" Core..=) Core.<$>
                    terminationWaitTimeInMinutes])

instance Core.FromJSON BlueInstanceTerminationOption where
        parseJSON
          = Core.withObject "BlueInstanceTerminationOption" Core.$
              \ x ->
                BlueInstanceTerminationOption' Core.<$>
                  (x Core..:? "action") Core.<*>
                    x Core..:? "terminationWaitTimeInMinutes"
