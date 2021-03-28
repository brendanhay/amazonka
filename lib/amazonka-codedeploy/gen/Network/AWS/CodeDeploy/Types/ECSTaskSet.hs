{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.ECSTaskSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.ECSTaskSet
  ( ECSTaskSet (..)
  -- * Smart constructor
  , mkECSTaskSet
  -- * Lenses
  , ecstsDesiredCount
  , ecstsIdentifer
  , ecstsPendingCount
  , ecstsRunningCount
  , ecstsStatus
  , ecstsTargetGroup
  , ecstsTaskSetLabel
  , ecstsTrafficWeight
  ) where

import qualified Network.AWS.CodeDeploy.Types.ECSTaskSetStatus as Types
import qualified Network.AWS.CodeDeploy.Types.Identifer as Types
import qualified Network.AWS.CodeDeploy.Types.TargetGroupInfo as Types
import qualified Network.AWS.CodeDeploy.Types.TargetLabel as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a set of Amazon ECS tasks in an AWS CodeDeploy deployment. An Amazon ECS task set includes details such as the desired number of tasks, how many tasks are running, and whether the task set serves production traffic. An AWS CodeDeploy application that uses the Amazon ECS compute platform deploys a containerized application in an Amazon ECS service as a task set. 
--
-- /See:/ 'mkECSTaskSet' smart constructor.
data ECSTaskSet = ECSTaskSet'
  { desiredCount :: Core.Maybe Core.Integer
    -- ^ The number of tasks in a task set. During a deployment that uses the Amazon ECS compute type, CodeDeploy instructs Amazon ECS to create a new task set and uses this value to determine how many tasks to create. After the updated task set is created, CodeDeploy shifts traffic to the new task set. 
  , identifer :: Core.Maybe Types.Identifer
    -- ^ A unique ID of an @ECSTaskSet@ . 
  , pendingCount :: Core.Maybe Core.Integer
    -- ^ The number of tasks in the task set that are in the @PENDING@ status during an Amazon ECS deployment. A task in the @PENDING@ state is preparing to enter the @RUNNING@ state. A task set enters the @PENDING@ status when it launches for the first time, or when it is restarted after being in the @STOPPED@ state. 
  , runningCount :: Core.Maybe Core.Integer
    -- ^ The number of tasks in the task set that are in the @RUNNING@ status during an Amazon ECS deployment. A task in the @RUNNING@ state is running and ready for use. 
  , status :: Core.Maybe Types.ECSTaskSetStatus
    -- ^ The status of the task set. There are three valid task set statuses: 
--
--
--     * @PRIMARY@ : Indicates the task set is serving production traffic. 
--
--
--     * @ACTIVE@ : Indicates the task set is not serving production traffic. 
--
--
--     * @DRAINING@ : Indicates the tasks in the task set are being stopped and their corresponding targets are being deregistered from their target group. 
--
--
  , targetGroup :: Core.Maybe Types.TargetGroupInfo
    -- ^ The target group associated with the task set. The target group is used by AWS CodeDeploy to manage traffic to a task set. 
  , taskSetLabel :: Core.Maybe Types.TargetLabel
    -- ^ A label that identifies whether the ECS task set is an original target (@BLUE@ ) or a replacement target (@GREEN@ ). 
  , trafficWeight :: Core.Maybe Core.Double
    -- ^ The percentage of traffic served by this task set. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ECSTaskSet' value with any optional fields omitted.
mkECSTaskSet
    :: ECSTaskSet
mkECSTaskSet
  = ECSTaskSet'{desiredCount = Core.Nothing,
                identifer = Core.Nothing, pendingCount = Core.Nothing,
                runningCount = Core.Nothing, status = Core.Nothing,
                targetGroup = Core.Nothing, taskSetLabel = Core.Nothing,
                trafficWeight = Core.Nothing}

-- | The number of tasks in a task set. During a deployment that uses the Amazon ECS compute type, CodeDeploy instructs Amazon ECS to create a new task set and uses this value to determine how many tasks to create. After the updated task set is created, CodeDeploy shifts traffic to the new task set. 
--
-- /Note:/ Consider using 'desiredCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstsDesiredCount :: Lens.Lens' ECSTaskSet (Core.Maybe Core.Integer)
ecstsDesiredCount = Lens.field @"desiredCount"
{-# INLINEABLE ecstsDesiredCount #-}
{-# DEPRECATED desiredCount "Use generic-lens or generic-optics with 'desiredCount' instead"  #-}

-- | A unique ID of an @ECSTaskSet@ . 
--
-- /Note:/ Consider using 'identifer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstsIdentifer :: Lens.Lens' ECSTaskSet (Core.Maybe Types.Identifer)
ecstsIdentifer = Lens.field @"identifer"
{-# INLINEABLE ecstsIdentifer #-}
{-# DEPRECATED identifer "Use generic-lens or generic-optics with 'identifer' instead"  #-}

-- | The number of tasks in the task set that are in the @PENDING@ status during an Amazon ECS deployment. A task in the @PENDING@ state is preparing to enter the @RUNNING@ state. A task set enters the @PENDING@ status when it launches for the first time, or when it is restarted after being in the @STOPPED@ state. 
--
-- /Note:/ Consider using 'pendingCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstsPendingCount :: Lens.Lens' ECSTaskSet (Core.Maybe Core.Integer)
ecstsPendingCount = Lens.field @"pendingCount"
{-# INLINEABLE ecstsPendingCount #-}
{-# DEPRECATED pendingCount "Use generic-lens or generic-optics with 'pendingCount' instead"  #-}

-- | The number of tasks in the task set that are in the @RUNNING@ status during an Amazon ECS deployment. A task in the @RUNNING@ state is running and ready for use. 
--
-- /Note:/ Consider using 'runningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstsRunningCount :: Lens.Lens' ECSTaskSet (Core.Maybe Core.Integer)
ecstsRunningCount = Lens.field @"runningCount"
{-# INLINEABLE ecstsRunningCount #-}
{-# DEPRECATED runningCount "Use generic-lens or generic-optics with 'runningCount' instead"  #-}

-- | The status of the task set. There are three valid task set statuses: 
--
--
--     * @PRIMARY@ : Indicates the task set is serving production traffic. 
--
--
--     * @ACTIVE@ : Indicates the task set is not serving production traffic. 
--
--
--     * @DRAINING@ : Indicates the tasks in the task set are being stopped and their corresponding targets are being deregistered from their target group. 
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstsStatus :: Lens.Lens' ECSTaskSet (Core.Maybe Types.ECSTaskSetStatus)
ecstsStatus = Lens.field @"status"
{-# INLINEABLE ecstsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The target group associated with the task set. The target group is used by AWS CodeDeploy to manage traffic to a task set. 
--
-- /Note:/ Consider using 'targetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstsTargetGroup :: Lens.Lens' ECSTaskSet (Core.Maybe Types.TargetGroupInfo)
ecstsTargetGroup = Lens.field @"targetGroup"
{-# INLINEABLE ecstsTargetGroup #-}
{-# DEPRECATED targetGroup "Use generic-lens or generic-optics with 'targetGroup' instead"  #-}

-- | A label that identifies whether the ECS task set is an original target (@BLUE@ ) or a replacement target (@GREEN@ ). 
--
-- /Note:/ Consider using 'taskSetLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstsTaskSetLabel :: Lens.Lens' ECSTaskSet (Core.Maybe Types.TargetLabel)
ecstsTaskSetLabel = Lens.field @"taskSetLabel"
{-# INLINEABLE ecstsTaskSetLabel #-}
{-# DEPRECATED taskSetLabel "Use generic-lens or generic-optics with 'taskSetLabel' instead"  #-}

-- | The percentage of traffic served by this task set. 
--
-- /Note:/ Consider using 'trafficWeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstsTrafficWeight :: Lens.Lens' ECSTaskSet (Core.Maybe Core.Double)
ecstsTrafficWeight = Lens.field @"trafficWeight"
{-# INLINEABLE ecstsTrafficWeight #-}
{-# DEPRECATED trafficWeight "Use generic-lens or generic-optics with 'trafficWeight' instead"  #-}

instance Core.FromJSON ECSTaskSet where
        parseJSON
          = Core.withObject "ECSTaskSet" Core.$
              \ x ->
                ECSTaskSet' Core.<$>
                  (x Core..:? "desiredCount") Core.<*> x Core..:? "identifer"
                    Core.<*> x Core..:? "pendingCount"
                    Core.<*> x Core..:? "runningCount"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "targetGroup"
                    Core.<*> x Core..:? "taskSetLabel"
                    Core.<*> x Core..:? "trafficWeight"
