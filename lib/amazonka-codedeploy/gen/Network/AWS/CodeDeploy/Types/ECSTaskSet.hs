-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.ECSTaskSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ECSTaskSet
  ( ECSTaskSet (..),

    -- * Smart constructor
    mkECSTaskSet,

    -- * Lenses
    ecstsRunningCount,
    ecstsStatus,
    ecstsIdentifer,
    ecstsDesiredCount,
    ecstsPendingCount,
    ecstsTrafficWeight,
    ecstsTargetGroup,
    ecstsTaskSetLabel,
  )
where

import Network.AWS.CodeDeploy.Types.TargetGroupInfo
import Network.AWS.CodeDeploy.Types.TargetLabel
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a set of Amazon ECS tasks in an AWS CodeDeploy deployment. An Amazon ECS task set includes details such as the desired number of tasks, how many tasks are running, and whether the task set serves production traffic. An AWS CodeDeploy application that uses the Amazon ECS compute platform deploys a containerized application in an Amazon ECS service as a task set.
--
-- /See:/ 'mkECSTaskSet' smart constructor.
data ECSTaskSet = ECSTaskSet'
  { runningCount ::
      Lude.Maybe Lude.Integer,
    status :: Lude.Maybe Lude.Text,
    identifer :: Lude.Maybe Lude.Text,
    desiredCount :: Lude.Maybe Lude.Integer,
    pendingCount :: Lude.Maybe Lude.Integer,
    trafficWeight :: Lude.Maybe Lude.Double,
    targetGroup :: Lude.Maybe TargetGroupInfo,
    taskSetLabel :: Lude.Maybe TargetLabel
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ECSTaskSet' with the minimum fields required to make a request.
--
-- * 'desiredCount' - The number of tasks in a task set. During a deployment that uses the Amazon ECS compute type, CodeDeploy instructs Amazon ECS to create a new task set and uses this value to determine how many tasks to create. After the updated task set is created, CodeDeploy shifts traffic to the new task set.
-- * 'identifer' - A unique ID of an @ECSTaskSet@ .
-- * 'pendingCount' - The number of tasks in the task set that are in the @PENDING@ status during an Amazon ECS deployment. A task in the @PENDING@ state is preparing to enter the @RUNNING@ state. A task set enters the @PENDING@ status when it launches for the first time, or when it is restarted after being in the @STOPPED@ state.
-- * 'runningCount' - The number of tasks in the task set that are in the @RUNNING@ status during an Amazon ECS deployment. A task in the @RUNNING@ state is running and ready for use.
-- * 'status' - The status of the task set. There are three valid task set statuses:
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
-- * 'targetGroup' - The target group associated with the task set. The target group is used by AWS CodeDeploy to manage traffic to a task set.
-- * 'taskSetLabel' - A label that identifies whether the ECS task set is an original target (@BLUE@ ) or a replacement target (@GREEN@ ).
-- * 'trafficWeight' - The percentage of traffic served by this task set.
mkECSTaskSet ::
  ECSTaskSet
mkECSTaskSet =
  ECSTaskSet'
    { runningCount = Lude.Nothing,
      status = Lude.Nothing,
      identifer = Lude.Nothing,
      desiredCount = Lude.Nothing,
      pendingCount = Lude.Nothing,
      trafficWeight = Lude.Nothing,
      targetGroup = Lude.Nothing,
      taskSetLabel = Lude.Nothing
    }

-- | The number of tasks in the task set that are in the @RUNNING@ status during an Amazon ECS deployment. A task in the @RUNNING@ state is running and ready for use.
--
-- /Note:/ Consider using 'runningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstsRunningCount :: Lens.Lens' ECSTaskSet (Lude.Maybe Lude.Integer)
ecstsRunningCount = Lens.lens (runningCount :: ECSTaskSet -> Lude.Maybe Lude.Integer) (\s a -> s {runningCount = a} :: ECSTaskSet)
{-# DEPRECATED ecstsRunningCount "Use generic-lens or generic-optics with 'runningCount' instead." #-}

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
ecstsStatus :: Lens.Lens' ECSTaskSet (Lude.Maybe Lude.Text)
ecstsStatus = Lens.lens (status :: ECSTaskSet -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ECSTaskSet)
{-# DEPRECATED ecstsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A unique ID of an @ECSTaskSet@ .
--
-- /Note:/ Consider using 'identifer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstsIdentifer :: Lens.Lens' ECSTaskSet (Lude.Maybe Lude.Text)
ecstsIdentifer = Lens.lens (identifer :: ECSTaskSet -> Lude.Maybe Lude.Text) (\s a -> s {identifer = a} :: ECSTaskSet)
{-# DEPRECATED ecstsIdentifer "Use generic-lens or generic-optics with 'identifer' instead." #-}

-- | The number of tasks in a task set. During a deployment that uses the Amazon ECS compute type, CodeDeploy instructs Amazon ECS to create a new task set and uses this value to determine how many tasks to create. After the updated task set is created, CodeDeploy shifts traffic to the new task set.
--
-- /Note:/ Consider using 'desiredCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstsDesiredCount :: Lens.Lens' ECSTaskSet (Lude.Maybe Lude.Integer)
ecstsDesiredCount = Lens.lens (desiredCount :: ECSTaskSet -> Lude.Maybe Lude.Integer) (\s a -> s {desiredCount = a} :: ECSTaskSet)
{-# DEPRECATED ecstsDesiredCount "Use generic-lens or generic-optics with 'desiredCount' instead." #-}

-- | The number of tasks in the task set that are in the @PENDING@ status during an Amazon ECS deployment. A task in the @PENDING@ state is preparing to enter the @RUNNING@ state. A task set enters the @PENDING@ status when it launches for the first time, or when it is restarted after being in the @STOPPED@ state.
--
-- /Note:/ Consider using 'pendingCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstsPendingCount :: Lens.Lens' ECSTaskSet (Lude.Maybe Lude.Integer)
ecstsPendingCount = Lens.lens (pendingCount :: ECSTaskSet -> Lude.Maybe Lude.Integer) (\s a -> s {pendingCount = a} :: ECSTaskSet)
{-# DEPRECATED ecstsPendingCount "Use generic-lens or generic-optics with 'pendingCount' instead." #-}

-- | The percentage of traffic served by this task set.
--
-- /Note:/ Consider using 'trafficWeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstsTrafficWeight :: Lens.Lens' ECSTaskSet (Lude.Maybe Lude.Double)
ecstsTrafficWeight = Lens.lens (trafficWeight :: ECSTaskSet -> Lude.Maybe Lude.Double) (\s a -> s {trafficWeight = a} :: ECSTaskSet)
{-# DEPRECATED ecstsTrafficWeight "Use generic-lens or generic-optics with 'trafficWeight' instead." #-}

-- | The target group associated with the task set. The target group is used by AWS CodeDeploy to manage traffic to a task set.
--
-- /Note:/ Consider using 'targetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstsTargetGroup :: Lens.Lens' ECSTaskSet (Lude.Maybe TargetGroupInfo)
ecstsTargetGroup = Lens.lens (targetGroup :: ECSTaskSet -> Lude.Maybe TargetGroupInfo) (\s a -> s {targetGroup = a} :: ECSTaskSet)
{-# DEPRECATED ecstsTargetGroup "Use generic-lens or generic-optics with 'targetGroup' instead." #-}

-- | A label that identifies whether the ECS task set is an original target (@BLUE@ ) or a replacement target (@GREEN@ ).
--
-- /Note:/ Consider using 'taskSetLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstsTaskSetLabel :: Lens.Lens' ECSTaskSet (Lude.Maybe TargetLabel)
ecstsTaskSetLabel = Lens.lens (taskSetLabel :: ECSTaskSet -> Lude.Maybe TargetLabel) (\s a -> s {taskSetLabel = a} :: ECSTaskSet)
{-# DEPRECATED ecstsTaskSetLabel "Use generic-lens or generic-optics with 'taskSetLabel' instead." #-}

instance Lude.FromJSON ECSTaskSet where
  parseJSON =
    Lude.withObject
      "ECSTaskSet"
      ( \x ->
          ECSTaskSet'
            Lude.<$> (x Lude..:? "runningCount")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "identifer")
            Lude.<*> (x Lude..:? "desiredCount")
            Lude.<*> (x Lude..:? "pendingCount")
            Lude.<*> (x Lude..:? "trafficWeight")
            Lude.<*> (x Lude..:? "targetGroup")
            Lude.<*> (x Lude..:? "taskSetLabel")
      )
