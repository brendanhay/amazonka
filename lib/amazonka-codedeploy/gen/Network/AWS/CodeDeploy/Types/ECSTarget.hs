-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.ECSTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ECSTarget
  ( ECSTarget (..),

    -- * Smart constructor
    mkECSTarget,

    -- * Lenses
    ecstTargetARN,
    ecstTargetId,
    ecstStatus,
    ecstDeploymentId,
    ecstLastUpdatedAt,
    ecstTaskSetsInfo,
    ecstLifecycleEvents,
  )
where

import Network.AWS.CodeDeploy.Types.ECSTaskSet
import Network.AWS.CodeDeploy.Types.LifecycleEvent
import Network.AWS.CodeDeploy.Types.TargetStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the target of an Amazon ECS deployment.
--
-- /See:/ 'mkECSTarget' smart constructor.
data ECSTarget = ECSTarget'
  { targetARN :: Lude.Maybe Lude.Text,
    targetId :: Lude.Maybe Lude.Text,
    status :: Lude.Maybe TargetStatus,
    deploymentId :: Lude.Maybe Lude.Text,
    lastUpdatedAt :: Lude.Maybe Lude.Timestamp,
    taskSetsInfo :: Lude.Maybe [ECSTaskSet],
    lifecycleEvents :: Lude.Maybe [LifecycleEvent]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ECSTarget' with the minimum fields required to make a request.
--
-- * 'deploymentId' - The unique ID of a deployment.
-- * 'lastUpdatedAt' - The date and time when the target Amazon ECS application was updated by a deployment.
-- * 'lifecycleEvents' - The lifecycle events of the deployment to this target Amazon ECS application.
-- * 'status' - The status an Amazon ECS deployment's target ECS application.
-- * 'targetARN' - The Amazon Resource Name (ARN) of the target.
-- * 'targetId' - The unique ID of a deployment target that has a type of @ecsTarget@ .
-- * 'taskSetsInfo' - The @ECSTaskSet@ objects associated with the ECS target.
mkECSTarget ::
  ECSTarget
mkECSTarget =
  ECSTarget'
    { targetARN = Lude.Nothing,
      targetId = Lude.Nothing,
      status = Lude.Nothing,
      deploymentId = Lude.Nothing,
      lastUpdatedAt = Lude.Nothing,
      taskSetsInfo = Lude.Nothing,
      lifecycleEvents = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the target.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstTargetARN :: Lens.Lens' ECSTarget (Lude.Maybe Lude.Text)
ecstTargetARN = Lens.lens (targetARN :: ECSTarget -> Lude.Maybe Lude.Text) (\s a -> s {targetARN = a} :: ECSTarget)
{-# DEPRECATED ecstTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | The unique ID of a deployment target that has a type of @ecsTarget@ .
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstTargetId :: Lens.Lens' ECSTarget (Lude.Maybe Lude.Text)
ecstTargetId = Lens.lens (targetId :: ECSTarget -> Lude.Maybe Lude.Text) (\s a -> s {targetId = a} :: ECSTarget)
{-# DEPRECATED ecstTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

-- | The status an Amazon ECS deployment's target ECS application.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstStatus :: Lens.Lens' ECSTarget (Lude.Maybe TargetStatus)
ecstStatus = Lens.lens (status :: ECSTarget -> Lude.Maybe TargetStatus) (\s a -> s {status = a} :: ECSTarget)
{-# DEPRECATED ecstStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique ID of a deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstDeploymentId :: Lens.Lens' ECSTarget (Lude.Maybe Lude.Text)
ecstDeploymentId = Lens.lens (deploymentId :: ECSTarget -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: ECSTarget)
{-# DEPRECATED ecstDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The date and time when the target Amazon ECS application was updated by a deployment.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstLastUpdatedAt :: Lens.Lens' ECSTarget (Lude.Maybe Lude.Timestamp)
ecstLastUpdatedAt = Lens.lens (lastUpdatedAt :: ECSTarget -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedAt = a} :: ECSTarget)
{-# DEPRECATED ecstLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The @ECSTaskSet@ objects associated with the ECS target.
--
-- /Note:/ Consider using 'taskSetsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstTaskSetsInfo :: Lens.Lens' ECSTarget (Lude.Maybe [ECSTaskSet])
ecstTaskSetsInfo = Lens.lens (taskSetsInfo :: ECSTarget -> Lude.Maybe [ECSTaskSet]) (\s a -> s {taskSetsInfo = a} :: ECSTarget)
{-# DEPRECATED ecstTaskSetsInfo "Use generic-lens or generic-optics with 'taskSetsInfo' instead." #-}

-- | The lifecycle events of the deployment to this target Amazon ECS application.
--
-- /Note:/ Consider using 'lifecycleEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstLifecycleEvents :: Lens.Lens' ECSTarget (Lude.Maybe [LifecycleEvent])
ecstLifecycleEvents = Lens.lens (lifecycleEvents :: ECSTarget -> Lude.Maybe [LifecycleEvent]) (\s a -> s {lifecycleEvents = a} :: ECSTarget)
{-# DEPRECATED ecstLifecycleEvents "Use generic-lens or generic-optics with 'lifecycleEvents' instead." #-}

instance Lude.FromJSON ECSTarget where
  parseJSON =
    Lude.withObject
      "ECSTarget"
      ( \x ->
          ECSTarget'
            Lude.<$> (x Lude..:? "targetArn")
            Lude.<*> (x Lude..:? "targetId")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "deploymentId")
            Lude.<*> (x Lude..:? "lastUpdatedAt")
            Lude.<*> (x Lude..:? "taskSetsInfo" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "lifecycleEvents" Lude..!= Lude.mempty)
      )
