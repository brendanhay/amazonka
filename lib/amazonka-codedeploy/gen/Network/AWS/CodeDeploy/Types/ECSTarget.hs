{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    ecstDeploymentId,
    ecstLastUpdatedAt,
    ecstLifecycleEvents,
    ecstStatus,
    ecstTargetArn,
    ecstTargetId,
    ecstTaskSetsInfo,
  )
where

import qualified Network.AWS.CodeDeploy.Types.DeploymentId as Types
import qualified Network.AWS.CodeDeploy.Types.ECSTaskSet as Types
import qualified Network.AWS.CodeDeploy.Types.LifecycleEvent as Types
import qualified Network.AWS.CodeDeploy.Types.TargetArn as Types
import qualified Network.AWS.CodeDeploy.Types.TargetId as Types
import qualified Network.AWS.CodeDeploy.Types.TargetStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the target of an Amazon ECS deployment.
--
-- /See:/ 'mkECSTarget' smart constructor.
data ECSTarget = ECSTarget'
  { -- | The unique ID of a deployment.
    deploymentId :: Core.Maybe Types.DeploymentId,
    -- | The date and time when the target Amazon ECS application was updated by a deployment.
    lastUpdatedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The lifecycle events of the deployment to this target Amazon ECS application.
    lifecycleEvents :: Core.Maybe [Types.LifecycleEvent],
    -- | The status an Amazon ECS deployment's target ECS application.
    status :: Core.Maybe Types.TargetStatus,
    -- | The Amazon Resource Name (ARN) of the target.
    targetArn :: Core.Maybe Types.TargetArn,
    -- | The unique ID of a deployment target that has a type of @ecsTarget@ .
    targetId :: Core.Maybe Types.TargetId,
    -- | The @ECSTaskSet@ objects associated with the ECS target.
    taskSetsInfo :: Core.Maybe [Types.ECSTaskSet]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ECSTarget' value with any optional fields omitted.
mkECSTarget ::
  ECSTarget
mkECSTarget =
  ECSTarget'
    { deploymentId = Core.Nothing,
      lastUpdatedAt = Core.Nothing,
      lifecycleEvents = Core.Nothing,
      status = Core.Nothing,
      targetArn = Core.Nothing,
      targetId = Core.Nothing,
      taskSetsInfo = Core.Nothing
    }

-- | The unique ID of a deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstDeploymentId :: Lens.Lens' ECSTarget (Core.Maybe Types.DeploymentId)
ecstDeploymentId = Lens.field @"deploymentId"
{-# DEPRECATED ecstDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The date and time when the target Amazon ECS application was updated by a deployment.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstLastUpdatedAt :: Lens.Lens' ECSTarget (Core.Maybe Core.NominalDiffTime)
ecstLastUpdatedAt = Lens.field @"lastUpdatedAt"
{-# DEPRECATED ecstLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The lifecycle events of the deployment to this target Amazon ECS application.
--
-- /Note:/ Consider using 'lifecycleEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstLifecycleEvents :: Lens.Lens' ECSTarget (Core.Maybe [Types.LifecycleEvent])
ecstLifecycleEvents = Lens.field @"lifecycleEvents"
{-# DEPRECATED ecstLifecycleEvents "Use generic-lens or generic-optics with 'lifecycleEvents' instead." #-}

-- | The status an Amazon ECS deployment's target ECS application.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstStatus :: Lens.Lens' ECSTarget (Core.Maybe Types.TargetStatus)
ecstStatus = Lens.field @"status"
{-# DEPRECATED ecstStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Name (ARN) of the target.
--
-- /Note:/ Consider using 'targetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstTargetArn :: Lens.Lens' ECSTarget (Core.Maybe Types.TargetArn)
ecstTargetArn = Lens.field @"targetArn"
{-# DEPRECATED ecstTargetArn "Use generic-lens or generic-optics with 'targetArn' instead." #-}

-- | The unique ID of a deployment target that has a type of @ecsTarget@ .
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstTargetId :: Lens.Lens' ECSTarget (Core.Maybe Types.TargetId)
ecstTargetId = Lens.field @"targetId"
{-# DEPRECATED ecstTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

-- | The @ECSTaskSet@ objects associated with the ECS target.
--
-- /Note:/ Consider using 'taskSetsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecstTaskSetsInfo :: Lens.Lens' ECSTarget (Core.Maybe [Types.ECSTaskSet])
ecstTaskSetsInfo = Lens.field @"taskSetsInfo"
{-# DEPRECATED ecstTaskSetsInfo "Use generic-lens or generic-optics with 'taskSetsInfo' instead." #-}

instance Core.FromJSON ECSTarget where
  parseJSON =
    Core.withObject "ECSTarget" Core.$
      \x ->
        ECSTarget'
          Core.<$> (x Core..:? "deploymentId")
          Core.<*> (x Core..:? "lastUpdatedAt")
          Core.<*> (x Core..:? "lifecycleEvents")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "targetArn")
          Core.<*> (x Core..:? "targetId")
          Core.<*> (x Core..:? "taskSetsInfo")
