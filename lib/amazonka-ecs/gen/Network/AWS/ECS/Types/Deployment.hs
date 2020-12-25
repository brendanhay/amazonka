{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Deployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Deployment
  ( Deployment (..),

    -- * Smart constructor
    mkDeployment,

    -- * Lenses
    dCapacityProviderStrategy,
    dCreatedAt,
    dDesiredCount,
    dFailedTasks,
    dId,
    dLaunchType,
    dNetworkConfiguration,
    dPendingCount,
    dPlatformVersion,
    dRolloutState,
    dRolloutStateReason,
    dRunningCount,
    dStatus,
    dTaskDefinition,
    dUpdatedAt,
  )
where

import qualified Network.AWS.ECS.Types.CapacityProviderStrategyItem as Types
import qualified Network.AWS.ECS.Types.DeploymentRolloutState as Types
import qualified Network.AWS.ECS.Types.LaunchType as Types
import qualified Network.AWS.ECS.Types.NetworkConfiguration as Types
import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of an Amazon ECS service deployment. This is used only when a service uses the @ECS@ deployment controller type.
--
-- /See:/ 'mkDeployment' smart constructor.
data Deployment = Deployment'
  { -- | The capacity provider strategy that the deployment is using.
    capacityProviderStrategy :: Core.Maybe [Types.CapacityProviderStrategyItem],
    -- | The Unix timestamp for when the service deployment was created.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | The most recent desired count of tasks that was specified for the service to deploy or maintain.
    desiredCount :: Core.Maybe Core.Int,
    -- | The number of consecutively failed tasks in the deployment. A task is considered a failure if the service scheduler can't launch the task, the task doesn't transition to a @RUNNING@ state, or if it fails any of its defined health checks and is stopped.
    failedTasks :: Core.Maybe Core.Int,
    -- | The ID of the deployment.
    id :: Core.Maybe Types.String,
    -- | The launch type the tasks in the service are using. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
    launchType :: Core.Maybe Types.LaunchType,
    -- | The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
    networkConfiguration :: Core.Maybe Types.NetworkConfiguration,
    -- | The number of tasks in the deployment that are in the @PENDING@ status.
    pendingCount :: Core.Maybe Core.Int,
    -- | The platform version on which your tasks in the service are running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
    platformVersion :: Core.Maybe Types.String,
    -- | The rollout state of the deployment. When a service deployment is started, it begins in an @IN_PROGRESS@ state. When the service reaches a steady state, the deployment will transition to a @COMPLETED@ state. If the service fails to reach a steady state and circuit breaker is enabled, the deployment will transition to a @FAILED@ state. A deployment in @FAILED@ state will launch no new tasks. For more information, see 'DeploymentCircuitBreaker' .
    rolloutState :: Core.Maybe Types.DeploymentRolloutState,
    -- | A description of the rollout state of a deployment.
    rolloutStateReason :: Core.Maybe Types.String,
    -- | The number of tasks in the deployment that are in the @RUNNING@ status.
    runningCount :: Core.Maybe Core.Int,
    -- | The status of the deployment. The following describes each state:
    --
    --
    --     * PRIMARY
    --
    --     * The most recent deployment of a service.
    --
    --
    --     * ACTIVE
    --
    --     * A service deployment that still has running tasks, but are in the process of being replaced with a new @PRIMARY@ deployment.
    --
    --
    --     * INACTIVE
    --
    --     * A deployment that has been completely replaced.
    status :: Core.Maybe Types.String,
    -- | The most recent task definition that was specified for the tasks in the service to use.
    taskDefinition :: Core.Maybe Types.String,
    -- | The Unix timestamp for when the service deployment was last updated.
    updatedAt :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Deployment' value with any optional fields omitted.
mkDeployment ::
  Deployment
mkDeployment =
  Deployment'
    { capacityProviderStrategy = Core.Nothing,
      createdAt = Core.Nothing,
      desiredCount = Core.Nothing,
      failedTasks = Core.Nothing,
      id = Core.Nothing,
      launchType = Core.Nothing,
      networkConfiguration = Core.Nothing,
      pendingCount = Core.Nothing,
      platformVersion = Core.Nothing,
      rolloutState = Core.Nothing,
      rolloutStateReason = Core.Nothing,
      runningCount = Core.Nothing,
      status = Core.Nothing,
      taskDefinition = Core.Nothing,
      updatedAt = Core.Nothing
    }

-- | The capacity provider strategy that the deployment is using.
--
-- /Note:/ Consider using 'capacityProviderStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCapacityProviderStrategy :: Lens.Lens' Deployment (Core.Maybe [Types.CapacityProviderStrategyItem])
dCapacityProviderStrategy = Lens.field @"capacityProviderStrategy"
{-# DEPRECATED dCapacityProviderStrategy "Use generic-lens or generic-optics with 'capacityProviderStrategy' instead." #-}

-- | The Unix timestamp for when the service deployment was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreatedAt :: Lens.Lens' Deployment (Core.Maybe Core.NominalDiffTime)
dCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED dCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The most recent desired count of tasks that was specified for the service to deploy or maintain.
--
-- /Note:/ Consider using 'desiredCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDesiredCount :: Lens.Lens' Deployment (Core.Maybe Core.Int)
dDesiredCount = Lens.field @"desiredCount"
{-# DEPRECATED dDesiredCount "Use generic-lens or generic-optics with 'desiredCount' instead." #-}

-- | The number of consecutively failed tasks in the deployment. A task is considered a failure if the service scheduler can't launch the task, the task doesn't transition to a @RUNNING@ state, or if it fails any of its defined health checks and is stopped.
--
-- /Note:/ Consider using 'failedTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFailedTasks :: Lens.Lens' Deployment (Core.Maybe Core.Int)
dFailedTasks = Lens.field @"failedTasks"
{-# DEPRECATED dFailedTasks "Use generic-lens or generic-optics with 'failedTasks' instead." #-}

-- | The ID of the deployment.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' Deployment (Core.Maybe Types.String)
dId = Lens.field @"id"
{-# DEPRECATED dId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The launch type the tasks in the service are using. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'launchType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLaunchType :: Lens.Lens' Deployment (Core.Maybe Types.LaunchType)
dLaunchType = Lens.field @"launchType"
{-# DEPRECATED dLaunchType "Use generic-lens or generic-optics with 'launchType' instead." #-}

-- | The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
--
-- /Note:/ Consider using 'networkConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNetworkConfiguration :: Lens.Lens' Deployment (Core.Maybe Types.NetworkConfiguration)
dNetworkConfiguration = Lens.field @"networkConfiguration"
{-# DEPRECATED dNetworkConfiguration "Use generic-lens or generic-optics with 'networkConfiguration' instead." #-}

-- | The number of tasks in the deployment that are in the @PENDING@ status.
--
-- /Note:/ Consider using 'pendingCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPendingCount :: Lens.Lens' Deployment (Core.Maybe Core.Int)
dPendingCount = Lens.field @"pendingCount"
{-# DEPRECATED dPendingCount "Use generic-lens or generic-optics with 'pendingCount' instead." #-}

-- | The platform version on which your tasks in the service are running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPlatformVersion :: Lens.Lens' Deployment (Core.Maybe Types.String)
dPlatformVersion = Lens.field @"platformVersion"
{-# DEPRECATED dPlatformVersion "Use generic-lens or generic-optics with 'platformVersion' instead." #-}

-- | The rollout state of the deployment. When a service deployment is started, it begins in an @IN_PROGRESS@ state. When the service reaches a steady state, the deployment will transition to a @COMPLETED@ state. If the service fails to reach a steady state and circuit breaker is enabled, the deployment will transition to a @FAILED@ state. A deployment in @FAILED@ state will launch no new tasks. For more information, see 'DeploymentCircuitBreaker' .
--
-- /Note:/ Consider using 'rolloutState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRolloutState :: Lens.Lens' Deployment (Core.Maybe Types.DeploymentRolloutState)
dRolloutState = Lens.field @"rolloutState"
{-# DEPRECATED dRolloutState "Use generic-lens or generic-optics with 'rolloutState' instead." #-}

-- | A description of the rollout state of a deployment.
--
-- /Note:/ Consider using 'rolloutStateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRolloutStateReason :: Lens.Lens' Deployment (Core.Maybe Types.String)
dRolloutStateReason = Lens.field @"rolloutStateReason"
{-# DEPRECATED dRolloutStateReason "Use generic-lens or generic-optics with 'rolloutStateReason' instead." #-}

-- | The number of tasks in the deployment that are in the @RUNNING@ status.
--
-- /Note:/ Consider using 'runningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRunningCount :: Lens.Lens' Deployment (Core.Maybe Core.Int)
dRunningCount = Lens.field @"runningCount"
{-# DEPRECATED dRunningCount "Use generic-lens or generic-optics with 'runningCount' instead." #-}

-- | The status of the deployment. The following describes each state:
--
--
--     * PRIMARY
--
--     * The most recent deployment of a service.
--
--
--     * ACTIVE
--
--     * A service deployment that still has running tasks, but are in the process of being replaced with a new @PRIMARY@ deployment.
--
--
--     * INACTIVE
--
--     * A deployment that has been completely replaced.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStatus :: Lens.Lens' Deployment (Core.Maybe Types.String)
dStatus = Lens.field @"status"
{-# DEPRECATED dStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The most recent task definition that was specified for the tasks in the service to use.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTaskDefinition :: Lens.Lens' Deployment (Core.Maybe Types.String)
dTaskDefinition = Lens.field @"taskDefinition"
{-# DEPRECATED dTaskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead." #-}

-- | The Unix timestamp for when the service deployment was last updated.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUpdatedAt :: Lens.Lens' Deployment (Core.Maybe Core.NominalDiffTime)
dUpdatedAt = Lens.field @"updatedAt"
{-# DEPRECATED dUpdatedAt "Use generic-lens or generic-optics with 'updatedAt' instead." #-}

instance Core.FromJSON Deployment where
  parseJSON =
    Core.withObject "Deployment" Core.$
      \x ->
        Deployment'
          Core.<$> (x Core..:? "capacityProviderStrategy")
          Core.<*> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "desiredCount")
          Core.<*> (x Core..:? "failedTasks")
          Core.<*> (x Core..:? "id")
          Core.<*> (x Core..:? "launchType")
          Core.<*> (x Core..:? "networkConfiguration")
          Core.<*> (x Core..:? "pendingCount")
          Core.<*> (x Core..:? "platformVersion")
          Core.<*> (x Core..:? "rolloutState")
          Core.<*> (x Core..:? "rolloutStateReason")
          Core.<*> (x Core..:? "runningCount")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "taskDefinition")
          Core.<*> (x Core..:? "updatedAt")
