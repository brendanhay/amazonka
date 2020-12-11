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
    dRolloutState,
    dRunningCount,
    dStatus,
    dCreatedAt,
    dPlatformVersion,
    dDesiredCount,
    dPendingCount,
    dId,
    dFailedTasks,
    dLaunchType,
    dUpdatedAt,
    dTaskDefinition,
    dRolloutStateReason,
    dNetworkConfiguration,
    dCapacityProviderStrategy,
  )
where

import Network.AWS.ECS.Types.CapacityProviderStrategyItem
import Network.AWS.ECS.Types.DeploymentRolloutState
import Network.AWS.ECS.Types.LaunchType
import Network.AWS.ECS.Types.NetworkConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of an Amazon ECS service deployment. This is used only when a service uses the @ECS@ deployment controller type.
--
-- /See:/ 'mkDeployment' smart constructor.
data Deployment = Deployment'
  { rolloutState ::
      Lude.Maybe DeploymentRolloutState,
    runningCount :: Lude.Maybe Lude.Int,
    status :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    platformVersion :: Lude.Maybe Lude.Text,
    desiredCount :: Lude.Maybe Lude.Int,
    pendingCount :: Lude.Maybe Lude.Int,
    id :: Lude.Maybe Lude.Text,
    failedTasks :: Lude.Maybe Lude.Int,
    launchType :: Lude.Maybe LaunchType,
    updatedAt :: Lude.Maybe Lude.Timestamp,
    taskDefinition :: Lude.Maybe Lude.Text,
    rolloutStateReason :: Lude.Maybe Lude.Text,
    networkConfiguration :: Lude.Maybe NetworkConfiguration,
    capacityProviderStrategy ::
      Lude.Maybe [CapacityProviderStrategyItem]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Deployment' with the minimum fields required to make a request.
--
-- * 'capacityProviderStrategy' - The capacity provider strategy that the deployment is using.
-- * 'createdAt' - The Unix timestamp for when the service deployment was created.
-- * 'desiredCount' - The most recent desired count of tasks that was specified for the service to deploy or maintain.
-- * 'failedTasks' - The number of consecutively failed tasks in the deployment. A task is considered a failure if the service scheduler can't launch the task, the task doesn't transition to a @RUNNING@ state, or if it fails any of its defined health checks and is stopped.
-- * 'id' - The ID of the deployment.
-- * 'launchType' - The launch type the tasks in the service are using. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'networkConfiguration' - The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
-- * 'pendingCount' - The number of tasks in the deployment that are in the @PENDING@ status.
-- * 'platformVersion' - The platform version on which your tasks in the service are running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'rolloutState' - The rollout state of the deployment. When a service deployment is started, it begins in an @IN_PROGRESS@ state. When the service reaches a steady state, the deployment will transition to a @COMPLETED@ state. If the service fails to reach a steady state and circuit breaker is enabled, the deployment will transition to a @FAILED@ state. A deployment in @FAILED@ state will launch no new tasks. For more information, see 'DeploymentCircuitBreaker' .
-- * 'rolloutStateReason' - A description of the rollout state of a deployment.
-- * 'runningCount' - The number of tasks in the deployment that are in the @RUNNING@ status.
-- * 'status' - The status of the deployment. The following describes each state:
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
-- * 'taskDefinition' - The most recent task definition that was specified for the tasks in the service to use.
-- * 'updatedAt' - The Unix timestamp for when the service deployment was last updated.
mkDeployment ::
  Deployment
mkDeployment =
  Deployment'
    { rolloutState = Lude.Nothing,
      runningCount = Lude.Nothing,
      status = Lude.Nothing,
      createdAt = Lude.Nothing,
      platformVersion = Lude.Nothing,
      desiredCount = Lude.Nothing,
      pendingCount = Lude.Nothing,
      id = Lude.Nothing,
      failedTasks = Lude.Nothing,
      launchType = Lude.Nothing,
      updatedAt = Lude.Nothing,
      taskDefinition = Lude.Nothing,
      rolloutStateReason = Lude.Nothing,
      networkConfiguration = Lude.Nothing,
      capacityProviderStrategy = Lude.Nothing
    }

-- | The rollout state of the deployment. When a service deployment is started, it begins in an @IN_PROGRESS@ state. When the service reaches a steady state, the deployment will transition to a @COMPLETED@ state. If the service fails to reach a steady state and circuit breaker is enabled, the deployment will transition to a @FAILED@ state. A deployment in @FAILED@ state will launch no new tasks. For more information, see 'DeploymentCircuitBreaker' .
--
-- /Note:/ Consider using 'rolloutState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRolloutState :: Lens.Lens' Deployment (Lude.Maybe DeploymentRolloutState)
dRolloutState = Lens.lens (rolloutState :: Deployment -> Lude.Maybe DeploymentRolloutState) (\s a -> s {rolloutState = a} :: Deployment)
{-# DEPRECATED dRolloutState "Use generic-lens or generic-optics with 'rolloutState' instead." #-}

-- | The number of tasks in the deployment that are in the @RUNNING@ status.
--
-- /Note:/ Consider using 'runningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRunningCount :: Lens.Lens' Deployment (Lude.Maybe Lude.Int)
dRunningCount = Lens.lens (runningCount :: Deployment -> Lude.Maybe Lude.Int) (\s a -> s {runningCount = a} :: Deployment)
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
dStatus :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dStatus = Lens.lens (status :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: Deployment)
{-# DEPRECATED dStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Unix timestamp for when the service deployment was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreatedAt :: Lens.Lens' Deployment (Lude.Maybe Lude.Timestamp)
dCreatedAt = Lens.lens (createdAt :: Deployment -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: Deployment)
{-# DEPRECATED dCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The platform version on which your tasks in the service are running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPlatformVersion :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dPlatformVersion = Lens.lens (platformVersion :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {platformVersion = a} :: Deployment)
{-# DEPRECATED dPlatformVersion "Use generic-lens or generic-optics with 'platformVersion' instead." #-}

-- | The most recent desired count of tasks that was specified for the service to deploy or maintain.
--
-- /Note:/ Consider using 'desiredCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDesiredCount :: Lens.Lens' Deployment (Lude.Maybe Lude.Int)
dDesiredCount = Lens.lens (desiredCount :: Deployment -> Lude.Maybe Lude.Int) (\s a -> s {desiredCount = a} :: Deployment)
{-# DEPRECATED dDesiredCount "Use generic-lens or generic-optics with 'desiredCount' instead." #-}

-- | The number of tasks in the deployment that are in the @PENDING@ status.
--
-- /Note:/ Consider using 'pendingCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPendingCount :: Lens.Lens' Deployment (Lude.Maybe Lude.Int)
dPendingCount = Lens.lens (pendingCount :: Deployment -> Lude.Maybe Lude.Int) (\s a -> s {pendingCount = a} :: Deployment)
{-# DEPRECATED dPendingCount "Use generic-lens or generic-optics with 'pendingCount' instead." #-}

-- | The ID of the deployment.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dId = Lens.lens (id :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Deployment)
{-# DEPRECATED dId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The number of consecutively failed tasks in the deployment. A task is considered a failure if the service scheduler can't launch the task, the task doesn't transition to a @RUNNING@ state, or if it fails any of its defined health checks and is stopped.
--
-- /Note:/ Consider using 'failedTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFailedTasks :: Lens.Lens' Deployment (Lude.Maybe Lude.Int)
dFailedTasks = Lens.lens (failedTasks :: Deployment -> Lude.Maybe Lude.Int) (\s a -> s {failedTasks = a} :: Deployment)
{-# DEPRECATED dFailedTasks "Use generic-lens or generic-optics with 'failedTasks' instead." #-}

-- | The launch type the tasks in the service are using. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'launchType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLaunchType :: Lens.Lens' Deployment (Lude.Maybe LaunchType)
dLaunchType = Lens.lens (launchType :: Deployment -> Lude.Maybe LaunchType) (\s a -> s {launchType = a} :: Deployment)
{-# DEPRECATED dLaunchType "Use generic-lens or generic-optics with 'launchType' instead." #-}

-- | The Unix timestamp for when the service deployment was last updated.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUpdatedAt :: Lens.Lens' Deployment (Lude.Maybe Lude.Timestamp)
dUpdatedAt = Lens.lens (updatedAt :: Deployment -> Lude.Maybe Lude.Timestamp) (\s a -> s {updatedAt = a} :: Deployment)
{-# DEPRECATED dUpdatedAt "Use generic-lens or generic-optics with 'updatedAt' instead." #-}

-- | The most recent task definition that was specified for the tasks in the service to use.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTaskDefinition :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dTaskDefinition = Lens.lens (taskDefinition :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {taskDefinition = a} :: Deployment)
{-# DEPRECATED dTaskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead." #-}

-- | A description of the rollout state of a deployment.
--
-- /Note:/ Consider using 'rolloutStateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRolloutStateReason :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dRolloutStateReason = Lens.lens (rolloutStateReason :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {rolloutStateReason = a} :: Deployment)
{-# DEPRECATED dRolloutStateReason "Use generic-lens or generic-optics with 'rolloutStateReason' instead." #-}

-- | The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
--
-- /Note:/ Consider using 'networkConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNetworkConfiguration :: Lens.Lens' Deployment (Lude.Maybe NetworkConfiguration)
dNetworkConfiguration = Lens.lens (networkConfiguration :: Deployment -> Lude.Maybe NetworkConfiguration) (\s a -> s {networkConfiguration = a} :: Deployment)
{-# DEPRECATED dNetworkConfiguration "Use generic-lens or generic-optics with 'networkConfiguration' instead." #-}

-- | The capacity provider strategy that the deployment is using.
--
-- /Note:/ Consider using 'capacityProviderStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCapacityProviderStrategy :: Lens.Lens' Deployment (Lude.Maybe [CapacityProviderStrategyItem])
dCapacityProviderStrategy = Lens.lens (capacityProviderStrategy :: Deployment -> Lude.Maybe [CapacityProviderStrategyItem]) (\s a -> s {capacityProviderStrategy = a} :: Deployment)
{-# DEPRECATED dCapacityProviderStrategy "Use generic-lens or generic-optics with 'capacityProviderStrategy' instead." #-}

instance Lude.FromJSON Deployment where
  parseJSON =
    Lude.withObject
      "Deployment"
      ( \x ->
          Deployment'
            Lude.<$> (x Lude..:? "rolloutState")
            Lude.<*> (x Lude..:? "runningCount")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "platformVersion")
            Lude.<*> (x Lude..:? "desiredCount")
            Lude.<*> (x Lude..:? "pendingCount")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "failedTasks")
            Lude.<*> (x Lude..:? "launchType")
            Lude.<*> (x Lude..:? "updatedAt")
            Lude.<*> (x Lude..:? "taskDefinition")
            Lude.<*> (x Lude..:? "rolloutStateReason")
            Lude.<*> (x Lude..:? "networkConfiguration")
            Lude.<*> (x Lude..:? "capacityProviderStrategy" Lude..!= Lude.mempty)
      )
