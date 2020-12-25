{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.TaskSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.TaskSet
  ( TaskSet (..),

    -- * Smart constructor
    mkTaskSet,

    -- * Lenses
    tsCapacityProviderStrategy,
    tsClusterArn,
    tsComputedDesiredCount,
    tsCreatedAt,
    tsExternalId,
    tsId,
    tsLaunchType,
    tsLoadBalancers,
    tsNetworkConfiguration,
    tsPendingCount,
    tsPlatformVersion,
    tsRunningCount,
    tsScale,
    tsServiceArn,
    tsServiceRegistries,
    tsStabilityStatus,
    tsStabilityStatusAt,
    tsStartedBy,
    tsStatus,
    tsTags,
    tsTaskDefinition,
    tsTaskSetArn,
    tsUpdatedAt,
  )
where

import qualified Network.AWS.ECS.Types.CapacityProviderStrategyItem as Types
import qualified Network.AWS.ECS.Types.ClusterArn as Types
import qualified Network.AWS.ECS.Types.ExternalId as Types
import qualified Network.AWS.ECS.Types.Id as Types
import qualified Network.AWS.ECS.Types.LaunchType as Types
import qualified Network.AWS.ECS.Types.LoadBalancer as Types
import qualified Network.AWS.ECS.Types.NetworkConfiguration as Types
import qualified Network.AWS.ECS.Types.PlatformVersion as Types
import qualified Network.AWS.ECS.Types.Scale as Types
import qualified Network.AWS.ECS.Types.ServiceArn as Types
import qualified Network.AWS.ECS.Types.ServiceRegistry as Types
import qualified Network.AWS.ECS.Types.StabilityStatus as Types
import qualified Network.AWS.ECS.Types.StartedBy as Types
import qualified Network.AWS.ECS.Types.Status as Types
import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.ECS.Types.Tag as Types
import qualified Network.AWS.ECS.Types.TaskSetArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a set of Amazon ECS tasks in either an AWS CodeDeploy or an @EXTERNAL@ deployment. An Amazon ECS task set includes details such as the desired number of tasks, how many tasks are running, and whether the task set serves production traffic.
--
-- /See:/ 'mkTaskSet' smart constructor.
data TaskSet = TaskSet'
  { -- | The capacity provider strategy associated with the task set.
    capacityProviderStrategy :: Core.Maybe [Types.CapacityProviderStrategyItem],
    -- | The Amazon Resource Name (ARN) of the cluster that the service that hosts the task set exists in.
    clusterArn :: Core.Maybe Types.ClusterArn,
    -- | The computed desired count for the task set. This is calculated by multiplying the service's @desiredCount@ by the task set's @scale@ percentage. The result is always rounded up. For example, if the computed desired count is 1.2, it rounds up to 2 tasks.
    computedDesiredCount :: Core.Maybe Core.Int,
    -- | The Unix timestamp for when the task set was created.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | The external ID associated with the task set.
    --
    -- If a task set is created by an AWS CodeDeploy deployment, the @externalId@ parameter contains the AWS CodeDeploy deployment ID.
    -- If a task set is created for an external deployment and is associated with a service discovery registry, the @externalId@ parameter contains the @ECS_TASK_SET_EXTERNAL_ID@ AWS Cloud Map attribute.
    externalId :: Core.Maybe Types.ExternalId,
    -- | The ID of the task set.
    id :: Core.Maybe Types.Id,
    -- | The launch type the tasks in the task set are using. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
    launchType :: Core.Maybe Types.LaunchType,
    -- | Details on a load balancer that is used with a task set.
    loadBalancers :: Core.Maybe [Types.LoadBalancer],
    -- | The network configuration for the task set.
    networkConfiguration :: Core.Maybe Types.NetworkConfiguration,
    -- | The number of tasks in the task set that are in the @PENDING@ status during a deployment. A task in the @PENDING@ state is preparing to enter the @RUNNING@ state. A task set enters the @PENDING@ status when it launches for the first time or when it is restarted after being in the @STOPPED@ state.
    pendingCount :: Core.Maybe Core.Int,
    -- | The platform version on which the tasks in the task set are running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
    platformVersion :: Core.Maybe Types.PlatformVersion,
    -- | The number of tasks in the task set that are in the @RUNNING@ status during a deployment. A task in the @RUNNING@ state is running and ready for use.
    runningCount :: Core.Maybe Core.Int,
    -- | A floating-point percentage of the desired number of tasks to place and keep running in the task set.
    scale :: Core.Maybe Types.Scale,
    -- | The Amazon Resource Name (ARN) of the service the task set exists in.
    serviceArn :: Core.Maybe Types.ServiceArn,
    -- | The details of the service discovery registries to assign to this task set. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
    serviceRegistries :: Core.Maybe [Types.ServiceRegistry],
    -- | The stability status, which indicates whether the task set has reached a steady state. If the following conditions are met, the task set will be in @STEADY_STATE@ :
    --
    --
    --     * The task @runningCount@ is equal to the @computedDesiredCount@ .
    --
    --
    --     * The @pendingCount@ is @0@ .
    --
    --
    --     * There are no tasks running on container instances in the @DRAINING@ status.
    --
    --
    --     * All tasks are reporting a healthy status from the load balancers, service discovery, and container health checks.
    --
    --
    -- If any of those conditions are not met, the stability status returns @STABILIZING@ .
    stabilityStatus :: Core.Maybe Types.StabilityStatus,
    -- | The Unix timestamp for when the task set stability status was retrieved.
    stabilityStatusAt :: Core.Maybe Core.NominalDiffTime,
    -- | The tag specified when a task set is started. If the task set is created by an AWS CodeDeploy deployment, the @startedBy@ parameter is @CODE_DEPLOY@ . For a task set created for an external deployment, the startedBy field isn't used.
    startedBy :: Core.Maybe Types.StartedBy,
    -- | The status of the task set. The following describes each state:
    --
    --
    --     * PRIMARY
    --
    --     * The task set is serving production traffic.
    --
    --
    --     * ACTIVE
    --
    --     * The task set is not serving production traffic.
    --
    --
    --     * DRAINING
    --
    --     * The tasks in the task set are being stopped and their corresponding targets are being deregistered from their target group.
    status :: Core.Maybe Types.Status,
    -- | The metadata that you apply to the task set to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
    --
    -- The following basic restrictions apply to tags:
    --
    --     * Maximum number of tags per resource - 50
    --
    --
    --     * For each resource, each tag key must be unique, and each tag key can have only one value.
    --
    --
    --     * Maximum key length - 128 Unicode characters in UTF-8
    --
    --
    --     * Maximum value length - 256 Unicode characters in UTF-8
    --
    --
    --     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
    --
    --
    --     * Tag keys and values are case-sensitive.
    --
    --
    --     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
    tags :: Core.Maybe [Types.Tag],
    -- | The task definition the task set is using.
    taskDefinition :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) of the task set.
    taskSetArn :: Core.Maybe Types.TaskSetArn,
    -- | The Unix timestamp for when the task set was last updated.
    updatedAt :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TaskSet' value with any optional fields omitted.
mkTaskSet ::
  TaskSet
mkTaskSet =
  TaskSet'
    { capacityProviderStrategy = Core.Nothing,
      clusterArn = Core.Nothing,
      computedDesiredCount = Core.Nothing,
      createdAt = Core.Nothing,
      externalId = Core.Nothing,
      id = Core.Nothing,
      launchType = Core.Nothing,
      loadBalancers = Core.Nothing,
      networkConfiguration = Core.Nothing,
      pendingCount = Core.Nothing,
      platformVersion = Core.Nothing,
      runningCount = Core.Nothing,
      scale = Core.Nothing,
      serviceArn = Core.Nothing,
      serviceRegistries = Core.Nothing,
      stabilityStatus = Core.Nothing,
      stabilityStatusAt = Core.Nothing,
      startedBy = Core.Nothing,
      status = Core.Nothing,
      tags = Core.Nothing,
      taskDefinition = Core.Nothing,
      taskSetArn = Core.Nothing,
      updatedAt = Core.Nothing
    }

-- | The capacity provider strategy associated with the task set.
--
-- /Note:/ Consider using 'capacityProviderStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsCapacityProviderStrategy :: Lens.Lens' TaskSet (Core.Maybe [Types.CapacityProviderStrategyItem])
tsCapacityProviderStrategy = Lens.field @"capacityProviderStrategy"
{-# DEPRECATED tsCapacityProviderStrategy "Use generic-lens or generic-optics with 'capacityProviderStrategy' instead." #-}

-- | The Amazon Resource Name (ARN) of the cluster that the service that hosts the task set exists in.
--
-- /Note:/ Consider using 'clusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsClusterArn :: Lens.Lens' TaskSet (Core.Maybe Types.ClusterArn)
tsClusterArn = Lens.field @"clusterArn"
{-# DEPRECATED tsClusterArn "Use generic-lens or generic-optics with 'clusterArn' instead." #-}

-- | The computed desired count for the task set. This is calculated by multiplying the service's @desiredCount@ by the task set's @scale@ percentage. The result is always rounded up. For example, if the computed desired count is 1.2, it rounds up to 2 tasks.
--
-- /Note:/ Consider using 'computedDesiredCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsComputedDesiredCount :: Lens.Lens' TaskSet (Core.Maybe Core.Int)
tsComputedDesiredCount = Lens.field @"computedDesiredCount"
{-# DEPRECATED tsComputedDesiredCount "Use generic-lens or generic-optics with 'computedDesiredCount' instead." #-}

-- | The Unix timestamp for when the task set was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsCreatedAt :: Lens.Lens' TaskSet (Core.Maybe Core.NominalDiffTime)
tsCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED tsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The external ID associated with the task set.
--
-- If a task set is created by an AWS CodeDeploy deployment, the @externalId@ parameter contains the AWS CodeDeploy deployment ID.
-- If a task set is created for an external deployment and is associated with a service discovery registry, the @externalId@ parameter contains the @ECS_TASK_SET_EXTERNAL_ID@ AWS Cloud Map attribute.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsExternalId :: Lens.Lens' TaskSet (Core.Maybe Types.ExternalId)
tsExternalId = Lens.field @"externalId"
{-# DEPRECATED tsExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

-- | The ID of the task set.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsId :: Lens.Lens' TaskSet (Core.Maybe Types.Id)
tsId = Lens.field @"id"
{-# DEPRECATED tsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The launch type the tasks in the task set are using. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'launchType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsLaunchType :: Lens.Lens' TaskSet (Core.Maybe Types.LaunchType)
tsLaunchType = Lens.field @"launchType"
{-# DEPRECATED tsLaunchType "Use generic-lens or generic-optics with 'launchType' instead." #-}

-- | Details on a load balancer that is used with a task set.
--
-- /Note:/ Consider using 'loadBalancers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsLoadBalancers :: Lens.Lens' TaskSet (Core.Maybe [Types.LoadBalancer])
tsLoadBalancers = Lens.field @"loadBalancers"
{-# DEPRECATED tsLoadBalancers "Use generic-lens or generic-optics with 'loadBalancers' instead." #-}

-- | The network configuration for the task set.
--
-- /Note:/ Consider using 'networkConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsNetworkConfiguration :: Lens.Lens' TaskSet (Core.Maybe Types.NetworkConfiguration)
tsNetworkConfiguration = Lens.field @"networkConfiguration"
{-# DEPRECATED tsNetworkConfiguration "Use generic-lens or generic-optics with 'networkConfiguration' instead." #-}

-- | The number of tasks in the task set that are in the @PENDING@ status during a deployment. A task in the @PENDING@ state is preparing to enter the @RUNNING@ state. A task set enters the @PENDING@ status when it launches for the first time or when it is restarted after being in the @STOPPED@ state.
--
-- /Note:/ Consider using 'pendingCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsPendingCount :: Lens.Lens' TaskSet (Core.Maybe Core.Int)
tsPendingCount = Lens.field @"pendingCount"
{-# DEPRECATED tsPendingCount "Use generic-lens or generic-optics with 'pendingCount' instead." #-}

-- | The platform version on which the tasks in the task set are running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsPlatformVersion :: Lens.Lens' TaskSet (Core.Maybe Types.PlatformVersion)
tsPlatformVersion = Lens.field @"platformVersion"
{-# DEPRECATED tsPlatformVersion "Use generic-lens or generic-optics with 'platformVersion' instead." #-}

-- | The number of tasks in the task set that are in the @RUNNING@ status during a deployment. A task in the @RUNNING@ state is running and ready for use.
--
-- /Note:/ Consider using 'runningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsRunningCount :: Lens.Lens' TaskSet (Core.Maybe Core.Int)
tsRunningCount = Lens.field @"runningCount"
{-# DEPRECATED tsRunningCount "Use generic-lens or generic-optics with 'runningCount' instead." #-}

-- | A floating-point percentage of the desired number of tasks to place and keep running in the task set.
--
-- /Note:/ Consider using 'scale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsScale :: Lens.Lens' TaskSet (Core.Maybe Types.Scale)
tsScale = Lens.field @"scale"
{-# DEPRECATED tsScale "Use generic-lens or generic-optics with 'scale' instead." #-}

-- | The Amazon Resource Name (ARN) of the service the task set exists in.
--
-- /Note:/ Consider using 'serviceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsServiceArn :: Lens.Lens' TaskSet (Core.Maybe Types.ServiceArn)
tsServiceArn = Lens.field @"serviceArn"
{-# DEPRECATED tsServiceArn "Use generic-lens or generic-optics with 'serviceArn' instead." #-}

-- | The details of the service discovery registries to assign to this task set. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
--
-- /Note:/ Consider using 'serviceRegistries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsServiceRegistries :: Lens.Lens' TaskSet (Core.Maybe [Types.ServiceRegistry])
tsServiceRegistries = Lens.field @"serviceRegistries"
{-# DEPRECATED tsServiceRegistries "Use generic-lens or generic-optics with 'serviceRegistries' instead." #-}

-- | The stability status, which indicates whether the task set has reached a steady state. If the following conditions are met, the task set will be in @STEADY_STATE@ :
--
--
--     * The task @runningCount@ is equal to the @computedDesiredCount@ .
--
--
--     * The @pendingCount@ is @0@ .
--
--
--     * There are no tasks running on container instances in the @DRAINING@ status.
--
--
--     * All tasks are reporting a healthy status from the load balancers, service discovery, and container health checks.
--
--
-- If any of those conditions are not met, the stability status returns @STABILIZING@ .
--
-- /Note:/ Consider using 'stabilityStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsStabilityStatus :: Lens.Lens' TaskSet (Core.Maybe Types.StabilityStatus)
tsStabilityStatus = Lens.field @"stabilityStatus"
{-# DEPRECATED tsStabilityStatus "Use generic-lens or generic-optics with 'stabilityStatus' instead." #-}

-- | The Unix timestamp for when the task set stability status was retrieved.
--
-- /Note:/ Consider using 'stabilityStatusAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsStabilityStatusAt :: Lens.Lens' TaskSet (Core.Maybe Core.NominalDiffTime)
tsStabilityStatusAt = Lens.field @"stabilityStatusAt"
{-# DEPRECATED tsStabilityStatusAt "Use generic-lens or generic-optics with 'stabilityStatusAt' instead." #-}

-- | The tag specified when a task set is started. If the task set is created by an AWS CodeDeploy deployment, the @startedBy@ parameter is @CODE_DEPLOY@ . For a task set created for an external deployment, the startedBy field isn't used.
--
-- /Note:/ Consider using 'startedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsStartedBy :: Lens.Lens' TaskSet (Core.Maybe Types.StartedBy)
tsStartedBy = Lens.field @"startedBy"
{-# DEPRECATED tsStartedBy "Use generic-lens or generic-optics with 'startedBy' instead." #-}

-- | The status of the task set. The following describes each state:
--
--
--     * PRIMARY
--
--     * The task set is serving production traffic.
--
--
--     * ACTIVE
--
--     * The task set is not serving production traffic.
--
--
--     * DRAINING
--
--     * The tasks in the task set are being stopped and their corresponding targets are being deregistered from their target group.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsStatus :: Lens.Lens' TaskSet (Core.Maybe Types.Status)
tsStatus = Lens.field @"status"
{-# DEPRECATED tsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The metadata that you apply to the task set to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
--     * Maximum number of tags per resource - 50
--
--
--     * For each resource, each tag key must be unique, and each tag key can have only one value.
--
--
--     * Maximum key length - 128 Unicode characters in UTF-8
--
--
--     * Maximum value length - 256 Unicode characters in UTF-8
--
--
--     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
--
--
--     * Tag keys and values are case-sensitive.
--
--
--     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTags :: Lens.Lens' TaskSet (Core.Maybe [Types.Tag])
tsTags = Lens.field @"tags"
{-# DEPRECATED tsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The task definition the task set is using.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTaskDefinition :: Lens.Lens' TaskSet (Core.Maybe Types.String)
tsTaskDefinition = Lens.field @"taskDefinition"
{-# DEPRECATED tsTaskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead." #-}

-- | The Amazon Resource Name (ARN) of the task set.
--
-- /Note:/ Consider using 'taskSetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTaskSetArn :: Lens.Lens' TaskSet (Core.Maybe Types.TaskSetArn)
tsTaskSetArn = Lens.field @"taskSetArn"
{-# DEPRECATED tsTaskSetArn "Use generic-lens or generic-optics with 'taskSetArn' instead." #-}

-- | The Unix timestamp for when the task set was last updated.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsUpdatedAt :: Lens.Lens' TaskSet (Core.Maybe Core.NominalDiffTime)
tsUpdatedAt = Lens.field @"updatedAt"
{-# DEPRECATED tsUpdatedAt "Use generic-lens or generic-optics with 'updatedAt' instead." #-}

instance Core.FromJSON TaskSet where
  parseJSON =
    Core.withObject "TaskSet" Core.$
      \x ->
        TaskSet'
          Core.<$> (x Core..:? "capacityProviderStrategy")
          Core.<*> (x Core..:? "clusterArn")
          Core.<*> (x Core..:? "computedDesiredCount")
          Core.<*> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "externalId")
          Core.<*> (x Core..:? "id")
          Core.<*> (x Core..:? "launchType")
          Core.<*> (x Core..:? "loadBalancers")
          Core.<*> (x Core..:? "networkConfiguration")
          Core.<*> (x Core..:? "pendingCount")
          Core.<*> (x Core..:? "platformVersion")
          Core.<*> (x Core..:? "runningCount")
          Core.<*> (x Core..:? "scale")
          Core.<*> (x Core..:? "serviceArn")
          Core.<*> (x Core..:? "serviceRegistries")
          Core.<*> (x Core..:? "stabilityStatus")
          Core.<*> (x Core..:? "stabilityStatusAt")
          Core.<*> (x Core..:? "startedBy")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "tags")
          Core.<*> (x Core..:? "taskDefinition")
          Core.<*> (x Core..:? "taskSetArn")
          Core.<*> (x Core..:? "updatedAt")
