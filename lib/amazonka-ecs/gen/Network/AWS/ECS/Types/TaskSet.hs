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
    tsRunningCount,
    tsStatus,
    tsClusterARN,
    tsComputedDesiredCount,
    tsCreatedAt,
    tsPlatformVersion,
    tsScale,
    tsLoadBalancers,
    tsStabilityStatusAt,
    tsPendingCount,
    tsTaskSetARN,
    tsStartedBy,
    tsId,
    tsLaunchType,
    tsUpdatedAt,
    tsServiceARN,
    tsTaskDefinition,
    tsExternalId,
    tsNetworkConfiguration,
    tsServiceRegistries,
    tsCapacityProviderStrategy,
    tsStabilityStatus,
    tsTags,
  )
where

import Network.AWS.ECS.Types.CapacityProviderStrategyItem
import Network.AWS.ECS.Types.LaunchType
import Network.AWS.ECS.Types.LoadBalancer
import Network.AWS.ECS.Types.NetworkConfiguration
import Network.AWS.ECS.Types.Scale
import Network.AWS.ECS.Types.ServiceRegistry
import Network.AWS.ECS.Types.StabilityStatus
import Network.AWS.ECS.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a set of Amazon ECS tasks in either an AWS CodeDeploy or an @EXTERNAL@ deployment. An Amazon ECS task set includes details such as the desired number of tasks, how many tasks are running, and whether the task set serves production traffic.
--
-- /See:/ 'mkTaskSet' smart constructor.
data TaskSet = TaskSet'
  { runningCount :: Lude.Maybe Lude.Int,
    status :: Lude.Maybe Lude.Text,
    clusterARN :: Lude.Maybe Lude.Text,
    computedDesiredCount :: Lude.Maybe Lude.Int,
    createdAt :: Lude.Maybe Lude.Timestamp,
    platformVersion :: Lude.Maybe Lude.Text,
    scale :: Lude.Maybe Scale,
    loadBalancers :: Lude.Maybe [LoadBalancer],
    stabilityStatusAt :: Lude.Maybe Lude.Timestamp,
    pendingCount :: Lude.Maybe Lude.Int,
    taskSetARN :: Lude.Maybe Lude.Text,
    startedBy :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    launchType :: Lude.Maybe LaunchType,
    updatedAt :: Lude.Maybe Lude.Timestamp,
    serviceARN :: Lude.Maybe Lude.Text,
    taskDefinition :: Lude.Maybe Lude.Text,
    externalId :: Lude.Maybe Lude.Text,
    networkConfiguration :: Lude.Maybe NetworkConfiguration,
    serviceRegistries :: Lude.Maybe [ServiceRegistry],
    capacityProviderStrategy ::
      Lude.Maybe [CapacityProviderStrategyItem],
    stabilityStatus :: Lude.Maybe StabilityStatus,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TaskSet' with the minimum fields required to make a request.
--
-- * 'capacityProviderStrategy' - The capacity provider strategy associated with the task set.
-- * 'clusterARN' - The Amazon Resource Name (ARN) of the cluster that the service that hosts the task set exists in.
-- * 'computedDesiredCount' - The computed desired count for the task set. This is calculated by multiplying the service's @desiredCount@ by the task set's @scale@ percentage. The result is always rounded up. For example, if the computed desired count is 1.2, it rounds up to 2 tasks.
-- * 'createdAt' - The Unix timestamp for when the task set was created.
-- * 'externalId' - The external ID associated with the task set.
--
-- If a task set is created by an AWS CodeDeploy deployment, the @externalId@ parameter contains the AWS CodeDeploy deployment ID.
-- If a task set is created for an external deployment and is associated with a service discovery registry, the @externalId@ parameter contains the @ECS_TASK_SET_EXTERNAL_ID@ AWS Cloud Map attribute.
-- * 'id' - The ID of the task set.
-- * 'launchType' - The launch type the tasks in the task set are using. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'loadBalancers' - Details on a load balancer that is used with a task set.
-- * 'networkConfiguration' - The network configuration for the task set.
-- * 'pendingCount' - The number of tasks in the task set that are in the @PENDING@ status during a deployment. A task in the @PENDING@ state is preparing to enter the @RUNNING@ state. A task set enters the @PENDING@ status when it launches for the first time or when it is restarted after being in the @STOPPED@ state.
-- * 'platformVersion' - The platform version on which the tasks in the task set are running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'runningCount' - The number of tasks in the task set that are in the @RUNNING@ status during a deployment. A task in the @RUNNING@ state is running and ready for use.
-- * 'scale' - A floating-point percentage of the desired number of tasks to place and keep running in the task set.
-- * 'serviceARN' - The Amazon Resource Name (ARN) of the service the task set exists in.
-- * 'serviceRegistries' - The details of the service discovery registries to assign to this task set. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
-- * 'stabilityStatus' - The stability status, which indicates whether the task set has reached a steady state. If the following conditions are met, the task set will be in @STEADY_STATE@ :
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
-- * 'stabilityStatusAt' - The Unix timestamp for when the task set stability status was retrieved.
-- * 'startedBy' - The tag specified when a task set is started. If the task set is created by an AWS CodeDeploy deployment, the @startedBy@ parameter is @CODE_DEPLOY@ . For a task set created for an external deployment, the startedBy field isn't used.
-- * 'status' - The status of the task set. The following describes each state:
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
-- * 'tags' - The metadata that you apply to the task set to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
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
-- * 'taskDefinition' - The task definition the task set is using.
-- * 'taskSetARN' - The Amazon Resource Name (ARN) of the task set.
-- * 'updatedAt' - The Unix timestamp for when the task set was last updated.
mkTaskSet ::
  TaskSet
mkTaskSet =
  TaskSet'
    { runningCount = Lude.Nothing,
      status = Lude.Nothing,
      clusterARN = Lude.Nothing,
      computedDesiredCount = Lude.Nothing,
      createdAt = Lude.Nothing,
      platformVersion = Lude.Nothing,
      scale = Lude.Nothing,
      loadBalancers = Lude.Nothing,
      stabilityStatusAt = Lude.Nothing,
      pendingCount = Lude.Nothing,
      taskSetARN = Lude.Nothing,
      startedBy = Lude.Nothing,
      id = Lude.Nothing,
      launchType = Lude.Nothing,
      updatedAt = Lude.Nothing,
      serviceARN = Lude.Nothing,
      taskDefinition = Lude.Nothing,
      externalId = Lude.Nothing,
      networkConfiguration = Lude.Nothing,
      serviceRegistries = Lude.Nothing,
      capacityProviderStrategy = Lude.Nothing,
      stabilityStatus = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The number of tasks in the task set that are in the @RUNNING@ status during a deployment. A task in the @RUNNING@ state is running and ready for use.
--
-- /Note:/ Consider using 'runningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsRunningCount :: Lens.Lens' TaskSet (Lude.Maybe Lude.Int)
tsRunningCount = Lens.lens (runningCount :: TaskSet -> Lude.Maybe Lude.Int) (\s a -> s {runningCount = a} :: TaskSet)
{-# DEPRECATED tsRunningCount "Use generic-lens or generic-optics with 'runningCount' instead." #-}

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
tsStatus :: Lens.Lens' TaskSet (Lude.Maybe Lude.Text)
tsStatus = Lens.lens (status :: TaskSet -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: TaskSet)
{-# DEPRECATED tsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Name (ARN) of the cluster that the service that hosts the task set exists in.
--
-- /Note:/ Consider using 'clusterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsClusterARN :: Lens.Lens' TaskSet (Lude.Maybe Lude.Text)
tsClusterARN = Lens.lens (clusterARN :: TaskSet -> Lude.Maybe Lude.Text) (\s a -> s {clusterARN = a} :: TaskSet)
{-# DEPRECATED tsClusterARN "Use generic-lens or generic-optics with 'clusterARN' instead." #-}

-- | The computed desired count for the task set. This is calculated by multiplying the service's @desiredCount@ by the task set's @scale@ percentage. The result is always rounded up. For example, if the computed desired count is 1.2, it rounds up to 2 tasks.
--
-- /Note:/ Consider using 'computedDesiredCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsComputedDesiredCount :: Lens.Lens' TaskSet (Lude.Maybe Lude.Int)
tsComputedDesiredCount = Lens.lens (computedDesiredCount :: TaskSet -> Lude.Maybe Lude.Int) (\s a -> s {computedDesiredCount = a} :: TaskSet)
{-# DEPRECATED tsComputedDesiredCount "Use generic-lens or generic-optics with 'computedDesiredCount' instead." #-}

-- | The Unix timestamp for when the task set was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsCreatedAt :: Lens.Lens' TaskSet (Lude.Maybe Lude.Timestamp)
tsCreatedAt = Lens.lens (createdAt :: TaskSet -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: TaskSet)
{-# DEPRECATED tsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The platform version on which the tasks in the task set are running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsPlatformVersion :: Lens.Lens' TaskSet (Lude.Maybe Lude.Text)
tsPlatformVersion = Lens.lens (platformVersion :: TaskSet -> Lude.Maybe Lude.Text) (\s a -> s {platformVersion = a} :: TaskSet)
{-# DEPRECATED tsPlatformVersion "Use generic-lens or generic-optics with 'platformVersion' instead." #-}

-- | A floating-point percentage of the desired number of tasks to place and keep running in the task set.
--
-- /Note:/ Consider using 'scale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsScale :: Lens.Lens' TaskSet (Lude.Maybe Scale)
tsScale = Lens.lens (scale :: TaskSet -> Lude.Maybe Scale) (\s a -> s {scale = a} :: TaskSet)
{-# DEPRECATED tsScale "Use generic-lens or generic-optics with 'scale' instead." #-}

-- | Details on a load balancer that is used with a task set.
--
-- /Note:/ Consider using 'loadBalancers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsLoadBalancers :: Lens.Lens' TaskSet (Lude.Maybe [LoadBalancer])
tsLoadBalancers = Lens.lens (loadBalancers :: TaskSet -> Lude.Maybe [LoadBalancer]) (\s a -> s {loadBalancers = a} :: TaskSet)
{-# DEPRECATED tsLoadBalancers "Use generic-lens or generic-optics with 'loadBalancers' instead." #-}

-- | The Unix timestamp for when the task set stability status was retrieved.
--
-- /Note:/ Consider using 'stabilityStatusAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsStabilityStatusAt :: Lens.Lens' TaskSet (Lude.Maybe Lude.Timestamp)
tsStabilityStatusAt = Lens.lens (stabilityStatusAt :: TaskSet -> Lude.Maybe Lude.Timestamp) (\s a -> s {stabilityStatusAt = a} :: TaskSet)
{-# DEPRECATED tsStabilityStatusAt "Use generic-lens or generic-optics with 'stabilityStatusAt' instead." #-}

-- | The number of tasks in the task set that are in the @PENDING@ status during a deployment. A task in the @PENDING@ state is preparing to enter the @RUNNING@ state. A task set enters the @PENDING@ status when it launches for the first time or when it is restarted after being in the @STOPPED@ state.
--
-- /Note:/ Consider using 'pendingCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsPendingCount :: Lens.Lens' TaskSet (Lude.Maybe Lude.Int)
tsPendingCount = Lens.lens (pendingCount :: TaskSet -> Lude.Maybe Lude.Int) (\s a -> s {pendingCount = a} :: TaskSet)
{-# DEPRECATED tsPendingCount "Use generic-lens or generic-optics with 'pendingCount' instead." #-}

-- | The Amazon Resource Name (ARN) of the task set.
--
-- /Note:/ Consider using 'taskSetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTaskSetARN :: Lens.Lens' TaskSet (Lude.Maybe Lude.Text)
tsTaskSetARN = Lens.lens (taskSetARN :: TaskSet -> Lude.Maybe Lude.Text) (\s a -> s {taskSetARN = a} :: TaskSet)
{-# DEPRECATED tsTaskSetARN "Use generic-lens or generic-optics with 'taskSetARN' instead." #-}

-- | The tag specified when a task set is started. If the task set is created by an AWS CodeDeploy deployment, the @startedBy@ parameter is @CODE_DEPLOY@ . For a task set created for an external deployment, the startedBy field isn't used.
--
-- /Note:/ Consider using 'startedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsStartedBy :: Lens.Lens' TaskSet (Lude.Maybe Lude.Text)
tsStartedBy = Lens.lens (startedBy :: TaskSet -> Lude.Maybe Lude.Text) (\s a -> s {startedBy = a} :: TaskSet)
{-# DEPRECATED tsStartedBy "Use generic-lens or generic-optics with 'startedBy' instead." #-}

-- | The ID of the task set.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsId :: Lens.Lens' TaskSet (Lude.Maybe Lude.Text)
tsId = Lens.lens (id :: TaskSet -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: TaskSet)
{-# DEPRECATED tsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The launch type the tasks in the task set are using. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'launchType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsLaunchType :: Lens.Lens' TaskSet (Lude.Maybe LaunchType)
tsLaunchType = Lens.lens (launchType :: TaskSet -> Lude.Maybe LaunchType) (\s a -> s {launchType = a} :: TaskSet)
{-# DEPRECATED tsLaunchType "Use generic-lens or generic-optics with 'launchType' instead." #-}

-- | The Unix timestamp for when the task set was last updated.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsUpdatedAt :: Lens.Lens' TaskSet (Lude.Maybe Lude.Timestamp)
tsUpdatedAt = Lens.lens (updatedAt :: TaskSet -> Lude.Maybe Lude.Timestamp) (\s a -> s {updatedAt = a} :: TaskSet)
{-# DEPRECATED tsUpdatedAt "Use generic-lens or generic-optics with 'updatedAt' instead." #-}

-- | The Amazon Resource Name (ARN) of the service the task set exists in.
--
-- /Note:/ Consider using 'serviceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsServiceARN :: Lens.Lens' TaskSet (Lude.Maybe Lude.Text)
tsServiceARN = Lens.lens (serviceARN :: TaskSet -> Lude.Maybe Lude.Text) (\s a -> s {serviceARN = a} :: TaskSet)
{-# DEPRECATED tsServiceARN "Use generic-lens or generic-optics with 'serviceARN' instead." #-}

-- | The task definition the task set is using.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTaskDefinition :: Lens.Lens' TaskSet (Lude.Maybe Lude.Text)
tsTaskDefinition = Lens.lens (taskDefinition :: TaskSet -> Lude.Maybe Lude.Text) (\s a -> s {taskDefinition = a} :: TaskSet)
{-# DEPRECATED tsTaskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead." #-}

-- | The external ID associated with the task set.
--
-- If a task set is created by an AWS CodeDeploy deployment, the @externalId@ parameter contains the AWS CodeDeploy deployment ID.
-- If a task set is created for an external deployment and is associated with a service discovery registry, the @externalId@ parameter contains the @ECS_TASK_SET_EXTERNAL_ID@ AWS Cloud Map attribute.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsExternalId :: Lens.Lens' TaskSet (Lude.Maybe Lude.Text)
tsExternalId = Lens.lens (externalId :: TaskSet -> Lude.Maybe Lude.Text) (\s a -> s {externalId = a} :: TaskSet)
{-# DEPRECATED tsExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

-- | The network configuration for the task set.
--
-- /Note:/ Consider using 'networkConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsNetworkConfiguration :: Lens.Lens' TaskSet (Lude.Maybe NetworkConfiguration)
tsNetworkConfiguration = Lens.lens (networkConfiguration :: TaskSet -> Lude.Maybe NetworkConfiguration) (\s a -> s {networkConfiguration = a} :: TaskSet)
{-# DEPRECATED tsNetworkConfiguration "Use generic-lens or generic-optics with 'networkConfiguration' instead." #-}

-- | The details of the service discovery registries to assign to this task set. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
--
-- /Note:/ Consider using 'serviceRegistries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsServiceRegistries :: Lens.Lens' TaskSet (Lude.Maybe [ServiceRegistry])
tsServiceRegistries = Lens.lens (serviceRegistries :: TaskSet -> Lude.Maybe [ServiceRegistry]) (\s a -> s {serviceRegistries = a} :: TaskSet)
{-# DEPRECATED tsServiceRegistries "Use generic-lens or generic-optics with 'serviceRegistries' instead." #-}

-- | The capacity provider strategy associated with the task set.
--
-- /Note:/ Consider using 'capacityProviderStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsCapacityProviderStrategy :: Lens.Lens' TaskSet (Lude.Maybe [CapacityProviderStrategyItem])
tsCapacityProviderStrategy = Lens.lens (capacityProviderStrategy :: TaskSet -> Lude.Maybe [CapacityProviderStrategyItem]) (\s a -> s {capacityProviderStrategy = a} :: TaskSet)
{-# DEPRECATED tsCapacityProviderStrategy "Use generic-lens or generic-optics with 'capacityProviderStrategy' instead." #-}

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
tsStabilityStatus :: Lens.Lens' TaskSet (Lude.Maybe StabilityStatus)
tsStabilityStatus = Lens.lens (stabilityStatus :: TaskSet -> Lude.Maybe StabilityStatus) (\s a -> s {stabilityStatus = a} :: TaskSet)
{-# DEPRECATED tsStabilityStatus "Use generic-lens or generic-optics with 'stabilityStatus' instead." #-}

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
tsTags :: Lens.Lens' TaskSet (Lude.Maybe [Tag])
tsTags = Lens.lens (tags :: TaskSet -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: TaskSet)
{-# DEPRECATED tsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON TaskSet where
  parseJSON =
    Lude.withObject
      "TaskSet"
      ( \x ->
          TaskSet'
            Lude.<$> (x Lude..:? "runningCount")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "clusterArn")
            Lude.<*> (x Lude..:? "computedDesiredCount")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "platformVersion")
            Lude.<*> (x Lude..:? "scale")
            Lude.<*> (x Lude..:? "loadBalancers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "stabilityStatusAt")
            Lude.<*> (x Lude..:? "pendingCount")
            Lude.<*> (x Lude..:? "taskSetArn")
            Lude.<*> (x Lude..:? "startedBy")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "launchType")
            Lude.<*> (x Lude..:? "updatedAt")
            Lude.<*> (x Lude..:? "serviceArn")
            Lude.<*> (x Lude..:? "taskDefinition")
            Lude.<*> (x Lude..:? "externalId")
            Lude.<*> (x Lude..:? "networkConfiguration")
            Lude.<*> (x Lude..:? "serviceRegistries" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "capacityProviderStrategy" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "stabilityStatus")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
