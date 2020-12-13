{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerService
  ( ContainerService (..),

    -- * Smart constructor
    mkContainerService,

    -- * Lenses
    csTaskSets,
    csRunningCount,
    csStatus,
    csClusterARN,
    csPropagateTags,
    csCreatedAt,
    csPlatformVersion,
    csEnableECSManagedTags,
    csCreatedBy,
    csDesiredCount,
    csLoadBalancers,
    csPendingCount,
    csPlacementConstraints,
    csEvents,
    csPlacementStrategy,
    csDeployments,
    csServiceName,
    csDeploymentController,
    csLaunchType,
    csServiceARN,
    csTaskDefinition,
    csSchedulingStrategy,
    csHealthCheckGracePeriodSeconds,
    csNetworkConfiguration,
    csServiceRegistries,
    csCapacityProviderStrategy,
    csTags,
    csRoleARN,
    csDeploymentConfiguration,
  )
where

import Network.AWS.ECS.Types.CapacityProviderStrategyItem
import Network.AWS.ECS.Types.Deployment
import Network.AWS.ECS.Types.DeploymentConfiguration
import Network.AWS.ECS.Types.DeploymentController
import Network.AWS.ECS.Types.LaunchType
import Network.AWS.ECS.Types.LoadBalancer
import Network.AWS.ECS.Types.NetworkConfiguration
import Network.AWS.ECS.Types.PlacementConstraint
import Network.AWS.ECS.Types.PlacementStrategy
import Network.AWS.ECS.Types.PropagateTags
import Network.AWS.ECS.Types.SchedulingStrategy
import Network.AWS.ECS.Types.ServiceEvent
import Network.AWS.ECS.Types.ServiceRegistry
import Network.AWS.ECS.Types.Tag
import Network.AWS.ECS.Types.TaskSet
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details on a service within a cluster
--
-- /See:/ 'mkContainerService' smart constructor.
data ContainerService = ContainerService'
  { -- | Information about a set of Amazon ECS tasks in either an AWS CodeDeploy or an @EXTERNAL@ deployment. An Amazon ECS task set includes details such as the desired number of tasks, how many tasks are running, and whether the task set serves production traffic.
    taskSets :: Lude.Maybe [TaskSet],
    -- | The number of tasks in the cluster that are in the @RUNNING@ state.
    runningCount :: Lude.Maybe Lude.Int,
    -- | The status of the service. The valid values are @ACTIVE@ , @DRAINING@ , or @INACTIVE@ .
    status :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the cluster that hosts the service.
    clusterARN :: Lude.Maybe Lude.Text,
    -- | Specifies whether to propagate the tags from the task definition or the service to the task. If no value is specified, the tags are not propagated.
    propagateTags :: Lude.Maybe PropagateTags,
    -- | The Unix timestamp for when the service was created.
    createdAt :: Lude.Maybe Lude.Timestamp,
    -- | The platform version on which to run your service. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
    platformVersion :: Lude.Maybe Lude.Text,
    -- | Specifies whether to enable Amazon ECS managed tags for the tasks in the service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
    enableECSManagedTags :: Lude.Maybe Lude.Bool,
    -- | The principal that created the service.
    createdBy :: Lude.Maybe Lude.Text,
    -- | The desired number of instantiations of the task definition to keep running on the service. This value is specified when the service is created with 'CreateService' , and it can be modified with 'UpdateService' .
    desiredCount :: Lude.Maybe Lude.Int,
    -- | A list of Elastic Load Balancing load balancer objects, containing the load balancer name, the container name (as it appears in a container definition), and the container port to access from the load balancer.
    loadBalancers :: Lude.Maybe [LoadBalancer],
    -- | The number of tasks in the cluster that are in the @PENDING@ state.
    pendingCount :: Lude.Maybe Lude.Int,
    -- | The placement constraints for the tasks in the service.
    placementConstraints :: Lude.Maybe [PlacementConstraint],
    -- | The event stream for your service. A maximum of 100 of the latest events are displayed.
    events :: Lude.Maybe [ServiceEvent],
    -- | The placement strategy that determines how tasks for the service are placed.
    placementStrategy :: Lude.Maybe [PlacementStrategy],
    -- | The current state of deployments for the service.
    deployments :: Lude.Maybe [Deployment],
    -- | The name of your service. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. Service names must be unique within a cluster, but you can have similarly named services in multiple clusters within a Region or across multiple Regions.
    serviceName :: Lude.Maybe Lude.Text,
    -- | The deployment controller type the service is using. When using the DescribeServices API, this field is omitted if the service is using the @ECS@ deployment controller type.
    deploymentController :: Lude.Maybe DeploymentController,
    -- | The launch type on which your service is running. If no value is specified, it will default to @EC2@ . Valid values include @EC2@ and @FARGATE@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
    launchType :: Lude.Maybe LaunchType,
    -- | The ARN that identifies the service. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the service, the AWS account ID of the service owner, the @service@ namespace, and then the service name. For example, @arn:aws:ecs:region:012345678910:service/my-service@ .
    serviceARN :: Lude.Maybe Lude.Text,
    -- | The task definition to use for tasks in the service. This value is specified when the service is created with 'CreateService' , and it can be modified with 'UpdateService' .
    taskDefinition :: Lude.Maybe Lude.Text,
    -- | The scheduling strategy to use for the service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Services> .
    --
    -- There are two service scheduler strategies available:
    --
    --     * @REPLICA@ -The replica scheduling strategy places and maintains the desired number of tasks across your cluster. By default, the service scheduler spreads tasks across Availability Zones. You can use task placement strategies and constraints to customize task placement decisions.
    --
    --
    --     * @DAEMON@ -The daemon scheduling strategy deploys exactly one task on each active container instance that meets all of the task placement constraints that you specify in your cluster. The service scheduler also evaluates the task placement constraints for running tasks and will stop tasks that do not meet the placement constraints.
    schedulingStrategy :: Lude.Maybe SchedulingStrategy,
    -- | The period of time, in seconds, that the Amazon ECS service scheduler ignores unhealthy Elastic Load Balancing target health checks after a task has first started.
    healthCheckGracePeriodSeconds :: Lude.Maybe Lude.Int,
    -- | The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
    networkConfiguration :: Lude.Maybe NetworkConfiguration,
    -- | The details of the service discovery registries to assign to this service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
    serviceRegistries :: Lude.Maybe [ServiceRegistry],
    -- | The capacity provider strategy associated with the service.
    capacityProviderStrategy :: Lude.Maybe [CapacityProviderStrategyItem],
    -- | The metadata that you apply to the service to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
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
    tags :: Lude.Maybe [Tag],
    -- | The ARN of the IAM role associated with the service that allows the Amazon ECS container agent to register container instances with an Elastic Load Balancing load balancer.
    roleARN :: Lude.Maybe Lude.Text,
    -- | Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
    deploymentConfiguration :: Lude.Maybe DeploymentConfiguration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContainerService' with the minimum fields required to make a request.
--
-- * 'taskSets' - Information about a set of Amazon ECS tasks in either an AWS CodeDeploy or an @EXTERNAL@ deployment. An Amazon ECS task set includes details such as the desired number of tasks, how many tasks are running, and whether the task set serves production traffic.
-- * 'runningCount' - The number of tasks in the cluster that are in the @RUNNING@ state.
-- * 'status' - The status of the service. The valid values are @ACTIVE@ , @DRAINING@ , or @INACTIVE@ .
-- * 'clusterARN' - The Amazon Resource Name (ARN) of the cluster that hosts the service.
-- * 'propagateTags' - Specifies whether to propagate the tags from the task definition or the service to the task. If no value is specified, the tags are not propagated.
-- * 'createdAt' - The Unix timestamp for when the service was created.
-- * 'platformVersion' - The platform version on which to run your service. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'enableECSManagedTags' - Specifies whether to enable Amazon ECS managed tags for the tasks in the service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'createdBy' - The principal that created the service.
-- * 'desiredCount' - The desired number of instantiations of the task definition to keep running on the service. This value is specified when the service is created with 'CreateService' , and it can be modified with 'UpdateService' .
-- * 'loadBalancers' - A list of Elastic Load Balancing load balancer objects, containing the load balancer name, the container name (as it appears in a container definition), and the container port to access from the load balancer.
-- * 'pendingCount' - The number of tasks in the cluster that are in the @PENDING@ state.
-- * 'placementConstraints' - The placement constraints for the tasks in the service.
-- * 'events' - The event stream for your service. A maximum of 100 of the latest events are displayed.
-- * 'placementStrategy' - The placement strategy that determines how tasks for the service are placed.
-- * 'deployments' - The current state of deployments for the service.
-- * 'serviceName' - The name of your service. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. Service names must be unique within a cluster, but you can have similarly named services in multiple clusters within a Region or across multiple Regions.
-- * 'deploymentController' - The deployment controller type the service is using. When using the DescribeServices API, this field is omitted if the service is using the @ECS@ deployment controller type.
-- * 'launchType' - The launch type on which your service is running. If no value is specified, it will default to @EC2@ . Valid values include @EC2@ and @FARGATE@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'serviceARN' - The ARN that identifies the service. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the service, the AWS account ID of the service owner, the @service@ namespace, and then the service name. For example, @arn:aws:ecs:region:012345678910:service/my-service@ .
-- * 'taskDefinition' - The task definition to use for tasks in the service. This value is specified when the service is created with 'CreateService' , and it can be modified with 'UpdateService' .
-- * 'schedulingStrategy' - The scheduling strategy to use for the service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Services> .
--
-- There are two service scheduler strategies available:
--
--     * @REPLICA@ -The replica scheduling strategy places and maintains the desired number of tasks across your cluster. By default, the service scheduler spreads tasks across Availability Zones. You can use task placement strategies and constraints to customize task placement decisions.
--
--
--     * @DAEMON@ -The daemon scheduling strategy deploys exactly one task on each active container instance that meets all of the task placement constraints that you specify in your cluster. The service scheduler also evaluates the task placement constraints for running tasks and will stop tasks that do not meet the placement constraints.
--
--
-- * 'healthCheckGracePeriodSeconds' - The period of time, in seconds, that the Amazon ECS service scheduler ignores unhealthy Elastic Load Balancing target health checks after a task has first started.
-- * 'networkConfiguration' - The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
-- * 'serviceRegistries' - The details of the service discovery registries to assign to this service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
-- * 'capacityProviderStrategy' - The capacity provider strategy associated with the service.
-- * 'tags' - The metadata that you apply to the service to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
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
-- * 'roleARN' - The ARN of the IAM role associated with the service that allows the Amazon ECS container agent to register container instances with an Elastic Load Balancing load balancer.
-- * 'deploymentConfiguration' - Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
mkContainerService ::
  ContainerService
mkContainerService =
  ContainerService'
    { taskSets = Lude.Nothing,
      runningCount = Lude.Nothing,
      status = Lude.Nothing,
      clusterARN = Lude.Nothing,
      propagateTags = Lude.Nothing,
      createdAt = Lude.Nothing,
      platformVersion = Lude.Nothing,
      enableECSManagedTags = Lude.Nothing,
      createdBy = Lude.Nothing,
      desiredCount = Lude.Nothing,
      loadBalancers = Lude.Nothing,
      pendingCount = Lude.Nothing,
      placementConstraints = Lude.Nothing,
      events = Lude.Nothing,
      placementStrategy = Lude.Nothing,
      deployments = Lude.Nothing,
      serviceName = Lude.Nothing,
      deploymentController = Lude.Nothing,
      launchType = Lude.Nothing,
      serviceARN = Lude.Nothing,
      taskDefinition = Lude.Nothing,
      schedulingStrategy = Lude.Nothing,
      healthCheckGracePeriodSeconds = Lude.Nothing,
      networkConfiguration = Lude.Nothing,
      serviceRegistries = Lude.Nothing,
      capacityProviderStrategy = Lude.Nothing,
      tags = Lude.Nothing,
      roleARN = Lude.Nothing,
      deploymentConfiguration = Lude.Nothing
    }

-- | Information about a set of Amazon ECS tasks in either an AWS CodeDeploy or an @EXTERNAL@ deployment. An Amazon ECS task set includes details such as the desired number of tasks, how many tasks are running, and whether the task set serves production traffic.
--
-- /Note:/ Consider using 'taskSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTaskSets :: Lens.Lens' ContainerService (Lude.Maybe [TaskSet])
csTaskSets = Lens.lens (taskSets :: ContainerService -> Lude.Maybe [TaskSet]) (\s a -> s {taskSets = a} :: ContainerService)
{-# DEPRECATED csTaskSets "Use generic-lens or generic-optics with 'taskSets' instead." #-}

-- | The number of tasks in the cluster that are in the @RUNNING@ state.
--
-- /Note:/ Consider using 'runningCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csRunningCount :: Lens.Lens' ContainerService (Lude.Maybe Lude.Int)
csRunningCount = Lens.lens (runningCount :: ContainerService -> Lude.Maybe Lude.Int) (\s a -> s {runningCount = a} :: ContainerService)
{-# DEPRECATED csRunningCount "Use generic-lens or generic-optics with 'runningCount' instead." #-}

-- | The status of the service. The valid values are @ACTIVE@ , @DRAINING@ , or @INACTIVE@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStatus :: Lens.Lens' ContainerService (Lude.Maybe Lude.Text)
csStatus = Lens.lens (status :: ContainerService -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ContainerService)
{-# DEPRECATED csStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Name (ARN) of the cluster that hosts the service.
--
-- /Note:/ Consider using 'clusterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csClusterARN :: Lens.Lens' ContainerService (Lude.Maybe Lude.Text)
csClusterARN = Lens.lens (clusterARN :: ContainerService -> Lude.Maybe Lude.Text) (\s a -> s {clusterARN = a} :: ContainerService)
{-# DEPRECATED csClusterARN "Use generic-lens or generic-optics with 'clusterARN' instead." #-}

-- | Specifies whether to propagate the tags from the task definition or the service to the task. If no value is specified, the tags are not propagated.
--
-- /Note:/ Consider using 'propagateTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPropagateTags :: Lens.Lens' ContainerService (Lude.Maybe PropagateTags)
csPropagateTags = Lens.lens (propagateTags :: ContainerService -> Lude.Maybe PropagateTags) (\s a -> s {propagateTags = a} :: ContainerService)
{-# DEPRECATED csPropagateTags "Use generic-lens or generic-optics with 'propagateTags' instead." #-}

-- | The Unix timestamp for when the service was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCreatedAt :: Lens.Lens' ContainerService (Lude.Maybe Lude.Timestamp)
csCreatedAt = Lens.lens (createdAt :: ContainerService -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: ContainerService)
{-# DEPRECATED csCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The platform version on which to run your service. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPlatformVersion :: Lens.Lens' ContainerService (Lude.Maybe Lude.Text)
csPlatformVersion = Lens.lens (platformVersion :: ContainerService -> Lude.Maybe Lude.Text) (\s a -> s {platformVersion = a} :: ContainerService)
{-# DEPRECATED csPlatformVersion "Use generic-lens or generic-optics with 'platformVersion' instead." #-}

-- | Specifies whether to enable Amazon ECS managed tags for the tasks in the service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'enableECSManagedTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csEnableECSManagedTags :: Lens.Lens' ContainerService (Lude.Maybe Lude.Bool)
csEnableECSManagedTags = Lens.lens (enableECSManagedTags :: ContainerService -> Lude.Maybe Lude.Bool) (\s a -> s {enableECSManagedTags = a} :: ContainerService)
{-# DEPRECATED csEnableECSManagedTags "Use generic-lens or generic-optics with 'enableECSManagedTags' instead." #-}

-- | The principal that created the service.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCreatedBy :: Lens.Lens' ContainerService (Lude.Maybe Lude.Text)
csCreatedBy = Lens.lens (createdBy :: ContainerService -> Lude.Maybe Lude.Text) (\s a -> s {createdBy = a} :: ContainerService)
{-# DEPRECATED csCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | The desired number of instantiations of the task definition to keep running on the service. This value is specified when the service is created with 'CreateService' , and it can be modified with 'UpdateService' .
--
-- /Note:/ Consider using 'desiredCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDesiredCount :: Lens.Lens' ContainerService (Lude.Maybe Lude.Int)
csDesiredCount = Lens.lens (desiredCount :: ContainerService -> Lude.Maybe Lude.Int) (\s a -> s {desiredCount = a} :: ContainerService)
{-# DEPRECATED csDesiredCount "Use generic-lens or generic-optics with 'desiredCount' instead." #-}

-- | A list of Elastic Load Balancing load balancer objects, containing the load balancer name, the container name (as it appears in a container definition), and the container port to access from the load balancer.
--
-- /Note:/ Consider using 'loadBalancers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csLoadBalancers :: Lens.Lens' ContainerService (Lude.Maybe [LoadBalancer])
csLoadBalancers = Lens.lens (loadBalancers :: ContainerService -> Lude.Maybe [LoadBalancer]) (\s a -> s {loadBalancers = a} :: ContainerService)
{-# DEPRECATED csLoadBalancers "Use generic-lens or generic-optics with 'loadBalancers' instead." #-}

-- | The number of tasks in the cluster that are in the @PENDING@ state.
--
-- /Note:/ Consider using 'pendingCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPendingCount :: Lens.Lens' ContainerService (Lude.Maybe Lude.Int)
csPendingCount = Lens.lens (pendingCount :: ContainerService -> Lude.Maybe Lude.Int) (\s a -> s {pendingCount = a} :: ContainerService)
{-# DEPRECATED csPendingCount "Use generic-lens or generic-optics with 'pendingCount' instead." #-}

-- | The placement constraints for the tasks in the service.
--
-- /Note:/ Consider using 'placementConstraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPlacementConstraints :: Lens.Lens' ContainerService (Lude.Maybe [PlacementConstraint])
csPlacementConstraints = Lens.lens (placementConstraints :: ContainerService -> Lude.Maybe [PlacementConstraint]) (\s a -> s {placementConstraints = a} :: ContainerService)
{-# DEPRECATED csPlacementConstraints "Use generic-lens or generic-optics with 'placementConstraints' instead." #-}

-- | The event stream for your service. A maximum of 100 of the latest events are displayed.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csEvents :: Lens.Lens' ContainerService (Lude.Maybe [ServiceEvent])
csEvents = Lens.lens (events :: ContainerService -> Lude.Maybe [ServiceEvent]) (\s a -> s {events = a} :: ContainerService)
{-# DEPRECATED csEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | The placement strategy that determines how tasks for the service are placed.
--
-- /Note:/ Consider using 'placementStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPlacementStrategy :: Lens.Lens' ContainerService (Lude.Maybe [PlacementStrategy])
csPlacementStrategy = Lens.lens (placementStrategy :: ContainerService -> Lude.Maybe [PlacementStrategy]) (\s a -> s {placementStrategy = a} :: ContainerService)
{-# DEPRECATED csPlacementStrategy "Use generic-lens or generic-optics with 'placementStrategy' instead." #-}

-- | The current state of deployments for the service.
--
-- /Note:/ Consider using 'deployments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDeployments :: Lens.Lens' ContainerService (Lude.Maybe [Deployment])
csDeployments = Lens.lens (deployments :: ContainerService -> Lude.Maybe [Deployment]) (\s a -> s {deployments = a} :: ContainerService)
{-# DEPRECATED csDeployments "Use generic-lens or generic-optics with 'deployments' instead." #-}

-- | The name of your service. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. Service names must be unique within a cluster, but you can have similarly named services in multiple clusters within a Region or across multiple Regions.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csServiceName :: Lens.Lens' ContainerService (Lude.Maybe Lude.Text)
csServiceName = Lens.lens (serviceName :: ContainerService -> Lude.Maybe Lude.Text) (\s a -> s {serviceName = a} :: ContainerService)
{-# DEPRECATED csServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The deployment controller type the service is using. When using the DescribeServices API, this field is omitted if the service is using the @ECS@ deployment controller type.
--
-- /Note:/ Consider using 'deploymentController' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDeploymentController :: Lens.Lens' ContainerService (Lude.Maybe DeploymentController)
csDeploymentController = Lens.lens (deploymentController :: ContainerService -> Lude.Maybe DeploymentController) (\s a -> s {deploymentController = a} :: ContainerService)
{-# DEPRECATED csDeploymentController "Use generic-lens or generic-optics with 'deploymentController' instead." #-}

-- | The launch type on which your service is running. If no value is specified, it will default to @EC2@ . Valid values include @EC2@ and @FARGATE@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'launchType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csLaunchType :: Lens.Lens' ContainerService (Lude.Maybe LaunchType)
csLaunchType = Lens.lens (launchType :: ContainerService -> Lude.Maybe LaunchType) (\s a -> s {launchType = a} :: ContainerService)
{-# DEPRECATED csLaunchType "Use generic-lens or generic-optics with 'launchType' instead." #-}

-- | The ARN that identifies the service. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the service, the AWS account ID of the service owner, the @service@ namespace, and then the service name. For example, @arn:aws:ecs:region:012345678910:service/my-service@ .
--
-- /Note:/ Consider using 'serviceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csServiceARN :: Lens.Lens' ContainerService (Lude.Maybe Lude.Text)
csServiceARN = Lens.lens (serviceARN :: ContainerService -> Lude.Maybe Lude.Text) (\s a -> s {serviceARN = a} :: ContainerService)
{-# DEPRECATED csServiceARN "Use generic-lens or generic-optics with 'serviceARN' instead." #-}

-- | The task definition to use for tasks in the service. This value is specified when the service is created with 'CreateService' , and it can be modified with 'UpdateService' .
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTaskDefinition :: Lens.Lens' ContainerService (Lude.Maybe Lude.Text)
csTaskDefinition = Lens.lens (taskDefinition :: ContainerService -> Lude.Maybe Lude.Text) (\s a -> s {taskDefinition = a} :: ContainerService)
{-# DEPRECATED csTaskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead." #-}

-- | The scheduling strategy to use for the service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Services> .
--
-- There are two service scheduler strategies available:
--
--     * @REPLICA@ -The replica scheduling strategy places and maintains the desired number of tasks across your cluster. By default, the service scheduler spreads tasks across Availability Zones. You can use task placement strategies and constraints to customize task placement decisions.
--
--
--     * @DAEMON@ -The daemon scheduling strategy deploys exactly one task on each active container instance that meets all of the task placement constraints that you specify in your cluster. The service scheduler also evaluates the task placement constraints for running tasks and will stop tasks that do not meet the placement constraints.
--
--
--
-- /Note:/ Consider using 'schedulingStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSchedulingStrategy :: Lens.Lens' ContainerService (Lude.Maybe SchedulingStrategy)
csSchedulingStrategy = Lens.lens (schedulingStrategy :: ContainerService -> Lude.Maybe SchedulingStrategy) (\s a -> s {schedulingStrategy = a} :: ContainerService)
{-# DEPRECATED csSchedulingStrategy "Use generic-lens or generic-optics with 'schedulingStrategy' instead." #-}

-- | The period of time, in seconds, that the Amazon ECS service scheduler ignores unhealthy Elastic Load Balancing target health checks after a task has first started.
--
-- /Note:/ Consider using 'healthCheckGracePeriodSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csHealthCheckGracePeriodSeconds :: Lens.Lens' ContainerService (Lude.Maybe Lude.Int)
csHealthCheckGracePeriodSeconds = Lens.lens (healthCheckGracePeriodSeconds :: ContainerService -> Lude.Maybe Lude.Int) (\s a -> s {healthCheckGracePeriodSeconds = a} :: ContainerService)
{-# DEPRECATED csHealthCheckGracePeriodSeconds "Use generic-lens or generic-optics with 'healthCheckGracePeriodSeconds' instead." #-}

-- | The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
--
-- /Note:/ Consider using 'networkConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csNetworkConfiguration :: Lens.Lens' ContainerService (Lude.Maybe NetworkConfiguration)
csNetworkConfiguration = Lens.lens (networkConfiguration :: ContainerService -> Lude.Maybe NetworkConfiguration) (\s a -> s {networkConfiguration = a} :: ContainerService)
{-# DEPRECATED csNetworkConfiguration "Use generic-lens or generic-optics with 'networkConfiguration' instead." #-}

-- | The details of the service discovery registries to assign to this service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
--
-- /Note:/ Consider using 'serviceRegistries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csServiceRegistries :: Lens.Lens' ContainerService (Lude.Maybe [ServiceRegistry])
csServiceRegistries = Lens.lens (serviceRegistries :: ContainerService -> Lude.Maybe [ServiceRegistry]) (\s a -> s {serviceRegistries = a} :: ContainerService)
{-# DEPRECATED csServiceRegistries "Use generic-lens or generic-optics with 'serviceRegistries' instead." #-}

-- | The capacity provider strategy associated with the service.
--
-- /Note:/ Consider using 'capacityProviderStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCapacityProviderStrategy :: Lens.Lens' ContainerService (Lude.Maybe [CapacityProviderStrategyItem])
csCapacityProviderStrategy = Lens.lens (capacityProviderStrategy :: ContainerService -> Lude.Maybe [CapacityProviderStrategyItem]) (\s a -> s {capacityProviderStrategy = a} :: ContainerService)
{-# DEPRECATED csCapacityProviderStrategy "Use generic-lens or generic-optics with 'capacityProviderStrategy' instead." #-}

-- | The metadata that you apply to the service to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
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
csTags :: Lens.Lens' ContainerService (Lude.Maybe [Tag])
csTags = Lens.lens (tags :: ContainerService -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ContainerService)
{-# DEPRECATED csTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ARN of the IAM role associated with the service that allows the Amazon ECS container agent to register container instances with an Elastic Load Balancing load balancer.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csRoleARN :: Lens.Lens' ContainerService (Lude.Maybe Lude.Text)
csRoleARN = Lens.lens (roleARN :: ContainerService -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: ContainerService)
{-# DEPRECATED csRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
--
-- /Note:/ Consider using 'deploymentConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDeploymentConfiguration :: Lens.Lens' ContainerService (Lude.Maybe DeploymentConfiguration)
csDeploymentConfiguration = Lens.lens (deploymentConfiguration :: ContainerService -> Lude.Maybe DeploymentConfiguration) (\s a -> s {deploymentConfiguration = a} :: ContainerService)
{-# DEPRECATED csDeploymentConfiguration "Use generic-lens or generic-optics with 'deploymentConfiguration' instead." #-}

instance Lude.FromJSON ContainerService where
  parseJSON =
    Lude.withObject
      "ContainerService"
      ( \x ->
          ContainerService'
            Lude.<$> (x Lude..:? "taskSets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "runningCount")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "clusterArn")
            Lude.<*> (x Lude..:? "propagateTags")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "platformVersion")
            Lude.<*> (x Lude..:? "enableECSManagedTags")
            Lude.<*> (x Lude..:? "createdBy")
            Lude.<*> (x Lude..:? "desiredCount")
            Lude.<*> (x Lude..:? "loadBalancers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "pendingCount")
            Lude.<*> (x Lude..:? "placementConstraints" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "events" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "placementStrategy" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "deployments" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "serviceName")
            Lude.<*> (x Lude..:? "deploymentController")
            Lude.<*> (x Lude..:? "launchType")
            Lude.<*> (x Lude..:? "serviceArn")
            Lude.<*> (x Lude..:? "taskDefinition")
            Lude.<*> (x Lude..:? "schedulingStrategy")
            Lude.<*> (x Lude..:? "healthCheckGracePeriodSeconds")
            Lude.<*> (x Lude..:? "networkConfiguration")
            Lude.<*> (x Lude..:? "serviceRegistries" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "capacityProviderStrategy" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "roleArn")
            Lude.<*> (x Lude..:? "deploymentConfiguration")
      )
