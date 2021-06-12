{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerService where

import qualified Network.AWS.Core as Core
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

-- | Details on a service within a cluster
--
-- /See:/ 'newContainerService' smart constructor.
data ContainerService = ContainerService'
  { -- | The Amazon Resource Name (ARN) of the cluster that hosts the service.
    clusterArn :: Core.Maybe Core.Text,
    -- | Information about a set of Amazon ECS tasks in either an AWS CodeDeploy
    -- or an @EXTERNAL@ deployment. An Amazon ECS task set includes details
    -- such as the desired number of tasks, how many tasks are running, and
    -- whether the task set serves production traffic.
    taskSets :: Core.Maybe [TaskSet],
    -- | The status of the service. The valid values are @ACTIVE@, @DRAINING@, or
    -- @INACTIVE@.
    status :: Core.Maybe Core.Text,
    -- | The number of tasks in the cluster that are in the @RUNNING@ state.
    runningCount :: Core.Maybe Core.Int,
    -- | The ARN of the IAM role associated with the service that allows the
    -- Amazon ECS container agent to register container instances with an
    -- Elastic Load Balancing load balancer.
    roleArn :: Core.Maybe Core.Text,
    -- | Optional deployment parameters that control how many tasks run during
    -- the deployment and the ordering of stopping and starting tasks.
    deploymentConfiguration :: Core.Maybe DeploymentConfiguration,
    -- | The VPC subnet and security group configuration for tasks that receive
    -- their own elastic network interface by using the @awsvpc@ networking
    -- mode.
    networkConfiguration :: Core.Maybe NetworkConfiguration,
    -- | The capacity provider strategy associated with the service.
    capacityProviderStrategy :: Core.Maybe [CapacityProviderStrategyItem],
    -- | The desired number of instantiations of the task definition to keep
    -- running on the service. This value is specified when the service is
    -- created with CreateService, and it can be modified with UpdateService.
    desiredCount :: Core.Maybe Core.Int,
    -- | Specifies whether to enable Amazon ECS managed tags for the tasks in the
    -- service. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    enableECSManagedTags :: Core.Maybe Core.Bool,
    -- | The launch type on which your service is running. If no value is
    -- specified, it will default to @EC2@. Valid values include @EC2@ and
    -- @FARGATE@. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    launchType :: Core.Maybe LaunchType,
    -- | The Unix timestamp for when the service was created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The platform version on which to run your service. A platform version is
    -- only specified for tasks using the Fargate launch type. If one is not
    -- specified, the @LATEST@ platform version is used by default. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    platformVersion :: Core.Maybe Core.Text,
    -- | The deployment controller type the service is using. When using the
    -- DescribeServices API, this field is omitted if the service is using the
    -- @ECS@ deployment controller type.
    deploymentController :: Core.Maybe DeploymentController,
    -- | The name of your service. Up to 255 letters (uppercase and lowercase),
    -- numbers, and hyphens are allowed. Service names must be unique within a
    -- cluster, but you can have similarly named services in multiple clusters
    -- within a Region or across multiple Regions.
    serviceName :: Core.Maybe Core.Text,
    -- | The placement strategy that determines how tasks for the service are
    -- placed.
    placementStrategy :: Core.Maybe [PlacementStrategy],
    -- | The current state of deployments for the service.
    deployments :: Core.Maybe [Deployment],
    -- | The placement constraints for the tasks in the service.
    placementConstraints :: Core.Maybe [PlacementConstraint],
    -- | The event stream for your service. A maximum of 100 of the latest events
    -- are displayed.
    events :: Core.Maybe [ServiceEvent],
    -- | The number of tasks in the cluster that are in the @PENDING@ state.
    pendingCount :: Core.Maybe Core.Int,
    -- | A list of Elastic Load Balancing load balancer objects, containing the
    -- load balancer name, the container name (as it appears in a container
    -- definition), and the container port to access from the load balancer.
    loadBalancers :: Core.Maybe [LoadBalancer],
    -- | The metadata that you apply to the service to help you categorize and
    -- organize them. Each tag consists of a key and an optional value, both of
    -- which you define.
    --
    -- The following basic restrictions apply to tags:
    --
    -- -   Maximum number of tags per resource - 50
    --
    -- -   For each resource, each tag key must be unique, and each tag key can
    --     have only one value.
    --
    -- -   Maximum key length - 128 Unicode characters in UTF-8
    --
    -- -   Maximum value length - 256 Unicode characters in UTF-8
    --
    -- -   If your tagging schema is used across multiple services and
    --     resources, remember that other services may have restrictions on
    --     allowed characters. Generally allowed characters are: letters,
    --     numbers, and spaces representable in UTF-8, and the following
    --     characters: + - = . _ : \/ \@.
    --
    -- -   Tag keys and values are case-sensitive.
    --
    -- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
    --     such as a prefix for either keys or values as it is reserved for AWS
    --     use. You cannot edit or delete tag keys or values with this prefix.
    --     Tags with this prefix do not count against your tags per resource
    --     limit.
    tags :: Core.Maybe [Tag],
    -- | The details of the service discovery registries to assign to this
    -- service. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery>.
    serviceRegistries :: Core.Maybe [ServiceRegistry],
    -- | The period of time, in seconds, that the Amazon ECS service scheduler
    -- ignores unhealthy Elastic Load Balancing target health checks after a
    -- task has first started.
    healthCheckGracePeriodSeconds :: Core.Maybe Core.Int,
    -- | The scheduling strategy to use for the service. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Services>.
    --
    -- There are two service scheduler strategies available:
    --
    -- -   @REPLICA@-The replica scheduling strategy places and maintains the
    --     desired number of tasks across your cluster. By default, the service
    --     scheduler spreads tasks across Availability Zones. You can use task
    --     placement strategies and constraints to customize task placement
    --     decisions.
    --
    -- -   @DAEMON@-The daemon scheduling strategy deploys exactly one task on
    --     each active container instance that meets all of the task placement
    --     constraints that you specify in your cluster. The service scheduler
    --     also evaluates the task placement constraints for running tasks and
    --     will stop tasks that do not meet the placement constraints.
    --
    --     Fargate tasks do not support the @DAEMON@ scheduling strategy.
    schedulingStrategy :: Core.Maybe SchedulingStrategy,
    -- | The principal that created the service.
    createdBy :: Core.Maybe Core.Text,
    -- | The task definition to use for tasks in the service. This value is
    -- specified when the service is created with CreateService, and it can be
    -- modified with UpdateService.
    taskDefinition :: Core.Maybe Core.Text,
    -- | The ARN that identifies the service. The ARN contains the @arn:aws:ecs@
    -- namespace, followed by the Region of the service, the AWS account ID of
    -- the service owner, the @service@ namespace, and then the service name.
    -- For example, @arn:aws:ecs:region:012345678910:service\/my-service@.
    serviceArn :: Core.Maybe Core.Text,
    -- | Specifies whether to propagate the tags from the task definition or the
    -- service to the task. If no value is specified, the tags are not
    -- propagated.
    propagateTags :: Core.Maybe PropagateTags
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ContainerService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'containerService_clusterArn' - The Amazon Resource Name (ARN) of the cluster that hosts the service.
--
-- 'taskSets', 'containerService_taskSets' - Information about a set of Amazon ECS tasks in either an AWS CodeDeploy
-- or an @EXTERNAL@ deployment. An Amazon ECS task set includes details
-- such as the desired number of tasks, how many tasks are running, and
-- whether the task set serves production traffic.
--
-- 'status', 'containerService_status' - The status of the service. The valid values are @ACTIVE@, @DRAINING@, or
-- @INACTIVE@.
--
-- 'runningCount', 'containerService_runningCount' - The number of tasks in the cluster that are in the @RUNNING@ state.
--
-- 'roleArn', 'containerService_roleArn' - The ARN of the IAM role associated with the service that allows the
-- Amazon ECS container agent to register container instances with an
-- Elastic Load Balancing load balancer.
--
-- 'deploymentConfiguration', 'containerService_deploymentConfiguration' - Optional deployment parameters that control how many tasks run during
-- the deployment and the ordering of stopping and starting tasks.
--
-- 'networkConfiguration', 'containerService_networkConfiguration' - The VPC subnet and security group configuration for tasks that receive
-- their own elastic network interface by using the @awsvpc@ networking
-- mode.
--
-- 'capacityProviderStrategy', 'containerService_capacityProviderStrategy' - The capacity provider strategy associated with the service.
--
-- 'desiredCount', 'containerService_desiredCount' - The desired number of instantiations of the task definition to keep
-- running on the service. This value is specified when the service is
-- created with CreateService, and it can be modified with UpdateService.
--
-- 'enableECSManagedTags', 'containerService_enableECSManagedTags' - Specifies whether to enable Amazon ECS managed tags for the tasks in the
-- service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'launchType', 'containerService_launchType' - The launch type on which your service is running. If no value is
-- specified, it will default to @EC2@. Valid values include @EC2@ and
-- @FARGATE@. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'createdAt', 'containerService_createdAt' - The Unix timestamp for when the service was created.
--
-- 'platformVersion', 'containerService_platformVersion' - The platform version on which to run your service. A platform version is
-- only specified for tasks using the Fargate launch type. If one is not
-- specified, the @LATEST@ platform version is used by default. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'deploymentController', 'containerService_deploymentController' - The deployment controller type the service is using. When using the
-- DescribeServices API, this field is omitted if the service is using the
-- @ECS@ deployment controller type.
--
-- 'serviceName', 'containerService_serviceName' - The name of your service. Up to 255 letters (uppercase and lowercase),
-- numbers, and hyphens are allowed. Service names must be unique within a
-- cluster, but you can have similarly named services in multiple clusters
-- within a Region or across multiple Regions.
--
-- 'placementStrategy', 'containerService_placementStrategy' - The placement strategy that determines how tasks for the service are
-- placed.
--
-- 'deployments', 'containerService_deployments' - The current state of deployments for the service.
--
-- 'placementConstraints', 'containerService_placementConstraints' - The placement constraints for the tasks in the service.
--
-- 'events', 'containerService_events' - The event stream for your service. A maximum of 100 of the latest events
-- are displayed.
--
-- 'pendingCount', 'containerService_pendingCount' - The number of tasks in the cluster that are in the @PENDING@ state.
--
-- 'loadBalancers', 'containerService_loadBalancers' - A list of Elastic Load Balancing load balancer objects, containing the
-- load balancer name, the container name (as it appears in a container
-- definition), and the container port to access from the load balancer.
--
-- 'tags', 'containerService_tags' - The metadata that you apply to the service to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8
--
-- -   Maximum value length - 256 Unicode characters in UTF-8
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case-sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for either keys or values as it is reserved for AWS
--     use. You cannot edit or delete tag keys or values with this prefix.
--     Tags with this prefix do not count against your tags per resource
--     limit.
--
-- 'serviceRegistries', 'containerService_serviceRegistries' - The details of the service discovery registries to assign to this
-- service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery>.
--
-- 'healthCheckGracePeriodSeconds', 'containerService_healthCheckGracePeriodSeconds' - The period of time, in seconds, that the Amazon ECS service scheduler
-- ignores unhealthy Elastic Load Balancing target health checks after a
-- task has first started.
--
-- 'schedulingStrategy', 'containerService_schedulingStrategy' - The scheduling strategy to use for the service. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Services>.
--
-- There are two service scheduler strategies available:
--
-- -   @REPLICA@-The replica scheduling strategy places and maintains the
--     desired number of tasks across your cluster. By default, the service
--     scheduler spreads tasks across Availability Zones. You can use task
--     placement strategies and constraints to customize task placement
--     decisions.
--
-- -   @DAEMON@-The daemon scheduling strategy deploys exactly one task on
--     each active container instance that meets all of the task placement
--     constraints that you specify in your cluster. The service scheduler
--     also evaluates the task placement constraints for running tasks and
--     will stop tasks that do not meet the placement constraints.
--
--     Fargate tasks do not support the @DAEMON@ scheduling strategy.
--
-- 'createdBy', 'containerService_createdBy' - The principal that created the service.
--
-- 'taskDefinition', 'containerService_taskDefinition' - The task definition to use for tasks in the service. This value is
-- specified when the service is created with CreateService, and it can be
-- modified with UpdateService.
--
-- 'serviceArn', 'containerService_serviceArn' - The ARN that identifies the service. The ARN contains the @arn:aws:ecs@
-- namespace, followed by the Region of the service, the AWS account ID of
-- the service owner, the @service@ namespace, and then the service name.
-- For example, @arn:aws:ecs:region:012345678910:service\/my-service@.
--
-- 'propagateTags', 'containerService_propagateTags' - Specifies whether to propagate the tags from the task definition or the
-- service to the task. If no value is specified, the tags are not
-- propagated.
newContainerService ::
  ContainerService
newContainerService =
  ContainerService'
    { clusterArn = Core.Nothing,
      taskSets = Core.Nothing,
      status = Core.Nothing,
      runningCount = Core.Nothing,
      roleArn = Core.Nothing,
      deploymentConfiguration = Core.Nothing,
      networkConfiguration = Core.Nothing,
      capacityProviderStrategy = Core.Nothing,
      desiredCount = Core.Nothing,
      enableECSManagedTags = Core.Nothing,
      launchType = Core.Nothing,
      createdAt = Core.Nothing,
      platformVersion = Core.Nothing,
      deploymentController = Core.Nothing,
      serviceName = Core.Nothing,
      placementStrategy = Core.Nothing,
      deployments = Core.Nothing,
      placementConstraints = Core.Nothing,
      events = Core.Nothing,
      pendingCount = Core.Nothing,
      loadBalancers = Core.Nothing,
      tags = Core.Nothing,
      serviceRegistries = Core.Nothing,
      healthCheckGracePeriodSeconds = Core.Nothing,
      schedulingStrategy = Core.Nothing,
      createdBy = Core.Nothing,
      taskDefinition = Core.Nothing,
      serviceArn = Core.Nothing,
      propagateTags = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the cluster that hosts the service.
containerService_clusterArn :: Lens.Lens' ContainerService (Core.Maybe Core.Text)
containerService_clusterArn = Lens.lens (\ContainerService' {clusterArn} -> clusterArn) (\s@ContainerService' {} a -> s {clusterArn = a} :: ContainerService)

-- | Information about a set of Amazon ECS tasks in either an AWS CodeDeploy
-- or an @EXTERNAL@ deployment. An Amazon ECS task set includes details
-- such as the desired number of tasks, how many tasks are running, and
-- whether the task set serves production traffic.
containerService_taskSets :: Lens.Lens' ContainerService (Core.Maybe [TaskSet])
containerService_taskSets = Lens.lens (\ContainerService' {taskSets} -> taskSets) (\s@ContainerService' {} a -> s {taskSets = a} :: ContainerService) Core.. Lens.mapping Lens._Coerce

-- | The status of the service. The valid values are @ACTIVE@, @DRAINING@, or
-- @INACTIVE@.
containerService_status :: Lens.Lens' ContainerService (Core.Maybe Core.Text)
containerService_status = Lens.lens (\ContainerService' {status} -> status) (\s@ContainerService' {} a -> s {status = a} :: ContainerService)

-- | The number of tasks in the cluster that are in the @RUNNING@ state.
containerService_runningCount :: Lens.Lens' ContainerService (Core.Maybe Core.Int)
containerService_runningCount = Lens.lens (\ContainerService' {runningCount} -> runningCount) (\s@ContainerService' {} a -> s {runningCount = a} :: ContainerService)

-- | The ARN of the IAM role associated with the service that allows the
-- Amazon ECS container agent to register container instances with an
-- Elastic Load Balancing load balancer.
containerService_roleArn :: Lens.Lens' ContainerService (Core.Maybe Core.Text)
containerService_roleArn = Lens.lens (\ContainerService' {roleArn} -> roleArn) (\s@ContainerService' {} a -> s {roleArn = a} :: ContainerService)

-- | Optional deployment parameters that control how many tasks run during
-- the deployment and the ordering of stopping and starting tasks.
containerService_deploymentConfiguration :: Lens.Lens' ContainerService (Core.Maybe DeploymentConfiguration)
containerService_deploymentConfiguration = Lens.lens (\ContainerService' {deploymentConfiguration} -> deploymentConfiguration) (\s@ContainerService' {} a -> s {deploymentConfiguration = a} :: ContainerService)

-- | The VPC subnet and security group configuration for tasks that receive
-- their own elastic network interface by using the @awsvpc@ networking
-- mode.
containerService_networkConfiguration :: Lens.Lens' ContainerService (Core.Maybe NetworkConfiguration)
containerService_networkConfiguration = Lens.lens (\ContainerService' {networkConfiguration} -> networkConfiguration) (\s@ContainerService' {} a -> s {networkConfiguration = a} :: ContainerService)

-- | The capacity provider strategy associated with the service.
containerService_capacityProviderStrategy :: Lens.Lens' ContainerService (Core.Maybe [CapacityProviderStrategyItem])
containerService_capacityProviderStrategy = Lens.lens (\ContainerService' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@ContainerService' {} a -> s {capacityProviderStrategy = a} :: ContainerService) Core.. Lens.mapping Lens._Coerce

-- | The desired number of instantiations of the task definition to keep
-- running on the service. This value is specified when the service is
-- created with CreateService, and it can be modified with UpdateService.
containerService_desiredCount :: Lens.Lens' ContainerService (Core.Maybe Core.Int)
containerService_desiredCount = Lens.lens (\ContainerService' {desiredCount} -> desiredCount) (\s@ContainerService' {} a -> s {desiredCount = a} :: ContainerService)

-- | Specifies whether to enable Amazon ECS managed tags for the tasks in the
-- service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
-- in the /Amazon Elastic Container Service Developer Guide/.
containerService_enableECSManagedTags :: Lens.Lens' ContainerService (Core.Maybe Core.Bool)
containerService_enableECSManagedTags = Lens.lens (\ContainerService' {enableECSManagedTags} -> enableECSManagedTags) (\s@ContainerService' {} a -> s {enableECSManagedTags = a} :: ContainerService)

-- | The launch type on which your service is running. If no value is
-- specified, it will default to @EC2@. Valid values include @EC2@ and
-- @FARGATE@. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
containerService_launchType :: Lens.Lens' ContainerService (Core.Maybe LaunchType)
containerService_launchType = Lens.lens (\ContainerService' {launchType} -> launchType) (\s@ContainerService' {} a -> s {launchType = a} :: ContainerService)

-- | The Unix timestamp for when the service was created.
containerService_createdAt :: Lens.Lens' ContainerService (Core.Maybe Core.UTCTime)
containerService_createdAt = Lens.lens (\ContainerService' {createdAt} -> createdAt) (\s@ContainerService' {} a -> s {createdAt = a} :: ContainerService) Core.. Lens.mapping Core._Time

-- | The platform version on which to run your service. A platform version is
-- only specified for tasks using the Fargate launch type. If one is not
-- specified, the @LATEST@ platform version is used by default. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
containerService_platformVersion :: Lens.Lens' ContainerService (Core.Maybe Core.Text)
containerService_platformVersion = Lens.lens (\ContainerService' {platformVersion} -> platformVersion) (\s@ContainerService' {} a -> s {platformVersion = a} :: ContainerService)

-- | The deployment controller type the service is using. When using the
-- DescribeServices API, this field is omitted if the service is using the
-- @ECS@ deployment controller type.
containerService_deploymentController :: Lens.Lens' ContainerService (Core.Maybe DeploymentController)
containerService_deploymentController = Lens.lens (\ContainerService' {deploymentController} -> deploymentController) (\s@ContainerService' {} a -> s {deploymentController = a} :: ContainerService)

-- | The name of your service. Up to 255 letters (uppercase and lowercase),
-- numbers, and hyphens are allowed. Service names must be unique within a
-- cluster, but you can have similarly named services in multiple clusters
-- within a Region or across multiple Regions.
containerService_serviceName :: Lens.Lens' ContainerService (Core.Maybe Core.Text)
containerService_serviceName = Lens.lens (\ContainerService' {serviceName} -> serviceName) (\s@ContainerService' {} a -> s {serviceName = a} :: ContainerService)

-- | The placement strategy that determines how tasks for the service are
-- placed.
containerService_placementStrategy :: Lens.Lens' ContainerService (Core.Maybe [PlacementStrategy])
containerService_placementStrategy = Lens.lens (\ContainerService' {placementStrategy} -> placementStrategy) (\s@ContainerService' {} a -> s {placementStrategy = a} :: ContainerService) Core.. Lens.mapping Lens._Coerce

-- | The current state of deployments for the service.
containerService_deployments :: Lens.Lens' ContainerService (Core.Maybe [Deployment])
containerService_deployments = Lens.lens (\ContainerService' {deployments} -> deployments) (\s@ContainerService' {} a -> s {deployments = a} :: ContainerService) Core.. Lens.mapping Lens._Coerce

-- | The placement constraints for the tasks in the service.
containerService_placementConstraints :: Lens.Lens' ContainerService (Core.Maybe [PlacementConstraint])
containerService_placementConstraints = Lens.lens (\ContainerService' {placementConstraints} -> placementConstraints) (\s@ContainerService' {} a -> s {placementConstraints = a} :: ContainerService) Core.. Lens.mapping Lens._Coerce

-- | The event stream for your service. A maximum of 100 of the latest events
-- are displayed.
containerService_events :: Lens.Lens' ContainerService (Core.Maybe [ServiceEvent])
containerService_events = Lens.lens (\ContainerService' {events} -> events) (\s@ContainerService' {} a -> s {events = a} :: ContainerService) Core.. Lens.mapping Lens._Coerce

-- | The number of tasks in the cluster that are in the @PENDING@ state.
containerService_pendingCount :: Lens.Lens' ContainerService (Core.Maybe Core.Int)
containerService_pendingCount = Lens.lens (\ContainerService' {pendingCount} -> pendingCount) (\s@ContainerService' {} a -> s {pendingCount = a} :: ContainerService)

-- | A list of Elastic Load Balancing load balancer objects, containing the
-- load balancer name, the container name (as it appears in a container
-- definition), and the container port to access from the load balancer.
containerService_loadBalancers :: Lens.Lens' ContainerService (Core.Maybe [LoadBalancer])
containerService_loadBalancers = Lens.lens (\ContainerService' {loadBalancers} -> loadBalancers) (\s@ContainerService' {} a -> s {loadBalancers = a} :: ContainerService) Core.. Lens.mapping Lens._Coerce

-- | The metadata that you apply to the service to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8
--
-- -   Maximum value length - 256 Unicode characters in UTF-8
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case-sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for either keys or values as it is reserved for AWS
--     use. You cannot edit or delete tag keys or values with this prefix.
--     Tags with this prefix do not count against your tags per resource
--     limit.
containerService_tags :: Lens.Lens' ContainerService (Core.Maybe [Tag])
containerService_tags = Lens.lens (\ContainerService' {tags} -> tags) (\s@ContainerService' {} a -> s {tags = a} :: ContainerService) Core.. Lens.mapping Lens._Coerce

-- | The details of the service discovery registries to assign to this
-- service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery>.
containerService_serviceRegistries :: Lens.Lens' ContainerService (Core.Maybe [ServiceRegistry])
containerService_serviceRegistries = Lens.lens (\ContainerService' {serviceRegistries} -> serviceRegistries) (\s@ContainerService' {} a -> s {serviceRegistries = a} :: ContainerService) Core.. Lens.mapping Lens._Coerce

-- | The period of time, in seconds, that the Amazon ECS service scheduler
-- ignores unhealthy Elastic Load Balancing target health checks after a
-- task has first started.
containerService_healthCheckGracePeriodSeconds :: Lens.Lens' ContainerService (Core.Maybe Core.Int)
containerService_healthCheckGracePeriodSeconds = Lens.lens (\ContainerService' {healthCheckGracePeriodSeconds} -> healthCheckGracePeriodSeconds) (\s@ContainerService' {} a -> s {healthCheckGracePeriodSeconds = a} :: ContainerService)

-- | The scheduling strategy to use for the service. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Services>.
--
-- There are two service scheduler strategies available:
--
-- -   @REPLICA@-The replica scheduling strategy places and maintains the
--     desired number of tasks across your cluster. By default, the service
--     scheduler spreads tasks across Availability Zones. You can use task
--     placement strategies and constraints to customize task placement
--     decisions.
--
-- -   @DAEMON@-The daemon scheduling strategy deploys exactly one task on
--     each active container instance that meets all of the task placement
--     constraints that you specify in your cluster. The service scheduler
--     also evaluates the task placement constraints for running tasks and
--     will stop tasks that do not meet the placement constraints.
--
--     Fargate tasks do not support the @DAEMON@ scheduling strategy.
containerService_schedulingStrategy :: Lens.Lens' ContainerService (Core.Maybe SchedulingStrategy)
containerService_schedulingStrategy = Lens.lens (\ContainerService' {schedulingStrategy} -> schedulingStrategy) (\s@ContainerService' {} a -> s {schedulingStrategy = a} :: ContainerService)

-- | The principal that created the service.
containerService_createdBy :: Lens.Lens' ContainerService (Core.Maybe Core.Text)
containerService_createdBy = Lens.lens (\ContainerService' {createdBy} -> createdBy) (\s@ContainerService' {} a -> s {createdBy = a} :: ContainerService)

-- | The task definition to use for tasks in the service. This value is
-- specified when the service is created with CreateService, and it can be
-- modified with UpdateService.
containerService_taskDefinition :: Lens.Lens' ContainerService (Core.Maybe Core.Text)
containerService_taskDefinition = Lens.lens (\ContainerService' {taskDefinition} -> taskDefinition) (\s@ContainerService' {} a -> s {taskDefinition = a} :: ContainerService)

-- | The ARN that identifies the service. The ARN contains the @arn:aws:ecs@
-- namespace, followed by the Region of the service, the AWS account ID of
-- the service owner, the @service@ namespace, and then the service name.
-- For example, @arn:aws:ecs:region:012345678910:service\/my-service@.
containerService_serviceArn :: Lens.Lens' ContainerService (Core.Maybe Core.Text)
containerService_serviceArn = Lens.lens (\ContainerService' {serviceArn} -> serviceArn) (\s@ContainerService' {} a -> s {serviceArn = a} :: ContainerService)

-- | Specifies whether to propagate the tags from the task definition or the
-- service to the task. If no value is specified, the tags are not
-- propagated.
containerService_propagateTags :: Lens.Lens' ContainerService (Core.Maybe PropagateTags)
containerService_propagateTags = Lens.lens (\ContainerService' {propagateTags} -> propagateTags) (\s@ContainerService' {} a -> s {propagateTags = a} :: ContainerService)

instance Core.FromJSON ContainerService where
  parseJSON =
    Core.withObject
      "ContainerService"
      ( \x ->
          ContainerService'
            Core.<$> (x Core..:? "clusterArn")
            Core.<*> (x Core..:? "taskSets" Core..!= Core.mempty)
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "runningCount")
            Core.<*> (x Core..:? "roleArn")
            Core.<*> (x Core..:? "deploymentConfiguration")
            Core.<*> (x Core..:? "networkConfiguration")
            Core.<*> ( x Core..:? "capacityProviderStrategy"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "desiredCount")
            Core.<*> (x Core..:? "enableECSManagedTags")
            Core.<*> (x Core..:? "launchType")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "platformVersion")
            Core.<*> (x Core..:? "deploymentController")
            Core.<*> (x Core..:? "serviceName")
            Core.<*> (x Core..:? "placementStrategy" Core..!= Core.mempty)
            Core.<*> (x Core..:? "deployments" Core..!= Core.mempty)
            Core.<*> ( x Core..:? "placementConstraints"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "events" Core..!= Core.mempty)
            Core.<*> (x Core..:? "pendingCount")
            Core.<*> (x Core..:? "loadBalancers" Core..!= Core.mempty)
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "serviceRegistries" Core..!= Core.mempty)
            Core.<*> (x Core..:? "healthCheckGracePeriodSeconds")
            Core.<*> (x Core..:? "schedulingStrategy")
            Core.<*> (x Core..:? "createdBy")
            Core.<*> (x Core..:? "taskDefinition")
            Core.<*> (x Core..:? "serviceArn")
            Core.<*> (x Core..:? "propagateTags")
      )

instance Core.Hashable ContainerService

instance Core.NFData ContainerService
