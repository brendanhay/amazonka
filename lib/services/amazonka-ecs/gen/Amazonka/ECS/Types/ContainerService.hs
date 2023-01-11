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
-- Module      : Amazonka.ECS.Types.ContainerService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ContainerService where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.CapacityProviderStrategyItem
import Amazonka.ECS.Types.Deployment
import Amazonka.ECS.Types.DeploymentConfiguration
import Amazonka.ECS.Types.DeploymentController
import Amazonka.ECS.Types.LaunchType
import Amazonka.ECS.Types.LoadBalancer
import Amazonka.ECS.Types.NetworkConfiguration
import Amazonka.ECS.Types.PlacementConstraint
import Amazonka.ECS.Types.PlacementStrategy
import Amazonka.ECS.Types.PropagateTags
import Amazonka.ECS.Types.SchedulingStrategy
import Amazonka.ECS.Types.ServiceEvent
import Amazonka.ECS.Types.ServiceRegistry
import Amazonka.ECS.Types.Tag
import Amazonka.ECS.Types.TaskSet
import qualified Amazonka.Prelude as Prelude

-- | Details on a service within a cluster
--
-- /See:/ 'newContainerService' smart constructor.
data ContainerService = ContainerService'
  { -- | The capacity provider strategy the service uses. When using the
    -- DescribeServices API, this field is omitted if the service was created
    -- using a launch type.
    capacityProviderStrategy :: Prelude.Maybe [CapacityProviderStrategyItem],
    -- | The Amazon Resource Name (ARN) of the cluster that hosts the service.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp for the time when the service was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The principal that created the service.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | Optional deployment parameters that control how many tasks run during
    -- the deployment and the ordering of stopping and starting tasks.
    deploymentConfiguration :: Prelude.Maybe DeploymentConfiguration,
    -- | The deployment controller type the service is using.
    deploymentController :: Prelude.Maybe DeploymentController,
    -- | The current state of deployments for the service.
    deployments :: Prelude.Maybe [Deployment],
    -- | The desired number of instantiations of the task definition to keep
    -- running on the service. This value is specified when the service is
    -- created with CreateService, and it can be modified with UpdateService.
    desiredCount :: Prelude.Maybe Prelude.Int,
    -- | Determines whether to use Amazon ECS managed tags for the tasks in the
    -- service. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    enableECSManagedTags :: Prelude.Maybe Prelude.Bool,
    -- | Determines whether the execute command functionality is enabled for the
    -- service. If @true@, the execute command functionality is enabled for all
    -- containers in tasks as part of the service.
    enableExecuteCommand :: Prelude.Maybe Prelude.Bool,
    -- | The event stream for your service. A maximum of 100 of the latest events
    -- are displayed.
    events :: Prelude.Maybe [ServiceEvent],
    -- | The period of time, in seconds, that the Amazon ECS service scheduler
    -- ignores unhealthy Elastic Load Balancing target health checks after a
    -- task has first started.
    healthCheckGracePeriodSeconds :: Prelude.Maybe Prelude.Int,
    -- | The launch type the service is using. When using the DescribeServices
    -- API, this field is omitted if the service was created using a capacity
    -- provider strategy.
    launchType :: Prelude.Maybe LaunchType,
    -- | A list of Elastic Load Balancing load balancer objects. It contains the
    -- load balancer name, the container name, and the container port to access
    -- from the load balancer. The container name is as it appears in a
    -- container definition.
    loadBalancers :: Prelude.Maybe [LoadBalancer],
    -- | The VPC subnet and security group configuration for tasks that receive
    -- their own elastic network interface by using the @awsvpc@ networking
    -- mode.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | The number of tasks in the cluster that are in the @PENDING@ state.
    pendingCount :: Prelude.Maybe Prelude.Int,
    -- | The placement constraints for the tasks in the service.
    placementConstraints :: Prelude.Maybe [PlacementConstraint],
    -- | The placement strategy that determines how tasks for the service are
    -- placed.
    placementStrategy :: Prelude.Maybe [PlacementStrategy],
    -- | The operating system that your tasks in the service run on. A platform
    -- family is specified only for tasks using the Fargate launch type.
    --
    -- All tasks that run as part of this service must use the same
    -- @platformFamily@ value as the service (for example, @LINUX@).
    platformFamily :: Prelude.Maybe Prelude.Text,
    -- | The platform version to run your service on. A platform version is only
    -- specified for tasks that are hosted on Fargate. If one isn\'t specified,
    -- the @LATEST@ platform version is used. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate Platform Versions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | Determines whether to propagate the tags from the task definition or the
    -- service to the task. If no value is specified, the tags aren\'t
    -- propagated.
    propagateTags :: Prelude.Maybe PropagateTags,
    -- | The ARN of the IAM role that\'s associated with the service. It allows
    -- the Amazon ECS container agent to register container instances with an
    -- Elastic Load Balancing load balancer.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The number of tasks in the cluster that are in the @RUNNING@ state.
    runningCount :: Prelude.Maybe Prelude.Int,
    -- | The scheduling strategy to use for the service. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Services>.
    --
    -- There are two service scheduler strategies available.
    --
    -- -   @REPLICA@-The replica scheduling strategy places and maintains the
    --     desired number of tasks across your cluster. By default, the service
    --     scheduler spreads tasks across Availability Zones. You can use task
    --     placement strategies and constraints to customize task placement
    --     decisions.
    --
    -- -   @DAEMON@-The daemon scheduling strategy deploys exactly one task on
    --     each active container instance. This task meets all of the task
    --     placement constraints that you specify in your cluster. The service
    --     scheduler also evaluates the task placement constraints for running
    --     tasks. It stop tasks that don\'t meet the placement constraints.
    --
    --     Fargate tasks don\'t support the @DAEMON@ scheduling strategy.
    schedulingStrategy :: Prelude.Maybe SchedulingStrategy,
    -- | The ARN that identifies the service. For more information about the ARN
    -- format, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-account-settings.html#ecs-resource-ids Amazon Resource Name (ARN)>
    -- in the /Amazon ECS Developer Guide/.
    serviceArn :: Prelude.Maybe Prelude.Text,
    -- | The name of your service. Up to 255 letters (uppercase and lowercase),
    -- numbers, underscores, and hyphens are allowed. Service names must be
    -- unique within a cluster. However, you can have similarly named services
    -- in multiple clusters within a Region or across multiple Regions.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The details for the service discovery registries to assign to this
    -- service. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery>.
    serviceRegistries :: Prelude.Maybe [ServiceRegistry],
    -- | The status of the service. The valid values are @ACTIVE@, @DRAINING@, or
    -- @INACTIVE@.
    status :: Prelude.Maybe Prelude.Text,
    -- | The metadata that you apply to the service to help you categorize and
    -- organize them. Each tag consists of a key and an optional value. You
    -- define bot the key and value.
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
    --     such as a prefix for either keys or values as it is reserved for
    --     Amazon Web Services use. You cannot edit or delete tag keys or
    --     values with this prefix. Tags with this prefix do not count against
    --     your tags per resource limit.
    tags :: Prelude.Maybe [Tag],
    -- | The task definition to use for tasks in the service. This value is
    -- specified when the service is created with CreateService, and it can be
    -- modified with UpdateService.
    taskDefinition :: Prelude.Maybe Prelude.Text,
    -- | Information about a set of Amazon ECS tasks in either an CodeDeploy or
    -- an @EXTERNAL@ deployment. An Amazon ECS task set includes details such
    -- as the desired number of tasks, how many tasks are running, and whether
    -- the task set serves production traffic.
    taskSets :: Prelude.Maybe [TaskSet]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityProviderStrategy', 'containerService_capacityProviderStrategy' - The capacity provider strategy the service uses. When using the
-- DescribeServices API, this field is omitted if the service was created
-- using a launch type.
--
-- 'clusterArn', 'containerService_clusterArn' - The Amazon Resource Name (ARN) of the cluster that hosts the service.
--
-- 'createdAt', 'containerService_createdAt' - The Unix timestamp for the time when the service was created.
--
-- 'createdBy', 'containerService_createdBy' - The principal that created the service.
--
-- 'deploymentConfiguration', 'containerService_deploymentConfiguration' - Optional deployment parameters that control how many tasks run during
-- the deployment and the ordering of stopping and starting tasks.
--
-- 'deploymentController', 'containerService_deploymentController' - The deployment controller type the service is using.
--
-- 'deployments', 'containerService_deployments' - The current state of deployments for the service.
--
-- 'desiredCount', 'containerService_desiredCount' - The desired number of instantiations of the task definition to keep
-- running on the service. This value is specified when the service is
-- created with CreateService, and it can be modified with UpdateService.
--
-- 'enableECSManagedTags', 'containerService_enableECSManagedTags' - Determines whether to use Amazon ECS managed tags for the tasks in the
-- service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'enableExecuteCommand', 'containerService_enableExecuteCommand' - Determines whether the execute command functionality is enabled for the
-- service. If @true@, the execute command functionality is enabled for all
-- containers in tasks as part of the service.
--
-- 'events', 'containerService_events' - The event stream for your service. A maximum of 100 of the latest events
-- are displayed.
--
-- 'healthCheckGracePeriodSeconds', 'containerService_healthCheckGracePeriodSeconds' - The period of time, in seconds, that the Amazon ECS service scheduler
-- ignores unhealthy Elastic Load Balancing target health checks after a
-- task has first started.
--
-- 'launchType', 'containerService_launchType' - The launch type the service is using. When using the DescribeServices
-- API, this field is omitted if the service was created using a capacity
-- provider strategy.
--
-- 'loadBalancers', 'containerService_loadBalancers' - A list of Elastic Load Balancing load balancer objects. It contains the
-- load balancer name, the container name, and the container port to access
-- from the load balancer. The container name is as it appears in a
-- container definition.
--
-- 'networkConfiguration', 'containerService_networkConfiguration' - The VPC subnet and security group configuration for tasks that receive
-- their own elastic network interface by using the @awsvpc@ networking
-- mode.
--
-- 'pendingCount', 'containerService_pendingCount' - The number of tasks in the cluster that are in the @PENDING@ state.
--
-- 'placementConstraints', 'containerService_placementConstraints' - The placement constraints for the tasks in the service.
--
-- 'placementStrategy', 'containerService_placementStrategy' - The placement strategy that determines how tasks for the service are
-- placed.
--
-- 'platformFamily', 'containerService_platformFamily' - The operating system that your tasks in the service run on. A platform
-- family is specified only for tasks using the Fargate launch type.
--
-- All tasks that run as part of this service must use the same
-- @platformFamily@ value as the service (for example, @LINUX@).
--
-- 'platformVersion', 'containerService_platformVersion' - The platform version to run your service on. A platform version is only
-- specified for tasks that are hosted on Fargate. If one isn\'t specified,
-- the @LATEST@ platform version is used. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'propagateTags', 'containerService_propagateTags' - Determines whether to propagate the tags from the task definition or the
-- service to the task. If no value is specified, the tags aren\'t
-- propagated.
--
-- 'roleArn', 'containerService_roleArn' - The ARN of the IAM role that\'s associated with the service. It allows
-- the Amazon ECS container agent to register container instances with an
-- Elastic Load Balancing load balancer.
--
-- 'runningCount', 'containerService_runningCount' - The number of tasks in the cluster that are in the @RUNNING@ state.
--
-- 'schedulingStrategy', 'containerService_schedulingStrategy' - The scheduling strategy to use for the service. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Services>.
--
-- There are two service scheduler strategies available.
--
-- -   @REPLICA@-The replica scheduling strategy places and maintains the
--     desired number of tasks across your cluster. By default, the service
--     scheduler spreads tasks across Availability Zones. You can use task
--     placement strategies and constraints to customize task placement
--     decisions.
--
-- -   @DAEMON@-The daemon scheduling strategy deploys exactly one task on
--     each active container instance. This task meets all of the task
--     placement constraints that you specify in your cluster. The service
--     scheduler also evaluates the task placement constraints for running
--     tasks. It stop tasks that don\'t meet the placement constraints.
--
--     Fargate tasks don\'t support the @DAEMON@ scheduling strategy.
--
-- 'serviceArn', 'containerService_serviceArn' - The ARN that identifies the service. For more information about the ARN
-- format, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-account-settings.html#ecs-resource-ids Amazon Resource Name (ARN)>
-- in the /Amazon ECS Developer Guide/.
--
-- 'serviceName', 'containerService_serviceName' - The name of your service. Up to 255 letters (uppercase and lowercase),
-- numbers, underscores, and hyphens are allowed. Service names must be
-- unique within a cluster. However, you can have similarly named services
-- in multiple clusters within a Region or across multiple Regions.
--
-- 'serviceRegistries', 'containerService_serviceRegistries' - The details for the service discovery registries to assign to this
-- service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery>.
--
-- 'status', 'containerService_status' - The status of the service. The valid values are @ACTIVE@, @DRAINING@, or
-- @INACTIVE@.
--
-- 'tags', 'containerService_tags' - The metadata that you apply to the service to help you categorize and
-- organize them. Each tag consists of a key and an optional value. You
-- define bot the key and value.
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
--     such as a prefix for either keys or values as it is reserved for
--     Amazon Web Services use. You cannot edit or delete tag keys or
--     values with this prefix. Tags with this prefix do not count against
--     your tags per resource limit.
--
-- 'taskDefinition', 'containerService_taskDefinition' - The task definition to use for tasks in the service. This value is
-- specified when the service is created with CreateService, and it can be
-- modified with UpdateService.
--
-- 'taskSets', 'containerService_taskSets' - Information about a set of Amazon ECS tasks in either an CodeDeploy or
-- an @EXTERNAL@ deployment. An Amazon ECS task set includes details such
-- as the desired number of tasks, how many tasks are running, and whether
-- the task set serves production traffic.
newContainerService ::
  ContainerService
newContainerService =
  ContainerService'
    { capacityProviderStrategy =
        Prelude.Nothing,
      clusterArn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      deploymentConfiguration = Prelude.Nothing,
      deploymentController = Prelude.Nothing,
      deployments = Prelude.Nothing,
      desiredCount = Prelude.Nothing,
      enableECSManagedTags = Prelude.Nothing,
      enableExecuteCommand = Prelude.Nothing,
      events = Prelude.Nothing,
      healthCheckGracePeriodSeconds = Prelude.Nothing,
      launchType = Prelude.Nothing,
      loadBalancers = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
      pendingCount = Prelude.Nothing,
      placementConstraints = Prelude.Nothing,
      placementStrategy = Prelude.Nothing,
      platformFamily = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      propagateTags = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      runningCount = Prelude.Nothing,
      schedulingStrategy = Prelude.Nothing,
      serviceArn = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      serviceRegistries = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      taskDefinition = Prelude.Nothing,
      taskSets = Prelude.Nothing
    }

-- | The capacity provider strategy the service uses. When using the
-- DescribeServices API, this field is omitted if the service was created
-- using a launch type.
containerService_capacityProviderStrategy :: Lens.Lens' ContainerService (Prelude.Maybe [CapacityProviderStrategyItem])
containerService_capacityProviderStrategy = Lens.lens (\ContainerService' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@ContainerService' {} a -> s {capacityProviderStrategy = a} :: ContainerService) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the cluster that hosts the service.
containerService_clusterArn :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Text)
containerService_clusterArn = Lens.lens (\ContainerService' {clusterArn} -> clusterArn) (\s@ContainerService' {} a -> s {clusterArn = a} :: ContainerService)

-- | The Unix timestamp for the time when the service was created.
containerService_createdAt :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.UTCTime)
containerService_createdAt = Lens.lens (\ContainerService' {createdAt} -> createdAt) (\s@ContainerService' {} a -> s {createdAt = a} :: ContainerService) Prelude.. Lens.mapping Data._Time

-- | The principal that created the service.
containerService_createdBy :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Text)
containerService_createdBy = Lens.lens (\ContainerService' {createdBy} -> createdBy) (\s@ContainerService' {} a -> s {createdBy = a} :: ContainerService)

-- | Optional deployment parameters that control how many tasks run during
-- the deployment and the ordering of stopping and starting tasks.
containerService_deploymentConfiguration :: Lens.Lens' ContainerService (Prelude.Maybe DeploymentConfiguration)
containerService_deploymentConfiguration = Lens.lens (\ContainerService' {deploymentConfiguration} -> deploymentConfiguration) (\s@ContainerService' {} a -> s {deploymentConfiguration = a} :: ContainerService)

-- | The deployment controller type the service is using.
containerService_deploymentController :: Lens.Lens' ContainerService (Prelude.Maybe DeploymentController)
containerService_deploymentController = Lens.lens (\ContainerService' {deploymentController} -> deploymentController) (\s@ContainerService' {} a -> s {deploymentController = a} :: ContainerService)

-- | The current state of deployments for the service.
containerService_deployments :: Lens.Lens' ContainerService (Prelude.Maybe [Deployment])
containerService_deployments = Lens.lens (\ContainerService' {deployments} -> deployments) (\s@ContainerService' {} a -> s {deployments = a} :: ContainerService) Prelude.. Lens.mapping Lens.coerced

-- | The desired number of instantiations of the task definition to keep
-- running on the service. This value is specified when the service is
-- created with CreateService, and it can be modified with UpdateService.
containerService_desiredCount :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Int)
containerService_desiredCount = Lens.lens (\ContainerService' {desiredCount} -> desiredCount) (\s@ContainerService' {} a -> s {desiredCount = a} :: ContainerService)

-- | Determines whether to use Amazon ECS managed tags for the tasks in the
-- service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
-- in the /Amazon Elastic Container Service Developer Guide/.
containerService_enableECSManagedTags :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Bool)
containerService_enableECSManagedTags = Lens.lens (\ContainerService' {enableECSManagedTags} -> enableECSManagedTags) (\s@ContainerService' {} a -> s {enableECSManagedTags = a} :: ContainerService)

-- | Determines whether the execute command functionality is enabled for the
-- service. If @true@, the execute command functionality is enabled for all
-- containers in tasks as part of the service.
containerService_enableExecuteCommand :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Bool)
containerService_enableExecuteCommand = Lens.lens (\ContainerService' {enableExecuteCommand} -> enableExecuteCommand) (\s@ContainerService' {} a -> s {enableExecuteCommand = a} :: ContainerService)

-- | The event stream for your service. A maximum of 100 of the latest events
-- are displayed.
containerService_events :: Lens.Lens' ContainerService (Prelude.Maybe [ServiceEvent])
containerService_events = Lens.lens (\ContainerService' {events} -> events) (\s@ContainerService' {} a -> s {events = a} :: ContainerService) Prelude.. Lens.mapping Lens.coerced

-- | The period of time, in seconds, that the Amazon ECS service scheduler
-- ignores unhealthy Elastic Load Balancing target health checks after a
-- task has first started.
containerService_healthCheckGracePeriodSeconds :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Int)
containerService_healthCheckGracePeriodSeconds = Lens.lens (\ContainerService' {healthCheckGracePeriodSeconds} -> healthCheckGracePeriodSeconds) (\s@ContainerService' {} a -> s {healthCheckGracePeriodSeconds = a} :: ContainerService)

-- | The launch type the service is using. When using the DescribeServices
-- API, this field is omitted if the service was created using a capacity
-- provider strategy.
containerService_launchType :: Lens.Lens' ContainerService (Prelude.Maybe LaunchType)
containerService_launchType = Lens.lens (\ContainerService' {launchType} -> launchType) (\s@ContainerService' {} a -> s {launchType = a} :: ContainerService)

-- | A list of Elastic Load Balancing load balancer objects. It contains the
-- load balancer name, the container name, and the container port to access
-- from the load balancer. The container name is as it appears in a
-- container definition.
containerService_loadBalancers :: Lens.Lens' ContainerService (Prelude.Maybe [LoadBalancer])
containerService_loadBalancers = Lens.lens (\ContainerService' {loadBalancers} -> loadBalancers) (\s@ContainerService' {} a -> s {loadBalancers = a} :: ContainerService) Prelude.. Lens.mapping Lens.coerced

-- | The VPC subnet and security group configuration for tasks that receive
-- their own elastic network interface by using the @awsvpc@ networking
-- mode.
containerService_networkConfiguration :: Lens.Lens' ContainerService (Prelude.Maybe NetworkConfiguration)
containerService_networkConfiguration = Lens.lens (\ContainerService' {networkConfiguration} -> networkConfiguration) (\s@ContainerService' {} a -> s {networkConfiguration = a} :: ContainerService)

-- | The number of tasks in the cluster that are in the @PENDING@ state.
containerService_pendingCount :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Int)
containerService_pendingCount = Lens.lens (\ContainerService' {pendingCount} -> pendingCount) (\s@ContainerService' {} a -> s {pendingCount = a} :: ContainerService)

-- | The placement constraints for the tasks in the service.
containerService_placementConstraints :: Lens.Lens' ContainerService (Prelude.Maybe [PlacementConstraint])
containerService_placementConstraints = Lens.lens (\ContainerService' {placementConstraints} -> placementConstraints) (\s@ContainerService' {} a -> s {placementConstraints = a} :: ContainerService) Prelude.. Lens.mapping Lens.coerced

-- | The placement strategy that determines how tasks for the service are
-- placed.
containerService_placementStrategy :: Lens.Lens' ContainerService (Prelude.Maybe [PlacementStrategy])
containerService_placementStrategy = Lens.lens (\ContainerService' {placementStrategy} -> placementStrategy) (\s@ContainerService' {} a -> s {placementStrategy = a} :: ContainerService) Prelude.. Lens.mapping Lens.coerced

-- | The operating system that your tasks in the service run on. A platform
-- family is specified only for tasks using the Fargate launch type.
--
-- All tasks that run as part of this service must use the same
-- @platformFamily@ value as the service (for example, @LINUX@).
containerService_platformFamily :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Text)
containerService_platformFamily = Lens.lens (\ContainerService' {platformFamily} -> platformFamily) (\s@ContainerService' {} a -> s {platformFamily = a} :: ContainerService)

-- | The platform version to run your service on. A platform version is only
-- specified for tasks that are hosted on Fargate. If one isn\'t specified,
-- the @LATEST@ platform version is used. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
containerService_platformVersion :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Text)
containerService_platformVersion = Lens.lens (\ContainerService' {platformVersion} -> platformVersion) (\s@ContainerService' {} a -> s {platformVersion = a} :: ContainerService)

-- | Determines whether to propagate the tags from the task definition or the
-- service to the task. If no value is specified, the tags aren\'t
-- propagated.
containerService_propagateTags :: Lens.Lens' ContainerService (Prelude.Maybe PropagateTags)
containerService_propagateTags = Lens.lens (\ContainerService' {propagateTags} -> propagateTags) (\s@ContainerService' {} a -> s {propagateTags = a} :: ContainerService)

-- | The ARN of the IAM role that\'s associated with the service. It allows
-- the Amazon ECS container agent to register container instances with an
-- Elastic Load Balancing load balancer.
containerService_roleArn :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Text)
containerService_roleArn = Lens.lens (\ContainerService' {roleArn} -> roleArn) (\s@ContainerService' {} a -> s {roleArn = a} :: ContainerService)

-- | The number of tasks in the cluster that are in the @RUNNING@ state.
containerService_runningCount :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Int)
containerService_runningCount = Lens.lens (\ContainerService' {runningCount} -> runningCount) (\s@ContainerService' {} a -> s {runningCount = a} :: ContainerService)

-- | The scheduling strategy to use for the service. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Services>.
--
-- There are two service scheduler strategies available.
--
-- -   @REPLICA@-The replica scheduling strategy places and maintains the
--     desired number of tasks across your cluster. By default, the service
--     scheduler spreads tasks across Availability Zones. You can use task
--     placement strategies and constraints to customize task placement
--     decisions.
--
-- -   @DAEMON@-The daemon scheduling strategy deploys exactly one task on
--     each active container instance. This task meets all of the task
--     placement constraints that you specify in your cluster. The service
--     scheduler also evaluates the task placement constraints for running
--     tasks. It stop tasks that don\'t meet the placement constraints.
--
--     Fargate tasks don\'t support the @DAEMON@ scheduling strategy.
containerService_schedulingStrategy :: Lens.Lens' ContainerService (Prelude.Maybe SchedulingStrategy)
containerService_schedulingStrategy = Lens.lens (\ContainerService' {schedulingStrategy} -> schedulingStrategy) (\s@ContainerService' {} a -> s {schedulingStrategy = a} :: ContainerService)

-- | The ARN that identifies the service. For more information about the ARN
-- format, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-account-settings.html#ecs-resource-ids Amazon Resource Name (ARN)>
-- in the /Amazon ECS Developer Guide/.
containerService_serviceArn :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Text)
containerService_serviceArn = Lens.lens (\ContainerService' {serviceArn} -> serviceArn) (\s@ContainerService' {} a -> s {serviceArn = a} :: ContainerService)

-- | The name of your service. Up to 255 letters (uppercase and lowercase),
-- numbers, underscores, and hyphens are allowed. Service names must be
-- unique within a cluster. However, you can have similarly named services
-- in multiple clusters within a Region or across multiple Regions.
containerService_serviceName :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Text)
containerService_serviceName = Lens.lens (\ContainerService' {serviceName} -> serviceName) (\s@ContainerService' {} a -> s {serviceName = a} :: ContainerService)

-- | The details for the service discovery registries to assign to this
-- service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery>.
containerService_serviceRegistries :: Lens.Lens' ContainerService (Prelude.Maybe [ServiceRegistry])
containerService_serviceRegistries = Lens.lens (\ContainerService' {serviceRegistries} -> serviceRegistries) (\s@ContainerService' {} a -> s {serviceRegistries = a} :: ContainerService) Prelude.. Lens.mapping Lens.coerced

-- | The status of the service. The valid values are @ACTIVE@, @DRAINING@, or
-- @INACTIVE@.
containerService_status :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Text)
containerService_status = Lens.lens (\ContainerService' {status} -> status) (\s@ContainerService' {} a -> s {status = a} :: ContainerService)

-- | The metadata that you apply to the service to help you categorize and
-- organize them. Each tag consists of a key and an optional value. You
-- define bot the key and value.
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
--     such as a prefix for either keys or values as it is reserved for
--     Amazon Web Services use. You cannot edit or delete tag keys or
--     values with this prefix. Tags with this prefix do not count against
--     your tags per resource limit.
containerService_tags :: Lens.Lens' ContainerService (Prelude.Maybe [Tag])
containerService_tags = Lens.lens (\ContainerService' {tags} -> tags) (\s@ContainerService' {} a -> s {tags = a} :: ContainerService) Prelude.. Lens.mapping Lens.coerced

-- | The task definition to use for tasks in the service. This value is
-- specified when the service is created with CreateService, and it can be
-- modified with UpdateService.
containerService_taskDefinition :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Text)
containerService_taskDefinition = Lens.lens (\ContainerService' {taskDefinition} -> taskDefinition) (\s@ContainerService' {} a -> s {taskDefinition = a} :: ContainerService)

-- | Information about a set of Amazon ECS tasks in either an CodeDeploy or
-- an @EXTERNAL@ deployment. An Amazon ECS task set includes details such
-- as the desired number of tasks, how many tasks are running, and whether
-- the task set serves production traffic.
containerService_taskSets :: Lens.Lens' ContainerService (Prelude.Maybe [TaskSet])
containerService_taskSets = Lens.lens (\ContainerService' {taskSets} -> taskSets) (\s@ContainerService' {} a -> s {taskSets = a} :: ContainerService) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ContainerService where
  parseJSON =
    Data.withObject
      "ContainerService"
      ( \x ->
          ContainerService'
            Prelude.<$> ( x Data..:? "capacityProviderStrategy"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "clusterArn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "createdBy")
            Prelude.<*> (x Data..:? "deploymentConfiguration")
            Prelude.<*> (x Data..:? "deploymentController")
            Prelude.<*> (x Data..:? "deployments" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "desiredCount")
            Prelude.<*> (x Data..:? "enableECSManagedTags")
            Prelude.<*> (x Data..:? "enableExecuteCommand")
            Prelude.<*> (x Data..:? "events" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "healthCheckGracePeriodSeconds")
            Prelude.<*> (x Data..:? "launchType")
            Prelude.<*> (x Data..:? "loadBalancers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "networkConfiguration")
            Prelude.<*> (x Data..:? "pendingCount")
            Prelude.<*> ( x Data..:? "placementConstraints"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "placementStrategy"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "platformFamily")
            Prelude.<*> (x Data..:? "platformVersion")
            Prelude.<*> (x Data..:? "propagateTags")
            Prelude.<*> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "runningCount")
            Prelude.<*> (x Data..:? "schedulingStrategy")
            Prelude.<*> (x Data..:? "serviceArn")
            Prelude.<*> (x Data..:? "serviceName")
            Prelude.<*> ( x Data..:? "serviceRegistries"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "taskDefinition")
            Prelude.<*> (x Data..:? "taskSets" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ContainerService where
  hashWithSalt _salt ContainerService' {..} =
    _salt
      `Prelude.hashWithSalt` capacityProviderStrategy
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` deploymentConfiguration
      `Prelude.hashWithSalt` deploymentController
      `Prelude.hashWithSalt` deployments
      `Prelude.hashWithSalt` desiredCount
      `Prelude.hashWithSalt` enableECSManagedTags
      `Prelude.hashWithSalt` enableExecuteCommand
      `Prelude.hashWithSalt` events
      `Prelude.hashWithSalt` healthCheckGracePeriodSeconds
      `Prelude.hashWithSalt` launchType
      `Prelude.hashWithSalt` loadBalancers
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` pendingCount
      `Prelude.hashWithSalt` placementConstraints
      `Prelude.hashWithSalt` placementStrategy
      `Prelude.hashWithSalt` platformFamily
      `Prelude.hashWithSalt` platformVersion
      `Prelude.hashWithSalt` propagateTags
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` runningCount
      `Prelude.hashWithSalt` schedulingStrategy
      `Prelude.hashWithSalt` serviceArn
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` serviceRegistries
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` taskDefinition
      `Prelude.hashWithSalt` taskSets

instance Prelude.NFData ContainerService where
  rnf ContainerService' {..} =
    Prelude.rnf capacityProviderStrategy
      `Prelude.seq` Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf deploymentConfiguration
      `Prelude.seq` Prelude.rnf deploymentController
      `Prelude.seq` Prelude.rnf deployments
      `Prelude.seq` Prelude.rnf desiredCount
      `Prelude.seq` Prelude.rnf enableECSManagedTags
      `Prelude.seq` Prelude.rnf enableExecuteCommand
      `Prelude.seq` Prelude.rnf events
      `Prelude.seq` Prelude.rnf healthCheckGracePeriodSeconds
      `Prelude.seq` Prelude.rnf launchType
      `Prelude.seq` Prelude.rnf loadBalancers
      `Prelude.seq` Prelude.rnf networkConfiguration
      `Prelude.seq` Prelude.rnf pendingCount
      `Prelude.seq` Prelude.rnf placementConstraints
      `Prelude.seq` Prelude.rnf placementStrategy
      `Prelude.seq` Prelude.rnf platformFamily
      `Prelude.seq` Prelude.rnf platformVersion
      `Prelude.seq` Prelude.rnf propagateTags
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf runningCount
      `Prelude.seq` Prelude.rnf
        schedulingStrategy
      `Prelude.seq` Prelude.rnf
        serviceArn
      `Prelude.seq` Prelude.rnf
        serviceName
      `Prelude.seq` Prelude.rnf
        serviceRegistries
      `Prelude.seq` Prelude.rnf
        status
      `Prelude.seq` Prelude.rnf
        tags
      `Prelude.seq` Prelude.rnf
        taskDefinition
      `Prelude.seq` Prelude.rnf
        taskSets
