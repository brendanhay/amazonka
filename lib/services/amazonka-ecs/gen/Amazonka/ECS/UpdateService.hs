{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ECS.UpdateService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a service.
--
-- For services using the rolling update (@ECS@) you can update the desired
-- count, deployment configuration, network configuration, load balancers,
-- service registries, enable ECS managed tags option, propagate tags
-- option, task placement constraints and strategies, and task definition.
-- When you update any of these parameters, Amazon ECS starts new tasks
-- with the new configuration.
--
-- For services using the blue\/green (@CODE_DEPLOY@) deployment
-- controller, only the desired count, deployment configuration, health
-- check grace period, task placement constraints and strategies, enable
-- ECS managed tags option, and propagate tags can be updated using this
-- API. If the network configuration, platform version, task definition, or
-- load balancer need to be updated, create a new CodeDeploy deployment.
-- For more information, see
-- <https://docs.aws.amazon.com/codedeploy/latest/APIReference/API_CreateDeployment.html CreateDeployment>
-- in the /CodeDeploy API Reference/.
--
-- For services using an external deployment controller, you can update
-- only the desired count, task placement constraints and strategies,
-- health check grace period, enable ECS managed tags option, and propagate
-- tags option, using this API. If the launch type, load balancer, network
-- configuration, platform version, or task definition need to be updated,
-- create a new task set For more information, see CreateTaskSet.
--
-- You can add to or subtract from the number of instantiations of a task
-- definition in a service by specifying the cluster that the service is
-- running in and a new @desiredCount@ parameter.
--
-- If you have updated the Docker image of your application, you can create
-- a new task definition with that image and deploy it to your service. The
-- service scheduler uses the minimum healthy percent and maximum percent
-- parameters (in the service\'s deployment configuration) to determine the
-- deployment strategy.
--
-- If your updated Docker image uses the same tag as what is in the
-- existing task definition for your service (for example,
-- @my_image:latest@), you don\'t need to create a new revision of your
-- task definition. You can update the service using the
-- @forceNewDeployment@ option. The new tasks launched by the deployment
-- pull the current image\/tag combination from your repository when they
-- start.
--
-- You can also update the deployment configuration of a service. When a
-- deployment is triggered by updating the task definition of a service,
-- the service scheduler uses the deployment configuration parameters,
-- @minimumHealthyPercent@ and @maximumPercent@, to determine the
-- deployment strategy.
--
-- -   If @minimumHealthyPercent@ is below 100%, the scheduler can ignore
--     @desiredCount@ temporarily during a deployment. For example, if
--     @desiredCount@ is four tasks, a minimum of 50% allows the scheduler
--     to stop two existing tasks before starting two new tasks. Tasks for
--     services that don\'t use a load balancer are considered healthy if
--     they\'re in the @RUNNING@ state. Tasks for services that use a load
--     balancer are considered healthy if they\'re in the @RUNNING@ state
--     and are reported as healthy by the load balancer.
--
-- -   The @maximumPercent@ parameter represents an upper limit on the
--     number of running tasks during a deployment. You can use it to
--     define the deployment batch size. For example, if @desiredCount@ is
--     four tasks, a maximum of 200% starts four new tasks before stopping
--     the four older tasks (provided that the cluster resources required
--     to do this are available).
--
-- When UpdateService stops a task during a deployment, the equivalent of
-- @docker stop@ is issued to the containers running in the task. This
-- results in a @SIGTERM@ and a 30-second timeout. After this, @SIGKILL@ is
-- sent and the containers are forcibly stopped. If the container handles
-- the @SIGTERM@ gracefully and exits within 30 seconds from receiving it,
-- no @SIGKILL@ is sent.
--
-- When the service scheduler launches new tasks, it determines task
-- placement in your cluster with the following logic.
--
-- -   Determine which of the container instances in your cluster can
--     support your service\'s task definition. For example, they have the
--     required CPU, memory, ports, and container instance attributes.
--
-- -   By default, the service scheduler attempts to balance tasks across
--     Availability Zones in this manner even though you can choose a
--     different placement strategy.
--
--     -   Sort the valid container instances by the fewest number of
--         running tasks for this service in the same Availability Zone as
--         the instance. For example, if zone A has one running service
--         task and zones B and C each have zero, valid container instances
--         in either zone B or C are considered optimal for placement.
--
--     -   Place the new service task on a valid container instance in an
--         optimal Availability Zone (based on the previous steps),
--         favoring container instances with the fewest number of running
--         tasks for this service.
--
-- When the service scheduler stops running tasks, it attempts to maintain
-- balance across the Availability Zones in your cluster using the
-- following logic:
--
-- -   Sort the container instances by the largest number of running tasks
--     for this service in the same Availability Zone as the instance. For
--     example, if zone A has one running service task and zones B and C
--     each have two, container instances in either zone B or C are
--     considered optimal for termination.
--
-- -   Stop the task on a container instance in an optimal Availability
--     Zone (based on the previous steps), favoring container instances
--     with the largest number of running tasks for this service.
--
-- You must have a service-linked role when you update any of the following
-- service properties. If you specified a custom IAM role when you created
-- the service, Amazon ECS automatically replaces the
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_Service.html#ECS-Type-Service-roleArn roleARN>
-- associated with the service with the ARN of your service-linked role.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Service-linked roles>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- -   @loadBalancers,@
--
-- -   @serviceRegistries@
module Amazonka.ECS.UpdateService
  ( -- * Creating a Request
    UpdateService (..),
    newUpdateService,

    -- * Request Lenses
    updateService_capacityProviderStrategy,
    updateService_cluster,
    updateService_deploymentConfiguration,
    updateService_desiredCount,
    updateService_enableECSManagedTags,
    updateService_enableExecuteCommand,
    updateService_forceNewDeployment,
    updateService_healthCheckGracePeriodSeconds,
    updateService_loadBalancers,
    updateService_networkConfiguration,
    updateService_placementConstraints,
    updateService_placementStrategy,
    updateService_platformVersion,
    updateService_propagateTags,
    updateService_serviceConnectConfiguration,
    updateService_serviceRegistries,
    updateService_taskDefinition,
    updateService_service,

    -- * Destructuring the Response
    UpdateServiceResponse (..),
    newUpdateServiceResponse,

    -- * Response Lenses
    updateServiceResponse_service,
    updateServiceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateService' smart constructor.
data UpdateService = UpdateService'
  { -- | The capacity provider strategy to update the service to use.
    --
    -- if the service uses the default capacity provider strategy for the
    -- cluster, the service can be updated to use one or more capacity
    -- providers as opposed to the default capacity provider strategy. However,
    -- when a service is using a capacity provider strategy that\'s not the
    -- default capacity provider strategy, the service can\'t be updated to use
    -- the cluster\'s default capacity provider strategy.
    --
    -- A capacity provider strategy consists of one or more capacity providers
    -- along with the @base@ and @weight@ to assign to them. A capacity
    -- provider must be associated with the cluster to be used in a capacity
    -- provider strategy. The PutClusterCapacityProviders API is used to
    -- associate a capacity provider with a cluster. Only capacity providers
    -- with an @ACTIVE@ or @UPDATING@ status can be used.
    --
    -- If specifying a capacity provider that uses an Auto Scaling group, the
    -- capacity provider must already be created. New capacity providers can be
    -- created with the CreateCapacityProvider API operation.
    --
    -- To use a Fargate capacity provider, specify either the @FARGATE@ or
    -- @FARGATE_SPOT@ capacity providers. The Fargate capacity providers are
    -- available to all accounts and only need to be associated with a cluster
    -- to be used.
    --
    -- The PutClusterCapacityProviders API operation is used to update the list
    -- of available capacity providers for a cluster after the cluster is
    -- created.
    capacityProviderStrategy :: Prelude.Maybe [CapacityProviderStrategyItem],
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- your service runs on. If you do not specify a cluster, the default
    -- cluster is assumed.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | Optional deployment parameters that control how many tasks run during
    -- the deployment and the ordering of stopping and starting tasks.
    deploymentConfiguration :: Prelude.Maybe DeploymentConfiguration,
    -- | The number of instantiations of the task to place and keep running in
    -- your service.
    desiredCount :: Prelude.Maybe Prelude.Int,
    -- | Determines whether to turn on Amazon ECS managed tags for the tasks in
    -- the service. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- Only tasks launched after the update will reflect the update. To update
    -- the tags on all tasks, set @forceNewDeployment@ to @true@, so that
    -- Amazon ECS starts new tasks with the updated tags.
    enableECSManagedTags :: Prelude.Maybe Prelude.Bool,
    -- | If @true@, this enables execute command functionality on all task
    -- containers.
    --
    -- If you do not want to override the value that was set when the service
    -- was created, you can set this to @null@ when performing this action.
    enableExecuteCommand :: Prelude.Maybe Prelude.Bool,
    -- | Determines whether to force a new deployment of the service. By default,
    -- deployments aren\'t forced. You can use this option to start a new
    -- deployment with no service definition changes. For example, you can
    -- update a service\'s tasks to use a newer Docker image with the same
    -- image\/tag combination (@my_image:latest@) or to roll Fargate tasks onto
    -- a newer platform version.
    forceNewDeployment :: Prelude.Maybe Prelude.Bool,
    -- | The period of time, in seconds, that the Amazon ECS service scheduler
    -- ignores unhealthy Elastic Load Balancing target health checks after a
    -- task has first started. This is only valid if your service is configured
    -- to use a load balancer. If your service\'s tasks take a while to start
    -- and respond to Elastic Load Balancing health checks, you can specify a
    -- health check grace period of up to 2,147,483,647 seconds. During that
    -- time, the Amazon ECS service scheduler ignores the Elastic Load
    -- Balancing health check status. This grace period can prevent the ECS
    -- service scheduler from marking tasks as unhealthy and stopping them
    -- before they have time to come up.
    healthCheckGracePeriodSeconds :: Prelude.Maybe Prelude.Int,
    -- | A list of Elastic Load Balancing load balancer objects. It contains the
    -- load balancer name, the container name, and the container port to access
    -- from the load balancer. The container name is as it appears in a
    -- container definition.
    --
    -- When you add, update, or remove a load balancer configuration, Amazon
    -- ECS starts new tasks with the updated Elastic Load Balancing
    -- configuration, and then stops the old tasks when the new tasks are
    -- running.
    --
    -- For services that use rolling updates, you can add, update, or remove
    -- Elastic Load Balancing target groups. You can update from a single
    -- target group to multiple target groups and from multiple target groups
    -- to a single target group.
    --
    -- For services that use blue\/green deployments, you can update Elastic
    -- Load Balancing target groups by using
    -- @ @<https://docs.aws.amazon.com/codedeploy/latest/APIReference/API_CreateDeployment.html CreateDeployment>@ @
    -- through CodeDeploy. Note that multiple target groups are not supported
    -- for blue\/green deployments. For more information see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/register-multiple-targetgroups.html Register multiple target groups with a service>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- For services that use the external deployment controller, you can add,
    -- update, or remove load balancers by using
    -- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_CreateTaskSet.html CreateTaskSet>.
    -- Note that multiple target groups are not supported for external
    -- deployments. For more information see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/register-multiple-targetgroups.html Register multiple target groups with a service>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- You can remove existing @loadBalancers@ by passing an empty list.
    loadBalancers :: Prelude.Maybe [LoadBalancer],
    -- | An object representing the network configuration for the service.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | An array of task placement constraint objects to update the service to
    -- use. If no value is specified, the existing placement constraints for
    -- the service will remain unchanged. If this value is specified, it will
    -- override any existing placement constraints defined for the service. To
    -- remove all existing placement constraints, specify an empty array.
    --
    -- You can specify a maximum of 10 constraints for each task. This limit
    -- includes constraints in the task definition and those specified at
    -- runtime.
    placementConstraints :: Prelude.Maybe [PlacementConstraint],
    -- | The task placement strategy objects to update the service to use. If no
    -- value is specified, the existing placement strategy for the service will
    -- remain unchanged. If this value is specified, it will override the
    -- existing placement strategy defined for the service. To remove an
    -- existing placement strategy, specify an empty object.
    --
    -- You can specify a maximum of five strategy rules for each service.
    placementStrategy :: Prelude.Maybe [PlacementStrategy],
    -- | The platform version that your tasks in the service run on. A platform
    -- version is only specified for tasks using the Fargate launch type. If a
    -- platform version is not specified, the @LATEST@ platform version is
    -- used. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate Platform Versions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | Determines whether to propagate the tags from the task definition or the
    -- service to the task. If no value is specified, the tags aren\'t
    -- propagated.
    --
    -- Only tasks launched after the update will reflect the update. To update
    -- the tags on all tasks, set @forceNewDeployment@ to @true@, so that
    -- Amazon ECS starts new tasks with the updated tags.
    propagateTags :: Prelude.Maybe PropagateTags,
    -- | The configuration for this service to discover and connect to services,
    -- and be discovered by, and connected from, other services within a
    -- namespace.
    --
    -- Tasks that run in a namespace can use short names to connect to services
    -- in the namespace. Tasks can connect to services across all of the
    -- clusters in the namespace. Tasks connect through a managed proxy
    -- container that collects logs and metrics for increased visibility. Only
    -- the tasks that Amazon ECS services create are supported with Service
    -- Connect. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    serviceConnectConfiguration :: Prelude.Maybe ServiceConnectConfiguration,
    -- | The details for the service discovery registries to assign to this
    -- service. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery>.
    --
    -- When you add, update, or remove the service registries configuration,
    -- Amazon ECS starts new tasks with the updated service registries
    -- configuration, and then stops the old tasks when the new tasks are
    -- running.
    --
    -- You can remove existing @serviceRegistries@ by passing an empty list.
    serviceRegistries :: Prelude.Maybe [ServiceRegistry],
    -- | The @family@ and @revision@ (@family:revision@) or full ARN of the task
    -- definition to run in your service. If a @revision@ is not specified, the
    -- latest @ACTIVE@ revision is used. If you modify the task definition with
    -- @UpdateService@, Amazon ECS spawns a task with the new version of the
    -- task definition and then stops an old task after the new version is
    -- running.
    taskDefinition :: Prelude.Maybe Prelude.Text,
    -- | The name of the service to update.
    service :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityProviderStrategy', 'updateService_capacityProviderStrategy' - The capacity provider strategy to update the service to use.
--
-- if the service uses the default capacity provider strategy for the
-- cluster, the service can be updated to use one or more capacity
-- providers as opposed to the default capacity provider strategy. However,
-- when a service is using a capacity provider strategy that\'s not the
-- default capacity provider strategy, the service can\'t be updated to use
-- the cluster\'s default capacity provider strategy.
--
-- A capacity provider strategy consists of one or more capacity providers
-- along with the @base@ and @weight@ to assign to them. A capacity
-- provider must be associated with the cluster to be used in a capacity
-- provider strategy. The PutClusterCapacityProviders API is used to
-- associate a capacity provider with a cluster. Only capacity providers
-- with an @ACTIVE@ or @UPDATING@ status can be used.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the
-- capacity provider must already be created. New capacity providers can be
-- created with the CreateCapacityProvider API operation.
--
-- To use a Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The Fargate capacity providers are
-- available to all accounts and only need to be associated with a cluster
-- to be used.
--
-- The PutClusterCapacityProviders API operation is used to update the list
-- of available capacity providers for a cluster after the cluster is
-- created.
--
-- 'cluster', 'updateService_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- your service runs on. If you do not specify a cluster, the default
-- cluster is assumed.
--
-- 'deploymentConfiguration', 'updateService_deploymentConfiguration' - Optional deployment parameters that control how many tasks run during
-- the deployment and the ordering of stopping and starting tasks.
--
-- 'desiredCount', 'updateService_desiredCount' - The number of instantiations of the task to place and keep running in
-- your service.
--
-- 'enableECSManagedTags', 'updateService_enableECSManagedTags' - Determines whether to turn on Amazon ECS managed tags for the tasks in
-- the service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- Only tasks launched after the update will reflect the update. To update
-- the tags on all tasks, set @forceNewDeployment@ to @true@, so that
-- Amazon ECS starts new tasks with the updated tags.
--
-- 'enableExecuteCommand', 'updateService_enableExecuteCommand' - If @true@, this enables execute command functionality on all task
-- containers.
--
-- If you do not want to override the value that was set when the service
-- was created, you can set this to @null@ when performing this action.
--
-- 'forceNewDeployment', 'updateService_forceNewDeployment' - Determines whether to force a new deployment of the service. By default,
-- deployments aren\'t forced. You can use this option to start a new
-- deployment with no service definition changes. For example, you can
-- update a service\'s tasks to use a newer Docker image with the same
-- image\/tag combination (@my_image:latest@) or to roll Fargate tasks onto
-- a newer platform version.
--
-- 'healthCheckGracePeriodSeconds', 'updateService_healthCheckGracePeriodSeconds' - The period of time, in seconds, that the Amazon ECS service scheduler
-- ignores unhealthy Elastic Load Balancing target health checks after a
-- task has first started. This is only valid if your service is configured
-- to use a load balancer. If your service\'s tasks take a while to start
-- and respond to Elastic Load Balancing health checks, you can specify a
-- health check grace period of up to 2,147,483,647 seconds. During that
-- time, the Amazon ECS service scheduler ignores the Elastic Load
-- Balancing health check status. This grace period can prevent the ECS
-- service scheduler from marking tasks as unhealthy and stopping them
-- before they have time to come up.
--
-- 'loadBalancers', 'updateService_loadBalancers' - A list of Elastic Load Balancing load balancer objects. It contains the
-- load balancer name, the container name, and the container port to access
-- from the load balancer. The container name is as it appears in a
-- container definition.
--
-- When you add, update, or remove a load balancer configuration, Amazon
-- ECS starts new tasks with the updated Elastic Load Balancing
-- configuration, and then stops the old tasks when the new tasks are
-- running.
--
-- For services that use rolling updates, you can add, update, or remove
-- Elastic Load Balancing target groups. You can update from a single
-- target group to multiple target groups and from multiple target groups
-- to a single target group.
--
-- For services that use blue\/green deployments, you can update Elastic
-- Load Balancing target groups by using
-- @ @<https://docs.aws.amazon.com/codedeploy/latest/APIReference/API_CreateDeployment.html CreateDeployment>@ @
-- through CodeDeploy. Note that multiple target groups are not supported
-- for blue\/green deployments. For more information see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/register-multiple-targetgroups.html Register multiple target groups with a service>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- For services that use the external deployment controller, you can add,
-- update, or remove load balancers by using
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_CreateTaskSet.html CreateTaskSet>.
-- Note that multiple target groups are not supported for external
-- deployments. For more information see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/register-multiple-targetgroups.html Register multiple target groups with a service>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- You can remove existing @loadBalancers@ by passing an empty list.
--
-- 'networkConfiguration', 'updateService_networkConfiguration' - An object representing the network configuration for the service.
--
-- 'placementConstraints', 'updateService_placementConstraints' - An array of task placement constraint objects to update the service to
-- use. If no value is specified, the existing placement constraints for
-- the service will remain unchanged. If this value is specified, it will
-- override any existing placement constraints defined for the service. To
-- remove all existing placement constraints, specify an empty array.
--
-- You can specify a maximum of 10 constraints for each task. This limit
-- includes constraints in the task definition and those specified at
-- runtime.
--
-- 'placementStrategy', 'updateService_placementStrategy' - The task placement strategy objects to update the service to use. If no
-- value is specified, the existing placement strategy for the service will
-- remain unchanged. If this value is specified, it will override the
-- existing placement strategy defined for the service. To remove an
-- existing placement strategy, specify an empty object.
--
-- You can specify a maximum of five strategy rules for each service.
--
-- 'platformVersion', 'updateService_platformVersion' - The platform version that your tasks in the service run on. A platform
-- version is only specified for tasks using the Fargate launch type. If a
-- platform version is not specified, the @LATEST@ platform version is
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'propagateTags', 'updateService_propagateTags' - Determines whether to propagate the tags from the task definition or the
-- service to the task. If no value is specified, the tags aren\'t
-- propagated.
--
-- Only tasks launched after the update will reflect the update. To update
-- the tags on all tasks, set @forceNewDeployment@ to @true@, so that
-- Amazon ECS starts new tasks with the updated tags.
--
-- 'serviceConnectConfiguration', 'updateService_serviceConnectConfiguration' - The configuration for this service to discover and connect to services,
-- and be discovered by, and connected from, other services within a
-- namespace.
--
-- Tasks that run in a namespace can use short names to connect to services
-- in the namespace. Tasks can connect to services across all of the
-- clusters in the namespace. Tasks connect through a managed proxy
-- container that collects logs and metrics for increased visibility. Only
-- the tasks that Amazon ECS services create are supported with Service
-- Connect. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'serviceRegistries', 'updateService_serviceRegistries' - The details for the service discovery registries to assign to this
-- service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery>.
--
-- When you add, update, or remove the service registries configuration,
-- Amazon ECS starts new tasks with the updated service registries
-- configuration, and then stops the old tasks when the new tasks are
-- running.
--
-- You can remove existing @serviceRegistries@ by passing an empty list.
--
-- 'taskDefinition', 'updateService_taskDefinition' - The @family@ and @revision@ (@family:revision@) or full ARN of the task
-- definition to run in your service. If a @revision@ is not specified, the
-- latest @ACTIVE@ revision is used. If you modify the task definition with
-- @UpdateService@, Amazon ECS spawns a task with the new version of the
-- task definition and then stops an old task after the new version is
-- running.
--
-- 'service', 'updateService_service' - The name of the service to update.
newUpdateService ::
  -- | 'service'
  Prelude.Text ->
  UpdateService
newUpdateService pService_ =
  UpdateService'
    { capacityProviderStrategy =
        Prelude.Nothing,
      cluster = Prelude.Nothing,
      deploymentConfiguration = Prelude.Nothing,
      desiredCount = Prelude.Nothing,
      enableECSManagedTags = Prelude.Nothing,
      enableExecuteCommand = Prelude.Nothing,
      forceNewDeployment = Prelude.Nothing,
      healthCheckGracePeriodSeconds = Prelude.Nothing,
      loadBalancers = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
      placementConstraints = Prelude.Nothing,
      placementStrategy = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      propagateTags = Prelude.Nothing,
      serviceConnectConfiguration = Prelude.Nothing,
      serviceRegistries = Prelude.Nothing,
      taskDefinition = Prelude.Nothing,
      service = pService_
    }

-- | The capacity provider strategy to update the service to use.
--
-- if the service uses the default capacity provider strategy for the
-- cluster, the service can be updated to use one or more capacity
-- providers as opposed to the default capacity provider strategy. However,
-- when a service is using a capacity provider strategy that\'s not the
-- default capacity provider strategy, the service can\'t be updated to use
-- the cluster\'s default capacity provider strategy.
--
-- A capacity provider strategy consists of one or more capacity providers
-- along with the @base@ and @weight@ to assign to them. A capacity
-- provider must be associated with the cluster to be used in a capacity
-- provider strategy. The PutClusterCapacityProviders API is used to
-- associate a capacity provider with a cluster. Only capacity providers
-- with an @ACTIVE@ or @UPDATING@ status can be used.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the
-- capacity provider must already be created. New capacity providers can be
-- created with the CreateCapacityProvider API operation.
--
-- To use a Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The Fargate capacity providers are
-- available to all accounts and only need to be associated with a cluster
-- to be used.
--
-- The PutClusterCapacityProviders API operation is used to update the list
-- of available capacity providers for a cluster after the cluster is
-- created.
updateService_capacityProviderStrategy :: Lens.Lens' UpdateService (Prelude.Maybe [CapacityProviderStrategyItem])
updateService_capacityProviderStrategy = Lens.lens (\UpdateService' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@UpdateService' {} a -> s {capacityProviderStrategy = a} :: UpdateService) Prelude.. Lens.mapping Lens.coerced

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- your service runs on. If you do not specify a cluster, the default
-- cluster is assumed.
updateService_cluster :: Lens.Lens' UpdateService (Prelude.Maybe Prelude.Text)
updateService_cluster = Lens.lens (\UpdateService' {cluster} -> cluster) (\s@UpdateService' {} a -> s {cluster = a} :: UpdateService)

-- | Optional deployment parameters that control how many tasks run during
-- the deployment and the ordering of stopping and starting tasks.
updateService_deploymentConfiguration :: Lens.Lens' UpdateService (Prelude.Maybe DeploymentConfiguration)
updateService_deploymentConfiguration = Lens.lens (\UpdateService' {deploymentConfiguration} -> deploymentConfiguration) (\s@UpdateService' {} a -> s {deploymentConfiguration = a} :: UpdateService)

-- | The number of instantiations of the task to place and keep running in
-- your service.
updateService_desiredCount :: Lens.Lens' UpdateService (Prelude.Maybe Prelude.Int)
updateService_desiredCount = Lens.lens (\UpdateService' {desiredCount} -> desiredCount) (\s@UpdateService' {} a -> s {desiredCount = a} :: UpdateService)

-- | Determines whether to turn on Amazon ECS managed tags for the tasks in
-- the service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- Only tasks launched after the update will reflect the update. To update
-- the tags on all tasks, set @forceNewDeployment@ to @true@, so that
-- Amazon ECS starts new tasks with the updated tags.
updateService_enableECSManagedTags :: Lens.Lens' UpdateService (Prelude.Maybe Prelude.Bool)
updateService_enableECSManagedTags = Lens.lens (\UpdateService' {enableECSManagedTags} -> enableECSManagedTags) (\s@UpdateService' {} a -> s {enableECSManagedTags = a} :: UpdateService)

-- | If @true@, this enables execute command functionality on all task
-- containers.
--
-- If you do not want to override the value that was set when the service
-- was created, you can set this to @null@ when performing this action.
updateService_enableExecuteCommand :: Lens.Lens' UpdateService (Prelude.Maybe Prelude.Bool)
updateService_enableExecuteCommand = Lens.lens (\UpdateService' {enableExecuteCommand} -> enableExecuteCommand) (\s@UpdateService' {} a -> s {enableExecuteCommand = a} :: UpdateService)

-- | Determines whether to force a new deployment of the service. By default,
-- deployments aren\'t forced. You can use this option to start a new
-- deployment with no service definition changes. For example, you can
-- update a service\'s tasks to use a newer Docker image with the same
-- image\/tag combination (@my_image:latest@) or to roll Fargate tasks onto
-- a newer platform version.
updateService_forceNewDeployment :: Lens.Lens' UpdateService (Prelude.Maybe Prelude.Bool)
updateService_forceNewDeployment = Lens.lens (\UpdateService' {forceNewDeployment} -> forceNewDeployment) (\s@UpdateService' {} a -> s {forceNewDeployment = a} :: UpdateService)

-- | The period of time, in seconds, that the Amazon ECS service scheduler
-- ignores unhealthy Elastic Load Balancing target health checks after a
-- task has first started. This is only valid if your service is configured
-- to use a load balancer. If your service\'s tasks take a while to start
-- and respond to Elastic Load Balancing health checks, you can specify a
-- health check grace period of up to 2,147,483,647 seconds. During that
-- time, the Amazon ECS service scheduler ignores the Elastic Load
-- Balancing health check status. This grace period can prevent the ECS
-- service scheduler from marking tasks as unhealthy and stopping them
-- before they have time to come up.
updateService_healthCheckGracePeriodSeconds :: Lens.Lens' UpdateService (Prelude.Maybe Prelude.Int)
updateService_healthCheckGracePeriodSeconds = Lens.lens (\UpdateService' {healthCheckGracePeriodSeconds} -> healthCheckGracePeriodSeconds) (\s@UpdateService' {} a -> s {healthCheckGracePeriodSeconds = a} :: UpdateService)

-- | A list of Elastic Load Balancing load balancer objects. It contains the
-- load balancer name, the container name, and the container port to access
-- from the load balancer. The container name is as it appears in a
-- container definition.
--
-- When you add, update, or remove a load balancer configuration, Amazon
-- ECS starts new tasks with the updated Elastic Load Balancing
-- configuration, and then stops the old tasks when the new tasks are
-- running.
--
-- For services that use rolling updates, you can add, update, or remove
-- Elastic Load Balancing target groups. You can update from a single
-- target group to multiple target groups and from multiple target groups
-- to a single target group.
--
-- For services that use blue\/green deployments, you can update Elastic
-- Load Balancing target groups by using
-- @ @<https://docs.aws.amazon.com/codedeploy/latest/APIReference/API_CreateDeployment.html CreateDeployment>@ @
-- through CodeDeploy. Note that multiple target groups are not supported
-- for blue\/green deployments. For more information see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/register-multiple-targetgroups.html Register multiple target groups with a service>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- For services that use the external deployment controller, you can add,
-- update, or remove load balancers by using
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_CreateTaskSet.html CreateTaskSet>.
-- Note that multiple target groups are not supported for external
-- deployments. For more information see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/register-multiple-targetgroups.html Register multiple target groups with a service>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- You can remove existing @loadBalancers@ by passing an empty list.
updateService_loadBalancers :: Lens.Lens' UpdateService (Prelude.Maybe [LoadBalancer])
updateService_loadBalancers = Lens.lens (\UpdateService' {loadBalancers} -> loadBalancers) (\s@UpdateService' {} a -> s {loadBalancers = a} :: UpdateService) Prelude.. Lens.mapping Lens.coerced

-- | An object representing the network configuration for the service.
updateService_networkConfiguration :: Lens.Lens' UpdateService (Prelude.Maybe NetworkConfiguration)
updateService_networkConfiguration = Lens.lens (\UpdateService' {networkConfiguration} -> networkConfiguration) (\s@UpdateService' {} a -> s {networkConfiguration = a} :: UpdateService)

-- | An array of task placement constraint objects to update the service to
-- use. If no value is specified, the existing placement constraints for
-- the service will remain unchanged. If this value is specified, it will
-- override any existing placement constraints defined for the service. To
-- remove all existing placement constraints, specify an empty array.
--
-- You can specify a maximum of 10 constraints for each task. This limit
-- includes constraints in the task definition and those specified at
-- runtime.
updateService_placementConstraints :: Lens.Lens' UpdateService (Prelude.Maybe [PlacementConstraint])
updateService_placementConstraints = Lens.lens (\UpdateService' {placementConstraints} -> placementConstraints) (\s@UpdateService' {} a -> s {placementConstraints = a} :: UpdateService) Prelude.. Lens.mapping Lens.coerced

-- | The task placement strategy objects to update the service to use. If no
-- value is specified, the existing placement strategy for the service will
-- remain unchanged. If this value is specified, it will override the
-- existing placement strategy defined for the service. To remove an
-- existing placement strategy, specify an empty object.
--
-- You can specify a maximum of five strategy rules for each service.
updateService_placementStrategy :: Lens.Lens' UpdateService (Prelude.Maybe [PlacementStrategy])
updateService_placementStrategy = Lens.lens (\UpdateService' {placementStrategy} -> placementStrategy) (\s@UpdateService' {} a -> s {placementStrategy = a} :: UpdateService) Prelude.. Lens.mapping Lens.coerced

-- | The platform version that your tasks in the service run on. A platform
-- version is only specified for tasks using the Fargate launch type. If a
-- platform version is not specified, the @LATEST@ platform version is
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
updateService_platformVersion :: Lens.Lens' UpdateService (Prelude.Maybe Prelude.Text)
updateService_platformVersion = Lens.lens (\UpdateService' {platformVersion} -> platformVersion) (\s@UpdateService' {} a -> s {platformVersion = a} :: UpdateService)

-- | Determines whether to propagate the tags from the task definition or the
-- service to the task. If no value is specified, the tags aren\'t
-- propagated.
--
-- Only tasks launched after the update will reflect the update. To update
-- the tags on all tasks, set @forceNewDeployment@ to @true@, so that
-- Amazon ECS starts new tasks with the updated tags.
updateService_propagateTags :: Lens.Lens' UpdateService (Prelude.Maybe PropagateTags)
updateService_propagateTags = Lens.lens (\UpdateService' {propagateTags} -> propagateTags) (\s@UpdateService' {} a -> s {propagateTags = a} :: UpdateService)

-- | The configuration for this service to discover and connect to services,
-- and be discovered by, and connected from, other services within a
-- namespace.
--
-- Tasks that run in a namespace can use short names to connect to services
-- in the namespace. Tasks can connect to services across all of the
-- clusters in the namespace. Tasks connect through a managed proxy
-- container that collects logs and metrics for increased visibility. Only
-- the tasks that Amazon ECS services create are supported with Service
-- Connect. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
updateService_serviceConnectConfiguration :: Lens.Lens' UpdateService (Prelude.Maybe ServiceConnectConfiguration)
updateService_serviceConnectConfiguration = Lens.lens (\UpdateService' {serviceConnectConfiguration} -> serviceConnectConfiguration) (\s@UpdateService' {} a -> s {serviceConnectConfiguration = a} :: UpdateService)

-- | The details for the service discovery registries to assign to this
-- service. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery>.
--
-- When you add, update, or remove the service registries configuration,
-- Amazon ECS starts new tasks with the updated service registries
-- configuration, and then stops the old tasks when the new tasks are
-- running.
--
-- You can remove existing @serviceRegistries@ by passing an empty list.
updateService_serviceRegistries :: Lens.Lens' UpdateService (Prelude.Maybe [ServiceRegistry])
updateService_serviceRegistries = Lens.lens (\UpdateService' {serviceRegistries} -> serviceRegistries) (\s@UpdateService' {} a -> s {serviceRegistries = a} :: UpdateService) Prelude.. Lens.mapping Lens.coerced

-- | The @family@ and @revision@ (@family:revision@) or full ARN of the task
-- definition to run in your service. If a @revision@ is not specified, the
-- latest @ACTIVE@ revision is used. If you modify the task definition with
-- @UpdateService@, Amazon ECS spawns a task with the new version of the
-- task definition and then stops an old task after the new version is
-- running.
updateService_taskDefinition :: Lens.Lens' UpdateService (Prelude.Maybe Prelude.Text)
updateService_taskDefinition = Lens.lens (\UpdateService' {taskDefinition} -> taskDefinition) (\s@UpdateService' {} a -> s {taskDefinition = a} :: UpdateService)

-- | The name of the service to update.
updateService_service :: Lens.Lens' UpdateService Prelude.Text
updateService_service = Lens.lens (\UpdateService' {service} -> service) (\s@UpdateService' {} a -> s {service = a} :: UpdateService)

instance Core.AWSRequest UpdateService where
  type
    AWSResponse UpdateService =
      UpdateServiceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServiceResponse'
            Prelude.<$> (x Data..?> "service")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateService where
  hashWithSalt _salt UpdateService' {..} =
    _salt
      `Prelude.hashWithSalt` capacityProviderStrategy
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` deploymentConfiguration
      `Prelude.hashWithSalt` desiredCount
      `Prelude.hashWithSalt` enableECSManagedTags
      `Prelude.hashWithSalt` enableExecuteCommand
      `Prelude.hashWithSalt` forceNewDeployment
      `Prelude.hashWithSalt` healthCheckGracePeriodSeconds
      `Prelude.hashWithSalt` loadBalancers
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` placementConstraints
      `Prelude.hashWithSalt` placementStrategy
      `Prelude.hashWithSalt` platformVersion
      `Prelude.hashWithSalt` propagateTags
      `Prelude.hashWithSalt` serviceConnectConfiguration
      `Prelude.hashWithSalt` serviceRegistries
      `Prelude.hashWithSalt` taskDefinition
      `Prelude.hashWithSalt` service

instance Prelude.NFData UpdateService where
  rnf UpdateService' {..} =
    Prelude.rnf capacityProviderStrategy
      `Prelude.seq` Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf deploymentConfiguration
      `Prelude.seq` Prelude.rnf desiredCount
      `Prelude.seq` Prelude.rnf enableECSManagedTags
      `Prelude.seq` Prelude.rnf enableExecuteCommand
      `Prelude.seq` Prelude.rnf forceNewDeployment
      `Prelude.seq` Prelude.rnf healthCheckGracePeriodSeconds
      `Prelude.seq` Prelude.rnf loadBalancers
      `Prelude.seq` Prelude.rnf networkConfiguration
      `Prelude.seq` Prelude.rnf placementConstraints
      `Prelude.seq` Prelude.rnf placementStrategy
      `Prelude.seq` Prelude.rnf platformVersion
      `Prelude.seq` Prelude.rnf propagateTags
      `Prelude.seq` Prelude.rnf serviceConnectConfiguration
      `Prelude.seq` Prelude.rnf serviceRegistries
      `Prelude.seq` Prelude.rnf taskDefinition
      `Prelude.seq` Prelude.rnf service

instance Data.ToHeaders UpdateService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.UpdateService" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateService where
  toJSON UpdateService' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("capacityProviderStrategy" Data..=)
              Prelude.<$> capacityProviderStrategy,
            ("cluster" Data..=) Prelude.<$> cluster,
            ("deploymentConfiguration" Data..=)
              Prelude.<$> deploymentConfiguration,
            ("desiredCount" Data..=) Prelude.<$> desiredCount,
            ("enableECSManagedTags" Data..=)
              Prelude.<$> enableECSManagedTags,
            ("enableExecuteCommand" Data..=)
              Prelude.<$> enableExecuteCommand,
            ("forceNewDeployment" Data..=)
              Prelude.<$> forceNewDeployment,
            ("healthCheckGracePeriodSeconds" Data..=)
              Prelude.<$> healthCheckGracePeriodSeconds,
            ("loadBalancers" Data..=) Prelude.<$> loadBalancers,
            ("networkConfiguration" Data..=)
              Prelude.<$> networkConfiguration,
            ("placementConstraints" Data..=)
              Prelude.<$> placementConstraints,
            ("placementStrategy" Data..=)
              Prelude.<$> placementStrategy,
            ("platformVersion" Data..=)
              Prelude.<$> platformVersion,
            ("propagateTags" Data..=) Prelude.<$> propagateTags,
            ("serviceConnectConfiguration" Data..=)
              Prelude.<$> serviceConnectConfiguration,
            ("serviceRegistries" Data..=)
              Prelude.<$> serviceRegistries,
            ("taskDefinition" Data..=)
              Prelude.<$> taskDefinition,
            Prelude.Just ("service" Data..= service)
          ]
      )

instance Data.ToPath UpdateService where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateServiceResponse' smart constructor.
data UpdateServiceResponse = UpdateServiceResponse'
  { -- | The full description of your service following the update call.
    service :: Prelude.Maybe ContainerService,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'service', 'updateServiceResponse_service' - The full description of your service following the update call.
--
-- 'httpStatus', 'updateServiceResponse_httpStatus' - The response's http status code.
newUpdateServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateServiceResponse
newUpdateServiceResponse pHttpStatus_ =
  UpdateServiceResponse'
    { service = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of your service following the update call.
updateServiceResponse_service :: Lens.Lens' UpdateServiceResponse (Prelude.Maybe ContainerService)
updateServiceResponse_service = Lens.lens (\UpdateServiceResponse' {service} -> service) (\s@UpdateServiceResponse' {} a -> s {service = a} :: UpdateServiceResponse)

-- | The response's http status code.
updateServiceResponse_httpStatus :: Lens.Lens' UpdateServiceResponse Prelude.Int
updateServiceResponse_httpStatus = Lens.lens (\UpdateServiceResponse' {httpStatus} -> httpStatus) (\s@UpdateServiceResponse' {} a -> s {httpStatus = a} :: UpdateServiceResponse)

instance Prelude.NFData UpdateServiceResponse where
  rnf UpdateServiceResponse' {..} =
    Prelude.rnf service
      `Prelude.seq` Prelude.rnf httpStatus
