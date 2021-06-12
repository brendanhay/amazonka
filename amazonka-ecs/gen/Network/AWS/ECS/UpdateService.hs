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
-- Module      : Network.AWS.ECS.UpdateService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updating the task placement strategies and constraints on an Amazon ECS
-- service remains in preview and is a Beta Service as defined by and
-- subject to the Beta Service Participation Service Terms located at
-- <https://aws.amazon.com/service-terms> (\"Beta Terms\"). These Beta
-- Terms apply to your participation in this preview.
--
-- Modifies the parameters of a service.
--
-- For services using the rolling update (@ECS@) deployment controller, the
-- desired count, deployment configuration, network configuration, task
-- placement constraints and strategies, or task definition used can be
-- updated.
--
-- For services using the blue\/green (@CODE_DEPLOY@) deployment
-- controller, only the desired count, deployment configuration, task
-- placement constraints and strategies, and health check grace period can
-- be updated using this API. If the network configuration, platform
-- version, or task definition need to be updated, a new AWS CodeDeploy
-- deployment should be created. For more information, see
-- <https://docs.aws.amazon.com/codedeploy/latest/APIReference/API_CreateDeployment.html CreateDeployment>
-- in the /AWS CodeDeploy API Reference/.
--
-- For services using an external deployment controller, you can update
-- only the desired count, task placement constraints and strategies, and
-- health check grace period using this API. If the launch type, load
-- balancer, network configuration, platform version, or task definition
-- need to be updated, you should create a new task set. For more
-- information, see CreateTaskSet.
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
-- @my_image:latest@), you do not need to create a new revision of your
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
--     services that do not use a load balancer are considered healthy if
--     they are in the @RUNNING@ state. Tasks for services that use a load
--     balancer are considered healthy if they are in the @RUNNING@ state
--     and the container instance they are hosted on is reported as healthy
--     by the load balancer.
--
-- -   The @maximumPercent@ parameter represents an upper limit on the
--     number of running tasks during a deployment, which enables you to
--     define the deployment batch size. For example, if @desiredCount@ is
--     four tasks, a maximum of 200% starts four new tasks before stopping
--     the four older tasks (provided that the cluster resources required
--     to do this are available).
--
-- When UpdateService stops a task during a deployment, the equivalent of
-- @docker stop@ is issued to the containers running in the task. This
-- results in a @SIGTERM@ and a 30-second timeout, after which @SIGKILL@ is
-- sent and the containers are forcibly stopped. If the container handles
-- the @SIGTERM@ gracefully and exits within 30 seconds from receiving it,
-- no @SIGKILL@ is sent.
--
-- When the service scheduler launches new tasks, it determines task
-- placement in your cluster with the following logic:
--
-- -   Determine which of the container instances in your cluster can
--     support your service\'s task definition (for example, they have the
--     required CPU, memory, ports, and container instance attributes).
--
-- -   By default, the service scheduler attempts to balance tasks across
--     Availability Zones in this manner (although you can choose a
--     different placement strategy):
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
module Network.AWS.ECS.UpdateService
  ( -- * Creating a Request
    UpdateService (..),
    newUpdateService,

    -- * Request Lenses
    updateService_deploymentConfiguration,
    updateService_networkConfiguration,
    updateService_capacityProviderStrategy,
    updateService_desiredCount,
    updateService_platformVersion,
    updateService_placementStrategy,
    updateService_placementConstraints,
    updateService_healthCheckGracePeriodSeconds,
    updateService_forceNewDeployment,
    updateService_taskDefinition,
    updateService_cluster,
    updateService_service,

    -- * Destructuring the Response
    UpdateServiceResponse (..),
    newUpdateServiceResponse,

    -- * Response Lenses
    updateServiceResponse_service,
    updateServiceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateService' smart constructor.
data UpdateService = UpdateService'
  { -- | Optional deployment parameters that control how many tasks run during
    -- the deployment and the ordering of stopping and starting tasks.
    deploymentConfiguration :: Core.Maybe DeploymentConfiguration,
    networkConfiguration :: Core.Maybe NetworkConfiguration,
    -- | The capacity provider strategy to update the service to use.
    --
    -- If the service is using the default capacity provider strategy for the
    -- cluster, the service can be updated to use one or more capacity
    -- providers as opposed to the default capacity provider strategy. However,
    -- when a service is using a capacity provider strategy that is not the
    -- default capacity provider strategy, the service cannot be updated to use
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
    -- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or
    -- @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers
    -- are available to all accounts and only need to be associated with a
    -- cluster to be used.
    --
    -- The PutClusterCapacityProviders API operation is used to update the list
    -- of available capacity providers for a cluster after the cluster is
    -- created.
    capacityProviderStrategy :: Core.Maybe [CapacityProviderStrategyItem],
    -- | The number of instantiations of the task to place and keep running in
    -- your service.
    desiredCount :: Core.Maybe Core.Int,
    -- | The platform version on which your tasks in the service are running. A
    -- platform version is only specified for tasks using the Fargate launch
    -- type. If a platform version is not specified, the @LATEST@ platform
    -- version is used by default. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    platformVersion :: Core.Maybe Core.Text,
    -- | The task placement strategy objects to update the service to use. If no
    -- value is specified, the existing placement strategy for the service will
    -- remain unchanged. If this value is specified, it will override the
    -- existing placement strategy defined for the service. To remove an
    -- existing placement strategy, specify an empty object.
    --
    -- You can specify a maximum of five strategy rules per service.
    placementStrategy :: Core.Maybe [PlacementStrategy],
    -- | An array of task placement constraint objects to update the service to
    -- use. If no value is specified, the existing placement constraints for
    -- the service will remain unchanged. If this value is specified, it will
    -- override any existing placement constraints defined for the service. To
    -- remove all existing placement constraints, specify an empty array.
    --
    -- You can specify a maximum of 10 constraints per task (this limit
    -- includes constraints in the task definition and those specified at
    -- runtime).
    placementConstraints :: Core.Maybe [PlacementConstraint],
    -- | The period of time, in seconds, that the Amazon ECS service scheduler
    -- should ignore unhealthy Elastic Load Balancing target health checks
    -- after a task has first started. This is only valid if your service is
    -- configured to use a load balancer. If your service\'s tasks take a while
    -- to start and respond to Elastic Load Balancing health checks, you can
    -- specify a health check grace period of up to 2,147,483,647 seconds.
    -- During that time, the Amazon ECS service scheduler ignores the Elastic
    -- Load Balancing health check status. This grace period can prevent the
    -- ECS service scheduler from marking tasks as unhealthy and stopping them
    -- before they have time to come up.
    healthCheckGracePeriodSeconds :: Core.Maybe Core.Int,
    -- | Whether to force a new deployment of the service. Deployments are not
    -- forced by default. You can use this option to trigger a new deployment
    -- with no service definition changes. For example, you can update a
    -- service\'s tasks to use a newer Docker image with the same image\/tag
    -- combination (@my_image:latest@) or to roll Fargate tasks onto a newer
    -- platform version.
    forceNewDeployment :: Core.Maybe Core.Bool,
    -- | The @family@ and @revision@ (@family:revision@) or full ARN of the task
    -- definition to run in your service. If a @revision@ is not specified, the
    -- latest @ACTIVE@ revision is used. If you modify the task definition with
    -- @UpdateService@, Amazon ECS spawns a task with the new version of the
    -- task definition and then stops an old task after the new version is
    -- running.
    taskDefinition :: Core.Maybe Core.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- your service is running on. If you do not specify a cluster, the default
    -- cluster is assumed.
    cluster :: Core.Maybe Core.Text,
    -- | The name of the service to update.
    service :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentConfiguration', 'updateService_deploymentConfiguration' - Optional deployment parameters that control how many tasks run during
-- the deployment and the ordering of stopping and starting tasks.
--
-- 'networkConfiguration', 'updateService_networkConfiguration' - Undocumented member.
--
-- 'capacityProviderStrategy', 'updateService_capacityProviderStrategy' - The capacity provider strategy to update the service to use.
--
-- If the service is using the default capacity provider strategy for the
-- cluster, the service can be updated to use one or more capacity
-- providers as opposed to the default capacity provider strategy. However,
-- when a service is using a capacity provider strategy that is not the
-- default capacity provider strategy, the service cannot be updated to use
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
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers
-- are available to all accounts and only need to be associated with a
-- cluster to be used.
--
-- The PutClusterCapacityProviders API operation is used to update the list
-- of available capacity providers for a cluster after the cluster is
-- created.
--
-- 'desiredCount', 'updateService_desiredCount' - The number of instantiations of the task to place and keep running in
-- your service.
--
-- 'platformVersion', 'updateService_platformVersion' - The platform version on which your tasks in the service are running. A
-- platform version is only specified for tasks using the Fargate launch
-- type. If a platform version is not specified, the @LATEST@ platform
-- version is used by default. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'placementStrategy', 'updateService_placementStrategy' - The task placement strategy objects to update the service to use. If no
-- value is specified, the existing placement strategy for the service will
-- remain unchanged. If this value is specified, it will override the
-- existing placement strategy defined for the service. To remove an
-- existing placement strategy, specify an empty object.
--
-- You can specify a maximum of five strategy rules per service.
--
-- 'placementConstraints', 'updateService_placementConstraints' - An array of task placement constraint objects to update the service to
-- use. If no value is specified, the existing placement constraints for
-- the service will remain unchanged. If this value is specified, it will
-- override any existing placement constraints defined for the service. To
-- remove all existing placement constraints, specify an empty array.
--
-- You can specify a maximum of 10 constraints per task (this limit
-- includes constraints in the task definition and those specified at
-- runtime).
--
-- 'healthCheckGracePeriodSeconds', 'updateService_healthCheckGracePeriodSeconds' - The period of time, in seconds, that the Amazon ECS service scheduler
-- should ignore unhealthy Elastic Load Balancing target health checks
-- after a task has first started. This is only valid if your service is
-- configured to use a load balancer. If your service\'s tasks take a while
-- to start and respond to Elastic Load Balancing health checks, you can
-- specify a health check grace period of up to 2,147,483,647 seconds.
-- During that time, the Amazon ECS service scheduler ignores the Elastic
-- Load Balancing health check status. This grace period can prevent the
-- ECS service scheduler from marking tasks as unhealthy and stopping them
-- before they have time to come up.
--
-- 'forceNewDeployment', 'updateService_forceNewDeployment' - Whether to force a new deployment of the service. Deployments are not
-- forced by default. You can use this option to trigger a new deployment
-- with no service definition changes. For example, you can update a
-- service\'s tasks to use a newer Docker image with the same image\/tag
-- combination (@my_image:latest@) or to roll Fargate tasks onto a newer
-- platform version.
--
-- 'taskDefinition', 'updateService_taskDefinition' - The @family@ and @revision@ (@family:revision@) or full ARN of the task
-- definition to run in your service. If a @revision@ is not specified, the
-- latest @ACTIVE@ revision is used. If you modify the task definition with
-- @UpdateService@, Amazon ECS spawns a task with the new version of the
-- task definition and then stops an old task after the new version is
-- running.
--
-- 'cluster', 'updateService_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- your service is running on. If you do not specify a cluster, the default
-- cluster is assumed.
--
-- 'service', 'updateService_service' - The name of the service to update.
newUpdateService ::
  -- | 'service'
  Core.Text ->
  UpdateService
newUpdateService pService_ =
  UpdateService'
    { deploymentConfiguration =
        Core.Nothing,
      networkConfiguration = Core.Nothing,
      capacityProviderStrategy = Core.Nothing,
      desiredCount = Core.Nothing,
      platformVersion = Core.Nothing,
      placementStrategy = Core.Nothing,
      placementConstraints = Core.Nothing,
      healthCheckGracePeriodSeconds = Core.Nothing,
      forceNewDeployment = Core.Nothing,
      taskDefinition = Core.Nothing,
      cluster = Core.Nothing,
      service = pService_
    }

-- | Optional deployment parameters that control how many tasks run during
-- the deployment and the ordering of stopping and starting tasks.
updateService_deploymentConfiguration :: Lens.Lens' UpdateService (Core.Maybe DeploymentConfiguration)
updateService_deploymentConfiguration = Lens.lens (\UpdateService' {deploymentConfiguration} -> deploymentConfiguration) (\s@UpdateService' {} a -> s {deploymentConfiguration = a} :: UpdateService)

-- | Undocumented member.
updateService_networkConfiguration :: Lens.Lens' UpdateService (Core.Maybe NetworkConfiguration)
updateService_networkConfiguration = Lens.lens (\UpdateService' {networkConfiguration} -> networkConfiguration) (\s@UpdateService' {} a -> s {networkConfiguration = a} :: UpdateService)

-- | The capacity provider strategy to update the service to use.
--
-- If the service is using the default capacity provider strategy for the
-- cluster, the service can be updated to use one or more capacity
-- providers as opposed to the default capacity provider strategy. However,
-- when a service is using a capacity provider strategy that is not the
-- default capacity provider strategy, the service cannot be updated to use
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
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers
-- are available to all accounts and only need to be associated with a
-- cluster to be used.
--
-- The PutClusterCapacityProviders API operation is used to update the list
-- of available capacity providers for a cluster after the cluster is
-- created.
updateService_capacityProviderStrategy :: Lens.Lens' UpdateService (Core.Maybe [CapacityProviderStrategyItem])
updateService_capacityProviderStrategy = Lens.lens (\UpdateService' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@UpdateService' {} a -> s {capacityProviderStrategy = a} :: UpdateService) Core.. Lens.mapping Lens._Coerce

-- | The number of instantiations of the task to place and keep running in
-- your service.
updateService_desiredCount :: Lens.Lens' UpdateService (Core.Maybe Core.Int)
updateService_desiredCount = Lens.lens (\UpdateService' {desiredCount} -> desiredCount) (\s@UpdateService' {} a -> s {desiredCount = a} :: UpdateService)

-- | The platform version on which your tasks in the service are running. A
-- platform version is only specified for tasks using the Fargate launch
-- type. If a platform version is not specified, the @LATEST@ platform
-- version is used by default. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
updateService_platformVersion :: Lens.Lens' UpdateService (Core.Maybe Core.Text)
updateService_platformVersion = Lens.lens (\UpdateService' {platformVersion} -> platformVersion) (\s@UpdateService' {} a -> s {platformVersion = a} :: UpdateService)

-- | The task placement strategy objects to update the service to use. If no
-- value is specified, the existing placement strategy for the service will
-- remain unchanged. If this value is specified, it will override the
-- existing placement strategy defined for the service. To remove an
-- existing placement strategy, specify an empty object.
--
-- You can specify a maximum of five strategy rules per service.
updateService_placementStrategy :: Lens.Lens' UpdateService (Core.Maybe [PlacementStrategy])
updateService_placementStrategy = Lens.lens (\UpdateService' {placementStrategy} -> placementStrategy) (\s@UpdateService' {} a -> s {placementStrategy = a} :: UpdateService) Core.. Lens.mapping Lens._Coerce

-- | An array of task placement constraint objects to update the service to
-- use. If no value is specified, the existing placement constraints for
-- the service will remain unchanged. If this value is specified, it will
-- override any existing placement constraints defined for the service. To
-- remove all existing placement constraints, specify an empty array.
--
-- You can specify a maximum of 10 constraints per task (this limit
-- includes constraints in the task definition and those specified at
-- runtime).
updateService_placementConstraints :: Lens.Lens' UpdateService (Core.Maybe [PlacementConstraint])
updateService_placementConstraints = Lens.lens (\UpdateService' {placementConstraints} -> placementConstraints) (\s@UpdateService' {} a -> s {placementConstraints = a} :: UpdateService) Core.. Lens.mapping Lens._Coerce

-- | The period of time, in seconds, that the Amazon ECS service scheduler
-- should ignore unhealthy Elastic Load Balancing target health checks
-- after a task has first started. This is only valid if your service is
-- configured to use a load balancer. If your service\'s tasks take a while
-- to start and respond to Elastic Load Balancing health checks, you can
-- specify a health check grace period of up to 2,147,483,647 seconds.
-- During that time, the Amazon ECS service scheduler ignores the Elastic
-- Load Balancing health check status. This grace period can prevent the
-- ECS service scheduler from marking tasks as unhealthy and stopping them
-- before they have time to come up.
updateService_healthCheckGracePeriodSeconds :: Lens.Lens' UpdateService (Core.Maybe Core.Int)
updateService_healthCheckGracePeriodSeconds = Lens.lens (\UpdateService' {healthCheckGracePeriodSeconds} -> healthCheckGracePeriodSeconds) (\s@UpdateService' {} a -> s {healthCheckGracePeriodSeconds = a} :: UpdateService)

-- | Whether to force a new deployment of the service. Deployments are not
-- forced by default. You can use this option to trigger a new deployment
-- with no service definition changes. For example, you can update a
-- service\'s tasks to use a newer Docker image with the same image\/tag
-- combination (@my_image:latest@) or to roll Fargate tasks onto a newer
-- platform version.
updateService_forceNewDeployment :: Lens.Lens' UpdateService (Core.Maybe Core.Bool)
updateService_forceNewDeployment = Lens.lens (\UpdateService' {forceNewDeployment} -> forceNewDeployment) (\s@UpdateService' {} a -> s {forceNewDeployment = a} :: UpdateService)

-- | The @family@ and @revision@ (@family:revision@) or full ARN of the task
-- definition to run in your service. If a @revision@ is not specified, the
-- latest @ACTIVE@ revision is used. If you modify the task definition with
-- @UpdateService@, Amazon ECS spawns a task with the new version of the
-- task definition and then stops an old task after the new version is
-- running.
updateService_taskDefinition :: Lens.Lens' UpdateService (Core.Maybe Core.Text)
updateService_taskDefinition = Lens.lens (\UpdateService' {taskDefinition} -> taskDefinition) (\s@UpdateService' {} a -> s {taskDefinition = a} :: UpdateService)

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- your service is running on. If you do not specify a cluster, the default
-- cluster is assumed.
updateService_cluster :: Lens.Lens' UpdateService (Core.Maybe Core.Text)
updateService_cluster = Lens.lens (\UpdateService' {cluster} -> cluster) (\s@UpdateService' {} a -> s {cluster = a} :: UpdateService)

-- | The name of the service to update.
updateService_service :: Lens.Lens' UpdateService Core.Text
updateService_service = Lens.lens (\UpdateService' {service} -> service) (\s@UpdateService' {} a -> s {service = a} :: UpdateService)

instance Core.AWSRequest UpdateService where
  type
    AWSResponse UpdateService =
      UpdateServiceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServiceResponse'
            Core.<$> (x Core..?> "service")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateService

instance Core.NFData UpdateService

instance Core.ToHeaders UpdateService where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.UpdateService" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateService where
  toJSON UpdateService' {..} =
    Core.object
      ( Core.catMaybes
          [ ("deploymentConfiguration" Core..=)
              Core.<$> deploymentConfiguration,
            ("networkConfiguration" Core..=)
              Core.<$> networkConfiguration,
            ("capacityProviderStrategy" Core..=)
              Core.<$> capacityProviderStrategy,
            ("desiredCount" Core..=) Core.<$> desiredCount,
            ("platformVersion" Core..=) Core.<$> platformVersion,
            ("placementStrategy" Core..=)
              Core.<$> placementStrategy,
            ("placementConstraints" Core..=)
              Core.<$> placementConstraints,
            ("healthCheckGracePeriodSeconds" Core..=)
              Core.<$> healthCheckGracePeriodSeconds,
            ("forceNewDeployment" Core..=)
              Core.<$> forceNewDeployment,
            ("taskDefinition" Core..=) Core.<$> taskDefinition,
            ("cluster" Core..=) Core.<$> cluster,
            Core.Just ("service" Core..= service)
          ]
      )

instance Core.ToPath UpdateService where
  toPath = Core.const "/"

instance Core.ToQuery UpdateService where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateServiceResponse' smart constructor.
data UpdateServiceResponse = UpdateServiceResponse'
  { -- | The full description of your service following the update call.
    service :: Core.Maybe ContainerService,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateServiceResponse
newUpdateServiceResponse pHttpStatus_ =
  UpdateServiceResponse'
    { service = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of your service following the update call.
updateServiceResponse_service :: Lens.Lens' UpdateServiceResponse (Core.Maybe ContainerService)
updateServiceResponse_service = Lens.lens (\UpdateServiceResponse' {service} -> service) (\s@UpdateServiceResponse' {} a -> s {service = a} :: UpdateServiceResponse)

-- | The response's http status code.
updateServiceResponse_httpStatus :: Lens.Lens' UpdateServiceResponse Core.Int
updateServiceResponse_httpStatus = Lens.lens (\UpdateServiceResponse' {httpStatus} -> httpStatus) (\s@UpdateServiceResponse' {} a -> s {httpStatus = a} :: UpdateServiceResponse)

instance Core.NFData UpdateServiceResponse
