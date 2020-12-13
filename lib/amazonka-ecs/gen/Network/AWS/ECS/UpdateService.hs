{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.UpdateService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- /Important:/ Updating the task placement strategies and constraints on an Amazon ECS service remains in preview and is a Beta Service as defined by and subject to the Beta Service Participation Service Terms located at <https://aws.amazon.com/service-terms https://aws.amazon.com/service-terms> ("Beta Terms"). These Beta Terms apply to your participation in this preview.
--
-- Modifies the parameters of a service.
-- For services using the rolling update (@ECS@ ) deployment controller, the desired count, deployment configuration, network configuration, task placement constraints and strategies, or task definition used can be updated.
-- For services using the blue/green (@CODE_DEPLOY@ ) deployment controller, only the desired count, deployment configuration, task placement constraints and strategies, and health check grace period can be updated using this API. If the network configuration, platform version, or task definition need to be updated, a new AWS CodeDeploy deployment should be created. For more information, see <https://docs.aws.amazon.com/codedeploy/latest/APIReference/API_CreateDeployment.html CreateDeployment> in the /AWS CodeDeploy API Reference/ .
-- For services using an external deployment controller, you can update only the desired count, task placement constraints and strategies, and health check grace period using this API. If the launch type, load balancer, network configuration, platform version, or task definition need to be updated, you should create a new task set. For more information, see 'CreateTaskSet' .
-- You can add to or subtract from the number of instantiations of a task definition in a service by specifying the cluster that the service is running in and a new @desiredCount@ parameter.
-- If you have updated the Docker image of your application, you can create a new task definition with that image and deploy it to your service. The service scheduler uses the minimum healthy percent and maximum percent parameters (in the service's deployment configuration) to determine the deployment strategy.
-- You can also update the deployment configuration of a service. When a deployment is triggered by updating the task definition of a service, the service scheduler uses the deployment configuration parameters, @minimumHealthyPercent@ and @maximumPercent@ , to determine the deployment strategy.
--
--     * If @minimumHealthyPercent@ is below 100%, the scheduler can ignore @desiredCount@ temporarily during a deployment. For example, if @desiredCount@ is four tasks, a minimum of 50% allows the scheduler to stop two existing tasks before starting two new tasks. Tasks for services that do not use a load balancer are considered healthy if they are in the @RUNNING@ state. Tasks for services that use a load balancer are considered healthy if they are in the @RUNNING@ state and the container instance they are hosted on is reported as healthy by the load balancer.
--
--
--     * The @maximumPercent@ parameter represents an upper limit on the number of running tasks during a deployment, which enables you to define the deployment batch size. For example, if @desiredCount@ is four tasks, a maximum of 200% starts four new tasks before stopping the four older tasks (provided that the cluster resources required to do this are available).
--
--
-- When 'UpdateService' stops a task during a deployment, the equivalent of @docker stop@ is issued to the containers running in the task. This results in a @SIGTERM@ and a 30-second timeout, after which @SIGKILL@ is sent and the containers are forcibly stopped. If the container handles the @SIGTERM@ gracefully and exits within 30 seconds from receiving it, no @SIGKILL@ is sent.
-- When the service scheduler launches new tasks, it determines task placement in your cluster with the following logic:
--
--     * Determine which of the container instances in your cluster can support your service's task definition (for example, they have the required CPU, memory, ports, and container instance attributes).
--
--
--     * By default, the service scheduler attempts to balance tasks across Availability Zones in this manner (although you can choose a different placement strategy):
--
--     * Sort the valid container instances by the fewest number of running tasks for this service in the same Availability Zone as the instance. For example, if zone A has one running service task and zones B and C each have zero, valid container instances in either zone B or C are considered optimal for placement.
--
--
--     * Place the new service task on a valid container instance in an optimal Availability Zone (based on the previous steps), favoring container instances with the fewest number of running tasks for this service.
--
--
--
--
-- When the service scheduler stops running tasks, it attempts to maintain balance across the Availability Zones in your cluster using the following logic:
--
--     * Sort the container instances by the largest number of running tasks for this service in the same Availability Zone as the instance. For example, if zone A has one running service task and zones B and C each have two, container instances in either zone B or C are considered optimal for termination.
--
--
--     * Stop the task on a container instance in an optimal Availability Zone (based on the previous steps), favoring container instances with the largest number of running tasks for this service.
module Network.AWS.ECS.UpdateService
  ( -- * Creating a request
    UpdateService (..),
    mkUpdateService,

    -- ** Request lenses
    usCluster,
    usService,
    usPlatformVersion,
    usDesiredCount,
    usPlacementConstraints,
    usPlacementStrategy,
    usForceNewDeployment,
    usTaskDefinition,
    usHealthCheckGracePeriodSeconds,
    usNetworkConfiguration,
    usCapacityProviderStrategy,
    usDeploymentConfiguration,

    -- * Destructuring the response
    UpdateServiceResponse (..),
    mkUpdateServiceResponse,

    -- ** Response lenses
    usrsService,
    usrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateService' smart constructor.
data UpdateService = UpdateService'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that your service is running on. If you do not specify a cluster, the default cluster is assumed.
    cluster :: Lude.Maybe Lude.Text,
    -- | The name of the service to update.
    service :: Lude.Text,
    -- | The platform version on which your tasks in the service are running. A platform version is only specified for tasks using the Fargate launch type. If a platform version is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
    platformVersion :: Lude.Maybe Lude.Text,
    -- | The number of instantiations of the task to place and keep running in your service.
    desiredCount :: Lude.Maybe Lude.Int,
    -- | An array of task placement constraint objects to update the service to use. If no value is specified, the existing placement constraints for the service will remain unchanged. If this value is specified, it will override any existing placement constraints defined for the service. To remove all existing placement constraints, specify an empty array.
    --
    -- You can specify a maximum of 10 constraints per task (this limit includes constraints in the task definition and those specified at runtime).
    placementConstraints :: Lude.Maybe [PlacementConstraint],
    -- | The task placement strategy objects to update the service to use. If no value is specified, the existing placement strategy for the service will remain unchanged. If this value is specified, it will override the existing placement strategy defined for the service. To remove an existing placement strategy, specify an empty object.
    --
    -- You can specify a maximum of five strategy rules per service.
    placementStrategy :: Lude.Maybe [PlacementStrategy],
    -- | Whether to force a new deployment of the service. Deployments are not forced by default. You can use this option to trigger a new deployment with no service definition changes. For example, you can update a service's tasks to use a newer Docker image with the same image/tag combination (@my_image:latest@ ) or to roll Fargate tasks onto a newer platform version.
    forceNewDeployment :: Lude.Maybe Lude.Bool,
    -- | The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to run in your service. If a @revision@ is not specified, the latest @ACTIVE@ revision is used. If you modify the task definition with @UpdateService@ , Amazon ECS spawns a task with the new version of the task definition and then stops an old task after the new version is running.
    taskDefinition :: Lude.Maybe Lude.Text,
    -- | The period of time, in seconds, that the Amazon ECS service scheduler should ignore unhealthy Elastic Load Balancing target health checks after a task has first started. This is only valid if your service is configured to use a load balancer. If your service's tasks take a while to start and respond to Elastic Load Balancing health checks, you can specify a health check grace period of up to 2,147,483,647 seconds. During that time, the Amazon ECS service scheduler ignores the Elastic Load Balancing health check status. This grace period can prevent the ECS service scheduler from marking tasks as unhealthy and stopping them before they have time to come up.
    healthCheckGracePeriodSeconds :: Lude.Maybe Lude.Int,
    networkConfiguration :: Lude.Maybe NetworkConfiguration,
    -- | The capacity provider strategy to update the service to use.
    --
    -- If the service is using the default capacity provider strategy for the cluster, the service can be updated to use one or more capacity providers as opposed to the default capacity provider strategy. However, when a service is using a capacity provider strategy that is not the default capacity provider strategy, the service cannot be updated to use the cluster's default capacity provider strategy.
    -- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
    -- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
    -- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
    -- The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
    capacityProviderStrategy :: Lude.Maybe [CapacityProviderStrategyItem],
    -- | Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
    deploymentConfiguration :: Lude.Maybe DeploymentConfiguration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateService' with the minimum fields required to make a request.
--
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that your service is running on. If you do not specify a cluster, the default cluster is assumed.
-- * 'service' - The name of the service to update.
-- * 'platformVersion' - The platform version on which your tasks in the service are running. A platform version is only specified for tasks using the Fargate launch type. If a platform version is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'desiredCount' - The number of instantiations of the task to place and keep running in your service.
-- * 'placementConstraints' - An array of task placement constraint objects to update the service to use. If no value is specified, the existing placement constraints for the service will remain unchanged. If this value is specified, it will override any existing placement constraints defined for the service. To remove all existing placement constraints, specify an empty array.
--
-- You can specify a maximum of 10 constraints per task (this limit includes constraints in the task definition and those specified at runtime).
-- * 'placementStrategy' - The task placement strategy objects to update the service to use. If no value is specified, the existing placement strategy for the service will remain unchanged. If this value is specified, it will override the existing placement strategy defined for the service. To remove an existing placement strategy, specify an empty object.
--
-- You can specify a maximum of five strategy rules per service.
-- * 'forceNewDeployment' - Whether to force a new deployment of the service. Deployments are not forced by default. You can use this option to trigger a new deployment with no service definition changes. For example, you can update a service's tasks to use a newer Docker image with the same image/tag combination (@my_image:latest@ ) or to roll Fargate tasks onto a newer platform version.
-- * 'taskDefinition' - The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to run in your service. If a @revision@ is not specified, the latest @ACTIVE@ revision is used. If you modify the task definition with @UpdateService@ , Amazon ECS spawns a task with the new version of the task definition and then stops an old task after the new version is running.
-- * 'healthCheckGracePeriodSeconds' - The period of time, in seconds, that the Amazon ECS service scheduler should ignore unhealthy Elastic Load Balancing target health checks after a task has first started. This is only valid if your service is configured to use a load balancer. If your service's tasks take a while to start and respond to Elastic Load Balancing health checks, you can specify a health check grace period of up to 2,147,483,647 seconds. During that time, the Amazon ECS service scheduler ignores the Elastic Load Balancing health check status. This grace period can prevent the ECS service scheduler from marking tasks as unhealthy and stopping them before they have time to come up.
-- * 'networkConfiguration' -
-- * 'capacityProviderStrategy' - The capacity provider strategy to update the service to use.
--
-- If the service is using the default capacity provider strategy for the cluster, the service can be updated to use one or more capacity providers as opposed to the default capacity provider strategy. However, when a service is using a capacity provider strategy that is not the default capacity provider strategy, the service cannot be updated to use the cluster's default capacity provider strategy.
-- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
--
-- * 'deploymentConfiguration' - Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
mkUpdateService ::
  -- | 'service'
  Lude.Text ->
  UpdateService
mkUpdateService pService_ =
  UpdateService'
    { cluster = Lude.Nothing,
      service = pService_,
      platformVersion = Lude.Nothing,
      desiredCount = Lude.Nothing,
      placementConstraints = Lude.Nothing,
      placementStrategy = Lude.Nothing,
      forceNewDeployment = Lude.Nothing,
      taskDefinition = Lude.Nothing,
      healthCheckGracePeriodSeconds = Lude.Nothing,
      networkConfiguration = Lude.Nothing,
      capacityProviderStrategy = Lude.Nothing,
      deploymentConfiguration = Lude.Nothing
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that your service is running on. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usCluster :: Lens.Lens' UpdateService (Lude.Maybe Lude.Text)
usCluster = Lens.lens (cluster :: UpdateService -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: UpdateService)
{-# DEPRECATED usCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The name of the service to update.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usService :: Lens.Lens' UpdateService Lude.Text
usService = Lens.lens (service :: UpdateService -> Lude.Text) (\s a -> s {service = a} :: UpdateService)
{-# DEPRECATED usService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The platform version on which your tasks in the service are running. A platform version is only specified for tasks using the Fargate launch type. If a platform version is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usPlatformVersion :: Lens.Lens' UpdateService (Lude.Maybe Lude.Text)
usPlatformVersion = Lens.lens (platformVersion :: UpdateService -> Lude.Maybe Lude.Text) (\s a -> s {platformVersion = a} :: UpdateService)
{-# DEPRECATED usPlatformVersion "Use generic-lens or generic-optics with 'platformVersion' instead." #-}

-- | The number of instantiations of the task to place and keep running in your service.
--
-- /Note:/ Consider using 'desiredCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDesiredCount :: Lens.Lens' UpdateService (Lude.Maybe Lude.Int)
usDesiredCount = Lens.lens (desiredCount :: UpdateService -> Lude.Maybe Lude.Int) (\s a -> s {desiredCount = a} :: UpdateService)
{-# DEPRECATED usDesiredCount "Use generic-lens or generic-optics with 'desiredCount' instead." #-}

-- | An array of task placement constraint objects to update the service to use. If no value is specified, the existing placement constraints for the service will remain unchanged. If this value is specified, it will override any existing placement constraints defined for the service. To remove all existing placement constraints, specify an empty array.
--
-- You can specify a maximum of 10 constraints per task (this limit includes constraints in the task definition and those specified at runtime).
--
-- /Note:/ Consider using 'placementConstraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usPlacementConstraints :: Lens.Lens' UpdateService (Lude.Maybe [PlacementConstraint])
usPlacementConstraints = Lens.lens (placementConstraints :: UpdateService -> Lude.Maybe [PlacementConstraint]) (\s a -> s {placementConstraints = a} :: UpdateService)
{-# DEPRECATED usPlacementConstraints "Use generic-lens or generic-optics with 'placementConstraints' instead." #-}

-- | The task placement strategy objects to update the service to use. If no value is specified, the existing placement strategy for the service will remain unchanged. If this value is specified, it will override the existing placement strategy defined for the service. To remove an existing placement strategy, specify an empty object.
--
-- You can specify a maximum of five strategy rules per service.
--
-- /Note:/ Consider using 'placementStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usPlacementStrategy :: Lens.Lens' UpdateService (Lude.Maybe [PlacementStrategy])
usPlacementStrategy = Lens.lens (placementStrategy :: UpdateService -> Lude.Maybe [PlacementStrategy]) (\s a -> s {placementStrategy = a} :: UpdateService)
{-# DEPRECATED usPlacementStrategy "Use generic-lens or generic-optics with 'placementStrategy' instead." #-}

-- | Whether to force a new deployment of the service. Deployments are not forced by default. You can use this option to trigger a new deployment with no service definition changes. For example, you can update a service's tasks to use a newer Docker image with the same image/tag combination (@my_image:latest@ ) or to roll Fargate tasks onto a newer platform version.
--
-- /Note:/ Consider using 'forceNewDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usForceNewDeployment :: Lens.Lens' UpdateService (Lude.Maybe Lude.Bool)
usForceNewDeployment = Lens.lens (forceNewDeployment :: UpdateService -> Lude.Maybe Lude.Bool) (\s a -> s {forceNewDeployment = a} :: UpdateService)
{-# DEPRECATED usForceNewDeployment "Use generic-lens or generic-optics with 'forceNewDeployment' instead." #-}

-- | The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to run in your service. If a @revision@ is not specified, the latest @ACTIVE@ revision is used. If you modify the task definition with @UpdateService@ , Amazon ECS spawns a task with the new version of the task definition and then stops an old task after the new version is running.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usTaskDefinition :: Lens.Lens' UpdateService (Lude.Maybe Lude.Text)
usTaskDefinition = Lens.lens (taskDefinition :: UpdateService -> Lude.Maybe Lude.Text) (\s a -> s {taskDefinition = a} :: UpdateService)
{-# DEPRECATED usTaskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead." #-}

-- | The period of time, in seconds, that the Amazon ECS service scheduler should ignore unhealthy Elastic Load Balancing target health checks after a task has first started. This is only valid if your service is configured to use a load balancer. If your service's tasks take a while to start and respond to Elastic Load Balancing health checks, you can specify a health check grace period of up to 2,147,483,647 seconds. During that time, the Amazon ECS service scheduler ignores the Elastic Load Balancing health check status. This grace period can prevent the ECS service scheduler from marking tasks as unhealthy and stopping them before they have time to come up.
--
-- /Note:/ Consider using 'healthCheckGracePeriodSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usHealthCheckGracePeriodSeconds :: Lens.Lens' UpdateService (Lude.Maybe Lude.Int)
usHealthCheckGracePeriodSeconds = Lens.lens (healthCheckGracePeriodSeconds :: UpdateService -> Lude.Maybe Lude.Int) (\s a -> s {healthCheckGracePeriodSeconds = a} :: UpdateService)
{-# DEPRECATED usHealthCheckGracePeriodSeconds "Use generic-lens or generic-optics with 'healthCheckGracePeriodSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'networkConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usNetworkConfiguration :: Lens.Lens' UpdateService (Lude.Maybe NetworkConfiguration)
usNetworkConfiguration = Lens.lens (networkConfiguration :: UpdateService -> Lude.Maybe NetworkConfiguration) (\s a -> s {networkConfiguration = a} :: UpdateService)
{-# DEPRECATED usNetworkConfiguration "Use generic-lens or generic-optics with 'networkConfiguration' instead." #-}

-- | The capacity provider strategy to update the service to use.
--
-- If the service is using the default capacity provider strategy for the cluster, the service can be updated to use one or more capacity providers as opposed to the default capacity provider strategy. However, when a service is using a capacity provider strategy that is not the default capacity provider strategy, the service cannot be updated to use the cluster's default capacity provider strategy.
-- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
--
--
-- /Note:/ Consider using 'capacityProviderStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usCapacityProviderStrategy :: Lens.Lens' UpdateService (Lude.Maybe [CapacityProviderStrategyItem])
usCapacityProviderStrategy = Lens.lens (capacityProviderStrategy :: UpdateService -> Lude.Maybe [CapacityProviderStrategyItem]) (\s a -> s {capacityProviderStrategy = a} :: UpdateService)
{-# DEPRECATED usCapacityProviderStrategy "Use generic-lens or generic-optics with 'capacityProviderStrategy' instead." #-}

-- | Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
--
-- /Note:/ Consider using 'deploymentConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDeploymentConfiguration :: Lens.Lens' UpdateService (Lude.Maybe DeploymentConfiguration)
usDeploymentConfiguration = Lens.lens (deploymentConfiguration :: UpdateService -> Lude.Maybe DeploymentConfiguration) (\s a -> s {deploymentConfiguration = a} :: UpdateService)
{-# DEPRECATED usDeploymentConfiguration "Use generic-lens or generic-optics with 'deploymentConfiguration' instead." #-}

instance Lude.AWSRequest UpdateService where
  type Rs UpdateService = UpdateServiceResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateServiceResponse'
            Lude.<$> (x Lude..?> "service") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateService where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.UpdateService" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateService where
  toJSON UpdateService' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("cluster" Lude..=) Lude.<$> cluster,
            Lude.Just ("service" Lude..= service),
            ("platformVersion" Lude..=) Lude.<$> platformVersion,
            ("desiredCount" Lude..=) Lude.<$> desiredCount,
            ("placementConstraints" Lude..=) Lude.<$> placementConstraints,
            ("placementStrategy" Lude..=) Lude.<$> placementStrategy,
            ("forceNewDeployment" Lude..=) Lude.<$> forceNewDeployment,
            ("taskDefinition" Lude..=) Lude.<$> taskDefinition,
            ("healthCheckGracePeriodSeconds" Lude..=)
              Lude.<$> healthCheckGracePeriodSeconds,
            ("networkConfiguration" Lude..=) Lude.<$> networkConfiguration,
            ("capacityProviderStrategy" Lude..=)
              Lude.<$> capacityProviderStrategy,
            ("deploymentConfiguration" Lude..=)
              Lude.<$> deploymentConfiguration
          ]
      )

instance Lude.ToPath UpdateService where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateService where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateServiceResponse' smart constructor.
data UpdateServiceResponse = UpdateServiceResponse'
  { -- | The full description of your service following the update call.
    service :: Lude.Maybe ContainerService,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateServiceResponse' with the minimum fields required to make a request.
--
-- * 'service' - The full description of your service following the update call.
-- * 'responseStatus' - The response status code.
mkUpdateServiceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateServiceResponse
mkUpdateServiceResponse pResponseStatus_ =
  UpdateServiceResponse'
    { service = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The full description of your service following the update call.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsService :: Lens.Lens' UpdateServiceResponse (Lude.Maybe ContainerService)
usrsService = Lens.lens (service :: UpdateServiceResponse -> Lude.Maybe ContainerService) (\s a -> s {service = a} :: UpdateServiceResponse)
{-# DEPRECATED usrsService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsResponseStatus :: Lens.Lens' UpdateServiceResponse Lude.Int
usrsResponseStatus = Lens.lens (responseStatus :: UpdateServiceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateServiceResponse)
{-# DEPRECATED usrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
