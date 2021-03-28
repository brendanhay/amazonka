{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.ECS.UpdateService
    (
    -- * Creating a request
      UpdateService (..)
    , mkUpdateService
    -- ** Request lenses
    , usService
    , usCapacityProviderStrategy
    , usCluster
    , usDeploymentConfiguration
    , usDesiredCount
    , usForceNewDeployment
    , usHealthCheckGracePeriodSeconds
    , usNetworkConfiguration
    , usPlacementConstraints
    , usPlacementStrategy
    , usPlatformVersion
    , usTaskDefinition

    -- * Destructuring the response
    , UpdateServiceResponse (..)
    , mkUpdateServiceResponse
    -- ** Response lenses
    , usrrsService
    , usrrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateService' smart constructor.
data UpdateService = UpdateService'
  { service :: Core.Text
    -- ^ The name of the service to update.
  , capacityProviderStrategy :: Core.Maybe [Types.CapacityProviderStrategyItem]
    -- ^ The capacity provider strategy to update the service to use.
--
-- If the service is using the default capacity provider strategy for the cluster, the service can be updated to use one or more capacity providers as opposed to the default capacity provider strategy. However, when a service is using a capacity provider strategy that is not the default capacity provider strategy, the service cannot be updated to use the cluster's default capacity provider strategy.
-- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
--
  , cluster :: Core.Maybe Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the cluster that your service is running on. If you do not specify a cluster, the default cluster is assumed.
  , deploymentConfiguration :: Core.Maybe Types.DeploymentConfiguration
    -- ^ Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
  , desiredCount :: Core.Maybe Core.Int
    -- ^ The number of instantiations of the task to place and keep running in your service.
  , forceNewDeployment :: Core.Maybe Core.Bool
    -- ^ Whether to force a new deployment of the service. Deployments are not forced by default. You can use this option to trigger a new deployment with no service definition changes. For example, you can update a service's tasks to use a newer Docker image with the same image/tag combination (@my_image:latest@ ) or to roll Fargate tasks onto a newer platform version.
  , healthCheckGracePeriodSeconds :: Core.Maybe Core.Int
    -- ^ The period of time, in seconds, that the Amazon ECS service scheduler should ignore unhealthy Elastic Load Balancing target health checks after a task has first started. This is only valid if your service is configured to use a load balancer. If your service's tasks take a while to start and respond to Elastic Load Balancing health checks, you can specify a health check grace period of up to 2,147,483,647 seconds. During that time, the Amazon ECS service scheduler ignores the Elastic Load Balancing health check status. This grace period can prevent the ECS service scheduler from marking tasks as unhealthy and stopping them before they have time to come up.
  , networkConfiguration :: Core.Maybe Types.NetworkConfiguration
  , placementConstraints :: Core.Maybe [Types.PlacementConstraint]
    -- ^ An array of task placement constraint objects to update the service to use. If no value is specified, the existing placement constraints for the service will remain unchanged. If this value is specified, it will override any existing placement constraints defined for the service. To remove all existing placement constraints, specify an empty array.
--
-- You can specify a maximum of 10 constraints per task (this limit includes constraints in the task definition and those specified at runtime).
  , placementStrategy :: Core.Maybe [Types.PlacementStrategy]
    -- ^ The task placement strategy objects to update the service to use. If no value is specified, the existing placement strategy for the service will remain unchanged. If this value is specified, it will override the existing placement strategy defined for the service. To remove an existing placement strategy, specify an empty object.
--
-- You can specify a maximum of five strategy rules per service.
  , platformVersion :: Core.Maybe Core.Text
    -- ^ The platform version on which your tasks in the service are running. A platform version is only specified for tasks using the Fargate launch type. If a platform version is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
  , taskDefinition :: Core.Maybe Core.Text
    -- ^ The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to run in your service. If a @revision@ is not specified, the latest @ACTIVE@ revision is used. If you modify the task definition with @UpdateService@ , Amazon ECS spawns a task with the new version of the task definition and then stops an old task after the new version is running.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateService' value with any optional fields omitted.
mkUpdateService
    :: Core.Text -- ^ 'service'
    -> UpdateService
mkUpdateService service
  = UpdateService'{service, capacityProviderStrategy = Core.Nothing,
                   cluster = Core.Nothing, deploymentConfiguration = Core.Nothing,
                   desiredCount = Core.Nothing, forceNewDeployment = Core.Nothing,
                   healthCheckGracePeriodSeconds = Core.Nothing,
                   networkConfiguration = Core.Nothing,
                   placementConstraints = Core.Nothing,
                   placementStrategy = Core.Nothing, platformVersion = Core.Nothing,
                   taskDefinition = Core.Nothing}

-- | The name of the service to update.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usService :: Lens.Lens' UpdateService Core.Text
usService = Lens.field @"service"
{-# INLINEABLE usService #-}
{-# DEPRECATED service "Use generic-lens or generic-optics with 'service' instead"  #-}

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
usCapacityProviderStrategy :: Lens.Lens' UpdateService (Core.Maybe [Types.CapacityProviderStrategyItem])
usCapacityProviderStrategy = Lens.field @"capacityProviderStrategy"
{-# INLINEABLE usCapacityProviderStrategy #-}
{-# DEPRECATED capacityProviderStrategy "Use generic-lens or generic-optics with 'capacityProviderStrategy' instead"  #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that your service is running on. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usCluster :: Lens.Lens' UpdateService (Core.Maybe Core.Text)
usCluster = Lens.field @"cluster"
{-# INLINEABLE usCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
--
-- /Note:/ Consider using 'deploymentConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDeploymentConfiguration :: Lens.Lens' UpdateService (Core.Maybe Types.DeploymentConfiguration)
usDeploymentConfiguration = Lens.field @"deploymentConfiguration"
{-# INLINEABLE usDeploymentConfiguration #-}
{-# DEPRECATED deploymentConfiguration "Use generic-lens or generic-optics with 'deploymentConfiguration' instead"  #-}

-- | The number of instantiations of the task to place and keep running in your service.
--
-- /Note:/ Consider using 'desiredCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDesiredCount :: Lens.Lens' UpdateService (Core.Maybe Core.Int)
usDesiredCount = Lens.field @"desiredCount"
{-# INLINEABLE usDesiredCount #-}
{-# DEPRECATED desiredCount "Use generic-lens or generic-optics with 'desiredCount' instead"  #-}

-- | Whether to force a new deployment of the service. Deployments are not forced by default. You can use this option to trigger a new deployment with no service definition changes. For example, you can update a service's tasks to use a newer Docker image with the same image/tag combination (@my_image:latest@ ) or to roll Fargate tasks onto a newer platform version.
--
-- /Note:/ Consider using 'forceNewDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usForceNewDeployment :: Lens.Lens' UpdateService (Core.Maybe Core.Bool)
usForceNewDeployment = Lens.field @"forceNewDeployment"
{-# INLINEABLE usForceNewDeployment #-}
{-# DEPRECATED forceNewDeployment "Use generic-lens or generic-optics with 'forceNewDeployment' instead"  #-}

-- | The period of time, in seconds, that the Amazon ECS service scheduler should ignore unhealthy Elastic Load Balancing target health checks after a task has first started. This is only valid if your service is configured to use a load balancer. If your service's tasks take a while to start and respond to Elastic Load Balancing health checks, you can specify a health check grace period of up to 2,147,483,647 seconds. During that time, the Amazon ECS service scheduler ignores the Elastic Load Balancing health check status. This grace period can prevent the ECS service scheduler from marking tasks as unhealthy and stopping them before they have time to come up.
--
-- /Note:/ Consider using 'healthCheckGracePeriodSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usHealthCheckGracePeriodSeconds :: Lens.Lens' UpdateService (Core.Maybe Core.Int)
usHealthCheckGracePeriodSeconds = Lens.field @"healthCheckGracePeriodSeconds"
{-# INLINEABLE usHealthCheckGracePeriodSeconds #-}
{-# DEPRECATED healthCheckGracePeriodSeconds "Use generic-lens or generic-optics with 'healthCheckGracePeriodSeconds' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'networkConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usNetworkConfiguration :: Lens.Lens' UpdateService (Core.Maybe Types.NetworkConfiguration)
usNetworkConfiguration = Lens.field @"networkConfiguration"
{-# INLINEABLE usNetworkConfiguration #-}
{-# DEPRECATED networkConfiguration "Use generic-lens or generic-optics with 'networkConfiguration' instead"  #-}

-- | An array of task placement constraint objects to update the service to use. If no value is specified, the existing placement constraints for the service will remain unchanged. If this value is specified, it will override any existing placement constraints defined for the service. To remove all existing placement constraints, specify an empty array.
--
-- You can specify a maximum of 10 constraints per task (this limit includes constraints in the task definition and those specified at runtime).
--
-- /Note:/ Consider using 'placementConstraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usPlacementConstraints :: Lens.Lens' UpdateService (Core.Maybe [Types.PlacementConstraint])
usPlacementConstraints = Lens.field @"placementConstraints"
{-# INLINEABLE usPlacementConstraints #-}
{-# DEPRECATED placementConstraints "Use generic-lens or generic-optics with 'placementConstraints' instead"  #-}

-- | The task placement strategy objects to update the service to use. If no value is specified, the existing placement strategy for the service will remain unchanged. If this value is specified, it will override the existing placement strategy defined for the service. To remove an existing placement strategy, specify an empty object.
--
-- You can specify a maximum of five strategy rules per service.
--
-- /Note:/ Consider using 'placementStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usPlacementStrategy :: Lens.Lens' UpdateService (Core.Maybe [Types.PlacementStrategy])
usPlacementStrategy = Lens.field @"placementStrategy"
{-# INLINEABLE usPlacementStrategy #-}
{-# DEPRECATED placementStrategy "Use generic-lens or generic-optics with 'placementStrategy' instead"  #-}

-- | The platform version on which your tasks in the service are running. A platform version is only specified for tasks using the Fargate launch type. If a platform version is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usPlatformVersion :: Lens.Lens' UpdateService (Core.Maybe Core.Text)
usPlatformVersion = Lens.field @"platformVersion"
{-# INLINEABLE usPlatformVersion #-}
{-# DEPRECATED platformVersion "Use generic-lens or generic-optics with 'platformVersion' instead"  #-}

-- | The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to run in your service. If a @revision@ is not specified, the latest @ACTIVE@ revision is used. If you modify the task definition with @UpdateService@ , Amazon ECS spawns a task with the new version of the task definition and then stops an old task after the new version is running.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usTaskDefinition :: Lens.Lens' UpdateService (Core.Maybe Core.Text)
usTaskDefinition = Lens.field @"taskDefinition"
{-# INLINEABLE usTaskDefinition #-}
{-# DEPRECATED taskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead"  #-}

instance Core.ToQuery UpdateService where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateService where
        toHeaders UpdateService{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.UpdateService")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateService where
        toJSON UpdateService{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("service" Core..= service),
                  ("capacityProviderStrategy" Core..=) Core.<$>
                    capacityProviderStrategy,
                  ("cluster" Core..=) Core.<$> cluster,
                  ("deploymentConfiguration" Core..=) Core.<$>
                    deploymentConfiguration,
                  ("desiredCount" Core..=) Core.<$> desiredCount,
                  ("forceNewDeployment" Core..=) Core.<$> forceNewDeployment,
                  ("healthCheckGracePeriodSeconds" Core..=) Core.<$>
                    healthCheckGracePeriodSeconds,
                  ("networkConfiguration" Core..=) Core.<$> networkConfiguration,
                  ("placementConstraints" Core..=) Core.<$> placementConstraints,
                  ("placementStrategy" Core..=) Core.<$> placementStrategy,
                  ("platformVersion" Core..=) Core.<$> platformVersion,
                  ("taskDefinition" Core..=) Core.<$> taskDefinition])

instance Core.AWSRequest UpdateService where
        type Rs UpdateService = UpdateServiceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateServiceResponse' Core.<$>
                   (x Core..:? "service") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateServiceResponse' smart constructor.
data UpdateServiceResponse = UpdateServiceResponse'
  { service :: Core.Maybe Types.ContainerService
    -- ^ The full description of your service following the update call.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateServiceResponse' value with any optional fields omitted.
mkUpdateServiceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateServiceResponse
mkUpdateServiceResponse responseStatus
  = UpdateServiceResponse'{service = Core.Nothing, responseStatus}

-- | The full description of your service following the update call.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsService :: Lens.Lens' UpdateServiceResponse (Core.Maybe Types.ContainerService)
usrrsService = Lens.field @"service"
{-# INLINEABLE usrrsService #-}
{-# DEPRECATED service "Use generic-lens or generic-optics with 'service' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsResponseStatus :: Lens.Lens' UpdateServiceResponse Core.Int
usrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
