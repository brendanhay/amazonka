{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.CreateService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs and maintains a desired number of tasks from a specified task definition. If the number of tasks running in a service drops below the @desiredCount@ , Amazon ECS runs another copy of the task in the specified cluster. To update an existing service, see the UpdateService action.
--
-- In addition to maintaining the desired count of tasks in your service, you can optionally run your service behind one or more load balancers. The load balancers distribute traffic across the tasks that are associated with the service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-load-balancing.html Service Load Balancing> in the /Amazon Elastic Container Service Developer Guide/ .
-- Tasks for services that /do not/ use a load balancer are considered healthy if they're in the @RUNNING@ state. Tasks for services that /do/ use a load balancer are considered healthy if they're in the @RUNNING@ state and the container instance that they're hosted on is reported as healthy by the load balancer.
-- There are two service scheduler strategies available:
--
--     * @REPLICA@ - The replica scheduling strategy places and maintains the desired number of tasks across your cluster. By default, the service scheduler spreads tasks across Availability Zones. You can use task placement strategies and constraints to customize task placement decisions. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Service Scheduler Concepts> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
--     * @DAEMON@ - The daemon scheduling strategy deploys exactly one task on each active container instance that meets all of the task placement constraints that you specify in your cluster. The service scheduler also evaluates the task placement constraints for running tasks and will stop tasks that do not meet the placement constraints. When using this strategy, you don't need to specify a desired number of tasks, a task placement strategy, or use Service Auto Scaling policies. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Service Scheduler Concepts> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
-- You can optionally specify a deployment configuration for your service. The deployment is triggered by changing properties, such as the task definition or the desired count of a service, with an 'UpdateService' operation. The default value for a replica service for @minimumHealthyPercent@ is 100%. The default value for a daemon service for @minimumHealthyPercent@ is 0%.
-- If a service is using the @ECS@ deployment controller, the minimum healthy percent represents a lower limit on the number of tasks in a service that must remain in the @RUNNING@ state during a deployment, as a percentage of the desired number of tasks (rounded up to the nearest integer), and while any container instances are in the @DRAINING@ state if the service contains tasks using the EC2 launch type. This parameter enables you to deploy without using additional cluster capacity. For example, if your service has a desired number of four tasks and a minimum healthy percent of 50%, the scheduler might stop two existing tasks to free up cluster capacity before starting two new tasks. Tasks for services that /do not/ use a load balancer are considered healthy if they're in the @RUNNING@ state. Tasks for services that /do/ use a load balancer are considered healthy if they're in the @RUNNING@ state and they're reported as healthy by the load balancer. The default value for minimum healthy percent is 100%.
-- If a service is using the @ECS@ deployment controller, the __maximum percent__ parameter represents an upper limit on the number of tasks in a service that are allowed in the @RUNNING@ or @PENDING@ state during a deployment, as a percentage of the desired number of tasks (rounded down to the nearest integer), and while any container instances are in the @DRAINING@ state if the service contains tasks using the EC2 launch type. This parameter enables you to define the deployment batch size. For example, if your service has a desired number of four tasks and a maximum percent value of 200%, the scheduler may start four new tasks before stopping the four older tasks (provided that the cluster resources required to do this are available). The default value for maximum percent is 200%.
-- If a service is using either the @CODE_DEPLOY@ or @EXTERNAL@ deployment controller types and tasks that use the EC2 launch type, the __minimum healthy percent__ and __maximum percent__ values are used only to define the lower and upper limit on the number of the tasks in the service that remain in the @RUNNING@ state while the container instances are in the @DRAINING@ state. If the tasks in the service use the Fargate launch type, the minimum healthy percent and maximum percent values aren't used, although they're currently visible when describing your service.
-- When creating a service that uses the @EXTERNAL@ deployment controller, you can specify only parameters that aren't controlled at the task set level. The only required parameter is the service name. You control your services using the 'CreateTaskSet' operation. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
-- When the service scheduler launches new tasks, it determines task placement in your cluster using the following logic:
--
--     * Determine which of the container instances in your cluster can support your service's task definition (for example, they have the required CPU, memory, ports, and container instance attributes).
--
--
--     * By default, the service scheduler attempts to balance tasks across Availability Zones in this manner (although you can choose a different placement strategy) with the @placementStrategy@ parameter):
--
--     * Sort the valid container instances, giving priority to instances that have the fewest number of running tasks for this service in their respective Availability Zone. For example, if zone A has one running service task and zones B and C each have zero, valid container instances in either zone B or C are considered optimal for placement.
--
--
--     * Place the new service task on a valid container instance in an optimal Availability Zone (based on the previous steps), favoring container instances with the fewest number of running tasks for this service.
--
--
--
--
module Network.AWS.ECS.CreateService
    (
    -- * Creating a request
      CreateService (..)
    , mkCreateService
    -- ** Request lenses
    , csfServiceName
    , csfCapacityProviderStrategy
    , csfClientToken
    , csfCluster
    , csfDeploymentConfiguration
    , csfDeploymentController
    , csfDesiredCount
    , csfEnableECSManagedTags
    , csfHealthCheckGracePeriodSeconds
    , csfLaunchType
    , csfLoadBalancers
    , csfNetworkConfiguration
    , csfPlacementConstraints
    , csfPlacementStrategy
    , csfPlatformVersion
    , csfPropagateTags
    , csfRole
    , csfSchedulingStrategy
    , csfServiceRegistries
    , csfTags
    , csfTaskDefinition

    -- * Destructuring the response
    , CreateServiceResponse (..)
    , mkCreateServiceResponse
    -- ** Response lenses
    , csrrsService
    , csrrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateService' smart constructor.
data CreateService = CreateService'
  { serviceName :: Core.Text
    -- ^ The name of your service. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. Service names must be unique within a cluster, but you can have similarly named services in multiple clusters within a Region or across multiple Regions.
  , capacityProviderStrategy :: Core.Maybe [Types.CapacityProviderStrategyItem]
    -- ^ The capacity provider strategy to use for the service.
--
-- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter must be omitted. If no @capacityProviderStrategy@ or @launchType@ is specified, the @defaultCapacityProviderStrategy@ for the cluster is used.
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 32 ASCII characters are allowed.
  , cluster :: Core.Maybe Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the cluster on which to run your service. If you do not specify a cluster, the default cluster is assumed.
  , deploymentConfiguration :: Core.Maybe Types.DeploymentConfiguration
    -- ^ Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
  , deploymentController :: Core.Maybe Types.DeploymentController
    -- ^ The deployment controller to use for the service.
  , desiredCount :: Core.Maybe Core.Int
    -- ^ The number of instantiations of the specified task definition to place and keep running on your cluster.
--
-- This is required if @schedulingStrategy@ is @REPLICA@ or is not specified. If @schedulingStrategy@ is @DAEMON@ then this is not required.
  , enableECSManagedTags :: Core.Maybe Core.Bool
    -- ^ Specifies whether to enable Amazon ECS managed tags for the tasks within the service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
  , healthCheckGracePeriodSeconds :: Core.Maybe Core.Int
    -- ^ The period of time, in seconds, that the Amazon ECS service scheduler should ignore unhealthy Elastic Load Balancing target health checks after a task has first started. This is only used when your service is configured to use a load balancer. If your service has a load balancer defined and you don't specify a health check grace period value, the default value of @0@ is used.
--
-- If your service's tasks take a while to start and respond to Elastic Load Balancing health checks, you can specify a health check grace period of up to 2,147,483,647 seconds. During that time, the Amazon ECS service scheduler ignores health check status. This grace period can prevent the service scheduler from marking tasks as unhealthy and stopping them before they have time to come up.
  , launchType :: Core.Maybe Types.LaunchType
    -- ^ The launch type on which to run your service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter must be omitted.
  , loadBalancers :: Core.Maybe [Types.LoadBalancer]
    -- ^ A load balancer object representing the load balancers to use with your service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-load-balancing.html Service Load Balancing> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- If the service is using the rolling update (@ECS@ ) deployment controller and using either an Application Load Balancer or Network Load Balancer, you must specify one or more target group ARNs to attach to the service. The service-linked role is required for services that make use of multiple target groups. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/ .
-- If the service is using the @CODE_DEPLOY@ deployment controller, the service is required to use either an Application Load Balancer or Network Load Balancer. When creating an AWS CodeDeploy deployment group, you specify two target groups (referred to as a @targetGroupPair@ ). During a deployment, AWS CodeDeploy determines which task set in your service has the status @PRIMARY@ and associates one target group with it, and then associates the other target group with the replacement task set. The load balancer can also have up to two listeners: a required listener for production traffic and an optional listener that allows you perform validation tests with Lambda functions before routing production traffic to it.
-- After you create a service using the @ECS@ deployment controller, the load balancer name or target group ARN, container name, and container port specified in the service definition are immutable. If you are using the @CODE_DEPLOY@ deployment controller, these values can be changed when updating the service.
-- For Application Load Balancers and Network Load Balancers, this object must contain the load balancer target group ARN, the container name (as it appears in a container definition), and the container port to access from the load balancer. The load balancer name parameter must be omitted. When a task from this service is placed on a container instance, the container instance and port combination is registered as a target in the target group specified here.
-- For Classic Load Balancers, this object must contain the load balancer name, the container name (as it appears in a container definition), and the container port to access from the load balancer. The target group ARN parameter must be omitted. When a task from this service is placed on a container instance, the container instance is registered with the load balancer specified here.
-- Services with tasks that use the @awsvpc@ network mode (for example, those with the Fargate launch type) only support Application Load Balancers and Network Load Balancers. Classic Load Balancers are not supported. Also, when you create any target groups for these services, you must choose @ip@ as the target type, not @instance@ , because tasks that use the @awsvpc@ network mode are associated with an elastic network interface, not an Amazon EC2 instance.
  , networkConfiguration :: Core.Maybe Types.NetworkConfiguration
    -- ^ The network configuration for the service. This parameter is required for task definitions that use the @awsvpc@ network mode to receive their own elastic network interface, and it is not supported for other network modes. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ .
  , placementConstraints :: Core.Maybe [Types.PlacementConstraint]
    -- ^ An array of placement constraint objects to use for tasks in your service. You can specify a maximum of 10 constraints per task (this limit includes constraints in the task definition and those specified at runtime). 
  , placementStrategy :: Core.Maybe [Types.PlacementStrategy]
    -- ^ The placement strategy objects to use for tasks in your service. You can specify a maximum of five strategy rules per service.
  , platformVersion :: Core.Maybe Core.Text
    -- ^ The platform version that your tasks in the service are running on. A platform version is specified only for tasks using the Fargate launch type. If one isn't specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
  , propagateTags :: Core.Maybe Types.PropagateTags
    -- ^ Specifies whether to propagate the tags from the task definition or the service to the tasks in the service. If no value is specified, the tags are not propagated. Tags can only be propagated to the tasks within the service during service creation. To add tags to a task after service creation, use the 'TagResource' API action.
  , role' :: Core.Maybe Core.Text
    -- ^ The name or full Amazon Resource Name (ARN) of the IAM role that allows Amazon ECS to make calls to your load balancer on your behalf. This parameter is only permitted if you are using a load balancer with your service and your task definition does not use the @awsvpc@ network mode. If you specify the @role@ parameter, you must also specify a load balancer object with the @loadBalancers@ parameter.
--
-- /Important:/ If your account has already created the Amazon ECS service-linked role, that role is used by default for your service unless you specify a role here. The service-linked role is required if your task definition uses the @awsvpc@ network mode or if the service is configured to use service discovery, an external deployment controller, multiple target groups, or Elastic Inference accelerators in which case you should not specify a role here. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/ .
-- If your specified role has a path other than @/@ , then you must either specify the full role ARN (this is recommended) or prefix the role name with the path. For example, if a role with the name @bar@ has a path of @/foo/@ then you would specify @/foo/bar@ as the role name. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-friendly-names Friendly Names and Paths> in the /IAM User Guide/ .
  , schedulingStrategy :: Core.Maybe Types.SchedulingStrategy
    -- ^ The scheduling strategy to use for the service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Services> .
--
-- There are two service scheduler strategies available:
--
--     * @REPLICA@ -The replica scheduling strategy places and maintains the desired number of tasks across your cluster. By default, the service scheduler spreads tasks across Availability Zones. You can use task placement strategies and constraints to customize task placement decisions. This scheduler strategy is required if the service is using the @CODE_DEPLOY@ or @EXTERNAL@ deployment controller types.
--
--
--     * @DAEMON@ -The daemon scheduling strategy deploys exactly one task on each active container instance that meets all of the task placement constraints that you specify in your cluster. The service scheduler also evaluates the task placement constraints for running tasks and will stop tasks that do not meet the placement constraints. When you're using this strategy, you don't need to specify a desired number of tasks, a task placement strategy, or use Service Auto Scaling policies.
--
--
  , serviceRegistries :: Core.Maybe [Types.ServiceRegistry]
    -- ^ The details of the service discovery registries to assign to this service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The metadata that you apply to the service to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. When a service is deleted, the tags are deleted as well.
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
  , taskDefinition :: Core.Maybe Core.Text
    -- ^ The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to run in your service. If a @revision@ is not specified, the latest @ACTIVE@ revision is used.
--
-- A task definition must be specified if the service is using either the @ECS@ or @CODE_DEPLOY@ deployment controllers.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateService' value with any optional fields omitted.
mkCreateService
    :: Core.Text -- ^ 'serviceName'
    -> CreateService
mkCreateService serviceName
  = CreateService'{serviceName,
                   capacityProviderStrategy = Core.Nothing,
                   clientToken = Core.Nothing, cluster = Core.Nothing,
                   deploymentConfiguration = Core.Nothing,
                   deploymentController = Core.Nothing, desiredCount = Core.Nothing,
                   enableECSManagedTags = Core.Nothing,
                   healthCheckGracePeriodSeconds = Core.Nothing,
                   launchType = Core.Nothing, loadBalancers = Core.Nothing,
                   networkConfiguration = Core.Nothing,
                   placementConstraints = Core.Nothing,
                   placementStrategy = Core.Nothing, platformVersion = Core.Nothing,
                   propagateTags = Core.Nothing, role' = Core.Nothing,
                   schedulingStrategy = Core.Nothing,
                   serviceRegistries = Core.Nothing, tags = Core.Nothing,
                   taskDefinition = Core.Nothing}

-- | The name of your service. Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. Service names must be unique within a cluster, but you can have similarly named services in multiple clusters within a Region or across multiple Regions.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfServiceName :: Lens.Lens' CreateService Core.Text
csfServiceName = Lens.field @"serviceName"
{-# INLINEABLE csfServiceName #-}
{-# DEPRECATED serviceName "Use generic-lens or generic-optics with 'serviceName' instead"  #-}

-- | The capacity provider strategy to use for the service.
--
-- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter must be omitted. If no @capacityProviderStrategy@ or @launchType@ is specified, the @defaultCapacityProviderStrategy@ for the cluster is used.
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
--
-- /Note:/ Consider using 'capacityProviderStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfCapacityProviderStrategy :: Lens.Lens' CreateService (Core.Maybe [Types.CapacityProviderStrategyItem])
csfCapacityProviderStrategy = Lens.field @"capacityProviderStrategy"
{-# INLINEABLE csfCapacityProviderStrategy #-}
{-# DEPRECATED capacityProviderStrategy "Use generic-lens or generic-optics with 'capacityProviderStrategy' instead"  #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 32 ASCII characters are allowed.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfClientToken :: Lens.Lens' CreateService (Core.Maybe Core.Text)
csfClientToken = Lens.field @"clientToken"
{-# INLINEABLE csfClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster on which to run your service. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfCluster :: Lens.Lens' CreateService (Core.Maybe Core.Text)
csfCluster = Lens.field @"cluster"
{-# INLINEABLE csfCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.
--
-- /Note:/ Consider using 'deploymentConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfDeploymentConfiguration :: Lens.Lens' CreateService (Core.Maybe Types.DeploymentConfiguration)
csfDeploymentConfiguration = Lens.field @"deploymentConfiguration"
{-# INLINEABLE csfDeploymentConfiguration #-}
{-# DEPRECATED deploymentConfiguration "Use generic-lens or generic-optics with 'deploymentConfiguration' instead"  #-}

-- | The deployment controller to use for the service.
--
-- /Note:/ Consider using 'deploymentController' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfDeploymentController :: Lens.Lens' CreateService (Core.Maybe Types.DeploymentController)
csfDeploymentController = Lens.field @"deploymentController"
{-# INLINEABLE csfDeploymentController #-}
{-# DEPRECATED deploymentController "Use generic-lens or generic-optics with 'deploymentController' instead"  #-}

-- | The number of instantiations of the specified task definition to place and keep running on your cluster.
--
-- This is required if @schedulingStrategy@ is @REPLICA@ or is not specified. If @schedulingStrategy@ is @DAEMON@ then this is not required.
--
-- /Note:/ Consider using 'desiredCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfDesiredCount :: Lens.Lens' CreateService (Core.Maybe Core.Int)
csfDesiredCount = Lens.field @"desiredCount"
{-# INLINEABLE csfDesiredCount #-}
{-# DEPRECATED desiredCount "Use generic-lens or generic-optics with 'desiredCount' instead"  #-}

-- | Specifies whether to enable Amazon ECS managed tags for the tasks within the service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'enableECSManagedTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfEnableECSManagedTags :: Lens.Lens' CreateService (Core.Maybe Core.Bool)
csfEnableECSManagedTags = Lens.field @"enableECSManagedTags"
{-# INLINEABLE csfEnableECSManagedTags #-}
{-# DEPRECATED enableECSManagedTags "Use generic-lens or generic-optics with 'enableECSManagedTags' instead"  #-}

-- | The period of time, in seconds, that the Amazon ECS service scheduler should ignore unhealthy Elastic Load Balancing target health checks after a task has first started. This is only used when your service is configured to use a load balancer. If your service has a load balancer defined and you don't specify a health check grace period value, the default value of @0@ is used.
--
-- If your service's tasks take a while to start and respond to Elastic Load Balancing health checks, you can specify a health check grace period of up to 2,147,483,647 seconds. During that time, the Amazon ECS service scheduler ignores health check status. This grace period can prevent the service scheduler from marking tasks as unhealthy and stopping them before they have time to come up.
--
-- /Note:/ Consider using 'healthCheckGracePeriodSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfHealthCheckGracePeriodSeconds :: Lens.Lens' CreateService (Core.Maybe Core.Int)
csfHealthCheckGracePeriodSeconds = Lens.field @"healthCheckGracePeriodSeconds"
{-# INLINEABLE csfHealthCheckGracePeriodSeconds #-}
{-# DEPRECATED healthCheckGracePeriodSeconds "Use generic-lens or generic-optics with 'healthCheckGracePeriodSeconds' instead"  #-}

-- | The launch type on which to run your service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter must be omitted.
--
-- /Note:/ Consider using 'launchType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfLaunchType :: Lens.Lens' CreateService (Core.Maybe Types.LaunchType)
csfLaunchType = Lens.field @"launchType"
{-# INLINEABLE csfLaunchType #-}
{-# DEPRECATED launchType "Use generic-lens or generic-optics with 'launchType' instead"  #-}

-- | A load balancer object representing the load balancers to use with your service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-load-balancing.html Service Load Balancing> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- If the service is using the rolling update (@ECS@ ) deployment controller and using either an Application Load Balancer or Network Load Balancer, you must specify one or more target group ARNs to attach to the service. The service-linked role is required for services that make use of multiple target groups. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/ .
-- If the service is using the @CODE_DEPLOY@ deployment controller, the service is required to use either an Application Load Balancer or Network Load Balancer. When creating an AWS CodeDeploy deployment group, you specify two target groups (referred to as a @targetGroupPair@ ). During a deployment, AWS CodeDeploy determines which task set in your service has the status @PRIMARY@ and associates one target group with it, and then associates the other target group with the replacement task set. The load balancer can also have up to two listeners: a required listener for production traffic and an optional listener that allows you perform validation tests with Lambda functions before routing production traffic to it.
-- After you create a service using the @ECS@ deployment controller, the load balancer name or target group ARN, container name, and container port specified in the service definition are immutable. If you are using the @CODE_DEPLOY@ deployment controller, these values can be changed when updating the service.
-- For Application Load Balancers and Network Load Balancers, this object must contain the load balancer target group ARN, the container name (as it appears in a container definition), and the container port to access from the load balancer. The load balancer name parameter must be omitted. When a task from this service is placed on a container instance, the container instance and port combination is registered as a target in the target group specified here.
-- For Classic Load Balancers, this object must contain the load balancer name, the container name (as it appears in a container definition), and the container port to access from the load balancer. The target group ARN parameter must be omitted. When a task from this service is placed on a container instance, the container instance is registered with the load balancer specified here.
-- Services with tasks that use the @awsvpc@ network mode (for example, those with the Fargate launch type) only support Application Load Balancers and Network Load Balancers. Classic Load Balancers are not supported. Also, when you create any target groups for these services, you must choose @ip@ as the target type, not @instance@ , because tasks that use the @awsvpc@ network mode are associated with an elastic network interface, not an Amazon EC2 instance.
--
-- /Note:/ Consider using 'loadBalancers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfLoadBalancers :: Lens.Lens' CreateService (Core.Maybe [Types.LoadBalancer])
csfLoadBalancers = Lens.field @"loadBalancers"
{-# INLINEABLE csfLoadBalancers #-}
{-# DEPRECATED loadBalancers "Use generic-lens or generic-optics with 'loadBalancers' instead"  #-}

-- | The network configuration for the service. This parameter is required for task definitions that use the @awsvpc@ network mode to receive their own elastic network interface, and it is not supported for other network modes. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'networkConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfNetworkConfiguration :: Lens.Lens' CreateService (Core.Maybe Types.NetworkConfiguration)
csfNetworkConfiguration = Lens.field @"networkConfiguration"
{-# INLINEABLE csfNetworkConfiguration #-}
{-# DEPRECATED networkConfiguration "Use generic-lens or generic-optics with 'networkConfiguration' instead"  #-}

-- | An array of placement constraint objects to use for tasks in your service. You can specify a maximum of 10 constraints per task (this limit includes constraints in the task definition and those specified at runtime). 
--
-- /Note:/ Consider using 'placementConstraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfPlacementConstraints :: Lens.Lens' CreateService (Core.Maybe [Types.PlacementConstraint])
csfPlacementConstraints = Lens.field @"placementConstraints"
{-# INLINEABLE csfPlacementConstraints #-}
{-# DEPRECATED placementConstraints "Use generic-lens or generic-optics with 'placementConstraints' instead"  #-}

-- | The placement strategy objects to use for tasks in your service. You can specify a maximum of five strategy rules per service.
--
-- /Note:/ Consider using 'placementStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfPlacementStrategy :: Lens.Lens' CreateService (Core.Maybe [Types.PlacementStrategy])
csfPlacementStrategy = Lens.field @"placementStrategy"
{-# INLINEABLE csfPlacementStrategy #-}
{-# DEPRECATED placementStrategy "Use generic-lens or generic-optics with 'placementStrategy' instead"  #-}

-- | The platform version that your tasks in the service are running on. A platform version is specified only for tasks using the Fargate launch type. If one isn't specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfPlatformVersion :: Lens.Lens' CreateService (Core.Maybe Core.Text)
csfPlatformVersion = Lens.field @"platformVersion"
{-# INLINEABLE csfPlatformVersion #-}
{-# DEPRECATED platformVersion "Use generic-lens or generic-optics with 'platformVersion' instead"  #-}

-- | Specifies whether to propagate the tags from the task definition or the service to the tasks in the service. If no value is specified, the tags are not propagated. Tags can only be propagated to the tasks within the service during service creation. To add tags to a task after service creation, use the 'TagResource' API action.
--
-- /Note:/ Consider using 'propagateTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfPropagateTags :: Lens.Lens' CreateService (Core.Maybe Types.PropagateTags)
csfPropagateTags = Lens.field @"propagateTags"
{-# INLINEABLE csfPropagateTags #-}
{-# DEPRECATED propagateTags "Use generic-lens or generic-optics with 'propagateTags' instead"  #-}

-- | The name or full Amazon Resource Name (ARN) of the IAM role that allows Amazon ECS to make calls to your load balancer on your behalf. This parameter is only permitted if you are using a load balancer with your service and your task definition does not use the @awsvpc@ network mode. If you specify the @role@ parameter, you must also specify a load balancer object with the @loadBalancers@ parameter.
--
-- /Important:/ If your account has already created the Amazon ECS service-linked role, that role is used by default for your service unless you specify a role here. The service-linked role is required if your task definition uses the @awsvpc@ network mode or if the service is configured to use service discovery, an external deployment controller, multiple target groups, or Elastic Inference accelerators in which case you should not specify a role here. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/ .
-- If your specified role has a path other than @/@ , then you must either specify the full role ARN (this is recommended) or prefix the role name with the path. For example, if a role with the name @bar@ has a path of @/foo/@ then you would specify @/foo/bar@ as the role name. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-friendly-names Friendly Names and Paths> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfRole :: Lens.Lens' CreateService (Core.Maybe Core.Text)
csfRole = Lens.field @"role'"
{-# INLINEABLE csfRole #-}
{-# DEPRECATED role' "Use generic-lens or generic-optics with 'role'' instead"  #-}

-- | The scheduling strategy to use for the service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs_services.html Services> .
--
-- There are two service scheduler strategies available:
--
--     * @REPLICA@ -The replica scheduling strategy places and maintains the desired number of tasks across your cluster. By default, the service scheduler spreads tasks across Availability Zones. You can use task placement strategies and constraints to customize task placement decisions. This scheduler strategy is required if the service is using the @CODE_DEPLOY@ or @EXTERNAL@ deployment controller types.
--
--
--     * @DAEMON@ -The daemon scheduling strategy deploys exactly one task on each active container instance that meets all of the task placement constraints that you specify in your cluster. The service scheduler also evaluates the task placement constraints for running tasks and will stop tasks that do not meet the placement constraints. When you're using this strategy, you don't need to specify a desired number of tasks, a task placement strategy, or use Service Auto Scaling policies.
--
--
--
-- /Note:/ Consider using 'schedulingStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfSchedulingStrategy :: Lens.Lens' CreateService (Core.Maybe Types.SchedulingStrategy)
csfSchedulingStrategy = Lens.field @"schedulingStrategy"
{-# INLINEABLE csfSchedulingStrategy #-}
{-# DEPRECATED schedulingStrategy "Use generic-lens or generic-optics with 'schedulingStrategy' instead"  #-}

-- | The details of the service discovery registries to assign to this service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
--
-- /Note:/ Consider using 'serviceRegistries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfServiceRegistries :: Lens.Lens' CreateService (Core.Maybe [Types.ServiceRegistry])
csfServiceRegistries = Lens.field @"serviceRegistries"
{-# INLINEABLE csfServiceRegistries #-}
{-# DEPRECATED serviceRegistries "Use generic-lens or generic-optics with 'serviceRegistries' instead"  #-}

-- | The metadata that you apply to the service to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. When a service is deleted, the tags are deleted as well.
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
csfTags :: Lens.Lens' CreateService (Core.Maybe [Types.Tag])
csfTags = Lens.field @"tags"
{-# INLINEABLE csfTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to run in your service. If a @revision@ is not specified, the latest @ACTIVE@ revision is used.
--
-- A task definition must be specified if the service is using either the @ECS@ or @CODE_DEPLOY@ deployment controllers.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfTaskDefinition :: Lens.Lens' CreateService (Core.Maybe Core.Text)
csfTaskDefinition = Lens.field @"taskDefinition"
{-# INLINEABLE csfTaskDefinition #-}
{-# DEPRECATED taskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead"  #-}

instance Core.ToQuery CreateService where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateService where
        toHeaders CreateService{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.CreateService")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateService where
        toJSON CreateService{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("serviceName" Core..= serviceName),
                  ("capacityProviderStrategy" Core..=) Core.<$>
                    capacityProviderStrategy,
                  ("clientToken" Core..=) Core.<$> clientToken,
                  ("cluster" Core..=) Core.<$> cluster,
                  ("deploymentConfiguration" Core..=) Core.<$>
                    deploymentConfiguration,
                  ("deploymentController" Core..=) Core.<$> deploymentController,
                  ("desiredCount" Core..=) Core.<$> desiredCount,
                  ("enableECSManagedTags" Core..=) Core.<$> enableECSManagedTags,
                  ("healthCheckGracePeriodSeconds" Core..=) Core.<$>
                    healthCheckGracePeriodSeconds,
                  ("launchType" Core..=) Core.<$> launchType,
                  ("loadBalancers" Core..=) Core.<$> loadBalancers,
                  ("networkConfiguration" Core..=) Core.<$> networkConfiguration,
                  ("placementConstraints" Core..=) Core.<$> placementConstraints,
                  ("placementStrategy" Core..=) Core.<$> placementStrategy,
                  ("platformVersion" Core..=) Core.<$> platformVersion,
                  ("propagateTags" Core..=) Core.<$> propagateTags,
                  ("role" Core..=) Core.<$> role',
                  ("schedulingStrategy" Core..=) Core.<$> schedulingStrategy,
                  ("serviceRegistries" Core..=) Core.<$> serviceRegistries,
                  ("tags" Core..=) Core.<$> tags,
                  ("taskDefinition" Core..=) Core.<$> taskDefinition])

instance Core.AWSRequest CreateService where
        type Rs CreateService = CreateServiceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateServiceResponse' Core.<$>
                   (x Core..:? "service") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateServiceResponse' smart constructor.
data CreateServiceResponse = CreateServiceResponse'
  { service :: Core.Maybe Types.ContainerService
    -- ^ The full description of your service following the create call.
--
-- If a service is using the @ECS@ deployment controller, the @deploymentController@ and @taskSets@ parameters will not be returned.
-- If the service is using the @CODE_DEPLOY@ deployment controller, the @deploymentController@ , @taskSets@ and @deployments@ parameters will be returned, however the @deployments@ parameter will be an empty list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateServiceResponse' value with any optional fields omitted.
mkCreateServiceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateServiceResponse
mkCreateServiceResponse responseStatus
  = CreateServiceResponse'{service = Core.Nothing, responseStatus}

-- | The full description of your service following the create call.
--
-- If a service is using the @ECS@ deployment controller, the @deploymentController@ and @taskSets@ parameters will not be returned.
-- If the service is using the @CODE_DEPLOY@ deployment controller, the @deploymentController@ , @taskSets@ and @deployments@ parameters will be returned, however the @deployments@ parameter will be an empty list.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsService :: Lens.Lens' CreateServiceResponse (Core.Maybe Types.ContainerService)
csrrsService = Lens.field @"service"
{-# INLINEABLE csrrsService #-}
{-# DEPRECATED service "Use generic-lens or generic-optics with 'service' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateServiceResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
