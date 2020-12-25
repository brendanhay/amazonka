{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.CreateTaskSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a task set in the specified cluster and service. This is used when a service uses the @EXTERNAL@ deployment controller type. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.CreateTaskSet
  ( -- * Creating a request
    CreateTaskSet (..),
    mkCreateTaskSet,

    -- ** Request lenses
    ctsService,
    ctsCluster,
    ctsTaskDefinition,
    ctsCapacityProviderStrategy,
    ctsClientToken,
    ctsExternalId,
    ctsLaunchType,
    ctsLoadBalancers,
    ctsNetworkConfiguration,
    ctsPlatformVersion,
    ctsScale,
    ctsServiceRegistries,
    ctsTags,

    -- * Destructuring the response
    CreateTaskSetResponse (..),
    mkCreateTaskSetResponse,

    -- ** Response lenses
    ctsrrsTaskSet,
    ctsrrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTaskSet' smart constructor.
data CreateTaskSet = CreateTaskSet'
  { -- | The short name or full Amazon Resource Name (ARN) of the service to create the task set in.
    service :: Types.String,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service to create the task set in.
    cluster :: Types.String,
    -- | The task definition for the tasks in the task set to use.
    taskDefinition :: Types.String,
    -- | The capacity provider strategy to use for the task set.
    --
    -- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
    -- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter must be omitted. If no @capacityProviderStrategy@ or @launchType@ is specified, the @defaultCapacityProviderStrategy@ for the cluster is used.
    -- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
    -- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
    -- The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
    capacityProviderStrategy :: Core.Maybe [Types.CapacityProviderStrategyItem],
    -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 32 ASCII characters are allowed.
    clientToken :: Core.Maybe Types.String,
    -- | An optional non-unique tag that identifies this task set in external systems. If the task set is associated with a service discovery registry, the tasks in this task set will have the @ECS_TASK_SET_EXTERNAL_ID@ AWS Cloud Map attribute set to the provided value.
    externalId :: Core.Maybe Types.String,
    -- | The launch type that new tasks in the task set will use. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
    --
    -- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter must be omitted.
    launchType :: Core.Maybe Types.LaunchType,
    -- | A load balancer object representing the load balancer to use with the task set. The supported load balancer types are either an Application Load Balancer or a Network Load Balancer.
    loadBalancers :: Core.Maybe [Types.LoadBalancer],
    networkConfiguration :: Core.Maybe Types.NetworkConfiguration,
    -- | The platform version that the tasks in the task set should use. A platform version is specified only for tasks using the Fargate launch type. If one isn't specified, the @LATEST@ platform version is used by default.
    platformVersion :: Core.Maybe Types.String,
    scale :: Core.Maybe Types.Scale,
    -- | The details of the service discovery registries to assign to this task set. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
    serviceRegistries :: Core.Maybe [Types.ServiceRegistry],
    -- | The metadata that you apply to the task set to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. When a service is deleted, the tags are deleted as well.
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
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTaskSet' value with any optional fields omitted.
mkCreateTaskSet ::
  -- | 'service'
  Types.String ->
  -- | 'cluster'
  Types.String ->
  -- | 'taskDefinition'
  Types.String ->
  CreateTaskSet
mkCreateTaskSet service cluster taskDefinition =
  CreateTaskSet'
    { service,
      cluster,
      taskDefinition,
      capacityProviderStrategy = Core.Nothing,
      clientToken = Core.Nothing,
      externalId = Core.Nothing,
      launchType = Core.Nothing,
      loadBalancers = Core.Nothing,
      networkConfiguration = Core.Nothing,
      platformVersion = Core.Nothing,
      scale = Core.Nothing,
      serviceRegistries = Core.Nothing,
      tags = Core.Nothing
    }

-- | The short name or full Amazon Resource Name (ARN) of the service to create the task set in.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsService :: Lens.Lens' CreateTaskSet Types.String
ctsService = Lens.field @"service"
{-# DEPRECATED ctsService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service to create the task set in.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsCluster :: Lens.Lens' CreateTaskSet Types.String
ctsCluster = Lens.field @"cluster"
{-# DEPRECATED ctsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The task definition for the tasks in the task set to use.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsTaskDefinition :: Lens.Lens' CreateTaskSet Types.String
ctsTaskDefinition = Lens.field @"taskDefinition"
{-# DEPRECATED ctsTaskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead." #-}

-- | The capacity provider strategy to use for the task set.
--
-- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter must be omitted. If no @capacityProviderStrategy@ or @launchType@ is specified, the @defaultCapacityProviderStrategy@ for the cluster is used.
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
--
-- /Note:/ Consider using 'capacityProviderStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsCapacityProviderStrategy :: Lens.Lens' CreateTaskSet (Core.Maybe [Types.CapacityProviderStrategyItem])
ctsCapacityProviderStrategy = Lens.field @"capacityProviderStrategy"
{-# DEPRECATED ctsCapacityProviderStrategy "Use generic-lens or generic-optics with 'capacityProviderStrategy' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 32 ASCII characters are allowed.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsClientToken :: Lens.Lens' CreateTaskSet (Core.Maybe Types.String)
ctsClientToken = Lens.field @"clientToken"
{-# DEPRECATED ctsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | An optional non-unique tag that identifies this task set in external systems. If the task set is associated with a service discovery registry, the tasks in this task set will have the @ECS_TASK_SET_EXTERNAL_ID@ AWS Cloud Map attribute set to the provided value.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsExternalId :: Lens.Lens' CreateTaskSet (Core.Maybe Types.String)
ctsExternalId = Lens.field @"externalId"
{-# DEPRECATED ctsExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

-- | The launch type that new tasks in the task set will use. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter must be omitted.
--
-- /Note:/ Consider using 'launchType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsLaunchType :: Lens.Lens' CreateTaskSet (Core.Maybe Types.LaunchType)
ctsLaunchType = Lens.field @"launchType"
{-# DEPRECATED ctsLaunchType "Use generic-lens or generic-optics with 'launchType' instead." #-}

-- | A load balancer object representing the load balancer to use with the task set. The supported load balancer types are either an Application Load Balancer or a Network Load Balancer.
--
-- /Note:/ Consider using 'loadBalancers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsLoadBalancers :: Lens.Lens' CreateTaskSet (Core.Maybe [Types.LoadBalancer])
ctsLoadBalancers = Lens.field @"loadBalancers"
{-# DEPRECATED ctsLoadBalancers "Use generic-lens or generic-optics with 'loadBalancers' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'networkConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsNetworkConfiguration :: Lens.Lens' CreateTaskSet (Core.Maybe Types.NetworkConfiguration)
ctsNetworkConfiguration = Lens.field @"networkConfiguration"
{-# DEPRECATED ctsNetworkConfiguration "Use generic-lens or generic-optics with 'networkConfiguration' instead." #-}

-- | The platform version that the tasks in the task set should use. A platform version is specified only for tasks using the Fargate launch type. If one isn't specified, the @LATEST@ platform version is used by default.
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsPlatformVersion :: Lens.Lens' CreateTaskSet (Core.Maybe Types.String)
ctsPlatformVersion = Lens.field @"platformVersion"
{-# DEPRECATED ctsPlatformVersion "Use generic-lens or generic-optics with 'platformVersion' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsScale :: Lens.Lens' CreateTaskSet (Core.Maybe Types.Scale)
ctsScale = Lens.field @"scale"
{-# DEPRECATED ctsScale "Use generic-lens or generic-optics with 'scale' instead." #-}

-- | The details of the service discovery registries to assign to this task set. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
--
-- /Note:/ Consider using 'serviceRegistries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsServiceRegistries :: Lens.Lens' CreateTaskSet (Core.Maybe [Types.ServiceRegistry])
ctsServiceRegistries = Lens.field @"serviceRegistries"
{-# DEPRECATED ctsServiceRegistries "Use generic-lens or generic-optics with 'serviceRegistries' instead." #-}

-- | The metadata that you apply to the task set to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. When a service is deleted, the tags are deleted as well.
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
ctsTags :: Lens.Lens' CreateTaskSet (Core.Maybe [Types.Tag])
ctsTags = Lens.field @"tags"
{-# DEPRECATED ctsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateTaskSet where
  toJSON CreateTaskSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("service" Core..= service),
            Core.Just ("cluster" Core..= cluster),
            Core.Just ("taskDefinition" Core..= taskDefinition),
            ("capacityProviderStrategy" Core..=)
              Core.<$> capacityProviderStrategy,
            ("clientToken" Core..=) Core.<$> clientToken,
            ("externalId" Core..=) Core.<$> externalId,
            ("launchType" Core..=) Core.<$> launchType,
            ("loadBalancers" Core..=) Core.<$> loadBalancers,
            ("networkConfiguration" Core..=) Core.<$> networkConfiguration,
            ("platformVersion" Core..=) Core.<$> platformVersion,
            ("scale" Core..=) Core.<$> scale,
            ("serviceRegistries" Core..=) Core.<$> serviceRegistries,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateTaskSet where
  type Rs CreateTaskSet = CreateTaskSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.CreateTaskSet"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTaskSetResponse'
            Core.<$> (x Core..:? "taskSet") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateTaskSetResponse' smart constructor.
data CreateTaskSetResponse = CreateTaskSetResponse'
  { taskSet :: Core.Maybe Types.TaskSet,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateTaskSetResponse' value with any optional fields omitted.
mkCreateTaskSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateTaskSetResponse
mkCreateTaskSetResponse responseStatus =
  CreateTaskSetResponse' {taskSet = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'taskSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsrrsTaskSet :: Lens.Lens' CreateTaskSetResponse (Core.Maybe Types.TaskSet)
ctsrrsTaskSet = Lens.field @"taskSet"
{-# DEPRECATED ctsrrsTaskSet "Use generic-lens or generic-optics with 'taskSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsrrsResponseStatus :: Lens.Lens' CreateTaskSetResponse Core.Int
ctsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
