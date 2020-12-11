{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ctsClientToken,
    ctsPlatformVersion,
    ctsScale,
    ctsLoadBalancers,
    ctsLaunchType,
    ctsExternalId,
    ctsNetworkConfiguration,
    ctsServiceRegistries,
    ctsCapacityProviderStrategy,
    ctsTags,
    ctsService,
    ctsCluster,
    ctsTaskDefinition,

    -- * Destructuring the response
    CreateTaskSetResponse (..),
    mkCreateTaskSetResponse,

    -- ** Response lenses
    ctsrsTaskSet,
    ctsrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTaskSet' smart constructor.
data CreateTaskSet = CreateTaskSet'
  { clientToken ::
      Lude.Maybe Lude.Text,
    platformVersion :: Lude.Maybe Lude.Text,
    scale :: Lude.Maybe Scale,
    loadBalancers :: Lude.Maybe [LoadBalancer],
    launchType :: Lude.Maybe LaunchType,
    externalId :: Lude.Maybe Lude.Text,
    networkConfiguration :: Lude.Maybe NetworkConfiguration,
    serviceRegistries :: Lude.Maybe [ServiceRegistry],
    capacityProviderStrategy ::
      Lude.Maybe [CapacityProviderStrategyItem],
    tags :: Lude.Maybe [Tag],
    service :: Lude.Text,
    cluster :: Lude.Text,
    taskDefinition :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTaskSet' with the minimum fields required to make a request.
--
-- * 'capacityProviderStrategy' - The capacity provider strategy to use for the task set.
--
-- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter must be omitted. If no @capacityProviderStrategy@ or @launchType@ is specified, the @defaultCapacityProviderStrategy@ for the cluster is used.
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 32 ASCII characters are allowed.
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service to create the task set in.
-- * 'externalId' - An optional non-unique tag that identifies this task set in external systems. If the task set is associated with a service discovery registry, the tasks in this task set will have the @ECS_TASK_SET_EXTERNAL_ID@ AWS Cloud Map attribute set to the provided value.
-- * 'launchType' - The launch type that new tasks in the task set will use. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter must be omitted.
-- * 'loadBalancers' - A load balancer object representing the load balancer to use with the task set. The supported load balancer types are either an Application Load Balancer or a Network Load Balancer.
-- * 'networkConfiguration' - Undocumented field.
-- * 'platformVersion' - The platform version that the tasks in the task set should use. A platform version is specified only for tasks using the Fargate launch type. If one isn't specified, the @LATEST@ platform version is used by default.
-- * 'scale' - Undocumented field.
-- * 'service' - The short name or full Amazon Resource Name (ARN) of the service to create the task set in.
-- * 'serviceRegistries' - The details of the service discovery registries to assign to this task set. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
-- * 'tags' - The metadata that you apply to the task set to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. When a service is deleted, the tags are deleted as well.
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
-- * 'taskDefinition' - The task definition for the tasks in the task set to use.
mkCreateTaskSet ::
  -- | 'service'
  Lude.Text ->
  -- | 'cluster'
  Lude.Text ->
  -- | 'taskDefinition'
  Lude.Text ->
  CreateTaskSet
mkCreateTaskSet pService_ pCluster_ pTaskDefinition_ =
  CreateTaskSet'
    { clientToken = Lude.Nothing,
      platformVersion = Lude.Nothing,
      scale = Lude.Nothing,
      loadBalancers = Lude.Nothing,
      launchType = Lude.Nothing,
      externalId = Lude.Nothing,
      networkConfiguration = Lude.Nothing,
      serviceRegistries = Lude.Nothing,
      capacityProviderStrategy = Lude.Nothing,
      tags = Lude.Nothing,
      service = pService_,
      cluster = pCluster_,
      taskDefinition = pTaskDefinition_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 32 ASCII characters are allowed.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsClientToken :: Lens.Lens' CreateTaskSet (Lude.Maybe Lude.Text)
ctsClientToken = Lens.lens (clientToken :: CreateTaskSet -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateTaskSet)
{-# DEPRECATED ctsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The platform version that the tasks in the task set should use. A platform version is specified only for tasks using the Fargate launch type. If one isn't specified, the @LATEST@ platform version is used by default.
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsPlatformVersion :: Lens.Lens' CreateTaskSet (Lude.Maybe Lude.Text)
ctsPlatformVersion = Lens.lens (platformVersion :: CreateTaskSet -> Lude.Maybe Lude.Text) (\s a -> s {platformVersion = a} :: CreateTaskSet)
{-# DEPRECATED ctsPlatformVersion "Use generic-lens or generic-optics with 'platformVersion' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsScale :: Lens.Lens' CreateTaskSet (Lude.Maybe Scale)
ctsScale = Lens.lens (scale :: CreateTaskSet -> Lude.Maybe Scale) (\s a -> s {scale = a} :: CreateTaskSet)
{-# DEPRECATED ctsScale "Use generic-lens or generic-optics with 'scale' instead." #-}

-- | A load balancer object representing the load balancer to use with the task set. The supported load balancer types are either an Application Load Balancer or a Network Load Balancer.
--
-- /Note:/ Consider using 'loadBalancers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsLoadBalancers :: Lens.Lens' CreateTaskSet (Lude.Maybe [LoadBalancer])
ctsLoadBalancers = Lens.lens (loadBalancers :: CreateTaskSet -> Lude.Maybe [LoadBalancer]) (\s a -> s {loadBalancers = a} :: CreateTaskSet)
{-# DEPRECATED ctsLoadBalancers "Use generic-lens or generic-optics with 'loadBalancers' instead." #-}

-- | The launch type that new tasks in the task set will use. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter must be omitted.
--
-- /Note:/ Consider using 'launchType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsLaunchType :: Lens.Lens' CreateTaskSet (Lude.Maybe LaunchType)
ctsLaunchType = Lens.lens (launchType :: CreateTaskSet -> Lude.Maybe LaunchType) (\s a -> s {launchType = a} :: CreateTaskSet)
{-# DEPRECATED ctsLaunchType "Use generic-lens or generic-optics with 'launchType' instead." #-}

-- | An optional non-unique tag that identifies this task set in external systems. If the task set is associated with a service discovery registry, the tasks in this task set will have the @ECS_TASK_SET_EXTERNAL_ID@ AWS Cloud Map attribute set to the provided value.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsExternalId :: Lens.Lens' CreateTaskSet (Lude.Maybe Lude.Text)
ctsExternalId = Lens.lens (externalId :: CreateTaskSet -> Lude.Maybe Lude.Text) (\s a -> s {externalId = a} :: CreateTaskSet)
{-# DEPRECATED ctsExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'networkConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsNetworkConfiguration :: Lens.Lens' CreateTaskSet (Lude.Maybe NetworkConfiguration)
ctsNetworkConfiguration = Lens.lens (networkConfiguration :: CreateTaskSet -> Lude.Maybe NetworkConfiguration) (\s a -> s {networkConfiguration = a} :: CreateTaskSet)
{-# DEPRECATED ctsNetworkConfiguration "Use generic-lens or generic-optics with 'networkConfiguration' instead." #-}

-- | The details of the service discovery registries to assign to this task set. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
--
-- /Note:/ Consider using 'serviceRegistries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsServiceRegistries :: Lens.Lens' CreateTaskSet (Lude.Maybe [ServiceRegistry])
ctsServiceRegistries = Lens.lens (serviceRegistries :: CreateTaskSet -> Lude.Maybe [ServiceRegistry]) (\s a -> s {serviceRegistries = a} :: CreateTaskSet)
{-# DEPRECATED ctsServiceRegistries "Use generic-lens or generic-optics with 'serviceRegistries' instead." #-}

-- | The capacity provider strategy to use for the task set.
--
-- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter must be omitted. If no @capacityProviderStrategy@ or @launchType@ is specified, the @defaultCapacityProviderStrategy@ for the cluster is used.
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
--
-- /Note:/ Consider using 'capacityProviderStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsCapacityProviderStrategy :: Lens.Lens' CreateTaskSet (Lude.Maybe [CapacityProviderStrategyItem])
ctsCapacityProviderStrategy = Lens.lens (capacityProviderStrategy :: CreateTaskSet -> Lude.Maybe [CapacityProviderStrategyItem]) (\s a -> s {capacityProviderStrategy = a} :: CreateTaskSet)
{-# DEPRECATED ctsCapacityProviderStrategy "Use generic-lens or generic-optics with 'capacityProviderStrategy' instead." #-}

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
ctsTags :: Lens.Lens' CreateTaskSet (Lude.Maybe [Tag])
ctsTags = Lens.lens (tags :: CreateTaskSet -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateTaskSet)
{-# DEPRECATED ctsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the service to create the task set in.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsService :: Lens.Lens' CreateTaskSet Lude.Text
ctsService = Lens.lens (service :: CreateTaskSet -> Lude.Text) (\s a -> s {service = a} :: CreateTaskSet)
{-# DEPRECATED ctsService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service to create the task set in.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsCluster :: Lens.Lens' CreateTaskSet Lude.Text
ctsCluster = Lens.lens (cluster :: CreateTaskSet -> Lude.Text) (\s a -> s {cluster = a} :: CreateTaskSet)
{-# DEPRECATED ctsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The task definition for the tasks in the task set to use.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsTaskDefinition :: Lens.Lens' CreateTaskSet Lude.Text
ctsTaskDefinition = Lens.lens (taskDefinition :: CreateTaskSet -> Lude.Text) (\s a -> s {taskDefinition = a} :: CreateTaskSet)
{-# DEPRECATED ctsTaskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead." #-}

instance Lude.AWSRequest CreateTaskSet where
  type Rs CreateTaskSet = CreateTaskSetResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateTaskSetResponse'
            Lude.<$> (x Lude..?> "taskSet") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTaskSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.CreateTaskSet" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateTaskSet where
  toJSON CreateTaskSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("clientToken" Lude..=) Lude.<$> clientToken,
            ("platformVersion" Lude..=) Lude.<$> platformVersion,
            ("scale" Lude..=) Lude.<$> scale,
            ("loadBalancers" Lude..=) Lude.<$> loadBalancers,
            ("launchType" Lude..=) Lude.<$> launchType,
            ("externalId" Lude..=) Lude.<$> externalId,
            ("networkConfiguration" Lude..=) Lude.<$> networkConfiguration,
            ("serviceRegistries" Lude..=) Lude.<$> serviceRegistries,
            ("capacityProviderStrategy" Lude..=)
              Lude.<$> capacityProviderStrategy,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("service" Lude..= service),
            Lude.Just ("cluster" Lude..= cluster),
            Lude.Just ("taskDefinition" Lude..= taskDefinition)
          ]
      )

instance Lude.ToPath CreateTaskSet where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTaskSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateTaskSetResponse' smart constructor.
data CreateTaskSetResponse = CreateTaskSetResponse'
  { taskSet ::
      Lude.Maybe TaskSet,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTaskSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'taskSet' - Undocumented field.
mkCreateTaskSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTaskSetResponse
mkCreateTaskSetResponse pResponseStatus_ =
  CreateTaskSetResponse'
    { taskSet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'taskSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsrsTaskSet :: Lens.Lens' CreateTaskSetResponse (Lude.Maybe TaskSet)
ctsrsTaskSet = Lens.lens (taskSet :: CreateTaskSetResponse -> Lude.Maybe TaskSet) (\s a -> s {taskSet = a} :: CreateTaskSetResponse)
{-# DEPRECATED ctsrsTaskSet "Use generic-lens or generic-optics with 'taskSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctsrsResponseStatus :: Lens.Lens' CreateTaskSetResponse Lude.Int
ctsrsResponseStatus = Lens.lens (responseStatus :: CreateTaskSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTaskSetResponse)
{-# DEPRECATED ctsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
