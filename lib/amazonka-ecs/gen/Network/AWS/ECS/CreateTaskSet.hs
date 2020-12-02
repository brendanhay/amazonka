{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
  ( -- * Creating a Request
    createTaskSet,
    CreateTaskSet,

    -- * Request Lenses
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

    -- * Destructuring the Response
    createTaskSetResponse,
    CreateTaskSetResponse,

    -- * Response Lenses
    ctsrsTaskSet,
    ctsrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTaskSet' smart constructor.
data CreateTaskSet = CreateTaskSet'
  { _ctsClientToken ::
      !(Maybe Text),
    _ctsPlatformVersion :: !(Maybe Text),
    _ctsScale :: !(Maybe Scale),
    _ctsLoadBalancers :: !(Maybe [LoadBalancer]),
    _ctsLaunchType :: !(Maybe LaunchType),
    _ctsExternalId :: !(Maybe Text),
    _ctsNetworkConfiguration :: !(Maybe NetworkConfiguration),
    _ctsServiceRegistries :: !(Maybe [ServiceRegistry]),
    _ctsCapacityProviderStrategy ::
      !(Maybe [CapacityProviderStrategyItem]),
    _ctsTags :: !(Maybe [Tag]),
    _ctsService :: !Text,
    _ctsCluster :: !Text,
    _ctsTaskDefinition :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTaskSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctsClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 32 ASCII characters are allowed.
--
-- * 'ctsPlatformVersion' - The platform version that the tasks in the task set should use. A platform version is specified only for tasks using the Fargate launch type. If one isn't specified, the @LATEST@ platform version is used by default.
--
-- * 'ctsScale' - Undocumented member.
--
-- * 'ctsLoadBalancers' - A load balancer object representing the load balancer to use with the task set. The supported load balancer types are either an Application Load Balancer or a Network Load Balancer.
--
-- * 'ctsLaunchType' - The launch type that new tasks in the task set will use. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ . If a @launchType@ is specified, the @capacityProviderStrategy@ parameter must be omitted.
--
-- * 'ctsExternalId' - An optional non-unique tag that identifies this task set in external systems. If the task set is associated with a service discovery registry, the tasks in this task set will have the @ECS_TASK_SET_EXTERNAL_ID@ AWS Cloud Map attribute set to the provided value.
--
-- * 'ctsNetworkConfiguration' - Undocumented member.
--
-- * 'ctsServiceRegistries' - The details of the service discovery registries to assign to this task set. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
--
-- * 'ctsCapacityProviderStrategy' - The capacity provider strategy to use for the task set. A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used. If a @capacityProviderStrategy@ is specified, the @launchType@ parameter must be omitted. If no @capacityProviderStrategy@ or @launchType@ is specified, the @defaultCapacityProviderStrategy@ for the cluster is used. If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation. To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used. The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
--
-- * 'ctsTags' - The metadata that you apply to the task set to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. When a service is deleted, the tags are deleted as well. The following basic restrictions apply to tags:     * Maximum number of tags per resource - 50     * For each resource, each tag key must be unique, and each tag key can have only one value.     * Maximum key length - 128 Unicode characters in UTF-8     * Maximum value length - 256 Unicode characters in UTF-8     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.     * Tag keys and values are case-sensitive.     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
--
-- * 'ctsService' - The short name or full Amazon Resource Name (ARN) of the service to create the task set in.
--
-- * 'ctsCluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service to create the task set in.
--
-- * 'ctsTaskDefinition' - The task definition for the tasks in the task set to use.
createTaskSet ::
  -- | 'ctsService'
  Text ->
  -- | 'ctsCluster'
  Text ->
  -- | 'ctsTaskDefinition'
  Text ->
  CreateTaskSet
createTaskSet pService_ pCluster_ pTaskDefinition_ =
  CreateTaskSet'
    { _ctsClientToken = Nothing,
      _ctsPlatformVersion = Nothing,
      _ctsScale = Nothing,
      _ctsLoadBalancers = Nothing,
      _ctsLaunchType = Nothing,
      _ctsExternalId = Nothing,
      _ctsNetworkConfiguration = Nothing,
      _ctsServiceRegistries = Nothing,
      _ctsCapacityProviderStrategy = Nothing,
      _ctsTags = Nothing,
      _ctsService = pService_,
      _ctsCluster = pCluster_,
      _ctsTaskDefinition = pTaskDefinition_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. Up to 32 ASCII characters are allowed.
ctsClientToken :: Lens' CreateTaskSet (Maybe Text)
ctsClientToken = lens _ctsClientToken (\s a -> s {_ctsClientToken = a})

-- | The platform version that the tasks in the task set should use. A platform version is specified only for tasks using the Fargate launch type. If one isn't specified, the @LATEST@ platform version is used by default.
ctsPlatformVersion :: Lens' CreateTaskSet (Maybe Text)
ctsPlatformVersion = lens _ctsPlatformVersion (\s a -> s {_ctsPlatformVersion = a})

-- | Undocumented member.
ctsScale :: Lens' CreateTaskSet (Maybe Scale)
ctsScale = lens _ctsScale (\s a -> s {_ctsScale = a})

-- | A load balancer object representing the load balancer to use with the task set. The supported load balancer types are either an Application Load Balancer or a Network Load Balancer.
ctsLoadBalancers :: Lens' CreateTaskSet [LoadBalancer]
ctsLoadBalancers = lens _ctsLoadBalancers (\s a -> s {_ctsLoadBalancers = a}) . _Default . _Coerce

-- | The launch type that new tasks in the task set will use. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ . If a @launchType@ is specified, the @capacityProviderStrategy@ parameter must be omitted.
ctsLaunchType :: Lens' CreateTaskSet (Maybe LaunchType)
ctsLaunchType = lens _ctsLaunchType (\s a -> s {_ctsLaunchType = a})

-- | An optional non-unique tag that identifies this task set in external systems. If the task set is associated with a service discovery registry, the tasks in this task set will have the @ECS_TASK_SET_EXTERNAL_ID@ AWS Cloud Map attribute set to the provided value.
ctsExternalId :: Lens' CreateTaskSet (Maybe Text)
ctsExternalId = lens _ctsExternalId (\s a -> s {_ctsExternalId = a})

-- | Undocumented member.
ctsNetworkConfiguration :: Lens' CreateTaskSet (Maybe NetworkConfiguration)
ctsNetworkConfiguration = lens _ctsNetworkConfiguration (\s a -> s {_ctsNetworkConfiguration = a})

-- | The details of the service discovery registries to assign to this task set. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery> .
ctsServiceRegistries :: Lens' CreateTaskSet [ServiceRegistry]
ctsServiceRegistries = lens _ctsServiceRegistries (\s a -> s {_ctsServiceRegistries = a}) . _Default . _Coerce

-- | The capacity provider strategy to use for the task set. A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used. If a @capacityProviderStrategy@ is specified, the @launchType@ parameter must be omitted. If no @capacityProviderStrategy@ or @launchType@ is specified, the @defaultCapacityProviderStrategy@ for the cluster is used. If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation. To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used. The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
ctsCapacityProviderStrategy :: Lens' CreateTaskSet [CapacityProviderStrategyItem]
ctsCapacityProviderStrategy = lens _ctsCapacityProviderStrategy (\s a -> s {_ctsCapacityProviderStrategy = a}) . _Default . _Coerce

-- | The metadata that you apply to the task set to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. When a service is deleted, the tags are deleted as well. The following basic restrictions apply to tags:     * Maximum number of tags per resource - 50     * For each resource, each tag key must be unique, and each tag key can have only one value.     * Maximum key length - 128 Unicode characters in UTF-8     * Maximum value length - 256 Unicode characters in UTF-8     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.     * Tag keys and values are case-sensitive.     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
ctsTags :: Lens' CreateTaskSet [Tag]
ctsTags = lens _ctsTags (\s a -> s {_ctsTags = a}) . _Default . _Coerce

-- | The short name or full Amazon Resource Name (ARN) of the service to create the task set in.
ctsService :: Lens' CreateTaskSet Text
ctsService = lens _ctsService (\s a -> s {_ctsService = a})

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service to create the task set in.
ctsCluster :: Lens' CreateTaskSet Text
ctsCluster = lens _ctsCluster (\s a -> s {_ctsCluster = a})

-- | The task definition for the tasks in the task set to use.
ctsTaskDefinition :: Lens' CreateTaskSet Text
ctsTaskDefinition = lens _ctsTaskDefinition (\s a -> s {_ctsTaskDefinition = a})

instance AWSRequest CreateTaskSet where
  type Rs CreateTaskSet = CreateTaskSetResponse
  request = postJSON ecs
  response =
    receiveJSON
      ( \s h x ->
          CreateTaskSetResponse'
            <$> (x .?> "taskSet") <*> (pure (fromEnum s))
      )

instance Hashable CreateTaskSet

instance NFData CreateTaskSet

instance ToHeaders CreateTaskSet where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonEC2ContainerServiceV20141113.CreateTaskSet" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateTaskSet where
  toJSON CreateTaskSet' {..} =
    object
      ( catMaybes
          [ ("clientToken" .=) <$> _ctsClientToken,
            ("platformVersion" .=) <$> _ctsPlatformVersion,
            ("scale" .=) <$> _ctsScale,
            ("loadBalancers" .=) <$> _ctsLoadBalancers,
            ("launchType" .=) <$> _ctsLaunchType,
            ("externalId" .=) <$> _ctsExternalId,
            ("networkConfiguration" .=) <$> _ctsNetworkConfiguration,
            ("serviceRegistries" .=) <$> _ctsServiceRegistries,
            ("capacityProviderStrategy" .=) <$> _ctsCapacityProviderStrategy,
            ("tags" .=) <$> _ctsTags,
            Just ("service" .= _ctsService),
            Just ("cluster" .= _ctsCluster),
            Just ("taskDefinition" .= _ctsTaskDefinition)
          ]
      )

instance ToPath CreateTaskSet where
  toPath = const "/"

instance ToQuery CreateTaskSet where
  toQuery = const mempty

-- | /See:/ 'createTaskSetResponse' smart constructor.
data CreateTaskSetResponse = CreateTaskSetResponse'
  { _ctsrsTaskSet ::
      !(Maybe TaskSet),
    _ctsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTaskSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctsrsTaskSet' - Undocumented member.
--
-- * 'ctsrsResponseStatus' - -- | The response status code.
createTaskSetResponse ::
  -- | 'ctsrsResponseStatus'
  Int ->
  CreateTaskSetResponse
createTaskSetResponse pResponseStatus_ =
  CreateTaskSetResponse'
    { _ctsrsTaskSet = Nothing,
      _ctsrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
ctsrsTaskSet :: Lens' CreateTaskSetResponse (Maybe TaskSet)
ctsrsTaskSet = lens _ctsrsTaskSet (\s a -> s {_ctsrsTaskSet = a})

-- | -- | The response status code.
ctsrsResponseStatus :: Lens' CreateTaskSetResponse Int
ctsrsResponseStatus = lens _ctsrsResponseStatus (\s a -> s {_ctsrsResponseStatus = a})

instance NFData CreateTaskSetResponse
