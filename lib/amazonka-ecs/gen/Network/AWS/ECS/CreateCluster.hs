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
-- Module      : Network.AWS.ECS.CreateCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon ECS cluster. By default, your account receives a @default@ cluster when you launch your first container instance. However, you can create your own cluster with a unique name with the @CreateCluster@ action.
module Network.AWS.ECS.CreateCluster
  ( -- * Creating a Request
    createCluster,
    CreateCluster,

    -- * Request Lenses
    ccDefaultCapacityProviderStrategy,
    ccSettings,
    ccClusterName,
    ccCapacityProviders,
    ccTags,

    -- * Destructuring the Response
    createClusterResponse,
    CreateClusterResponse,

    -- * Response Lenses
    ccrsCluster,
    ccrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCluster' smart constructor.
data CreateCluster = CreateCluster'
  { _ccDefaultCapacityProviderStrategy ::
      !(Maybe [CapacityProviderStrategyItem]),
    _ccSettings :: !(Maybe [ClusterSetting]),
    _ccClusterName :: !(Maybe Text),
    _ccCapacityProviders :: !(Maybe [Text]),
    _ccTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccDefaultCapacityProviderStrategy' - The capacity provider strategy to use by default for the cluster. When creating a service or running a task on a cluster, if no capacity provider or launch type is specified then the default capacity provider strategy for the cluster is used. A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used. If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation. To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used. If a default capacity provider strategy is not defined for a cluster during creation, it can be defined later with the 'PutClusterCapacityProviders' API operation.
--
-- * 'ccSettings' - The setting to use when creating a cluster. This parameter is used to enable CloudWatch Container Insights for a cluster. If this value is specified, it will override the @containerInsights@ value set with 'PutAccountSetting' or 'PutAccountSettingDefault' .
--
-- * 'ccClusterName' - The name of your cluster. If you do not specify a name for your cluster, you create a cluster named @default@ . Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed.
--
-- * 'ccCapacityProviders' - The short name of one or more capacity providers to associate with the cluster. If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created and not already associated with another cluster. New capacity providers can be created with the 'CreateCapacityProvider' API operation. To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used. The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
--
-- * 'ccTags' - The metadata that you apply to the cluster to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. The following basic restrictions apply to tags:     * Maximum number of tags per resource - 50     * For each resource, each tag key must be unique, and each tag key can have only one value.     * Maximum key length - 128 Unicode characters in UTF-8     * Maximum value length - 256 Unicode characters in UTF-8     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.     * Tag keys and values are case-sensitive.     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
createCluster ::
  CreateCluster
createCluster =
  CreateCluster'
    { _ccDefaultCapacityProviderStrategy = Nothing,
      _ccSettings = Nothing,
      _ccClusterName = Nothing,
      _ccCapacityProviders = Nothing,
      _ccTags = Nothing
    }

-- | The capacity provider strategy to use by default for the cluster. When creating a service or running a task on a cluster, if no capacity provider or launch type is specified then the default capacity provider strategy for the cluster is used. A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used. If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation. To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used. If a default capacity provider strategy is not defined for a cluster during creation, it can be defined later with the 'PutClusterCapacityProviders' API operation.
ccDefaultCapacityProviderStrategy :: Lens' CreateCluster [CapacityProviderStrategyItem]
ccDefaultCapacityProviderStrategy = lens _ccDefaultCapacityProviderStrategy (\s a -> s {_ccDefaultCapacityProviderStrategy = a}) . _Default . _Coerce

-- | The setting to use when creating a cluster. This parameter is used to enable CloudWatch Container Insights for a cluster. If this value is specified, it will override the @containerInsights@ value set with 'PutAccountSetting' or 'PutAccountSettingDefault' .
ccSettings :: Lens' CreateCluster [ClusterSetting]
ccSettings = lens _ccSettings (\s a -> s {_ccSettings = a}) . _Default . _Coerce

-- | The name of your cluster. If you do not specify a name for your cluster, you create a cluster named @default@ . Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed.
ccClusterName :: Lens' CreateCluster (Maybe Text)
ccClusterName = lens _ccClusterName (\s a -> s {_ccClusterName = a})

-- | The short name of one or more capacity providers to associate with the cluster. If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created and not already associated with another cluster. New capacity providers can be created with the 'CreateCapacityProvider' API operation. To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used. The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
ccCapacityProviders :: Lens' CreateCluster [Text]
ccCapacityProviders = lens _ccCapacityProviders (\s a -> s {_ccCapacityProviders = a}) . _Default . _Coerce

-- | The metadata that you apply to the cluster to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. The following basic restrictions apply to tags:     * Maximum number of tags per resource - 50     * For each resource, each tag key must be unique, and each tag key can have only one value.     * Maximum key length - 128 Unicode characters in UTF-8     * Maximum value length - 256 Unicode characters in UTF-8     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.     * Tag keys and values are case-sensitive.     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
ccTags :: Lens' CreateCluster [Tag]
ccTags = lens _ccTags (\s a -> s {_ccTags = a}) . _Default . _Coerce

instance AWSRequest CreateCluster where
  type Rs CreateCluster = CreateClusterResponse
  request = postJSON ecs
  response =
    receiveJSON
      ( \s h x ->
          CreateClusterResponse'
            <$> (x .?> "cluster") <*> (pure (fromEnum s))
      )

instance Hashable CreateCluster

instance NFData CreateCluster

instance ToHeaders CreateCluster where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonEC2ContainerServiceV20141113.CreateCluster" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateCluster where
  toJSON CreateCluster' {..} =
    object
      ( catMaybes
          [ ("defaultCapacityProviderStrategy" .=)
              <$> _ccDefaultCapacityProviderStrategy,
            ("settings" .=) <$> _ccSettings,
            ("clusterName" .=) <$> _ccClusterName,
            ("capacityProviders" .=) <$> _ccCapacityProviders,
            ("tags" .=) <$> _ccTags
          ]
      )

instance ToPath CreateCluster where
  toPath = const "/"

instance ToQuery CreateCluster where
  toQuery = const mempty

-- | /See:/ 'createClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { _ccrsCluster ::
      !(Maybe Cluster),
    _ccrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsCluster' - The full description of your new cluster.
--
-- * 'ccrsResponseStatus' - -- | The response status code.
createClusterResponse ::
  -- | 'ccrsResponseStatus'
  Int ->
  CreateClusterResponse
createClusterResponse pResponseStatus_ =
  CreateClusterResponse'
    { _ccrsCluster = Nothing,
      _ccrsResponseStatus = pResponseStatus_
    }

-- | The full description of your new cluster.
ccrsCluster :: Lens' CreateClusterResponse (Maybe Cluster)
ccrsCluster = lens _ccrsCluster (\s a -> s {_ccrsCluster = a})

-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateClusterResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\s a -> s {_ccrsResponseStatus = a})

instance NFData CreateClusterResponse
