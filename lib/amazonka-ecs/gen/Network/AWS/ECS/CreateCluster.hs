{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  ( -- * Creating a request
    CreateCluster (..),
    mkCreateCluster,

    -- ** Request lenses
    ccDefaultCapacityProviderStrategy,
    ccSettings,
    ccClusterName,
    ccCapacityProviders,
    ccTags,

    -- * Destructuring the response
    CreateClusterResponse (..),
    mkCreateClusterResponse,

    -- ** Response lenses
    ccrsCluster,
    ccrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { defaultCapacityProviderStrategy ::
      Lude.Maybe [CapacityProviderStrategyItem],
    settings :: Lude.Maybe [ClusterSetting],
    clusterName :: Lude.Maybe Lude.Text,
    capacityProviders :: Lude.Maybe [Lude.Text],
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCluster' with the minimum fields required to make a request.
--
-- * 'capacityProviders' - The short name of one or more capacity providers to associate with the cluster.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created and not already associated with another cluster. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
-- * 'clusterName' - The name of your cluster. If you do not specify a name for your cluster, you create a cluster named @default@ . Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed.
-- * 'defaultCapacityProviderStrategy' - The capacity provider strategy to use by default for the cluster.
--
-- When creating a service or running a task on a cluster, if no capacity provider or launch type is specified then the default capacity provider strategy for the cluster is used.
-- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- If a default capacity provider strategy is not defined for a cluster during creation, it can be defined later with the 'PutClusterCapacityProviders' API operation.
-- * 'settings' - The setting to use when creating a cluster. This parameter is used to enable CloudWatch Container Insights for a cluster. If this value is specified, it will override the @containerInsights@ value set with 'PutAccountSetting' or 'PutAccountSettingDefault' .
-- * 'tags' - The metadata that you apply to the cluster to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
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
mkCreateCluster ::
  CreateCluster
mkCreateCluster =
  CreateCluster'
    { defaultCapacityProviderStrategy = Lude.Nothing,
      settings = Lude.Nothing,
      clusterName = Lude.Nothing,
      capacityProviders = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The capacity provider strategy to use by default for the cluster.
--
-- When creating a service or running a task on a cluster, if no capacity provider or launch type is specified then the default capacity provider strategy for the cluster is used.
-- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- If a default capacity provider strategy is not defined for a cluster during creation, it can be defined later with the 'PutClusterCapacityProviders' API operation.
--
-- /Note:/ Consider using 'defaultCapacityProviderStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDefaultCapacityProviderStrategy :: Lens.Lens' CreateCluster (Lude.Maybe [CapacityProviderStrategyItem])
ccDefaultCapacityProviderStrategy = Lens.lens (defaultCapacityProviderStrategy :: CreateCluster -> Lude.Maybe [CapacityProviderStrategyItem]) (\s a -> s {defaultCapacityProviderStrategy = a} :: CreateCluster)
{-# DEPRECATED ccDefaultCapacityProviderStrategy "Use generic-lens or generic-optics with 'defaultCapacityProviderStrategy' instead." #-}

-- | The setting to use when creating a cluster. This parameter is used to enable CloudWatch Container Insights for a cluster. If this value is specified, it will override the @containerInsights@ value set with 'PutAccountSetting' or 'PutAccountSettingDefault' .
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSettings :: Lens.Lens' CreateCluster (Lude.Maybe [ClusterSetting])
ccSettings = Lens.lens (settings :: CreateCluster -> Lude.Maybe [ClusterSetting]) (\s a -> s {settings = a} :: CreateCluster)
{-# DEPRECATED ccSettings "Use generic-lens or generic-optics with 'settings' instead." #-}

-- | The name of your cluster. If you do not specify a name for your cluster, you create a cluster named @default@ . Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed.
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClusterName :: Lens.Lens' CreateCluster (Lude.Maybe Lude.Text)
ccClusterName = Lens.lens (clusterName :: CreateCluster -> Lude.Maybe Lude.Text) (\s a -> s {clusterName = a} :: CreateCluster)
{-# DEPRECATED ccClusterName "Use generic-lens or generic-optics with 'clusterName' instead." #-}

-- | The short name of one or more capacity providers to associate with the cluster.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created and not already associated with another cluster. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
--
-- /Note:/ Consider using 'capacityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCapacityProviders :: Lens.Lens' CreateCluster (Lude.Maybe [Lude.Text])
ccCapacityProviders = Lens.lens (capacityProviders :: CreateCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {capacityProviders = a} :: CreateCluster)
{-# DEPRECATED ccCapacityProviders "Use generic-lens or generic-optics with 'capacityProviders' instead." #-}

-- | The metadata that you apply to the cluster to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
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
ccTags :: Lens.Lens' CreateCluster (Lude.Maybe [Tag])
ccTags = Lens.lens (tags :: CreateCluster -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateCluster)
{-# DEPRECATED ccTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateCluster where
  type Rs CreateCluster = CreateClusterResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateClusterResponse'
            Lude.<$> (x Lude..?> "cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCluster where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.CreateCluster" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCluster where
  toJSON CreateCluster' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("defaultCapacityProviderStrategy" Lude..=)
              Lude.<$> defaultCapacityProviderStrategy,
            ("settings" Lude..=) Lude.<$> settings,
            ("clusterName" Lude..=) Lude.<$> clusterName,
            ("capacityProviders" Lude..=) Lude.<$> capacityProviders,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCluster where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { cluster ::
      Lude.Maybe Cluster,
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

-- | Creates a value of 'CreateClusterResponse' with the minimum fields required to make a request.
--
-- * 'cluster' - The full description of your new cluster.
-- * 'responseStatus' - The response status code.
mkCreateClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateClusterResponse
mkCreateClusterResponse pResponseStatus_ =
  CreateClusterResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The full description of your new cluster.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsCluster :: Lens.Lens' CreateClusterResponse (Lude.Maybe Cluster)
ccrsCluster = Lens.lens (cluster :: CreateClusterResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: CreateClusterResponse)
{-# DEPRECATED ccrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CreateClusterResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CreateClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateClusterResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
