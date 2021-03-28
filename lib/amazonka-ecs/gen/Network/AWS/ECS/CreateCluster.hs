{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateCluster (..)
    , mkCreateCluster
    -- ** Request lenses
    , ccCapacityProviders
    , ccClusterName
    , ccDefaultCapacityProviderStrategy
    , ccSettings
    , ccTags

    -- * Destructuring the response
    , CreateClusterResponse (..)
    , mkCreateClusterResponse
    -- ** Response lenses
    , ccrrsCluster
    , ccrrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { capacityProviders :: Core.Maybe [Core.Text]
    -- ^ The short name of one or more capacity providers to associate with the cluster.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created and not already associated with another cluster. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
  , clusterName :: Core.Maybe Core.Text
    -- ^ The name of your cluster. If you do not specify a name for your cluster, you create a cluster named @default@ . Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. 
  , defaultCapacityProviderStrategy :: Core.Maybe [Types.CapacityProviderStrategyItem]
    -- ^ The capacity provider strategy to use by default for the cluster.
--
-- When creating a service or running a task on a cluster, if no capacity provider or launch type is specified then the default capacity provider strategy for the cluster is used.
-- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- If a default capacity provider strategy is not defined for a cluster during creation, it can be defined later with the 'PutClusterCapacityProviders' API operation.
  , settings :: Core.Maybe [Types.ClusterSetting]
    -- ^ The setting to use when creating a cluster. This parameter is used to enable CloudWatch Container Insights for a cluster. If this value is specified, it will override the @containerInsights@ value set with 'PutAccountSetting' or 'PutAccountSettingDefault' .
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The metadata that you apply to the cluster to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCluster' value with any optional fields omitted.
mkCreateCluster
    :: CreateCluster
mkCreateCluster
  = CreateCluster'{capacityProviders = Core.Nothing,
                   clusterName = Core.Nothing,
                   defaultCapacityProviderStrategy = Core.Nothing,
                   settings = Core.Nothing, tags = Core.Nothing}

-- | The short name of one or more capacity providers to associate with the cluster.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created and not already associated with another cluster. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
--
-- /Note:/ Consider using 'capacityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCapacityProviders :: Lens.Lens' CreateCluster (Core.Maybe [Core.Text])
ccCapacityProviders = Lens.field @"capacityProviders"
{-# INLINEABLE ccCapacityProviders #-}
{-# DEPRECATED capacityProviders "Use generic-lens or generic-optics with 'capacityProviders' instead"  #-}

-- | The name of your cluster. If you do not specify a name for your cluster, you create a cluster named @default@ . Up to 255 letters (uppercase and lowercase), numbers, and hyphens are allowed. 
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClusterName :: Lens.Lens' CreateCluster (Core.Maybe Core.Text)
ccClusterName = Lens.field @"clusterName"
{-# INLINEABLE ccClusterName #-}
{-# DEPRECATED clusterName "Use generic-lens or generic-optics with 'clusterName' instead"  #-}

-- | The capacity provider strategy to use by default for the cluster.
--
-- When creating a service or running a task on a cluster, if no capacity provider or launch type is specified then the default capacity provider strategy for the cluster is used.
-- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- If a default capacity provider strategy is not defined for a cluster during creation, it can be defined later with the 'PutClusterCapacityProviders' API operation.
--
-- /Note:/ Consider using 'defaultCapacityProviderStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDefaultCapacityProviderStrategy :: Lens.Lens' CreateCluster (Core.Maybe [Types.CapacityProviderStrategyItem])
ccDefaultCapacityProviderStrategy = Lens.field @"defaultCapacityProviderStrategy"
{-# INLINEABLE ccDefaultCapacityProviderStrategy #-}
{-# DEPRECATED defaultCapacityProviderStrategy "Use generic-lens or generic-optics with 'defaultCapacityProviderStrategy' instead"  #-}

-- | The setting to use when creating a cluster. This parameter is used to enable CloudWatch Container Insights for a cluster. If this value is specified, it will override the @containerInsights@ value set with 'PutAccountSetting' or 'PutAccountSettingDefault' .
--
-- /Note:/ Consider using 'settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSettings :: Lens.Lens' CreateCluster (Core.Maybe [Types.ClusterSetting])
ccSettings = Lens.field @"settings"
{-# INLINEABLE ccSettings #-}
{-# DEPRECATED settings "Use generic-lens or generic-optics with 'settings' instead"  #-}

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
ccTags :: Lens.Lens' CreateCluster (Core.Maybe [Types.Tag])
ccTags = Lens.field @"tags"
{-# INLINEABLE ccTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateCluster where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateCluster where
        toHeaders CreateCluster{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.CreateCluster")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateCluster where
        toJSON CreateCluster{..}
          = Core.object
              (Core.catMaybes
                 [("capacityProviders" Core..=) Core.<$> capacityProviders,
                  ("clusterName" Core..=) Core.<$> clusterName,
                  ("defaultCapacityProviderStrategy" Core..=) Core.<$>
                    defaultCapacityProviderStrategy,
                  ("settings" Core..=) Core.<$> settings,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateCluster where
        type Rs CreateCluster = CreateClusterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateClusterResponse' Core.<$>
                   (x Core..:? "cluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { cluster :: Core.Maybe Types.Cluster
    -- ^ The full description of your new cluster.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateClusterResponse' value with any optional fields omitted.
mkCreateClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateClusterResponse
mkCreateClusterResponse responseStatus
  = CreateClusterResponse'{cluster = Core.Nothing, responseStatus}

-- | The full description of your new cluster.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsCluster :: Lens.Lens' CreateClusterResponse (Core.Maybe Types.Cluster)
ccrrsCluster = Lens.field @"cluster"
{-# INLINEABLE ccrrsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateClusterResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
