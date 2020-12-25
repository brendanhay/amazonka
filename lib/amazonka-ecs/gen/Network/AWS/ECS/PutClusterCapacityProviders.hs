{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.PutClusterCapacityProviders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the available capacity providers and the default capacity provider strategy for a cluster.
--
-- You must specify both the available capacity providers and a default capacity provider strategy for the cluster. If the specified cluster has existing capacity providers associated with it, you must specify all existing capacity providers in addition to any new ones you want to add. Any existing capacity providers associated with a cluster that are omitted from a 'PutClusterCapacityProviders' API call will be disassociated with the cluster. You can only disassociate an existing capacity provider from a cluster if it's not being used by any existing tasks.
-- When creating a service or running a task on a cluster, if no capacity provider or launch type is specified, then the cluster's default capacity provider strategy is used. It is recommended to define a default capacity provider strategy for your cluster, however you may specify an empty array (@[]@ ) to bypass defining a default strategy.
module Network.AWS.ECS.PutClusterCapacityProviders
  ( -- * Creating a request
    PutClusterCapacityProviders (..),
    mkPutClusterCapacityProviders,

    -- ** Request lenses
    pccpCluster,
    pccpCapacityProviders,
    pccpDefaultCapacityProviderStrategy,

    -- * Destructuring the response
    PutClusterCapacityProvidersResponse (..),
    mkPutClusterCapacityProvidersResponse,

    -- ** Response lenses
    pccprrsCluster,
    pccprrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutClusterCapacityProviders' smart constructor.
data PutClusterCapacityProviders = PutClusterCapacityProviders'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster to modify the capacity provider settings for. If you do not specify a cluster, the default cluster is assumed.
    cluster :: Types.String,
    -- | The name of one or more capacity providers to associate with the cluster.
    --
    -- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
    -- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
    capacityProviders :: [Types.String],
    -- | The capacity provider strategy to use by default for the cluster.
    --
    -- When creating a service or running a task on a cluster, if no capacity provider or launch type is specified then the default capacity provider strategy for the cluster is used.
    -- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
    -- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
    -- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
    defaultCapacityProviderStrategy :: [Types.CapacityProviderStrategyItem]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutClusterCapacityProviders' value with any optional fields omitted.
mkPutClusterCapacityProviders ::
  -- | 'cluster'
  Types.String ->
  PutClusterCapacityProviders
mkPutClusterCapacityProviders cluster =
  PutClusterCapacityProviders'
    { cluster,
      capacityProviders = Core.mempty,
      defaultCapacityProviderStrategy = Core.mempty
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster to modify the capacity provider settings for. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccpCluster :: Lens.Lens' PutClusterCapacityProviders Types.String
pccpCluster = Lens.field @"cluster"
{-# DEPRECATED pccpCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The name of one or more capacity providers to associate with the cluster.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
--
-- /Note:/ Consider using 'capacityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccpCapacityProviders :: Lens.Lens' PutClusterCapacityProviders [Types.String]
pccpCapacityProviders = Lens.field @"capacityProviders"
{-# DEPRECATED pccpCapacityProviders "Use generic-lens or generic-optics with 'capacityProviders' instead." #-}

-- | The capacity provider strategy to use by default for the cluster.
--
-- When creating a service or running a task on a cluster, if no capacity provider or launch type is specified then the default capacity provider strategy for the cluster is used.
-- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
--
-- /Note:/ Consider using 'defaultCapacityProviderStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccpDefaultCapacityProviderStrategy :: Lens.Lens' PutClusterCapacityProviders [Types.CapacityProviderStrategyItem]
pccpDefaultCapacityProviderStrategy = Lens.field @"defaultCapacityProviderStrategy"
{-# DEPRECATED pccpDefaultCapacityProviderStrategy "Use generic-lens or generic-optics with 'defaultCapacityProviderStrategy' instead." #-}

instance Core.FromJSON PutClusterCapacityProviders where
  toJSON PutClusterCapacityProviders {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("cluster" Core..= cluster),
            Core.Just ("capacityProviders" Core..= capacityProviders),
            Core.Just
              ( "defaultCapacityProviderStrategy"
                  Core..= defaultCapacityProviderStrategy
              )
          ]
      )

instance Core.AWSRequest PutClusterCapacityProviders where
  type
    Rs PutClusterCapacityProviders =
      PutClusterCapacityProvidersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.PutClusterCapacityProviders"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutClusterCapacityProvidersResponse'
            Core.<$> (x Core..:? "cluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutClusterCapacityProvidersResponse' smart constructor.
data PutClusterCapacityProvidersResponse = PutClusterCapacityProvidersResponse'
  { cluster :: Core.Maybe Types.Cluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutClusterCapacityProvidersResponse' value with any optional fields omitted.
mkPutClusterCapacityProvidersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutClusterCapacityProvidersResponse
mkPutClusterCapacityProvidersResponse responseStatus =
  PutClusterCapacityProvidersResponse'
    { cluster = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccprrsCluster :: Lens.Lens' PutClusterCapacityProvidersResponse (Core.Maybe Types.Cluster)
pccprrsCluster = Lens.field @"cluster"
{-# DEPRECATED pccprrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccprrsResponseStatus :: Lens.Lens' PutClusterCapacityProvidersResponse Core.Int
pccprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pccprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
