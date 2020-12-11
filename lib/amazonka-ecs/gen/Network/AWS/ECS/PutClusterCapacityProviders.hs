{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    pccprsCluster,
    pccprsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutClusterCapacityProviders' smart constructor.
data PutClusterCapacityProviders = PutClusterCapacityProviders'
  { cluster ::
      Lude.Text,
    capacityProviders :: [Lude.Text],
    defaultCapacityProviderStrategy ::
      [CapacityProviderStrategyItem]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutClusterCapacityProviders' with the minimum fields required to make a request.
--
-- * 'capacityProviders' - The name of one or more capacity providers to associate with the cluster.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster to modify the capacity provider settings for. If you do not specify a cluster, the default cluster is assumed.
-- * 'defaultCapacityProviderStrategy' - The capacity provider strategy to use by default for the cluster.
--
-- When creating a service or running a task on a cluster, if no capacity provider or launch type is specified then the default capacity provider strategy for the cluster is used.
-- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
mkPutClusterCapacityProviders ::
  -- | 'cluster'
  Lude.Text ->
  PutClusterCapacityProviders
mkPutClusterCapacityProviders pCluster_ =
  PutClusterCapacityProviders'
    { cluster = pCluster_,
      capacityProviders = Lude.mempty,
      defaultCapacityProviderStrategy = Lude.mempty
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster to modify the capacity provider settings for. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccpCluster :: Lens.Lens' PutClusterCapacityProviders Lude.Text
pccpCluster = Lens.lens (cluster :: PutClusterCapacityProviders -> Lude.Text) (\s a -> s {cluster = a} :: PutClusterCapacityProviders)
{-# DEPRECATED pccpCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The name of one or more capacity providers to associate with the cluster.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
--
-- /Note:/ Consider using 'capacityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccpCapacityProviders :: Lens.Lens' PutClusterCapacityProviders [Lude.Text]
pccpCapacityProviders = Lens.lens (capacityProviders :: PutClusterCapacityProviders -> [Lude.Text]) (\s a -> s {capacityProviders = a} :: PutClusterCapacityProviders)
{-# DEPRECATED pccpCapacityProviders "Use generic-lens or generic-optics with 'capacityProviders' instead." #-}

-- | The capacity provider strategy to use by default for the cluster.
--
-- When creating a service or running a task on a cluster, if no capacity provider or launch type is specified then the default capacity provider strategy for the cluster is used.
-- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
--
-- /Note:/ Consider using 'defaultCapacityProviderStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccpDefaultCapacityProviderStrategy :: Lens.Lens' PutClusterCapacityProviders [CapacityProviderStrategyItem]
pccpDefaultCapacityProviderStrategy = Lens.lens (defaultCapacityProviderStrategy :: PutClusterCapacityProviders -> [CapacityProviderStrategyItem]) (\s a -> s {defaultCapacityProviderStrategy = a} :: PutClusterCapacityProviders)
{-# DEPRECATED pccpDefaultCapacityProviderStrategy "Use generic-lens or generic-optics with 'defaultCapacityProviderStrategy' instead." #-}

instance Lude.AWSRequest PutClusterCapacityProviders where
  type
    Rs PutClusterCapacityProviders =
      PutClusterCapacityProvidersResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutClusterCapacityProvidersResponse'
            Lude.<$> (x Lude..?> "cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutClusterCapacityProviders where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.PutClusterCapacityProviders" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutClusterCapacityProviders where
  toJSON PutClusterCapacityProviders' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("cluster" Lude..= cluster),
            Lude.Just ("capacityProviders" Lude..= capacityProviders),
            Lude.Just
              ( "defaultCapacityProviderStrategy"
                  Lude..= defaultCapacityProviderStrategy
              )
          ]
      )

instance Lude.ToPath PutClusterCapacityProviders where
  toPath = Lude.const "/"

instance Lude.ToQuery PutClusterCapacityProviders where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutClusterCapacityProvidersResponse' smart constructor.
data PutClusterCapacityProvidersResponse = PutClusterCapacityProvidersResponse'
  { cluster ::
      Lude.Maybe Cluster,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutClusterCapacityProvidersResponse' with the minimum fields required to make a request.
--
-- * 'cluster' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkPutClusterCapacityProvidersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutClusterCapacityProvidersResponse
mkPutClusterCapacityProvidersResponse pResponseStatus_ =
  PutClusterCapacityProvidersResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccprsCluster :: Lens.Lens' PutClusterCapacityProvidersResponse (Lude.Maybe Cluster)
pccprsCluster = Lens.lens (cluster :: PutClusterCapacityProvidersResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: PutClusterCapacityProvidersResponse)
{-# DEPRECATED pccprsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pccprsResponseStatus :: Lens.Lens' PutClusterCapacityProvidersResponse Lude.Int
pccprsResponseStatus = Lens.lens (responseStatus :: PutClusterCapacityProvidersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutClusterCapacityProvidersResponse)
{-# DEPRECATED pccprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
