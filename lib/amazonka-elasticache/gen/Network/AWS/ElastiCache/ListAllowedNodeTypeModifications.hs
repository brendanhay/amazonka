{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ListAllowedNodeTypeModifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all available node types that you can scale your Redis cluster's or replication group's current node type.
--
-- When you use the @ModifyCacheCluster@ or @ModifyReplicationGroup@ operations to scale your cluster or replication group, the value of the @CacheNodeType@ parameter must be one of the node types returned by this operation.
module Network.AWS.ElastiCache.ListAllowedNodeTypeModifications
  ( -- * Creating a request
    ListAllowedNodeTypeModifications (..),
    mkListAllowedNodeTypeModifications,

    -- ** Request lenses
    lantmCacheClusterId,
    lantmReplicationGroupId,

    -- * Destructuring the response
    ListAllowedNodeTypeModificationsResponse (..),
    mkListAllowedNodeTypeModificationsResponse,

    -- ** Response lenses
    lantmrsScaleUpModifications,
    lantmrsScaleDownModifications,
    lantmrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input parameters for the @ListAllowedNodeTypeModifications@ operation.
--
-- /See:/ 'mkListAllowedNodeTypeModifications' smart constructor.
data ListAllowedNodeTypeModifications = ListAllowedNodeTypeModifications'
  { -- | The name of the cluster you want to scale up to a larger node instanced type. ElastiCache uses the cluster id to identify the current node type of this cluster and from that to create a list of node types you can scale up to.
    --
    -- /Important:/ You must provide a value for either the @CacheClusterId@ or the @ReplicationGroupId@ .
    cacheClusterId :: Lude.Maybe Lude.Text,
    -- | The name of the replication group want to scale up to a larger node type. ElastiCache uses the replication group id to identify the current node type being used by this replication group, and from that to create a list of node types you can scale up to.
    --
    -- /Important:/ You must provide a value for either the @CacheClusterId@ or the @ReplicationGroupId@ .
    replicationGroupId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAllowedNodeTypeModifications' with the minimum fields required to make a request.
--
-- * 'cacheClusterId' - The name of the cluster you want to scale up to a larger node instanced type. ElastiCache uses the cluster id to identify the current node type of this cluster and from that to create a list of node types you can scale up to.
--
-- /Important:/ You must provide a value for either the @CacheClusterId@ or the @ReplicationGroupId@ .
-- * 'replicationGroupId' - The name of the replication group want to scale up to a larger node type. ElastiCache uses the replication group id to identify the current node type being used by this replication group, and from that to create a list of node types you can scale up to.
--
-- /Important:/ You must provide a value for either the @CacheClusterId@ or the @ReplicationGroupId@ .
mkListAllowedNodeTypeModifications ::
  ListAllowedNodeTypeModifications
mkListAllowedNodeTypeModifications =
  ListAllowedNodeTypeModifications'
    { cacheClusterId = Lude.Nothing,
      replicationGroupId = Lude.Nothing
    }

-- | The name of the cluster you want to scale up to a larger node instanced type. ElastiCache uses the cluster id to identify the current node type of this cluster and from that to create a list of node types you can scale up to.
--
-- /Important:/ You must provide a value for either the @CacheClusterId@ or the @ReplicationGroupId@ .
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lantmCacheClusterId :: Lens.Lens' ListAllowedNodeTypeModifications (Lude.Maybe Lude.Text)
lantmCacheClusterId = Lens.lens (cacheClusterId :: ListAllowedNodeTypeModifications -> Lude.Maybe Lude.Text) (\s a -> s {cacheClusterId = a} :: ListAllowedNodeTypeModifications)
{-# DEPRECATED lantmCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | The name of the replication group want to scale up to a larger node type. ElastiCache uses the replication group id to identify the current node type being used by this replication group, and from that to create a list of node types you can scale up to.
--
-- /Important:/ You must provide a value for either the @CacheClusterId@ or the @ReplicationGroupId@ .
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lantmReplicationGroupId :: Lens.Lens' ListAllowedNodeTypeModifications (Lude.Maybe Lude.Text)
lantmReplicationGroupId = Lens.lens (replicationGroupId :: ListAllowedNodeTypeModifications -> Lude.Maybe Lude.Text) (\s a -> s {replicationGroupId = a} :: ListAllowedNodeTypeModifications)
{-# DEPRECATED lantmReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

instance Lude.AWSRequest ListAllowedNodeTypeModifications where
  type
    Rs ListAllowedNodeTypeModifications =
      ListAllowedNodeTypeModificationsResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "ListAllowedNodeTypeModificationsResult"
      ( \s h x ->
          ListAllowedNodeTypeModificationsResponse'
            Lude.<$> ( x Lude..@? "ScaleUpModifications" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> ( x Lude..@? "ScaleDownModifications" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAllowedNodeTypeModifications where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListAllowedNodeTypeModifications where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAllowedNodeTypeModifications where
  toQuery ListAllowedNodeTypeModifications' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ListAllowedNodeTypeModifications" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheClusterId" Lude.=: cacheClusterId,
        "ReplicationGroupId" Lude.=: replicationGroupId
      ]

-- | Represents the allowed node types you can use to modify your cluster or replication group.
--
-- /See:/ 'mkListAllowedNodeTypeModificationsResponse' smart constructor.
data ListAllowedNodeTypeModificationsResponse = ListAllowedNodeTypeModificationsResponse'
  { -- | A string list, each element of which specifies a cache node type which you can use to scale your cluster or replication group.
    --
    -- When scaling up a Redis cluster or replication group using @ModifyCacheCluster@ or @ModifyReplicationGroup@ , use a value from this list for the @CacheNodeType@ parameter.
    scaleUpModifications :: Lude.Maybe [Lude.Text],
    -- | A string list, each element of which specifies a cache node type which you can use to scale your cluster or replication group. When scaling down a Redis cluster or replication group using ModifyCacheCluster or ModifyReplicationGroup, use a value from this list for the CacheNodeType parameter.
    scaleDownModifications :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAllowedNodeTypeModificationsResponse' with the minimum fields required to make a request.
--
-- * 'scaleUpModifications' - A string list, each element of which specifies a cache node type which you can use to scale your cluster or replication group.
--
-- When scaling up a Redis cluster or replication group using @ModifyCacheCluster@ or @ModifyReplicationGroup@ , use a value from this list for the @CacheNodeType@ parameter.
-- * 'scaleDownModifications' - A string list, each element of which specifies a cache node type which you can use to scale your cluster or replication group. When scaling down a Redis cluster or replication group using ModifyCacheCluster or ModifyReplicationGroup, use a value from this list for the CacheNodeType parameter.
-- * 'responseStatus' - The response status code.
mkListAllowedNodeTypeModificationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAllowedNodeTypeModificationsResponse
mkListAllowedNodeTypeModificationsResponse pResponseStatus_ =
  ListAllowedNodeTypeModificationsResponse'
    { scaleUpModifications =
        Lude.Nothing,
      scaleDownModifications = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A string list, each element of which specifies a cache node type which you can use to scale your cluster or replication group.
--
-- When scaling up a Redis cluster or replication group using @ModifyCacheCluster@ or @ModifyReplicationGroup@ , use a value from this list for the @CacheNodeType@ parameter.
--
-- /Note:/ Consider using 'scaleUpModifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lantmrsScaleUpModifications :: Lens.Lens' ListAllowedNodeTypeModificationsResponse (Lude.Maybe [Lude.Text])
lantmrsScaleUpModifications = Lens.lens (scaleUpModifications :: ListAllowedNodeTypeModificationsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {scaleUpModifications = a} :: ListAllowedNodeTypeModificationsResponse)
{-# DEPRECATED lantmrsScaleUpModifications "Use generic-lens or generic-optics with 'scaleUpModifications' instead." #-}

-- | A string list, each element of which specifies a cache node type which you can use to scale your cluster or replication group. When scaling down a Redis cluster or replication group using ModifyCacheCluster or ModifyReplicationGroup, use a value from this list for the CacheNodeType parameter.
--
-- /Note:/ Consider using 'scaleDownModifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lantmrsScaleDownModifications :: Lens.Lens' ListAllowedNodeTypeModificationsResponse (Lude.Maybe [Lude.Text])
lantmrsScaleDownModifications = Lens.lens (scaleDownModifications :: ListAllowedNodeTypeModificationsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {scaleDownModifications = a} :: ListAllowedNodeTypeModificationsResponse)
{-# DEPRECATED lantmrsScaleDownModifications "Use generic-lens or generic-optics with 'scaleDownModifications' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lantmrsResponseStatus :: Lens.Lens' ListAllowedNodeTypeModificationsResponse Lude.Int
lantmrsResponseStatus = Lens.lens (responseStatus :: ListAllowedNodeTypeModificationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAllowedNodeTypeModificationsResponse)
{-# DEPRECATED lantmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
