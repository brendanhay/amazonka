{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListAllowedNodeTypeModifications (..)
    , mkListAllowedNodeTypeModifications
    -- ** Request lenses
    , lantmCacheClusterId
    , lantmReplicationGroupId

    -- * Destructuring the response
    , ListAllowedNodeTypeModificationsResponse (..)
    , mkListAllowedNodeTypeModificationsResponse
    -- ** Response lenses
    , lantmrrsScaleDownModifications
    , lantmrrsScaleUpModifications
    , lantmrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input parameters for the @ListAllowedNodeTypeModifications@ operation.
--
-- /See:/ 'mkListAllowedNodeTypeModifications' smart constructor.
data ListAllowedNodeTypeModifications = ListAllowedNodeTypeModifications'
  { cacheClusterId :: Core.Maybe Core.Text
    -- ^ The name of the cluster you want to scale up to a larger node instanced type. ElastiCache uses the cluster id to identify the current node type of this cluster and from that to create a list of node types you can scale up to.
--
-- /Important:/ You must provide a value for either the @CacheClusterId@ or the @ReplicationGroupId@ .
  , replicationGroupId :: Core.Maybe Core.Text
    -- ^ The name of the replication group want to scale up to a larger node type. ElastiCache uses the replication group id to identify the current node type being used by this replication group, and from that to create a list of node types you can scale up to.
--
-- /Important:/ You must provide a value for either the @CacheClusterId@ or the @ReplicationGroupId@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAllowedNodeTypeModifications' value with any optional fields omitted.
mkListAllowedNodeTypeModifications
    :: ListAllowedNodeTypeModifications
mkListAllowedNodeTypeModifications
  = ListAllowedNodeTypeModifications'{cacheClusterId = Core.Nothing,
                                      replicationGroupId = Core.Nothing}

-- | The name of the cluster you want to scale up to a larger node instanced type. ElastiCache uses the cluster id to identify the current node type of this cluster and from that to create a list of node types you can scale up to.
--
-- /Important:/ You must provide a value for either the @CacheClusterId@ or the @ReplicationGroupId@ .
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lantmCacheClusterId :: Lens.Lens' ListAllowedNodeTypeModifications (Core.Maybe Core.Text)
lantmCacheClusterId = Lens.field @"cacheClusterId"
{-# INLINEABLE lantmCacheClusterId #-}
{-# DEPRECATED cacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead"  #-}

-- | The name of the replication group want to scale up to a larger node type. ElastiCache uses the replication group id to identify the current node type being used by this replication group, and from that to create a list of node types you can scale up to.
--
-- /Important:/ You must provide a value for either the @CacheClusterId@ or the @ReplicationGroupId@ .
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lantmReplicationGroupId :: Lens.Lens' ListAllowedNodeTypeModifications (Core.Maybe Core.Text)
lantmReplicationGroupId = Lens.field @"replicationGroupId"
{-# INLINEABLE lantmReplicationGroupId #-}
{-# DEPRECATED replicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead"  #-}

instance Core.ToQuery ListAllowedNodeTypeModifications where
        toQuery ListAllowedNodeTypeModifications{..}
          = Core.toQueryPair "Action"
              ("ListAllowedNodeTypeModifications" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CacheClusterId")
                cacheClusterId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ReplicationGroupId")
                replicationGroupId

instance Core.ToHeaders ListAllowedNodeTypeModifications where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListAllowedNodeTypeModifications where
        type Rs ListAllowedNodeTypeModifications =
             ListAllowedNodeTypeModificationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper
              "ListAllowedNodeTypeModificationsResult"
              (\ s h x ->
                 ListAllowedNodeTypeModificationsResponse' Core.<$>
                   (x Core..@? "ScaleDownModifications" Core..<@>
                      Core.parseXMLList "member")
                     Core.<*>
                     x Core..@? "ScaleUpModifications" Core..<@>
                       Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the allowed node types you can use to modify your cluster or replication group.
--
-- /See:/ 'mkListAllowedNodeTypeModificationsResponse' smart constructor.
data ListAllowedNodeTypeModificationsResponse = ListAllowedNodeTypeModificationsResponse'
  { scaleDownModifications :: Core.Maybe [Core.Text]
    -- ^ A string list, each element of which specifies a cache node type which you can use to scale your cluster or replication group. When scaling down a Redis cluster or replication group using ModifyCacheCluster or ModifyReplicationGroup, use a value from this list for the CacheNodeType parameter. 
  , scaleUpModifications :: Core.Maybe [Core.Text]
    -- ^ A string list, each element of which specifies a cache node type which you can use to scale your cluster or replication group.
--
-- When scaling up a Redis cluster or replication group using @ModifyCacheCluster@ or @ModifyReplicationGroup@ , use a value from this list for the @CacheNodeType@ parameter.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAllowedNodeTypeModificationsResponse' value with any optional fields omitted.
mkListAllowedNodeTypeModificationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAllowedNodeTypeModificationsResponse
mkListAllowedNodeTypeModificationsResponse responseStatus
  = ListAllowedNodeTypeModificationsResponse'{scaleDownModifications
                                                = Core.Nothing,
                                              scaleUpModifications = Core.Nothing, responseStatus}

-- | A string list, each element of which specifies a cache node type which you can use to scale your cluster or replication group. When scaling down a Redis cluster or replication group using ModifyCacheCluster or ModifyReplicationGroup, use a value from this list for the CacheNodeType parameter. 
--
-- /Note:/ Consider using 'scaleDownModifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lantmrrsScaleDownModifications :: Lens.Lens' ListAllowedNodeTypeModificationsResponse (Core.Maybe [Core.Text])
lantmrrsScaleDownModifications = Lens.field @"scaleDownModifications"
{-# INLINEABLE lantmrrsScaleDownModifications #-}
{-# DEPRECATED scaleDownModifications "Use generic-lens or generic-optics with 'scaleDownModifications' instead"  #-}

-- | A string list, each element of which specifies a cache node type which you can use to scale your cluster or replication group.
--
-- When scaling up a Redis cluster or replication group using @ModifyCacheCluster@ or @ModifyReplicationGroup@ , use a value from this list for the @CacheNodeType@ parameter.
--
-- /Note:/ Consider using 'scaleUpModifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lantmrrsScaleUpModifications :: Lens.Lens' ListAllowedNodeTypeModificationsResponse (Core.Maybe [Core.Text])
lantmrrsScaleUpModifications = Lens.field @"scaleUpModifications"
{-# INLINEABLE lantmrrsScaleUpModifications #-}
{-# DEPRECATED scaleUpModifications "Use generic-lens or generic-optics with 'scaleUpModifications' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lantmrrsResponseStatus :: Lens.Lens' ListAllowedNodeTypeModificationsResponse Core.Int
lantmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lantmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
