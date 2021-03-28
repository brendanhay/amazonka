{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DecreaseReplicaCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Dynamically decreases the number of replicas in a Redis (cluster mode disabled) replication group or the number of replica nodes in one or more node groups (shards) of a Redis (cluster mode enabled) replication group. This operation is performed with no cluster down time.
module Network.AWS.ElastiCache.DecreaseReplicaCount
    (
    -- * Creating a request
      DecreaseReplicaCount (..)
    , mkDecreaseReplicaCount
    -- ** Request lenses
    , drcReplicationGroupId
    , drcApplyImmediately
    , drcNewReplicaCount
    , drcReplicaConfiguration
    , drcReplicasToRemove

    -- * Destructuring the response
    , DecreaseReplicaCountResponse (..)
    , mkDecreaseReplicaCountResponse
    -- ** Response lenses
    , drcrrsReplicationGroup
    , drcrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDecreaseReplicaCount' smart constructor.
data DecreaseReplicaCount = DecreaseReplicaCount'
  { replicationGroupId :: Core.Text
    -- ^ The id of the replication group from which you want to remove replica nodes.
  , applyImmediately :: Core.Bool
    -- ^ If @True@ , the number of replica nodes is decreased immediately. @ApplyImmediately=False@ is not currently supported.
  , newReplicaCount :: Core.Maybe Core.Int
    -- ^ The number of read replica nodes you want at the completion of this operation. For Redis (cluster mode disabled) replication groups, this is the number of replica nodes in the replication group. For Redis (cluster mode enabled) replication groups, this is the number of replica nodes in each of the replication group's node groups.
--
-- The minimum number of replicas in a shard or replication group is:
--
--     * Redis (cluster mode disabled)
--
--     * If Multi-AZ is enabled: 1
--
--
--     * If Multi-AZ is not enabled: 0
--
--
--
--
--     * Redis (cluster mode enabled): 0 (though you will not be able to failover to a replica if your primary node fails)
--
--
  , replicaConfiguration :: Core.Maybe [Types.ConfigureShard]
    -- ^ A list of @ConfigureShard@ objects that can be used to configure each shard in a Redis (cluster mode enabled) replication group. The @ConfigureShard@ has three members: @NewReplicaCount@ , @NodeGroupId@ , and @PreferredAvailabilityZones@ .
  , replicasToRemove :: Core.Maybe [Core.Text]
    -- ^ A list of the node ids to remove from the replication group or node group (shard).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DecreaseReplicaCount' value with any optional fields omitted.
mkDecreaseReplicaCount
    :: Core.Text -- ^ 'replicationGroupId'
    -> Core.Bool -- ^ 'applyImmediately'
    -> DecreaseReplicaCount
mkDecreaseReplicaCount replicationGroupId applyImmediately
  = DecreaseReplicaCount'{replicationGroupId, applyImmediately,
                          newReplicaCount = Core.Nothing,
                          replicaConfiguration = Core.Nothing,
                          replicasToRemove = Core.Nothing}

-- | The id of the replication group from which you want to remove replica nodes.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcReplicationGroupId :: Lens.Lens' DecreaseReplicaCount Core.Text
drcReplicationGroupId = Lens.field @"replicationGroupId"
{-# INLINEABLE drcReplicationGroupId #-}
{-# DEPRECATED replicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead"  #-}

-- | If @True@ , the number of replica nodes is decreased immediately. @ApplyImmediately=False@ is not currently supported.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcApplyImmediately :: Lens.Lens' DecreaseReplicaCount Core.Bool
drcApplyImmediately = Lens.field @"applyImmediately"
{-# INLINEABLE drcApplyImmediately #-}
{-# DEPRECATED applyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead"  #-}

-- | The number of read replica nodes you want at the completion of this operation. For Redis (cluster mode disabled) replication groups, this is the number of replica nodes in the replication group. For Redis (cluster mode enabled) replication groups, this is the number of replica nodes in each of the replication group's node groups.
--
-- The minimum number of replicas in a shard or replication group is:
--
--     * Redis (cluster mode disabled)
--
--     * If Multi-AZ is enabled: 1
--
--
--     * If Multi-AZ is not enabled: 0
--
--
--
--
--     * Redis (cluster mode enabled): 0 (though you will not be able to failover to a replica if your primary node fails)
--
--
--
-- /Note:/ Consider using 'newReplicaCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcNewReplicaCount :: Lens.Lens' DecreaseReplicaCount (Core.Maybe Core.Int)
drcNewReplicaCount = Lens.field @"newReplicaCount"
{-# INLINEABLE drcNewReplicaCount #-}
{-# DEPRECATED newReplicaCount "Use generic-lens or generic-optics with 'newReplicaCount' instead"  #-}

-- | A list of @ConfigureShard@ objects that can be used to configure each shard in a Redis (cluster mode enabled) replication group. The @ConfigureShard@ has three members: @NewReplicaCount@ , @NodeGroupId@ , and @PreferredAvailabilityZones@ .
--
-- /Note:/ Consider using 'replicaConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcReplicaConfiguration :: Lens.Lens' DecreaseReplicaCount (Core.Maybe [Types.ConfigureShard])
drcReplicaConfiguration = Lens.field @"replicaConfiguration"
{-# INLINEABLE drcReplicaConfiguration #-}
{-# DEPRECATED replicaConfiguration "Use generic-lens or generic-optics with 'replicaConfiguration' instead"  #-}

-- | A list of the node ids to remove from the replication group or node group (shard).
--
-- /Note:/ Consider using 'replicasToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcReplicasToRemove :: Lens.Lens' DecreaseReplicaCount (Core.Maybe [Core.Text])
drcReplicasToRemove = Lens.field @"replicasToRemove"
{-# INLINEABLE drcReplicasToRemove #-}
{-# DEPRECATED replicasToRemove "Use generic-lens or generic-optics with 'replicasToRemove' instead"  #-}

instance Core.ToQuery DecreaseReplicaCount where
        toQuery DecreaseReplicaCount{..}
          = Core.toQueryPair "Action" ("DecreaseReplicaCount" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.toQueryPair "ReplicationGroupId" replicationGroupId
              Core.<> Core.toQueryPair "ApplyImmediately" applyImmediately
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NewReplicaCount")
                newReplicaCount
              Core.<>
              Core.toQueryPair "ReplicaConfiguration"
                (Core.maybe Core.mempty (Core.toQueryList "ConfigureShard")
                   replicaConfiguration)
              Core.<>
              Core.toQueryPair "ReplicasToRemove"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   replicasToRemove)

instance Core.ToHeaders DecreaseReplicaCount where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DecreaseReplicaCount where
        type Rs DecreaseReplicaCount = DecreaseReplicaCountResponse
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
          = Response.receiveXMLWrapper "DecreaseReplicaCountResult"
              (\ s h x ->
                 DecreaseReplicaCountResponse' Core.<$>
                   (x Core..@? "ReplicationGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDecreaseReplicaCountResponse' smart constructor.
data DecreaseReplicaCountResponse = DecreaseReplicaCountResponse'
  { replicationGroup :: Core.Maybe Types.ReplicationGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DecreaseReplicaCountResponse' value with any optional fields omitted.
mkDecreaseReplicaCountResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DecreaseReplicaCountResponse
mkDecreaseReplicaCountResponse responseStatus
  = DecreaseReplicaCountResponse'{replicationGroup = Core.Nothing,
                                  responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrrsReplicationGroup :: Lens.Lens' DecreaseReplicaCountResponse (Core.Maybe Types.ReplicationGroup)
drcrrsReplicationGroup = Lens.field @"replicationGroup"
{-# INLINEABLE drcrrsReplicationGroup #-}
{-# DEPRECATED replicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrrsResponseStatus :: Lens.Lens' DecreaseReplicaCountResponse Core.Int
drcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
