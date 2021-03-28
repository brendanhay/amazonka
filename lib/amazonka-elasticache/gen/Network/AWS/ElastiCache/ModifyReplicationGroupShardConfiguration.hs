{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ModifyReplicationGroupShardConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a replication group's shards (node groups) by allowing you to add shards, remove shards, or rebalance the keyspaces among exisiting shards.
module Network.AWS.ElastiCache.ModifyReplicationGroupShardConfiguration
    (
    -- * Creating a request
      ModifyReplicationGroupShardConfiguration (..)
    , mkModifyReplicationGroupShardConfiguration
    -- ** Request lenses
    , mrgscReplicationGroupId
    , mrgscNodeGroupCount
    , mrgscApplyImmediately
    , mrgscNodeGroupsToRemove
    , mrgscNodeGroupsToRetain
    , mrgscReshardingConfiguration

    -- * Destructuring the response
    , ModifyReplicationGroupShardConfigurationResponse (..)
    , mkModifyReplicationGroupShardConfigurationResponse
    -- ** Response lenses
    , mrgscrrsReplicationGroup
    , mrgscrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a @ModifyReplicationGroupShardConfiguration@ operation.
--
-- /See:/ 'mkModifyReplicationGroupShardConfiguration' smart constructor.
data ModifyReplicationGroupShardConfiguration = ModifyReplicationGroupShardConfiguration'
  { replicationGroupId :: Core.Text
    -- ^ The name of the Redis (cluster mode enabled) cluster (replication group) on which the shards are to be configured.
  , nodeGroupCount :: Core.Int
    -- ^ The number of node groups (shards) that results from the modification of the shard configuration.
  , applyImmediately :: Core.Bool
    -- ^ Indicates that the shard reconfiguration process begins immediately. At present, the only permitted value for this parameter is @true@ .
--
-- Value: true
  , nodeGroupsToRemove :: Core.Maybe [Types.AllowedNodeGroupId]
    -- ^ If the value of @NodeGroupCount@ is less than the current number of node groups (shards), then either @NodeGroupsToRemove@ or @NodeGroupsToRetain@ is required. @NodeGroupsToRemove@ is a list of @NodeGroupId@ s to remove from the cluster.
--
-- ElastiCache for Redis will attempt to remove all node groups listed by @NodeGroupsToRemove@ from the cluster.
  , nodeGroupsToRetain :: Core.Maybe [Types.AllowedNodeGroupId]
    -- ^ If the value of @NodeGroupCount@ is less than the current number of node groups (shards), then either @NodeGroupsToRemove@ or @NodeGroupsToRetain@ is required. @NodeGroupsToRetain@ is a list of @NodeGroupId@ s to retain in the cluster.
--
-- ElastiCache for Redis will attempt to remove all node groups except those listed by @NodeGroupsToRetain@ from the cluster.
  , reshardingConfiguration :: Core.Maybe [Types.ReshardingConfiguration]
    -- ^ Specifies the preferred availability zones for each node group in the cluster. If the value of @NodeGroupCount@ is greater than the current number of node groups (shards), you can use this parameter to specify the preferred availability zones of the cluster's shards. If you omit this parameter ElastiCache selects availability zones for you.
--
-- You can specify this parameter only if the value of @NodeGroupCount@ is greater than the current number of node groups (shards).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyReplicationGroupShardConfiguration' value with any optional fields omitted.
mkModifyReplicationGroupShardConfiguration
    :: Core.Text -- ^ 'replicationGroupId'
    -> Core.Int -- ^ 'nodeGroupCount'
    -> Core.Bool -- ^ 'applyImmediately'
    -> ModifyReplicationGroupShardConfiguration
mkModifyReplicationGroupShardConfiguration replicationGroupId
  nodeGroupCount applyImmediately
  = ModifyReplicationGroupShardConfiguration'{replicationGroupId,
                                              nodeGroupCount, applyImmediately,
                                              nodeGroupsToRemove = Core.Nothing,
                                              nodeGroupsToRetain = Core.Nothing,
                                              reshardingConfiguration = Core.Nothing}

-- | The name of the Redis (cluster mode enabled) cluster (replication group) on which the shards are to be configured.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscReplicationGroupId :: Lens.Lens' ModifyReplicationGroupShardConfiguration Core.Text
mrgscReplicationGroupId = Lens.field @"replicationGroupId"
{-# INLINEABLE mrgscReplicationGroupId #-}
{-# DEPRECATED replicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead"  #-}

-- | The number of node groups (shards) that results from the modification of the shard configuration.
--
-- /Note:/ Consider using 'nodeGroupCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscNodeGroupCount :: Lens.Lens' ModifyReplicationGroupShardConfiguration Core.Int
mrgscNodeGroupCount = Lens.field @"nodeGroupCount"
{-# INLINEABLE mrgscNodeGroupCount #-}
{-# DEPRECATED nodeGroupCount "Use generic-lens or generic-optics with 'nodeGroupCount' instead"  #-}

-- | Indicates that the shard reconfiguration process begins immediately. At present, the only permitted value for this parameter is @true@ .
--
-- Value: true
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscApplyImmediately :: Lens.Lens' ModifyReplicationGroupShardConfiguration Core.Bool
mrgscApplyImmediately = Lens.field @"applyImmediately"
{-# INLINEABLE mrgscApplyImmediately #-}
{-# DEPRECATED applyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead"  #-}

-- | If the value of @NodeGroupCount@ is less than the current number of node groups (shards), then either @NodeGroupsToRemove@ or @NodeGroupsToRetain@ is required. @NodeGroupsToRemove@ is a list of @NodeGroupId@ s to remove from the cluster.
--
-- ElastiCache for Redis will attempt to remove all node groups listed by @NodeGroupsToRemove@ from the cluster.
--
-- /Note:/ Consider using 'nodeGroupsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscNodeGroupsToRemove :: Lens.Lens' ModifyReplicationGroupShardConfiguration (Core.Maybe [Types.AllowedNodeGroupId])
mrgscNodeGroupsToRemove = Lens.field @"nodeGroupsToRemove"
{-# INLINEABLE mrgscNodeGroupsToRemove #-}
{-# DEPRECATED nodeGroupsToRemove "Use generic-lens or generic-optics with 'nodeGroupsToRemove' instead"  #-}

-- | If the value of @NodeGroupCount@ is less than the current number of node groups (shards), then either @NodeGroupsToRemove@ or @NodeGroupsToRetain@ is required. @NodeGroupsToRetain@ is a list of @NodeGroupId@ s to retain in the cluster.
--
-- ElastiCache for Redis will attempt to remove all node groups except those listed by @NodeGroupsToRetain@ from the cluster.
--
-- /Note:/ Consider using 'nodeGroupsToRetain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscNodeGroupsToRetain :: Lens.Lens' ModifyReplicationGroupShardConfiguration (Core.Maybe [Types.AllowedNodeGroupId])
mrgscNodeGroupsToRetain = Lens.field @"nodeGroupsToRetain"
{-# INLINEABLE mrgscNodeGroupsToRetain #-}
{-# DEPRECATED nodeGroupsToRetain "Use generic-lens or generic-optics with 'nodeGroupsToRetain' instead"  #-}

-- | Specifies the preferred availability zones for each node group in the cluster. If the value of @NodeGroupCount@ is greater than the current number of node groups (shards), you can use this parameter to specify the preferred availability zones of the cluster's shards. If you omit this parameter ElastiCache selects availability zones for you.
--
-- You can specify this parameter only if the value of @NodeGroupCount@ is greater than the current number of node groups (shards).
--
-- /Note:/ Consider using 'reshardingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscReshardingConfiguration :: Lens.Lens' ModifyReplicationGroupShardConfiguration (Core.Maybe [Types.ReshardingConfiguration])
mrgscReshardingConfiguration = Lens.field @"reshardingConfiguration"
{-# INLINEABLE mrgscReshardingConfiguration #-}
{-# DEPRECATED reshardingConfiguration "Use generic-lens or generic-optics with 'reshardingConfiguration' instead"  #-}

instance Core.ToQuery ModifyReplicationGroupShardConfiguration
         where
        toQuery ModifyReplicationGroupShardConfiguration{..}
          = Core.toQueryPair "Action"
              ("ModifyReplicationGroupShardConfiguration" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.toQueryPair "ReplicationGroupId" replicationGroupId
              Core.<> Core.toQueryPair "NodeGroupCount" nodeGroupCount
              Core.<> Core.toQueryPair "ApplyImmediately" applyImmediately
              Core.<>
              Core.toQueryPair "NodeGroupsToRemove"
                (Core.maybe Core.mempty (Core.toQueryList "NodeGroupToRemove")
                   nodeGroupsToRemove)
              Core.<>
              Core.toQueryPair "NodeGroupsToRetain"
                (Core.maybe Core.mempty (Core.toQueryList "NodeGroupToRetain")
                   nodeGroupsToRetain)
              Core.<>
              Core.toQueryPair "ReshardingConfiguration"
                (Core.maybe Core.mempty
                   (Core.toQueryList "ReshardingConfiguration")
                   reshardingConfiguration)

instance Core.ToHeaders ModifyReplicationGroupShardConfiguration
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyReplicationGroupShardConfiguration
         where
        type Rs ModifyReplicationGroupShardConfiguration =
             ModifyReplicationGroupShardConfigurationResponse
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
              "ModifyReplicationGroupShardConfigurationResult"
              (\ s h x ->
                 ModifyReplicationGroupShardConfigurationResponse' Core.<$>
                   (x Core..@? "ReplicationGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyReplicationGroupShardConfigurationResponse' smart constructor.
data ModifyReplicationGroupShardConfigurationResponse = ModifyReplicationGroupShardConfigurationResponse'
  { replicationGroup :: Core.Maybe Types.ReplicationGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyReplicationGroupShardConfigurationResponse' value with any optional fields omitted.
mkModifyReplicationGroupShardConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyReplicationGroupShardConfigurationResponse
mkModifyReplicationGroupShardConfigurationResponse responseStatus
  = ModifyReplicationGroupShardConfigurationResponse'{replicationGroup
                                                        = Core.Nothing,
                                                      responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscrrsReplicationGroup :: Lens.Lens' ModifyReplicationGroupShardConfigurationResponse (Core.Maybe Types.ReplicationGroup)
mrgscrrsReplicationGroup = Lens.field @"replicationGroup"
{-# INLINEABLE mrgscrrsReplicationGroup #-}
{-# DEPRECATED replicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscrrsResponseStatus :: Lens.Lens' ModifyReplicationGroupShardConfigurationResponse Core.Int
mrgscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mrgscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
