{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ModifyReplicationGroupShardConfiguration (..),
    mkModifyReplicationGroupShardConfiguration,

    -- ** Request lenses
    mrgscReplicationGroupId,
    mrgscNodeGroupCount,
    mrgscApplyImmediately,
    mrgscNodeGroupsToRemove,
    mrgscNodeGroupsToRetain,
    mrgscReshardingConfiguration,

    -- * Destructuring the response
    ModifyReplicationGroupShardConfigurationResponse (..),
    mkModifyReplicationGroupShardConfigurationResponse,

    -- ** Response lenses
    mrgscrrsReplicationGroup,
    mrgscrrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a @ModifyReplicationGroupShardConfiguration@ operation.
--
-- /See:/ 'mkModifyReplicationGroupShardConfiguration' smart constructor.
data ModifyReplicationGroupShardConfiguration = ModifyReplicationGroupShardConfiguration'
  { -- | The name of the Redis (cluster mode enabled) cluster (replication group) on which the shards are to be configured.
    replicationGroupId :: Types.String,
    -- | The number of node groups (shards) that results from the modification of the shard configuration.
    nodeGroupCount :: Core.Int,
    -- | Indicates that the shard reconfiguration process begins immediately. At present, the only permitted value for this parameter is @true@ .
    --
    -- Value: true
    applyImmediately :: Core.Bool,
    -- | If the value of @NodeGroupCount@ is less than the current number of node groups (shards), then either @NodeGroupsToRemove@ or @NodeGroupsToRetain@ is required. @NodeGroupsToRemove@ is a list of @NodeGroupId@ s to remove from the cluster.
    --
    -- ElastiCache for Redis will attempt to remove all node groups listed by @NodeGroupsToRemove@ from the cluster.
    nodeGroupsToRemove :: Core.Maybe [Types.AllowedNodeGroupId],
    -- | If the value of @NodeGroupCount@ is less than the current number of node groups (shards), then either @NodeGroupsToRemove@ or @NodeGroupsToRetain@ is required. @NodeGroupsToRetain@ is a list of @NodeGroupId@ s to retain in the cluster.
    --
    -- ElastiCache for Redis will attempt to remove all node groups except those listed by @NodeGroupsToRetain@ from the cluster.
    nodeGroupsToRetain :: Core.Maybe [Types.AllowedNodeGroupId],
    -- | Specifies the preferred availability zones for each node group in the cluster. If the value of @NodeGroupCount@ is greater than the current number of node groups (shards), you can use this parameter to specify the preferred availability zones of the cluster's shards. If you omit this parameter ElastiCache selects availability zones for you.
    --
    -- You can specify this parameter only if the value of @NodeGroupCount@ is greater than the current number of node groups (shards).
    reshardingConfiguration :: Core.Maybe [Types.ReshardingConfiguration]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyReplicationGroupShardConfiguration' value with any optional fields omitted.
mkModifyReplicationGroupShardConfiguration ::
  -- | 'replicationGroupId'
  Types.String ->
  -- | 'nodeGroupCount'
  Core.Int ->
  -- | 'applyImmediately'
  Core.Bool ->
  ModifyReplicationGroupShardConfiguration
mkModifyReplicationGroupShardConfiguration
  replicationGroupId
  nodeGroupCount
  applyImmediately =
    ModifyReplicationGroupShardConfiguration'
      { replicationGroupId,
        nodeGroupCount,
        applyImmediately,
        nodeGroupsToRemove = Core.Nothing,
        nodeGroupsToRetain = Core.Nothing,
        reshardingConfiguration = Core.Nothing
      }

-- | The name of the Redis (cluster mode enabled) cluster (replication group) on which the shards are to be configured.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscReplicationGroupId :: Lens.Lens' ModifyReplicationGroupShardConfiguration Types.String
mrgscReplicationGroupId = Lens.field @"replicationGroupId"
{-# DEPRECATED mrgscReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | The number of node groups (shards) that results from the modification of the shard configuration.
--
-- /Note:/ Consider using 'nodeGroupCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscNodeGroupCount :: Lens.Lens' ModifyReplicationGroupShardConfiguration Core.Int
mrgscNodeGroupCount = Lens.field @"nodeGroupCount"
{-# DEPRECATED mrgscNodeGroupCount "Use generic-lens or generic-optics with 'nodeGroupCount' instead." #-}

-- | Indicates that the shard reconfiguration process begins immediately. At present, the only permitted value for this parameter is @true@ .
--
-- Value: true
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscApplyImmediately :: Lens.Lens' ModifyReplicationGroupShardConfiguration Core.Bool
mrgscApplyImmediately = Lens.field @"applyImmediately"
{-# DEPRECATED mrgscApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | If the value of @NodeGroupCount@ is less than the current number of node groups (shards), then either @NodeGroupsToRemove@ or @NodeGroupsToRetain@ is required. @NodeGroupsToRemove@ is a list of @NodeGroupId@ s to remove from the cluster.
--
-- ElastiCache for Redis will attempt to remove all node groups listed by @NodeGroupsToRemove@ from the cluster.
--
-- /Note:/ Consider using 'nodeGroupsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscNodeGroupsToRemove :: Lens.Lens' ModifyReplicationGroupShardConfiguration (Core.Maybe [Types.AllowedNodeGroupId])
mrgscNodeGroupsToRemove = Lens.field @"nodeGroupsToRemove"
{-# DEPRECATED mrgscNodeGroupsToRemove "Use generic-lens or generic-optics with 'nodeGroupsToRemove' instead." #-}

-- | If the value of @NodeGroupCount@ is less than the current number of node groups (shards), then either @NodeGroupsToRemove@ or @NodeGroupsToRetain@ is required. @NodeGroupsToRetain@ is a list of @NodeGroupId@ s to retain in the cluster.
--
-- ElastiCache for Redis will attempt to remove all node groups except those listed by @NodeGroupsToRetain@ from the cluster.
--
-- /Note:/ Consider using 'nodeGroupsToRetain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscNodeGroupsToRetain :: Lens.Lens' ModifyReplicationGroupShardConfiguration (Core.Maybe [Types.AllowedNodeGroupId])
mrgscNodeGroupsToRetain = Lens.field @"nodeGroupsToRetain"
{-# DEPRECATED mrgscNodeGroupsToRetain "Use generic-lens or generic-optics with 'nodeGroupsToRetain' instead." #-}

-- | Specifies the preferred availability zones for each node group in the cluster. If the value of @NodeGroupCount@ is greater than the current number of node groups (shards), you can use this parameter to specify the preferred availability zones of the cluster's shards. If you omit this parameter ElastiCache selects availability zones for you.
--
-- You can specify this parameter only if the value of @NodeGroupCount@ is greater than the current number of node groups (shards).
--
-- /Note:/ Consider using 'reshardingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscReshardingConfiguration :: Lens.Lens' ModifyReplicationGroupShardConfiguration (Core.Maybe [Types.ReshardingConfiguration])
mrgscReshardingConfiguration = Lens.field @"reshardingConfiguration"
{-# DEPRECATED mrgscReshardingConfiguration "Use generic-lens or generic-optics with 'reshardingConfiguration' instead." #-}

instance Core.AWSRequest ModifyReplicationGroupShardConfiguration where
  type
    Rs ModifyReplicationGroupShardConfiguration =
      ModifyReplicationGroupShardConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ModifyReplicationGroupShardConfiguration")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "ReplicationGroupId" replicationGroupId)
                Core.<> (Core.toQueryValue "NodeGroupCount" nodeGroupCount)
                Core.<> (Core.toQueryValue "ApplyImmediately" applyImmediately)
                Core.<> ( Core.toQueryValue
                            "NodeGroupsToRemove"
                            ( Core.toQueryList "NodeGroupToRemove"
                                Core.<$> nodeGroupsToRemove
                            )
                        )
                Core.<> ( Core.toQueryValue
                            "NodeGroupsToRetain"
                            ( Core.toQueryList "NodeGroupToRetain"
                                Core.<$> nodeGroupsToRetain
                            )
                        )
                Core.<> ( Core.toQueryValue
                            "ReshardingConfiguration"
                            ( Core.toQueryList "ReshardingConfiguration"
                                Core.<$> reshardingConfiguration
                            )
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyReplicationGroupShardConfigurationResult"
      ( \s h x ->
          ModifyReplicationGroupShardConfigurationResponse'
            Core.<$> (x Core..@? "ReplicationGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyReplicationGroupShardConfigurationResponse' smart constructor.
data ModifyReplicationGroupShardConfigurationResponse = ModifyReplicationGroupShardConfigurationResponse'
  { replicationGroup :: Core.Maybe Types.ReplicationGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ModifyReplicationGroupShardConfigurationResponse' value with any optional fields omitted.
mkModifyReplicationGroupShardConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyReplicationGroupShardConfigurationResponse
mkModifyReplicationGroupShardConfigurationResponse responseStatus =
  ModifyReplicationGroupShardConfigurationResponse'
    { replicationGroup =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscrrsReplicationGroup :: Lens.Lens' ModifyReplicationGroupShardConfigurationResponse (Core.Maybe Types.ReplicationGroup)
mrgscrrsReplicationGroup = Lens.field @"replicationGroup"
{-# DEPRECATED mrgscrrsReplicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscrrsResponseStatus :: Lens.Lens' ModifyReplicationGroupShardConfigurationResponse Core.Int
mrgscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mrgscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
