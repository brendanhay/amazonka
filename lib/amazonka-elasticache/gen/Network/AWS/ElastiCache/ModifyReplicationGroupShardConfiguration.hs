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
    mrgscNodeGroupsToRetain,
    mrgscReshardingConfiguration,
    mrgscNodeGroupCount,
    mrgscApplyImmediately,
    mrgscReplicationGroupId,
    mrgscNodeGroupsToRemove,

    -- * Destructuring the response
    ModifyReplicationGroupShardConfigurationResponse (..),
    mkModifyReplicationGroupShardConfigurationResponse,

    -- ** Response lenses
    mrgscrsReplicationGroup,
    mrgscrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a @ModifyReplicationGroupShardConfiguration@ operation.
--
-- /See:/ 'mkModifyReplicationGroupShardConfiguration' smart constructor.
data ModifyReplicationGroupShardConfiguration = ModifyReplicationGroupShardConfiguration'
  { -- | If the value of @NodeGroupCount@ is less than the current number of node groups (shards), then either @NodeGroupsToRemove@ or @NodeGroupsToRetain@ is required. @NodeGroupsToRetain@ is a list of @NodeGroupId@ s to retain in the cluster.
    --
    -- ElastiCache for Redis will attempt to remove all node groups except those listed by @NodeGroupsToRetain@ from the cluster.
    nodeGroupsToRetain :: Lude.Maybe [Lude.Text],
    -- | Specifies the preferred availability zones for each node group in the cluster. If the value of @NodeGroupCount@ is greater than the current number of node groups (shards), you can use this parameter to specify the preferred availability zones of the cluster's shards. If you omit this parameter ElastiCache selects availability zones for you.
    --
    -- You can specify this parameter only if the value of @NodeGroupCount@ is greater than the current number of node groups (shards).
    reshardingConfiguration :: Lude.Maybe [ReshardingConfiguration],
    -- | The number of node groups (shards) that results from the modification of the shard configuration.
    nodeGroupCount :: Lude.Int,
    -- | Indicates that the shard reconfiguration process begins immediately. At present, the only permitted value for this parameter is @true@ .
    --
    -- Value: true
    applyImmediately :: Lude.Bool,
    -- | The name of the Redis (cluster mode enabled) cluster (replication group) on which the shards are to be configured.
    replicationGroupId :: Lude.Text,
    -- | If the value of @NodeGroupCount@ is less than the current number of node groups (shards), then either @NodeGroupsToRemove@ or @NodeGroupsToRetain@ is required. @NodeGroupsToRemove@ is a list of @NodeGroupId@ s to remove from the cluster.
    --
    -- ElastiCache for Redis will attempt to remove all node groups listed by @NodeGroupsToRemove@ from the cluster.
    nodeGroupsToRemove :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyReplicationGroupShardConfiguration' with the minimum fields required to make a request.
--
-- * 'nodeGroupsToRetain' - If the value of @NodeGroupCount@ is less than the current number of node groups (shards), then either @NodeGroupsToRemove@ or @NodeGroupsToRetain@ is required. @NodeGroupsToRetain@ is a list of @NodeGroupId@ s to retain in the cluster.
--
-- ElastiCache for Redis will attempt to remove all node groups except those listed by @NodeGroupsToRetain@ from the cluster.
-- * 'reshardingConfiguration' - Specifies the preferred availability zones for each node group in the cluster. If the value of @NodeGroupCount@ is greater than the current number of node groups (shards), you can use this parameter to specify the preferred availability zones of the cluster's shards. If you omit this parameter ElastiCache selects availability zones for you.
--
-- You can specify this parameter only if the value of @NodeGroupCount@ is greater than the current number of node groups (shards).
-- * 'nodeGroupCount' - The number of node groups (shards) that results from the modification of the shard configuration.
-- * 'applyImmediately' - Indicates that the shard reconfiguration process begins immediately. At present, the only permitted value for this parameter is @true@ .
--
-- Value: true
-- * 'replicationGroupId' - The name of the Redis (cluster mode enabled) cluster (replication group) on which the shards are to be configured.
-- * 'nodeGroupsToRemove' - If the value of @NodeGroupCount@ is less than the current number of node groups (shards), then either @NodeGroupsToRemove@ or @NodeGroupsToRetain@ is required. @NodeGroupsToRemove@ is a list of @NodeGroupId@ s to remove from the cluster.
--
-- ElastiCache for Redis will attempt to remove all node groups listed by @NodeGroupsToRemove@ from the cluster.
mkModifyReplicationGroupShardConfiguration ::
  -- | 'nodeGroupCount'
  Lude.Int ->
  -- | 'applyImmediately'
  Lude.Bool ->
  -- | 'replicationGroupId'
  Lude.Text ->
  ModifyReplicationGroupShardConfiguration
mkModifyReplicationGroupShardConfiguration
  pNodeGroupCount_
  pApplyImmediately_
  pReplicationGroupId_ =
    ModifyReplicationGroupShardConfiguration'
      { nodeGroupsToRetain =
          Lude.Nothing,
        reshardingConfiguration = Lude.Nothing,
        nodeGroupCount = pNodeGroupCount_,
        applyImmediately = pApplyImmediately_,
        replicationGroupId = pReplicationGroupId_,
        nodeGroupsToRemove = Lude.Nothing
      }

-- | If the value of @NodeGroupCount@ is less than the current number of node groups (shards), then either @NodeGroupsToRemove@ or @NodeGroupsToRetain@ is required. @NodeGroupsToRetain@ is a list of @NodeGroupId@ s to retain in the cluster.
--
-- ElastiCache for Redis will attempt to remove all node groups except those listed by @NodeGroupsToRetain@ from the cluster.
--
-- /Note:/ Consider using 'nodeGroupsToRetain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscNodeGroupsToRetain :: Lens.Lens' ModifyReplicationGroupShardConfiguration (Lude.Maybe [Lude.Text])
mrgscNodeGroupsToRetain = Lens.lens (nodeGroupsToRetain :: ModifyReplicationGroupShardConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {nodeGroupsToRetain = a} :: ModifyReplicationGroupShardConfiguration)
{-# DEPRECATED mrgscNodeGroupsToRetain "Use generic-lens or generic-optics with 'nodeGroupsToRetain' instead." #-}

-- | Specifies the preferred availability zones for each node group in the cluster. If the value of @NodeGroupCount@ is greater than the current number of node groups (shards), you can use this parameter to specify the preferred availability zones of the cluster's shards. If you omit this parameter ElastiCache selects availability zones for you.
--
-- You can specify this parameter only if the value of @NodeGroupCount@ is greater than the current number of node groups (shards).
--
-- /Note:/ Consider using 'reshardingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscReshardingConfiguration :: Lens.Lens' ModifyReplicationGroupShardConfiguration (Lude.Maybe [ReshardingConfiguration])
mrgscReshardingConfiguration = Lens.lens (reshardingConfiguration :: ModifyReplicationGroupShardConfiguration -> Lude.Maybe [ReshardingConfiguration]) (\s a -> s {reshardingConfiguration = a} :: ModifyReplicationGroupShardConfiguration)
{-# DEPRECATED mrgscReshardingConfiguration "Use generic-lens or generic-optics with 'reshardingConfiguration' instead." #-}

-- | The number of node groups (shards) that results from the modification of the shard configuration.
--
-- /Note:/ Consider using 'nodeGroupCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscNodeGroupCount :: Lens.Lens' ModifyReplicationGroupShardConfiguration Lude.Int
mrgscNodeGroupCount = Lens.lens (nodeGroupCount :: ModifyReplicationGroupShardConfiguration -> Lude.Int) (\s a -> s {nodeGroupCount = a} :: ModifyReplicationGroupShardConfiguration)
{-# DEPRECATED mrgscNodeGroupCount "Use generic-lens or generic-optics with 'nodeGroupCount' instead." #-}

-- | Indicates that the shard reconfiguration process begins immediately. At present, the only permitted value for this parameter is @true@ .
--
-- Value: true
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscApplyImmediately :: Lens.Lens' ModifyReplicationGroupShardConfiguration Lude.Bool
mrgscApplyImmediately = Lens.lens (applyImmediately :: ModifyReplicationGroupShardConfiguration -> Lude.Bool) (\s a -> s {applyImmediately = a} :: ModifyReplicationGroupShardConfiguration)
{-# DEPRECATED mrgscApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | The name of the Redis (cluster mode enabled) cluster (replication group) on which the shards are to be configured.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscReplicationGroupId :: Lens.Lens' ModifyReplicationGroupShardConfiguration Lude.Text
mrgscReplicationGroupId = Lens.lens (replicationGroupId :: ModifyReplicationGroupShardConfiguration -> Lude.Text) (\s a -> s {replicationGroupId = a} :: ModifyReplicationGroupShardConfiguration)
{-# DEPRECATED mrgscReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | If the value of @NodeGroupCount@ is less than the current number of node groups (shards), then either @NodeGroupsToRemove@ or @NodeGroupsToRetain@ is required. @NodeGroupsToRemove@ is a list of @NodeGroupId@ s to remove from the cluster.
--
-- ElastiCache for Redis will attempt to remove all node groups listed by @NodeGroupsToRemove@ from the cluster.
--
-- /Note:/ Consider using 'nodeGroupsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscNodeGroupsToRemove :: Lens.Lens' ModifyReplicationGroupShardConfiguration (Lude.Maybe [Lude.Text])
mrgscNodeGroupsToRemove = Lens.lens (nodeGroupsToRemove :: ModifyReplicationGroupShardConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {nodeGroupsToRemove = a} :: ModifyReplicationGroupShardConfiguration)
{-# DEPRECATED mrgscNodeGroupsToRemove "Use generic-lens or generic-optics with 'nodeGroupsToRemove' instead." #-}

instance Lude.AWSRequest ModifyReplicationGroupShardConfiguration where
  type
    Rs ModifyReplicationGroupShardConfiguration =
      ModifyReplicationGroupShardConfigurationResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "ModifyReplicationGroupShardConfigurationResult"
      ( \s h x ->
          ModifyReplicationGroupShardConfigurationResponse'
            Lude.<$> (x Lude..@? "ReplicationGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyReplicationGroupShardConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyReplicationGroupShardConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyReplicationGroupShardConfiguration where
  toQuery ModifyReplicationGroupShardConfiguration' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyReplicationGroupShardConfiguration" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "NodeGroupsToRetain"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "NodeGroupToRetain" Lude.<$> nodeGroupsToRetain),
        "ReshardingConfiguration"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "ReshardingConfiguration"
                Lude.<$> reshardingConfiguration
            ),
        "NodeGroupCount" Lude.=: nodeGroupCount,
        "ApplyImmediately" Lude.=: applyImmediately,
        "ReplicationGroupId" Lude.=: replicationGroupId,
        "NodeGroupsToRemove"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "NodeGroupToRemove" Lude.<$> nodeGroupsToRemove)
      ]

-- | /See:/ 'mkModifyReplicationGroupShardConfigurationResponse' smart constructor.
data ModifyReplicationGroupShardConfigurationResponse = ModifyReplicationGroupShardConfigurationResponse'
  { replicationGroup :: Lude.Maybe ReplicationGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyReplicationGroupShardConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'replicationGroup' -
-- * 'responseStatus' - The response status code.
mkModifyReplicationGroupShardConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyReplicationGroupShardConfigurationResponse
mkModifyReplicationGroupShardConfigurationResponse pResponseStatus_ =
  ModifyReplicationGroupShardConfigurationResponse'
    { replicationGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscrsReplicationGroup :: Lens.Lens' ModifyReplicationGroupShardConfigurationResponse (Lude.Maybe ReplicationGroup)
mrgscrsReplicationGroup = Lens.lens (replicationGroup :: ModifyReplicationGroupShardConfigurationResponse -> Lude.Maybe ReplicationGroup) (\s a -> s {replicationGroup = a} :: ModifyReplicationGroupShardConfigurationResponse)
{-# DEPRECATED mrgscrsReplicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgscrsResponseStatus :: Lens.Lens' ModifyReplicationGroupShardConfigurationResponse Lude.Int
mrgscrsResponseStatus = Lens.lens (responseStatus :: ModifyReplicationGroupShardConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyReplicationGroupShardConfigurationResponse)
{-# DEPRECATED mrgscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
