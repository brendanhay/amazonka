{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DecreaseReplicaCount (..),
    mkDecreaseReplicaCount,

    -- ** Request lenses
    drcNewReplicaCount,
    drcReplicaConfiguration,
    drcReplicasToRemove,
    drcReplicationGroupId,
    drcApplyImmediately,

    -- * Destructuring the response
    DecreaseReplicaCountResponse (..),
    mkDecreaseReplicaCountResponse,

    -- ** Response lenses
    drcrsReplicationGroup,
    drcrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDecreaseReplicaCount' smart constructor.
data DecreaseReplicaCount = DecreaseReplicaCount'
  { newReplicaCount ::
      Lude.Maybe Lude.Int,
    replicaConfiguration ::
      Lude.Maybe [ConfigureShard],
    replicasToRemove :: Lude.Maybe [Lude.Text],
    replicationGroupId :: Lude.Text,
    applyImmediately :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DecreaseReplicaCount' with the minimum fields required to make a request.
--
-- * 'applyImmediately' - If @True@ , the number of replica nodes is decreased immediately. @ApplyImmediately=False@ is not currently supported.
-- * 'newReplicaCount' - The number of read replica nodes you want at the completion of this operation. For Redis (cluster mode disabled) replication groups, this is the number of replica nodes in the replication group. For Redis (cluster mode enabled) replication groups, this is the number of replica nodes in each of the replication group's node groups.
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
-- * 'replicaConfiguration' - A list of @ConfigureShard@ objects that can be used to configure each shard in a Redis (cluster mode enabled) replication group. The @ConfigureShard@ has three members: @NewReplicaCount@ , @NodeGroupId@ , and @PreferredAvailabilityZones@ .
-- * 'replicasToRemove' - A list of the node ids to remove from the replication group or node group (shard).
-- * 'replicationGroupId' - The id of the replication group from which you want to remove replica nodes.
mkDecreaseReplicaCount ::
  -- | 'replicationGroupId'
  Lude.Text ->
  -- | 'applyImmediately'
  Lude.Bool ->
  DecreaseReplicaCount
mkDecreaseReplicaCount pReplicationGroupId_ pApplyImmediately_ =
  DecreaseReplicaCount'
    { newReplicaCount = Lude.Nothing,
      replicaConfiguration = Lude.Nothing,
      replicasToRemove = Lude.Nothing,
      replicationGroupId = pReplicationGroupId_,
      applyImmediately = pApplyImmediately_
    }

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
drcNewReplicaCount :: Lens.Lens' DecreaseReplicaCount (Lude.Maybe Lude.Int)
drcNewReplicaCount = Lens.lens (newReplicaCount :: DecreaseReplicaCount -> Lude.Maybe Lude.Int) (\s a -> s {newReplicaCount = a} :: DecreaseReplicaCount)
{-# DEPRECATED drcNewReplicaCount "Use generic-lens or generic-optics with 'newReplicaCount' instead." #-}

-- | A list of @ConfigureShard@ objects that can be used to configure each shard in a Redis (cluster mode enabled) replication group. The @ConfigureShard@ has three members: @NewReplicaCount@ , @NodeGroupId@ , and @PreferredAvailabilityZones@ .
--
-- /Note:/ Consider using 'replicaConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcReplicaConfiguration :: Lens.Lens' DecreaseReplicaCount (Lude.Maybe [ConfigureShard])
drcReplicaConfiguration = Lens.lens (replicaConfiguration :: DecreaseReplicaCount -> Lude.Maybe [ConfigureShard]) (\s a -> s {replicaConfiguration = a} :: DecreaseReplicaCount)
{-# DEPRECATED drcReplicaConfiguration "Use generic-lens or generic-optics with 'replicaConfiguration' instead." #-}

-- | A list of the node ids to remove from the replication group or node group (shard).
--
-- /Note:/ Consider using 'replicasToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcReplicasToRemove :: Lens.Lens' DecreaseReplicaCount (Lude.Maybe [Lude.Text])
drcReplicasToRemove = Lens.lens (replicasToRemove :: DecreaseReplicaCount -> Lude.Maybe [Lude.Text]) (\s a -> s {replicasToRemove = a} :: DecreaseReplicaCount)
{-# DEPRECATED drcReplicasToRemove "Use generic-lens or generic-optics with 'replicasToRemove' instead." #-}

-- | The id of the replication group from which you want to remove replica nodes.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcReplicationGroupId :: Lens.Lens' DecreaseReplicaCount Lude.Text
drcReplicationGroupId = Lens.lens (replicationGroupId :: DecreaseReplicaCount -> Lude.Text) (\s a -> s {replicationGroupId = a} :: DecreaseReplicaCount)
{-# DEPRECATED drcReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | If @True@ , the number of replica nodes is decreased immediately. @ApplyImmediately=False@ is not currently supported.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcApplyImmediately :: Lens.Lens' DecreaseReplicaCount Lude.Bool
drcApplyImmediately = Lens.lens (applyImmediately :: DecreaseReplicaCount -> Lude.Bool) (\s a -> s {applyImmediately = a} :: DecreaseReplicaCount)
{-# DEPRECATED drcApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

instance Lude.AWSRequest DecreaseReplicaCount where
  type Rs DecreaseReplicaCount = DecreaseReplicaCountResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DecreaseReplicaCountResult"
      ( \s h x ->
          DecreaseReplicaCountResponse'
            Lude.<$> (x Lude..@? "ReplicationGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DecreaseReplicaCount where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DecreaseReplicaCount where
  toPath = Lude.const "/"

instance Lude.ToQuery DecreaseReplicaCount where
  toQuery DecreaseReplicaCount' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DecreaseReplicaCount" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "NewReplicaCount" Lude.=: newReplicaCount,
        "ReplicaConfiguration"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "ConfigureShard" Lude.<$> replicaConfiguration),
        "ReplicasToRemove"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> replicasToRemove),
        "ReplicationGroupId" Lude.=: replicationGroupId,
        "ApplyImmediately" Lude.=: applyImmediately
      ]

-- | /See:/ 'mkDecreaseReplicaCountResponse' smart constructor.
data DecreaseReplicaCountResponse = DecreaseReplicaCountResponse'
  { replicationGroup ::
      Lude.Maybe ReplicationGroup,
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

-- | Creates a value of 'DecreaseReplicaCountResponse' with the minimum fields required to make a request.
--
-- * 'replicationGroup' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDecreaseReplicaCountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DecreaseReplicaCountResponse
mkDecreaseReplicaCountResponse pResponseStatus_ =
  DecreaseReplicaCountResponse'
    { replicationGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrsReplicationGroup :: Lens.Lens' DecreaseReplicaCountResponse (Lude.Maybe ReplicationGroup)
drcrsReplicationGroup = Lens.lens (replicationGroup :: DecreaseReplicaCountResponse -> Lude.Maybe ReplicationGroup) (\s a -> s {replicationGroup = a} :: DecreaseReplicaCountResponse)
{-# DEPRECATED drcrsReplicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrsResponseStatus :: Lens.Lens' DecreaseReplicaCountResponse Lude.Int
drcrsResponseStatus = Lens.lens (responseStatus :: DecreaseReplicaCountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DecreaseReplicaCountResponse)
{-# DEPRECATED drcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
