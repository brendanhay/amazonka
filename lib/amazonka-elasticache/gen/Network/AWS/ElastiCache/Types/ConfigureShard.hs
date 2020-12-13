{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ConfigureShard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ConfigureShard
  ( ConfigureShard (..),

    -- * Smart constructor
    mkConfigureShard,

    -- * Lenses
    csPreferredAvailabilityZones,
    csNewReplicaCount,
    csNodeGroupId,
    csPreferredOutpostARNs,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Node group (shard) configuration options when adding or removing replicas. Each node group (shard) configuration has the following members: NodeGroupId, NewReplicaCount, and PreferredAvailabilityZones.
--
-- /See:/ 'mkConfigureShard' smart constructor.
data ConfigureShard = ConfigureShard'
  { -- | A list of @PreferredAvailabilityZone@ strings that specify which availability zones the replication group's nodes are to be in. The nummber of @PreferredAvailabilityZone@ values must equal the value of @NewReplicaCount@ plus 1 to account for the primary node. If this member of @ReplicaConfiguration@ is omitted, ElastiCache for Redis selects the availability zone for each of the replicas.
    preferredAvailabilityZones :: Lude.Maybe [Lude.Text],
    -- | The number of replicas you want in this node group at the end of this operation. The maximum value for @NewReplicaCount@ is 5. The minimum value depends upon the type of Redis replication group you are working with.
    --
    -- The minimum number of replicas in a shard or replication group is:
    --
    --     * Redis (cluster mode disabled)
    --
    --     * If Multi-AZ: 1
    --
    --
    --     * If Multi-AZ: 0
    --
    --
    --
    --
    --     * Redis (cluster mode enabled): 0 (though you will not be able to failover to a replica if your primary node fails)
    newReplicaCount :: Lude.Int,
    -- | The 4-digit id for the node group you are configuring. For Redis (cluster mode disabled) replication groups, the node group id is always 0001. To find a Redis (cluster mode enabled)'s node group's (shard's) id, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/shard-find-id.html Finding a Shard's Id> .
    nodeGroupId :: Lude.Text,
    -- | The outpost ARNs in which the cache cluster is created.
    preferredOutpostARNs :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfigureShard' with the minimum fields required to make a request.
--
-- * 'preferredAvailabilityZones' - A list of @PreferredAvailabilityZone@ strings that specify which availability zones the replication group's nodes are to be in. The nummber of @PreferredAvailabilityZone@ values must equal the value of @NewReplicaCount@ plus 1 to account for the primary node. If this member of @ReplicaConfiguration@ is omitted, ElastiCache for Redis selects the availability zone for each of the replicas.
-- * 'newReplicaCount' - The number of replicas you want in this node group at the end of this operation. The maximum value for @NewReplicaCount@ is 5. The minimum value depends upon the type of Redis replication group you are working with.
--
-- The minimum number of replicas in a shard or replication group is:
--
--     * Redis (cluster mode disabled)
--
--     * If Multi-AZ: 1
--
--
--     * If Multi-AZ: 0
--
--
--
--
--     * Redis (cluster mode enabled): 0 (though you will not be able to failover to a replica if your primary node fails)
--
--
-- * 'nodeGroupId' - The 4-digit id for the node group you are configuring. For Redis (cluster mode disabled) replication groups, the node group id is always 0001. To find a Redis (cluster mode enabled)'s node group's (shard's) id, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/shard-find-id.html Finding a Shard's Id> .
-- * 'preferredOutpostARNs' - The outpost ARNs in which the cache cluster is created.
mkConfigureShard ::
  -- | 'newReplicaCount'
  Lude.Int ->
  -- | 'nodeGroupId'
  Lude.Text ->
  ConfigureShard
mkConfigureShard pNewReplicaCount_ pNodeGroupId_ =
  ConfigureShard'
    { preferredAvailabilityZones = Lude.Nothing,
      newReplicaCount = pNewReplicaCount_,
      nodeGroupId = pNodeGroupId_,
      preferredOutpostARNs = Lude.Nothing
    }

-- | A list of @PreferredAvailabilityZone@ strings that specify which availability zones the replication group's nodes are to be in. The nummber of @PreferredAvailabilityZone@ values must equal the value of @NewReplicaCount@ plus 1 to account for the primary node. If this member of @ReplicaConfiguration@ is omitted, ElastiCache for Redis selects the availability zone for each of the replicas.
--
-- /Note:/ Consider using 'preferredAvailabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPreferredAvailabilityZones :: Lens.Lens' ConfigureShard (Lude.Maybe [Lude.Text])
csPreferredAvailabilityZones = Lens.lens (preferredAvailabilityZones :: ConfigureShard -> Lude.Maybe [Lude.Text]) (\s a -> s {preferredAvailabilityZones = a} :: ConfigureShard)
{-# DEPRECATED csPreferredAvailabilityZones "Use generic-lens or generic-optics with 'preferredAvailabilityZones' instead." #-}

-- | The number of replicas you want in this node group at the end of this operation. The maximum value for @NewReplicaCount@ is 5. The minimum value depends upon the type of Redis replication group you are working with.
--
-- The minimum number of replicas in a shard or replication group is:
--
--     * Redis (cluster mode disabled)
--
--     * If Multi-AZ: 1
--
--
--     * If Multi-AZ: 0
--
--
--
--
--     * Redis (cluster mode enabled): 0 (though you will not be able to failover to a replica if your primary node fails)
--
--
--
-- /Note:/ Consider using 'newReplicaCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csNewReplicaCount :: Lens.Lens' ConfigureShard Lude.Int
csNewReplicaCount = Lens.lens (newReplicaCount :: ConfigureShard -> Lude.Int) (\s a -> s {newReplicaCount = a} :: ConfigureShard)
{-# DEPRECATED csNewReplicaCount "Use generic-lens or generic-optics with 'newReplicaCount' instead." #-}

-- | The 4-digit id for the node group you are configuring. For Redis (cluster mode disabled) replication groups, the node group id is always 0001. To find a Redis (cluster mode enabled)'s node group's (shard's) id, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/shard-find-id.html Finding a Shard's Id> .
--
-- /Note:/ Consider using 'nodeGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csNodeGroupId :: Lens.Lens' ConfigureShard Lude.Text
csNodeGroupId = Lens.lens (nodeGroupId :: ConfigureShard -> Lude.Text) (\s a -> s {nodeGroupId = a} :: ConfigureShard)
{-# DEPRECATED csNodeGroupId "Use generic-lens or generic-optics with 'nodeGroupId' instead." #-}

-- | The outpost ARNs in which the cache cluster is created.
--
-- /Note:/ Consider using 'preferredOutpostARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPreferredOutpostARNs :: Lens.Lens' ConfigureShard (Lude.Maybe [Lude.Text])
csPreferredOutpostARNs = Lens.lens (preferredOutpostARNs :: ConfigureShard -> Lude.Maybe [Lude.Text]) (\s a -> s {preferredOutpostARNs = a} :: ConfigureShard)
{-# DEPRECATED csPreferredOutpostARNs "Use generic-lens or generic-optics with 'preferredOutpostARNs' instead." #-}

instance Lude.ToQuery ConfigureShard where
  toQuery ConfigureShard' {..} =
    Lude.mconcat
      [ "PreferredAvailabilityZones"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "PreferredAvailabilityZone"
                Lude.<$> preferredAvailabilityZones
            ),
        "NewReplicaCount" Lude.=: newReplicaCount,
        "NodeGroupId" Lude.=: nodeGroupId,
        "PreferredOutpostArns"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "PreferredOutpostArn"
                Lude.<$> preferredOutpostARNs
            )
      ]
