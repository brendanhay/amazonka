{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ConfigureShard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.ConfigureShard
  ( ConfigureShard (..)
  -- * Smart constructor
  , mkConfigureShard
  -- * Lenses
  , csNodeGroupId
  , csNewReplicaCount
  , csPreferredAvailabilityZones
  , csPreferredOutpostArns
  ) where

import qualified Network.AWS.ElastiCache.Types.AllowedNodeGroupId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Node group (shard) configuration options when adding or removing replicas. Each node group (shard) configuration has the following members: NodeGroupId, NewReplicaCount, and PreferredAvailabilityZones. 
--
-- /See:/ 'mkConfigureShard' smart constructor.
data ConfigureShard = ConfigureShard'
  { nodeGroupId :: Types.AllowedNodeGroupId
    -- ^ The 4-digit id for the node group you are configuring. For Redis (cluster mode disabled) replication groups, the node group id is always 0001. To find a Redis (cluster mode enabled)'s node group's (shard's) id, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/shard-find-id.html Finding a Shard's Id> .
  , newReplicaCount :: Core.Int
    -- ^ The number of replicas you want in this node group at the end of this operation. The maximum value for @NewReplicaCount@ is 5. The minimum value depends upon the type of Redis replication group you are working with.
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
  , preferredAvailabilityZones :: Core.Maybe [Core.Text]
    -- ^ A list of @PreferredAvailabilityZone@ strings that specify which availability zones the replication group's nodes are to be in. The nummber of @PreferredAvailabilityZone@ values must equal the value of @NewReplicaCount@ plus 1 to account for the primary node. If this member of @ReplicaConfiguration@ is omitted, ElastiCache for Redis selects the availability zone for each of the replicas.
  , preferredOutpostArns :: Core.Maybe [Core.Text]
    -- ^ The outpost ARNs in which the cache cluster is created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfigureShard' value with any optional fields omitted.
mkConfigureShard
    :: Types.AllowedNodeGroupId -- ^ 'nodeGroupId'
    -> Core.Int -- ^ 'newReplicaCount'
    -> ConfigureShard
mkConfigureShard nodeGroupId newReplicaCount
  = ConfigureShard'{nodeGroupId, newReplicaCount,
                    preferredAvailabilityZones = Core.Nothing,
                    preferredOutpostArns = Core.Nothing}

-- | The 4-digit id for the node group you are configuring. For Redis (cluster mode disabled) replication groups, the node group id is always 0001. To find a Redis (cluster mode enabled)'s node group's (shard's) id, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/shard-find-id.html Finding a Shard's Id> .
--
-- /Note:/ Consider using 'nodeGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csNodeGroupId :: Lens.Lens' ConfigureShard Types.AllowedNodeGroupId
csNodeGroupId = Lens.field @"nodeGroupId"
{-# INLINEABLE csNodeGroupId #-}
{-# DEPRECATED nodeGroupId "Use generic-lens or generic-optics with 'nodeGroupId' instead"  #-}

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
csNewReplicaCount :: Lens.Lens' ConfigureShard Core.Int
csNewReplicaCount = Lens.field @"newReplicaCount"
{-# INLINEABLE csNewReplicaCount #-}
{-# DEPRECATED newReplicaCount "Use generic-lens or generic-optics with 'newReplicaCount' instead"  #-}

-- | A list of @PreferredAvailabilityZone@ strings that specify which availability zones the replication group's nodes are to be in. The nummber of @PreferredAvailabilityZone@ values must equal the value of @NewReplicaCount@ plus 1 to account for the primary node. If this member of @ReplicaConfiguration@ is omitted, ElastiCache for Redis selects the availability zone for each of the replicas.
--
-- /Note:/ Consider using 'preferredAvailabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPreferredAvailabilityZones :: Lens.Lens' ConfigureShard (Core.Maybe [Core.Text])
csPreferredAvailabilityZones = Lens.field @"preferredAvailabilityZones"
{-# INLINEABLE csPreferredAvailabilityZones #-}
{-# DEPRECATED preferredAvailabilityZones "Use generic-lens or generic-optics with 'preferredAvailabilityZones' instead"  #-}

-- | The outpost ARNs in which the cache cluster is created.
--
-- /Note:/ Consider using 'preferredOutpostArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csPreferredOutpostArns :: Lens.Lens' ConfigureShard (Core.Maybe [Core.Text])
csPreferredOutpostArns = Lens.field @"preferredOutpostArns"
{-# INLINEABLE csPreferredOutpostArns #-}
{-# DEPRECATED preferredOutpostArns "Use generic-lens or generic-optics with 'preferredOutpostArns' instead"  #-}

instance Core.ToQuery ConfigureShard where
        toQuery ConfigureShard{..}
          = Core.toQueryPair "NodeGroupId" nodeGroupId Core.<>
              Core.toQueryPair "NewReplicaCount" newReplicaCount
              Core.<>
              Core.toQueryPair "PreferredAvailabilityZones"
                (Core.maybe Core.mempty
                   (Core.toQueryList "PreferredAvailabilityZone")
                   preferredAvailabilityZones)
              Core.<>
              Core.toQueryPair "PreferredOutpostArns"
                (Core.maybe Core.mempty (Core.toQueryList "PreferredOutpostArn")
                   preferredOutpostArns)
