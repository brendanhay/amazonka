{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeGroupConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeGroupConfiguration
  ( NodeGroupConfiguration (..),

    -- * Smart constructor
    mkNodeGroupConfiguration,

    -- * Lenses
    ngcNodeGroupId,
    ngcPrimaryAvailabilityZone,
    ngcPrimaryOutpostArn,
    ngcReplicaAvailabilityZones,
    ngcReplicaCount,
    ngcReplicaOutpostArns,
    ngcSlots,
  )
where

import qualified Network.AWS.ElastiCache.Types.NodeGroupId as Types
import qualified Network.AWS.ElastiCache.Types.PrimaryAvailabilityZone as Types
import qualified Network.AWS.ElastiCache.Types.PrimaryOutpostArn as Types
import qualified Network.AWS.ElastiCache.Types.Slots as Types
import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Node group (shard) configuration options. Each node group (shard) configuration has the following: @Slots@ , @PrimaryAvailabilityZone@ , @ReplicaAvailabilityZones@ , @ReplicaCount@ .
--
-- /See:/ 'mkNodeGroupConfiguration' smart constructor.
data NodeGroupConfiguration = NodeGroupConfiguration'
  { -- | Either the ElastiCache for Redis supplied 4-digit id or a user supplied id for the node group these configuration values apply to.
    nodeGroupId :: Core.Maybe Types.NodeGroupId,
    -- | The Availability Zone where the primary node of this node group (shard) is launched.
    primaryAvailabilityZone :: Core.Maybe Types.PrimaryAvailabilityZone,
    -- | The output ARN of the primary node.
    primaryOutpostArn :: Core.Maybe Types.PrimaryOutpostArn,
    -- | A list of Availability Zones to be used for the read replicas. The number of Availability Zones in this list must match the value of @ReplicaCount@ or @ReplicasPerNodeGroup@ if not specified.
    replicaAvailabilityZones :: Core.Maybe [Types.String],
    -- | The number of read replica nodes in this node group (shard).
    replicaCount :: Core.Maybe Core.Int,
    -- | The outpost ARN of the node replicas.
    replicaOutpostArns :: Core.Maybe [Types.String],
    -- | A string that specifies the keyspace for a particular node group. Keyspaces range from 0 to 16,383. The string is in the format @startkey-endkey@ .
    --
    -- Example: @"0-3999"@
    slots :: Core.Maybe Types.Slots
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NodeGroupConfiguration' value with any optional fields omitted.
mkNodeGroupConfiguration ::
  NodeGroupConfiguration
mkNodeGroupConfiguration =
  NodeGroupConfiguration'
    { nodeGroupId = Core.Nothing,
      primaryAvailabilityZone = Core.Nothing,
      primaryOutpostArn = Core.Nothing,
      replicaAvailabilityZones = Core.Nothing,
      replicaCount = Core.Nothing,
      replicaOutpostArns = Core.Nothing,
      slots = Core.Nothing
    }

-- | Either the ElastiCache for Redis supplied 4-digit id or a user supplied id for the node group these configuration values apply to.
--
-- /Note:/ Consider using 'nodeGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngcNodeGroupId :: Lens.Lens' NodeGroupConfiguration (Core.Maybe Types.NodeGroupId)
ngcNodeGroupId = Lens.field @"nodeGroupId"
{-# DEPRECATED ngcNodeGroupId "Use generic-lens or generic-optics with 'nodeGroupId' instead." #-}

-- | The Availability Zone where the primary node of this node group (shard) is launched.
--
-- /Note:/ Consider using 'primaryAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngcPrimaryAvailabilityZone :: Lens.Lens' NodeGroupConfiguration (Core.Maybe Types.PrimaryAvailabilityZone)
ngcPrimaryAvailabilityZone = Lens.field @"primaryAvailabilityZone"
{-# DEPRECATED ngcPrimaryAvailabilityZone "Use generic-lens or generic-optics with 'primaryAvailabilityZone' instead." #-}

-- | The output ARN of the primary node.
--
-- /Note:/ Consider using 'primaryOutpostArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngcPrimaryOutpostArn :: Lens.Lens' NodeGroupConfiguration (Core.Maybe Types.PrimaryOutpostArn)
ngcPrimaryOutpostArn = Lens.field @"primaryOutpostArn"
{-# DEPRECATED ngcPrimaryOutpostArn "Use generic-lens or generic-optics with 'primaryOutpostArn' instead." #-}

-- | A list of Availability Zones to be used for the read replicas. The number of Availability Zones in this list must match the value of @ReplicaCount@ or @ReplicasPerNodeGroup@ if not specified.
--
-- /Note:/ Consider using 'replicaAvailabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngcReplicaAvailabilityZones :: Lens.Lens' NodeGroupConfiguration (Core.Maybe [Types.String])
ngcReplicaAvailabilityZones = Lens.field @"replicaAvailabilityZones"
{-# DEPRECATED ngcReplicaAvailabilityZones "Use generic-lens or generic-optics with 'replicaAvailabilityZones' instead." #-}

-- | The number of read replica nodes in this node group (shard).
--
-- /Note:/ Consider using 'replicaCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngcReplicaCount :: Lens.Lens' NodeGroupConfiguration (Core.Maybe Core.Int)
ngcReplicaCount = Lens.field @"replicaCount"
{-# DEPRECATED ngcReplicaCount "Use generic-lens or generic-optics with 'replicaCount' instead." #-}

-- | The outpost ARN of the node replicas.
--
-- /Note:/ Consider using 'replicaOutpostArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngcReplicaOutpostArns :: Lens.Lens' NodeGroupConfiguration (Core.Maybe [Types.String])
ngcReplicaOutpostArns = Lens.field @"replicaOutpostArns"
{-# DEPRECATED ngcReplicaOutpostArns "Use generic-lens or generic-optics with 'replicaOutpostArns' instead." #-}

-- | A string that specifies the keyspace for a particular node group. Keyspaces range from 0 to 16,383. The string is in the format @startkey-endkey@ .
--
-- Example: @"0-3999"@
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngcSlots :: Lens.Lens' NodeGroupConfiguration (Core.Maybe Types.Slots)
ngcSlots = Lens.field @"slots"
{-# DEPRECATED ngcSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

instance Core.FromXML NodeGroupConfiguration where
  parseXML x =
    NodeGroupConfiguration'
      Core.<$> (x Core..@? "NodeGroupId")
      Core.<*> (x Core..@? "PrimaryAvailabilityZone")
      Core.<*> (x Core..@? "PrimaryOutpostArn")
      Core.<*> ( x Core..@? "ReplicaAvailabilityZones"
                   Core..<@> Core.parseXMLList "AvailabilityZone"
               )
      Core.<*> (x Core..@? "ReplicaCount")
      Core.<*> ( x Core..@? "ReplicaOutpostArns"
                   Core..<@> Core.parseXMLList "OutpostArn"
               )
      Core.<*> (x Core..@? "Slots")
