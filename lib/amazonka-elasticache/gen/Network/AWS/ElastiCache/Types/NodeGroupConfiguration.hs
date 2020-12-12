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
    ngcSlots,
    ngcReplicaOutpostARNs,
    ngcReplicaCount,
    ngcPrimaryAvailabilityZone,
    ngcReplicaAvailabilityZones,
    ngcPrimaryOutpostARN,
    ngcNodeGroupId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Node group (shard) configuration options. Each node group (shard) configuration has the following: @Slots@ , @PrimaryAvailabilityZone@ , @ReplicaAvailabilityZones@ , @ReplicaCount@ .
--
-- /See:/ 'mkNodeGroupConfiguration' smart constructor.
data NodeGroupConfiguration = NodeGroupConfiguration'
  { slots ::
      Lude.Maybe Lude.Text,
    replicaOutpostARNs :: Lude.Maybe [Lude.Text],
    replicaCount :: Lude.Maybe Lude.Int,
    primaryAvailabilityZone ::
      Lude.Maybe Lude.Text,
    replicaAvailabilityZones ::
      Lude.Maybe [Lude.Text],
    primaryOutpostARN :: Lude.Maybe Lude.Text,
    nodeGroupId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NodeGroupConfiguration' with the minimum fields required to make a request.
--
-- * 'nodeGroupId' - Either the ElastiCache for Redis supplied 4-digit id or a user supplied id for the node group these configuration values apply to.
-- * 'primaryAvailabilityZone' - The Availability Zone where the primary node of this node group (shard) is launched.
-- * 'primaryOutpostARN' - The output ARN of the primary node.
-- * 'replicaAvailabilityZones' - A list of Availability Zones to be used for the read replicas. The number of Availability Zones in this list must match the value of @ReplicaCount@ or @ReplicasPerNodeGroup@ if not specified.
-- * 'replicaCount' - The number of read replica nodes in this node group (shard).
-- * 'replicaOutpostARNs' - The outpost ARN of the node replicas.
-- * 'slots' - A string that specifies the keyspace for a particular node group. Keyspaces range from 0 to 16,383. The string is in the format @startkey-endkey@ .
--
-- Example: @"0-3999"@
mkNodeGroupConfiguration ::
  NodeGroupConfiguration
mkNodeGroupConfiguration =
  NodeGroupConfiguration'
    { slots = Lude.Nothing,
      replicaOutpostARNs = Lude.Nothing,
      replicaCount = Lude.Nothing,
      primaryAvailabilityZone = Lude.Nothing,
      replicaAvailabilityZones = Lude.Nothing,
      primaryOutpostARN = Lude.Nothing,
      nodeGroupId = Lude.Nothing
    }

-- | A string that specifies the keyspace for a particular node group. Keyspaces range from 0 to 16,383. The string is in the format @startkey-endkey@ .
--
-- Example: @"0-3999"@
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngcSlots :: Lens.Lens' NodeGroupConfiguration (Lude.Maybe Lude.Text)
ngcSlots = Lens.lens (slots :: NodeGroupConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {slots = a} :: NodeGroupConfiguration)
{-# DEPRECATED ngcSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

-- | The outpost ARN of the node replicas.
--
-- /Note:/ Consider using 'replicaOutpostARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngcReplicaOutpostARNs :: Lens.Lens' NodeGroupConfiguration (Lude.Maybe [Lude.Text])
ngcReplicaOutpostARNs = Lens.lens (replicaOutpostARNs :: NodeGroupConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {replicaOutpostARNs = a} :: NodeGroupConfiguration)
{-# DEPRECATED ngcReplicaOutpostARNs "Use generic-lens or generic-optics with 'replicaOutpostARNs' instead." #-}

-- | The number of read replica nodes in this node group (shard).
--
-- /Note:/ Consider using 'replicaCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngcReplicaCount :: Lens.Lens' NodeGroupConfiguration (Lude.Maybe Lude.Int)
ngcReplicaCount = Lens.lens (replicaCount :: NodeGroupConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {replicaCount = a} :: NodeGroupConfiguration)
{-# DEPRECATED ngcReplicaCount "Use generic-lens or generic-optics with 'replicaCount' instead." #-}

-- | The Availability Zone where the primary node of this node group (shard) is launched.
--
-- /Note:/ Consider using 'primaryAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngcPrimaryAvailabilityZone :: Lens.Lens' NodeGroupConfiguration (Lude.Maybe Lude.Text)
ngcPrimaryAvailabilityZone = Lens.lens (primaryAvailabilityZone :: NodeGroupConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {primaryAvailabilityZone = a} :: NodeGroupConfiguration)
{-# DEPRECATED ngcPrimaryAvailabilityZone "Use generic-lens or generic-optics with 'primaryAvailabilityZone' instead." #-}

-- | A list of Availability Zones to be used for the read replicas. The number of Availability Zones in this list must match the value of @ReplicaCount@ or @ReplicasPerNodeGroup@ if not specified.
--
-- /Note:/ Consider using 'replicaAvailabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngcReplicaAvailabilityZones :: Lens.Lens' NodeGroupConfiguration (Lude.Maybe [Lude.Text])
ngcReplicaAvailabilityZones = Lens.lens (replicaAvailabilityZones :: NodeGroupConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {replicaAvailabilityZones = a} :: NodeGroupConfiguration)
{-# DEPRECATED ngcReplicaAvailabilityZones "Use generic-lens or generic-optics with 'replicaAvailabilityZones' instead." #-}

-- | The output ARN of the primary node.
--
-- /Note:/ Consider using 'primaryOutpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngcPrimaryOutpostARN :: Lens.Lens' NodeGroupConfiguration (Lude.Maybe Lude.Text)
ngcPrimaryOutpostARN = Lens.lens (primaryOutpostARN :: NodeGroupConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {primaryOutpostARN = a} :: NodeGroupConfiguration)
{-# DEPRECATED ngcPrimaryOutpostARN "Use generic-lens or generic-optics with 'primaryOutpostARN' instead." #-}

-- | Either the ElastiCache for Redis supplied 4-digit id or a user supplied id for the node group these configuration values apply to.
--
-- /Note:/ Consider using 'nodeGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngcNodeGroupId :: Lens.Lens' NodeGroupConfiguration (Lude.Maybe Lude.Text)
ngcNodeGroupId = Lens.lens (nodeGroupId :: NodeGroupConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {nodeGroupId = a} :: NodeGroupConfiguration)
{-# DEPRECATED ngcNodeGroupId "Use generic-lens or generic-optics with 'nodeGroupId' instead." #-}

instance Lude.FromXML NodeGroupConfiguration where
  parseXML x =
    NodeGroupConfiguration'
      Lude.<$> (x Lude..@? "Slots")
      Lude.<*> ( x Lude..@? "ReplicaOutpostArns" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "OutpostArn")
               )
      Lude.<*> (x Lude..@? "ReplicaCount")
      Lude.<*> (x Lude..@? "PrimaryAvailabilityZone")
      Lude.<*> ( x Lude..@? "ReplicaAvailabilityZones" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "AvailabilityZone")
               )
      Lude.<*> (x Lude..@? "PrimaryOutpostArn")
      Lude.<*> (x Lude..@? "NodeGroupId")

instance Lude.ToQuery NodeGroupConfiguration where
  toQuery NodeGroupConfiguration' {..} =
    Lude.mconcat
      [ "Slots" Lude.=: slots,
        "ReplicaOutpostArns"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "OutpostArn" Lude.<$> replicaOutpostARNs),
        "ReplicaCount" Lude.=: replicaCount,
        "PrimaryAvailabilityZone" Lude.=: primaryAvailabilityZone,
        "ReplicaAvailabilityZones"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "AvailabilityZone"
                Lude.<$> replicaAvailabilityZones
            ),
        "PrimaryOutpostArn" Lude.=: primaryOutpostARN,
        "NodeGroupId" Lude.=: nodeGroupId
      ]
