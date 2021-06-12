{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeGroupConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeGroupConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Node group (shard) configuration options. Each node group (shard)
-- configuration has the following: @Slots@, @PrimaryAvailabilityZone@,
-- @ReplicaAvailabilityZones@, @ReplicaCount@.
--
-- /See:/ 'newNodeGroupConfiguration' smart constructor.
data NodeGroupConfiguration = NodeGroupConfiguration'
  { -- | The outpost ARN of the primary node.
    primaryOutpostArn :: Core.Maybe Core.Text,
    -- | The number of read replica nodes in this node group (shard).
    replicaCount :: Core.Maybe Core.Int,
    -- | The outpost ARN of the node replicas.
    replicaOutpostArns :: Core.Maybe [Core.Text],
    -- | Either the ElastiCache for Redis supplied 4-digit id or a user supplied
    -- id for the node group these configuration values apply to.
    nodeGroupId :: Core.Maybe Core.Text,
    -- | A string that specifies the keyspace for a particular node group.
    -- Keyspaces range from 0 to 16,383. The string is in the format
    -- @startkey-endkey@.
    --
    -- Example: @\"0-3999\"@
    slots :: Core.Maybe Core.Text,
    -- | A list of Availability Zones to be used for the read replicas. The
    -- number of Availability Zones in this list must match the value of
    -- @ReplicaCount@ or @ReplicasPerNodeGroup@ if not specified.
    replicaAvailabilityZones :: Core.Maybe [Core.Text],
    -- | The Availability Zone where the primary node of this node group (shard)
    -- is launched.
    primaryAvailabilityZone :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NodeGroupConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'primaryOutpostArn', 'nodeGroupConfiguration_primaryOutpostArn' - The outpost ARN of the primary node.
--
-- 'replicaCount', 'nodeGroupConfiguration_replicaCount' - The number of read replica nodes in this node group (shard).
--
-- 'replicaOutpostArns', 'nodeGroupConfiguration_replicaOutpostArns' - The outpost ARN of the node replicas.
--
-- 'nodeGroupId', 'nodeGroupConfiguration_nodeGroupId' - Either the ElastiCache for Redis supplied 4-digit id or a user supplied
-- id for the node group these configuration values apply to.
--
-- 'slots', 'nodeGroupConfiguration_slots' - A string that specifies the keyspace for a particular node group.
-- Keyspaces range from 0 to 16,383. The string is in the format
-- @startkey-endkey@.
--
-- Example: @\"0-3999\"@
--
-- 'replicaAvailabilityZones', 'nodeGroupConfiguration_replicaAvailabilityZones' - A list of Availability Zones to be used for the read replicas. The
-- number of Availability Zones in this list must match the value of
-- @ReplicaCount@ or @ReplicasPerNodeGroup@ if not specified.
--
-- 'primaryAvailabilityZone', 'nodeGroupConfiguration_primaryAvailabilityZone' - The Availability Zone where the primary node of this node group (shard)
-- is launched.
newNodeGroupConfiguration ::
  NodeGroupConfiguration
newNodeGroupConfiguration =
  NodeGroupConfiguration'
    { primaryOutpostArn =
        Core.Nothing,
      replicaCount = Core.Nothing,
      replicaOutpostArns = Core.Nothing,
      nodeGroupId = Core.Nothing,
      slots = Core.Nothing,
      replicaAvailabilityZones = Core.Nothing,
      primaryAvailabilityZone = Core.Nothing
    }

-- | The outpost ARN of the primary node.
nodeGroupConfiguration_primaryOutpostArn :: Lens.Lens' NodeGroupConfiguration (Core.Maybe Core.Text)
nodeGroupConfiguration_primaryOutpostArn = Lens.lens (\NodeGroupConfiguration' {primaryOutpostArn} -> primaryOutpostArn) (\s@NodeGroupConfiguration' {} a -> s {primaryOutpostArn = a} :: NodeGroupConfiguration)

-- | The number of read replica nodes in this node group (shard).
nodeGroupConfiguration_replicaCount :: Lens.Lens' NodeGroupConfiguration (Core.Maybe Core.Int)
nodeGroupConfiguration_replicaCount = Lens.lens (\NodeGroupConfiguration' {replicaCount} -> replicaCount) (\s@NodeGroupConfiguration' {} a -> s {replicaCount = a} :: NodeGroupConfiguration)

-- | The outpost ARN of the node replicas.
nodeGroupConfiguration_replicaOutpostArns :: Lens.Lens' NodeGroupConfiguration (Core.Maybe [Core.Text])
nodeGroupConfiguration_replicaOutpostArns = Lens.lens (\NodeGroupConfiguration' {replicaOutpostArns} -> replicaOutpostArns) (\s@NodeGroupConfiguration' {} a -> s {replicaOutpostArns = a} :: NodeGroupConfiguration) Core.. Lens.mapping Lens._Coerce

-- | Either the ElastiCache for Redis supplied 4-digit id or a user supplied
-- id for the node group these configuration values apply to.
nodeGroupConfiguration_nodeGroupId :: Lens.Lens' NodeGroupConfiguration (Core.Maybe Core.Text)
nodeGroupConfiguration_nodeGroupId = Lens.lens (\NodeGroupConfiguration' {nodeGroupId} -> nodeGroupId) (\s@NodeGroupConfiguration' {} a -> s {nodeGroupId = a} :: NodeGroupConfiguration)

-- | A string that specifies the keyspace for a particular node group.
-- Keyspaces range from 0 to 16,383. The string is in the format
-- @startkey-endkey@.
--
-- Example: @\"0-3999\"@
nodeGroupConfiguration_slots :: Lens.Lens' NodeGroupConfiguration (Core.Maybe Core.Text)
nodeGroupConfiguration_slots = Lens.lens (\NodeGroupConfiguration' {slots} -> slots) (\s@NodeGroupConfiguration' {} a -> s {slots = a} :: NodeGroupConfiguration)

-- | A list of Availability Zones to be used for the read replicas. The
-- number of Availability Zones in this list must match the value of
-- @ReplicaCount@ or @ReplicasPerNodeGroup@ if not specified.
nodeGroupConfiguration_replicaAvailabilityZones :: Lens.Lens' NodeGroupConfiguration (Core.Maybe [Core.Text])
nodeGroupConfiguration_replicaAvailabilityZones = Lens.lens (\NodeGroupConfiguration' {replicaAvailabilityZones} -> replicaAvailabilityZones) (\s@NodeGroupConfiguration' {} a -> s {replicaAvailabilityZones = a} :: NodeGroupConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The Availability Zone where the primary node of this node group (shard)
-- is launched.
nodeGroupConfiguration_primaryAvailabilityZone :: Lens.Lens' NodeGroupConfiguration (Core.Maybe Core.Text)
nodeGroupConfiguration_primaryAvailabilityZone = Lens.lens (\NodeGroupConfiguration' {primaryAvailabilityZone} -> primaryAvailabilityZone) (\s@NodeGroupConfiguration' {} a -> s {primaryAvailabilityZone = a} :: NodeGroupConfiguration)

instance Core.FromXML NodeGroupConfiguration where
  parseXML x =
    NodeGroupConfiguration'
      Core.<$> (x Core..@? "PrimaryOutpostArn")
      Core.<*> (x Core..@? "ReplicaCount")
      Core.<*> ( x Core..@? "ReplicaOutpostArns" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "OutpostArn")
               )
      Core.<*> (x Core..@? "NodeGroupId")
      Core.<*> (x Core..@? "Slots")
      Core.<*> ( x Core..@? "ReplicaAvailabilityZones"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "AvailabilityZone")
               )
      Core.<*> (x Core..@? "PrimaryAvailabilityZone")

instance Core.Hashable NodeGroupConfiguration

instance Core.NFData NodeGroupConfiguration

instance Core.ToQuery NodeGroupConfiguration where
  toQuery NodeGroupConfiguration' {..} =
    Core.mconcat
      [ "PrimaryOutpostArn" Core.=: primaryOutpostArn,
        "ReplicaCount" Core.=: replicaCount,
        "ReplicaOutpostArns"
          Core.=: Core.toQuery
            ( Core.toQueryList "OutpostArn"
                Core.<$> replicaOutpostArns
            ),
        "NodeGroupId" Core.=: nodeGroupId,
        "Slots" Core.=: slots,
        "ReplicaAvailabilityZones"
          Core.=: Core.toQuery
            ( Core.toQueryList "AvailabilityZone"
                Core.<$> replicaAvailabilityZones
            ),
        "PrimaryAvailabilityZone"
          Core.=: primaryAvailabilityZone
      ]
