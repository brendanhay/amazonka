{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElastiCache.Types.ConfigureShard
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ConfigureShard where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Node group (shard) configuration options when adding or removing
-- replicas. Each node group (shard) configuration has the following
-- members: NodeGroupId, NewReplicaCount, and PreferredAvailabilityZones.
--
-- /See:/ 'newConfigureShard' smart constructor.
data ConfigureShard = ConfigureShard'
  { -- | A list of @PreferredAvailabilityZone@ strings that specify which
    -- availability zones the replication group\'s nodes are to be in. The
    -- nummber of @PreferredAvailabilityZone@ values must equal the value of
    -- @NewReplicaCount@ plus 1 to account for the primary node. If this member
    -- of @ReplicaConfiguration@ is omitted, ElastiCache for Redis selects the
    -- availability zone for each of the replicas.
    preferredAvailabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The outpost ARNs in which the cache cluster is created.
    preferredOutpostArns :: Prelude.Maybe [Prelude.Text],
    -- | The 4-digit id for the node group you are configuring. For Redis
    -- (cluster mode disabled) replication groups, the node group id is always
    -- 0001. To find a Redis (cluster mode enabled)\'s node group\'s (shard\'s)
    -- id, see
    -- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/shard-find-id.html Finding a Shard\'s Id>.
    nodeGroupId :: Prelude.Text,
    -- | The number of replicas you want in this node group at the end of this
    -- operation. The maximum value for @NewReplicaCount@ is 5. The minimum
    -- value depends upon the type of Redis replication group you are working
    -- with.
    --
    -- The minimum number of replicas in a shard or replication group is:
    --
    -- -   Redis (cluster mode disabled)
    --
    --     -   If Multi-AZ: 1
    --
    --     -   If Multi-AZ: 0
    --
    -- -   Redis (cluster mode enabled): 0 (though you will not be able to
    --     failover to a replica if your primary node fails)
    newReplicaCount' :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConfigureShard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preferredAvailabilityZones', 'configureShard_preferredAvailabilityZones' - A list of @PreferredAvailabilityZone@ strings that specify which
-- availability zones the replication group\'s nodes are to be in. The
-- nummber of @PreferredAvailabilityZone@ values must equal the value of
-- @NewReplicaCount@ plus 1 to account for the primary node. If this member
-- of @ReplicaConfiguration@ is omitted, ElastiCache for Redis selects the
-- availability zone for each of the replicas.
--
-- 'preferredOutpostArns', 'configureShard_preferredOutpostArns' - The outpost ARNs in which the cache cluster is created.
--
-- 'nodeGroupId', 'configureShard_nodeGroupId' - The 4-digit id for the node group you are configuring. For Redis
-- (cluster mode disabled) replication groups, the node group id is always
-- 0001. To find a Redis (cluster mode enabled)\'s node group\'s (shard\'s)
-- id, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/shard-find-id.html Finding a Shard\'s Id>.
--
-- 'newReplicaCount'', 'configureShard_newReplicaCount' - The number of replicas you want in this node group at the end of this
-- operation. The maximum value for @NewReplicaCount@ is 5. The minimum
-- value depends upon the type of Redis replication group you are working
-- with.
--
-- The minimum number of replicas in a shard or replication group is:
--
-- -   Redis (cluster mode disabled)
--
--     -   If Multi-AZ: 1
--
--     -   If Multi-AZ: 0
--
-- -   Redis (cluster mode enabled): 0 (though you will not be able to
--     failover to a replica if your primary node fails)
newConfigureShard ::
  -- | 'nodeGroupId'
  Prelude.Text ->
  -- | 'newReplicaCount''
  Prelude.Int ->
  ConfigureShard
newConfigureShard pNodeGroupId_ pNewReplicaCount_ =
  ConfigureShard'
    { preferredAvailabilityZones =
        Prelude.Nothing,
      preferredOutpostArns = Prelude.Nothing,
      nodeGroupId = pNodeGroupId_,
      newReplicaCount' = pNewReplicaCount_
    }

-- | A list of @PreferredAvailabilityZone@ strings that specify which
-- availability zones the replication group\'s nodes are to be in. The
-- nummber of @PreferredAvailabilityZone@ values must equal the value of
-- @NewReplicaCount@ plus 1 to account for the primary node. If this member
-- of @ReplicaConfiguration@ is omitted, ElastiCache for Redis selects the
-- availability zone for each of the replicas.
configureShard_preferredAvailabilityZones :: Lens.Lens' ConfigureShard (Prelude.Maybe [Prelude.Text])
configureShard_preferredAvailabilityZones = Lens.lens (\ConfigureShard' {preferredAvailabilityZones} -> preferredAvailabilityZones) (\s@ConfigureShard' {} a -> s {preferredAvailabilityZones = a} :: ConfigureShard) Prelude.. Lens.mapping Prelude._Coerce

-- | The outpost ARNs in which the cache cluster is created.
configureShard_preferredOutpostArns :: Lens.Lens' ConfigureShard (Prelude.Maybe [Prelude.Text])
configureShard_preferredOutpostArns = Lens.lens (\ConfigureShard' {preferredOutpostArns} -> preferredOutpostArns) (\s@ConfigureShard' {} a -> s {preferredOutpostArns = a} :: ConfigureShard) Prelude.. Lens.mapping Prelude._Coerce

-- | The 4-digit id for the node group you are configuring. For Redis
-- (cluster mode disabled) replication groups, the node group id is always
-- 0001. To find a Redis (cluster mode enabled)\'s node group\'s (shard\'s)
-- id, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/shard-find-id.html Finding a Shard\'s Id>.
configureShard_nodeGroupId :: Lens.Lens' ConfigureShard Prelude.Text
configureShard_nodeGroupId = Lens.lens (\ConfigureShard' {nodeGroupId} -> nodeGroupId) (\s@ConfigureShard' {} a -> s {nodeGroupId = a} :: ConfigureShard)

-- | The number of replicas you want in this node group at the end of this
-- operation. The maximum value for @NewReplicaCount@ is 5. The minimum
-- value depends upon the type of Redis replication group you are working
-- with.
--
-- The minimum number of replicas in a shard or replication group is:
--
-- -   Redis (cluster mode disabled)
--
--     -   If Multi-AZ: 1
--
--     -   If Multi-AZ: 0
--
-- -   Redis (cluster mode enabled): 0 (though you will not be able to
--     failover to a replica if your primary node fails)
configureShard_newReplicaCount :: Lens.Lens' ConfigureShard Prelude.Int
configureShard_newReplicaCount = Lens.lens (\ConfigureShard' {newReplicaCount'} -> newReplicaCount') (\s@ConfigureShard' {} a -> s {newReplicaCount' = a} :: ConfigureShard)

instance Prelude.Hashable ConfigureShard

instance Prelude.NFData ConfigureShard

instance Prelude.ToQuery ConfigureShard where
  toQuery ConfigureShard' {..} =
    Prelude.mconcat
      [ "PreferredAvailabilityZones"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "PreferredAvailabilityZone"
                Prelude.<$> preferredAvailabilityZones
            ),
        "PreferredOutpostArns"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "PreferredOutpostArn"
                Prelude.<$> preferredOutpostArns
            ),
        "NodeGroupId" Prelude.=: nodeGroupId,
        "NewReplicaCount" Prelude.=: newReplicaCount'
      ]
