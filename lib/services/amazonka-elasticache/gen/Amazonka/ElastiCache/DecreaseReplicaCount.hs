{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElastiCache.DecreaseReplicaCount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Dynamically decreases the number of replicas in a Redis (cluster mode
-- disabled) replication group or the number of replica nodes in one or
-- more node groups (shards) of a Redis (cluster mode enabled) replication
-- group. This operation is performed with no cluster down time.
module Amazonka.ElastiCache.DecreaseReplicaCount
  ( -- * Creating a Request
    DecreaseReplicaCount (..),
    newDecreaseReplicaCount,

    -- * Request Lenses
    decreaseReplicaCount_replicasToRemove,
    decreaseReplicaCount_newReplicaCount,
    decreaseReplicaCount_replicaConfiguration,
    decreaseReplicaCount_replicationGroupId,
    decreaseReplicaCount_applyImmediately,

    -- * Destructuring the Response
    DecreaseReplicaCountResponse (..),
    newDecreaseReplicaCountResponse,

    -- * Response Lenses
    decreaseReplicaCountResponse_replicationGroup,
    decreaseReplicaCountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDecreaseReplicaCount' smart constructor.
data DecreaseReplicaCount = DecreaseReplicaCount'
  { -- | A list of the node ids to remove from the replication group or node
    -- group (shard).
    replicasToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The number of read replica nodes you want at the completion of this
    -- operation. For Redis (cluster mode disabled) replication groups, this is
    -- the number of replica nodes in the replication group. For Redis (cluster
    -- mode enabled) replication groups, this is the number of replica nodes in
    -- each of the replication group\'s node groups.
    --
    -- The minimum number of replicas in a shard or replication group is:
    --
    -- -   Redis (cluster mode disabled)
    --
    --     -   If Multi-AZ is enabled: 1
    --
    --     -   If Multi-AZ is not enabled: 0
    --
    -- -   Redis (cluster mode enabled): 0 (though you will not be able to
    --     failover to a replica if your primary node fails)
    newReplicaCount' :: Prelude.Maybe Prelude.Int,
    -- | A list of @ConfigureShard@ objects that can be used to configure each
    -- shard in a Redis (cluster mode enabled) replication group. The
    -- @ConfigureShard@ has three members: @NewReplicaCount@, @NodeGroupId@,
    -- and @PreferredAvailabilityZones@.
    replicaConfiguration :: Prelude.Maybe [ConfigureShard],
    -- | The id of the replication group from which you want to remove replica
    -- nodes.
    replicationGroupId :: Prelude.Text,
    -- | If @True@, the number of replica nodes is decreased immediately.
    -- @ApplyImmediately=False@ is not currently supported.
    applyImmediately :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecreaseReplicaCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicasToRemove', 'decreaseReplicaCount_replicasToRemove' - A list of the node ids to remove from the replication group or node
-- group (shard).
--
-- 'newReplicaCount'', 'decreaseReplicaCount_newReplicaCount' - The number of read replica nodes you want at the completion of this
-- operation. For Redis (cluster mode disabled) replication groups, this is
-- the number of replica nodes in the replication group. For Redis (cluster
-- mode enabled) replication groups, this is the number of replica nodes in
-- each of the replication group\'s node groups.
--
-- The minimum number of replicas in a shard or replication group is:
--
-- -   Redis (cluster mode disabled)
--
--     -   If Multi-AZ is enabled: 1
--
--     -   If Multi-AZ is not enabled: 0
--
-- -   Redis (cluster mode enabled): 0 (though you will not be able to
--     failover to a replica if your primary node fails)
--
-- 'replicaConfiguration', 'decreaseReplicaCount_replicaConfiguration' - A list of @ConfigureShard@ objects that can be used to configure each
-- shard in a Redis (cluster mode enabled) replication group. The
-- @ConfigureShard@ has three members: @NewReplicaCount@, @NodeGroupId@,
-- and @PreferredAvailabilityZones@.
--
-- 'replicationGroupId', 'decreaseReplicaCount_replicationGroupId' - The id of the replication group from which you want to remove replica
-- nodes.
--
-- 'applyImmediately', 'decreaseReplicaCount_applyImmediately' - If @True@, the number of replica nodes is decreased immediately.
-- @ApplyImmediately=False@ is not currently supported.
newDecreaseReplicaCount ::
  -- | 'replicationGroupId'
  Prelude.Text ->
  -- | 'applyImmediately'
  Prelude.Bool ->
  DecreaseReplicaCount
newDecreaseReplicaCount
  pReplicationGroupId_
  pApplyImmediately_ =
    DecreaseReplicaCount'
      { replicasToRemove =
          Prelude.Nothing,
        newReplicaCount' = Prelude.Nothing,
        replicaConfiguration = Prelude.Nothing,
        replicationGroupId = pReplicationGroupId_,
        applyImmediately = pApplyImmediately_
      }

-- | A list of the node ids to remove from the replication group or node
-- group (shard).
decreaseReplicaCount_replicasToRemove :: Lens.Lens' DecreaseReplicaCount (Prelude.Maybe [Prelude.Text])
decreaseReplicaCount_replicasToRemove = Lens.lens (\DecreaseReplicaCount' {replicasToRemove} -> replicasToRemove) (\s@DecreaseReplicaCount' {} a -> s {replicasToRemove = a} :: DecreaseReplicaCount) Prelude.. Lens.mapping Lens.coerced

-- | The number of read replica nodes you want at the completion of this
-- operation. For Redis (cluster mode disabled) replication groups, this is
-- the number of replica nodes in the replication group. For Redis (cluster
-- mode enabled) replication groups, this is the number of replica nodes in
-- each of the replication group\'s node groups.
--
-- The minimum number of replicas in a shard or replication group is:
--
-- -   Redis (cluster mode disabled)
--
--     -   If Multi-AZ is enabled: 1
--
--     -   If Multi-AZ is not enabled: 0
--
-- -   Redis (cluster mode enabled): 0 (though you will not be able to
--     failover to a replica if your primary node fails)
decreaseReplicaCount_newReplicaCount :: Lens.Lens' DecreaseReplicaCount (Prelude.Maybe Prelude.Int)
decreaseReplicaCount_newReplicaCount = Lens.lens (\DecreaseReplicaCount' {newReplicaCount'} -> newReplicaCount') (\s@DecreaseReplicaCount' {} a -> s {newReplicaCount' = a} :: DecreaseReplicaCount)

-- | A list of @ConfigureShard@ objects that can be used to configure each
-- shard in a Redis (cluster mode enabled) replication group. The
-- @ConfigureShard@ has three members: @NewReplicaCount@, @NodeGroupId@,
-- and @PreferredAvailabilityZones@.
decreaseReplicaCount_replicaConfiguration :: Lens.Lens' DecreaseReplicaCount (Prelude.Maybe [ConfigureShard])
decreaseReplicaCount_replicaConfiguration = Lens.lens (\DecreaseReplicaCount' {replicaConfiguration} -> replicaConfiguration) (\s@DecreaseReplicaCount' {} a -> s {replicaConfiguration = a} :: DecreaseReplicaCount) Prelude.. Lens.mapping Lens.coerced

-- | The id of the replication group from which you want to remove replica
-- nodes.
decreaseReplicaCount_replicationGroupId :: Lens.Lens' DecreaseReplicaCount Prelude.Text
decreaseReplicaCount_replicationGroupId = Lens.lens (\DecreaseReplicaCount' {replicationGroupId} -> replicationGroupId) (\s@DecreaseReplicaCount' {} a -> s {replicationGroupId = a} :: DecreaseReplicaCount)

-- | If @True@, the number of replica nodes is decreased immediately.
-- @ApplyImmediately=False@ is not currently supported.
decreaseReplicaCount_applyImmediately :: Lens.Lens' DecreaseReplicaCount Prelude.Bool
decreaseReplicaCount_applyImmediately = Lens.lens (\DecreaseReplicaCount' {applyImmediately} -> applyImmediately) (\s@DecreaseReplicaCount' {} a -> s {applyImmediately = a} :: DecreaseReplicaCount)

instance Core.AWSRequest DecreaseReplicaCount where
  type
    AWSResponse DecreaseReplicaCount =
      DecreaseReplicaCountResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DecreaseReplicaCountResult"
      ( \s h x ->
          DecreaseReplicaCountResponse'
            Prelude.<$> (x Data..@? "ReplicationGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DecreaseReplicaCount where
  hashWithSalt _salt DecreaseReplicaCount' {..} =
    _salt `Prelude.hashWithSalt` replicasToRemove
      `Prelude.hashWithSalt` newReplicaCount'
      `Prelude.hashWithSalt` replicaConfiguration
      `Prelude.hashWithSalt` replicationGroupId
      `Prelude.hashWithSalt` applyImmediately

instance Prelude.NFData DecreaseReplicaCount where
  rnf DecreaseReplicaCount' {..} =
    Prelude.rnf replicasToRemove
      `Prelude.seq` Prelude.rnf newReplicaCount'
      `Prelude.seq` Prelude.rnf replicaConfiguration
      `Prelude.seq` Prelude.rnf replicationGroupId
      `Prelude.seq` Prelude.rnf applyImmediately

instance Data.ToHeaders DecreaseReplicaCount where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DecreaseReplicaCount where
  toPath = Prelude.const "/"

instance Data.ToQuery DecreaseReplicaCount where
  toQuery DecreaseReplicaCount' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DecreaseReplicaCount" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "ReplicasToRemove"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> replicasToRemove
            ),
        "NewReplicaCount" Data.=: newReplicaCount',
        "ReplicaConfiguration"
          Data.=: Data.toQuery
            ( Data.toQueryList "ConfigureShard"
                Prelude.<$> replicaConfiguration
            ),
        "ReplicationGroupId" Data.=: replicationGroupId,
        "ApplyImmediately" Data.=: applyImmediately
      ]

-- | /See:/ 'newDecreaseReplicaCountResponse' smart constructor.
data DecreaseReplicaCountResponse = DecreaseReplicaCountResponse'
  { replicationGroup :: Prelude.Maybe ReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecreaseReplicaCountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationGroup', 'decreaseReplicaCountResponse_replicationGroup' - Undocumented member.
--
-- 'httpStatus', 'decreaseReplicaCountResponse_httpStatus' - The response's http status code.
newDecreaseReplicaCountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DecreaseReplicaCountResponse
newDecreaseReplicaCountResponse pHttpStatus_ =
  DecreaseReplicaCountResponse'
    { replicationGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
decreaseReplicaCountResponse_replicationGroup :: Lens.Lens' DecreaseReplicaCountResponse (Prelude.Maybe ReplicationGroup)
decreaseReplicaCountResponse_replicationGroup = Lens.lens (\DecreaseReplicaCountResponse' {replicationGroup} -> replicationGroup) (\s@DecreaseReplicaCountResponse' {} a -> s {replicationGroup = a} :: DecreaseReplicaCountResponse)

-- | The response's http status code.
decreaseReplicaCountResponse_httpStatus :: Lens.Lens' DecreaseReplicaCountResponse Prelude.Int
decreaseReplicaCountResponse_httpStatus = Lens.lens (\DecreaseReplicaCountResponse' {httpStatus} -> httpStatus) (\s@DecreaseReplicaCountResponse' {} a -> s {httpStatus = a} :: DecreaseReplicaCountResponse)

instance Prelude.NFData DecreaseReplicaCountResponse where
  rnf DecreaseReplicaCountResponse' {..} =
    Prelude.rnf replicationGroup
      `Prelude.seq` Prelude.rnf httpStatus
