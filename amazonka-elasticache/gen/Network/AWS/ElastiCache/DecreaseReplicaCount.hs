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
-- Module      : Network.AWS.ElastiCache.DecreaseReplicaCount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Dynamically decreases the number of replicas in a Redis (cluster mode
-- disabled) replication group or the number of replica nodes in one or
-- more node groups (shards) of a Redis (cluster mode enabled) replication
-- group. This operation is performed with no cluster down time.
module Network.AWS.ElastiCache.DecreaseReplicaCount
  ( -- * Creating a Request
    DecreaseReplicaCount (..),
    newDecreaseReplicaCount,

    -- * Request Lenses
    decreaseReplicaCount_newReplicaCount,
    decreaseReplicaCount_replicasToRemove,
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

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDecreaseReplicaCount' smart constructor.
data DecreaseReplicaCount = DecreaseReplicaCount'
  { -- | The number of read replica nodes you want at the completion of this
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
    newReplicaCount' :: Core.Maybe Core.Int,
    -- | A list of the node ids to remove from the replication group or node
    -- group (shard).
    replicasToRemove :: Core.Maybe [Core.Text],
    -- | A list of @ConfigureShard@ objects that can be used to configure each
    -- shard in a Redis (cluster mode enabled) replication group. The
    -- @ConfigureShard@ has three members: @NewReplicaCount@, @NodeGroupId@,
    -- and @PreferredAvailabilityZones@.
    replicaConfiguration :: Core.Maybe [ConfigureShard],
    -- | The id of the replication group from which you want to remove replica
    -- nodes.
    replicationGroupId :: Core.Text,
    -- | If @True@, the number of replica nodes is decreased immediately.
    -- @ApplyImmediately=False@ is not currently supported.
    applyImmediately :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DecreaseReplicaCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'replicasToRemove', 'decreaseReplicaCount_replicasToRemove' - A list of the node ids to remove from the replication group or node
-- group (shard).
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
  Core.Text ->
  -- | 'applyImmediately'
  Core.Bool ->
  DecreaseReplicaCount
newDecreaseReplicaCount
  pReplicationGroupId_
  pApplyImmediately_ =
    DecreaseReplicaCount'
      { newReplicaCount' =
          Core.Nothing,
        replicasToRemove = Core.Nothing,
        replicaConfiguration = Core.Nothing,
        replicationGroupId = pReplicationGroupId_,
        applyImmediately = pApplyImmediately_
      }

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
decreaseReplicaCount_newReplicaCount :: Lens.Lens' DecreaseReplicaCount (Core.Maybe Core.Int)
decreaseReplicaCount_newReplicaCount = Lens.lens (\DecreaseReplicaCount' {newReplicaCount'} -> newReplicaCount') (\s@DecreaseReplicaCount' {} a -> s {newReplicaCount' = a} :: DecreaseReplicaCount)

-- | A list of the node ids to remove from the replication group or node
-- group (shard).
decreaseReplicaCount_replicasToRemove :: Lens.Lens' DecreaseReplicaCount (Core.Maybe [Core.Text])
decreaseReplicaCount_replicasToRemove = Lens.lens (\DecreaseReplicaCount' {replicasToRemove} -> replicasToRemove) (\s@DecreaseReplicaCount' {} a -> s {replicasToRemove = a} :: DecreaseReplicaCount) Core.. Lens.mapping Lens._Coerce

-- | A list of @ConfigureShard@ objects that can be used to configure each
-- shard in a Redis (cluster mode enabled) replication group. The
-- @ConfigureShard@ has three members: @NewReplicaCount@, @NodeGroupId@,
-- and @PreferredAvailabilityZones@.
decreaseReplicaCount_replicaConfiguration :: Lens.Lens' DecreaseReplicaCount (Core.Maybe [ConfigureShard])
decreaseReplicaCount_replicaConfiguration = Lens.lens (\DecreaseReplicaCount' {replicaConfiguration} -> replicaConfiguration) (\s@DecreaseReplicaCount' {} a -> s {replicaConfiguration = a} :: DecreaseReplicaCount) Core.. Lens.mapping Lens._Coerce

-- | The id of the replication group from which you want to remove replica
-- nodes.
decreaseReplicaCount_replicationGroupId :: Lens.Lens' DecreaseReplicaCount Core.Text
decreaseReplicaCount_replicationGroupId = Lens.lens (\DecreaseReplicaCount' {replicationGroupId} -> replicationGroupId) (\s@DecreaseReplicaCount' {} a -> s {replicationGroupId = a} :: DecreaseReplicaCount)

-- | If @True@, the number of replica nodes is decreased immediately.
-- @ApplyImmediately=False@ is not currently supported.
decreaseReplicaCount_applyImmediately :: Lens.Lens' DecreaseReplicaCount Core.Bool
decreaseReplicaCount_applyImmediately = Lens.lens (\DecreaseReplicaCount' {applyImmediately} -> applyImmediately) (\s@DecreaseReplicaCount' {} a -> s {applyImmediately = a} :: DecreaseReplicaCount)

instance Core.AWSRequest DecreaseReplicaCount where
  type
    AWSResponse DecreaseReplicaCount =
      DecreaseReplicaCountResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DecreaseReplicaCountResult"
      ( \s h x ->
          DecreaseReplicaCountResponse'
            Core.<$> (x Core..@? "ReplicationGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DecreaseReplicaCount

instance Core.NFData DecreaseReplicaCount

instance Core.ToHeaders DecreaseReplicaCount where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DecreaseReplicaCount where
  toPath = Core.const "/"

instance Core.ToQuery DecreaseReplicaCount where
  toQuery DecreaseReplicaCount' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DecreaseReplicaCount" :: Core.ByteString),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "NewReplicaCount" Core.=: newReplicaCount',
        "ReplicasToRemove"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> replicasToRemove
            ),
        "ReplicaConfiguration"
          Core.=: Core.toQuery
            ( Core.toQueryList "ConfigureShard"
                Core.<$> replicaConfiguration
            ),
        "ReplicationGroupId" Core.=: replicationGroupId,
        "ApplyImmediately" Core.=: applyImmediately
      ]

-- | /See:/ 'newDecreaseReplicaCountResponse' smart constructor.
data DecreaseReplicaCountResponse = DecreaseReplicaCountResponse'
  { replicationGroup :: Core.Maybe ReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DecreaseReplicaCountResponse
newDecreaseReplicaCountResponse pHttpStatus_ =
  DecreaseReplicaCountResponse'
    { replicationGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
decreaseReplicaCountResponse_replicationGroup :: Lens.Lens' DecreaseReplicaCountResponse (Core.Maybe ReplicationGroup)
decreaseReplicaCountResponse_replicationGroup = Lens.lens (\DecreaseReplicaCountResponse' {replicationGroup} -> replicationGroup) (\s@DecreaseReplicaCountResponse' {} a -> s {replicationGroup = a} :: DecreaseReplicaCountResponse)

-- | The response's http status code.
decreaseReplicaCountResponse_httpStatus :: Lens.Lens' DecreaseReplicaCountResponse Core.Int
decreaseReplicaCountResponse_httpStatus = Lens.lens (\DecreaseReplicaCountResponse' {httpStatus} -> httpStatus) (\s@DecreaseReplicaCountResponse' {} a -> s {httpStatus = a} :: DecreaseReplicaCountResponse)

instance Core.NFData DecreaseReplicaCountResponse
