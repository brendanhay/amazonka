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
-- Module      : Network.AWS.ElastiCache.IncreaseReplicaCount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Dynamically increases the number of replicas in a Redis (cluster mode
-- disabled) replication group or the number of replica nodes in one or
-- more node groups (shards) of a Redis (cluster mode enabled) replication
-- group. This operation is performed with no cluster down time.
module Network.AWS.ElastiCache.IncreaseReplicaCount
  ( -- * Creating a Request
    IncreaseReplicaCount (..),
    newIncreaseReplicaCount,

    -- * Request Lenses
    increaseReplicaCount_newReplicaCount,
    increaseReplicaCount_replicaConfiguration,
    increaseReplicaCount_replicationGroupId,
    increaseReplicaCount_applyImmediately,

    -- * Destructuring the Response
    IncreaseReplicaCountResponse (..),
    newIncreaseReplicaCountResponse,

    -- * Response Lenses
    increaseReplicaCountResponse_replicationGroup,
    increaseReplicaCountResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newIncreaseReplicaCount' smart constructor.
data IncreaseReplicaCount = IncreaseReplicaCount'
  { -- | The number of read replica nodes you want at the completion of this
    -- operation. For Redis (cluster mode disabled) replication groups, this is
    -- the number of replica nodes in the replication group. For Redis (cluster
    -- mode enabled) replication groups, this is the number of replica nodes in
    -- each of the replication group\'s node groups.
    newReplicaCount' :: Core.Maybe Core.Int,
    -- | A list of @ConfigureShard@ objects that can be used to configure each
    -- shard in a Redis (cluster mode enabled) replication group. The
    -- @ConfigureShard@ has three members: @NewReplicaCount@, @NodeGroupId@,
    -- and @PreferredAvailabilityZones@.
    replicaConfiguration :: Core.Maybe [ConfigureShard],
    -- | The id of the replication group to which you want to add replica nodes.
    replicationGroupId :: Core.Text,
    -- | If @True@, the number of replica nodes is increased immediately.
    -- @ApplyImmediately=False@ is not currently supported.
    applyImmediately :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IncreaseReplicaCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newReplicaCount'', 'increaseReplicaCount_newReplicaCount' - The number of read replica nodes you want at the completion of this
-- operation. For Redis (cluster mode disabled) replication groups, this is
-- the number of replica nodes in the replication group. For Redis (cluster
-- mode enabled) replication groups, this is the number of replica nodes in
-- each of the replication group\'s node groups.
--
-- 'replicaConfiguration', 'increaseReplicaCount_replicaConfiguration' - A list of @ConfigureShard@ objects that can be used to configure each
-- shard in a Redis (cluster mode enabled) replication group. The
-- @ConfigureShard@ has three members: @NewReplicaCount@, @NodeGroupId@,
-- and @PreferredAvailabilityZones@.
--
-- 'replicationGroupId', 'increaseReplicaCount_replicationGroupId' - The id of the replication group to which you want to add replica nodes.
--
-- 'applyImmediately', 'increaseReplicaCount_applyImmediately' - If @True@, the number of replica nodes is increased immediately.
-- @ApplyImmediately=False@ is not currently supported.
newIncreaseReplicaCount ::
  -- | 'replicationGroupId'
  Core.Text ->
  -- | 'applyImmediately'
  Core.Bool ->
  IncreaseReplicaCount
newIncreaseReplicaCount
  pReplicationGroupId_
  pApplyImmediately_ =
    IncreaseReplicaCount'
      { newReplicaCount' =
          Core.Nothing,
        replicaConfiguration = Core.Nothing,
        replicationGroupId = pReplicationGroupId_,
        applyImmediately = pApplyImmediately_
      }

-- | The number of read replica nodes you want at the completion of this
-- operation. For Redis (cluster mode disabled) replication groups, this is
-- the number of replica nodes in the replication group. For Redis (cluster
-- mode enabled) replication groups, this is the number of replica nodes in
-- each of the replication group\'s node groups.
increaseReplicaCount_newReplicaCount :: Lens.Lens' IncreaseReplicaCount (Core.Maybe Core.Int)
increaseReplicaCount_newReplicaCount = Lens.lens (\IncreaseReplicaCount' {newReplicaCount'} -> newReplicaCount') (\s@IncreaseReplicaCount' {} a -> s {newReplicaCount' = a} :: IncreaseReplicaCount)

-- | A list of @ConfigureShard@ objects that can be used to configure each
-- shard in a Redis (cluster mode enabled) replication group. The
-- @ConfigureShard@ has three members: @NewReplicaCount@, @NodeGroupId@,
-- and @PreferredAvailabilityZones@.
increaseReplicaCount_replicaConfiguration :: Lens.Lens' IncreaseReplicaCount (Core.Maybe [ConfigureShard])
increaseReplicaCount_replicaConfiguration = Lens.lens (\IncreaseReplicaCount' {replicaConfiguration} -> replicaConfiguration) (\s@IncreaseReplicaCount' {} a -> s {replicaConfiguration = a} :: IncreaseReplicaCount) Core.. Lens.mapping Lens._Coerce

-- | The id of the replication group to which you want to add replica nodes.
increaseReplicaCount_replicationGroupId :: Lens.Lens' IncreaseReplicaCount Core.Text
increaseReplicaCount_replicationGroupId = Lens.lens (\IncreaseReplicaCount' {replicationGroupId} -> replicationGroupId) (\s@IncreaseReplicaCount' {} a -> s {replicationGroupId = a} :: IncreaseReplicaCount)

-- | If @True@, the number of replica nodes is increased immediately.
-- @ApplyImmediately=False@ is not currently supported.
increaseReplicaCount_applyImmediately :: Lens.Lens' IncreaseReplicaCount Core.Bool
increaseReplicaCount_applyImmediately = Lens.lens (\IncreaseReplicaCount' {applyImmediately} -> applyImmediately) (\s@IncreaseReplicaCount' {} a -> s {applyImmediately = a} :: IncreaseReplicaCount)

instance Core.AWSRequest IncreaseReplicaCount where
  type
    AWSResponse IncreaseReplicaCount =
      IncreaseReplicaCountResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "IncreaseReplicaCountResult"
      ( \s h x ->
          IncreaseReplicaCountResponse'
            Core.<$> (x Core..@? "ReplicationGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable IncreaseReplicaCount

instance Core.NFData IncreaseReplicaCount

instance Core.ToHeaders IncreaseReplicaCount where
  toHeaders = Core.const Core.mempty

instance Core.ToPath IncreaseReplicaCount where
  toPath = Core.const "/"

instance Core.ToQuery IncreaseReplicaCount where
  toQuery IncreaseReplicaCount' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("IncreaseReplicaCount" :: Core.ByteString),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "NewReplicaCount" Core.=: newReplicaCount',
        "ReplicaConfiguration"
          Core.=: Core.toQuery
            ( Core.toQueryList "ConfigureShard"
                Core.<$> replicaConfiguration
            ),
        "ReplicationGroupId" Core.=: replicationGroupId,
        "ApplyImmediately" Core.=: applyImmediately
      ]

-- | /See:/ 'newIncreaseReplicaCountResponse' smart constructor.
data IncreaseReplicaCountResponse = IncreaseReplicaCountResponse'
  { replicationGroup :: Core.Maybe ReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IncreaseReplicaCountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationGroup', 'increaseReplicaCountResponse_replicationGroup' - Undocumented member.
--
-- 'httpStatus', 'increaseReplicaCountResponse_httpStatus' - The response's http status code.
newIncreaseReplicaCountResponse ::
  -- | 'httpStatus'
  Core.Int ->
  IncreaseReplicaCountResponse
newIncreaseReplicaCountResponse pHttpStatus_ =
  IncreaseReplicaCountResponse'
    { replicationGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
increaseReplicaCountResponse_replicationGroup :: Lens.Lens' IncreaseReplicaCountResponse (Core.Maybe ReplicationGroup)
increaseReplicaCountResponse_replicationGroup = Lens.lens (\IncreaseReplicaCountResponse' {replicationGroup} -> replicationGroup) (\s@IncreaseReplicaCountResponse' {} a -> s {replicationGroup = a} :: IncreaseReplicaCountResponse)

-- | The response's http status code.
increaseReplicaCountResponse_httpStatus :: Lens.Lens' IncreaseReplicaCountResponse Core.Int
increaseReplicaCountResponse_httpStatus = Lens.lens (\IncreaseReplicaCountResponse' {httpStatus} -> httpStatus) (\s@IncreaseReplicaCountResponse' {} a -> s {httpStatus = a} :: IncreaseReplicaCountResponse)

instance Core.NFData IncreaseReplicaCountResponse
