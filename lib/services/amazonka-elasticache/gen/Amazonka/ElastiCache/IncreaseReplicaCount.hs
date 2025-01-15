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
-- Module      : Amazonka.ElastiCache.IncreaseReplicaCount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Dynamically increases the number of replicas in a Redis (cluster mode
-- disabled) replication group or the number of replica nodes in one or
-- more node groups (shards) of a Redis (cluster mode enabled) replication
-- group. This operation is performed with no cluster down time.
module Amazonka.ElastiCache.IncreaseReplicaCount
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newIncreaseReplicaCount' smart constructor.
data IncreaseReplicaCount = IncreaseReplicaCount'
  { -- | The number of read replica nodes you want at the completion of this
    -- operation. For Redis (cluster mode disabled) replication groups, this is
    -- the number of replica nodes in the replication group. For Redis (cluster
    -- mode enabled) replication groups, this is the number of replica nodes in
    -- each of the replication group\'s node groups.
    newReplicaCount' :: Prelude.Maybe Prelude.Int,
    -- | A list of @ConfigureShard@ objects that can be used to configure each
    -- shard in a Redis (cluster mode enabled) replication group. The
    -- @ConfigureShard@ has three members: @NewReplicaCount@, @NodeGroupId@,
    -- and @PreferredAvailabilityZones@.
    replicaConfiguration :: Prelude.Maybe [ConfigureShard],
    -- | The id of the replication group to which you want to add replica nodes.
    replicationGroupId :: Prelude.Text,
    -- | If @True@, the number of replica nodes is increased immediately.
    -- @ApplyImmediately=False@ is not currently supported.
    applyImmediately :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'applyImmediately'
  Prelude.Bool ->
  IncreaseReplicaCount
newIncreaseReplicaCount
  pReplicationGroupId_
  pApplyImmediately_ =
    IncreaseReplicaCount'
      { newReplicaCount' =
          Prelude.Nothing,
        replicaConfiguration = Prelude.Nothing,
        replicationGroupId = pReplicationGroupId_,
        applyImmediately = pApplyImmediately_
      }

-- | The number of read replica nodes you want at the completion of this
-- operation. For Redis (cluster mode disabled) replication groups, this is
-- the number of replica nodes in the replication group. For Redis (cluster
-- mode enabled) replication groups, this is the number of replica nodes in
-- each of the replication group\'s node groups.
increaseReplicaCount_newReplicaCount :: Lens.Lens' IncreaseReplicaCount (Prelude.Maybe Prelude.Int)
increaseReplicaCount_newReplicaCount = Lens.lens (\IncreaseReplicaCount' {newReplicaCount'} -> newReplicaCount') (\s@IncreaseReplicaCount' {} a -> s {newReplicaCount' = a} :: IncreaseReplicaCount)

-- | A list of @ConfigureShard@ objects that can be used to configure each
-- shard in a Redis (cluster mode enabled) replication group. The
-- @ConfigureShard@ has three members: @NewReplicaCount@, @NodeGroupId@,
-- and @PreferredAvailabilityZones@.
increaseReplicaCount_replicaConfiguration :: Lens.Lens' IncreaseReplicaCount (Prelude.Maybe [ConfigureShard])
increaseReplicaCount_replicaConfiguration = Lens.lens (\IncreaseReplicaCount' {replicaConfiguration} -> replicaConfiguration) (\s@IncreaseReplicaCount' {} a -> s {replicaConfiguration = a} :: IncreaseReplicaCount) Prelude.. Lens.mapping Lens.coerced

-- | The id of the replication group to which you want to add replica nodes.
increaseReplicaCount_replicationGroupId :: Lens.Lens' IncreaseReplicaCount Prelude.Text
increaseReplicaCount_replicationGroupId = Lens.lens (\IncreaseReplicaCount' {replicationGroupId} -> replicationGroupId) (\s@IncreaseReplicaCount' {} a -> s {replicationGroupId = a} :: IncreaseReplicaCount)

-- | If @True@, the number of replica nodes is increased immediately.
-- @ApplyImmediately=False@ is not currently supported.
increaseReplicaCount_applyImmediately :: Lens.Lens' IncreaseReplicaCount Prelude.Bool
increaseReplicaCount_applyImmediately = Lens.lens (\IncreaseReplicaCount' {applyImmediately} -> applyImmediately) (\s@IncreaseReplicaCount' {} a -> s {applyImmediately = a} :: IncreaseReplicaCount)

instance Core.AWSRequest IncreaseReplicaCount where
  type
    AWSResponse IncreaseReplicaCount =
      IncreaseReplicaCountResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "IncreaseReplicaCountResult"
      ( \s h x ->
          IncreaseReplicaCountResponse'
            Prelude.<$> (x Data..@? "ReplicationGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable IncreaseReplicaCount where
  hashWithSalt _salt IncreaseReplicaCount' {..} =
    _salt
      `Prelude.hashWithSalt` newReplicaCount'
      `Prelude.hashWithSalt` replicaConfiguration
      `Prelude.hashWithSalt` replicationGroupId
      `Prelude.hashWithSalt` applyImmediately

instance Prelude.NFData IncreaseReplicaCount where
  rnf IncreaseReplicaCount' {..} =
    Prelude.rnf newReplicaCount' `Prelude.seq`
      Prelude.rnf replicaConfiguration `Prelude.seq`
        Prelude.rnf replicationGroupId `Prelude.seq`
          Prelude.rnf applyImmediately

instance Data.ToHeaders IncreaseReplicaCount where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath IncreaseReplicaCount where
  toPath = Prelude.const "/"

instance Data.ToQuery IncreaseReplicaCount where
  toQuery IncreaseReplicaCount' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("IncreaseReplicaCount" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "NewReplicaCount" Data.=: newReplicaCount',
        "ReplicaConfiguration"
          Data.=: Data.toQuery
            ( Data.toQueryList "ConfigureShard"
                Prelude.<$> replicaConfiguration
            ),
        "ReplicationGroupId" Data.=: replicationGroupId,
        "ApplyImmediately" Data.=: applyImmediately
      ]

-- | /See:/ 'newIncreaseReplicaCountResponse' smart constructor.
data IncreaseReplicaCountResponse = IncreaseReplicaCountResponse'
  { replicationGroup :: Prelude.Maybe ReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  IncreaseReplicaCountResponse
newIncreaseReplicaCountResponse pHttpStatus_ =
  IncreaseReplicaCountResponse'
    { replicationGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
increaseReplicaCountResponse_replicationGroup :: Lens.Lens' IncreaseReplicaCountResponse (Prelude.Maybe ReplicationGroup)
increaseReplicaCountResponse_replicationGroup = Lens.lens (\IncreaseReplicaCountResponse' {replicationGroup} -> replicationGroup) (\s@IncreaseReplicaCountResponse' {} a -> s {replicationGroup = a} :: IncreaseReplicaCountResponse)

-- | The response's http status code.
increaseReplicaCountResponse_httpStatus :: Lens.Lens' IncreaseReplicaCountResponse Prelude.Int
increaseReplicaCountResponse_httpStatus = Lens.lens (\IncreaseReplicaCountResponse' {httpStatus} -> httpStatus) (\s@IncreaseReplicaCountResponse' {} a -> s {httpStatus = a} :: IncreaseReplicaCountResponse)

instance Prelude.NFData IncreaseReplicaCountResponse where
  rnf IncreaseReplicaCountResponse' {..} =
    Prelude.rnf replicationGroup `Prelude.seq`
      Prelude.rnf httpStatus
