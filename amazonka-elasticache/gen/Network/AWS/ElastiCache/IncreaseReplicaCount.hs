{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
increaseReplicaCount_replicaConfiguration = Lens.lens (\IncreaseReplicaCount' {replicaConfiguration} -> replicaConfiguration) (\s@IncreaseReplicaCount' {} a -> s {replicaConfiguration = a} :: IncreaseReplicaCount) Prelude.. Lens.mapping Prelude._Coerce

-- | The id of the replication group to which you want to add replica nodes.
increaseReplicaCount_replicationGroupId :: Lens.Lens' IncreaseReplicaCount Prelude.Text
increaseReplicaCount_replicationGroupId = Lens.lens (\IncreaseReplicaCount' {replicationGroupId} -> replicationGroupId) (\s@IncreaseReplicaCount' {} a -> s {replicationGroupId = a} :: IncreaseReplicaCount)

-- | If @True@, the number of replica nodes is increased immediately.
-- @ApplyImmediately=False@ is not currently supported.
increaseReplicaCount_applyImmediately :: Lens.Lens' IncreaseReplicaCount Prelude.Bool
increaseReplicaCount_applyImmediately = Lens.lens (\IncreaseReplicaCount' {applyImmediately} -> applyImmediately) (\s@IncreaseReplicaCount' {} a -> s {applyImmediately = a} :: IncreaseReplicaCount)

instance Prelude.AWSRequest IncreaseReplicaCount where
  type
    Rs IncreaseReplicaCount =
      IncreaseReplicaCountResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "IncreaseReplicaCountResult"
      ( \s h x ->
          IncreaseReplicaCountResponse'
            Prelude.<$> (x Prelude..@? "ReplicationGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable IncreaseReplicaCount

instance Prelude.NFData IncreaseReplicaCount

instance Prelude.ToHeaders IncreaseReplicaCount where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath IncreaseReplicaCount where
  toPath = Prelude.const "/"

instance Prelude.ToQuery IncreaseReplicaCount where
  toQuery IncreaseReplicaCount' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("IncreaseReplicaCount" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2015-02-02" :: Prelude.ByteString),
        "NewReplicaCount" Prelude.=: newReplicaCount',
        "ReplicaConfiguration"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "ConfigureShard"
                Prelude.<$> replicaConfiguration
            ),
        "ReplicationGroupId" Prelude.=: replicationGroupId,
        "ApplyImmediately" Prelude.=: applyImmediately
      ]

-- | /See:/ 'newIncreaseReplicaCountResponse' smart constructor.
data IncreaseReplicaCountResponse = IncreaseReplicaCountResponse'
  { replicationGroup :: Prelude.Maybe ReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData IncreaseReplicaCountResponse
