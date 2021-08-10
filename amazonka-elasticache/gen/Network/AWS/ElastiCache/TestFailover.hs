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
-- Module      : Network.AWS.ElastiCache.TestFailover
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents the input of a @TestFailover@ operation which test automatic
-- failover on a specified node group (called shard in the console) in a
-- replication group (called cluster in the console).
--
-- __Note the following__
--
-- -   A customer can use this operation to test automatic failover on up
--     to 5 shards (called node groups in the ElastiCache API and AWS CLI)
--     in any rolling 24-hour period.
--
-- -   If calling this operation on shards in different clusters (called
--     replication groups in the API and CLI), the calls can be made
--     concurrently.
--
-- -   If calling this operation multiple times on different shards in the
--     same Redis (cluster mode enabled) replication group, the first node
--     replacement must complete before a subsequent call can be made.
--
-- -   To determine whether the node replacement is complete you can check
--     Events using the Amazon ElastiCache console, the AWS CLI, or the
--     ElastiCache API. Look for the following automatic failover related
--     events, listed here in order of occurrance:
--
--     1.  Replication group message:
--         @Test Failover API called for node group \<node-group-id>@
--
--     2.  Cache cluster message:
--         @Failover from primary node \<primary-node-id> to replica node \<node-id> completed@
--
--     3.  Replication group message:
--         @Failover from primary node \<primary-node-id> to replica node \<node-id> completed@
--
--     4.  Cache cluster message: @Recovering cache nodes \<node-id>@
--
--     5.  Cache cluster message:
--         @Finished recovery for cache nodes \<node-id>@
--
--     For more information see:
--
--     -   <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/ECEvents.Viewing.html Viewing ElastiCache Events>
--         in the /ElastiCache User Guide/
--
--     -   <https://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeEvents.html DescribeEvents>
--         in the ElastiCache API Reference
--
-- Also see,
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html#auto-failover-test Testing Multi-AZ>
-- in the /ElastiCache User Guide/.
module Network.AWS.ElastiCache.TestFailover
  ( -- * Creating a Request
    TestFailover (..),
    newTestFailover,

    -- * Request Lenses
    testFailover_replicationGroupId,
    testFailover_nodeGroupId,

    -- * Destructuring the Response
    TestFailoverResponse (..),
    newTestFailoverResponse,

    -- * Response Lenses
    testFailoverResponse_replicationGroup,
    testFailoverResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTestFailover' smart constructor.
data TestFailover = TestFailover'
  { -- | The name of the replication group (console: cluster) whose automatic
    -- failover is being tested by this operation.
    replicationGroupId :: Prelude.Text,
    -- | The name of the node group (called shard in the console) in this
    -- replication group on which automatic failover is to be tested. You may
    -- test automatic failover on up to 5 node groups in any rolling 24-hour
    -- period.
    nodeGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestFailover' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationGroupId', 'testFailover_replicationGroupId' - The name of the replication group (console: cluster) whose automatic
-- failover is being tested by this operation.
--
-- 'nodeGroupId', 'testFailover_nodeGroupId' - The name of the node group (called shard in the console) in this
-- replication group on which automatic failover is to be tested. You may
-- test automatic failover on up to 5 node groups in any rolling 24-hour
-- period.
newTestFailover ::
  -- | 'replicationGroupId'
  Prelude.Text ->
  -- | 'nodeGroupId'
  Prelude.Text ->
  TestFailover
newTestFailover pReplicationGroupId_ pNodeGroupId_ =
  TestFailover'
    { replicationGroupId =
        pReplicationGroupId_,
      nodeGroupId = pNodeGroupId_
    }

-- | The name of the replication group (console: cluster) whose automatic
-- failover is being tested by this operation.
testFailover_replicationGroupId :: Lens.Lens' TestFailover Prelude.Text
testFailover_replicationGroupId = Lens.lens (\TestFailover' {replicationGroupId} -> replicationGroupId) (\s@TestFailover' {} a -> s {replicationGroupId = a} :: TestFailover)

-- | The name of the node group (called shard in the console) in this
-- replication group on which automatic failover is to be tested. You may
-- test automatic failover on up to 5 node groups in any rolling 24-hour
-- period.
testFailover_nodeGroupId :: Lens.Lens' TestFailover Prelude.Text
testFailover_nodeGroupId = Lens.lens (\TestFailover' {nodeGroupId} -> nodeGroupId) (\s@TestFailover' {} a -> s {nodeGroupId = a} :: TestFailover)

instance Core.AWSRequest TestFailover where
  type AWSResponse TestFailover = TestFailoverResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "TestFailoverResult"
      ( \s h x ->
          TestFailoverResponse'
            Prelude.<$> (x Core..@? "ReplicationGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TestFailover

instance Prelude.NFData TestFailover

instance Core.ToHeaders TestFailover where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath TestFailover where
  toPath = Prelude.const "/"

instance Core.ToQuery TestFailover where
  toQuery TestFailover' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("TestFailover" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "ReplicationGroupId" Core.=: replicationGroupId,
        "NodeGroupId" Core.=: nodeGroupId
      ]

-- | /See:/ 'newTestFailoverResponse' smart constructor.
data TestFailoverResponse = TestFailoverResponse'
  { replicationGroup :: Prelude.Maybe ReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestFailoverResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationGroup', 'testFailoverResponse_replicationGroup' - Undocumented member.
--
-- 'httpStatus', 'testFailoverResponse_httpStatus' - The response's http status code.
newTestFailoverResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TestFailoverResponse
newTestFailoverResponse pHttpStatus_ =
  TestFailoverResponse'
    { replicationGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
testFailoverResponse_replicationGroup :: Lens.Lens' TestFailoverResponse (Prelude.Maybe ReplicationGroup)
testFailoverResponse_replicationGroup = Lens.lens (\TestFailoverResponse' {replicationGroup} -> replicationGroup) (\s@TestFailoverResponse' {} a -> s {replicationGroup = a} :: TestFailoverResponse)

-- | The response's http status code.
testFailoverResponse_httpStatus :: Lens.Lens' TestFailoverResponse Prelude.Int
testFailoverResponse_httpStatus = Lens.lens (\TestFailoverResponse' {httpStatus} -> httpStatus) (\s@TestFailoverResponse' {} a -> s {httpStatus = a} :: TestFailoverResponse)

instance Prelude.NFData TestFailoverResponse
