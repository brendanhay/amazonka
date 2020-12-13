{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.TestFailover
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents the input of a @TestFailover@ operation which test automatic failover on a specified node group (called shard in the console) in a replication group (called cluster in the console).
--
-- __Note the following__
--
--     * A customer can use this operation to test automatic failover on up to 5 shards (called node groups in the ElastiCache API and AWS CLI) in any rolling 24-hour period.
--
--
--     * If calling this operation on shards in different clusters (called replication groups in the API and CLI), the calls can be made concurrently.
--
--
--
--     * If calling this operation multiple times on different shards in the same Redis (cluster mode enabled) replication group, the first node replacement must complete before a subsequent call can be made.
--
--
--     * To determine whether the node replacement is complete you can check Events using the Amazon ElastiCache console, the AWS CLI, or the ElastiCache API. Look for the following automatic failover related events, listed here in order of occurrance:
--
--     * Replication group message: @Test Failover API called for node group <node-group-id>@
--
--
--     * Cache cluster message: @Failover from primary node <primary-node-id> to replica node <node-id> completed@
--
--
--     * Replication group message: @Failover from primary node <primary-node-id> to replica node <node-id> completed@
--
--
--     * Cache cluster message: @Recovering cache nodes <node-id>@
--
--
--     * Cache cluster message: @Finished recovery for cache nodes <node-id>@
--
--
-- For more information see:
--
--     * <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/ECEvents.Viewing.html Viewing ElastiCache Events> in the /ElastiCache User Guide/
--
--
--     * <https://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeEvents.html DescribeEvents> in the ElastiCache API Reference
--
--
--
--
-- Also see, <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html#auto-failover-test Testing Multi-AZ > in the /ElastiCache User Guide/ .
module Network.AWS.ElastiCache.TestFailover
  ( -- * Creating a request
    TestFailover (..),
    mkTestFailover,

    -- ** Request lenses
    tfNodeGroupId,
    tfReplicationGroupId,

    -- * Destructuring the response
    TestFailoverResponse (..),
    mkTestFailoverResponse,

    -- ** Response lenses
    tfrsReplicationGroup,
    tfrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTestFailover' smart constructor.
data TestFailover = TestFailover'
  { -- | The name of the node group (called shard in the console) in this replication group on which automatic failover is to be tested. You may test automatic failover on up to 5 node groups in any rolling 24-hour period.
    nodeGroupId :: Lude.Text,
    -- | The name of the replication group (console: cluster) whose automatic failover is being tested by this operation.
    replicationGroupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestFailover' with the minimum fields required to make a request.
--
-- * 'nodeGroupId' - The name of the node group (called shard in the console) in this replication group on which automatic failover is to be tested. You may test automatic failover on up to 5 node groups in any rolling 24-hour period.
-- * 'replicationGroupId' - The name of the replication group (console: cluster) whose automatic failover is being tested by this operation.
mkTestFailover ::
  -- | 'nodeGroupId'
  Lude.Text ->
  -- | 'replicationGroupId'
  Lude.Text ->
  TestFailover
mkTestFailover pNodeGroupId_ pReplicationGroupId_ =
  TestFailover'
    { nodeGroupId = pNodeGroupId_,
      replicationGroupId = pReplicationGroupId_
    }

-- | The name of the node group (called shard in the console) in this replication group on which automatic failover is to be tested. You may test automatic failover on up to 5 node groups in any rolling 24-hour period.
--
-- /Note:/ Consider using 'nodeGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfNodeGroupId :: Lens.Lens' TestFailover Lude.Text
tfNodeGroupId = Lens.lens (nodeGroupId :: TestFailover -> Lude.Text) (\s a -> s {nodeGroupId = a} :: TestFailover)
{-# DEPRECATED tfNodeGroupId "Use generic-lens or generic-optics with 'nodeGroupId' instead." #-}

-- | The name of the replication group (console: cluster) whose automatic failover is being tested by this operation.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfReplicationGroupId :: Lens.Lens' TestFailover Lude.Text
tfReplicationGroupId = Lens.lens (replicationGroupId :: TestFailover -> Lude.Text) (\s a -> s {replicationGroupId = a} :: TestFailover)
{-# DEPRECATED tfReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

instance Lude.AWSRequest TestFailover where
  type Rs TestFailover = TestFailoverResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "TestFailoverResult"
      ( \s h x ->
          TestFailoverResponse'
            Lude.<$> (x Lude..@? "ReplicationGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TestFailover where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath TestFailover where
  toPath = Lude.const "/"

instance Lude.ToQuery TestFailover where
  toQuery TestFailover' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("TestFailover" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "NodeGroupId" Lude.=: nodeGroupId,
        "ReplicationGroupId" Lude.=: replicationGroupId
      ]

-- | /See:/ 'mkTestFailoverResponse' smart constructor.
data TestFailoverResponse = TestFailoverResponse'
  { replicationGroup :: Lude.Maybe ReplicationGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestFailoverResponse' with the minimum fields required to make a request.
--
-- * 'replicationGroup' -
-- * 'responseStatus' - The response status code.
mkTestFailoverResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TestFailoverResponse
mkTestFailoverResponse pResponseStatus_ =
  TestFailoverResponse'
    { replicationGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfrsReplicationGroup :: Lens.Lens' TestFailoverResponse (Lude.Maybe ReplicationGroup)
tfrsReplicationGroup = Lens.lens (replicationGroup :: TestFailoverResponse -> Lude.Maybe ReplicationGroup) (\s a -> s {replicationGroup = a} :: TestFailoverResponse)
{-# DEPRECATED tfrsReplicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfrsResponseStatus :: Lens.Lens' TestFailoverResponse Lude.Int
tfrsResponseStatus = Lens.lens (responseStatus :: TestFailoverResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TestFailoverResponse)
{-# DEPRECATED tfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
