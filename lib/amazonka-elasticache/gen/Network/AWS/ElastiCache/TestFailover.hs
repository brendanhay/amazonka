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
    tfReplicationGroupId,
    tfNodeGroupId,

    -- * Destructuring the response
    TestFailoverResponse (..),
    mkTestFailoverResponse,

    -- ** Response lenses
    tfrrsReplicationGroup,
    tfrrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTestFailover' smart constructor.
data TestFailover = TestFailover'
  { -- | The name of the replication group (console: cluster) whose automatic failover is being tested by this operation.
    replicationGroupId :: Types.String,
    -- | The name of the node group (called shard in the console) in this replication group on which automatic failover is to be tested. You may test automatic failover on up to 5 node groups in any rolling 24-hour period.
    nodeGroupId :: Types.AllowedNodeGroupId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestFailover' value with any optional fields omitted.
mkTestFailover ::
  -- | 'replicationGroupId'
  Types.String ->
  -- | 'nodeGroupId'
  Types.AllowedNodeGroupId ->
  TestFailover
mkTestFailover replicationGroupId nodeGroupId =
  TestFailover' {replicationGroupId, nodeGroupId}

-- | The name of the replication group (console: cluster) whose automatic failover is being tested by this operation.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfReplicationGroupId :: Lens.Lens' TestFailover Types.String
tfReplicationGroupId = Lens.field @"replicationGroupId"
{-# DEPRECATED tfReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | The name of the node group (called shard in the console) in this replication group on which automatic failover is to be tested. You may test automatic failover on up to 5 node groups in any rolling 24-hour period.
--
-- /Note:/ Consider using 'nodeGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfNodeGroupId :: Lens.Lens' TestFailover Types.AllowedNodeGroupId
tfNodeGroupId = Lens.field @"nodeGroupId"
{-# DEPRECATED tfNodeGroupId "Use generic-lens or generic-optics with 'nodeGroupId' instead." #-}

instance Core.AWSRequest TestFailover where
  type Rs TestFailover = TestFailoverResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "TestFailover")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "ReplicationGroupId" replicationGroupId)
                Core.<> (Core.toQueryValue "NodeGroupId" nodeGroupId)
            )
      }
  response =
    Response.receiveXMLWrapper
      "TestFailoverResult"
      ( \s h x ->
          TestFailoverResponse'
            Core.<$> (x Core..@? "ReplicationGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkTestFailoverResponse' smart constructor.
data TestFailoverResponse = TestFailoverResponse'
  { replicationGroup :: Core.Maybe Types.ReplicationGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TestFailoverResponse' value with any optional fields omitted.
mkTestFailoverResponse ::
  -- | 'responseStatus'
  Core.Int ->
  TestFailoverResponse
mkTestFailoverResponse responseStatus =
  TestFailoverResponse'
    { replicationGroup = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfrrsReplicationGroup :: Lens.Lens' TestFailoverResponse (Core.Maybe Types.ReplicationGroup)
tfrrsReplicationGroup = Lens.field @"replicationGroup"
{-# DEPRECATED tfrrsReplicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfrrsResponseStatus :: Lens.Lens' TestFailoverResponse Core.Int
tfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED tfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
