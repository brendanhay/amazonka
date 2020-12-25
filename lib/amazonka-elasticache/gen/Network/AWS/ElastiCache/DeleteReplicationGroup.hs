{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing replication group. By default, this operation deletes the entire replication group, including the primary/primaries and all of the read replicas. If the replication group has only one primary, you can optionally delete only the read replicas, while retaining the primary by setting @RetainPrimaryCluster=true@ .
--
-- When you receive a successful response from this operation, Amazon ElastiCache immediately begins deleting the selected resources; you cannot cancel or revert this operation.
module Network.AWS.ElastiCache.DeleteReplicationGroup
  ( -- * Creating a request
    DeleteReplicationGroup (..),
    mkDeleteReplicationGroup,

    -- ** Request lenses
    drgReplicationGroupId,
    drgFinalSnapshotIdentifier,
    drgRetainPrimaryCluster,

    -- * Destructuring the response
    DeleteReplicationGroupResponse (..),
    mkDeleteReplicationGroupResponse,

    -- ** Response lenses
    drgrrsReplicationGroup,
    drgrrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteReplicationGroup@ operation.
--
-- /See:/ 'mkDeleteReplicationGroup' smart constructor.
data DeleteReplicationGroup = DeleteReplicationGroup'
  { -- | The identifier for the cluster to be deleted. This parameter is not case sensitive.
    replicationGroupId :: Types.String,
    -- | The name of a final node group (shard) snapshot. ElastiCache creates the snapshot from the primary node in the cluster, rather than one of the replicas; this is to ensure that it captures the freshest data. After the final snapshot is taken, the replication group is immediately deleted.
    finalSnapshotIdentifier :: Core.Maybe Types.String,
    -- | If set to @true@ , all of the read replicas are deleted, but the primary node is retained.
    retainPrimaryCluster :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReplicationGroup' value with any optional fields omitted.
mkDeleteReplicationGroup ::
  -- | 'replicationGroupId'
  Types.String ->
  DeleteReplicationGroup
mkDeleteReplicationGroup replicationGroupId =
  DeleteReplicationGroup'
    { replicationGroupId,
      finalSnapshotIdentifier = Core.Nothing,
      retainPrimaryCluster = Core.Nothing
    }

-- | The identifier for the cluster to be deleted. This parameter is not case sensitive.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgReplicationGroupId :: Lens.Lens' DeleteReplicationGroup Types.String
drgReplicationGroupId = Lens.field @"replicationGroupId"
{-# DEPRECATED drgReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | The name of a final node group (shard) snapshot. ElastiCache creates the snapshot from the primary node in the cluster, rather than one of the replicas; this is to ensure that it captures the freshest data. After the final snapshot is taken, the replication group is immediately deleted.
--
-- /Note:/ Consider using 'finalSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgFinalSnapshotIdentifier :: Lens.Lens' DeleteReplicationGroup (Core.Maybe Types.String)
drgFinalSnapshotIdentifier = Lens.field @"finalSnapshotIdentifier"
{-# DEPRECATED drgFinalSnapshotIdentifier "Use generic-lens or generic-optics with 'finalSnapshotIdentifier' instead." #-}

-- | If set to @true@ , all of the read replicas are deleted, but the primary node is retained.
--
-- /Note:/ Consider using 'retainPrimaryCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgRetainPrimaryCluster :: Lens.Lens' DeleteReplicationGroup (Core.Maybe Core.Bool)
drgRetainPrimaryCluster = Lens.field @"retainPrimaryCluster"
{-# DEPRECATED drgRetainPrimaryCluster "Use generic-lens or generic-optics with 'retainPrimaryCluster' instead." #-}

instance Core.AWSRequest DeleteReplicationGroup where
  type Rs DeleteReplicationGroup = DeleteReplicationGroupResponse
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
            ( Core.pure ("Action", "DeleteReplicationGroup")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "ReplicationGroupId" replicationGroupId)
                Core.<> ( Core.toQueryValue "FinalSnapshotIdentifier"
                            Core.<$> finalSnapshotIdentifier
                        )
                Core.<> ( Core.toQueryValue "RetainPrimaryCluster"
                            Core.<$> retainPrimaryCluster
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteReplicationGroupResult"
      ( \s h x ->
          DeleteReplicationGroupResponse'
            Core.<$> (x Core..@? "ReplicationGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteReplicationGroupResponse' smart constructor.
data DeleteReplicationGroupResponse = DeleteReplicationGroupResponse'
  { replicationGroup :: Core.Maybe Types.ReplicationGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteReplicationGroupResponse' value with any optional fields omitted.
mkDeleteReplicationGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteReplicationGroupResponse
mkDeleteReplicationGroupResponse responseStatus =
  DeleteReplicationGroupResponse'
    { replicationGroup = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrrsReplicationGroup :: Lens.Lens' DeleteReplicationGroupResponse (Core.Maybe Types.ReplicationGroup)
drgrrsReplicationGroup = Lens.field @"replicationGroup"
{-# DEPRECATED drgrrsReplicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrrsResponseStatus :: Lens.Lens' DeleteReplicationGroupResponse Core.Int
drgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
