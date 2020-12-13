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
    drgfFinalSnapshotIdentifier,
    drgfRetainPrimaryCluster,
    drgfReplicationGroupId,

    -- * Destructuring the response
    DeleteReplicationGroupResponse (..),
    mkDeleteReplicationGroupResponse,

    -- ** Response lenses
    drgrsReplicationGroup,
    drgrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DeleteReplicationGroup@ operation.
--
-- /See:/ 'mkDeleteReplicationGroup' smart constructor.
data DeleteReplicationGroup = DeleteReplicationGroup'
  { -- | The name of a final node group (shard) snapshot. ElastiCache creates the snapshot from the primary node in the cluster, rather than one of the replicas; this is to ensure that it captures the freshest data. After the final snapshot is taken, the replication group is immediately deleted.
    finalSnapshotIdentifier :: Lude.Maybe Lude.Text,
    -- | If set to @true@ , all of the read replicas are deleted, but the primary node is retained.
    retainPrimaryCluster :: Lude.Maybe Lude.Bool,
    -- | The identifier for the cluster to be deleted. This parameter is not case sensitive.
    replicationGroupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReplicationGroup' with the minimum fields required to make a request.
--
-- * 'finalSnapshotIdentifier' - The name of a final node group (shard) snapshot. ElastiCache creates the snapshot from the primary node in the cluster, rather than one of the replicas; this is to ensure that it captures the freshest data. After the final snapshot is taken, the replication group is immediately deleted.
-- * 'retainPrimaryCluster' - If set to @true@ , all of the read replicas are deleted, but the primary node is retained.
-- * 'replicationGroupId' - The identifier for the cluster to be deleted. This parameter is not case sensitive.
mkDeleteReplicationGroup ::
  -- | 'replicationGroupId'
  Lude.Text ->
  DeleteReplicationGroup
mkDeleteReplicationGroup pReplicationGroupId_ =
  DeleteReplicationGroup'
    { finalSnapshotIdentifier = Lude.Nothing,
      retainPrimaryCluster = Lude.Nothing,
      replicationGroupId = pReplicationGroupId_
    }

-- | The name of a final node group (shard) snapshot. ElastiCache creates the snapshot from the primary node in the cluster, rather than one of the replicas; this is to ensure that it captures the freshest data. After the final snapshot is taken, the replication group is immediately deleted.
--
-- /Note:/ Consider using 'finalSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgfFinalSnapshotIdentifier :: Lens.Lens' DeleteReplicationGroup (Lude.Maybe Lude.Text)
drgfFinalSnapshotIdentifier = Lens.lens (finalSnapshotIdentifier :: DeleteReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {finalSnapshotIdentifier = a} :: DeleteReplicationGroup)
{-# DEPRECATED drgfFinalSnapshotIdentifier "Use generic-lens or generic-optics with 'finalSnapshotIdentifier' instead." #-}

-- | If set to @true@ , all of the read replicas are deleted, but the primary node is retained.
--
-- /Note:/ Consider using 'retainPrimaryCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgfRetainPrimaryCluster :: Lens.Lens' DeleteReplicationGroup (Lude.Maybe Lude.Bool)
drgfRetainPrimaryCluster = Lens.lens (retainPrimaryCluster :: DeleteReplicationGroup -> Lude.Maybe Lude.Bool) (\s a -> s {retainPrimaryCluster = a} :: DeleteReplicationGroup)
{-# DEPRECATED drgfRetainPrimaryCluster "Use generic-lens or generic-optics with 'retainPrimaryCluster' instead." #-}

-- | The identifier for the cluster to be deleted. This parameter is not case sensitive.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgfReplicationGroupId :: Lens.Lens' DeleteReplicationGroup Lude.Text
drgfReplicationGroupId = Lens.lens (replicationGroupId :: DeleteReplicationGroup -> Lude.Text) (\s a -> s {replicationGroupId = a} :: DeleteReplicationGroup)
{-# DEPRECATED drgfReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

instance Lude.AWSRequest DeleteReplicationGroup where
  type Rs DeleteReplicationGroup = DeleteReplicationGroupResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DeleteReplicationGroupResult"
      ( \s h x ->
          DeleteReplicationGroupResponse'
            Lude.<$> (x Lude..@? "ReplicationGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteReplicationGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteReplicationGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteReplicationGroup where
  toQuery DeleteReplicationGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteReplicationGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "FinalSnapshotIdentifier" Lude.=: finalSnapshotIdentifier,
        "RetainPrimaryCluster" Lude.=: retainPrimaryCluster,
        "ReplicationGroupId" Lude.=: replicationGroupId
      ]

-- | /See:/ 'mkDeleteReplicationGroupResponse' smart constructor.
data DeleteReplicationGroupResponse = DeleteReplicationGroupResponse'
  { replicationGroup :: Lude.Maybe ReplicationGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReplicationGroupResponse' with the minimum fields required to make a request.
--
-- * 'replicationGroup' -
-- * 'responseStatus' - The response status code.
mkDeleteReplicationGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteReplicationGroupResponse
mkDeleteReplicationGroupResponse pResponseStatus_ =
  DeleteReplicationGroupResponse'
    { replicationGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrsReplicationGroup :: Lens.Lens' DeleteReplicationGroupResponse (Lude.Maybe ReplicationGroup)
drgrsReplicationGroup = Lens.lens (replicationGroup :: DeleteReplicationGroupResponse -> Lude.Maybe ReplicationGroup) (\s a -> s {replicationGroup = a} :: DeleteReplicationGroupResponse)
{-# DEPRECATED drgrsReplicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrsResponseStatus :: Lens.Lens' DeleteReplicationGroupResponse Lude.Int
drgrsResponseStatus = Lens.lens (responseStatus :: DeleteReplicationGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteReplicationGroupResponse)
{-# DEPRECATED drgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
