{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteCacheCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously provisioned cluster. @DeleteCacheCluster@ deletes all associated cache nodes, node endpoints and the cluster itself. When you receive a successful response from this operation, Amazon ElastiCache immediately begins deleting the cluster; you cannot cancel or revert this operation.
--
-- This operation is not valid for:
--
--     * Redis (cluster mode enabled) clusters
--
--
--     * A cluster that is the last read replica of a replication group
--
--
--     * A node group (shard) that has Multi-AZ mode enabled
--
--
--     * A cluster from a Redis (cluster mode enabled) replication group
--
--
--     * A cluster that is not in the @available@ state
module Network.AWS.ElastiCache.DeleteCacheCluster
  ( -- * Creating a request
    DeleteCacheCluster (..),
    mkDeleteCacheCluster,

    -- ** Request lenses
    dccFinalSnapshotIdentifier,
    dccCacheClusterId,

    -- * Destructuring the response
    DeleteCacheClusterResponse (..),
    mkDeleteCacheClusterResponse,

    -- ** Response lenses
    dccrsCacheCluster,
    dccrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DeleteCacheCluster@ operation.
--
-- /See:/ 'mkDeleteCacheCluster' smart constructor.
data DeleteCacheCluster = DeleteCacheCluster'
  { finalSnapshotIdentifier ::
      Lude.Maybe Lude.Text,
    cacheClusterId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCacheCluster' with the minimum fields required to make a request.
--
-- * 'cacheClusterId' - The cluster identifier for the cluster to be deleted. This parameter is not case sensitive.
-- * 'finalSnapshotIdentifier' - The user-supplied name of a final cluster snapshot. This is the unique name that identifies the snapshot. ElastiCache creates the snapshot, and then deletes the cluster immediately afterward.
mkDeleteCacheCluster ::
  -- | 'cacheClusterId'
  Lude.Text ->
  DeleteCacheCluster
mkDeleteCacheCluster pCacheClusterId_ =
  DeleteCacheCluster'
    { finalSnapshotIdentifier = Lude.Nothing,
      cacheClusterId = pCacheClusterId_
    }

-- | The user-supplied name of a final cluster snapshot. This is the unique name that identifies the snapshot. ElastiCache creates the snapshot, and then deletes the cluster immediately afterward.
--
-- /Note:/ Consider using 'finalSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccFinalSnapshotIdentifier :: Lens.Lens' DeleteCacheCluster (Lude.Maybe Lude.Text)
dccFinalSnapshotIdentifier = Lens.lens (finalSnapshotIdentifier :: DeleteCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {finalSnapshotIdentifier = a} :: DeleteCacheCluster)
{-# DEPRECATED dccFinalSnapshotIdentifier "Use generic-lens or generic-optics with 'finalSnapshotIdentifier' instead." #-}

-- | The cluster identifier for the cluster to be deleted. This parameter is not case sensitive.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccCacheClusterId :: Lens.Lens' DeleteCacheCluster Lude.Text
dccCacheClusterId = Lens.lens (cacheClusterId :: DeleteCacheCluster -> Lude.Text) (\s a -> s {cacheClusterId = a} :: DeleteCacheCluster)
{-# DEPRECATED dccCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

instance Lude.AWSRequest DeleteCacheCluster where
  type Rs DeleteCacheCluster = DeleteCacheClusterResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DeleteCacheClusterResult"
      ( \s h x ->
          DeleteCacheClusterResponse'
            Lude.<$> (x Lude..@? "CacheCluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteCacheCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteCacheCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCacheCluster where
  toQuery DeleteCacheCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteCacheCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "FinalSnapshotIdentifier" Lude.=: finalSnapshotIdentifier,
        "CacheClusterId" Lude.=: cacheClusterId
      ]

-- | /See:/ 'mkDeleteCacheClusterResponse' smart constructor.
data DeleteCacheClusterResponse = DeleteCacheClusterResponse'
  { cacheCluster ::
      Lude.Maybe CacheCluster,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCacheClusterResponse' with the minimum fields required to make a request.
--
-- * 'cacheCluster' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDeleteCacheClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteCacheClusterResponse
mkDeleteCacheClusterResponse pResponseStatus_ =
  DeleteCacheClusterResponse'
    { cacheCluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrsCacheCluster :: Lens.Lens' DeleteCacheClusterResponse (Lude.Maybe CacheCluster)
dccrsCacheCluster = Lens.lens (cacheCluster :: DeleteCacheClusterResponse -> Lude.Maybe CacheCluster) (\s a -> s {cacheCluster = a} :: DeleteCacheClusterResponse)
{-# DEPRECATED dccrsCacheCluster "Use generic-lens or generic-optics with 'cacheCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrsResponseStatus :: Lens.Lens' DeleteCacheClusterResponse Lude.Int
dccrsResponseStatus = Lens.lens (responseStatus :: DeleteCacheClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteCacheClusterResponse)
{-# DEPRECATED dccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
