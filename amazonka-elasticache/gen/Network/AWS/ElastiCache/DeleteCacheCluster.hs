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
-- Module      : Network.AWS.ElastiCache.DeleteCacheCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously provisioned cluster. @DeleteCacheCluster@ deletes
-- all associated cache nodes, node endpoints and the cluster itself. When
-- you receive a successful response from this operation, Amazon
-- ElastiCache immediately begins deleting the cluster; you cannot cancel
-- or revert this operation.
--
-- This operation is not valid for:
--
-- -   Redis (cluster mode enabled) clusters
--
-- -   Redis (cluster mode disabled) clusters
--
-- -   A cluster that is the last read replica of a replication group
--
-- -   A cluster that is the primary node of a replication group
--
-- -   A node group (shard) that has Multi-AZ mode enabled
--
-- -   A cluster from a Redis (cluster mode enabled) replication group
--
-- -   A cluster that is not in the @available@ state
module Network.AWS.ElastiCache.DeleteCacheCluster
  ( -- * Creating a Request
    DeleteCacheCluster (..),
    newDeleteCacheCluster,

    -- * Request Lenses
    deleteCacheCluster_finalSnapshotIdentifier,
    deleteCacheCluster_cacheClusterId,

    -- * Destructuring the Response
    DeleteCacheClusterResponse (..),
    newDeleteCacheClusterResponse,

    -- * Response Lenses
    deleteCacheClusterResponse_cacheCluster,
    deleteCacheClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteCacheCluster@ operation.
--
-- /See:/ 'newDeleteCacheCluster' smart constructor.
data DeleteCacheCluster = DeleteCacheCluster'
  { -- | The user-supplied name of a final cluster snapshot. This is the unique
    -- name that identifies the snapshot. ElastiCache creates the snapshot, and
    -- then deletes the cluster immediately afterward.
    finalSnapshotIdentifier :: Core.Maybe Core.Text,
    -- | The cluster identifier for the cluster to be deleted. This parameter is
    -- not case sensitive.
    cacheClusterId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCacheCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finalSnapshotIdentifier', 'deleteCacheCluster_finalSnapshotIdentifier' - The user-supplied name of a final cluster snapshot. This is the unique
-- name that identifies the snapshot. ElastiCache creates the snapshot, and
-- then deletes the cluster immediately afterward.
--
-- 'cacheClusterId', 'deleteCacheCluster_cacheClusterId' - The cluster identifier for the cluster to be deleted. This parameter is
-- not case sensitive.
newDeleteCacheCluster ::
  -- | 'cacheClusterId'
  Core.Text ->
  DeleteCacheCluster
newDeleteCacheCluster pCacheClusterId_ =
  DeleteCacheCluster'
    { finalSnapshotIdentifier =
        Core.Nothing,
      cacheClusterId = pCacheClusterId_
    }

-- | The user-supplied name of a final cluster snapshot. This is the unique
-- name that identifies the snapshot. ElastiCache creates the snapshot, and
-- then deletes the cluster immediately afterward.
deleteCacheCluster_finalSnapshotIdentifier :: Lens.Lens' DeleteCacheCluster (Core.Maybe Core.Text)
deleteCacheCluster_finalSnapshotIdentifier = Lens.lens (\DeleteCacheCluster' {finalSnapshotIdentifier} -> finalSnapshotIdentifier) (\s@DeleteCacheCluster' {} a -> s {finalSnapshotIdentifier = a} :: DeleteCacheCluster)

-- | The cluster identifier for the cluster to be deleted. This parameter is
-- not case sensitive.
deleteCacheCluster_cacheClusterId :: Lens.Lens' DeleteCacheCluster Core.Text
deleteCacheCluster_cacheClusterId = Lens.lens (\DeleteCacheCluster' {cacheClusterId} -> cacheClusterId) (\s@DeleteCacheCluster' {} a -> s {cacheClusterId = a} :: DeleteCacheCluster)

instance Core.AWSRequest DeleteCacheCluster where
  type
    AWSResponse DeleteCacheCluster =
      DeleteCacheClusterResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteCacheClusterResult"
      ( \s h x ->
          DeleteCacheClusterResponse'
            Core.<$> (x Core..@? "CacheCluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteCacheCluster

instance Core.NFData DeleteCacheCluster

instance Core.ToHeaders DeleteCacheCluster where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteCacheCluster where
  toPath = Core.const "/"

instance Core.ToQuery DeleteCacheCluster where
  toQuery DeleteCacheCluster' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteCacheCluster" :: Core.ByteString),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "FinalSnapshotIdentifier"
          Core.=: finalSnapshotIdentifier,
        "CacheClusterId" Core.=: cacheClusterId
      ]

-- | /See:/ 'newDeleteCacheClusterResponse' smart constructor.
data DeleteCacheClusterResponse = DeleteCacheClusterResponse'
  { cacheCluster :: Core.Maybe CacheCluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCacheClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheCluster', 'deleteCacheClusterResponse_cacheCluster' - Undocumented member.
--
-- 'httpStatus', 'deleteCacheClusterResponse_httpStatus' - The response's http status code.
newDeleteCacheClusterResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteCacheClusterResponse
newDeleteCacheClusterResponse pHttpStatus_ =
  DeleteCacheClusterResponse'
    { cacheCluster =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteCacheClusterResponse_cacheCluster :: Lens.Lens' DeleteCacheClusterResponse (Core.Maybe CacheCluster)
deleteCacheClusterResponse_cacheCluster = Lens.lens (\DeleteCacheClusterResponse' {cacheCluster} -> cacheCluster) (\s@DeleteCacheClusterResponse' {} a -> s {cacheCluster = a} :: DeleteCacheClusterResponse)

-- | The response's http status code.
deleteCacheClusterResponse_httpStatus :: Lens.Lens' DeleteCacheClusterResponse Core.Int
deleteCacheClusterResponse_httpStatus = Lens.lens (\DeleteCacheClusterResponse' {httpStatus} -> httpStatus) (\s@DeleteCacheClusterResponse' {} a -> s {httpStatus = a} :: DeleteCacheClusterResponse)

instance Core.NFData DeleteCacheClusterResponse
