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
-- Module      : Amazonka.ElastiCache.DeleteCacheCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.ElastiCache.DeleteCacheCluster
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @DeleteCacheCluster@ operation.
--
-- /See:/ 'newDeleteCacheCluster' smart constructor.
data DeleteCacheCluster = DeleteCacheCluster'
  { -- | The user-supplied name of a final cluster snapshot. This is the unique
    -- name that identifies the snapshot. ElastiCache creates the snapshot, and
    -- then deletes the cluster immediately afterward.
    finalSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The cluster identifier for the cluster to be deleted. This parameter is
    -- not case sensitive.
    cacheClusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteCacheCluster
newDeleteCacheCluster pCacheClusterId_ =
  DeleteCacheCluster'
    { finalSnapshotIdentifier =
        Prelude.Nothing,
      cacheClusterId = pCacheClusterId_
    }

-- | The user-supplied name of a final cluster snapshot. This is the unique
-- name that identifies the snapshot. ElastiCache creates the snapshot, and
-- then deletes the cluster immediately afterward.
deleteCacheCluster_finalSnapshotIdentifier :: Lens.Lens' DeleteCacheCluster (Prelude.Maybe Prelude.Text)
deleteCacheCluster_finalSnapshotIdentifier = Lens.lens (\DeleteCacheCluster' {finalSnapshotIdentifier} -> finalSnapshotIdentifier) (\s@DeleteCacheCluster' {} a -> s {finalSnapshotIdentifier = a} :: DeleteCacheCluster)

-- | The cluster identifier for the cluster to be deleted. This parameter is
-- not case sensitive.
deleteCacheCluster_cacheClusterId :: Lens.Lens' DeleteCacheCluster Prelude.Text
deleteCacheCluster_cacheClusterId = Lens.lens (\DeleteCacheCluster' {cacheClusterId} -> cacheClusterId) (\s@DeleteCacheCluster' {} a -> s {cacheClusterId = a} :: DeleteCacheCluster)

instance Core.AWSRequest DeleteCacheCluster where
  type
    AWSResponse DeleteCacheCluster =
      DeleteCacheClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteCacheClusterResult"
      ( \s h x ->
          DeleteCacheClusterResponse'
            Prelude.<$> (x Data..@? "CacheCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCacheCluster where
  hashWithSalt _salt DeleteCacheCluster' {..} =
    _salt
      `Prelude.hashWithSalt` finalSnapshotIdentifier
      `Prelude.hashWithSalt` cacheClusterId

instance Prelude.NFData DeleteCacheCluster where
  rnf DeleteCacheCluster' {..} =
    Prelude.rnf finalSnapshotIdentifier
      `Prelude.seq` Prelude.rnf cacheClusterId

instance Data.ToHeaders DeleteCacheCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteCacheCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteCacheCluster where
  toQuery DeleteCacheCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteCacheCluster" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "FinalSnapshotIdentifier"
          Data.=: finalSnapshotIdentifier,
        "CacheClusterId" Data.=: cacheClusterId
      ]

-- | /See:/ 'newDeleteCacheClusterResponse' smart constructor.
data DeleteCacheClusterResponse = DeleteCacheClusterResponse'
  { cacheCluster :: Prelude.Maybe CacheCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteCacheClusterResponse
newDeleteCacheClusterResponse pHttpStatus_ =
  DeleteCacheClusterResponse'
    { cacheCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteCacheClusterResponse_cacheCluster :: Lens.Lens' DeleteCacheClusterResponse (Prelude.Maybe CacheCluster)
deleteCacheClusterResponse_cacheCluster = Lens.lens (\DeleteCacheClusterResponse' {cacheCluster} -> cacheCluster) (\s@DeleteCacheClusterResponse' {} a -> s {cacheCluster = a} :: DeleteCacheClusterResponse)

-- | The response's http status code.
deleteCacheClusterResponse_httpStatus :: Lens.Lens' DeleteCacheClusterResponse Prelude.Int
deleteCacheClusterResponse_httpStatus = Lens.lens (\DeleteCacheClusterResponse' {httpStatus} -> httpStatus) (\s@DeleteCacheClusterResponse' {} a -> s {httpStatus = a} :: DeleteCacheClusterResponse)

instance Prelude.NFData DeleteCacheClusterResponse where
  rnf DeleteCacheClusterResponse' {..} =
    Prelude.rnf cacheCluster
      `Prelude.seq` Prelude.rnf httpStatus
