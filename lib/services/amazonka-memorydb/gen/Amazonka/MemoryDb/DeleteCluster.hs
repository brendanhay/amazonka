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
-- Module      : Amazonka.MemoryDb.DeleteCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a cluster. It also deletes all associated nodes and node
-- endpoints
module Amazonka.MemoryDb.DeleteCluster
  ( -- * Creating a Request
    DeleteCluster (..),
    newDeleteCluster,

    -- * Request Lenses
    deleteCluster_finalSnapshotName,
    deleteCluster_clusterName,

    -- * Destructuring the Response
    DeleteClusterResponse (..),
    newDeleteClusterResponse,

    -- * Response Lenses
    deleteClusterResponse_cluster,
    deleteClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCluster' smart constructor.
data DeleteCluster = DeleteCluster'
  { -- | The user-supplied name of a final cluster snapshot. This is the unique
    -- name that identifies the snapshot. MemoryDB creates the snapshot, and
    -- then deletes the cluster immediately afterward.
    finalSnapshotName :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster to be deleted
    clusterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finalSnapshotName', 'deleteCluster_finalSnapshotName' - The user-supplied name of a final cluster snapshot. This is the unique
-- name that identifies the snapshot. MemoryDB creates the snapshot, and
-- then deletes the cluster immediately afterward.
--
-- 'clusterName', 'deleteCluster_clusterName' - The name of the cluster to be deleted
newDeleteCluster ::
  -- | 'clusterName'
  Prelude.Text ->
  DeleteCluster
newDeleteCluster pClusterName_ =
  DeleteCluster'
    { finalSnapshotName = Prelude.Nothing,
      clusterName = pClusterName_
    }

-- | The user-supplied name of a final cluster snapshot. This is the unique
-- name that identifies the snapshot. MemoryDB creates the snapshot, and
-- then deletes the cluster immediately afterward.
deleteCluster_finalSnapshotName :: Lens.Lens' DeleteCluster (Prelude.Maybe Prelude.Text)
deleteCluster_finalSnapshotName = Lens.lens (\DeleteCluster' {finalSnapshotName} -> finalSnapshotName) (\s@DeleteCluster' {} a -> s {finalSnapshotName = a} :: DeleteCluster)

-- | The name of the cluster to be deleted
deleteCluster_clusterName :: Lens.Lens' DeleteCluster Prelude.Text
deleteCluster_clusterName = Lens.lens (\DeleteCluster' {clusterName} -> clusterName) (\s@DeleteCluster' {} a -> s {clusterName = a} :: DeleteCluster)

instance Core.AWSRequest DeleteCluster where
  type
    AWSResponse DeleteCluster =
      DeleteClusterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteClusterResponse'
            Prelude.<$> (x Core..?> "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCluster where
  hashWithSalt _salt DeleteCluster' {..} =
    _salt `Prelude.hashWithSalt` finalSnapshotName
      `Prelude.hashWithSalt` clusterName

instance Prelude.NFData DeleteCluster where
  rnf DeleteCluster' {..} =
    Prelude.rnf finalSnapshotName
      `Prelude.seq` Prelude.rnf clusterName

instance Core.ToHeaders DeleteCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonMemoryDB.DeleteCluster" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteCluster where
  toJSON DeleteCluster' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FinalSnapshotName" Core..=)
              Prelude.<$> finalSnapshotName,
            Prelude.Just ("ClusterName" Core..= clusterName)
          ]
      )

instance Core.ToPath DeleteCluster where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteClusterResponse' smart constructor.
data DeleteClusterResponse = DeleteClusterResponse'
  { -- | The cluster object that has been deleted
    cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'deleteClusterResponse_cluster' - The cluster object that has been deleted
--
-- 'httpStatus', 'deleteClusterResponse_httpStatus' - The response's http status code.
newDeleteClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteClusterResponse
newDeleteClusterResponse pHttpStatus_ =
  DeleteClusterResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The cluster object that has been deleted
deleteClusterResponse_cluster :: Lens.Lens' DeleteClusterResponse (Prelude.Maybe Cluster)
deleteClusterResponse_cluster = Lens.lens (\DeleteClusterResponse' {cluster} -> cluster) (\s@DeleteClusterResponse' {} a -> s {cluster = a} :: DeleteClusterResponse)

-- | The response's http status code.
deleteClusterResponse_httpStatus :: Lens.Lens' DeleteClusterResponse Prelude.Int
deleteClusterResponse_httpStatus = Lens.lens (\DeleteClusterResponse' {httpStatus} -> httpStatus) (\s@DeleteClusterResponse' {} a -> s {httpStatus = a} :: DeleteClusterResponse)

instance Prelude.NFData DeleteClusterResponse where
  rnf DeleteClusterResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
