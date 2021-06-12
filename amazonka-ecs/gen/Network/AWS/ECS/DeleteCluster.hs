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
-- Module      : Network.AWS.ECS.DeleteCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified cluster. The cluster will transition to the
-- @INACTIVE@ state. Clusters with an @INACTIVE@ status may remain
-- discoverable in your account for a period of time. However, this
-- behavior is subject to change in the future, so you should not rely on
-- @INACTIVE@ clusters persisting.
--
-- You must deregister all container instances from this cluster before you
-- may delete it. You can list the container instances in a cluster with
-- ListContainerInstances and deregister them with
-- DeregisterContainerInstance.
module Network.AWS.ECS.DeleteCluster
  ( -- * Creating a Request
    DeleteCluster (..),
    newDeleteCluster,

    -- * Request Lenses
    deleteCluster_cluster,

    -- * Destructuring the Response
    DeleteClusterResponse (..),
    newDeleteClusterResponse,

    -- * Response Lenses
    deleteClusterResponse_cluster,
    deleteClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCluster' smart constructor.
data DeleteCluster = DeleteCluster'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster to
    -- delete.
    cluster :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'deleteCluster_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster to
-- delete.
newDeleteCluster ::
  -- | 'cluster'
  Core.Text ->
  DeleteCluster
newDeleteCluster pCluster_ =
  DeleteCluster' {cluster = pCluster_}

-- | The short name or full Amazon Resource Name (ARN) of the cluster to
-- delete.
deleteCluster_cluster :: Lens.Lens' DeleteCluster Core.Text
deleteCluster_cluster = Lens.lens (\DeleteCluster' {cluster} -> cluster) (\s@DeleteCluster' {} a -> s {cluster = a} :: DeleteCluster)

instance Core.AWSRequest DeleteCluster where
  type
    AWSResponse DeleteCluster =
      DeleteClusterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteClusterResponse'
            Core.<$> (x Core..?> "cluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteCluster

instance Core.NFData DeleteCluster

instance Core.ToHeaders DeleteCluster where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.DeleteCluster" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteCluster where
  toJSON DeleteCluster' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("cluster" Core..= cluster)]
      )

instance Core.ToPath DeleteCluster where
  toPath = Core.const "/"

instance Core.ToQuery DeleteCluster where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteClusterResponse' smart constructor.
data DeleteClusterResponse = DeleteClusterResponse'
  { -- | The full description of the deleted cluster.
    cluster :: Core.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'deleteClusterResponse_cluster' - The full description of the deleted cluster.
--
-- 'httpStatus', 'deleteClusterResponse_httpStatus' - The response's http status code.
newDeleteClusterResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteClusterResponse
newDeleteClusterResponse pHttpStatus_ =
  DeleteClusterResponse'
    { cluster = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of the deleted cluster.
deleteClusterResponse_cluster :: Lens.Lens' DeleteClusterResponse (Core.Maybe Cluster)
deleteClusterResponse_cluster = Lens.lens (\DeleteClusterResponse' {cluster} -> cluster) (\s@DeleteClusterResponse' {} a -> s {cluster = a} :: DeleteClusterResponse)

-- | The response's http status code.
deleteClusterResponse_httpStatus :: Lens.Lens' DeleteClusterResponse Core.Int
deleteClusterResponse_httpStatus = Lens.lens (\DeleteClusterResponse' {httpStatus} -> httpStatus) (\s@DeleteClusterResponse' {} a -> s {httpStatus = a} :: DeleteClusterResponse)

instance Core.NFData DeleteClusterResponse
