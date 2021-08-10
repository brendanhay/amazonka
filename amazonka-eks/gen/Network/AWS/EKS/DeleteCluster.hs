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
-- Module      : Network.AWS.EKS.DeleteCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the Amazon EKS cluster control plane.
--
-- If you have active services in your cluster that are associated with a
-- load balancer, you must delete those services before deleting the
-- cluster so that the load balancers are deleted properly. Otherwise, you
-- can have orphaned resources in your VPC that prevent you from being able
-- to delete the VPC. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/delete-cluster.html Deleting a Cluster>
-- in the /Amazon EKS User Guide/.
--
-- If you have managed node groups or Fargate profiles attached to the
-- cluster, you must delete them first. For more information, see
-- DeleteNodegroup and DeleteFargateProfile.
module Network.AWS.EKS.DeleteCluster
  ( -- * Creating a Request
    DeleteCluster (..),
    newDeleteCluster,

    -- * Request Lenses
    deleteCluster_name,

    -- * Destructuring the Response
    DeleteClusterResponse (..),
    newDeleteClusterResponse,

    -- * Response Lenses
    deleteClusterResponse_cluster,
    deleteClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCluster' smart constructor.
data DeleteCluster = DeleteCluster'
  { -- | The name of the cluster to delete.
    name :: Prelude.Text
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
-- 'name', 'deleteCluster_name' - The name of the cluster to delete.
newDeleteCluster ::
  -- | 'name'
  Prelude.Text ->
  DeleteCluster
newDeleteCluster pName_ =
  DeleteCluster' {name = pName_}

-- | The name of the cluster to delete.
deleteCluster_name :: Lens.Lens' DeleteCluster Prelude.Text
deleteCluster_name = Lens.lens (\DeleteCluster' {name} -> name) (\s@DeleteCluster' {} a -> s {name = a} :: DeleteCluster)

instance Core.AWSRequest DeleteCluster where
  type
    AWSResponse DeleteCluster =
      DeleteClusterResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteClusterResponse'
            Prelude.<$> (x Core..?> "cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCluster

instance Prelude.NFData DeleteCluster

instance Core.ToHeaders DeleteCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteCluster where
  toPath DeleteCluster' {..} =
    Prelude.mconcat ["/clusters/", Core.toBS name]

instance Core.ToQuery DeleteCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteClusterResponse' smart constructor.
data DeleteClusterResponse = DeleteClusterResponse'
  { -- | The full description of the cluster to delete.
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
-- 'cluster', 'deleteClusterResponse_cluster' - The full description of the cluster to delete.
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

-- | The full description of the cluster to delete.
deleteClusterResponse_cluster :: Lens.Lens' DeleteClusterResponse (Prelude.Maybe Cluster)
deleteClusterResponse_cluster = Lens.lens (\DeleteClusterResponse' {cluster} -> cluster) (\s@DeleteClusterResponse' {} a -> s {cluster = a} :: DeleteClusterResponse)

-- | The response's http status code.
deleteClusterResponse_httpStatus :: Lens.Lens' DeleteClusterResponse Prelude.Int
deleteClusterResponse_httpStatus = Lens.lens (\DeleteClusterResponse' {httpStatus} -> httpStatus) (\s@DeleteClusterResponse' {} a -> s {httpStatus = a} :: DeleteClusterResponse)

instance Prelude.NFData DeleteClusterResponse
