{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudHSMv2.DeleteCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified AWS CloudHSM cluster. Before you can delete a
-- cluster, you must delete all HSMs in the cluster. To see if the cluster
-- contains any HSMs, use DescribeClusters. To delete an HSM, use
-- DeleteHsm.
module Network.AWS.CloudHSMv2.DeleteCluster
  ( -- * Creating a Request
    DeleteCluster (..),
    newDeleteCluster,

    -- * Request Lenses
    deleteCluster_clusterId,

    -- * Destructuring the Response
    DeleteClusterResponse (..),
    newDeleteClusterResponse,

    -- * Response Lenses
    deleteClusterResponse_cluster,
    deleteClusterResponse_httpStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCluster' smart constructor.
data DeleteCluster = DeleteCluster'
  { -- | The identifier (ID) of the cluster that you are deleting. To find the
    -- cluster ID, use DescribeClusters.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'deleteCluster_clusterId' - The identifier (ID) of the cluster that you are deleting. To find the
-- cluster ID, use DescribeClusters.
newDeleteCluster ::
  -- | 'clusterId'
  Prelude.Text ->
  DeleteCluster
newDeleteCluster pClusterId_ =
  DeleteCluster' {clusterId = pClusterId_}

-- | The identifier (ID) of the cluster that you are deleting. To find the
-- cluster ID, use DescribeClusters.
deleteCluster_clusterId :: Lens.Lens' DeleteCluster Prelude.Text
deleteCluster_clusterId = Lens.lens (\DeleteCluster' {clusterId} -> clusterId) (\s@DeleteCluster' {} a -> s {clusterId = a} :: DeleteCluster)

instance Prelude.AWSRequest DeleteCluster where
  type Rs DeleteCluster = DeleteClusterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteClusterResponse'
            Prelude.<$> (x Prelude..?> "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCluster

instance Prelude.NFData DeleteCluster

instance Prelude.ToHeaders DeleteCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "BaldrApiService.DeleteCluster" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteCluster where
  toJSON DeleteCluster' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("ClusterId" Prelude..= clusterId)]
      )

instance Prelude.ToPath DeleteCluster where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteClusterResponse' smart constructor.
data DeleteClusterResponse = DeleteClusterResponse'
  { -- | Information about the cluster that was deleted.
    cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'deleteClusterResponse_cluster' - Information about the cluster that was deleted.
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

-- | Information about the cluster that was deleted.
deleteClusterResponse_cluster :: Lens.Lens' DeleteClusterResponse (Prelude.Maybe Cluster)
deleteClusterResponse_cluster = Lens.lens (\DeleteClusterResponse' {cluster} -> cluster) (\s@DeleteClusterResponse' {} a -> s {cluster = a} :: DeleteClusterResponse)

-- | The response's http status code.
deleteClusterResponse_httpStatus :: Lens.Lens' DeleteClusterResponse Prelude.Int
deleteClusterResponse_httpStatus = Lens.lens (\DeleteClusterResponse' {httpStatus} -> httpStatus) (\s@DeleteClusterResponse' {} a -> s {httpStatus = a} :: DeleteClusterResponse)

instance Prelude.NFData DeleteClusterResponse
