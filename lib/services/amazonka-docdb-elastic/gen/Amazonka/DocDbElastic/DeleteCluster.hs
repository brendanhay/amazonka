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
-- Module      : Amazonka.DocDbElastic.DeleteCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a Elastic DocumentDB cluster.
module Amazonka.DocDbElastic.DeleteCluster
  ( -- * Creating a Request
    DeleteCluster (..),
    newDeleteCluster,

    -- * Request Lenses
    deleteCluster_clusterArn,

    -- * Destructuring the Response
    DeleteClusterResponse (..),
    newDeleteClusterResponse,

    -- * Response Lenses
    deleteClusterResponse_httpStatus,
    deleteClusterResponse_cluster,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocDbElastic.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCluster' smart constructor.
data DeleteCluster = DeleteCluster'
  { -- | The arn of the Elastic DocumentDB cluster that is to be deleted.
    clusterArn :: Prelude.Text
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
-- 'clusterArn', 'deleteCluster_clusterArn' - The arn of the Elastic DocumentDB cluster that is to be deleted.
newDeleteCluster ::
  -- | 'clusterArn'
  Prelude.Text ->
  DeleteCluster
newDeleteCluster pClusterArn_ =
  DeleteCluster' {clusterArn = pClusterArn_}

-- | The arn of the Elastic DocumentDB cluster that is to be deleted.
deleteCluster_clusterArn :: Lens.Lens' DeleteCluster Prelude.Text
deleteCluster_clusterArn = Lens.lens (\DeleteCluster' {clusterArn} -> clusterArn) (\s@DeleteCluster' {} a -> s {clusterArn = a} :: DeleteCluster)

instance Core.AWSRequest DeleteCluster where
  type
    AWSResponse DeleteCluster =
      DeleteClusterResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteClusterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "cluster")
      )

instance Prelude.Hashable DeleteCluster where
  hashWithSalt _salt DeleteCluster' {..} =
    _salt `Prelude.hashWithSalt` clusterArn

instance Prelude.NFData DeleteCluster where
  rnf DeleteCluster' {..} = Prelude.rnf clusterArn

instance Data.ToHeaders DeleteCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteCluster where
  toPath DeleteCluster' {..} =
    Prelude.mconcat ["/cluster/", Data.toBS clusterArn]

instance Data.ToQuery DeleteCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteClusterResponse' smart constructor.
data DeleteClusterResponse = DeleteClusterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns information about the newly deleted Elastic DocumentDB cluster.
    cluster :: Cluster
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
-- 'httpStatus', 'deleteClusterResponse_httpStatus' - The response's http status code.
--
-- 'cluster', 'deleteClusterResponse_cluster' - Returns information about the newly deleted Elastic DocumentDB cluster.
newDeleteClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'cluster'
  Cluster ->
  DeleteClusterResponse
newDeleteClusterResponse pHttpStatus_ pCluster_ =
  DeleteClusterResponse'
    { httpStatus = pHttpStatus_,
      cluster = pCluster_
    }

-- | The response's http status code.
deleteClusterResponse_httpStatus :: Lens.Lens' DeleteClusterResponse Prelude.Int
deleteClusterResponse_httpStatus = Lens.lens (\DeleteClusterResponse' {httpStatus} -> httpStatus) (\s@DeleteClusterResponse' {} a -> s {httpStatus = a} :: DeleteClusterResponse)

-- | Returns information about the newly deleted Elastic DocumentDB cluster.
deleteClusterResponse_cluster :: Lens.Lens' DeleteClusterResponse Cluster
deleteClusterResponse_cluster = Lens.lens (\DeleteClusterResponse' {cluster} -> cluster) (\s@DeleteClusterResponse' {} a -> s {cluster = a} :: DeleteClusterResponse)

instance Prelude.NFData DeleteClusterResponse where
  rnf DeleteClusterResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf cluster
