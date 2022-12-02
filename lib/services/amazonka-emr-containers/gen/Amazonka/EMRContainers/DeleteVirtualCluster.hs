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
-- Module      : Amazonka.EMRContainers.DeleteVirtualCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a virtual cluster. Virtual cluster is a managed entity on Amazon
-- EMR on EKS. You can create, describe, list and delete virtual clusters.
-- They do not consume any additional resource in your system. A single
-- virtual cluster maps to a single Kubernetes namespace. Given this
-- relationship, you can model virtual clusters the same way you model
-- Kubernetes namespaces to meet your requirements.
module Amazonka.EMRContainers.DeleteVirtualCluster
  ( -- * Creating a Request
    DeleteVirtualCluster (..),
    newDeleteVirtualCluster,

    -- * Request Lenses
    deleteVirtualCluster_id,

    -- * Destructuring the Response
    DeleteVirtualClusterResponse (..),
    newDeleteVirtualClusterResponse,

    -- * Response Lenses
    deleteVirtualClusterResponse_id,
    deleteVirtualClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRContainers.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVirtualCluster' smart constructor.
data DeleteVirtualCluster = DeleteVirtualCluster'
  { -- | The ID of the virtual cluster that will be deleted.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVirtualCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteVirtualCluster_id' - The ID of the virtual cluster that will be deleted.
newDeleteVirtualCluster ::
  -- | 'id'
  Prelude.Text ->
  DeleteVirtualCluster
newDeleteVirtualCluster pId_ =
  DeleteVirtualCluster' {id = pId_}

-- | The ID of the virtual cluster that will be deleted.
deleteVirtualCluster_id :: Lens.Lens' DeleteVirtualCluster Prelude.Text
deleteVirtualCluster_id = Lens.lens (\DeleteVirtualCluster' {id} -> id) (\s@DeleteVirtualCluster' {} a -> s {id = a} :: DeleteVirtualCluster)

instance Core.AWSRequest DeleteVirtualCluster where
  type
    AWSResponse DeleteVirtualCluster =
      DeleteVirtualClusterResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVirtualClusterResponse'
            Prelude.<$> (x Data..?> "id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteVirtualCluster where
  hashWithSalt _salt DeleteVirtualCluster' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteVirtualCluster where
  rnf DeleteVirtualCluster' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteVirtualCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteVirtualCluster where
  toPath DeleteVirtualCluster' {..} =
    Prelude.mconcat ["/virtualclusters/", Data.toBS id]

instance Data.ToQuery DeleteVirtualCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVirtualClusterResponse' smart constructor.
data DeleteVirtualClusterResponse = DeleteVirtualClusterResponse'
  { -- | This output contains the ID of the virtual cluster that will be deleted.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVirtualClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteVirtualClusterResponse_id' - This output contains the ID of the virtual cluster that will be deleted.
--
-- 'httpStatus', 'deleteVirtualClusterResponse_httpStatus' - The response's http status code.
newDeleteVirtualClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteVirtualClusterResponse
newDeleteVirtualClusterResponse pHttpStatus_ =
  DeleteVirtualClusterResponse'
    { id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This output contains the ID of the virtual cluster that will be deleted.
deleteVirtualClusterResponse_id :: Lens.Lens' DeleteVirtualClusterResponse (Prelude.Maybe Prelude.Text)
deleteVirtualClusterResponse_id = Lens.lens (\DeleteVirtualClusterResponse' {id} -> id) (\s@DeleteVirtualClusterResponse' {} a -> s {id = a} :: DeleteVirtualClusterResponse)

-- | The response's http status code.
deleteVirtualClusterResponse_httpStatus :: Lens.Lens' DeleteVirtualClusterResponse Prelude.Int
deleteVirtualClusterResponse_httpStatus = Lens.lens (\DeleteVirtualClusterResponse' {httpStatus} -> httpStatus) (\s@DeleteVirtualClusterResponse' {} a -> s {httpStatus = a} :: DeleteVirtualClusterResponse)

instance Prelude.NFData DeleteVirtualClusterResponse where
  rnf DeleteVirtualClusterResponse' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf httpStatus
