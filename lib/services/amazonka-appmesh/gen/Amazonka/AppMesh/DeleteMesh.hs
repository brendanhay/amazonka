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
-- Module      : Amazonka.AppMesh.DeleteMesh
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing service mesh.
--
-- You must delete all resources (virtual services, routes, virtual
-- routers, and virtual nodes) in the service mesh before you can delete
-- the mesh itself.
module Amazonka.AppMesh.DeleteMesh
  ( -- * Creating a Request
    DeleteMesh (..),
    newDeleteMesh,

    -- * Request Lenses
    deleteMesh_meshName,

    -- * Destructuring the Response
    DeleteMeshResponse (..),
    newDeleteMeshResponse,

    -- * Response Lenses
    deleteMeshResponse_httpStatus,
    deleteMeshResponse_mesh,
  )
where

import Amazonka.AppMesh.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDeleteMesh' smart constructor.
data DeleteMesh = DeleteMesh'
  { -- | The name of the service mesh to delete.
    meshName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMesh' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meshName', 'deleteMesh_meshName' - The name of the service mesh to delete.
newDeleteMesh ::
  -- | 'meshName'
  Prelude.Text ->
  DeleteMesh
newDeleteMesh pMeshName_ =
  DeleteMesh' {meshName = pMeshName_}

-- | The name of the service mesh to delete.
deleteMesh_meshName :: Lens.Lens' DeleteMesh Prelude.Text
deleteMesh_meshName = Lens.lens (\DeleteMesh' {meshName} -> meshName) (\s@DeleteMesh' {} a -> s {meshName = a} :: DeleteMesh)

instance Core.AWSRequest DeleteMesh where
  type AWSResponse DeleteMesh = DeleteMeshResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteMeshResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteMesh where
  hashWithSalt _salt DeleteMesh' {..} =
    _salt `Prelude.hashWithSalt` meshName

instance Prelude.NFData DeleteMesh where
  rnf DeleteMesh' {..} = Prelude.rnf meshName

instance Data.ToHeaders DeleteMesh where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteMesh where
  toPath DeleteMesh' {..} =
    Prelude.mconcat
      ["/v20190125/meshes/", Data.toBS meshName]

instance Data.ToQuery DeleteMesh where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDeleteMeshResponse' smart constructor.
data DeleteMeshResponse = DeleteMeshResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The service mesh that was deleted.
    mesh :: MeshData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMeshResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteMeshResponse_httpStatus' - The response's http status code.
--
-- 'mesh', 'deleteMeshResponse_mesh' - The service mesh that was deleted.
newDeleteMeshResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'mesh'
  MeshData ->
  DeleteMeshResponse
newDeleteMeshResponse pHttpStatus_ pMesh_ =
  DeleteMeshResponse'
    { httpStatus = pHttpStatus_,
      mesh = pMesh_
    }

-- | The response's http status code.
deleteMeshResponse_httpStatus :: Lens.Lens' DeleteMeshResponse Prelude.Int
deleteMeshResponse_httpStatus = Lens.lens (\DeleteMeshResponse' {httpStatus} -> httpStatus) (\s@DeleteMeshResponse' {} a -> s {httpStatus = a} :: DeleteMeshResponse)

-- | The service mesh that was deleted.
deleteMeshResponse_mesh :: Lens.Lens' DeleteMeshResponse MeshData
deleteMeshResponse_mesh = Lens.lens (\DeleteMeshResponse' {mesh} -> mesh) (\s@DeleteMeshResponse' {} a -> s {mesh = a} :: DeleteMeshResponse)

instance Prelude.NFData DeleteMeshResponse where
  rnf DeleteMeshResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf mesh
