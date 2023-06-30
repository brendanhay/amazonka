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
-- Module      : Amazonka.AppMesh.UpdateMesh
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing service mesh.
module Amazonka.AppMesh.UpdateMesh
  ( -- * Creating a Request
    UpdateMesh (..),
    newUpdateMesh,

    -- * Request Lenses
    updateMesh_clientToken,
    updateMesh_spec,
    updateMesh_meshName,

    -- * Destructuring the Response
    UpdateMeshResponse (..),
    newUpdateMeshResponse,

    -- * Response Lenses
    updateMeshResponse_httpStatus,
    updateMeshResponse_mesh,
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
-- /See:/ 'newUpdateMesh' smart constructor.
data UpdateMesh = UpdateMesh'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. Up to 36 letters, numbers, hyphens, and
    -- underscores are allowed.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The service mesh specification to apply.
    spec :: Prelude.Maybe MeshSpec,
    -- | The name of the service mesh to update.
    meshName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMesh' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateMesh_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
--
-- 'spec', 'updateMesh_spec' - The service mesh specification to apply.
--
-- 'meshName', 'updateMesh_meshName' - The name of the service mesh to update.
newUpdateMesh ::
  -- | 'meshName'
  Prelude.Text ->
  UpdateMesh
newUpdateMesh pMeshName_ =
  UpdateMesh'
    { clientToken = Prelude.Nothing,
      spec = Prelude.Nothing,
      meshName = pMeshName_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
updateMesh_clientToken :: Lens.Lens' UpdateMesh (Prelude.Maybe Prelude.Text)
updateMesh_clientToken = Lens.lens (\UpdateMesh' {clientToken} -> clientToken) (\s@UpdateMesh' {} a -> s {clientToken = a} :: UpdateMesh)

-- | The service mesh specification to apply.
updateMesh_spec :: Lens.Lens' UpdateMesh (Prelude.Maybe MeshSpec)
updateMesh_spec = Lens.lens (\UpdateMesh' {spec} -> spec) (\s@UpdateMesh' {} a -> s {spec = a} :: UpdateMesh)

-- | The name of the service mesh to update.
updateMesh_meshName :: Lens.Lens' UpdateMesh Prelude.Text
updateMesh_meshName = Lens.lens (\UpdateMesh' {meshName} -> meshName) (\s@UpdateMesh' {} a -> s {meshName = a} :: UpdateMesh)

instance Core.AWSRequest UpdateMesh where
  type AWSResponse UpdateMesh = UpdateMeshResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMeshResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateMesh where
  hashWithSalt _salt UpdateMesh' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` meshName

instance Prelude.NFData UpdateMesh where
  rnf UpdateMesh' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf meshName

instance Data.ToHeaders UpdateMesh where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMesh where
  toJSON UpdateMesh' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("spec" Data..=) Prelude.<$> spec
          ]
      )

instance Data.ToPath UpdateMesh where
  toPath UpdateMesh' {..} =
    Prelude.mconcat
      ["/v20190125/meshes/", Data.toBS meshName]

instance Data.ToQuery UpdateMesh where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newUpdateMeshResponse' smart constructor.
data UpdateMeshResponse = UpdateMeshResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    mesh :: MeshData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMeshResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateMeshResponse_httpStatus' - The response's http status code.
--
-- 'mesh', 'updateMeshResponse_mesh' - Undocumented member.
newUpdateMeshResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'mesh'
  MeshData ->
  UpdateMeshResponse
newUpdateMeshResponse pHttpStatus_ pMesh_ =
  UpdateMeshResponse'
    { httpStatus = pHttpStatus_,
      mesh = pMesh_
    }

-- | The response's http status code.
updateMeshResponse_httpStatus :: Lens.Lens' UpdateMeshResponse Prelude.Int
updateMeshResponse_httpStatus = Lens.lens (\UpdateMeshResponse' {httpStatus} -> httpStatus) (\s@UpdateMeshResponse' {} a -> s {httpStatus = a} :: UpdateMeshResponse)

-- | Undocumented member.
updateMeshResponse_mesh :: Lens.Lens' UpdateMeshResponse MeshData
updateMeshResponse_mesh = Lens.lens (\UpdateMeshResponse' {mesh} -> mesh) (\s@UpdateMeshResponse' {} a -> s {mesh = a} :: UpdateMeshResponse)

instance Prelude.NFData UpdateMeshResponse where
  rnf UpdateMeshResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf mesh
