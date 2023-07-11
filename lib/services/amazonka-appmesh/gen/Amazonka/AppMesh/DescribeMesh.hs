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
-- Module      : Amazonka.AppMesh.DescribeMesh
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing service mesh.
module Amazonka.AppMesh.DescribeMesh
  ( -- * Creating a Request
    DescribeMesh (..),
    newDescribeMesh,

    -- * Request Lenses
    describeMesh_meshOwner,
    describeMesh_meshName,

    -- * Destructuring the Response
    DescribeMeshResponse (..),
    newDescribeMeshResponse,

    -- * Response Lenses
    describeMeshResponse_httpStatus,
    describeMeshResponse_mesh,
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
-- /See:/ 'newDescribeMesh' smart constructor.
data DescribeMesh = DescribeMesh'
  { -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the service mesh to describe.
    meshName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMesh' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meshOwner', 'describeMesh_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'meshName', 'describeMesh_meshName' - The name of the service mesh to describe.
newDescribeMesh ::
  -- | 'meshName'
  Prelude.Text ->
  DescribeMesh
newDescribeMesh pMeshName_ =
  DescribeMesh'
    { meshOwner = Prelude.Nothing,
      meshName = pMeshName_
    }

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
describeMesh_meshOwner :: Lens.Lens' DescribeMesh (Prelude.Maybe Prelude.Text)
describeMesh_meshOwner = Lens.lens (\DescribeMesh' {meshOwner} -> meshOwner) (\s@DescribeMesh' {} a -> s {meshOwner = a} :: DescribeMesh)

-- | The name of the service mesh to describe.
describeMesh_meshName :: Lens.Lens' DescribeMesh Prelude.Text
describeMesh_meshName = Lens.lens (\DescribeMesh' {meshName} -> meshName) (\s@DescribeMesh' {} a -> s {meshName = a} :: DescribeMesh)

instance Core.AWSRequest DescribeMesh where
  type AWSResponse DescribeMesh = DescribeMeshResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMeshResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DescribeMesh where
  hashWithSalt _salt DescribeMesh' {..} =
    _salt
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` meshName

instance Prelude.NFData DescribeMesh where
  rnf DescribeMesh' {..} =
    Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf meshName

instance Data.ToHeaders DescribeMesh where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeMesh where
  toPath DescribeMesh' {..} =
    Prelude.mconcat
      ["/v20190125/meshes/", Data.toBS meshName]

instance Data.ToQuery DescribeMesh where
  toQuery DescribeMesh' {..} =
    Prelude.mconcat ["meshOwner" Data.=: meshOwner]

-- |
--
-- /See:/ 'newDescribeMeshResponse' smart constructor.
data DescribeMeshResponse = DescribeMeshResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The full description of your service mesh.
    mesh :: MeshData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMeshResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeMeshResponse_httpStatus' - The response's http status code.
--
-- 'mesh', 'describeMeshResponse_mesh' - The full description of your service mesh.
newDescribeMeshResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'mesh'
  MeshData ->
  DescribeMeshResponse
newDescribeMeshResponse pHttpStatus_ pMesh_ =
  DescribeMeshResponse'
    { httpStatus = pHttpStatus_,
      mesh = pMesh_
    }

-- | The response's http status code.
describeMeshResponse_httpStatus :: Lens.Lens' DescribeMeshResponse Prelude.Int
describeMeshResponse_httpStatus = Lens.lens (\DescribeMeshResponse' {httpStatus} -> httpStatus) (\s@DescribeMeshResponse' {} a -> s {httpStatus = a} :: DescribeMeshResponse)

-- | The full description of your service mesh.
describeMeshResponse_mesh :: Lens.Lens' DescribeMeshResponse MeshData
describeMeshResponse_mesh = Lens.lens (\DescribeMeshResponse' {mesh} -> mesh) (\s@DescribeMeshResponse' {} a -> s {mesh = a} :: DescribeMeshResponse)

instance Prelude.NFData DescribeMeshResponse where
  rnf DescribeMeshResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf mesh
