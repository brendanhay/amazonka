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
-- Module      : Amazonka.AppMesh.CreateMesh
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a service mesh.
--
-- A service mesh is a logical boundary for network traffic between
-- services that are represented by resources within the mesh. After you
-- create your service mesh, you can create virtual services, virtual
-- nodes, virtual routers, and routes to distribute traffic between the
-- applications in your mesh.
--
-- For more information about service meshes, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/meshes.html Service meshes>.
module Amazonka.AppMesh.CreateMesh
  ( -- * Creating a Request
    CreateMesh (..),
    newCreateMesh,

    -- * Request Lenses
    createMesh_clientToken,
    createMesh_spec,
    createMesh_tags,
    createMesh_meshName,

    -- * Destructuring the Response
    CreateMeshResponse (..),
    newCreateMeshResponse,

    -- * Response Lenses
    createMeshResponse_httpStatus,
    createMeshResponse_mesh,
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
-- /See:/ 'newCreateMesh' smart constructor.
data CreateMesh = CreateMesh'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. Up to 36 letters, numbers, hyphens, and
    -- underscores are allowed.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The service mesh specification to apply.
    spec :: Prelude.Maybe MeshSpec,
    -- | Optional metadata that you can apply to the service mesh to assist with
    -- categorization and organization. Each tag consists of a key and an
    -- optional value, both of which you define. Tag keys can have a maximum
    -- character length of 128 characters, and tag values can have a maximum
    -- length of 256 characters.
    tags :: Prelude.Maybe [TagRef],
    -- | The name to use for the service mesh.
    meshName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMesh' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createMesh_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
--
-- 'spec', 'createMesh_spec' - The service mesh specification to apply.
--
-- 'tags', 'createMesh_tags' - Optional metadata that you can apply to the service mesh to assist with
-- categorization and organization. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
--
-- 'meshName', 'createMesh_meshName' - The name to use for the service mesh.
newCreateMesh ::
  -- | 'meshName'
  Prelude.Text ->
  CreateMesh
newCreateMesh pMeshName_ =
  CreateMesh'
    { clientToken = Prelude.Nothing,
      spec = Prelude.Nothing,
      tags = Prelude.Nothing,
      meshName = pMeshName_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
createMesh_clientToken :: Lens.Lens' CreateMesh (Prelude.Maybe Prelude.Text)
createMesh_clientToken = Lens.lens (\CreateMesh' {clientToken} -> clientToken) (\s@CreateMesh' {} a -> s {clientToken = a} :: CreateMesh)

-- | The service mesh specification to apply.
createMesh_spec :: Lens.Lens' CreateMesh (Prelude.Maybe MeshSpec)
createMesh_spec = Lens.lens (\CreateMesh' {spec} -> spec) (\s@CreateMesh' {} a -> s {spec = a} :: CreateMesh)

-- | Optional metadata that you can apply to the service mesh to assist with
-- categorization and organization. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
createMesh_tags :: Lens.Lens' CreateMesh (Prelude.Maybe [TagRef])
createMesh_tags = Lens.lens (\CreateMesh' {tags} -> tags) (\s@CreateMesh' {} a -> s {tags = a} :: CreateMesh) Prelude.. Lens.mapping Lens.coerced

-- | The name to use for the service mesh.
createMesh_meshName :: Lens.Lens' CreateMesh Prelude.Text
createMesh_meshName = Lens.lens (\CreateMesh' {meshName} -> meshName) (\s@CreateMesh' {} a -> s {meshName = a} :: CreateMesh)

instance Core.AWSRequest CreateMesh where
  type AWSResponse CreateMesh = CreateMeshResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMeshResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable CreateMesh where
  hashWithSalt _salt CreateMesh' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` meshName

instance Prelude.NFData CreateMesh where
  rnf CreateMesh' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf meshName

instance Data.ToHeaders CreateMesh where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateMesh where
  toJSON CreateMesh' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("spec" Data..=) Prelude.<$> spec,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("meshName" Data..= meshName)
          ]
      )

instance Data.ToPath CreateMesh where
  toPath = Prelude.const "/v20190125/meshes"

instance Data.ToQuery CreateMesh where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newCreateMeshResponse' smart constructor.
data CreateMeshResponse = CreateMeshResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The full description of your service mesh following the create call.
    mesh :: MeshData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMeshResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createMeshResponse_httpStatus' - The response's http status code.
--
-- 'mesh', 'createMeshResponse_mesh' - The full description of your service mesh following the create call.
newCreateMeshResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'mesh'
  MeshData ->
  CreateMeshResponse
newCreateMeshResponse pHttpStatus_ pMesh_ =
  CreateMeshResponse'
    { httpStatus = pHttpStatus_,
      mesh = pMesh_
    }

-- | The response's http status code.
createMeshResponse_httpStatus :: Lens.Lens' CreateMeshResponse Prelude.Int
createMeshResponse_httpStatus = Lens.lens (\CreateMeshResponse' {httpStatus} -> httpStatus) (\s@CreateMeshResponse' {} a -> s {httpStatus = a} :: CreateMeshResponse)

-- | The full description of your service mesh following the create call.
createMeshResponse_mesh :: Lens.Lens' CreateMeshResponse MeshData
createMeshResponse_mesh = Lens.lens (\CreateMeshResponse' {mesh} -> mesh) (\s@CreateMeshResponse' {} a -> s {mesh = a} :: CreateMeshResponse)

instance Prelude.NFData CreateMeshResponse where
  rnf CreateMeshResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf mesh
