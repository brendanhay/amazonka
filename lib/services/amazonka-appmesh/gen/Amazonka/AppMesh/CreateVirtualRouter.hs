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
-- Module      : Amazonka.AppMesh.CreateVirtualRouter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a virtual router within a service mesh.
--
-- Specify a @listener@ for any inbound traffic that your virtual router
-- receives. Create a virtual router for each protocol and port that you
-- need to route. Virtual routers handle traffic for one or more virtual
-- services within your mesh. After you create your virtual router, create
-- and associate routes for your virtual router that direct incoming
-- requests to different virtual nodes.
--
-- For more information about virtual routers, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/virtual_routers.html Virtual routers>.
module Amazonka.AppMesh.CreateVirtualRouter
  ( -- * Creating a Request
    CreateVirtualRouter (..),
    newCreateVirtualRouter,

    -- * Request Lenses
    createVirtualRouter_tags,
    createVirtualRouter_clientToken,
    createVirtualRouter_meshOwner,
    createVirtualRouter_meshName,
    createVirtualRouter_spec,
    createVirtualRouter_virtualRouterName,

    -- * Destructuring the Response
    CreateVirtualRouterResponse (..),
    newCreateVirtualRouterResponse,

    -- * Response Lenses
    createVirtualRouterResponse_httpStatus,
    createVirtualRouterResponse_virtualRouter,
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
-- /See:/ 'newCreateVirtualRouter' smart constructor.
data CreateVirtualRouter = CreateVirtualRouter'
  { -- | Optional metadata that you can apply to the virtual router to assist
    -- with categorization and organization. Each tag consists of a key and an
    -- optional value, both of which you define. Tag keys can have a maximum
    -- character length of 128 characters, and tag values can have a maximum
    -- length of 256 characters.
    tags :: Prelude.Maybe [TagRef],
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. Up to 36 letters, numbers, hyphens, and
    -- underscores are allowed.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then the account that you specify must share
    -- the mesh with your account before you can create the resource in the
    -- service mesh. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the service mesh to create the virtual router in.
    meshName :: Prelude.Text,
    -- | The virtual router specification to apply.
    spec :: VirtualRouterSpec,
    -- | The name to use for the virtual router.
    virtualRouterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVirtualRouter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createVirtualRouter_tags' - Optional metadata that you can apply to the virtual router to assist
-- with categorization and organization. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
--
-- 'clientToken', 'createVirtualRouter_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
--
-- 'meshOwner', 'createVirtualRouter_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then the account that you specify must share
-- the mesh with your account before you can create the resource in the
-- service mesh. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'meshName', 'createVirtualRouter_meshName' - The name of the service mesh to create the virtual router in.
--
-- 'spec', 'createVirtualRouter_spec' - The virtual router specification to apply.
--
-- 'virtualRouterName', 'createVirtualRouter_virtualRouterName' - The name to use for the virtual router.
newCreateVirtualRouter ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'spec'
  VirtualRouterSpec ->
  -- | 'virtualRouterName'
  Prelude.Text ->
  CreateVirtualRouter
newCreateVirtualRouter
  pMeshName_
  pSpec_
  pVirtualRouterName_ =
    CreateVirtualRouter'
      { tags = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        meshOwner = Prelude.Nothing,
        meshName = pMeshName_,
        spec = pSpec_,
        virtualRouterName = pVirtualRouterName_
      }

-- | Optional metadata that you can apply to the virtual router to assist
-- with categorization and organization. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
createVirtualRouter_tags :: Lens.Lens' CreateVirtualRouter (Prelude.Maybe [TagRef])
createVirtualRouter_tags = Lens.lens (\CreateVirtualRouter' {tags} -> tags) (\s@CreateVirtualRouter' {} a -> s {tags = a} :: CreateVirtualRouter) Prelude.. Lens.mapping Lens.coerced

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
createVirtualRouter_clientToken :: Lens.Lens' CreateVirtualRouter (Prelude.Maybe Prelude.Text)
createVirtualRouter_clientToken = Lens.lens (\CreateVirtualRouter' {clientToken} -> clientToken) (\s@CreateVirtualRouter' {} a -> s {clientToken = a} :: CreateVirtualRouter)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then the account that you specify must share
-- the mesh with your account before you can create the resource in the
-- service mesh. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
createVirtualRouter_meshOwner :: Lens.Lens' CreateVirtualRouter (Prelude.Maybe Prelude.Text)
createVirtualRouter_meshOwner = Lens.lens (\CreateVirtualRouter' {meshOwner} -> meshOwner) (\s@CreateVirtualRouter' {} a -> s {meshOwner = a} :: CreateVirtualRouter)

-- | The name of the service mesh to create the virtual router in.
createVirtualRouter_meshName :: Lens.Lens' CreateVirtualRouter Prelude.Text
createVirtualRouter_meshName = Lens.lens (\CreateVirtualRouter' {meshName} -> meshName) (\s@CreateVirtualRouter' {} a -> s {meshName = a} :: CreateVirtualRouter)

-- | The virtual router specification to apply.
createVirtualRouter_spec :: Lens.Lens' CreateVirtualRouter VirtualRouterSpec
createVirtualRouter_spec = Lens.lens (\CreateVirtualRouter' {spec} -> spec) (\s@CreateVirtualRouter' {} a -> s {spec = a} :: CreateVirtualRouter)

-- | The name to use for the virtual router.
createVirtualRouter_virtualRouterName :: Lens.Lens' CreateVirtualRouter Prelude.Text
createVirtualRouter_virtualRouterName = Lens.lens (\CreateVirtualRouter' {virtualRouterName} -> virtualRouterName) (\s@CreateVirtualRouter' {} a -> s {virtualRouterName = a} :: CreateVirtualRouter)

instance Core.AWSRequest CreateVirtualRouter where
  type
    AWSResponse CreateVirtualRouter =
      CreateVirtualRouterResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVirtualRouterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable CreateVirtualRouter where
  hashWithSalt _salt CreateVirtualRouter' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` virtualRouterName

instance Prelude.NFData CreateVirtualRouter where
  rnf CreateVirtualRouter' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf virtualRouterName

instance Data.ToHeaders CreateVirtualRouter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVirtualRouter where
  toJSON CreateVirtualRouter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("spec" Data..= spec),
            Prelude.Just
              ("virtualRouterName" Data..= virtualRouterName)
          ]
      )

instance Data.ToPath CreateVirtualRouter where
  toPath CreateVirtualRouter' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Data.toBS meshName,
        "/virtualRouters"
      ]

instance Data.ToQuery CreateVirtualRouter where
  toQuery CreateVirtualRouter' {..} =
    Prelude.mconcat ["meshOwner" Data.=: meshOwner]

-- |
--
-- /See:/ 'newCreateVirtualRouterResponse' smart constructor.
data CreateVirtualRouterResponse = CreateVirtualRouterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The full description of your virtual router following the create call.
    virtualRouter :: VirtualRouterData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVirtualRouterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createVirtualRouterResponse_httpStatus' - The response's http status code.
--
-- 'virtualRouter', 'createVirtualRouterResponse_virtualRouter' - The full description of your virtual router following the create call.
newCreateVirtualRouterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'virtualRouter'
  VirtualRouterData ->
  CreateVirtualRouterResponse
newCreateVirtualRouterResponse
  pHttpStatus_
  pVirtualRouter_ =
    CreateVirtualRouterResponse'
      { httpStatus =
          pHttpStatus_,
        virtualRouter = pVirtualRouter_
      }

-- | The response's http status code.
createVirtualRouterResponse_httpStatus :: Lens.Lens' CreateVirtualRouterResponse Prelude.Int
createVirtualRouterResponse_httpStatus = Lens.lens (\CreateVirtualRouterResponse' {httpStatus} -> httpStatus) (\s@CreateVirtualRouterResponse' {} a -> s {httpStatus = a} :: CreateVirtualRouterResponse)

-- | The full description of your virtual router following the create call.
createVirtualRouterResponse_virtualRouter :: Lens.Lens' CreateVirtualRouterResponse VirtualRouterData
createVirtualRouterResponse_virtualRouter = Lens.lens (\CreateVirtualRouterResponse' {virtualRouter} -> virtualRouter) (\s@CreateVirtualRouterResponse' {} a -> s {virtualRouter = a} :: CreateVirtualRouterResponse)

instance Prelude.NFData CreateVirtualRouterResponse where
  rnf CreateVirtualRouterResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf virtualRouter
