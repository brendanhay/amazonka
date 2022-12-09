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
-- Module      : Amazonka.AppMesh.CreateVirtualService
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a virtual service within a service mesh.
--
-- A virtual service is an abstraction of a real service that is provided
-- by a virtual node directly or indirectly by means of a virtual router.
-- Dependent services call your virtual service by its
-- @virtualServiceName@, and those requests are routed to the virtual node
-- or virtual router that is specified as the provider for the virtual
-- service.
--
-- For more information about virtual services, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/virtual_services.html Virtual services>.
module Amazonka.AppMesh.CreateVirtualService
  ( -- * Creating a Request
    CreateVirtualService (..),
    newCreateVirtualService,

    -- * Request Lenses
    createVirtualService_clientToken,
    createVirtualService_meshOwner,
    createVirtualService_tags,
    createVirtualService_meshName,
    createVirtualService_spec,
    createVirtualService_virtualServiceName,

    -- * Destructuring the Response
    CreateVirtualServiceResponse (..),
    newCreateVirtualServiceResponse,

    -- * Response Lenses
    createVirtualServiceResponse_httpStatus,
    createVirtualServiceResponse_virtualService,
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
-- /See:/ 'newCreateVirtualService' smart constructor.
data CreateVirtualService = CreateVirtualService'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. Up to 36 letters, numbers, hyphens, and
    -- underscores are allowed.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then the account that you specify must share
    -- the mesh with your account before you can create the resource in the
    -- service mesh. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | Optional metadata that you can apply to the virtual service to assist
    -- with categorization and organization. Each tag consists of a key and an
    -- optional value, both of which you define. Tag keys can have a maximum
    -- character length of 128 characters, and tag values can have a maximum
    -- length of 256 characters.
    tags :: Prelude.Maybe [TagRef],
    -- | The name of the service mesh to create the virtual service in.
    meshName :: Prelude.Text,
    -- | The virtual service specification to apply.
    spec :: VirtualServiceSpec,
    -- | The name to use for the virtual service.
    virtualServiceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVirtualService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createVirtualService_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
--
-- 'meshOwner', 'createVirtualService_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then the account that you specify must share
-- the mesh with your account before you can create the resource in the
-- service mesh. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'tags', 'createVirtualService_tags' - Optional metadata that you can apply to the virtual service to assist
-- with categorization and organization. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
--
-- 'meshName', 'createVirtualService_meshName' - The name of the service mesh to create the virtual service in.
--
-- 'spec', 'createVirtualService_spec' - The virtual service specification to apply.
--
-- 'virtualServiceName', 'createVirtualService_virtualServiceName' - The name to use for the virtual service.
newCreateVirtualService ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'spec'
  VirtualServiceSpec ->
  -- | 'virtualServiceName'
  Prelude.Text ->
  CreateVirtualService
newCreateVirtualService
  pMeshName_
  pSpec_
  pVirtualServiceName_ =
    CreateVirtualService'
      { clientToken =
          Prelude.Nothing,
        meshOwner = Prelude.Nothing,
        tags = Prelude.Nothing,
        meshName = pMeshName_,
        spec = pSpec_,
        virtualServiceName = pVirtualServiceName_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
createVirtualService_clientToken :: Lens.Lens' CreateVirtualService (Prelude.Maybe Prelude.Text)
createVirtualService_clientToken = Lens.lens (\CreateVirtualService' {clientToken} -> clientToken) (\s@CreateVirtualService' {} a -> s {clientToken = a} :: CreateVirtualService)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then the account that you specify must share
-- the mesh with your account before you can create the resource in the
-- service mesh. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
createVirtualService_meshOwner :: Lens.Lens' CreateVirtualService (Prelude.Maybe Prelude.Text)
createVirtualService_meshOwner = Lens.lens (\CreateVirtualService' {meshOwner} -> meshOwner) (\s@CreateVirtualService' {} a -> s {meshOwner = a} :: CreateVirtualService)

-- | Optional metadata that you can apply to the virtual service to assist
-- with categorization and organization. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
createVirtualService_tags :: Lens.Lens' CreateVirtualService (Prelude.Maybe [TagRef])
createVirtualService_tags = Lens.lens (\CreateVirtualService' {tags} -> tags) (\s@CreateVirtualService' {} a -> s {tags = a} :: CreateVirtualService) Prelude.. Lens.mapping Lens.coerced

-- | The name of the service mesh to create the virtual service in.
createVirtualService_meshName :: Lens.Lens' CreateVirtualService Prelude.Text
createVirtualService_meshName = Lens.lens (\CreateVirtualService' {meshName} -> meshName) (\s@CreateVirtualService' {} a -> s {meshName = a} :: CreateVirtualService)

-- | The virtual service specification to apply.
createVirtualService_spec :: Lens.Lens' CreateVirtualService VirtualServiceSpec
createVirtualService_spec = Lens.lens (\CreateVirtualService' {spec} -> spec) (\s@CreateVirtualService' {} a -> s {spec = a} :: CreateVirtualService)

-- | The name to use for the virtual service.
createVirtualService_virtualServiceName :: Lens.Lens' CreateVirtualService Prelude.Text
createVirtualService_virtualServiceName = Lens.lens (\CreateVirtualService' {virtualServiceName} -> virtualServiceName) (\s@CreateVirtualService' {} a -> s {virtualServiceName = a} :: CreateVirtualService)

instance Core.AWSRequest CreateVirtualService where
  type
    AWSResponse CreateVirtualService =
      CreateVirtualServiceResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVirtualServiceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable CreateVirtualService where
  hashWithSalt _salt CreateVirtualService' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` virtualServiceName

instance Prelude.NFData CreateVirtualService where
  rnf CreateVirtualService' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf virtualServiceName

instance Data.ToHeaders CreateVirtualService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVirtualService where
  toJSON CreateVirtualService' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("spec" Data..= spec),
            Prelude.Just
              ("virtualServiceName" Data..= virtualServiceName)
          ]
      )

instance Data.ToPath CreateVirtualService where
  toPath CreateVirtualService' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Data.toBS meshName,
        "/virtualServices"
      ]

instance Data.ToQuery CreateVirtualService where
  toQuery CreateVirtualService' {..} =
    Prelude.mconcat ["meshOwner" Data.=: meshOwner]

-- |
--
-- /See:/ 'newCreateVirtualServiceResponse' smart constructor.
data CreateVirtualServiceResponse = CreateVirtualServiceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The full description of your virtual service following the create call.
    virtualService :: VirtualServiceData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVirtualServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createVirtualServiceResponse_httpStatus' - The response's http status code.
--
-- 'virtualService', 'createVirtualServiceResponse_virtualService' - The full description of your virtual service following the create call.
newCreateVirtualServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'virtualService'
  VirtualServiceData ->
  CreateVirtualServiceResponse
newCreateVirtualServiceResponse
  pHttpStatus_
  pVirtualService_ =
    CreateVirtualServiceResponse'
      { httpStatus =
          pHttpStatus_,
        virtualService = pVirtualService_
      }

-- | The response's http status code.
createVirtualServiceResponse_httpStatus :: Lens.Lens' CreateVirtualServiceResponse Prelude.Int
createVirtualServiceResponse_httpStatus = Lens.lens (\CreateVirtualServiceResponse' {httpStatus} -> httpStatus) (\s@CreateVirtualServiceResponse' {} a -> s {httpStatus = a} :: CreateVirtualServiceResponse)

-- | The full description of your virtual service following the create call.
createVirtualServiceResponse_virtualService :: Lens.Lens' CreateVirtualServiceResponse VirtualServiceData
createVirtualServiceResponse_virtualService = Lens.lens (\CreateVirtualServiceResponse' {virtualService} -> virtualService) (\s@CreateVirtualServiceResponse' {} a -> s {virtualService = a} :: CreateVirtualServiceResponse)

instance Prelude.NFData CreateVirtualServiceResponse where
  rnf CreateVirtualServiceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf virtualService
