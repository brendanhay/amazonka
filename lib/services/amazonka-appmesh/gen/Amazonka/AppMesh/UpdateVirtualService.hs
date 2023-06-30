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
-- Module      : Amazonka.AppMesh.UpdateVirtualService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing virtual service in a specified service mesh.
module Amazonka.AppMesh.UpdateVirtualService
  ( -- * Creating a Request
    UpdateVirtualService (..),
    newUpdateVirtualService,

    -- * Request Lenses
    updateVirtualService_clientToken,
    updateVirtualService_meshOwner,
    updateVirtualService_meshName,
    updateVirtualService_spec,
    updateVirtualService_virtualServiceName,

    -- * Destructuring the Response
    UpdateVirtualServiceResponse (..),
    newUpdateVirtualServiceResponse,

    -- * Response Lenses
    updateVirtualServiceResponse_httpStatus,
    updateVirtualServiceResponse_virtualService,
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
-- /See:/ 'newUpdateVirtualService' smart constructor.
data UpdateVirtualService = UpdateVirtualService'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. Up to 36 letters, numbers, hyphens, and
    -- underscores are allowed.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the service mesh that the virtual service resides in.
    meshName :: Prelude.Text,
    -- | The new virtual service specification to apply. This overwrites the
    -- existing data.
    spec :: VirtualServiceSpec,
    -- | The name of the virtual service to update.
    virtualServiceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVirtualService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateVirtualService_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
--
-- 'meshOwner', 'updateVirtualService_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'meshName', 'updateVirtualService_meshName' - The name of the service mesh that the virtual service resides in.
--
-- 'spec', 'updateVirtualService_spec' - The new virtual service specification to apply. This overwrites the
-- existing data.
--
-- 'virtualServiceName', 'updateVirtualService_virtualServiceName' - The name of the virtual service to update.
newUpdateVirtualService ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'spec'
  VirtualServiceSpec ->
  -- | 'virtualServiceName'
  Prelude.Text ->
  UpdateVirtualService
newUpdateVirtualService
  pMeshName_
  pSpec_
  pVirtualServiceName_ =
    UpdateVirtualService'
      { clientToken =
          Prelude.Nothing,
        meshOwner = Prelude.Nothing,
        meshName = pMeshName_,
        spec = pSpec_,
        virtualServiceName = pVirtualServiceName_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
updateVirtualService_clientToken :: Lens.Lens' UpdateVirtualService (Prelude.Maybe Prelude.Text)
updateVirtualService_clientToken = Lens.lens (\UpdateVirtualService' {clientToken} -> clientToken) (\s@UpdateVirtualService' {} a -> s {clientToken = a} :: UpdateVirtualService)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
updateVirtualService_meshOwner :: Lens.Lens' UpdateVirtualService (Prelude.Maybe Prelude.Text)
updateVirtualService_meshOwner = Lens.lens (\UpdateVirtualService' {meshOwner} -> meshOwner) (\s@UpdateVirtualService' {} a -> s {meshOwner = a} :: UpdateVirtualService)

-- | The name of the service mesh that the virtual service resides in.
updateVirtualService_meshName :: Lens.Lens' UpdateVirtualService Prelude.Text
updateVirtualService_meshName = Lens.lens (\UpdateVirtualService' {meshName} -> meshName) (\s@UpdateVirtualService' {} a -> s {meshName = a} :: UpdateVirtualService)

-- | The new virtual service specification to apply. This overwrites the
-- existing data.
updateVirtualService_spec :: Lens.Lens' UpdateVirtualService VirtualServiceSpec
updateVirtualService_spec = Lens.lens (\UpdateVirtualService' {spec} -> spec) (\s@UpdateVirtualService' {} a -> s {spec = a} :: UpdateVirtualService)

-- | The name of the virtual service to update.
updateVirtualService_virtualServiceName :: Lens.Lens' UpdateVirtualService Prelude.Text
updateVirtualService_virtualServiceName = Lens.lens (\UpdateVirtualService' {virtualServiceName} -> virtualServiceName) (\s@UpdateVirtualService' {} a -> s {virtualServiceName = a} :: UpdateVirtualService)

instance Core.AWSRequest UpdateVirtualService where
  type
    AWSResponse UpdateVirtualService =
      UpdateVirtualServiceResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVirtualServiceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateVirtualService where
  hashWithSalt _salt UpdateVirtualService' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` virtualServiceName

instance Prelude.NFData UpdateVirtualService where
  rnf UpdateVirtualService' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf virtualServiceName

instance Data.ToHeaders UpdateVirtualService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateVirtualService where
  toJSON UpdateVirtualService' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("spec" Data..= spec)
          ]
      )

instance Data.ToPath UpdateVirtualService where
  toPath UpdateVirtualService' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Data.toBS meshName,
        "/virtualServices/",
        Data.toBS virtualServiceName
      ]

instance Data.ToQuery UpdateVirtualService where
  toQuery UpdateVirtualService' {..} =
    Prelude.mconcat ["meshOwner" Data.=: meshOwner]

-- |
--
-- /See:/ 'newUpdateVirtualServiceResponse' smart constructor.
data UpdateVirtualServiceResponse = UpdateVirtualServiceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A full description of the virtual service that was updated.
    virtualService :: VirtualServiceData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVirtualServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateVirtualServiceResponse_httpStatus' - The response's http status code.
--
-- 'virtualService', 'updateVirtualServiceResponse_virtualService' - A full description of the virtual service that was updated.
newUpdateVirtualServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'virtualService'
  VirtualServiceData ->
  UpdateVirtualServiceResponse
newUpdateVirtualServiceResponse
  pHttpStatus_
  pVirtualService_ =
    UpdateVirtualServiceResponse'
      { httpStatus =
          pHttpStatus_,
        virtualService = pVirtualService_
      }

-- | The response's http status code.
updateVirtualServiceResponse_httpStatus :: Lens.Lens' UpdateVirtualServiceResponse Prelude.Int
updateVirtualServiceResponse_httpStatus = Lens.lens (\UpdateVirtualServiceResponse' {httpStatus} -> httpStatus) (\s@UpdateVirtualServiceResponse' {} a -> s {httpStatus = a} :: UpdateVirtualServiceResponse)

-- | A full description of the virtual service that was updated.
updateVirtualServiceResponse_virtualService :: Lens.Lens' UpdateVirtualServiceResponse VirtualServiceData
updateVirtualServiceResponse_virtualService = Lens.lens (\UpdateVirtualServiceResponse' {virtualService} -> virtualService) (\s@UpdateVirtualServiceResponse' {} a -> s {virtualService = a} :: UpdateVirtualServiceResponse)

instance Prelude.NFData UpdateVirtualServiceResponse where
  rnf UpdateVirtualServiceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf virtualService
