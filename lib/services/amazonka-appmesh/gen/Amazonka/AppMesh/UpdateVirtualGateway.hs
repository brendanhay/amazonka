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
-- Module      : Amazonka.AppMesh.UpdateVirtualGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing virtual gateway in a specified service mesh.
module Amazonka.AppMesh.UpdateVirtualGateway
  ( -- * Creating a Request
    UpdateVirtualGateway (..),
    newUpdateVirtualGateway,

    -- * Request Lenses
    updateVirtualGateway_clientToken,
    updateVirtualGateway_meshOwner,
    updateVirtualGateway_meshName,
    updateVirtualGateway_spec,
    updateVirtualGateway_virtualGatewayName,

    -- * Destructuring the Response
    UpdateVirtualGatewayResponse (..),
    newUpdateVirtualGatewayResponse,

    -- * Response Lenses
    updateVirtualGatewayResponse_httpStatus,
    updateVirtualGatewayResponse_virtualGateway,
  )
where

import Amazonka.AppMesh.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateVirtualGateway' smart constructor.
data UpdateVirtualGateway = UpdateVirtualGateway'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. Up to 36 letters, numbers, hyphens, and
    -- underscores are allowed.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the service mesh that the virtual gateway resides in.
    meshName :: Prelude.Text,
    -- | The new virtual gateway specification to apply. This overwrites the
    -- existing data.
    spec :: VirtualGatewaySpec,
    -- | The name of the virtual gateway to update.
    virtualGatewayName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVirtualGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateVirtualGateway_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
--
-- 'meshOwner', 'updateVirtualGateway_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'meshName', 'updateVirtualGateway_meshName' - The name of the service mesh that the virtual gateway resides in.
--
-- 'spec', 'updateVirtualGateway_spec' - The new virtual gateway specification to apply. This overwrites the
-- existing data.
--
-- 'virtualGatewayName', 'updateVirtualGateway_virtualGatewayName' - The name of the virtual gateway to update.
newUpdateVirtualGateway ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'spec'
  VirtualGatewaySpec ->
  -- | 'virtualGatewayName'
  Prelude.Text ->
  UpdateVirtualGateway
newUpdateVirtualGateway
  pMeshName_
  pSpec_
  pVirtualGatewayName_ =
    UpdateVirtualGateway'
      { clientToken =
          Prelude.Nothing,
        meshOwner = Prelude.Nothing,
        meshName = pMeshName_,
        spec = pSpec_,
        virtualGatewayName = pVirtualGatewayName_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
updateVirtualGateway_clientToken :: Lens.Lens' UpdateVirtualGateway (Prelude.Maybe Prelude.Text)
updateVirtualGateway_clientToken = Lens.lens (\UpdateVirtualGateway' {clientToken} -> clientToken) (\s@UpdateVirtualGateway' {} a -> s {clientToken = a} :: UpdateVirtualGateway)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
updateVirtualGateway_meshOwner :: Lens.Lens' UpdateVirtualGateway (Prelude.Maybe Prelude.Text)
updateVirtualGateway_meshOwner = Lens.lens (\UpdateVirtualGateway' {meshOwner} -> meshOwner) (\s@UpdateVirtualGateway' {} a -> s {meshOwner = a} :: UpdateVirtualGateway)

-- | The name of the service mesh that the virtual gateway resides in.
updateVirtualGateway_meshName :: Lens.Lens' UpdateVirtualGateway Prelude.Text
updateVirtualGateway_meshName = Lens.lens (\UpdateVirtualGateway' {meshName} -> meshName) (\s@UpdateVirtualGateway' {} a -> s {meshName = a} :: UpdateVirtualGateway)

-- | The new virtual gateway specification to apply. This overwrites the
-- existing data.
updateVirtualGateway_spec :: Lens.Lens' UpdateVirtualGateway VirtualGatewaySpec
updateVirtualGateway_spec = Lens.lens (\UpdateVirtualGateway' {spec} -> spec) (\s@UpdateVirtualGateway' {} a -> s {spec = a} :: UpdateVirtualGateway)

-- | The name of the virtual gateway to update.
updateVirtualGateway_virtualGatewayName :: Lens.Lens' UpdateVirtualGateway Prelude.Text
updateVirtualGateway_virtualGatewayName = Lens.lens (\UpdateVirtualGateway' {virtualGatewayName} -> virtualGatewayName) (\s@UpdateVirtualGateway' {} a -> s {virtualGatewayName = a} :: UpdateVirtualGateway)

instance Core.AWSRequest UpdateVirtualGateway where
  type
    AWSResponse UpdateVirtualGateway =
      UpdateVirtualGatewayResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVirtualGatewayResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateVirtualGateway where
  hashWithSalt _salt UpdateVirtualGateway' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` virtualGatewayName

instance Prelude.NFData UpdateVirtualGateway where
  rnf UpdateVirtualGateway' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf meshOwner `Prelude.seq`
        Prelude.rnf meshName `Prelude.seq`
          Prelude.rnf spec `Prelude.seq`
            Prelude.rnf virtualGatewayName

instance Data.ToHeaders UpdateVirtualGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateVirtualGateway where
  toJSON UpdateVirtualGateway' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("spec" Data..= spec)
          ]
      )

instance Data.ToPath UpdateVirtualGateway where
  toPath UpdateVirtualGateway' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Data.toBS meshName,
        "/virtualGateways/",
        Data.toBS virtualGatewayName
      ]

instance Data.ToQuery UpdateVirtualGateway where
  toQuery UpdateVirtualGateway' {..} =
    Prelude.mconcat ["meshOwner" Data.=: meshOwner]

-- | /See:/ 'newUpdateVirtualGatewayResponse' smart constructor.
data UpdateVirtualGatewayResponse = UpdateVirtualGatewayResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A full description of the virtual gateway that was updated.
    virtualGateway :: VirtualGatewayData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVirtualGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateVirtualGatewayResponse_httpStatus' - The response's http status code.
--
-- 'virtualGateway', 'updateVirtualGatewayResponse_virtualGateway' - A full description of the virtual gateway that was updated.
newUpdateVirtualGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'virtualGateway'
  VirtualGatewayData ->
  UpdateVirtualGatewayResponse
newUpdateVirtualGatewayResponse
  pHttpStatus_
  pVirtualGateway_ =
    UpdateVirtualGatewayResponse'
      { httpStatus =
          pHttpStatus_,
        virtualGateway = pVirtualGateway_
      }

-- | The response's http status code.
updateVirtualGatewayResponse_httpStatus :: Lens.Lens' UpdateVirtualGatewayResponse Prelude.Int
updateVirtualGatewayResponse_httpStatus = Lens.lens (\UpdateVirtualGatewayResponse' {httpStatus} -> httpStatus) (\s@UpdateVirtualGatewayResponse' {} a -> s {httpStatus = a} :: UpdateVirtualGatewayResponse)

-- | A full description of the virtual gateway that was updated.
updateVirtualGatewayResponse_virtualGateway :: Lens.Lens' UpdateVirtualGatewayResponse VirtualGatewayData
updateVirtualGatewayResponse_virtualGateway = Lens.lens (\UpdateVirtualGatewayResponse' {virtualGateway} -> virtualGateway) (\s@UpdateVirtualGatewayResponse' {} a -> s {virtualGateway = a} :: UpdateVirtualGatewayResponse)

instance Prelude.NFData UpdateVirtualGatewayResponse where
  rnf UpdateVirtualGatewayResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf virtualGateway
