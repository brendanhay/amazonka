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
-- Module      : Amazonka.AppMesh.UpdateGatewayRoute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing gateway route that is associated to a specified
-- virtual gateway in a service mesh.
module Amazonka.AppMesh.UpdateGatewayRoute
  ( -- * Creating a Request
    UpdateGatewayRoute (..),
    newUpdateGatewayRoute,

    -- * Request Lenses
    updateGatewayRoute_clientToken,
    updateGatewayRoute_meshOwner,
    updateGatewayRoute_gatewayRouteName,
    updateGatewayRoute_meshName,
    updateGatewayRoute_spec,
    updateGatewayRoute_virtualGatewayName,

    -- * Destructuring the Response
    UpdateGatewayRouteResponse (..),
    newUpdateGatewayRouteResponse,

    -- * Response Lenses
    updateGatewayRouteResponse_httpStatus,
    updateGatewayRouteResponse_gatewayRoute,
  )
where

import Amazonka.AppMesh.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGatewayRoute' smart constructor.
data UpdateGatewayRoute = UpdateGatewayRoute'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. Up to 36 letters, numbers, hyphens, and
    -- underscores are allowed.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the gateway route to update.
    gatewayRouteName :: Prelude.Text,
    -- | The name of the service mesh that the gateway route resides in.
    meshName :: Prelude.Text,
    -- | The new gateway route specification to apply. This overwrites the
    -- existing data.
    spec :: GatewayRouteSpec,
    -- | The name of the virtual gateway that the gateway route is associated
    -- with.
    virtualGatewayName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGatewayRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateGatewayRoute_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
--
-- 'meshOwner', 'updateGatewayRoute_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'gatewayRouteName', 'updateGatewayRoute_gatewayRouteName' - The name of the gateway route to update.
--
-- 'meshName', 'updateGatewayRoute_meshName' - The name of the service mesh that the gateway route resides in.
--
-- 'spec', 'updateGatewayRoute_spec' - The new gateway route specification to apply. This overwrites the
-- existing data.
--
-- 'virtualGatewayName', 'updateGatewayRoute_virtualGatewayName' - The name of the virtual gateway that the gateway route is associated
-- with.
newUpdateGatewayRoute ::
  -- | 'gatewayRouteName'
  Prelude.Text ->
  -- | 'meshName'
  Prelude.Text ->
  -- | 'spec'
  GatewayRouteSpec ->
  -- | 'virtualGatewayName'
  Prelude.Text ->
  UpdateGatewayRoute
newUpdateGatewayRoute
  pGatewayRouteName_
  pMeshName_
  pSpec_
  pVirtualGatewayName_ =
    UpdateGatewayRoute'
      { clientToken = Prelude.Nothing,
        meshOwner = Prelude.Nothing,
        gatewayRouteName = pGatewayRouteName_,
        meshName = pMeshName_,
        spec = pSpec_,
        virtualGatewayName = pVirtualGatewayName_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
updateGatewayRoute_clientToken :: Lens.Lens' UpdateGatewayRoute (Prelude.Maybe Prelude.Text)
updateGatewayRoute_clientToken = Lens.lens (\UpdateGatewayRoute' {clientToken} -> clientToken) (\s@UpdateGatewayRoute' {} a -> s {clientToken = a} :: UpdateGatewayRoute)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
updateGatewayRoute_meshOwner :: Lens.Lens' UpdateGatewayRoute (Prelude.Maybe Prelude.Text)
updateGatewayRoute_meshOwner = Lens.lens (\UpdateGatewayRoute' {meshOwner} -> meshOwner) (\s@UpdateGatewayRoute' {} a -> s {meshOwner = a} :: UpdateGatewayRoute)

-- | The name of the gateway route to update.
updateGatewayRoute_gatewayRouteName :: Lens.Lens' UpdateGatewayRoute Prelude.Text
updateGatewayRoute_gatewayRouteName = Lens.lens (\UpdateGatewayRoute' {gatewayRouteName} -> gatewayRouteName) (\s@UpdateGatewayRoute' {} a -> s {gatewayRouteName = a} :: UpdateGatewayRoute)

-- | The name of the service mesh that the gateway route resides in.
updateGatewayRoute_meshName :: Lens.Lens' UpdateGatewayRoute Prelude.Text
updateGatewayRoute_meshName = Lens.lens (\UpdateGatewayRoute' {meshName} -> meshName) (\s@UpdateGatewayRoute' {} a -> s {meshName = a} :: UpdateGatewayRoute)

-- | The new gateway route specification to apply. This overwrites the
-- existing data.
updateGatewayRoute_spec :: Lens.Lens' UpdateGatewayRoute GatewayRouteSpec
updateGatewayRoute_spec = Lens.lens (\UpdateGatewayRoute' {spec} -> spec) (\s@UpdateGatewayRoute' {} a -> s {spec = a} :: UpdateGatewayRoute)

-- | The name of the virtual gateway that the gateway route is associated
-- with.
updateGatewayRoute_virtualGatewayName :: Lens.Lens' UpdateGatewayRoute Prelude.Text
updateGatewayRoute_virtualGatewayName = Lens.lens (\UpdateGatewayRoute' {virtualGatewayName} -> virtualGatewayName) (\s@UpdateGatewayRoute' {} a -> s {virtualGatewayName = a} :: UpdateGatewayRoute)

instance Core.AWSRequest UpdateGatewayRoute where
  type
    AWSResponse UpdateGatewayRoute =
      UpdateGatewayRouteResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGatewayRouteResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateGatewayRoute where
  hashWithSalt _salt UpdateGatewayRoute' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` gatewayRouteName
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` virtualGatewayName

instance Prelude.NFData UpdateGatewayRoute where
  rnf UpdateGatewayRoute' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf gatewayRouteName
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf virtualGatewayName

instance Core.ToHeaders UpdateGatewayRoute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateGatewayRoute where
  toJSON UpdateGatewayRoute' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just ("spec" Core..= spec)
          ]
      )

instance Core.ToPath UpdateGatewayRoute where
  toPath UpdateGatewayRoute' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Core.toBS meshName,
        "/virtualGateway/",
        Core.toBS virtualGatewayName,
        "/gatewayRoutes/",
        Core.toBS gatewayRouteName
      ]

instance Core.ToQuery UpdateGatewayRoute where
  toQuery UpdateGatewayRoute' {..} =
    Prelude.mconcat ["meshOwner" Core.=: meshOwner]

-- | /See:/ 'newUpdateGatewayRouteResponse' smart constructor.
data UpdateGatewayRouteResponse = UpdateGatewayRouteResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A full description of the gateway route that was updated.
    gatewayRoute :: GatewayRouteData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGatewayRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateGatewayRouteResponse_httpStatus' - The response's http status code.
--
-- 'gatewayRoute', 'updateGatewayRouteResponse_gatewayRoute' - A full description of the gateway route that was updated.
newUpdateGatewayRouteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'gatewayRoute'
  GatewayRouteData ->
  UpdateGatewayRouteResponse
newUpdateGatewayRouteResponse
  pHttpStatus_
  pGatewayRoute_ =
    UpdateGatewayRouteResponse'
      { httpStatus =
          pHttpStatus_,
        gatewayRoute = pGatewayRoute_
      }

-- | The response's http status code.
updateGatewayRouteResponse_httpStatus :: Lens.Lens' UpdateGatewayRouteResponse Prelude.Int
updateGatewayRouteResponse_httpStatus = Lens.lens (\UpdateGatewayRouteResponse' {httpStatus} -> httpStatus) (\s@UpdateGatewayRouteResponse' {} a -> s {httpStatus = a} :: UpdateGatewayRouteResponse)

-- | A full description of the gateway route that was updated.
updateGatewayRouteResponse_gatewayRoute :: Lens.Lens' UpdateGatewayRouteResponse GatewayRouteData
updateGatewayRouteResponse_gatewayRoute = Lens.lens (\UpdateGatewayRouteResponse' {gatewayRoute} -> gatewayRoute) (\s@UpdateGatewayRouteResponse' {} a -> s {gatewayRoute = a} :: UpdateGatewayRouteResponse)

instance Prelude.NFData UpdateGatewayRouteResponse where
  rnf UpdateGatewayRouteResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf gatewayRoute
