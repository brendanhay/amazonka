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
-- Module      : Amazonka.AppMesh.UpdateRoute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing route for a specified service mesh and virtual
-- router.
module Amazonka.AppMesh.UpdateRoute
  ( -- * Creating a Request
    UpdateRoute (..),
    newUpdateRoute,

    -- * Request Lenses
    updateRoute_clientToken,
    updateRoute_meshOwner,
    updateRoute_meshName,
    updateRoute_routeName,
    updateRoute_spec,
    updateRoute_virtualRouterName,

    -- * Destructuring the Response
    UpdateRouteResponse (..),
    newUpdateRouteResponse,

    -- * Response Lenses
    updateRouteResponse_httpStatus,
    updateRouteResponse_route,
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
-- /See:/ 'newUpdateRoute' smart constructor.
data UpdateRoute = UpdateRoute'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. Up to 36 letters, numbers, hyphens, and
    -- underscores are allowed.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the service mesh that the route resides in.
    meshName :: Prelude.Text,
    -- | The name of the route to update.
    routeName :: Prelude.Text,
    -- | The new route specification to apply. This overwrites the existing data.
    spec :: RouteSpec,
    -- | The name of the virtual router that the route is associated with.
    virtualRouterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateRoute_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
--
-- 'meshOwner', 'updateRoute_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'meshName', 'updateRoute_meshName' - The name of the service mesh that the route resides in.
--
-- 'routeName', 'updateRoute_routeName' - The name of the route to update.
--
-- 'spec', 'updateRoute_spec' - The new route specification to apply. This overwrites the existing data.
--
-- 'virtualRouterName', 'updateRoute_virtualRouterName' - The name of the virtual router that the route is associated with.
newUpdateRoute ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'routeName'
  Prelude.Text ->
  -- | 'spec'
  RouteSpec ->
  -- | 'virtualRouterName'
  Prelude.Text ->
  UpdateRoute
newUpdateRoute
  pMeshName_
  pRouteName_
  pSpec_
  pVirtualRouterName_ =
    UpdateRoute'
      { clientToken = Prelude.Nothing,
        meshOwner = Prelude.Nothing,
        meshName = pMeshName_,
        routeName = pRouteName_,
        spec = pSpec_,
        virtualRouterName = pVirtualRouterName_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
updateRoute_clientToken :: Lens.Lens' UpdateRoute (Prelude.Maybe Prelude.Text)
updateRoute_clientToken = Lens.lens (\UpdateRoute' {clientToken} -> clientToken) (\s@UpdateRoute' {} a -> s {clientToken = a} :: UpdateRoute)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
updateRoute_meshOwner :: Lens.Lens' UpdateRoute (Prelude.Maybe Prelude.Text)
updateRoute_meshOwner = Lens.lens (\UpdateRoute' {meshOwner} -> meshOwner) (\s@UpdateRoute' {} a -> s {meshOwner = a} :: UpdateRoute)

-- | The name of the service mesh that the route resides in.
updateRoute_meshName :: Lens.Lens' UpdateRoute Prelude.Text
updateRoute_meshName = Lens.lens (\UpdateRoute' {meshName} -> meshName) (\s@UpdateRoute' {} a -> s {meshName = a} :: UpdateRoute)

-- | The name of the route to update.
updateRoute_routeName :: Lens.Lens' UpdateRoute Prelude.Text
updateRoute_routeName = Lens.lens (\UpdateRoute' {routeName} -> routeName) (\s@UpdateRoute' {} a -> s {routeName = a} :: UpdateRoute)

-- | The new route specification to apply. This overwrites the existing data.
updateRoute_spec :: Lens.Lens' UpdateRoute RouteSpec
updateRoute_spec = Lens.lens (\UpdateRoute' {spec} -> spec) (\s@UpdateRoute' {} a -> s {spec = a} :: UpdateRoute)

-- | The name of the virtual router that the route is associated with.
updateRoute_virtualRouterName :: Lens.Lens' UpdateRoute Prelude.Text
updateRoute_virtualRouterName = Lens.lens (\UpdateRoute' {virtualRouterName} -> virtualRouterName) (\s@UpdateRoute' {} a -> s {virtualRouterName = a} :: UpdateRoute)

instance Core.AWSRequest UpdateRoute where
  type AWSResponse UpdateRoute = UpdateRouteResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRouteResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateRoute where
  hashWithSalt _salt UpdateRoute' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` routeName
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` virtualRouterName

instance Prelude.NFData UpdateRoute where
  rnf UpdateRoute' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf routeName
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf virtualRouterName

instance Data.ToHeaders UpdateRoute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRoute where
  toJSON UpdateRoute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("spec" Data..= spec)
          ]
      )

instance Data.ToPath UpdateRoute where
  toPath UpdateRoute' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Data.toBS meshName,
        "/virtualRouter/",
        Data.toBS virtualRouterName,
        "/routes/",
        Data.toBS routeName
      ]

instance Data.ToQuery UpdateRoute where
  toQuery UpdateRoute' {..} =
    Prelude.mconcat ["meshOwner" Data.=: meshOwner]

-- |
--
-- /See:/ 'newUpdateRouteResponse' smart constructor.
data UpdateRouteResponse = UpdateRouteResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A full description of the route that was updated.
    route :: RouteData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateRouteResponse_httpStatus' - The response's http status code.
--
-- 'route', 'updateRouteResponse_route' - A full description of the route that was updated.
newUpdateRouteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'route'
  RouteData ->
  UpdateRouteResponse
newUpdateRouteResponse pHttpStatus_ pRoute_ =
  UpdateRouteResponse'
    { httpStatus = pHttpStatus_,
      route = pRoute_
    }

-- | The response's http status code.
updateRouteResponse_httpStatus :: Lens.Lens' UpdateRouteResponse Prelude.Int
updateRouteResponse_httpStatus = Lens.lens (\UpdateRouteResponse' {httpStatus} -> httpStatus) (\s@UpdateRouteResponse' {} a -> s {httpStatus = a} :: UpdateRouteResponse)

-- | A full description of the route that was updated.
updateRouteResponse_route :: Lens.Lens' UpdateRouteResponse RouteData
updateRouteResponse_route = Lens.lens (\UpdateRouteResponse' {route} -> route) (\s@UpdateRouteResponse' {} a -> s {route = a} :: UpdateRouteResponse)

instance Prelude.NFData UpdateRouteResponse where
  rnf UpdateRouteResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf route
