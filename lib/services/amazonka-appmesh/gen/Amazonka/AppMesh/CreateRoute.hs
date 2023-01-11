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
-- Module      : Amazonka.AppMesh.CreateRoute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a route that is associated with a virtual router.
--
-- You can route several different protocols and define a retry policy for
-- a route. Traffic can be routed to one or more virtual nodes.
--
-- For more information about routes, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/routes.html Routes>.
module Amazonka.AppMesh.CreateRoute
  ( -- * Creating a Request
    CreateRoute (..),
    newCreateRoute,

    -- * Request Lenses
    createRoute_clientToken,
    createRoute_meshOwner,
    createRoute_tags,
    createRoute_meshName,
    createRoute_routeName,
    createRoute_spec,
    createRoute_virtualRouterName,

    -- * Destructuring the Response
    CreateRouteResponse (..),
    newCreateRouteResponse,

    -- * Response Lenses
    createRouteResponse_httpStatus,
    createRouteResponse_route,
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
-- /See:/ 'newCreateRoute' smart constructor.
data CreateRoute = CreateRoute'
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
    -- | Optional metadata that you can apply to the route to assist with
    -- categorization and organization. Each tag consists of a key and an
    -- optional value, both of which you define. Tag keys can have a maximum
    -- character length of 128 characters, and tag values can have a maximum
    -- length of 256 characters.
    tags :: Prelude.Maybe [TagRef],
    -- | The name of the service mesh to create the route in.
    meshName :: Prelude.Text,
    -- | The name to use for the route.
    routeName :: Prelude.Text,
    -- | The route specification to apply.
    spec :: RouteSpec,
    -- | The name of the virtual router in which to create the route. If the
    -- virtual router is in a shared mesh, then you must be the owner of the
    -- virtual router resource.
    virtualRouterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createRoute_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
--
-- 'meshOwner', 'createRoute_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then the account that you specify must share
-- the mesh with your account before you can create the resource in the
-- service mesh. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'tags', 'createRoute_tags' - Optional metadata that you can apply to the route to assist with
-- categorization and organization. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
--
-- 'meshName', 'createRoute_meshName' - The name of the service mesh to create the route in.
--
-- 'routeName', 'createRoute_routeName' - The name to use for the route.
--
-- 'spec', 'createRoute_spec' - The route specification to apply.
--
-- 'virtualRouterName', 'createRoute_virtualRouterName' - The name of the virtual router in which to create the route. If the
-- virtual router is in a shared mesh, then you must be the owner of the
-- virtual router resource.
newCreateRoute ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'routeName'
  Prelude.Text ->
  -- | 'spec'
  RouteSpec ->
  -- | 'virtualRouterName'
  Prelude.Text ->
  CreateRoute
newCreateRoute
  pMeshName_
  pRouteName_
  pSpec_
  pVirtualRouterName_ =
    CreateRoute'
      { clientToken = Prelude.Nothing,
        meshOwner = Prelude.Nothing,
        tags = Prelude.Nothing,
        meshName = pMeshName_,
        routeName = pRouteName_,
        spec = pSpec_,
        virtualRouterName = pVirtualRouterName_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. Up to 36 letters, numbers, hyphens, and
-- underscores are allowed.
createRoute_clientToken :: Lens.Lens' CreateRoute (Prelude.Maybe Prelude.Text)
createRoute_clientToken = Lens.lens (\CreateRoute' {clientToken} -> clientToken) (\s@CreateRoute' {} a -> s {clientToken = a} :: CreateRoute)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then the account that you specify must share
-- the mesh with your account before you can create the resource in the
-- service mesh. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
createRoute_meshOwner :: Lens.Lens' CreateRoute (Prelude.Maybe Prelude.Text)
createRoute_meshOwner = Lens.lens (\CreateRoute' {meshOwner} -> meshOwner) (\s@CreateRoute' {} a -> s {meshOwner = a} :: CreateRoute)

-- | Optional metadata that you can apply to the route to assist with
-- categorization and organization. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
createRoute_tags :: Lens.Lens' CreateRoute (Prelude.Maybe [TagRef])
createRoute_tags = Lens.lens (\CreateRoute' {tags} -> tags) (\s@CreateRoute' {} a -> s {tags = a} :: CreateRoute) Prelude.. Lens.mapping Lens.coerced

-- | The name of the service mesh to create the route in.
createRoute_meshName :: Lens.Lens' CreateRoute Prelude.Text
createRoute_meshName = Lens.lens (\CreateRoute' {meshName} -> meshName) (\s@CreateRoute' {} a -> s {meshName = a} :: CreateRoute)

-- | The name to use for the route.
createRoute_routeName :: Lens.Lens' CreateRoute Prelude.Text
createRoute_routeName = Lens.lens (\CreateRoute' {routeName} -> routeName) (\s@CreateRoute' {} a -> s {routeName = a} :: CreateRoute)

-- | The route specification to apply.
createRoute_spec :: Lens.Lens' CreateRoute RouteSpec
createRoute_spec = Lens.lens (\CreateRoute' {spec} -> spec) (\s@CreateRoute' {} a -> s {spec = a} :: CreateRoute)

-- | The name of the virtual router in which to create the route. If the
-- virtual router is in a shared mesh, then you must be the owner of the
-- virtual router resource.
createRoute_virtualRouterName :: Lens.Lens' CreateRoute Prelude.Text
createRoute_virtualRouterName = Lens.lens (\CreateRoute' {virtualRouterName} -> virtualRouterName) (\s@CreateRoute' {} a -> s {virtualRouterName = a} :: CreateRoute)

instance Core.AWSRequest CreateRoute where
  type AWSResponse CreateRoute = CreateRouteResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRouteResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable CreateRoute where
  hashWithSalt _salt CreateRoute' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` routeName
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` virtualRouterName

instance Prelude.NFData CreateRoute where
  rnf CreateRoute' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf routeName
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf virtualRouterName

instance Data.ToHeaders CreateRoute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRoute where
  toJSON CreateRoute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("routeName" Data..= routeName),
            Prelude.Just ("spec" Data..= spec)
          ]
      )

instance Data.ToPath CreateRoute where
  toPath CreateRoute' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Data.toBS meshName,
        "/virtualRouter/",
        Data.toBS virtualRouterName,
        "/routes"
      ]

instance Data.ToQuery CreateRoute where
  toQuery CreateRoute' {..} =
    Prelude.mconcat ["meshOwner" Data.=: meshOwner]

-- |
--
-- /See:/ 'newCreateRouteResponse' smart constructor.
data CreateRouteResponse = CreateRouteResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The full description of your mesh following the create call.
    route :: RouteData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createRouteResponse_httpStatus' - The response's http status code.
--
-- 'route', 'createRouteResponse_route' - The full description of your mesh following the create call.
newCreateRouteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'route'
  RouteData ->
  CreateRouteResponse
newCreateRouteResponse pHttpStatus_ pRoute_ =
  CreateRouteResponse'
    { httpStatus = pHttpStatus_,
      route = pRoute_
    }

-- | The response's http status code.
createRouteResponse_httpStatus :: Lens.Lens' CreateRouteResponse Prelude.Int
createRouteResponse_httpStatus = Lens.lens (\CreateRouteResponse' {httpStatus} -> httpStatus) (\s@CreateRouteResponse' {} a -> s {httpStatus = a} :: CreateRouteResponse)

-- | The full description of your mesh following the create call.
createRouteResponse_route :: Lens.Lens' CreateRouteResponse RouteData
createRouteResponse_route = Lens.lens (\CreateRouteResponse' {route} -> route) (\s@CreateRouteResponse' {} a -> s {route = a} :: CreateRouteResponse)

instance Prelude.NFData CreateRouteResponse where
  rnf CreateRouteResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf route
