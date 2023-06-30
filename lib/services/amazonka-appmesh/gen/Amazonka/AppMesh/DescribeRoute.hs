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
-- Module      : Amazonka.AppMesh.DescribeRoute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing route.
module Amazonka.AppMesh.DescribeRoute
  ( -- * Creating a Request
    DescribeRoute (..),
    newDescribeRoute,

    -- * Request Lenses
    describeRoute_meshOwner,
    describeRoute_meshName,
    describeRoute_routeName,
    describeRoute_virtualRouterName,

    -- * Destructuring the Response
    DescribeRouteResponse (..),
    newDescribeRouteResponse,

    -- * Response Lenses
    describeRouteResponse_httpStatus,
    describeRouteResponse_route,
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
-- /See:/ 'newDescribeRoute' smart constructor.
data DescribeRoute = DescribeRoute'
  { -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the service mesh that the route resides in.
    meshName :: Prelude.Text,
    -- | The name of the route to describe.
    routeName :: Prelude.Text,
    -- | The name of the virtual router that the route is associated with.
    virtualRouterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meshOwner', 'describeRoute_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'meshName', 'describeRoute_meshName' - The name of the service mesh that the route resides in.
--
-- 'routeName', 'describeRoute_routeName' - The name of the route to describe.
--
-- 'virtualRouterName', 'describeRoute_virtualRouterName' - The name of the virtual router that the route is associated with.
newDescribeRoute ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'routeName'
  Prelude.Text ->
  -- | 'virtualRouterName'
  Prelude.Text ->
  DescribeRoute
newDescribeRoute
  pMeshName_
  pRouteName_
  pVirtualRouterName_ =
    DescribeRoute'
      { meshOwner = Prelude.Nothing,
        meshName = pMeshName_,
        routeName = pRouteName_,
        virtualRouterName = pVirtualRouterName_
      }

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
describeRoute_meshOwner :: Lens.Lens' DescribeRoute (Prelude.Maybe Prelude.Text)
describeRoute_meshOwner = Lens.lens (\DescribeRoute' {meshOwner} -> meshOwner) (\s@DescribeRoute' {} a -> s {meshOwner = a} :: DescribeRoute)

-- | The name of the service mesh that the route resides in.
describeRoute_meshName :: Lens.Lens' DescribeRoute Prelude.Text
describeRoute_meshName = Lens.lens (\DescribeRoute' {meshName} -> meshName) (\s@DescribeRoute' {} a -> s {meshName = a} :: DescribeRoute)

-- | The name of the route to describe.
describeRoute_routeName :: Lens.Lens' DescribeRoute Prelude.Text
describeRoute_routeName = Lens.lens (\DescribeRoute' {routeName} -> routeName) (\s@DescribeRoute' {} a -> s {routeName = a} :: DescribeRoute)

-- | The name of the virtual router that the route is associated with.
describeRoute_virtualRouterName :: Lens.Lens' DescribeRoute Prelude.Text
describeRoute_virtualRouterName = Lens.lens (\DescribeRoute' {virtualRouterName} -> virtualRouterName) (\s@DescribeRoute' {} a -> s {virtualRouterName = a} :: DescribeRoute)

instance Core.AWSRequest DescribeRoute where
  type
    AWSResponse DescribeRoute =
      DescribeRouteResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRouteResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DescribeRoute where
  hashWithSalt _salt DescribeRoute' {..} =
    _salt
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` routeName
      `Prelude.hashWithSalt` virtualRouterName

instance Prelude.NFData DescribeRoute where
  rnf DescribeRoute' {..} =
    Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf routeName
      `Prelude.seq` Prelude.rnf virtualRouterName

instance Data.ToHeaders DescribeRoute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeRoute where
  toPath DescribeRoute' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Data.toBS meshName,
        "/virtualRouter/",
        Data.toBS virtualRouterName,
        "/routes/",
        Data.toBS routeName
      ]

instance Data.ToQuery DescribeRoute where
  toQuery DescribeRoute' {..} =
    Prelude.mconcat ["meshOwner" Data.=: meshOwner]

-- |
--
-- /See:/ 'newDescribeRouteResponse' smart constructor.
data DescribeRouteResponse = DescribeRouteResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The full description of your route.
    route :: RouteData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeRouteResponse_httpStatus' - The response's http status code.
--
-- 'route', 'describeRouteResponse_route' - The full description of your route.
newDescribeRouteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'route'
  RouteData ->
  DescribeRouteResponse
newDescribeRouteResponse pHttpStatus_ pRoute_ =
  DescribeRouteResponse'
    { httpStatus = pHttpStatus_,
      route = pRoute_
    }

-- | The response's http status code.
describeRouteResponse_httpStatus :: Lens.Lens' DescribeRouteResponse Prelude.Int
describeRouteResponse_httpStatus = Lens.lens (\DescribeRouteResponse' {httpStatus} -> httpStatus) (\s@DescribeRouteResponse' {} a -> s {httpStatus = a} :: DescribeRouteResponse)

-- | The full description of your route.
describeRouteResponse_route :: Lens.Lens' DescribeRouteResponse RouteData
describeRouteResponse_route = Lens.lens (\DescribeRouteResponse' {route} -> route) (\s@DescribeRouteResponse' {} a -> s {route = a} :: DescribeRouteResponse)

instance Prelude.NFData DescribeRouteResponse where
  rnf DescribeRouteResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf route
