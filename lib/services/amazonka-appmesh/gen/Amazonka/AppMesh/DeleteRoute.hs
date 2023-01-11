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
-- Module      : Amazonka.AppMesh.DeleteRoute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing route.
module Amazonka.AppMesh.DeleteRoute
  ( -- * Creating a Request
    DeleteRoute (..),
    newDeleteRoute,

    -- * Request Lenses
    deleteRoute_meshOwner,
    deleteRoute_meshName,
    deleteRoute_routeName,
    deleteRoute_virtualRouterName,

    -- * Destructuring the Response
    DeleteRouteResponse (..),
    newDeleteRouteResponse,

    -- * Response Lenses
    deleteRouteResponse_httpStatus,
    deleteRouteResponse_route,
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
-- /See:/ 'newDeleteRoute' smart constructor.
data DeleteRoute = DeleteRoute'
  { -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the service mesh to delete the route in.
    meshName :: Prelude.Text,
    -- | The name of the route to delete.
    routeName :: Prelude.Text,
    -- | The name of the virtual router to delete the route in.
    virtualRouterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meshOwner', 'deleteRoute_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'meshName', 'deleteRoute_meshName' - The name of the service mesh to delete the route in.
--
-- 'routeName', 'deleteRoute_routeName' - The name of the route to delete.
--
-- 'virtualRouterName', 'deleteRoute_virtualRouterName' - The name of the virtual router to delete the route in.
newDeleteRoute ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'routeName'
  Prelude.Text ->
  -- | 'virtualRouterName'
  Prelude.Text ->
  DeleteRoute
newDeleteRoute
  pMeshName_
  pRouteName_
  pVirtualRouterName_ =
    DeleteRoute'
      { meshOwner = Prelude.Nothing,
        meshName = pMeshName_,
        routeName = pRouteName_,
        virtualRouterName = pVirtualRouterName_
      }

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
deleteRoute_meshOwner :: Lens.Lens' DeleteRoute (Prelude.Maybe Prelude.Text)
deleteRoute_meshOwner = Lens.lens (\DeleteRoute' {meshOwner} -> meshOwner) (\s@DeleteRoute' {} a -> s {meshOwner = a} :: DeleteRoute)

-- | The name of the service mesh to delete the route in.
deleteRoute_meshName :: Lens.Lens' DeleteRoute Prelude.Text
deleteRoute_meshName = Lens.lens (\DeleteRoute' {meshName} -> meshName) (\s@DeleteRoute' {} a -> s {meshName = a} :: DeleteRoute)

-- | The name of the route to delete.
deleteRoute_routeName :: Lens.Lens' DeleteRoute Prelude.Text
deleteRoute_routeName = Lens.lens (\DeleteRoute' {routeName} -> routeName) (\s@DeleteRoute' {} a -> s {routeName = a} :: DeleteRoute)

-- | The name of the virtual router to delete the route in.
deleteRoute_virtualRouterName :: Lens.Lens' DeleteRoute Prelude.Text
deleteRoute_virtualRouterName = Lens.lens (\DeleteRoute' {virtualRouterName} -> virtualRouterName) (\s@DeleteRoute' {} a -> s {virtualRouterName = a} :: DeleteRoute)

instance Core.AWSRequest DeleteRoute where
  type AWSResponse DeleteRoute = DeleteRouteResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRouteResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteRoute where
  hashWithSalt _salt DeleteRoute' {..} =
    _salt `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` routeName
      `Prelude.hashWithSalt` virtualRouterName

instance Prelude.NFData DeleteRoute where
  rnf DeleteRoute' {..} =
    Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf routeName
      `Prelude.seq` Prelude.rnf virtualRouterName

instance Data.ToHeaders DeleteRoute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteRoute where
  toPath DeleteRoute' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Data.toBS meshName,
        "/virtualRouter/",
        Data.toBS virtualRouterName,
        "/routes/",
        Data.toBS routeName
      ]

instance Data.ToQuery DeleteRoute where
  toQuery DeleteRoute' {..} =
    Prelude.mconcat ["meshOwner" Data.=: meshOwner]

-- |
--
-- /See:/ 'newDeleteRouteResponse' smart constructor.
data DeleteRouteResponse = DeleteRouteResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The route that was deleted.
    route :: RouteData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRouteResponse_httpStatus' - The response's http status code.
--
-- 'route', 'deleteRouteResponse_route' - The route that was deleted.
newDeleteRouteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'route'
  RouteData ->
  DeleteRouteResponse
newDeleteRouteResponse pHttpStatus_ pRoute_ =
  DeleteRouteResponse'
    { httpStatus = pHttpStatus_,
      route = pRoute_
    }

-- | The response's http status code.
deleteRouteResponse_httpStatus :: Lens.Lens' DeleteRouteResponse Prelude.Int
deleteRouteResponse_httpStatus = Lens.lens (\DeleteRouteResponse' {httpStatus} -> httpStatus) (\s@DeleteRouteResponse' {} a -> s {httpStatus = a} :: DeleteRouteResponse)

-- | The route that was deleted.
deleteRouteResponse_route :: Lens.Lens' DeleteRouteResponse RouteData
deleteRouteResponse_route = Lens.lens (\DeleteRouteResponse' {route} -> route) (\s@DeleteRouteResponse' {} a -> s {route = a} :: DeleteRouteResponse)

instance Prelude.NFData DeleteRouteResponse where
  rnf DeleteRouteResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf route
