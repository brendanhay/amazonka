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
-- Module      : Amazonka.AppMesh.DeleteGatewayRoute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing gateway route.
module Amazonka.AppMesh.DeleteGatewayRoute
  ( -- * Creating a Request
    DeleteGatewayRoute (..),
    newDeleteGatewayRoute,

    -- * Request Lenses
    deleteGatewayRoute_meshOwner,
    deleteGatewayRoute_gatewayRouteName,
    deleteGatewayRoute_meshName,
    deleteGatewayRoute_virtualGatewayName,

    -- * Destructuring the Response
    DeleteGatewayRouteResponse (..),
    newDeleteGatewayRouteResponse,

    -- * Response Lenses
    deleteGatewayRouteResponse_httpStatus,
    deleteGatewayRouteResponse_gatewayRoute,
  )
where

import Amazonka.AppMesh.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteGatewayRoute' smart constructor.
data DeleteGatewayRoute = DeleteGatewayRoute'
  { -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the gateway route to delete.
    gatewayRouteName :: Prelude.Text,
    -- | The name of the service mesh to delete the gateway route from.
    meshName :: Prelude.Text,
    -- | The name of the virtual gateway to delete the route from.
    virtualGatewayName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGatewayRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meshOwner', 'deleteGatewayRoute_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'gatewayRouteName', 'deleteGatewayRoute_gatewayRouteName' - The name of the gateway route to delete.
--
-- 'meshName', 'deleteGatewayRoute_meshName' - The name of the service mesh to delete the gateway route from.
--
-- 'virtualGatewayName', 'deleteGatewayRoute_virtualGatewayName' - The name of the virtual gateway to delete the route from.
newDeleteGatewayRoute ::
  -- | 'gatewayRouteName'
  Prelude.Text ->
  -- | 'meshName'
  Prelude.Text ->
  -- | 'virtualGatewayName'
  Prelude.Text ->
  DeleteGatewayRoute
newDeleteGatewayRoute
  pGatewayRouteName_
  pMeshName_
  pVirtualGatewayName_ =
    DeleteGatewayRoute'
      { meshOwner = Prelude.Nothing,
        gatewayRouteName = pGatewayRouteName_,
        meshName = pMeshName_,
        virtualGatewayName = pVirtualGatewayName_
      }

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
deleteGatewayRoute_meshOwner :: Lens.Lens' DeleteGatewayRoute (Prelude.Maybe Prelude.Text)
deleteGatewayRoute_meshOwner = Lens.lens (\DeleteGatewayRoute' {meshOwner} -> meshOwner) (\s@DeleteGatewayRoute' {} a -> s {meshOwner = a} :: DeleteGatewayRoute)

-- | The name of the gateway route to delete.
deleteGatewayRoute_gatewayRouteName :: Lens.Lens' DeleteGatewayRoute Prelude.Text
deleteGatewayRoute_gatewayRouteName = Lens.lens (\DeleteGatewayRoute' {gatewayRouteName} -> gatewayRouteName) (\s@DeleteGatewayRoute' {} a -> s {gatewayRouteName = a} :: DeleteGatewayRoute)

-- | The name of the service mesh to delete the gateway route from.
deleteGatewayRoute_meshName :: Lens.Lens' DeleteGatewayRoute Prelude.Text
deleteGatewayRoute_meshName = Lens.lens (\DeleteGatewayRoute' {meshName} -> meshName) (\s@DeleteGatewayRoute' {} a -> s {meshName = a} :: DeleteGatewayRoute)

-- | The name of the virtual gateway to delete the route from.
deleteGatewayRoute_virtualGatewayName :: Lens.Lens' DeleteGatewayRoute Prelude.Text
deleteGatewayRoute_virtualGatewayName = Lens.lens (\DeleteGatewayRoute' {virtualGatewayName} -> virtualGatewayName) (\s@DeleteGatewayRoute' {} a -> s {virtualGatewayName = a} :: DeleteGatewayRoute)

instance Core.AWSRequest DeleteGatewayRoute where
  type
    AWSResponse DeleteGatewayRoute =
      DeleteGatewayRouteResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteGatewayRouteResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteGatewayRoute where
  hashWithSalt _salt DeleteGatewayRoute' {..} =
    _salt `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` gatewayRouteName
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` virtualGatewayName

instance Prelude.NFData DeleteGatewayRoute where
  rnf DeleteGatewayRoute' {..} =
    Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf gatewayRouteName
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf virtualGatewayName

instance Core.ToHeaders DeleteGatewayRoute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteGatewayRoute where
  toPath DeleteGatewayRoute' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Core.toBS meshName,
        "/virtualGateway/",
        Core.toBS virtualGatewayName,
        "/gatewayRoutes/",
        Core.toBS gatewayRouteName
      ]

instance Core.ToQuery DeleteGatewayRoute where
  toQuery DeleteGatewayRoute' {..} =
    Prelude.mconcat ["meshOwner" Core.=: meshOwner]

-- | /See:/ 'newDeleteGatewayRouteResponse' smart constructor.
data DeleteGatewayRouteResponse = DeleteGatewayRouteResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The gateway route that was deleted.
    gatewayRoute :: GatewayRouteData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGatewayRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteGatewayRouteResponse_httpStatus' - The response's http status code.
--
-- 'gatewayRoute', 'deleteGatewayRouteResponse_gatewayRoute' - The gateway route that was deleted.
newDeleteGatewayRouteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'gatewayRoute'
  GatewayRouteData ->
  DeleteGatewayRouteResponse
newDeleteGatewayRouteResponse
  pHttpStatus_
  pGatewayRoute_ =
    DeleteGatewayRouteResponse'
      { httpStatus =
          pHttpStatus_,
        gatewayRoute = pGatewayRoute_
      }

-- | The response's http status code.
deleteGatewayRouteResponse_httpStatus :: Lens.Lens' DeleteGatewayRouteResponse Prelude.Int
deleteGatewayRouteResponse_httpStatus = Lens.lens (\DeleteGatewayRouteResponse' {httpStatus} -> httpStatus) (\s@DeleteGatewayRouteResponse' {} a -> s {httpStatus = a} :: DeleteGatewayRouteResponse)

-- | The gateway route that was deleted.
deleteGatewayRouteResponse_gatewayRoute :: Lens.Lens' DeleteGatewayRouteResponse GatewayRouteData
deleteGatewayRouteResponse_gatewayRoute = Lens.lens (\DeleteGatewayRouteResponse' {gatewayRoute} -> gatewayRoute) (\s@DeleteGatewayRouteResponse' {} a -> s {gatewayRoute = a} :: DeleteGatewayRouteResponse)

instance Prelude.NFData DeleteGatewayRouteResponse where
  rnf DeleteGatewayRouteResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf gatewayRoute
