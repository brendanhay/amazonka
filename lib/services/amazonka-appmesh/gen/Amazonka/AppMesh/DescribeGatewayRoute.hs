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
-- Module      : Amazonka.AppMesh.DescribeGatewayRoute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing gateway route.
module Amazonka.AppMesh.DescribeGatewayRoute
  ( -- * Creating a Request
    DescribeGatewayRoute (..),
    newDescribeGatewayRoute,

    -- * Request Lenses
    describeGatewayRoute_meshOwner,
    describeGatewayRoute_gatewayRouteName,
    describeGatewayRoute_meshName,
    describeGatewayRoute_virtualGatewayName,

    -- * Destructuring the Response
    DescribeGatewayRouteResponse (..),
    newDescribeGatewayRouteResponse,

    -- * Response Lenses
    describeGatewayRouteResponse_httpStatus,
    describeGatewayRouteResponse_gatewayRoute,
  )
where

import Amazonka.AppMesh.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGatewayRoute' smart constructor.
data DescribeGatewayRoute = DescribeGatewayRoute'
  { -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the gateway route to describe.
    gatewayRouteName :: Prelude.Text,
    -- | The name of the service mesh that the gateway route resides in.
    meshName :: Prelude.Text,
    -- | The name of the virtual gateway that the gateway route is associated
    -- with.
    virtualGatewayName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGatewayRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meshOwner', 'describeGatewayRoute_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'gatewayRouteName', 'describeGatewayRoute_gatewayRouteName' - The name of the gateway route to describe.
--
-- 'meshName', 'describeGatewayRoute_meshName' - The name of the service mesh that the gateway route resides in.
--
-- 'virtualGatewayName', 'describeGatewayRoute_virtualGatewayName' - The name of the virtual gateway that the gateway route is associated
-- with.
newDescribeGatewayRoute ::
  -- | 'gatewayRouteName'
  Prelude.Text ->
  -- | 'meshName'
  Prelude.Text ->
  -- | 'virtualGatewayName'
  Prelude.Text ->
  DescribeGatewayRoute
newDescribeGatewayRoute
  pGatewayRouteName_
  pMeshName_
  pVirtualGatewayName_ =
    DescribeGatewayRoute'
      { meshOwner = Prelude.Nothing,
        gatewayRouteName = pGatewayRouteName_,
        meshName = pMeshName_,
        virtualGatewayName = pVirtualGatewayName_
      }

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
describeGatewayRoute_meshOwner :: Lens.Lens' DescribeGatewayRoute (Prelude.Maybe Prelude.Text)
describeGatewayRoute_meshOwner = Lens.lens (\DescribeGatewayRoute' {meshOwner} -> meshOwner) (\s@DescribeGatewayRoute' {} a -> s {meshOwner = a} :: DescribeGatewayRoute)

-- | The name of the gateway route to describe.
describeGatewayRoute_gatewayRouteName :: Lens.Lens' DescribeGatewayRoute Prelude.Text
describeGatewayRoute_gatewayRouteName = Lens.lens (\DescribeGatewayRoute' {gatewayRouteName} -> gatewayRouteName) (\s@DescribeGatewayRoute' {} a -> s {gatewayRouteName = a} :: DescribeGatewayRoute)

-- | The name of the service mesh that the gateway route resides in.
describeGatewayRoute_meshName :: Lens.Lens' DescribeGatewayRoute Prelude.Text
describeGatewayRoute_meshName = Lens.lens (\DescribeGatewayRoute' {meshName} -> meshName) (\s@DescribeGatewayRoute' {} a -> s {meshName = a} :: DescribeGatewayRoute)

-- | The name of the virtual gateway that the gateway route is associated
-- with.
describeGatewayRoute_virtualGatewayName :: Lens.Lens' DescribeGatewayRoute Prelude.Text
describeGatewayRoute_virtualGatewayName = Lens.lens (\DescribeGatewayRoute' {virtualGatewayName} -> virtualGatewayName) (\s@DescribeGatewayRoute' {} a -> s {virtualGatewayName = a} :: DescribeGatewayRoute)

instance Core.AWSRequest DescribeGatewayRoute where
  type
    AWSResponse DescribeGatewayRoute =
      DescribeGatewayRouteResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGatewayRouteResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DescribeGatewayRoute where
  hashWithSalt _salt DescribeGatewayRoute' {..} =
    _salt
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` gatewayRouteName
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` virtualGatewayName

instance Prelude.NFData DescribeGatewayRoute where
  rnf DescribeGatewayRoute' {..} =
    Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf gatewayRouteName
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf virtualGatewayName

instance Data.ToHeaders DescribeGatewayRoute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeGatewayRoute where
  toPath DescribeGatewayRoute' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Data.toBS meshName,
        "/virtualGateway/",
        Data.toBS virtualGatewayName,
        "/gatewayRoutes/",
        Data.toBS gatewayRouteName
      ]

instance Data.ToQuery DescribeGatewayRoute where
  toQuery DescribeGatewayRoute' {..} =
    Prelude.mconcat ["meshOwner" Data.=: meshOwner]

-- | /See:/ 'newDescribeGatewayRouteResponse' smart constructor.
data DescribeGatewayRouteResponse = DescribeGatewayRouteResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The full description of your gateway route.
    gatewayRoute :: GatewayRouteData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGatewayRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeGatewayRouteResponse_httpStatus' - The response's http status code.
--
-- 'gatewayRoute', 'describeGatewayRouteResponse_gatewayRoute' - The full description of your gateway route.
newDescribeGatewayRouteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'gatewayRoute'
  GatewayRouteData ->
  DescribeGatewayRouteResponse
newDescribeGatewayRouteResponse
  pHttpStatus_
  pGatewayRoute_ =
    DescribeGatewayRouteResponse'
      { httpStatus =
          pHttpStatus_,
        gatewayRoute = pGatewayRoute_
      }

-- | The response's http status code.
describeGatewayRouteResponse_httpStatus :: Lens.Lens' DescribeGatewayRouteResponse Prelude.Int
describeGatewayRouteResponse_httpStatus = Lens.lens (\DescribeGatewayRouteResponse' {httpStatus} -> httpStatus) (\s@DescribeGatewayRouteResponse' {} a -> s {httpStatus = a} :: DescribeGatewayRouteResponse)

-- | The full description of your gateway route.
describeGatewayRouteResponse_gatewayRoute :: Lens.Lens' DescribeGatewayRouteResponse GatewayRouteData
describeGatewayRouteResponse_gatewayRoute = Lens.lens (\DescribeGatewayRouteResponse' {gatewayRoute} -> gatewayRoute) (\s@DescribeGatewayRouteResponse' {} a -> s {gatewayRoute = a} :: DescribeGatewayRouteResponse)

instance Prelude.NFData DescribeGatewayRouteResponse where
  rnf DescribeGatewayRouteResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf gatewayRoute
