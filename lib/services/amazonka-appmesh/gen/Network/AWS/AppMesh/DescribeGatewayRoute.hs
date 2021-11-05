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
-- Module      : Network.AWS.AppMesh.DescribeGatewayRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing gateway route.
module Network.AWS.AppMesh.DescribeGatewayRoute
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

import Network.AWS.AppMesh.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeGatewayRoute' smart constructor.
data DescribeGatewayRoute = DescribeGatewayRoute'
  { -- | The AWS IAM account ID of the service mesh owner. If the account ID is
    -- not your own, then it\'s the ID of the account that shared the mesh with
    -- your account. For more information about mesh sharing, see
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
-- 'meshOwner', 'describeGatewayRoute_meshOwner' - The AWS IAM account ID of the service mesh owner. If the account ID is
-- not your own, then it\'s the ID of the account that shared the mesh with
-- your account. For more information about mesh sharing, see
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

-- | The AWS IAM account ID of the service mesh owner. If the account ID is
-- not your own, then it\'s the ID of the account that shared the mesh with
-- your account. For more information about mesh sharing, see
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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGatewayRouteResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable DescribeGatewayRoute

instance Prelude.NFData DescribeGatewayRoute

instance Core.ToHeaders DescribeGatewayRoute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeGatewayRoute where
  toPath DescribeGatewayRoute' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Core.toBS meshName,
        "/virtualGateway/",
        Core.toBS virtualGatewayName,
        "/gatewayRoutes/",
        Core.toBS gatewayRouteName
      ]

instance Core.ToQuery DescribeGatewayRoute where
  toQuery DescribeGatewayRoute' {..} =
    Prelude.mconcat ["meshOwner" Core.=: meshOwner]

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

instance Prelude.NFData DescribeGatewayRouteResponse
