{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.DeleteLocalGatewayRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route from the specified local gateway route
-- table.
module Network.AWS.EC2.DeleteLocalGatewayRoute
  ( -- * Creating a Request
    DeleteLocalGatewayRoute (..),
    newDeleteLocalGatewayRoute,

    -- * Request Lenses
    deleteLocalGatewayRoute_dryRun,
    deleteLocalGatewayRoute_destinationCidrBlock,
    deleteLocalGatewayRoute_localGatewayRouteTableId,

    -- * Destructuring the Response
    DeleteLocalGatewayRouteResponse (..),
    newDeleteLocalGatewayRouteResponse,

    -- * Response Lenses
    deleteLocalGatewayRouteResponse_route,
    deleteLocalGatewayRouteResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLocalGatewayRoute' smart constructor.
data DeleteLocalGatewayRoute = DeleteLocalGatewayRoute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The CIDR range for the route. This must match the CIDR for the route
    -- exactly.
    destinationCidrBlock :: Prelude.Text,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLocalGatewayRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteLocalGatewayRoute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'destinationCidrBlock', 'deleteLocalGatewayRoute_destinationCidrBlock' - The CIDR range for the route. This must match the CIDR for the route
-- exactly.
--
-- 'localGatewayRouteTableId', 'deleteLocalGatewayRoute_localGatewayRouteTableId' - The ID of the local gateway route table.
newDeleteLocalGatewayRoute ::
  -- | 'destinationCidrBlock'
  Prelude.Text ->
  -- | 'localGatewayRouteTableId'
  Prelude.Text ->
  DeleteLocalGatewayRoute
newDeleteLocalGatewayRoute
  pDestinationCidrBlock_
  pLocalGatewayRouteTableId_ =
    DeleteLocalGatewayRoute'
      { dryRun = Prelude.Nothing,
        destinationCidrBlock = pDestinationCidrBlock_,
        localGatewayRouteTableId =
          pLocalGatewayRouteTableId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteLocalGatewayRoute_dryRun :: Lens.Lens' DeleteLocalGatewayRoute (Prelude.Maybe Prelude.Bool)
deleteLocalGatewayRoute_dryRun = Lens.lens (\DeleteLocalGatewayRoute' {dryRun} -> dryRun) (\s@DeleteLocalGatewayRoute' {} a -> s {dryRun = a} :: DeleteLocalGatewayRoute)

-- | The CIDR range for the route. This must match the CIDR for the route
-- exactly.
deleteLocalGatewayRoute_destinationCidrBlock :: Lens.Lens' DeleteLocalGatewayRoute Prelude.Text
deleteLocalGatewayRoute_destinationCidrBlock = Lens.lens (\DeleteLocalGatewayRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@DeleteLocalGatewayRoute' {} a -> s {destinationCidrBlock = a} :: DeleteLocalGatewayRoute)

-- | The ID of the local gateway route table.
deleteLocalGatewayRoute_localGatewayRouteTableId :: Lens.Lens' DeleteLocalGatewayRoute Prelude.Text
deleteLocalGatewayRoute_localGatewayRouteTableId = Lens.lens (\DeleteLocalGatewayRoute' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@DeleteLocalGatewayRoute' {} a -> s {localGatewayRouteTableId = a} :: DeleteLocalGatewayRoute)

instance Prelude.AWSRequest DeleteLocalGatewayRoute where
  type
    Rs DeleteLocalGatewayRoute =
      DeleteLocalGatewayRouteResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteLocalGatewayRouteResponse'
            Prelude.<$> (x Prelude..@? "route")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLocalGatewayRoute

instance Prelude.NFData DeleteLocalGatewayRoute

instance Prelude.ToHeaders DeleteLocalGatewayRoute where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteLocalGatewayRoute where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteLocalGatewayRoute where
  toQuery DeleteLocalGatewayRoute' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteLocalGatewayRoute" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "DestinationCidrBlock"
          Prelude.=: destinationCidrBlock,
        "LocalGatewayRouteTableId"
          Prelude.=: localGatewayRouteTableId
      ]

-- | /See:/ 'newDeleteLocalGatewayRouteResponse' smart constructor.
data DeleteLocalGatewayRouteResponse = DeleteLocalGatewayRouteResponse'
  { -- | Information about the route.
    route :: Prelude.Maybe LocalGatewayRoute,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLocalGatewayRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'route', 'deleteLocalGatewayRouteResponse_route' - Information about the route.
--
-- 'httpStatus', 'deleteLocalGatewayRouteResponse_httpStatus' - The response's http status code.
newDeleteLocalGatewayRouteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLocalGatewayRouteResponse
newDeleteLocalGatewayRouteResponse pHttpStatus_ =
  DeleteLocalGatewayRouteResponse'
    { route =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the route.
deleteLocalGatewayRouteResponse_route :: Lens.Lens' DeleteLocalGatewayRouteResponse (Prelude.Maybe LocalGatewayRoute)
deleteLocalGatewayRouteResponse_route = Lens.lens (\DeleteLocalGatewayRouteResponse' {route} -> route) (\s@DeleteLocalGatewayRouteResponse' {} a -> s {route = a} :: DeleteLocalGatewayRouteResponse)

-- | The response's http status code.
deleteLocalGatewayRouteResponse_httpStatus :: Lens.Lens' DeleteLocalGatewayRouteResponse Prelude.Int
deleteLocalGatewayRouteResponse_httpStatus = Lens.lens (\DeleteLocalGatewayRouteResponse' {httpStatus} -> httpStatus) (\s@DeleteLocalGatewayRouteResponse' {} a -> s {httpStatus = a} :: DeleteLocalGatewayRouteResponse)

instance
  Prelude.NFData
    DeleteLocalGatewayRouteResponse
