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
-- Module      : Amazonka.EC2.DeleteTransitGatewayRoute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified route from the specified transit gateway route
-- table.
module Amazonka.EC2.DeleteTransitGatewayRoute
  ( -- * Creating a Request
    DeleteTransitGatewayRoute (..),
    newDeleteTransitGatewayRoute,

    -- * Request Lenses
    deleteTransitGatewayRoute_dryRun,
    deleteTransitGatewayRoute_transitGatewayRouteTableId,
    deleteTransitGatewayRoute_destinationCidrBlock,

    -- * Destructuring the Response
    DeleteTransitGatewayRouteResponse (..),
    newDeleteTransitGatewayRouteResponse,

    -- * Response Lenses
    deleteTransitGatewayRouteResponse_route,
    deleteTransitGatewayRouteResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTransitGatewayRoute' smart constructor.
data DeleteTransitGatewayRoute = DeleteTransitGatewayRoute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Prelude.Text,
    -- | The CIDR range for the route. This must match the CIDR for the route
    -- exactly.
    destinationCidrBlock :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteTransitGatewayRoute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayRouteTableId', 'deleteTransitGatewayRoute_transitGatewayRouteTableId' - The ID of the transit gateway route table.
--
-- 'destinationCidrBlock', 'deleteTransitGatewayRoute_destinationCidrBlock' - The CIDR range for the route. This must match the CIDR for the route
-- exactly.
newDeleteTransitGatewayRoute ::
  -- | 'transitGatewayRouteTableId'
  Prelude.Text ->
  -- | 'destinationCidrBlock'
  Prelude.Text ->
  DeleteTransitGatewayRoute
newDeleteTransitGatewayRoute
  pTransitGatewayRouteTableId_
  pDestinationCidrBlock_ =
    DeleteTransitGatewayRoute'
      { dryRun =
          Prelude.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        destinationCidrBlock = pDestinationCidrBlock_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTransitGatewayRoute_dryRun :: Lens.Lens' DeleteTransitGatewayRoute (Prelude.Maybe Prelude.Bool)
deleteTransitGatewayRoute_dryRun = Lens.lens (\DeleteTransitGatewayRoute' {dryRun} -> dryRun) (\s@DeleteTransitGatewayRoute' {} a -> s {dryRun = a} :: DeleteTransitGatewayRoute)

-- | The ID of the transit gateway route table.
deleteTransitGatewayRoute_transitGatewayRouteTableId :: Lens.Lens' DeleteTransitGatewayRoute Prelude.Text
deleteTransitGatewayRoute_transitGatewayRouteTableId = Lens.lens (\DeleteTransitGatewayRoute' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@DeleteTransitGatewayRoute' {} a -> s {transitGatewayRouteTableId = a} :: DeleteTransitGatewayRoute)

-- | The CIDR range for the route. This must match the CIDR for the route
-- exactly.
deleteTransitGatewayRoute_destinationCidrBlock :: Lens.Lens' DeleteTransitGatewayRoute Prelude.Text
deleteTransitGatewayRoute_destinationCidrBlock = Lens.lens (\DeleteTransitGatewayRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@DeleteTransitGatewayRoute' {} a -> s {destinationCidrBlock = a} :: DeleteTransitGatewayRoute)

instance Core.AWSRequest DeleteTransitGatewayRoute where
  type
    AWSResponse DeleteTransitGatewayRoute =
      DeleteTransitGatewayRouteResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayRouteResponse'
            Prelude.<$> (x Data..@? "route")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTransitGatewayRoute where
  hashWithSalt _salt DeleteTransitGatewayRoute' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` transitGatewayRouteTableId
      `Prelude.hashWithSalt` destinationCidrBlock

instance Prelude.NFData DeleteTransitGatewayRoute where
  rnf DeleteTransitGatewayRoute' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableId
      `Prelude.seq` Prelude.rnf destinationCidrBlock

instance Data.ToHeaders DeleteTransitGatewayRoute where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteTransitGatewayRoute where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTransitGatewayRoute where
  toQuery DeleteTransitGatewayRoute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteTransitGatewayRoute" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "TransitGatewayRouteTableId"
          Data.=: transitGatewayRouteTableId,
        "DestinationCidrBlock" Data.=: destinationCidrBlock
      ]

-- | /See:/ 'newDeleteTransitGatewayRouteResponse' smart constructor.
data DeleteTransitGatewayRouteResponse = DeleteTransitGatewayRouteResponse'
  { -- | Information about the route.
    route :: Prelude.Maybe TransitGatewayRoute,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'route', 'deleteTransitGatewayRouteResponse_route' - Information about the route.
--
-- 'httpStatus', 'deleteTransitGatewayRouteResponse_httpStatus' - The response's http status code.
newDeleteTransitGatewayRouteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTransitGatewayRouteResponse
newDeleteTransitGatewayRouteResponse pHttpStatus_ =
  DeleteTransitGatewayRouteResponse'
    { route =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the route.
deleteTransitGatewayRouteResponse_route :: Lens.Lens' DeleteTransitGatewayRouteResponse (Prelude.Maybe TransitGatewayRoute)
deleteTransitGatewayRouteResponse_route = Lens.lens (\DeleteTransitGatewayRouteResponse' {route} -> route) (\s@DeleteTransitGatewayRouteResponse' {} a -> s {route = a} :: DeleteTransitGatewayRouteResponse)

-- | The response's http status code.
deleteTransitGatewayRouteResponse_httpStatus :: Lens.Lens' DeleteTransitGatewayRouteResponse Prelude.Int
deleteTransitGatewayRouteResponse_httpStatus = Lens.lens (\DeleteTransitGatewayRouteResponse' {httpStatus} -> httpStatus) (\s@DeleteTransitGatewayRouteResponse' {} a -> s {httpStatus = a} :: DeleteTransitGatewayRouteResponse)

instance
  Prelude.NFData
    DeleteTransitGatewayRouteResponse
  where
  rnf DeleteTransitGatewayRouteResponse' {..} =
    Prelude.rnf route
      `Prelude.seq` Prelude.rnf httpStatus
