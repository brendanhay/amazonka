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
-- Module      : Amazonka.EC2.AssociateRouteTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a subnet in your VPC or an internet gateway or virtual
-- private gateway attached to your VPC with a route table in your VPC.
-- This association causes traffic from the subnet or gateway to be routed
-- according to the routes in the route table. The action returns an
-- association ID, which you need in order to disassociate the route table
-- later. A route table can be associated with multiple subnets.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route tables>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Amazonka.EC2.AssociateRouteTable
  ( -- * Creating a Request
    AssociateRouteTable (..),
    newAssociateRouteTable,

    -- * Request Lenses
    associateRouteTable_dryRun,
    associateRouteTable_gatewayId,
    associateRouteTable_subnetId,
    associateRouteTable_routeTableId,

    -- * Destructuring the Response
    AssociateRouteTableResponse (..),
    newAssociateRouteTableResponse,

    -- * Response Lenses
    associateRouteTableResponse_associationId,
    associateRouteTableResponse_associationState,
    associateRouteTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateRouteTable' smart constructor.
data AssociateRouteTable = AssociateRouteTable'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the internet gateway or virtual private gateway.
    gatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the route table.
    routeTableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateRouteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'associateRouteTable_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'gatewayId', 'associateRouteTable_gatewayId' - The ID of the internet gateway or virtual private gateway.
--
-- 'subnetId', 'associateRouteTable_subnetId' - The ID of the subnet.
--
-- 'routeTableId', 'associateRouteTable_routeTableId' - The ID of the route table.
newAssociateRouteTable ::
  -- | 'routeTableId'
  Prelude.Text ->
  AssociateRouteTable
newAssociateRouteTable pRouteTableId_ =
  AssociateRouteTable'
    { dryRun = Prelude.Nothing,
      gatewayId = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      routeTableId = pRouteTableId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
associateRouteTable_dryRun :: Lens.Lens' AssociateRouteTable (Prelude.Maybe Prelude.Bool)
associateRouteTable_dryRun = Lens.lens (\AssociateRouteTable' {dryRun} -> dryRun) (\s@AssociateRouteTable' {} a -> s {dryRun = a} :: AssociateRouteTable)

-- | The ID of the internet gateway or virtual private gateway.
associateRouteTable_gatewayId :: Lens.Lens' AssociateRouteTable (Prelude.Maybe Prelude.Text)
associateRouteTable_gatewayId = Lens.lens (\AssociateRouteTable' {gatewayId} -> gatewayId) (\s@AssociateRouteTable' {} a -> s {gatewayId = a} :: AssociateRouteTable)

-- | The ID of the subnet.
associateRouteTable_subnetId :: Lens.Lens' AssociateRouteTable (Prelude.Maybe Prelude.Text)
associateRouteTable_subnetId = Lens.lens (\AssociateRouteTable' {subnetId} -> subnetId) (\s@AssociateRouteTable' {} a -> s {subnetId = a} :: AssociateRouteTable)

-- | The ID of the route table.
associateRouteTable_routeTableId :: Lens.Lens' AssociateRouteTable Prelude.Text
associateRouteTable_routeTableId = Lens.lens (\AssociateRouteTable' {routeTableId} -> routeTableId) (\s@AssociateRouteTable' {} a -> s {routeTableId = a} :: AssociateRouteTable)

instance Core.AWSRequest AssociateRouteTable where
  type
    AWSResponse AssociateRouteTable =
      AssociateRouteTableResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateRouteTableResponse'
            Prelude.<$> (x Data..@? "associationId")
            Prelude.<*> (x Data..@? "associationState")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateRouteTable where
  hashWithSalt _salt AssociateRouteTable' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` gatewayId
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` routeTableId

instance Prelude.NFData AssociateRouteTable where
  rnf AssociateRouteTable' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf gatewayId
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf routeTableId

instance Data.ToHeaders AssociateRouteTable where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AssociateRouteTable where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateRouteTable where
  toQuery AssociateRouteTable' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AssociateRouteTable" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "GatewayId" Data.=: gatewayId,
        "SubnetId" Data.=: subnetId,
        "RouteTableId" Data.=: routeTableId
      ]

-- | /See:/ 'newAssociateRouteTableResponse' smart constructor.
data AssociateRouteTableResponse = AssociateRouteTableResponse'
  { -- | The route table association ID. This ID is required for disassociating
    -- the route table.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The state of the association.
    associationState :: Prelude.Maybe RouteTableAssociationState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateRouteTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'associateRouteTableResponse_associationId' - The route table association ID. This ID is required for disassociating
-- the route table.
--
-- 'associationState', 'associateRouteTableResponse_associationState' - The state of the association.
--
-- 'httpStatus', 'associateRouteTableResponse_httpStatus' - The response's http status code.
newAssociateRouteTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateRouteTableResponse
newAssociateRouteTableResponse pHttpStatus_ =
  AssociateRouteTableResponse'
    { associationId =
        Prelude.Nothing,
      associationState = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The route table association ID. This ID is required for disassociating
-- the route table.
associateRouteTableResponse_associationId :: Lens.Lens' AssociateRouteTableResponse (Prelude.Maybe Prelude.Text)
associateRouteTableResponse_associationId = Lens.lens (\AssociateRouteTableResponse' {associationId} -> associationId) (\s@AssociateRouteTableResponse' {} a -> s {associationId = a} :: AssociateRouteTableResponse)

-- | The state of the association.
associateRouteTableResponse_associationState :: Lens.Lens' AssociateRouteTableResponse (Prelude.Maybe RouteTableAssociationState)
associateRouteTableResponse_associationState = Lens.lens (\AssociateRouteTableResponse' {associationState} -> associationState) (\s@AssociateRouteTableResponse' {} a -> s {associationState = a} :: AssociateRouteTableResponse)

-- | The response's http status code.
associateRouteTableResponse_httpStatus :: Lens.Lens' AssociateRouteTableResponse Prelude.Int
associateRouteTableResponse_httpStatus = Lens.lens (\AssociateRouteTableResponse' {httpStatus} -> httpStatus) (\s@AssociateRouteTableResponse' {} a -> s {httpStatus = a} :: AssociateRouteTableResponse)

instance Prelude.NFData AssociateRouteTableResponse where
  rnf AssociateRouteTableResponse' {..} =
    Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf associationState
      `Prelude.seq` Prelude.rnf httpStatus
