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
-- Module      : Network.AWS.EC2.AssociateRouteTable
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route Tables>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Network.AWS.EC2.AssociateRouteTable
  ( -- * Creating a Request
    AssociateRouteTable (..),
    newAssociateRouteTable,

    -- * Request Lenses
    associateRouteTable_dryRun,
    associateRouteTable_subnetId,
    associateRouteTable_gatewayId,
    associateRouteTable_routeTableId,

    -- * Destructuring the Response
    AssociateRouteTableResponse (..),
    newAssociateRouteTableResponse,

    -- * Response Lenses
    associateRouteTableResponse_associationState,
    associateRouteTableResponse_associationId,
    associateRouteTableResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateRouteTable' smart constructor.
data AssociateRouteTable = AssociateRouteTable'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the subnet.
    subnetId :: Core.Maybe Core.Text,
    -- | The ID of the internet gateway or virtual private gateway.
    gatewayId :: Core.Maybe Core.Text,
    -- | The ID of the route table.
    routeTableId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'subnetId', 'associateRouteTable_subnetId' - The ID of the subnet.
--
-- 'gatewayId', 'associateRouteTable_gatewayId' - The ID of the internet gateway or virtual private gateway.
--
-- 'routeTableId', 'associateRouteTable_routeTableId' - The ID of the route table.
newAssociateRouteTable ::
  -- | 'routeTableId'
  Core.Text ->
  AssociateRouteTable
newAssociateRouteTable pRouteTableId_ =
  AssociateRouteTable'
    { dryRun = Core.Nothing,
      subnetId = Core.Nothing,
      gatewayId = Core.Nothing,
      routeTableId = pRouteTableId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
associateRouteTable_dryRun :: Lens.Lens' AssociateRouteTable (Core.Maybe Core.Bool)
associateRouteTable_dryRun = Lens.lens (\AssociateRouteTable' {dryRun} -> dryRun) (\s@AssociateRouteTable' {} a -> s {dryRun = a} :: AssociateRouteTable)

-- | The ID of the subnet.
associateRouteTable_subnetId :: Lens.Lens' AssociateRouteTable (Core.Maybe Core.Text)
associateRouteTable_subnetId = Lens.lens (\AssociateRouteTable' {subnetId} -> subnetId) (\s@AssociateRouteTable' {} a -> s {subnetId = a} :: AssociateRouteTable)

-- | The ID of the internet gateway or virtual private gateway.
associateRouteTable_gatewayId :: Lens.Lens' AssociateRouteTable (Core.Maybe Core.Text)
associateRouteTable_gatewayId = Lens.lens (\AssociateRouteTable' {gatewayId} -> gatewayId) (\s@AssociateRouteTable' {} a -> s {gatewayId = a} :: AssociateRouteTable)

-- | The ID of the route table.
associateRouteTable_routeTableId :: Lens.Lens' AssociateRouteTable Core.Text
associateRouteTable_routeTableId = Lens.lens (\AssociateRouteTable' {routeTableId} -> routeTableId) (\s@AssociateRouteTable' {} a -> s {routeTableId = a} :: AssociateRouteTable)

instance Core.AWSRequest AssociateRouteTable where
  type
    AWSResponse AssociateRouteTable =
      AssociateRouteTableResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateRouteTableResponse'
            Core.<$> (x Core..@? "associationState")
            Core.<*> (x Core..@? "associationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociateRouteTable

instance Core.NFData AssociateRouteTable

instance Core.ToHeaders AssociateRouteTable where
  toHeaders = Core.const Core.mempty

instance Core.ToPath AssociateRouteTable where
  toPath = Core.const "/"

instance Core.ToQuery AssociateRouteTable where
  toQuery AssociateRouteTable' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("AssociateRouteTable" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "SubnetId" Core.=: subnetId,
        "GatewayId" Core.=: gatewayId,
        "RouteTableId" Core.=: routeTableId
      ]

-- | /See:/ 'newAssociateRouteTableResponse' smart constructor.
data AssociateRouteTableResponse = AssociateRouteTableResponse'
  { -- | The state of the association.
    associationState :: Core.Maybe RouteTableAssociationState,
    -- | The route table association ID. This ID is required for disassociating
    -- the route table.
    associationId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateRouteTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationState', 'associateRouteTableResponse_associationState' - The state of the association.
--
-- 'associationId', 'associateRouteTableResponse_associationId' - The route table association ID. This ID is required for disassociating
-- the route table.
--
-- 'httpStatus', 'associateRouteTableResponse_httpStatus' - The response's http status code.
newAssociateRouteTableResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateRouteTableResponse
newAssociateRouteTableResponse pHttpStatus_ =
  AssociateRouteTableResponse'
    { associationState =
        Core.Nothing,
      associationId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The state of the association.
associateRouteTableResponse_associationState :: Lens.Lens' AssociateRouteTableResponse (Core.Maybe RouteTableAssociationState)
associateRouteTableResponse_associationState = Lens.lens (\AssociateRouteTableResponse' {associationState} -> associationState) (\s@AssociateRouteTableResponse' {} a -> s {associationState = a} :: AssociateRouteTableResponse)

-- | The route table association ID. This ID is required for disassociating
-- the route table.
associateRouteTableResponse_associationId :: Lens.Lens' AssociateRouteTableResponse (Core.Maybe Core.Text)
associateRouteTableResponse_associationId = Lens.lens (\AssociateRouteTableResponse' {associationId} -> associationId) (\s@AssociateRouteTableResponse' {} a -> s {associationId = a} :: AssociateRouteTableResponse)

-- | The response's http status code.
associateRouteTableResponse_httpStatus :: Lens.Lens' AssociateRouteTableResponse Core.Int
associateRouteTableResponse_httpStatus = Lens.lens (\AssociateRouteTableResponse' {httpStatus} -> httpStatus) (\s@AssociateRouteTableResponse' {} a -> s {httpStatus = a} :: AssociateRouteTableResponse)

instance Core.NFData AssociateRouteTableResponse
