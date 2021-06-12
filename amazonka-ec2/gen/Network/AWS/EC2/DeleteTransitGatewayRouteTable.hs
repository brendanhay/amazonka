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
-- Module      : Network.AWS.EC2.DeleteTransitGatewayRouteTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified transit gateway route table. You must disassociate
-- the route table from any transit gateway route tables before you can
-- delete it.
module Network.AWS.EC2.DeleteTransitGatewayRouteTable
  ( -- * Creating a Request
    DeleteTransitGatewayRouteTable (..),
    newDeleteTransitGatewayRouteTable,

    -- * Request Lenses
    deleteTransitGatewayRouteTable_dryRun,
    deleteTransitGatewayRouteTable_transitGatewayRouteTableId,

    -- * Destructuring the Response
    DeleteTransitGatewayRouteTableResponse (..),
    newDeleteTransitGatewayRouteTableResponse,

    -- * Response Lenses
    deleteTransitGatewayRouteTableResponse_transitGatewayRouteTable,
    deleteTransitGatewayRouteTableResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTransitGatewayRouteTable' smart constructor.
data DeleteTransitGatewayRouteTable = DeleteTransitGatewayRouteTable'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayRouteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteTransitGatewayRouteTable_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayRouteTableId', 'deleteTransitGatewayRouteTable_transitGatewayRouteTableId' - The ID of the transit gateway route table.
newDeleteTransitGatewayRouteTable ::
  -- | 'transitGatewayRouteTableId'
  Core.Text ->
  DeleteTransitGatewayRouteTable
newDeleteTransitGatewayRouteTable
  pTransitGatewayRouteTableId_ =
    DeleteTransitGatewayRouteTable'
      { dryRun =
          Core.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTransitGatewayRouteTable_dryRun :: Lens.Lens' DeleteTransitGatewayRouteTable (Core.Maybe Core.Bool)
deleteTransitGatewayRouteTable_dryRun = Lens.lens (\DeleteTransitGatewayRouteTable' {dryRun} -> dryRun) (\s@DeleteTransitGatewayRouteTable' {} a -> s {dryRun = a} :: DeleteTransitGatewayRouteTable)

-- | The ID of the transit gateway route table.
deleteTransitGatewayRouteTable_transitGatewayRouteTableId :: Lens.Lens' DeleteTransitGatewayRouteTable Core.Text
deleteTransitGatewayRouteTable_transitGatewayRouteTableId = Lens.lens (\DeleteTransitGatewayRouteTable' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@DeleteTransitGatewayRouteTable' {} a -> s {transitGatewayRouteTableId = a} :: DeleteTransitGatewayRouteTable)

instance
  Core.AWSRequest
    DeleteTransitGatewayRouteTable
  where
  type
    AWSResponse DeleteTransitGatewayRouteTable =
      DeleteTransitGatewayRouteTableResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayRouteTableResponse'
            Core.<$> (x Core..@? "transitGatewayRouteTable")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteTransitGatewayRouteTable

instance Core.NFData DeleteTransitGatewayRouteTable

instance
  Core.ToHeaders
    DeleteTransitGatewayRouteTable
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteTransitGatewayRouteTable where
  toPath = Core.const "/"

instance Core.ToQuery DeleteTransitGatewayRouteTable where
  toQuery DeleteTransitGatewayRouteTable' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DeleteTransitGatewayRouteTable" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "TransitGatewayRouteTableId"
          Core.=: transitGatewayRouteTableId
      ]

-- | /See:/ 'newDeleteTransitGatewayRouteTableResponse' smart constructor.
data DeleteTransitGatewayRouteTableResponse = DeleteTransitGatewayRouteTableResponse'
  { -- | Information about the deleted transit gateway route table.
    transitGatewayRouteTable :: Core.Maybe TransitGatewayRouteTable,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayRouteTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayRouteTable', 'deleteTransitGatewayRouteTableResponse_transitGatewayRouteTable' - Information about the deleted transit gateway route table.
--
-- 'httpStatus', 'deleteTransitGatewayRouteTableResponse_httpStatus' - The response's http status code.
newDeleteTransitGatewayRouteTableResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteTransitGatewayRouteTableResponse
newDeleteTransitGatewayRouteTableResponse
  pHttpStatus_ =
    DeleteTransitGatewayRouteTableResponse'
      { transitGatewayRouteTable =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the deleted transit gateway route table.
deleteTransitGatewayRouteTableResponse_transitGatewayRouteTable :: Lens.Lens' DeleteTransitGatewayRouteTableResponse (Core.Maybe TransitGatewayRouteTable)
deleteTransitGatewayRouteTableResponse_transitGatewayRouteTable = Lens.lens (\DeleteTransitGatewayRouteTableResponse' {transitGatewayRouteTable} -> transitGatewayRouteTable) (\s@DeleteTransitGatewayRouteTableResponse' {} a -> s {transitGatewayRouteTable = a} :: DeleteTransitGatewayRouteTableResponse)

-- | The response's http status code.
deleteTransitGatewayRouteTableResponse_httpStatus :: Lens.Lens' DeleteTransitGatewayRouteTableResponse Core.Int
deleteTransitGatewayRouteTableResponse_httpStatus = Lens.lens (\DeleteTransitGatewayRouteTableResponse' {httpStatus} -> httpStatus) (\s@DeleteTransitGatewayRouteTableResponse' {} a -> s {httpStatus = a} :: DeleteTransitGatewayRouteTableResponse)

instance
  Core.NFData
    DeleteTransitGatewayRouteTableResponse
