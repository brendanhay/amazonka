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
-- Module      : Amazonka.EC2.DeleteTransitGatewayRouteTable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified transit gateway route table. You must disassociate
-- the route table from any transit gateway route tables before you can
-- delete it.
module Amazonka.EC2.DeleteTransitGatewayRouteTable
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTransitGatewayRouteTable' smart constructor.
data DeleteTransitGatewayRouteTable = DeleteTransitGatewayRouteTable'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteTransitGatewayRouteTable
newDeleteTransitGatewayRouteTable
  pTransitGatewayRouteTableId_ =
    DeleteTransitGatewayRouteTable'
      { dryRun =
          Prelude.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTransitGatewayRouteTable_dryRun :: Lens.Lens' DeleteTransitGatewayRouteTable (Prelude.Maybe Prelude.Bool)
deleteTransitGatewayRouteTable_dryRun = Lens.lens (\DeleteTransitGatewayRouteTable' {dryRun} -> dryRun) (\s@DeleteTransitGatewayRouteTable' {} a -> s {dryRun = a} :: DeleteTransitGatewayRouteTable)

-- | The ID of the transit gateway route table.
deleteTransitGatewayRouteTable_transitGatewayRouteTableId :: Lens.Lens' DeleteTransitGatewayRouteTable Prelude.Text
deleteTransitGatewayRouteTable_transitGatewayRouteTableId = Lens.lens (\DeleteTransitGatewayRouteTable' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@DeleteTransitGatewayRouteTable' {} a -> s {transitGatewayRouteTableId = a} :: DeleteTransitGatewayRouteTable)

instance
  Core.AWSRequest
    DeleteTransitGatewayRouteTable
  where
  type
    AWSResponse DeleteTransitGatewayRouteTable =
      DeleteTransitGatewayRouteTableResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayRouteTableResponse'
            Prelude.<$> (x Core..@? "transitGatewayRouteTable")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteTransitGatewayRouteTable
  where
  hashWithSalt
    _salt
    DeleteTransitGatewayRouteTable' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` transitGatewayRouteTableId

instance
  Prelude.NFData
    DeleteTransitGatewayRouteTable
  where
  rnf DeleteTransitGatewayRouteTable' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableId

instance
  Core.ToHeaders
    DeleteTransitGatewayRouteTable
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteTransitGatewayRouteTable where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteTransitGatewayRouteTable where
  toQuery DeleteTransitGatewayRouteTable' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DeleteTransitGatewayRouteTable" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "TransitGatewayRouteTableId"
          Core.=: transitGatewayRouteTableId
      ]

-- | /See:/ 'newDeleteTransitGatewayRouteTableResponse' smart constructor.
data DeleteTransitGatewayRouteTableResponse = DeleteTransitGatewayRouteTableResponse'
  { -- | Information about the deleted transit gateway route table.
    transitGatewayRouteTable :: Prelude.Maybe TransitGatewayRouteTable,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteTransitGatewayRouteTableResponse
newDeleteTransitGatewayRouteTableResponse
  pHttpStatus_ =
    DeleteTransitGatewayRouteTableResponse'
      { transitGatewayRouteTable =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the deleted transit gateway route table.
deleteTransitGatewayRouteTableResponse_transitGatewayRouteTable :: Lens.Lens' DeleteTransitGatewayRouteTableResponse (Prelude.Maybe TransitGatewayRouteTable)
deleteTransitGatewayRouteTableResponse_transitGatewayRouteTable = Lens.lens (\DeleteTransitGatewayRouteTableResponse' {transitGatewayRouteTable} -> transitGatewayRouteTable) (\s@DeleteTransitGatewayRouteTableResponse' {} a -> s {transitGatewayRouteTable = a} :: DeleteTransitGatewayRouteTableResponse)

-- | The response's http status code.
deleteTransitGatewayRouteTableResponse_httpStatus :: Lens.Lens' DeleteTransitGatewayRouteTableResponse Prelude.Int
deleteTransitGatewayRouteTableResponse_httpStatus = Lens.lens (\DeleteTransitGatewayRouteTableResponse' {httpStatus} -> httpStatus) (\s@DeleteTransitGatewayRouteTableResponse' {} a -> s {httpStatus = a} :: DeleteTransitGatewayRouteTableResponse)

instance
  Prelude.NFData
    DeleteTransitGatewayRouteTableResponse
  where
  rnf DeleteTransitGatewayRouteTableResponse' {..} =
    Prelude.rnf transitGatewayRouteTable
      `Prelude.seq` Prelude.rnf httpStatus
