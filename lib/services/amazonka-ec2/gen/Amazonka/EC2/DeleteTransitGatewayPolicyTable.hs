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
-- Module      : Amazonka.EC2.DeleteTransitGatewayPolicyTable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified transit gateway policy table.
module Amazonka.EC2.DeleteTransitGatewayPolicyTable
  ( -- * Creating a Request
    DeleteTransitGatewayPolicyTable (..),
    newDeleteTransitGatewayPolicyTable,

    -- * Request Lenses
    deleteTransitGatewayPolicyTable_dryRun,
    deleteTransitGatewayPolicyTable_transitGatewayPolicyTableId,

    -- * Destructuring the Response
    DeleteTransitGatewayPolicyTableResponse (..),
    newDeleteTransitGatewayPolicyTableResponse,

    -- * Response Lenses
    deleteTransitGatewayPolicyTableResponse_transitGatewayPolicyTable,
    deleteTransitGatewayPolicyTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTransitGatewayPolicyTable' smart constructor.
data DeleteTransitGatewayPolicyTable = DeleteTransitGatewayPolicyTable'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The transit gateway policy table to delete.
    transitGatewayPolicyTableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayPolicyTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteTransitGatewayPolicyTable_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayPolicyTableId', 'deleteTransitGatewayPolicyTable_transitGatewayPolicyTableId' - The transit gateway policy table to delete.
newDeleteTransitGatewayPolicyTable ::
  -- | 'transitGatewayPolicyTableId'
  Prelude.Text ->
  DeleteTransitGatewayPolicyTable
newDeleteTransitGatewayPolicyTable
  pTransitGatewayPolicyTableId_ =
    DeleteTransitGatewayPolicyTable'
      { dryRun =
          Prelude.Nothing,
        transitGatewayPolicyTableId =
          pTransitGatewayPolicyTableId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTransitGatewayPolicyTable_dryRun :: Lens.Lens' DeleteTransitGatewayPolicyTable (Prelude.Maybe Prelude.Bool)
deleteTransitGatewayPolicyTable_dryRun = Lens.lens (\DeleteTransitGatewayPolicyTable' {dryRun} -> dryRun) (\s@DeleteTransitGatewayPolicyTable' {} a -> s {dryRun = a} :: DeleteTransitGatewayPolicyTable)

-- | The transit gateway policy table to delete.
deleteTransitGatewayPolicyTable_transitGatewayPolicyTableId :: Lens.Lens' DeleteTransitGatewayPolicyTable Prelude.Text
deleteTransitGatewayPolicyTable_transitGatewayPolicyTableId = Lens.lens (\DeleteTransitGatewayPolicyTable' {transitGatewayPolicyTableId} -> transitGatewayPolicyTableId) (\s@DeleteTransitGatewayPolicyTable' {} a -> s {transitGatewayPolicyTableId = a} :: DeleteTransitGatewayPolicyTable)

instance
  Core.AWSRequest
    DeleteTransitGatewayPolicyTable
  where
  type
    AWSResponse DeleteTransitGatewayPolicyTable =
      DeleteTransitGatewayPolicyTableResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayPolicyTableResponse'
            Prelude.<$> (x Data..@? "transitGatewayPolicyTable")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteTransitGatewayPolicyTable
  where
  hashWithSalt
    _salt
    DeleteTransitGatewayPolicyTable' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` transitGatewayPolicyTableId

instance
  Prelude.NFData
    DeleteTransitGatewayPolicyTable
  where
  rnf DeleteTransitGatewayPolicyTable' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf transitGatewayPolicyTableId

instance
  Data.ToHeaders
    DeleteTransitGatewayPolicyTable
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteTransitGatewayPolicyTable where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTransitGatewayPolicyTable where
  toQuery DeleteTransitGatewayPolicyTable' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteTransitGatewayPolicyTable" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "TransitGatewayPolicyTableId"
          Data.=: transitGatewayPolicyTableId
      ]

-- | /See:/ 'newDeleteTransitGatewayPolicyTableResponse' smart constructor.
data DeleteTransitGatewayPolicyTableResponse = DeleteTransitGatewayPolicyTableResponse'
  { -- | Provides details about the deleted transit gateway policy table.
    transitGatewayPolicyTable :: Prelude.Maybe TransitGatewayPolicyTable,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayPolicyTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayPolicyTable', 'deleteTransitGatewayPolicyTableResponse_transitGatewayPolicyTable' - Provides details about the deleted transit gateway policy table.
--
-- 'httpStatus', 'deleteTransitGatewayPolicyTableResponse_httpStatus' - The response's http status code.
newDeleteTransitGatewayPolicyTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTransitGatewayPolicyTableResponse
newDeleteTransitGatewayPolicyTableResponse
  pHttpStatus_ =
    DeleteTransitGatewayPolicyTableResponse'
      { transitGatewayPolicyTable =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Provides details about the deleted transit gateway policy table.
deleteTransitGatewayPolicyTableResponse_transitGatewayPolicyTable :: Lens.Lens' DeleteTransitGatewayPolicyTableResponse (Prelude.Maybe TransitGatewayPolicyTable)
deleteTransitGatewayPolicyTableResponse_transitGatewayPolicyTable = Lens.lens (\DeleteTransitGatewayPolicyTableResponse' {transitGatewayPolicyTable} -> transitGatewayPolicyTable) (\s@DeleteTransitGatewayPolicyTableResponse' {} a -> s {transitGatewayPolicyTable = a} :: DeleteTransitGatewayPolicyTableResponse)

-- | The response's http status code.
deleteTransitGatewayPolicyTableResponse_httpStatus :: Lens.Lens' DeleteTransitGatewayPolicyTableResponse Prelude.Int
deleteTransitGatewayPolicyTableResponse_httpStatus = Lens.lens (\DeleteTransitGatewayPolicyTableResponse' {httpStatus} -> httpStatus) (\s@DeleteTransitGatewayPolicyTableResponse' {} a -> s {httpStatus = a} :: DeleteTransitGatewayPolicyTableResponse)

instance
  Prelude.NFData
    DeleteTransitGatewayPolicyTableResponse
  where
  rnf DeleteTransitGatewayPolicyTableResponse' {..} =
    Prelude.rnf transitGatewayPolicyTable
      `Prelude.seq` Prelude.rnf httpStatus
