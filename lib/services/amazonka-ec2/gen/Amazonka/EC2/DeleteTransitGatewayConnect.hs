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
-- Module      : Amazonka.EC2.DeleteTransitGatewayConnect
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Connect attachment. You must first delete any
-- Connect peers for the attachment.
module Amazonka.EC2.DeleteTransitGatewayConnect
  ( -- * Creating a Request
    DeleteTransitGatewayConnect (..),
    newDeleteTransitGatewayConnect,

    -- * Request Lenses
    deleteTransitGatewayConnect_dryRun,
    deleteTransitGatewayConnect_transitGatewayAttachmentId,

    -- * Destructuring the Response
    DeleteTransitGatewayConnectResponse (..),
    newDeleteTransitGatewayConnectResponse,

    -- * Response Lenses
    deleteTransitGatewayConnectResponse_transitGatewayConnect,
    deleteTransitGatewayConnectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTransitGatewayConnect' smart constructor.
data DeleteTransitGatewayConnect = DeleteTransitGatewayConnect'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Connect attachment.
    transitGatewayAttachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayConnect' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteTransitGatewayConnect_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayAttachmentId', 'deleteTransitGatewayConnect_transitGatewayAttachmentId' - The ID of the Connect attachment.
newDeleteTransitGatewayConnect ::
  -- | 'transitGatewayAttachmentId'
  Prelude.Text ->
  DeleteTransitGatewayConnect
newDeleteTransitGatewayConnect
  pTransitGatewayAttachmentId_ =
    DeleteTransitGatewayConnect'
      { dryRun =
          Prelude.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTransitGatewayConnect_dryRun :: Lens.Lens' DeleteTransitGatewayConnect (Prelude.Maybe Prelude.Bool)
deleteTransitGatewayConnect_dryRun = Lens.lens (\DeleteTransitGatewayConnect' {dryRun} -> dryRun) (\s@DeleteTransitGatewayConnect' {} a -> s {dryRun = a} :: DeleteTransitGatewayConnect)

-- | The ID of the Connect attachment.
deleteTransitGatewayConnect_transitGatewayAttachmentId :: Lens.Lens' DeleteTransitGatewayConnect Prelude.Text
deleteTransitGatewayConnect_transitGatewayAttachmentId = Lens.lens (\DeleteTransitGatewayConnect' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@DeleteTransitGatewayConnect' {} a -> s {transitGatewayAttachmentId = a} :: DeleteTransitGatewayConnect)

instance Core.AWSRequest DeleteTransitGatewayConnect where
  type
    AWSResponse DeleteTransitGatewayConnect =
      DeleteTransitGatewayConnectResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayConnectResponse'
            Prelude.<$> (x Data..@? "transitGatewayConnect")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTransitGatewayConnect where
  hashWithSalt _salt DeleteTransitGatewayConnect' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` transitGatewayAttachmentId

instance Prelude.NFData DeleteTransitGatewayConnect where
  rnf DeleteTransitGatewayConnect' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentId

instance Data.ToHeaders DeleteTransitGatewayConnect where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteTransitGatewayConnect where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTransitGatewayConnect where
  toQuery DeleteTransitGatewayConnect' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteTransitGatewayConnect" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "TransitGatewayAttachmentId"
          Data.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newDeleteTransitGatewayConnectResponse' smart constructor.
data DeleteTransitGatewayConnectResponse = DeleteTransitGatewayConnectResponse'
  { -- | Information about the deleted Connect attachment.
    transitGatewayConnect :: Prelude.Maybe TransitGatewayConnect,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayConnectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayConnect', 'deleteTransitGatewayConnectResponse_transitGatewayConnect' - Information about the deleted Connect attachment.
--
-- 'httpStatus', 'deleteTransitGatewayConnectResponse_httpStatus' - The response's http status code.
newDeleteTransitGatewayConnectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTransitGatewayConnectResponse
newDeleteTransitGatewayConnectResponse pHttpStatus_ =
  DeleteTransitGatewayConnectResponse'
    { transitGatewayConnect =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the deleted Connect attachment.
deleteTransitGatewayConnectResponse_transitGatewayConnect :: Lens.Lens' DeleteTransitGatewayConnectResponse (Prelude.Maybe TransitGatewayConnect)
deleteTransitGatewayConnectResponse_transitGatewayConnect = Lens.lens (\DeleteTransitGatewayConnectResponse' {transitGatewayConnect} -> transitGatewayConnect) (\s@DeleteTransitGatewayConnectResponse' {} a -> s {transitGatewayConnect = a} :: DeleteTransitGatewayConnectResponse)

-- | The response's http status code.
deleteTransitGatewayConnectResponse_httpStatus :: Lens.Lens' DeleteTransitGatewayConnectResponse Prelude.Int
deleteTransitGatewayConnectResponse_httpStatus = Lens.lens (\DeleteTransitGatewayConnectResponse' {httpStatus} -> httpStatus) (\s@DeleteTransitGatewayConnectResponse' {} a -> s {httpStatus = a} :: DeleteTransitGatewayConnectResponse)

instance
  Prelude.NFData
    DeleteTransitGatewayConnectResponse
  where
  rnf DeleteTransitGatewayConnectResponse' {..} =
    Prelude.rnf transitGatewayConnect
      `Prelude.seq` Prelude.rnf httpStatus
