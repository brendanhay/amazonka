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
-- Module      : Network.AWS.EC2.DeleteTransitGatewayConnect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Connect attachment. You must first delete any
-- Connect peers for the attachment.
module Network.AWS.EC2.DeleteTransitGatewayConnect
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.AWSRequest
    DeleteTransitGatewayConnect
  where
  type
    Rs DeleteTransitGatewayConnect =
      DeleteTransitGatewayConnectResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayConnectResponse'
            Prelude.<$> (x Prelude..@? "transitGatewayConnect")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTransitGatewayConnect

instance Prelude.NFData DeleteTransitGatewayConnect

instance
  Prelude.ToHeaders
    DeleteTransitGatewayConnect
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteTransitGatewayConnect where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteTransitGatewayConnect where
  toQuery DeleteTransitGatewayConnect' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DeleteTransitGatewayConnect" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "TransitGatewayAttachmentId"
          Prelude.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newDeleteTransitGatewayConnectResponse' smart constructor.
data DeleteTransitGatewayConnectResponse = DeleteTransitGatewayConnectResponse'
  { -- | Information about the deleted Connect attachment.
    transitGatewayConnect :: Prelude.Maybe TransitGatewayConnect,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
