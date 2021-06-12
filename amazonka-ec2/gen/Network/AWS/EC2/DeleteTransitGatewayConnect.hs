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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTransitGatewayConnect' smart constructor.
data DeleteTransitGatewayConnect = DeleteTransitGatewayConnect'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the Connect attachment.
    transitGatewayAttachmentId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteTransitGatewayConnect
newDeleteTransitGatewayConnect
  pTransitGatewayAttachmentId_ =
    DeleteTransitGatewayConnect'
      { dryRun = Core.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTransitGatewayConnect_dryRun :: Lens.Lens' DeleteTransitGatewayConnect (Core.Maybe Core.Bool)
deleteTransitGatewayConnect_dryRun = Lens.lens (\DeleteTransitGatewayConnect' {dryRun} -> dryRun) (\s@DeleteTransitGatewayConnect' {} a -> s {dryRun = a} :: DeleteTransitGatewayConnect)

-- | The ID of the Connect attachment.
deleteTransitGatewayConnect_transitGatewayAttachmentId :: Lens.Lens' DeleteTransitGatewayConnect Core.Text
deleteTransitGatewayConnect_transitGatewayAttachmentId = Lens.lens (\DeleteTransitGatewayConnect' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@DeleteTransitGatewayConnect' {} a -> s {transitGatewayAttachmentId = a} :: DeleteTransitGatewayConnect)

instance Core.AWSRequest DeleteTransitGatewayConnect where
  type
    AWSResponse DeleteTransitGatewayConnect =
      DeleteTransitGatewayConnectResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayConnectResponse'
            Core.<$> (x Core..@? "transitGatewayConnect")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteTransitGatewayConnect

instance Core.NFData DeleteTransitGatewayConnect

instance Core.ToHeaders DeleteTransitGatewayConnect where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteTransitGatewayConnect where
  toPath = Core.const "/"

instance Core.ToQuery DeleteTransitGatewayConnect where
  toQuery DeleteTransitGatewayConnect' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteTransitGatewayConnect" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "TransitGatewayAttachmentId"
          Core.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newDeleteTransitGatewayConnectResponse' smart constructor.
data DeleteTransitGatewayConnectResponse = DeleteTransitGatewayConnectResponse'
  { -- | Information about the deleted Connect attachment.
    transitGatewayConnect :: Core.Maybe TransitGatewayConnect,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteTransitGatewayConnectResponse
newDeleteTransitGatewayConnectResponse pHttpStatus_ =
  DeleteTransitGatewayConnectResponse'
    { transitGatewayConnect =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the deleted Connect attachment.
deleteTransitGatewayConnectResponse_transitGatewayConnect :: Lens.Lens' DeleteTransitGatewayConnectResponse (Core.Maybe TransitGatewayConnect)
deleteTransitGatewayConnectResponse_transitGatewayConnect = Lens.lens (\DeleteTransitGatewayConnectResponse' {transitGatewayConnect} -> transitGatewayConnect) (\s@DeleteTransitGatewayConnectResponse' {} a -> s {transitGatewayConnect = a} :: DeleteTransitGatewayConnectResponse)

-- | The response's http status code.
deleteTransitGatewayConnectResponse_httpStatus :: Lens.Lens' DeleteTransitGatewayConnectResponse Core.Int
deleteTransitGatewayConnectResponse_httpStatus = Lens.lens (\DeleteTransitGatewayConnectResponse' {httpStatus} -> httpStatus) (\s@DeleteTransitGatewayConnectResponse' {} a -> s {httpStatus = a} :: DeleteTransitGatewayConnectResponse)

instance
  Core.NFData
    DeleteTransitGatewayConnectResponse
