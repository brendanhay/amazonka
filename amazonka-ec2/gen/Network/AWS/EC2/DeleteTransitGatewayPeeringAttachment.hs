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
-- Module      : Network.AWS.EC2.DeleteTransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a transit gateway peering attachment.
module Network.AWS.EC2.DeleteTransitGatewayPeeringAttachment
  ( -- * Creating a Request
    DeleteTransitGatewayPeeringAttachment (..),
    newDeleteTransitGatewayPeeringAttachment,

    -- * Request Lenses
    deleteTransitGatewayPeeringAttachment_dryRun,
    deleteTransitGatewayPeeringAttachment_transitGatewayAttachmentId,

    -- * Destructuring the Response
    DeleteTransitGatewayPeeringAttachmentResponse (..),
    newDeleteTransitGatewayPeeringAttachmentResponse,

    -- * Response Lenses
    deleteTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment,
    deleteTransitGatewayPeeringAttachmentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTransitGatewayPeeringAttachment' smart constructor.
data DeleteTransitGatewayPeeringAttachment = DeleteTransitGatewayPeeringAttachment'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the transit gateway peering attachment.
    transitGatewayAttachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayPeeringAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteTransitGatewayPeeringAttachment_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayAttachmentId', 'deleteTransitGatewayPeeringAttachment_transitGatewayAttachmentId' - The ID of the transit gateway peering attachment.
newDeleteTransitGatewayPeeringAttachment ::
  -- | 'transitGatewayAttachmentId'
  Prelude.Text ->
  DeleteTransitGatewayPeeringAttachment
newDeleteTransitGatewayPeeringAttachment
  pTransitGatewayAttachmentId_ =
    DeleteTransitGatewayPeeringAttachment'
      { dryRun =
          Prelude.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTransitGatewayPeeringAttachment_dryRun :: Lens.Lens' DeleteTransitGatewayPeeringAttachment (Prelude.Maybe Prelude.Bool)
deleteTransitGatewayPeeringAttachment_dryRun = Lens.lens (\DeleteTransitGatewayPeeringAttachment' {dryRun} -> dryRun) (\s@DeleteTransitGatewayPeeringAttachment' {} a -> s {dryRun = a} :: DeleteTransitGatewayPeeringAttachment)

-- | The ID of the transit gateway peering attachment.
deleteTransitGatewayPeeringAttachment_transitGatewayAttachmentId :: Lens.Lens' DeleteTransitGatewayPeeringAttachment Prelude.Text
deleteTransitGatewayPeeringAttachment_transitGatewayAttachmentId = Lens.lens (\DeleteTransitGatewayPeeringAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@DeleteTransitGatewayPeeringAttachment' {} a -> s {transitGatewayAttachmentId = a} :: DeleteTransitGatewayPeeringAttachment)

instance
  Core.AWSRequest
    DeleteTransitGatewayPeeringAttachment
  where
  type
    AWSResponse
      DeleteTransitGatewayPeeringAttachment =
      DeleteTransitGatewayPeeringAttachmentResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayPeeringAttachmentResponse'
            Prelude.<$> (x Core..@? "transitGatewayPeeringAttachment")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteTransitGatewayPeeringAttachment

instance
  Prelude.NFData
    DeleteTransitGatewayPeeringAttachment

instance
  Core.ToHeaders
    DeleteTransitGatewayPeeringAttachment
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DeleteTransitGatewayPeeringAttachment
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeleteTransitGatewayPeeringAttachment
  where
  toQuery DeleteTransitGatewayPeeringAttachment' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DeleteTransitGatewayPeeringAttachment" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "TransitGatewayAttachmentId"
          Core.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newDeleteTransitGatewayPeeringAttachmentResponse' smart constructor.
data DeleteTransitGatewayPeeringAttachmentResponse = DeleteTransitGatewayPeeringAttachmentResponse'
  { -- | The transit gateway peering attachment.
    transitGatewayPeeringAttachment :: Prelude.Maybe TransitGatewayPeeringAttachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayPeeringAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayPeeringAttachment', 'deleteTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment' - The transit gateway peering attachment.
--
-- 'httpStatus', 'deleteTransitGatewayPeeringAttachmentResponse_httpStatus' - The response's http status code.
newDeleteTransitGatewayPeeringAttachmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTransitGatewayPeeringAttachmentResponse
newDeleteTransitGatewayPeeringAttachmentResponse
  pHttpStatus_ =
    DeleteTransitGatewayPeeringAttachmentResponse'
      { transitGatewayPeeringAttachment =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The transit gateway peering attachment.
deleteTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment :: Lens.Lens' DeleteTransitGatewayPeeringAttachmentResponse (Prelude.Maybe TransitGatewayPeeringAttachment)
deleteTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment = Lens.lens (\DeleteTransitGatewayPeeringAttachmentResponse' {transitGatewayPeeringAttachment} -> transitGatewayPeeringAttachment) (\s@DeleteTransitGatewayPeeringAttachmentResponse' {} a -> s {transitGatewayPeeringAttachment = a} :: DeleteTransitGatewayPeeringAttachmentResponse)

-- | The response's http status code.
deleteTransitGatewayPeeringAttachmentResponse_httpStatus :: Lens.Lens' DeleteTransitGatewayPeeringAttachmentResponse Prelude.Int
deleteTransitGatewayPeeringAttachmentResponse_httpStatus = Lens.lens (\DeleteTransitGatewayPeeringAttachmentResponse' {httpStatus} -> httpStatus) (\s@DeleteTransitGatewayPeeringAttachmentResponse' {} a -> s {httpStatus = a} :: DeleteTransitGatewayPeeringAttachmentResponse)

instance
  Prelude.NFData
    DeleteTransitGatewayPeeringAttachmentResponse
