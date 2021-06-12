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
-- Module      : Network.AWS.EC2.DeleteTransitGatewayVpcAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified VPC attachment.
module Network.AWS.EC2.DeleteTransitGatewayVpcAttachment
  ( -- * Creating a Request
    DeleteTransitGatewayVpcAttachment (..),
    newDeleteTransitGatewayVpcAttachment,

    -- * Request Lenses
    deleteTransitGatewayVpcAttachment_dryRun,
    deleteTransitGatewayVpcAttachment_transitGatewayAttachmentId,

    -- * Destructuring the Response
    DeleteTransitGatewayVpcAttachmentResponse (..),
    newDeleteTransitGatewayVpcAttachmentResponse,

    -- * Response Lenses
    deleteTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    deleteTransitGatewayVpcAttachmentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTransitGatewayVpcAttachment' smart constructor.
data DeleteTransitGatewayVpcAttachment = DeleteTransitGatewayVpcAttachment'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayVpcAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteTransitGatewayVpcAttachment_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayAttachmentId', 'deleteTransitGatewayVpcAttachment_transitGatewayAttachmentId' - The ID of the attachment.
newDeleteTransitGatewayVpcAttachment ::
  -- | 'transitGatewayAttachmentId'
  Core.Text ->
  DeleteTransitGatewayVpcAttachment
newDeleteTransitGatewayVpcAttachment
  pTransitGatewayAttachmentId_ =
    DeleteTransitGatewayVpcAttachment'
      { dryRun =
          Core.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTransitGatewayVpcAttachment_dryRun :: Lens.Lens' DeleteTransitGatewayVpcAttachment (Core.Maybe Core.Bool)
deleteTransitGatewayVpcAttachment_dryRun = Lens.lens (\DeleteTransitGatewayVpcAttachment' {dryRun} -> dryRun) (\s@DeleteTransitGatewayVpcAttachment' {} a -> s {dryRun = a} :: DeleteTransitGatewayVpcAttachment)

-- | The ID of the attachment.
deleteTransitGatewayVpcAttachment_transitGatewayAttachmentId :: Lens.Lens' DeleteTransitGatewayVpcAttachment Core.Text
deleteTransitGatewayVpcAttachment_transitGatewayAttachmentId = Lens.lens (\DeleteTransitGatewayVpcAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@DeleteTransitGatewayVpcAttachment' {} a -> s {transitGatewayAttachmentId = a} :: DeleteTransitGatewayVpcAttachment)

instance
  Core.AWSRequest
    DeleteTransitGatewayVpcAttachment
  where
  type
    AWSResponse DeleteTransitGatewayVpcAttachment =
      DeleteTransitGatewayVpcAttachmentResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayVpcAttachmentResponse'
            Core.<$> (x Core..@? "transitGatewayVpcAttachment")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DeleteTransitGatewayVpcAttachment

instance
  Core.NFData
    DeleteTransitGatewayVpcAttachment

instance
  Core.ToHeaders
    DeleteTransitGatewayVpcAttachment
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DeleteTransitGatewayVpcAttachment
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DeleteTransitGatewayVpcAttachment
  where
  toQuery DeleteTransitGatewayVpcAttachment' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DeleteTransitGatewayVpcAttachment" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "TransitGatewayAttachmentId"
          Core.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newDeleteTransitGatewayVpcAttachmentResponse' smart constructor.
data DeleteTransitGatewayVpcAttachmentResponse = DeleteTransitGatewayVpcAttachmentResponse'
  { -- | Information about the deleted VPC attachment.
    transitGatewayVpcAttachment :: Core.Maybe TransitGatewayVpcAttachment,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayVpcAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayVpcAttachment', 'deleteTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment' - Information about the deleted VPC attachment.
--
-- 'httpStatus', 'deleteTransitGatewayVpcAttachmentResponse_httpStatus' - The response's http status code.
newDeleteTransitGatewayVpcAttachmentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteTransitGatewayVpcAttachmentResponse
newDeleteTransitGatewayVpcAttachmentResponse
  pHttpStatus_ =
    DeleteTransitGatewayVpcAttachmentResponse'
      { transitGatewayVpcAttachment =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the deleted VPC attachment.
deleteTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment :: Lens.Lens' DeleteTransitGatewayVpcAttachmentResponse (Core.Maybe TransitGatewayVpcAttachment)
deleteTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment = Lens.lens (\DeleteTransitGatewayVpcAttachmentResponse' {transitGatewayVpcAttachment} -> transitGatewayVpcAttachment) (\s@DeleteTransitGatewayVpcAttachmentResponse' {} a -> s {transitGatewayVpcAttachment = a} :: DeleteTransitGatewayVpcAttachmentResponse)

-- | The response's http status code.
deleteTransitGatewayVpcAttachmentResponse_httpStatus :: Lens.Lens' DeleteTransitGatewayVpcAttachmentResponse Core.Int
deleteTransitGatewayVpcAttachmentResponse_httpStatus = Lens.lens (\DeleteTransitGatewayVpcAttachmentResponse' {httpStatus} -> httpStatus) (\s@DeleteTransitGatewayVpcAttachmentResponse' {} a -> s {httpStatus = a} :: DeleteTransitGatewayVpcAttachmentResponse)

instance
  Core.NFData
    DeleteTransitGatewayVpcAttachmentResponse
