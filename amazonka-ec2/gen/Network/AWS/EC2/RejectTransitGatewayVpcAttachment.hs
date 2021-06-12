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
-- Module      : Network.AWS.EC2.RejectTransitGatewayVpcAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a request to attach a VPC to a transit gateway.
--
-- The VPC attachment must be in the @pendingAcceptance@ state. Use
-- DescribeTransitGatewayVpcAttachments to view your pending VPC attachment
-- requests. Use AcceptTransitGatewayVpcAttachment to accept a VPC
-- attachment request.
module Network.AWS.EC2.RejectTransitGatewayVpcAttachment
  ( -- * Creating a Request
    RejectTransitGatewayVpcAttachment (..),
    newRejectTransitGatewayVpcAttachment,

    -- * Request Lenses
    rejectTransitGatewayVpcAttachment_dryRun,
    rejectTransitGatewayVpcAttachment_transitGatewayAttachmentId,

    -- * Destructuring the Response
    RejectTransitGatewayVpcAttachmentResponse (..),
    newRejectTransitGatewayVpcAttachmentResponse,

    -- * Response Lenses
    rejectTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    rejectTransitGatewayVpcAttachmentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRejectTransitGatewayVpcAttachment' smart constructor.
data RejectTransitGatewayVpcAttachment = RejectTransitGatewayVpcAttachment'
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
-- Create a value of 'RejectTransitGatewayVpcAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'rejectTransitGatewayVpcAttachment_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayAttachmentId', 'rejectTransitGatewayVpcAttachment_transitGatewayAttachmentId' - The ID of the attachment.
newRejectTransitGatewayVpcAttachment ::
  -- | 'transitGatewayAttachmentId'
  Core.Text ->
  RejectTransitGatewayVpcAttachment
newRejectTransitGatewayVpcAttachment
  pTransitGatewayAttachmentId_ =
    RejectTransitGatewayVpcAttachment'
      { dryRun =
          Core.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rejectTransitGatewayVpcAttachment_dryRun :: Lens.Lens' RejectTransitGatewayVpcAttachment (Core.Maybe Core.Bool)
rejectTransitGatewayVpcAttachment_dryRun = Lens.lens (\RejectTransitGatewayVpcAttachment' {dryRun} -> dryRun) (\s@RejectTransitGatewayVpcAttachment' {} a -> s {dryRun = a} :: RejectTransitGatewayVpcAttachment)

-- | The ID of the attachment.
rejectTransitGatewayVpcAttachment_transitGatewayAttachmentId :: Lens.Lens' RejectTransitGatewayVpcAttachment Core.Text
rejectTransitGatewayVpcAttachment_transitGatewayAttachmentId = Lens.lens (\RejectTransitGatewayVpcAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@RejectTransitGatewayVpcAttachment' {} a -> s {transitGatewayAttachmentId = a} :: RejectTransitGatewayVpcAttachment)

instance
  Core.AWSRequest
    RejectTransitGatewayVpcAttachment
  where
  type
    AWSResponse RejectTransitGatewayVpcAttachment =
      RejectTransitGatewayVpcAttachmentResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          RejectTransitGatewayVpcAttachmentResponse'
            Core.<$> (x Core..@? "transitGatewayVpcAttachment")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    RejectTransitGatewayVpcAttachment

instance
  Core.NFData
    RejectTransitGatewayVpcAttachment

instance
  Core.ToHeaders
    RejectTransitGatewayVpcAttachment
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    RejectTransitGatewayVpcAttachment
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    RejectTransitGatewayVpcAttachment
  where
  toQuery RejectTransitGatewayVpcAttachment' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "RejectTransitGatewayVpcAttachment" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "TransitGatewayAttachmentId"
          Core.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newRejectTransitGatewayVpcAttachmentResponse' smart constructor.
data RejectTransitGatewayVpcAttachmentResponse = RejectTransitGatewayVpcAttachmentResponse'
  { -- | Information about the attachment.
    transitGatewayVpcAttachment :: Core.Maybe TransitGatewayVpcAttachment,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RejectTransitGatewayVpcAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayVpcAttachment', 'rejectTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment' - Information about the attachment.
--
-- 'httpStatus', 'rejectTransitGatewayVpcAttachmentResponse_httpStatus' - The response's http status code.
newRejectTransitGatewayVpcAttachmentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RejectTransitGatewayVpcAttachmentResponse
newRejectTransitGatewayVpcAttachmentResponse
  pHttpStatus_ =
    RejectTransitGatewayVpcAttachmentResponse'
      { transitGatewayVpcAttachment =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the attachment.
rejectTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment :: Lens.Lens' RejectTransitGatewayVpcAttachmentResponse (Core.Maybe TransitGatewayVpcAttachment)
rejectTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment = Lens.lens (\RejectTransitGatewayVpcAttachmentResponse' {transitGatewayVpcAttachment} -> transitGatewayVpcAttachment) (\s@RejectTransitGatewayVpcAttachmentResponse' {} a -> s {transitGatewayVpcAttachment = a} :: RejectTransitGatewayVpcAttachmentResponse)

-- | The response's http status code.
rejectTransitGatewayVpcAttachmentResponse_httpStatus :: Lens.Lens' RejectTransitGatewayVpcAttachmentResponse Core.Int
rejectTransitGatewayVpcAttachmentResponse_httpStatus = Lens.lens (\RejectTransitGatewayVpcAttachmentResponse' {httpStatus} -> httpStatus) (\s@RejectTransitGatewayVpcAttachmentResponse' {} a -> s {httpStatus = a} :: RejectTransitGatewayVpcAttachmentResponse)

instance
  Core.NFData
    RejectTransitGatewayVpcAttachmentResponse
