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
-- Module      : Network.AWS.EC2.AcceptTransitGatewayVpcAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a request to attach a VPC to a transit gateway.
--
-- The VPC attachment must be in the @pendingAcceptance@ state. Use
-- DescribeTransitGatewayVpcAttachments to view your pending VPC attachment
-- requests. Use RejectTransitGatewayVpcAttachment to reject a VPC
-- attachment request.
module Network.AWS.EC2.AcceptTransitGatewayVpcAttachment
  ( -- * Creating a Request
    AcceptTransitGatewayVpcAttachment (..),
    newAcceptTransitGatewayVpcAttachment,

    -- * Request Lenses
    acceptTransitGatewayVpcAttachment_dryRun,
    acceptTransitGatewayVpcAttachment_transitGatewayAttachmentId,

    -- * Destructuring the Response
    AcceptTransitGatewayVpcAttachmentResponse (..),
    newAcceptTransitGatewayVpcAttachmentResponse,

    -- * Response Lenses
    acceptTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment,
    acceptTransitGatewayVpcAttachmentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAcceptTransitGatewayVpcAttachment' smart constructor.
data AcceptTransitGatewayVpcAttachment = AcceptTransitGatewayVpcAttachment'
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
-- Create a value of 'AcceptTransitGatewayVpcAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'acceptTransitGatewayVpcAttachment_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayAttachmentId', 'acceptTransitGatewayVpcAttachment_transitGatewayAttachmentId' - The ID of the attachment.
newAcceptTransitGatewayVpcAttachment ::
  -- | 'transitGatewayAttachmentId'
  Core.Text ->
  AcceptTransitGatewayVpcAttachment
newAcceptTransitGatewayVpcAttachment
  pTransitGatewayAttachmentId_ =
    AcceptTransitGatewayVpcAttachment'
      { dryRun =
          Core.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
acceptTransitGatewayVpcAttachment_dryRun :: Lens.Lens' AcceptTransitGatewayVpcAttachment (Core.Maybe Core.Bool)
acceptTransitGatewayVpcAttachment_dryRun = Lens.lens (\AcceptTransitGatewayVpcAttachment' {dryRun} -> dryRun) (\s@AcceptTransitGatewayVpcAttachment' {} a -> s {dryRun = a} :: AcceptTransitGatewayVpcAttachment)

-- | The ID of the attachment.
acceptTransitGatewayVpcAttachment_transitGatewayAttachmentId :: Lens.Lens' AcceptTransitGatewayVpcAttachment Core.Text
acceptTransitGatewayVpcAttachment_transitGatewayAttachmentId = Lens.lens (\AcceptTransitGatewayVpcAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@AcceptTransitGatewayVpcAttachment' {} a -> s {transitGatewayAttachmentId = a} :: AcceptTransitGatewayVpcAttachment)

instance
  Core.AWSRequest
    AcceptTransitGatewayVpcAttachment
  where
  type
    AWSResponse AcceptTransitGatewayVpcAttachment =
      AcceptTransitGatewayVpcAttachmentResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AcceptTransitGatewayVpcAttachmentResponse'
            Core.<$> (x Core..@? "transitGatewayVpcAttachment")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    AcceptTransitGatewayVpcAttachment

instance
  Core.NFData
    AcceptTransitGatewayVpcAttachment

instance
  Core.ToHeaders
    AcceptTransitGatewayVpcAttachment
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    AcceptTransitGatewayVpcAttachment
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    AcceptTransitGatewayVpcAttachment
  where
  toQuery AcceptTransitGatewayVpcAttachment' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "AcceptTransitGatewayVpcAttachment" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "TransitGatewayAttachmentId"
          Core.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newAcceptTransitGatewayVpcAttachmentResponse' smart constructor.
data AcceptTransitGatewayVpcAttachmentResponse = AcceptTransitGatewayVpcAttachmentResponse'
  { -- | The VPC attachment.
    transitGatewayVpcAttachment :: Core.Maybe TransitGatewayVpcAttachment,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcceptTransitGatewayVpcAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayVpcAttachment', 'acceptTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment' - The VPC attachment.
--
-- 'httpStatus', 'acceptTransitGatewayVpcAttachmentResponse_httpStatus' - The response's http status code.
newAcceptTransitGatewayVpcAttachmentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AcceptTransitGatewayVpcAttachmentResponse
newAcceptTransitGatewayVpcAttachmentResponse
  pHttpStatus_ =
    AcceptTransitGatewayVpcAttachmentResponse'
      { transitGatewayVpcAttachment =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The VPC attachment.
acceptTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment :: Lens.Lens' AcceptTransitGatewayVpcAttachmentResponse (Core.Maybe TransitGatewayVpcAttachment)
acceptTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment = Lens.lens (\AcceptTransitGatewayVpcAttachmentResponse' {transitGatewayVpcAttachment} -> transitGatewayVpcAttachment) (\s@AcceptTransitGatewayVpcAttachmentResponse' {} a -> s {transitGatewayVpcAttachment = a} :: AcceptTransitGatewayVpcAttachmentResponse)

-- | The response's http status code.
acceptTransitGatewayVpcAttachmentResponse_httpStatus :: Lens.Lens' AcceptTransitGatewayVpcAttachmentResponse Core.Int
acceptTransitGatewayVpcAttachmentResponse_httpStatus = Lens.lens (\AcceptTransitGatewayVpcAttachmentResponse' {httpStatus} -> httpStatus) (\s@AcceptTransitGatewayVpcAttachmentResponse' {} a -> s {httpStatus = a} :: AcceptTransitGatewayVpcAttachmentResponse)

instance
  Core.NFData
    AcceptTransitGatewayVpcAttachmentResponse
