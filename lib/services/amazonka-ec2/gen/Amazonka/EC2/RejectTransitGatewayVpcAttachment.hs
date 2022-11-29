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
-- Module      : Amazonka.EC2.RejectTransitGatewayVpcAttachment
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.EC2.RejectTransitGatewayVpcAttachment
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRejectTransitGatewayVpcAttachment' smart constructor.
data RejectTransitGatewayVpcAttachment = RejectTransitGatewayVpcAttachment'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  RejectTransitGatewayVpcAttachment
newRejectTransitGatewayVpcAttachment
  pTransitGatewayAttachmentId_ =
    RejectTransitGatewayVpcAttachment'
      { dryRun =
          Prelude.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rejectTransitGatewayVpcAttachment_dryRun :: Lens.Lens' RejectTransitGatewayVpcAttachment (Prelude.Maybe Prelude.Bool)
rejectTransitGatewayVpcAttachment_dryRun = Lens.lens (\RejectTransitGatewayVpcAttachment' {dryRun} -> dryRun) (\s@RejectTransitGatewayVpcAttachment' {} a -> s {dryRun = a} :: RejectTransitGatewayVpcAttachment)

-- | The ID of the attachment.
rejectTransitGatewayVpcAttachment_transitGatewayAttachmentId :: Lens.Lens' RejectTransitGatewayVpcAttachment Prelude.Text
rejectTransitGatewayVpcAttachment_transitGatewayAttachmentId = Lens.lens (\RejectTransitGatewayVpcAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@RejectTransitGatewayVpcAttachment' {} a -> s {transitGatewayAttachmentId = a} :: RejectTransitGatewayVpcAttachment)

instance
  Core.AWSRequest
    RejectTransitGatewayVpcAttachment
  where
  type
    AWSResponse RejectTransitGatewayVpcAttachment =
      RejectTransitGatewayVpcAttachmentResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          RejectTransitGatewayVpcAttachmentResponse'
            Prelude.<$> (x Core..@? "transitGatewayVpcAttachment")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RejectTransitGatewayVpcAttachment
  where
  hashWithSalt
    _salt
    RejectTransitGatewayVpcAttachment' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` transitGatewayAttachmentId

instance
  Prelude.NFData
    RejectTransitGatewayVpcAttachment
  where
  rnf RejectTransitGatewayVpcAttachment' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentId

instance
  Core.ToHeaders
    RejectTransitGatewayVpcAttachment
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    RejectTransitGatewayVpcAttachment
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    RejectTransitGatewayVpcAttachment
  where
  toQuery RejectTransitGatewayVpcAttachment' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "RejectTransitGatewayVpcAttachment" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "TransitGatewayAttachmentId"
          Core.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newRejectTransitGatewayVpcAttachmentResponse' smart constructor.
data RejectTransitGatewayVpcAttachmentResponse = RejectTransitGatewayVpcAttachmentResponse'
  { -- | Information about the attachment.
    transitGatewayVpcAttachment :: Prelude.Maybe TransitGatewayVpcAttachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RejectTransitGatewayVpcAttachmentResponse
newRejectTransitGatewayVpcAttachmentResponse
  pHttpStatus_ =
    RejectTransitGatewayVpcAttachmentResponse'
      { transitGatewayVpcAttachment =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the attachment.
rejectTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment :: Lens.Lens' RejectTransitGatewayVpcAttachmentResponse (Prelude.Maybe TransitGatewayVpcAttachment)
rejectTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment = Lens.lens (\RejectTransitGatewayVpcAttachmentResponse' {transitGatewayVpcAttachment} -> transitGatewayVpcAttachment) (\s@RejectTransitGatewayVpcAttachmentResponse' {} a -> s {transitGatewayVpcAttachment = a} :: RejectTransitGatewayVpcAttachmentResponse)

-- | The response's http status code.
rejectTransitGatewayVpcAttachmentResponse_httpStatus :: Lens.Lens' RejectTransitGatewayVpcAttachmentResponse Prelude.Int
rejectTransitGatewayVpcAttachmentResponse_httpStatus = Lens.lens (\RejectTransitGatewayVpcAttachmentResponse' {httpStatus} -> httpStatus) (\s@RejectTransitGatewayVpcAttachmentResponse' {} a -> s {httpStatus = a} :: RejectTransitGatewayVpcAttachmentResponse)

instance
  Prelude.NFData
    RejectTransitGatewayVpcAttachmentResponse
  where
  rnf RejectTransitGatewayVpcAttachmentResponse' {..} =
    Prelude.rnf transitGatewayVpcAttachment
      `Prelude.seq` Prelude.rnf httpStatus
