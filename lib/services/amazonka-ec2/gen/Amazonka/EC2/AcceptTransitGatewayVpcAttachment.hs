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
-- Module      : Amazonka.EC2.AcceptTransitGatewayVpcAttachment
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.EC2.AcceptTransitGatewayVpcAttachment
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAcceptTransitGatewayVpcAttachment' smart constructor.
data AcceptTransitGatewayVpcAttachment = AcceptTransitGatewayVpcAttachment'
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
  Prelude.Text ->
  AcceptTransitGatewayVpcAttachment
newAcceptTransitGatewayVpcAttachment
  pTransitGatewayAttachmentId_ =
    AcceptTransitGatewayVpcAttachment'
      { dryRun =
          Prelude.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
acceptTransitGatewayVpcAttachment_dryRun :: Lens.Lens' AcceptTransitGatewayVpcAttachment (Prelude.Maybe Prelude.Bool)
acceptTransitGatewayVpcAttachment_dryRun = Lens.lens (\AcceptTransitGatewayVpcAttachment' {dryRun} -> dryRun) (\s@AcceptTransitGatewayVpcAttachment' {} a -> s {dryRun = a} :: AcceptTransitGatewayVpcAttachment)

-- | The ID of the attachment.
acceptTransitGatewayVpcAttachment_transitGatewayAttachmentId :: Lens.Lens' AcceptTransitGatewayVpcAttachment Prelude.Text
acceptTransitGatewayVpcAttachment_transitGatewayAttachmentId = Lens.lens (\AcceptTransitGatewayVpcAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@AcceptTransitGatewayVpcAttachment' {} a -> s {transitGatewayAttachmentId = a} :: AcceptTransitGatewayVpcAttachment)

instance
  Core.AWSRequest
    AcceptTransitGatewayVpcAttachment
  where
  type
    AWSResponse AcceptTransitGatewayVpcAttachment =
      AcceptTransitGatewayVpcAttachmentResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AcceptTransitGatewayVpcAttachmentResponse'
            Prelude.<$> (x Core..@? "transitGatewayVpcAttachment")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AcceptTransitGatewayVpcAttachment
  where
  hashWithSalt
    _salt
    AcceptTransitGatewayVpcAttachment' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` transitGatewayAttachmentId

instance
  Prelude.NFData
    AcceptTransitGatewayVpcAttachment
  where
  rnf AcceptTransitGatewayVpcAttachment' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentId

instance
  Core.ToHeaders
    AcceptTransitGatewayVpcAttachment
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    AcceptTransitGatewayVpcAttachment
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    AcceptTransitGatewayVpcAttachment
  where
  toQuery AcceptTransitGatewayVpcAttachment' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "AcceptTransitGatewayVpcAttachment" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "TransitGatewayAttachmentId"
          Core.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newAcceptTransitGatewayVpcAttachmentResponse' smart constructor.
data AcceptTransitGatewayVpcAttachmentResponse = AcceptTransitGatewayVpcAttachmentResponse'
  { -- | The VPC attachment.
    transitGatewayVpcAttachment :: Prelude.Maybe TransitGatewayVpcAttachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AcceptTransitGatewayVpcAttachmentResponse
newAcceptTransitGatewayVpcAttachmentResponse
  pHttpStatus_ =
    AcceptTransitGatewayVpcAttachmentResponse'
      { transitGatewayVpcAttachment =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The VPC attachment.
acceptTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment :: Lens.Lens' AcceptTransitGatewayVpcAttachmentResponse (Prelude.Maybe TransitGatewayVpcAttachment)
acceptTransitGatewayVpcAttachmentResponse_transitGatewayVpcAttachment = Lens.lens (\AcceptTransitGatewayVpcAttachmentResponse' {transitGatewayVpcAttachment} -> transitGatewayVpcAttachment) (\s@AcceptTransitGatewayVpcAttachmentResponse' {} a -> s {transitGatewayVpcAttachment = a} :: AcceptTransitGatewayVpcAttachmentResponse)

-- | The response's http status code.
acceptTransitGatewayVpcAttachmentResponse_httpStatus :: Lens.Lens' AcceptTransitGatewayVpcAttachmentResponse Prelude.Int
acceptTransitGatewayVpcAttachmentResponse_httpStatus = Lens.lens (\AcceptTransitGatewayVpcAttachmentResponse' {httpStatus} -> httpStatus) (\s@AcceptTransitGatewayVpcAttachmentResponse' {} a -> s {httpStatus = a} :: AcceptTransitGatewayVpcAttachmentResponse)

instance
  Prelude.NFData
    AcceptTransitGatewayVpcAttachmentResponse
  where
  rnf AcceptTransitGatewayVpcAttachmentResponse' {..} =
    Prelude.rnf transitGatewayVpcAttachment
      `Prelude.seq` Prelude.rnf httpStatus
