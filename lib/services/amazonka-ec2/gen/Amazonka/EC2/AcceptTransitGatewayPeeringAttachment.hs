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
-- Module      : Amazonka.EC2.AcceptTransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a transit gateway peering attachment request. The peering
-- attachment must be in the @pendingAcceptance@ state.
module Amazonka.EC2.AcceptTransitGatewayPeeringAttachment
  ( -- * Creating a Request
    AcceptTransitGatewayPeeringAttachment (..),
    newAcceptTransitGatewayPeeringAttachment,

    -- * Request Lenses
    acceptTransitGatewayPeeringAttachment_dryRun,
    acceptTransitGatewayPeeringAttachment_transitGatewayAttachmentId,

    -- * Destructuring the Response
    AcceptTransitGatewayPeeringAttachmentResponse (..),
    newAcceptTransitGatewayPeeringAttachmentResponse,

    -- * Response Lenses
    acceptTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment,
    acceptTransitGatewayPeeringAttachmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAcceptTransitGatewayPeeringAttachment' smart constructor.
data AcceptTransitGatewayPeeringAttachment = AcceptTransitGatewayPeeringAttachment'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the transit gateway attachment.
    transitGatewayAttachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptTransitGatewayPeeringAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'acceptTransitGatewayPeeringAttachment_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayAttachmentId', 'acceptTransitGatewayPeeringAttachment_transitGatewayAttachmentId' - The ID of the transit gateway attachment.
newAcceptTransitGatewayPeeringAttachment ::
  -- | 'transitGatewayAttachmentId'
  Prelude.Text ->
  AcceptTransitGatewayPeeringAttachment
newAcceptTransitGatewayPeeringAttachment
  pTransitGatewayAttachmentId_ =
    AcceptTransitGatewayPeeringAttachment'
      { dryRun =
          Prelude.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
acceptTransitGatewayPeeringAttachment_dryRun :: Lens.Lens' AcceptTransitGatewayPeeringAttachment (Prelude.Maybe Prelude.Bool)
acceptTransitGatewayPeeringAttachment_dryRun = Lens.lens (\AcceptTransitGatewayPeeringAttachment' {dryRun} -> dryRun) (\s@AcceptTransitGatewayPeeringAttachment' {} a -> s {dryRun = a} :: AcceptTransitGatewayPeeringAttachment)

-- | The ID of the transit gateway attachment.
acceptTransitGatewayPeeringAttachment_transitGatewayAttachmentId :: Lens.Lens' AcceptTransitGatewayPeeringAttachment Prelude.Text
acceptTransitGatewayPeeringAttachment_transitGatewayAttachmentId = Lens.lens (\AcceptTransitGatewayPeeringAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@AcceptTransitGatewayPeeringAttachment' {} a -> s {transitGatewayAttachmentId = a} :: AcceptTransitGatewayPeeringAttachment)

instance
  Core.AWSRequest
    AcceptTransitGatewayPeeringAttachment
  where
  type
    AWSResponse
      AcceptTransitGatewayPeeringAttachment =
      AcceptTransitGatewayPeeringAttachmentResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AcceptTransitGatewayPeeringAttachmentResponse'
            Prelude.<$> (x Core..@? "transitGatewayPeeringAttachment")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AcceptTransitGatewayPeeringAttachment
  where
  hashWithSalt
    _salt
    AcceptTransitGatewayPeeringAttachment' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` transitGatewayAttachmentId

instance
  Prelude.NFData
    AcceptTransitGatewayPeeringAttachment
  where
  rnf AcceptTransitGatewayPeeringAttachment' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentId

instance
  Core.ToHeaders
    AcceptTransitGatewayPeeringAttachment
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    AcceptTransitGatewayPeeringAttachment
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    AcceptTransitGatewayPeeringAttachment
  where
  toQuery AcceptTransitGatewayPeeringAttachment' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "AcceptTransitGatewayPeeringAttachment" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "TransitGatewayAttachmentId"
          Core.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newAcceptTransitGatewayPeeringAttachmentResponse' smart constructor.
data AcceptTransitGatewayPeeringAttachmentResponse = AcceptTransitGatewayPeeringAttachmentResponse'
  { -- | The transit gateway peering attachment.
    transitGatewayPeeringAttachment :: Prelude.Maybe TransitGatewayPeeringAttachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptTransitGatewayPeeringAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayPeeringAttachment', 'acceptTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment' - The transit gateway peering attachment.
--
-- 'httpStatus', 'acceptTransitGatewayPeeringAttachmentResponse_httpStatus' - The response's http status code.
newAcceptTransitGatewayPeeringAttachmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcceptTransitGatewayPeeringAttachmentResponse
newAcceptTransitGatewayPeeringAttachmentResponse
  pHttpStatus_ =
    AcceptTransitGatewayPeeringAttachmentResponse'
      { transitGatewayPeeringAttachment =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The transit gateway peering attachment.
acceptTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment :: Lens.Lens' AcceptTransitGatewayPeeringAttachmentResponse (Prelude.Maybe TransitGatewayPeeringAttachment)
acceptTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment = Lens.lens (\AcceptTransitGatewayPeeringAttachmentResponse' {transitGatewayPeeringAttachment} -> transitGatewayPeeringAttachment) (\s@AcceptTransitGatewayPeeringAttachmentResponse' {} a -> s {transitGatewayPeeringAttachment = a} :: AcceptTransitGatewayPeeringAttachmentResponse)

-- | The response's http status code.
acceptTransitGatewayPeeringAttachmentResponse_httpStatus :: Lens.Lens' AcceptTransitGatewayPeeringAttachmentResponse Prelude.Int
acceptTransitGatewayPeeringAttachmentResponse_httpStatus = Lens.lens (\AcceptTransitGatewayPeeringAttachmentResponse' {httpStatus} -> httpStatus) (\s@AcceptTransitGatewayPeeringAttachmentResponse' {} a -> s {httpStatus = a} :: AcceptTransitGatewayPeeringAttachmentResponse)

instance
  Prelude.NFData
    AcceptTransitGatewayPeeringAttachmentResponse
  where
  rnf
    AcceptTransitGatewayPeeringAttachmentResponse' {..} =
      Prelude.rnf transitGatewayPeeringAttachment
        `Prelude.seq` Prelude.rnf httpStatus
