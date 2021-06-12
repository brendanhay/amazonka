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
-- Module      : Network.AWS.EC2.AcceptTransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a transit gateway peering attachment request. The peering
-- attachment must be in the @pendingAcceptance@ state.
module Network.AWS.EC2.AcceptTransitGatewayPeeringAttachment
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAcceptTransitGatewayPeeringAttachment' smart constructor.
data AcceptTransitGatewayPeeringAttachment = AcceptTransitGatewayPeeringAttachment'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the transit gateway attachment.
    transitGatewayAttachmentId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  AcceptTransitGatewayPeeringAttachment
newAcceptTransitGatewayPeeringAttachment
  pTransitGatewayAttachmentId_ =
    AcceptTransitGatewayPeeringAttachment'
      { dryRun =
          Core.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
acceptTransitGatewayPeeringAttachment_dryRun :: Lens.Lens' AcceptTransitGatewayPeeringAttachment (Core.Maybe Core.Bool)
acceptTransitGatewayPeeringAttachment_dryRun = Lens.lens (\AcceptTransitGatewayPeeringAttachment' {dryRun} -> dryRun) (\s@AcceptTransitGatewayPeeringAttachment' {} a -> s {dryRun = a} :: AcceptTransitGatewayPeeringAttachment)

-- | The ID of the transit gateway attachment.
acceptTransitGatewayPeeringAttachment_transitGatewayAttachmentId :: Lens.Lens' AcceptTransitGatewayPeeringAttachment Core.Text
acceptTransitGatewayPeeringAttachment_transitGatewayAttachmentId = Lens.lens (\AcceptTransitGatewayPeeringAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@AcceptTransitGatewayPeeringAttachment' {} a -> s {transitGatewayAttachmentId = a} :: AcceptTransitGatewayPeeringAttachment)

instance
  Core.AWSRequest
    AcceptTransitGatewayPeeringAttachment
  where
  type
    AWSResponse
      AcceptTransitGatewayPeeringAttachment =
      AcceptTransitGatewayPeeringAttachmentResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AcceptTransitGatewayPeeringAttachmentResponse'
            Core.<$> (x Core..@? "transitGatewayPeeringAttachment")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    AcceptTransitGatewayPeeringAttachment

instance
  Core.NFData
    AcceptTransitGatewayPeeringAttachment

instance
  Core.ToHeaders
    AcceptTransitGatewayPeeringAttachment
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    AcceptTransitGatewayPeeringAttachment
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    AcceptTransitGatewayPeeringAttachment
  where
  toQuery AcceptTransitGatewayPeeringAttachment' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "AcceptTransitGatewayPeeringAttachment" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "TransitGatewayAttachmentId"
          Core.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newAcceptTransitGatewayPeeringAttachmentResponse' smart constructor.
data AcceptTransitGatewayPeeringAttachmentResponse = AcceptTransitGatewayPeeringAttachmentResponse'
  { -- | The transit gateway peering attachment.
    transitGatewayPeeringAttachment :: Core.Maybe TransitGatewayPeeringAttachment,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  AcceptTransitGatewayPeeringAttachmentResponse
newAcceptTransitGatewayPeeringAttachmentResponse
  pHttpStatus_ =
    AcceptTransitGatewayPeeringAttachmentResponse'
      { transitGatewayPeeringAttachment =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The transit gateway peering attachment.
acceptTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment :: Lens.Lens' AcceptTransitGatewayPeeringAttachmentResponse (Core.Maybe TransitGatewayPeeringAttachment)
acceptTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment = Lens.lens (\AcceptTransitGatewayPeeringAttachmentResponse' {transitGatewayPeeringAttachment} -> transitGatewayPeeringAttachment) (\s@AcceptTransitGatewayPeeringAttachmentResponse' {} a -> s {transitGatewayPeeringAttachment = a} :: AcceptTransitGatewayPeeringAttachmentResponse)

-- | The response's http status code.
acceptTransitGatewayPeeringAttachmentResponse_httpStatus :: Lens.Lens' AcceptTransitGatewayPeeringAttachmentResponse Core.Int
acceptTransitGatewayPeeringAttachmentResponse_httpStatus = Lens.lens (\AcceptTransitGatewayPeeringAttachmentResponse' {httpStatus} -> httpStatus) (\s@AcceptTransitGatewayPeeringAttachmentResponse' {} a -> s {httpStatus = a} :: AcceptTransitGatewayPeeringAttachmentResponse)

instance
  Core.NFData
    AcceptTransitGatewayPeeringAttachmentResponse
