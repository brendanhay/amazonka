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
-- Module      : Network.AWS.EC2.RejectTransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a transit gateway peering attachment request.
module Network.AWS.EC2.RejectTransitGatewayPeeringAttachment
  ( -- * Creating a Request
    RejectTransitGatewayPeeringAttachment (..),
    newRejectTransitGatewayPeeringAttachment,

    -- * Request Lenses
    rejectTransitGatewayPeeringAttachment_dryRun,
    rejectTransitGatewayPeeringAttachment_transitGatewayAttachmentId,

    -- * Destructuring the Response
    RejectTransitGatewayPeeringAttachmentResponse (..),
    newRejectTransitGatewayPeeringAttachmentResponse,

    -- * Response Lenses
    rejectTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment,
    rejectTransitGatewayPeeringAttachmentResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRejectTransitGatewayPeeringAttachment' smart constructor.
data RejectTransitGatewayPeeringAttachment = RejectTransitGatewayPeeringAttachment'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the transit gateway peering attachment.
    transitGatewayAttachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RejectTransitGatewayPeeringAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'rejectTransitGatewayPeeringAttachment_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayAttachmentId', 'rejectTransitGatewayPeeringAttachment_transitGatewayAttachmentId' - The ID of the transit gateway peering attachment.
newRejectTransitGatewayPeeringAttachment ::
  -- | 'transitGatewayAttachmentId'
  Prelude.Text ->
  RejectTransitGatewayPeeringAttachment
newRejectTransitGatewayPeeringAttachment
  pTransitGatewayAttachmentId_ =
    RejectTransitGatewayPeeringAttachment'
      { dryRun =
          Prelude.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rejectTransitGatewayPeeringAttachment_dryRun :: Lens.Lens' RejectTransitGatewayPeeringAttachment (Prelude.Maybe Prelude.Bool)
rejectTransitGatewayPeeringAttachment_dryRun = Lens.lens (\RejectTransitGatewayPeeringAttachment' {dryRun} -> dryRun) (\s@RejectTransitGatewayPeeringAttachment' {} a -> s {dryRun = a} :: RejectTransitGatewayPeeringAttachment)

-- | The ID of the transit gateway peering attachment.
rejectTransitGatewayPeeringAttachment_transitGatewayAttachmentId :: Lens.Lens' RejectTransitGatewayPeeringAttachment Prelude.Text
rejectTransitGatewayPeeringAttachment_transitGatewayAttachmentId = Lens.lens (\RejectTransitGatewayPeeringAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@RejectTransitGatewayPeeringAttachment' {} a -> s {transitGatewayAttachmentId = a} :: RejectTransitGatewayPeeringAttachment)

instance
  Prelude.AWSRequest
    RejectTransitGatewayPeeringAttachment
  where
  type
    Rs RejectTransitGatewayPeeringAttachment =
      RejectTransitGatewayPeeringAttachmentResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          RejectTransitGatewayPeeringAttachmentResponse'
            Prelude.<$> (x Prelude..@? "transitGatewayPeeringAttachment")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RejectTransitGatewayPeeringAttachment

instance
  Prelude.NFData
    RejectTransitGatewayPeeringAttachment

instance
  Prelude.ToHeaders
    RejectTransitGatewayPeeringAttachment
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    RejectTransitGatewayPeeringAttachment
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    RejectTransitGatewayPeeringAttachment
  where
  toQuery RejectTransitGatewayPeeringAttachment' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "RejectTransitGatewayPeeringAttachment" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "TransitGatewayAttachmentId"
          Prelude.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newRejectTransitGatewayPeeringAttachmentResponse' smart constructor.
data RejectTransitGatewayPeeringAttachmentResponse = RejectTransitGatewayPeeringAttachmentResponse'
  { -- | The transit gateway peering attachment.
    transitGatewayPeeringAttachment :: Prelude.Maybe TransitGatewayPeeringAttachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RejectTransitGatewayPeeringAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayPeeringAttachment', 'rejectTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment' - The transit gateway peering attachment.
--
-- 'httpStatus', 'rejectTransitGatewayPeeringAttachmentResponse_httpStatus' - The response's http status code.
newRejectTransitGatewayPeeringAttachmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RejectTransitGatewayPeeringAttachmentResponse
newRejectTransitGatewayPeeringAttachmentResponse
  pHttpStatus_ =
    RejectTransitGatewayPeeringAttachmentResponse'
      { transitGatewayPeeringAttachment =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The transit gateway peering attachment.
rejectTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment :: Lens.Lens' RejectTransitGatewayPeeringAttachmentResponse (Prelude.Maybe TransitGatewayPeeringAttachment)
rejectTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment = Lens.lens (\RejectTransitGatewayPeeringAttachmentResponse' {transitGatewayPeeringAttachment} -> transitGatewayPeeringAttachment) (\s@RejectTransitGatewayPeeringAttachmentResponse' {} a -> s {transitGatewayPeeringAttachment = a} :: RejectTransitGatewayPeeringAttachmentResponse)

-- | The response's http status code.
rejectTransitGatewayPeeringAttachmentResponse_httpStatus :: Lens.Lens' RejectTransitGatewayPeeringAttachmentResponse Prelude.Int
rejectTransitGatewayPeeringAttachmentResponse_httpStatus = Lens.lens (\RejectTransitGatewayPeeringAttachmentResponse' {httpStatus} -> httpStatus) (\s@RejectTransitGatewayPeeringAttachmentResponse' {} a -> s {httpStatus = a} :: RejectTransitGatewayPeeringAttachmentResponse)

instance
  Prelude.NFData
    RejectTransitGatewayPeeringAttachmentResponse
