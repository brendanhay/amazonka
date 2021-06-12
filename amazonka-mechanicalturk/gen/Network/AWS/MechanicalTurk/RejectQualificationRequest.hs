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
-- Module      : Network.AWS.MechanicalTurk.RejectQualificationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @RejectQualificationRequest@ operation rejects a user\'s request for
-- a Qualification.
--
-- You can provide a text message explaining why the request was rejected.
-- The Worker who made the request can see this message.
module Network.AWS.MechanicalTurk.RejectQualificationRequest
  ( -- * Creating a Request
    RejectQualificationRequest (..),
    newRejectQualificationRequest,

    -- * Request Lenses
    rejectQualificationRequest_reason,
    rejectQualificationRequest_qualificationRequestId,

    -- * Destructuring the Response
    RejectQualificationRequestResponse (..),
    newRejectQualificationRequestResponse,

    -- * Response Lenses
    rejectQualificationRequestResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRejectQualificationRequest' smart constructor.
data RejectQualificationRequest = RejectQualificationRequest'
  { -- | A text message explaining why the request was rejected, to be shown to
    -- the Worker who made the request.
    reason :: Core.Maybe Core.Text,
    -- | The ID of the Qualification request, as returned by the
    -- @ListQualificationRequests@ operation.
    qualificationRequestId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RejectQualificationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'rejectQualificationRequest_reason' - A text message explaining why the request was rejected, to be shown to
-- the Worker who made the request.
--
-- 'qualificationRequestId', 'rejectQualificationRequest_qualificationRequestId' - The ID of the Qualification request, as returned by the
-- @ListQualificationRequests@ operation.
newRejectQualificationRequest ::
  -- | 'qualificationRequestId'
  Core.Text ->
  RejectQualificationRequest
newRejectQualificationRequest
  pQualificationRequestId_ =
    RejectQualificationRequest'
      { reason = Core.Nothing,
        qualificationRequestId =
          pQualificationRequestId_
      }

-- | A text message explaining why the request was rejected, to be shown to
-- the Worker who made the request.
rejectQualificationRequest_reason :: Lens.Lens' RejectQualificationRequest (Core.Maybe Core.Text)
rejectQualificationRequest_reason = Lens.lens (\RejectQualificationRequest' {reason} -> reason) (\s@RejectQualificationRequest' {} a -> s {reason = a} :: RejectQualificationRequest)

-- | The ID of the Qualification request, as returned by the
-- @ListQualificationRequests@ operation.
rejectQualificationRequest_qualificationRequestId :: Lens.Lens' RejectQualificationRequest Core.Text
rejectQualificationRequest_qualificationRequestId = Lens.lens (\RejectQualificationRequest' {qualificationRequestId} -> qualificationRequestId) (\s@RejectQualificationRequest' {} a -> s {qualificationRequestId = a} :: RejectQualificationRequest)

instance Core.AWSRequest RejectQualificationRequest where
  type
    AWSResponse RejectQualificationRequest =
      RejectQualificationRequestResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RejectQualificationRequestResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RejectQualificationRequest

instance Core.NFData RejectQualificationRequest

instance Core.ToHeaders RejectQualificationRequest where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.RejectQualificationRequest" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RejectQualificationRequest where
  toJSON RejectQualificationRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Reason" Core..=) Core.<$> reason,
            Core.Just
              ( "QualificationRequestId"
                  Core..= qualificationRequestId
              )
          ]
      )

instance Core.ToPath RejectQualificationRequest where
  toPath = Core.const "/"

instance Core.ToQuery RejectQualificationRequest where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRejectQualificationRequestResponse' smart constructor.
data RejectQualificationRequestResponse = RejectQualificationRequestResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RejectQualificationRequestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'rejectQualificationRequestResponse_httpStatus' - The response's http status code.
newRejectQualificationRequestResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RejectQualificationRequestResponse
newRejectQualificationRequestResponse pHttpStatus_ =
  RejectQualificationRequestResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
rejectQualificationRequestResponse_httpStatus :: Lens.Lens' RejectQualificationRequestResponse Core.Int
rejectQualificationRequestResponse_httpStatus = Lens.lens (\RejectQualificationRequestResponse' {httpStatus} -> httpStatus) (\s@RejectQualificationRequestResponse' {} a -> s {httpStatus = a} :: RejectQualificationRequestResponse)

instance
  Core.NFData
    RejectQualificationRequestResponse
