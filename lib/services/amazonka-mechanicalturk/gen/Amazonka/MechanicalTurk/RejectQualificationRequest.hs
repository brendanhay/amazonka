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
-- Module      : Amazonka.MechanicalTurk.RejectQualificationRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.MechanicalTurk.RejectQualificationRequest
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRejectQualificationRequest' smart constructor.
data RejectQualificationRequest = RejectQualificationRequest'
  { -- | A text message explaining why the request was rejected, to be shown to
    -- the Worker who made the request.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Qualification request, as returned by the
    -- @ListQualificationRequests@ operation.
    qualificationRequestId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  RejectQualificationRequest
newRejectQualificationRequest
  pQualificationRequestId_ =
    RejectQualificationRequest'
      { reason =
          Prelude.Nothing,
        qualificationRequestId =
          pQualificationRequestId_
      }

-- | A text message explaining why the request was rejected, to be shown to
-- the Worker who made the request.
rejectQualificationRequest_reason :: Lens.Lens' RejectQualificationRequest (Prelude.Maybe Prelude.Text)
rejectQualificationRequest_reason = Lens.lens (\RejectQualificationRequest' {reason} -> reason) (\s@RejectQualificationRequest' {} a -> s {reason = a} :: RejectQualificationRequest)

-- | The ID of the Qualification request, as returned by the
-- @ListQualificationRequests@ operation.
rejectQualificationRequest_qualificationRequestId :: Lens.Lens' RejectQualificationRequest Prelude.Text
rejectQualificationRequest_qualificationRequestId = Lens.lens (\RejectQualificationRequest' {qualificationRequestId} -> qualificationRequestId) (\s@RejectQualificationRequest' {} a -> s {qualificationRequestId = a} :: RejectQualificationRequest)

instance Core.AWSRequest RejectQualificationRequest where
  type
    AWSResponse RejectQualificationRequest =
      RejectQualificationRequestResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RejectQualificationRequestResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RejectQualificationRequest where
  hashWithSalt _salt RejectQualificationRequest' {..} =
    _salt `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` qualificationRequestId

instance Prelude.NFData RejectQualificationRequest where
  rnf RejectQualificationRequest' {..} =
    Prelude.rnf reason
      `Prelude.seq` Prelude.rnf qualificationRequestId

instance Data.ToHeaders RejectQualificationRequest where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.RejectQualificationRequest" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RejectQualificationRequest where
  toJSON RejectQualificationRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Reason" Data..=) Prelude.<$> reason,
            Prelude.Just
              ( "QualificationRequestId"
                  Data..= qualificationRequestId
              )
          ]
      )

instance Data.ToPath RejectQualificationRequest where
  toPath = Prelude.const "/"

instance Data.ToQuery RejectQualificationRequest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRejectQualificationRequestResponse' smart constructor.
data RejectQualificationRequestResponse = RejectQualificationRequestResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RejectQualificationRequestResponse
newRejectQualificationRequestResponse pHttpStatus_ =
  RejectQualificationRequestResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
rejectQualificationRequestResponse_httpStatus :: Lens.Lens' RejectQualificationRequestResponse Prelude.Int
rejectQualificationRequestResponse_httpStatus = Lens.lens (\RejectQualificationRequestResponse' {httpStatus} -> httpStatus) (\s@RejectQualificationRequestResponse' {} a -> s {httpStatus = a} :: RejectQualificationRequestResponse)

instance
  Prelude.NFData
    RejectQualificationRequestResponse
  where
  rnf RejectQualificationRequestResponse' {..} =
    Prelude.rnf httpStatus
