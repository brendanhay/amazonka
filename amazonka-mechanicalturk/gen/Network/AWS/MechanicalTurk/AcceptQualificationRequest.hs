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
-- Module      : Network.AWS.MechanicalTurk.AcceptQualificationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @AcceptQualificationRequest@ operation approves a Worker\'s request
-- for a Qualification.
--
-- Only the owner of the Qualification type can grant a Qualification
-- request for that type.
--
-- A successful request for the @AcceptQualificationRequest@ operation
-- returns with no errors and an empty body.
module Network.AWS.MechanicalTurk.AcceptQualificationRequest
  ( -- * Creating a Request
    AcceptQualificationRequest (..),
    newAcceptQualificationRequest,

    -- * Request Lenses
    acceptQualificationRequest_integerValue,
    acceptQualificationRequest_qualificationRequestId,

    -- * Destructuring the Response
    AcceptQualificationRequestResponse (..),
    newAcceptQualificationRequestResponse,

    -- * Response Lenses
    acceptQualificationRequestResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAcceptQualificationRequest' smart constructor.
data AcceptQualificationRequest = AcceptQualificationRequest'
  { -- | The value of the Qualification. You can omit this value if you are using
    -- the presence or absence of the Qualification as the basis for a HIT
    -- requirement.
    integerValue :: Core.Maybe Core.Int,
    -- | The ID of the Qualification request, as returned by the
    -- @GetQualificationRequests@ operation.
    qualificationRequestId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcceptQualificationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'integerValue', 'acceptQualificationRequest_integerValue' - The value of the Qualification. You can omit this value if you are using
-- the presence or absence of the Qualification as the basis for a HIT
-- requirement.
--
-- 'qualificationRequestId', 'acceptQualificationRequest_qualificationRequestId' - The ID of the Qualification request, as returned by the
-- @GetQualificationRequests@ operation.
newAcceptQualificationRequest ::
  -- | 'qualificationRequestId'
  Core.Text ->
  AcceptQualificationRequest
newAcceptQualificationRequest
  pQualificationRequestId_ =
    AcceptQualificationRequest'
      { integerValue =
          Core.Nothing,
        qualificationRequestId =
          pQualificationRequestId_
      }

-- | The value of the Qualification. You can omit this value if you are using
-- the presence or absence of the Qualification as the basis for a HIT
-- requirement.
acceptQualificationRequest_integerValue :: Lens.Lens' AcceptQualificationRequest (Core.Maybe Core.Int)
acceptQualificationRequest_integerValue = Lens.lens (\AcceptQualificationRequest' {integerValue} -> integerValue) (\s@AcceptQualificationRequest' {} a -> s {integerValue = a} :: AcceptQualificationRequest)

-- | The ID of the Qualification request, as returned by the
-- @GetQualificationRequests@ operation.
acceptQualificationRequest_qualificationRequestId :: Lens.Lens' AcceptQualificationRequest Core.Text
acceptQualificationRequest_qualificationRequestId = Lens.lens (\AcceptQualificationRequest' {qualificationRequestId} -> qualificationRequestId) (\s@AcceptQualificationRequest' {} a -> s {qualificationRequestId = a} :: AcceptQualificationRequest)

instance Core.AWSRequest AcceptQualificationRequest where
  type
    AWSResponse AcceptQualificationRequest =
      AcceptQualificationRequestResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AcceptQualificationRequestResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AcceptQualificationRequest

instance Core.NFData AcceptQualificationRequest

instance Core.ToHeaders AcceptQualificationRequest where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.AcceptQualificationRequest" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AcceptQualificationRequest where
  toJSON AcceptQualificationRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("IntegerValue" Core..=) Core.<$> integerValue,
            Core.Just
              ( "QualificationRequestId"
                  Core..= qualificationRequestId
              )
          ]
      )

instance Core.ToPath AcceptQualificationRequest where
  toPath = Core.const "/"

instance Core.ToQuery AcceptQualificationRequest where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAcceptQualificationRequestResponse' smart constructor.
data AcceptQualificationRequestResponse = AcceptQualificationRequestResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcceptQualificationRequestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'acceptQualificationRequestResponse_httpStatus' - The response's http status code.
newAcceptQualificationRequestResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AcceptQualificationRequestResponse
newAcceptQualificationRequestResponse pHttpStatus_ =
  AcceptQualificationRequestResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
acceptQualificationRequestResponse_httpStatus :: Lens.Lens' AcceptQualificationRequestResponse Core.Int
acceptQualificationRequestResponse_httpStatus = Lens.lens (\AcceptQualificationRequestResponse' {httpStatus} -> httpStatus) (\s@AcceptQualificationRequestResponse' {} a -> s {httpStatus = a} :: AcceptQualificationRequestResponse)

instance
  Core.NFData
    AcceptQualificationRequestResponse
