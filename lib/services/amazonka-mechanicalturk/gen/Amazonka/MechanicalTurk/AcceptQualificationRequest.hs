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
-- Module      : Amazonka.MechanicalTurk.AcceptQualificationRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.MechanicalTurk.AcceptQualificationRequest
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAcceptQualificationRequest' smart constructor.
data AcceptQualificationRequest = AcceptQualificationRequest'
  { -- | The value of the Qualification. You can omit this value if you are using
    -- the presence or absence of the Qualification as the basis for a HIT
    -- requirement.
    integerValue :: Prelude.Maybe Prelude.Int,
    -- | The ID of the Qualification request, as returned by the
    -- @GetQualificationRequests@ operation.
    qualificationRequestId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  AcceptQualificationRequest
newAcceptQualificationRequest
  pQualificationRequestId_ =
    AcceptQualificationRequest'
      { integerValue =
          Prelude.Nothing,
        qualificationRequestId =
          pQualificationRequestId_
      }

-- | The value of the Qualification. You can omit this value if you are using
-- the presence or absence of the Qualification as the basis for a HIT
-- requirement.
acceptQualificationRequest_integerValue :: Lens.Lens' AcceptQualificationRequest (Prelude.Maybe Prelude.Int)
acceptQualificationRequest_integerValue = Lens.lens (\AcceptQualificationRequest' {integerValue} -> integerValue) (\s@AcceptQualificationRequest' {} a -> s {integerValue = a} :: AcceptQualificationRequest)

-- | The ID of the Qualification request, as returned by the
-- @GetQualificationRequests@ operation.
acceptQualificationRequest_qualificationRequestId :: Lens.Lens' AcceptQualificationRequest Prelude.Text
acceptQualificationRequest_qualificationRequestId = Lens.lens (\AcceptQualificationRequest' {qualificationRequestId} -> qualificationRequestId) (\s@AcceptQualificationRequest' {} a -> s {qualificationRequestId = a} :: AcceptQualificationRequest)

instance Core.AWSRequest AcceptQualificationRequest where
  type
    AWSResponse AcceptQualificationRequest =
      AcceptQualificationRequestResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AcceptQualificationRequestResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AcceptQualificationRequest where
  hashWithSalt _salt AcceptQualificationRequest' {..} =
    _salt
      `Prelude.hashWithSalt` integerValue
      `Prelude.hashWithSalt` qualificationRequestId

instance Prelude.NFData AcceptQualificationRequest where
  rnf AcceptQualificationRequest' {..} =
    Prelude.rnf integerValue
      `Prelude.seq` Prelude.rnf qualificationRequestId

instance Data.ToHeaders AcceptQualificationRequest where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.AcceptQualificationRequest" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AcceptQualificationRequest where
  toJSON AcceptQualificationRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IntegerValue" Data..=) Prelude.<$> integerValue,
            Prelude.Just
              ( "QualificationRequestId"
                  Data..= qualificationRequestId
              )
          ]
      )

instance Data.ToPath AcceptQualificationRequest where
  toPath = Prelude.const "/"

instance Data.ToQuery AcceptQualificationRequest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAcceptQualificationRequestResponse' smart constructor.
data AcceptQualificationRequestResponse = AcceptQualificationRequestResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AcceptQualificationRequestResponse
newAcceptQualificationRequestResponse pHttpStatus_ =
  AcceptQualificationRequestResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
acceptQualificationRequestResponse_httpStatus :: Lens.Lens' AcceptQualificationRequestResponse Prelude.Int
acceptQualificationRequestResponse_httpStatus = Lens.lens (\AcceptQualificationRequestResponse' {httpStatus} -> httpStatus) (\s@AcceptQualificationRequestResponse' {} a -> s {httpStatus = a} :: AcceptQualificationRequestResponse)

instance
  Prelude.NFData
    AcceptQualificationRequestResponse
  where
  rnf AcceptQualificationRequestResponse' {..} =
    Prelude.rnf httpStatus
