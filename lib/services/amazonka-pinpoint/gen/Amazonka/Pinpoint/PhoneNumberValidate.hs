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
-- Module      : Amazonka.Pinpoint.PhoneNumberValidate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a phone number.
module Amazonka.Pinpoint.PhoneNumberValidate
  ( -- * Creating a Request
    PhoneNumberValidate (..),
    newPhoneNumberValidate,

    -- * Request Lenses
    phoneNumberValidate_numberValidateRequest,

    -- * Destructuring the Response
    PhoneNumberValidateResponse (..),
    newPhoneNumberValidateResponse,

    -- * Response Lenses
    phoneNumberValidateResponse_httpStatus,
    phoneNumberValidateResponse_numberValidateResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPhoneNumberValidate' smart constructor.
data PhoneNumberValidate = PhoneNumberValidate'
  { numberValidateRequest :: NumberValidateRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhoneNumberValidate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberValidateRequest', 'phoneNumberValidate_numberValidateRequest' - Undocumented member.
newPhoneNumberValidate ::
  -- | 'numberValidateRequest'
  NumberValidateRequest ->
  PhoneNumberValidate
newPhoneNumberValidate pNumberValidateRequest_ =
  PhoneNumberValidate'
    { numberValidateRequest =
        pNumberValidateRequest_
    }

-- | Undocumented member.
phoneNumberValidate_numberValidateRequest :: Lens.Lens' PhoneNumberValidate NumberValidateRequest
phoneNumberValidate_numberValidateRequest = Lens.lens (\PhoneNumberValidate' {numberValidateRequest} -> numberValidateRequest) (\s@PhoneNumberValidate' {} a -> s {numberValidateRequest = a} :: PhoneNumberValidate)

instance Core.AWSRequest PhoneNumberValidate where
  type
    AWSResponse PhoneNumberValidate =
      PhoneNumberValidateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PhoneNumberValidateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable PhoneNumberValidate where
  hashWithSalt _salt PhoneNumberValidate' {..} =
    _salt `Prelude.hashWithSalt` numberValidateRequest

instance Prelude.NFData PhoneNumberValidate where
  rnf PhoneNumberValidate' {..} =
    Prelude.rnf numberValidateRequest

instance Core.ToHeaders PhoneNumberValidate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PhoneNumberValidate where
  toJSON PhoneNumberValidate' {..} =
    Core.toJSON numberValidateRequest

instance Core.ToPath PhoneNumberValidate where
  toPath = Prelude.const "/v1/phone/number/validate"

instance Core.ToQuery PhoneNumberValidate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPhoneNumberValidateResponse' smart constructor.
data PhoneNumberValidateResponse = PhoneNumberValidateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    numberValidateResponse :: NumberValidateResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhoneNumberValidateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'phoneNumberValidateResponse_httpStatus' - The response's http status code.
--
-- 'numberValidateResponse', 'phoneNumberValidateResponse_numberValidateResponse' - Undocumented member.
newPhoneNumberValidateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'numberValidateResponse'
  NumberValidateResponse ->
  PhoneNumberValidateResponse
newPhoneNumberValidateResponse
  pHttpStatus_
  pNumberValidateResponse_ =
    PhoneNumberValidateResponse'
      { httpStatus =
          pHttpStatus_,
        numberValidateResponse =
          pNumberValidateResponse_
      }

-- | The response's http status code.
phoneNumberValidateResponse_httpStatus :: Lens.Lens' PhoneNumberValidateResponse Prelude.Int
phoneNumberValidateResponse_httpStatus = Lens.lens (\PhoneNumberValidateResponse' {httpStatus} -> httpStatus) (\s@PhoneNumberValidateResponse' {} a -> s {httpStatus = a} :: PhoneNumberValidateResponse)

-- | Undocumented member.
phoneNumberValidateResponse_numberValidateResponse :: Lens.Lens' PhoneNumberValidateResponse NumberValidateResponse
phoneNumberValidateResponse_numberValidateResponse = Lens.lens (\PhoneNumberValidateResponse' {numberValidateResponse} -> numberValidateResponse) (\s@PhoneNumberValidateResponse' {} a -> s {numberValidateResponse = a} :: PhoneNumberValidateResponse)

instance Prelude.NFData PhoneNumberValidateResponse where
  rnf PhoneNumberValidateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf numberValidateResponse
