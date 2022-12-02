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
-- Module      : Amazonka.Pinpoint.VerifyOTPMessage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verify an OTP
module Amazonka.Pinpoint.VerifyOTPMessage
  ( -- * Creating a Request
    VerifyOTPMessage (..),
    newVerifyOTPMessage,

    -- * Request Lenses
    verifyOTPMessage_applicationId,
    verifyOTPMessage_verifyOTPMessageRequestParameters,

    -- * Destructuring the Response
    VerifyOTPMessageResponse (..),
    newVerifyOTPMessageResponse,

    -- * Response Lenses
    verifyOTPMessageResponse_httpStatus,
    verifyOTPMessageResponse_verificationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newVerifyOTPMessage' smart constructor.
data VerifyOTPMessage = VerifyOTPMessage'
  { -- | The unique ID of your Amazon Pinpoint application.
    applicationId :: Prelude.Text,
    verifyOTPMessageRequestParameters :: VerifyOTPMessageRequestParameters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyOTPMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'verifyOTPMessage_applicationId' - The unique ID of your Amazon Pinpoint application.
--
-- 'verifyOTPMessageRequestParameters', 'verifyOTPMessage_verifyOTPMessageRequestParameters' - Undocumented member.
newVerifyOTPMessage ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'verifyOTPMessageRequestParameters'
  VerifyOTPMessageRequestParameters ->
  VerifyOTPMessage
newVerifyOTPMessage
  pApplicationId_
  pVerifyOTPMessageRequestParameters_ =
    VerifyOTPMessage'
      { applicationId = pApplicationId_,
        verifyOTPMessageRequestParameters =
          pVerifyOTPMessageRequestParameters_
      }

-- | The unique ID of your Amazon Pinpoint application.
verifyOTPMessage_applicationId :: Lens.Lens' VerifyOTPMessage Prelude.Text
verifyOTPMessage_applicationId = Lens.lens (\VerifyOTPMessage' {applicationId} -> applicationId) (\s@VerifyOTPMessage' {} a -> s {applicationId = a} :: VerifyOTPMessage)

-- | Undocumented member.
verifyOTPMessage_verifyOTPMessageRequestParameters :: Lens.Lens' VerifyOTPMessage VerifyOTPMessageRequestParameters
verifyOTPMessage_verifyOTPMessageRequestParameters = Lens.lens (\VerifyOTPMessage' {verifyOTPMessageRequestParameters} -> verifyOTPMessageRequestParameters) (\s@VerifyOTPMessage' {} a -> s {verifyOTPMessageRequestParameters = a} :: VerifyOTPMessage)

instance Core.AWSRequest VerifyOTPMessage where
  type
    AWSResponse VerifyOTPMessage =
      VerifyOTPMessageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          VerifyOTPMessageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable VerifyOTPMessage where
  hashWithSalt _salt VerifyOTPMessage' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` verifyOTPMessageRequestParameters

instance Prelude.NFData VerifyOTPMessage where
  rnf VerifyOTPMessage' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf verifyOTPMessageRequestParameters

instance Data.ToHeaders VerifyOTPMessage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON VerifyOTPMessage where
  toJSON VerifyOTPMessage' {..} =
    Data.toJSON verifyOTPMessageRequestParameters

instance Data.ToPath VerifyOTPMessage where
  toPath VerifyOTPMessage' {..} =
    Prelude.mconcat
      ["/v1/apps/", Data.toBS applicationId, "/verify-otp"]

instance Data.ToQuery VerifyOTPMessage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newVerifyOTPMessageResponse' smart constructor.
data VerifyOTPMessageResponse = VerifyOTPMessageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    verificationResponse :: VerificationResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyOTPMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'verifyOTPMessageResponse_httpStatus' - The response's http status code.
--
-- 'verificationResponse', 'verifyOTPMessageResponse_verificationResponse' - Undocumented member.
newVerifyOTPMessageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'verificationResponse'
  VerificationResponse ->
  VerifyOTPMessageResponse
newVerifyOTPMessageResponse
  pHttpStatus_
  pVerificationResponse_ =
    VerifyOTPMessageResponse'
      { httpStatus =
          pHttpStatus_,
        verificationResponse = pVerificationResponse_
      }

-- | The response's http status code.
verifyOTPMessageResponse_httpStatus :: Lens.Lens' VerifyOTPMessageResponse Prelude.Int
verifyOTPMessageResponse_httpStatus = Lens.lens (\VerifyOTPMessageResponse' {httpStatus} -> httpStatus) (\s@VerifyOTPMessageResponse' {} a -> s {httpStatus = a} :: VerifyOTPMessageResponse)

-- | Undocumented member.
verifyOTPMessageResponse_verificationResponse :: Lens.Lens' VerifyOTPMessageResponse VerificationResponse
verifyOTPMessageResponse_verificationResponse = Lens.lens (\VerifyOTPMessageResponse' {verificationResponse} -> verificationResponse) (\s@VerifyOTPMessageResponse' {} a -> s {verificationResponse = a} :: VerifyOTPMessageResponse)

instance Prelude.NFData VerifyOTPMessageResponse where
  rnf VerifyOTPMessageResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf verificationResponse
