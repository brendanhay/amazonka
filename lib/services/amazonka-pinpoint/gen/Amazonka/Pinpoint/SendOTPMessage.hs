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
-- Module      : Amazonka.Pinpoint.SendOTPMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Send an OTP message
module Amazonka.Pinpoint.SendOTPMessage
  ( -- * Creating a Request
    SendOTPMessage (..),
    newSendOTPMessage,

    -- * Request Lenses
    sendOTPMessage_applicationId,
    sendOTPMessage_sendOTPMessageRequestParameters,

    -- * Destructuring the Response
    SendOTPMessageResponse (..),
    newSendOTPMessageResponse,

    -- * Response Lenses
    sendOTPMessageResponse_httpStatus,
    sendOTPMessageResponse_messageResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendOTPMessage' smart constructor.
data SendOTPMessage = SendOTPMessage'
  { -- | The unique ID of your Amazon Pinpoint application.
    applicationId :: Prelude.Text,
    sendOTPMessageRequestParameters :: SendOTPMessageRequestParameters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendOTPMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'sendOTPMessage_applicationId' - The unique ID of your Amazon Pinpoint application.
--
-- 'sendOTPMessageRequestParameters', 'sendOTPMessage_sendOTPMessageRequestParameters' - Undocumented member.
newSendOTPMessage ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'sendOTPMessageRequestParameters'
  SendOTPMessageRequestParameters ->
  SendOTPMessage
newSendOTPMessage
  pApplicationId_
  pSendOTPMessageRequestParameters_ =
    SendOTPMessage'
      { applicationId = pApplicationId_,
        sendOTPMessageRequestParameters =
          pSendOTPMessageRequestParameters_
      }

-- | The unique ID of your Amazon Pinpoint application.
sendOTPMessage_applicationId :: Lens.Lens' SendOTPMessage Prelude.Text
sendOTPMessage_applicationId = Lens.lens (\SendOTPMessage' {applicationId} -> applicationId) (\s@SendOTPMessage' {} a -> s {applicationId = a} :: SendOTPMessage)

-- | Undocumented member.
sendOTPMessage_sendOTPMessageRequestParameters :: Lens.Lens' SendOTPMessage SendOTPMessageRequestParameters
sendOTPMessage_sendOTPMessageRequestParameters = Lens.lens (\SendOTPMessage' {sendOTPMessageRequestParameters} -> sendOTPMessageRequestParameters) (\s@SendOTPMessage' {} a -> s {sendOTPMessageRequestParameters = a} :: SendOTPMessage)

instance Core.AWSRequest SendOTPMessage where
  type
    AWSResponse SendOTPMessage =
      SendOTPMessageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SendOTPMessageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable SendOTPMessage where
  hashWithSalt _salt SendOTPMessage' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` sendOTPMessageRequestParameters

instance Prelude.NFData SendOTPMessage where
  rnf SendOTPMessage' {..} =
    Prelude.rnf applicationId `Prelude.seq`
      Prelude.rnf sendOTPMessageRequestParameters

instance Data.ToHeaders SendOTPMessage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SendOTPMessage where
  toJSON SendOTPMessage' {..} =
    Data.toJSON sendOTPMessageRequestParameters

instance Data.ToPath SendOTPMessage where
  toPath SendOTPMessage' {..} =
    Prelude.mconcat
      ["/v1/apps/", Data.toBS applicationId, "/otp"]

instance Data.ToQuery SendOTPMessage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendOTPMessageResponse' smart constructor.
data SendOTPMessageResponse = SendOTPMessageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    messageResponse :: MessageResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendOTPMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'sendOTPMessageResponse_httpStatus' - The response's http status code.
--
-- 'messageResponse', 'sendOTPMessageResponse_messageResponse' - Undocumented member.
newSendOTPMessageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'messageResponse'
  MessageResponse ->
  SendOTPMessageResponse
newSendOTPMessageResponse
  pHttpStatus_
  pMessageResponse_ =
    SendOTPMessageResponse'
      { httpStatus = pHttpStatus_,
        messageResponse = pMessageResponse_
      }

-- | The response's http status code.
sendOTPMessageResponse_httpStatus :: Lens.Lens' SendOTPMessageResponse Prelude.Int
sendOTPMessageResponse_httpStatus = Lens.lens (\SendOTPMessageResponse' {httpStatus} -> httpStatus) (\s@SendOTPMessageResponse' {} a -> s {httpStatus = a} :: SendOTPMessageResponse)

-- | Undocumented member.
sendOTPMessageResponse_messageResponse :: Lens.Lens' SendOTPMessageResponse MessageResponse
sendOTPMessageResponse_messageResponse = Lens.lens (\SendOTPMessageResponse' {messageResponse} -> messageResponse) (\s@SendOTPMessageResponse' {} a -> s {messageResponse = a} :: SendOTPMessageResponse)

instance Prelude.NFData SendOTPMessageResponse where
  rnf SendOTPMessageResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf messageResponse
