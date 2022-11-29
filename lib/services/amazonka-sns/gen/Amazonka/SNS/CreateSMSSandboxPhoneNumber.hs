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
-- Module      : Amazonka.SNS.CreateSMSSandboxPhoneNumber
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a destination phone number to an Amazon Web Services account in the
-- SMS sandbox and sends a one-time password (OTP) to that phone number.
--
-- When you start using Amazon SNS to send SMS messages, your Amazon Web
-- Services account is in the /SMS sandbox/. The SMS sandbox provides a
-- safe environment for you to try Amazon SNS features without risking your
-- reputation as an SMS sender. While your Amazon Web Services account is
-- in the SMS sandbox, you can use all of the features of Amazon SNS.
-- However, you can send SMS messages only to verified destination phone
-- numbers. For more information, including how to move out of the sandbox
-- to send messages without restrictions, see
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-sms-sandbox.html SMS sandbox>
-- in the /Amazon SNS Developer Guide/.
module Amazonka.SNS.CreateSMSSandboxPhoneNumber
  ( -- * Creating a Request
    CreateSMSSandboxPhoneNumber (..),
    newCreateSMSSandboxPhoneNumber,

    -- * Request Lenses
    createSMSSandboxPhoneNumber_languageCode,
    createSMSSandboxPhoneNumber_phoneNumber,

    -- * Destructuring the Response
    CreateSMSSandboxPhoneNumberResponse (..),
    newCreateSMSSandboxPhoneNumberResponse,

    -- * Response Lenses
    createSMSSandboxPhoneNumberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | /See:/ 'newCreateSMSSandboxPhoneNumber' smart constructor.
data CreateSMSSandboxPhoneNumber = CreateSMSSandboxPhoneNumber'
  { -- | The language to use for sending the OTP. The default value is @en-US@.
    languageCode :: Prelude.Maybe LanguageCodeString,
    -- | The destination phone number to verify. On verification, Amazon SNS adds
    -- this phone number to the list of verified phone numbers that you can
    -- send SMS messages to.
    phoneNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSMSSandboxPhoneNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'createSMSSandboxPhoneNumber_languageCode' - The language to use for sending the OTP. The default value is @en-US@.
--
-- 'phoneNumber', 'createSMSSandboxPhoneNumber_phoneNumber' - The destination phone number to verify. On verification, Amazon SNS adds
-- this phone number to the list of verified phone numbers that you can
-- send SMS messages to.
newCreateSMSSandboxPhoneNumber ::
  -- | 'phoneNumber'
  Prelude.Text ->
  CreateSMSSandboxPhoneNumber
newCreateSMSSandboxPhoneNumber pPhoneNumber_ =
  CreateSMSSandboxPhoneNumber'
    { languageCode =
        Prelude.Nothing,
      phoneNumber = pPhoneNumber_
    }

-- | The language to use for sending the OTP. The default value is @en-US@.
createSMSSandboxPhoneNumber_languageCode :: Lens.Lens' CreateSMSSandboxPhoneNumber (Prelude.Maybe LanguageCodeString)
createSMSSandboxPhoneNumber_languageCode = Lens.lens (\CreateSMSSandboxPhoneNumber' {languageCode} -> languageCode) (\s@CreateSMSSandboxPhoneNumber' {} a -> s {languageCode = a} :: CreateSMSSandboxPhoneNumber)

-- | The destination phone number to verify. On verification, Amazon SNS adds
-- this phone number to the list of verified phone numbers that you can
-- send SMS messages to.
createSMSSandboxPhoneNumber_phoneNumber :: Lens.Lens' CreateSMSSandboxPhoneNumber Prelude.Text
createSMSSandboxPhoneNumber_phoneNumber = Lens.lens (\CreateSMSSandboxPhoneNumber' {phoneNumber} -> phoneNumber) (\s@CreateSMSSandboxPhoneNumber' {} a -> s {phoneNumber = a} :: CreateSMSSandboxPhoneNumber)

instance Core.AWSRequest CreateSMSSandboxPhoneNumber where
  type
    AWSResponse CreateSMSSandboxPhoneNumber =
      CreateSMSSandboxPhoneNumberResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateSMSSandboxPhoneNumberResult"
      ( \s h x ->
          CreateSMSSandboxPhoneNumberResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSMSSandboxPhoneNumber where
  hashWithSalt _salt CreateSMSSandboxPhoneNumber' {..} =
    _salt `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` phoneNumber

instance Prelude.NFData CreateSMSSandboxPhoneNumber where
  rnf CreateSMSSandboxPhoneNumber' {..} =
    Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf phoneNumber

instance Core.ToHeaders CreateSMSSandboxPhoneNumber where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateSMSSandboxPhoneNumber where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateSMSSandboxPhoneNumber where
  toQuery CreateSMSSandboxPhoneNumber' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "CreateSMSSandboxPhoneNumber" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-03-31" :: Prelude.ByteString),
        "LanguageCode" Core.=: languageCode,
        "PhoneNumber" Core.=: phoneNumber
      ]

-- | /See:/ 'newCreateSMSSandboxPhoneNumberResponse' smart constructor.
data CreateSMSSandboxPhoneNumberResponse = CreateSMSSandboxPhoneNumberResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSMSSandboxPhoneNumberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createSMSSandboxPhoneNumberResponse_httpStatus' - The response's http status code.
newCreateSMSSandboxPhoneNumberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSMSSandboxPhoneNumberResponse
newCreateSMSSandboxPhoneNumberResponse pHttpStatus_ =
  CreateSMSSandboxPhoneNumberResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createSMSSandboxPhoneNumberResponse_httpStatus :: Lens.Lens' CreateSMSSandboxPhoneNumberResponse Prelude.Int
createSMSSandboxPhoneNumberResponse_httpStatus = Lens.lens (\CreateSMSSandboxPhoneNumberResponse' {httpStatus} -> httpStatus) (\s@CreateSMSSandboxPhoneNumberResponse' {} a -> s {httpStatus = a} :: CreateSMSSandboxPhoneNumberResponse)

instance
  Prelude.NFData
    CreateSMSSandboxPhoneNumberResponse
  where
  rnf CreateSMSSandboxPhoneNumberResponse' {..} =
    Prelude.rnf httpStatus
