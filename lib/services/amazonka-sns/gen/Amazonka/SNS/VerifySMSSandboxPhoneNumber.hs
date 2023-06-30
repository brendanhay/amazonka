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
-- Module      : Amazonka.SNS.VerifySMSSandboxPhoneNumber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies a destination phone number with a one-time password (OTP) for
-- the calling Amazon Web Services account.
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
module Amazonka.SNS.VerifySMSSandboxPhoneNumber
  ( -- * Creating a Request
    VerifySMSSandboxPhoneNumber (..),
    newVerifySMSSandboxPhoneNumber,

    -- * Request Lenses
    verifySMSSandboxPhoneNumber_phoneNumber,
    verifySMSSandboxPhoneNumber_oneTimePassword,

    -- * Destructuring the Response
    VerifySMSSandboxPhoneNumberResponse (..),
    newVerifySMSSandboxPhoneNumberResponse,

    -- * Response Lenses
    verifySMSSandboxPhoneNumberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | /See:/ 'newVerifySMSSandboxPhoneNumber' smart constructor.
data VerifySMSSandboxPhoneNumber = VerifySMSSandboxPhoneNumber'
  { -- | The destination phone number to verify.
    phoneNumber :: Prelude.Text,
    -- | The OTP sent to the destination number from the
    -- @CreateSMSSandBoxPhoneNumber@ call.
    oneTimePassword :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifySMSSandboxPhoneNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumber', 'verifySMSSandboxPhoneNumber_phoneNumber' - The destination phone number to verify.
--
-- 'oneTimePassword', 'verifySMSSandboxPhoneNumber_oneTimePassword' - The OTP sent to the destination number from the
-- @CreateSMSSandBoxPhoneNumber@ call.
newVerifySMSSandboxPhoneNumber ::
  -- | 'phoneNumber'
  Prelude.Text ->
  -- | 'oneTimePassword'
  Prelude.Text ->
  VerifySMSSandboxPhoneNumber
newVerifySMSSandboxPhoneNumber
  pPhoneNumber_
  pOneTimePassword_ =
    VerifySMSSandboxPhoneNumber'
      { phoneNumber =
          pPhoneNumber_,
        oneTimePassword = pOneTimePassword_
      }

-- | The destination phone number to verify.
verifySMSSandboxPhoneNumber_phoneNumber :: Lens.Lens' VerifySMSSandboxPhoneNumber Prelude.Text
verifySMSSandboxPhoneNumber_phoneNumber = Lens.lens (\VerifySMSSandboxPhoneNumber' {phoneNumber} -> phoneNumber) (\s@VerifySMSSandboxPhoneNumber' {} a -> s {phoneNumber = a} :: VerifySMSSandboxPhoneNumber)

-- | The OTP sent to the destination number from the
-- @CreateSMSSandBoxPhoneNumber@ call.
verifySMSSandboxPhoneNumber_oneTimePassword :: Lens.Lens' VerifySMSSandboxPhoneNumber Prelude.Text
verifySMSSandboxPhoneNumber_oneTimePassword = Lens.lens (\VerifySMSSandboxPhoneNumber' {oneTimePassword} -> oneTimePassword) (\s@VerifySMSSandboxPhoneNumber' {} a -> s {oneTimePassword = a} :: VerifySMSSandboxPhoneNumber)

instance Core.AWSRequest VerifySMSSandboxPhoneNumber where
  type
    AWSResponse VerifySMSSandboxPhoneNumber =
      VerifySMSSandboxPhoneNumberResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "VerifySMSSandboxPhoneNumberResult"
      ( \s h x ->
          VerifySMSSandboxPhoneNumberResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable VerifySMSSandboxPhoneNumber where
  hashWithSalt _salt VerifySMSSandboxPhoneNumber' {..} =
    _salt
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` oneTimePassword

instance Prelude.NFData VerifySMSSandboxPhoneNumber where
  rnf VerifySMSSandboxPhoneNumber' {..} =
    Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf oneTimePassword

instance Data.ToHeaders VerifySMSSandboxPhoneNumber where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath VerifySMSSandboxPhoneNumber where
  toPath = Prelude.const "/"

instance Data.ToQuery VerifySMSSandboxPhoneNumber where
  toQuery VerifySMSSandboxPhoneNumber' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "VerifySMSSandboxPhoneNumber" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "PhoneNumber" Data.=: phoneNumber,
        "OneTimePassword" Data.=: oneTimePassword
      ]

-- | The destination phone number\'s verification status.
--
-- /See:/ 'newVerifySMSSandboxPhoneNumberResponse' smart constructor.
data VerifySMSSandboxPhoneNumberResponse = VerifySMSSandboxPhoneNumberResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifySMSSandboxPhoneNumberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'verifySMSSandboxPhoneNumberResponse_httpStatus' - The response's http status code.
newVerifySMSSandboxPhoneNumberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  VerifySMSSandboxPhoneNumberResponse
newVerifySMSSandboxPhoneNumberResponse pHttpStatus_ =
  VerifySMSSandboxPhoneNumberResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
verifySMSSandboxPhoneNumberResponse_httpStatus :: Lens.Lens' VerifySMSSandboxPhoneNumberResponse Prelude.Int
verifySMSSandboxPhoneNumberResponse_httpStatus = Lens.lens (\VerifySMSSandboxPhoneNumberResponse' {httpStatus} -> httpStatus) (\s@VerifySMSSandboxPhoneNumberResponse' {} a -> s {httpStatus = a} :: VerifySMSSandboxPhoneNumberResponse)

instance
  Prelude.NFData
    VerifySMSSandboxPhoneNumberResponse
  where
  rnf VerifySMSSandboxPhoneNumberResponse' {..} =
    Prelude.rnf httpStatus
