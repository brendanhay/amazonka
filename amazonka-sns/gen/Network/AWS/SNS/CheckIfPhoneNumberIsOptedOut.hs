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
-- Module      : Network.AWS.SNS.CheckIfPhoneNumberIsOptedOut
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a phone number and indicates whether the phone holder has opted
-- out of receiving SMS messages from your account. You cannot send SMS
-- messages to a number that is opted out.
--
-- To resume sending messages, you can opt in the number by using the
-- @OptInPhoneNumber@ action.
module Network.AWS.SNS.CheckIfPhoneNumberIsOptedOut
  ( -- * Creating a Request
    CheckIfPhoneNumberIsOptedOut (..),
    newCheckIfPhoneNumberIsOptedOut,

    -- * Request Lenses
    checkIfPhoneNumberIsOptedOut_phoneNumber,

    -- * Destructuring the Response
    CheckIfPhoneNumberIsOptedOutResponse (..),
    newCheckIfPhoneNumberIsOptedOutResponse,

    -- * Response Lenses
    checkIfPhoneNumberIsOptedOutResponse_isOptedOut,
    checkIfPhoneNumberIsOptedOutResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

-- | The input for the @CheckIfPhoneNumberIsOptedOut@ action.
--
-- /See:/ 'newCheckIfPhoneNumberIsOptedOut' smart constructor.
data CheckIfPhoneNumberIsOptedOut = CheckIfPhoneNumberIsOptedOut'
  { -- | The phone number for which you want to check the opt out status.
    phoneNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CheckIfPhoneNumberIsOptedOut' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumber', 'checkIfPhoneNumberIsOptedOut_phoneNumber' - The phone number for which you want to check the opt out status.
newCheckIfPhoneNumberIsOptedOut ::
  -- | 'phoneNumber'
  Prelude.Text ->
  CheckIfPhoneNumberIsOptedOut
newCheckIfPhoneNumberIsOptedOut pPhoneNumber_ =
  CheckIfPhoneNumberIsOptedOut'
    { phoneNumber =
        pPhoneNumber_
    }

-- | The phone number for which you want to check the opt out status.
checkIfPhoneNumberIsOptedOut_phoneNumber :: Lens.Lens' CheckIfPhoneNumberIsOptedOut Prelude.Text
checkIfPhoneNumberIsOptedOut_phoneNumber = Lens.lens (\CheckIfPhoneNumberIsOptedOut' {phoneNumber} -> phoneNumber) (\s@CheckIfPhoneNumberIsOptedOut' {} a -> s {phoneNumber = a} :: CheckIfPhoneNumberIsOptedOut)

instance
  Prelude.AWSRequest
    CheckIfPhoneNumberIsOptedOut
  where
  type
    Rs CheckIfPhoneNumberIsOptedOut =
      CheckIfPhoneNumberIsOptedOutResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CheckIfPhoneNumberIsOptedOutResult"
      ( \s h x ->
          CheckIfPhoneNumberIsOptedOutResponse'
            Prelude.<$> (x Prelude..@? "isOptedOut")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CheckIfPhoneNumberIsOptedOut

instance Prelude.NFData CheckIfPhoneNumberIsOptedOut

instance
  Prelude.ToHeaders
    CheckIfPhoneNumberIsOptedOut
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CheckIfPhoneNumberIsOptedOut where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CheckIfPhoneNumberIsOptedOut where
  toQuery CheckIfPhoneNumberIsOptedOut' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "CheckIfPhoneNumberIsOptedOut" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2010-03-31" :: Prelude.ByteString),
        "phoneNumber" Prelude.=: phoneNumber
      ]

-- | The response from the @CheckIfPhoneNumberIsOptedOut@ action.
--
-- /See:/ 'newCheckIfPhoneNumberIsOptedOutResponse' smart constructor.
data CheckIfPhoneNumberIsOptedOutResponse = CheckIfPhoneNumberIsOptedOutResponse'
  { -- | Indicates whether the phone number is opted out:
    --
    -- -   @true@ – The phone number is opted out, meaning you cannot publish
    --     SMS messages to it.
    --
    -- -   @false@ – The phone number is opted in, meaning you can publish SMS
    --     messages to it.
    isOptedOut :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CheckIfPhoneNumberIsOptedOutResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isOptedOut', 'checkIfPhoneNumberIsOptedOutResponse_isOptedOut' - Indicates whether the phone number is opted out:
--
-- -   @true@ – The phone number is opted out, meaning you cannot publish
--     SMS messages to it.
--
-- -   @false@ – The phone number is opted in, meaning you can publish SMS
--     messages to it.
--
-- 'httpStatus', 'checkIfPhoneNumberIsOptedOutResponse_httpStatus' - The response's http status code.
newCheckIfPhoneNumberIsOptedOutResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CheckIfPhoneNumberIsOptedOutResponse
newCheckIfPhoneNumberIsOptedOutResponse pHttpStatus_ =
  CheckIfPhoneNumberIsOptedOutResponse'
    { isOptedOut =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether the phone number is opted out:
--
-- -   @true@ – The phone number is opted out, meaning you cannot publish
--     SMS messages to it.
--
-- -   @false@ – The phone number is opted in, meaning you can publish SMS
--     messages to it.
checkIfPhoneNumberIsOptedOutResponse_isOptedOut :: Lens.Lens' CheckIfPhoneNumberIsOptedOutResponse (Prelude.Maybe Prelude.Bool)
checkIfPhoneNumberIsOptedOutResponse_isOptedOut = Lens.lens (\CheckIfPhoneNumberIsOptedOutResponse' {isOptedOut} -> isOptedOut) (\s@CheckIfPhoneNumberIsOptedOutResponse' {} a -> s {isOptedOut = a} :: CheckIfPhoneNumberIsOptedOutResponse)

-- | The response's http status code.
checkIfPhoneNumberIsOptedOutResponse_httpStatus :: Lens.Lens' CheckIfPhoneNumberIsOptedOutResponse Prelude.Int
checkIfPhoneNumberIsOptedOutResponse_httpStatus = Lens.lens (\CheckIfPhoneNumberIsOptedOutResponse' {httpStatus} -> httpStatus) (\s@CheckIfPhoneNumberIsOptedOutResponse' {} a -> s {httpStatus = a} :: CheckIfPhoneNumberIsOptedOutResponse)

instance
  Prelude.NFData
    CheckIfPhoneNumberIsOptedOutResponse
