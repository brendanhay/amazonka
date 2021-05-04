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
-- Module      : Network.AWS.SNS.OptInPhoneNumber
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this request to opt in a phone number that is opted out, which
-- enables you to resume sending SMS messages to the number.
--
-- You can opt in a phone number only once every 30 days.
module Network.AWS.SNS.OptInPhoneNumber
  ( -- * Creating a Request
    OptInPhoneNumber (..),
    newOptInPhoneNumber,

    -- * Request Lenses
    optInPhoneNumber_phoneNumber,

    -- * Destructuring the Response
    OptInPhoneNumberResponse (..),
    newOptInPhoneNumberResponse,

    -- * Response Lenses
    optInPhoneNumberResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

-- | Input for the OptInPhoneNumber action.
--
-- /See:/ 'newOptInPhoneNumber' smart constructor.
data OptInPhoneNumber = OptInPhoneNumber'
  { -- | The phone number to opt in.
    phoneNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OptInPhoneNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumber', 'optInPhoneNumber_phoneNumber' - The phone number to opt in.
newOptInPhoneNumber ::
  -- | 'phoneNumber'
  Prelude.Text ->
  OptInPhoneNumber
newOptInPhoneNumber pPhoneNumber_ =
  OptInPhoneNumber' {phoneNumber = pPhoneNumber_}

-- | The phone number to opt in.
optInPhoneNumber_phoneNumber :: Lens.Lens' OptInPhoneNumber Prelude.Text
optInPhoneNumber_phoneNumber = Lens.lens (\OptInPhoneNumber' {phoneNumber} -> phoneNumber) (\s@OptInPhoneNumber' {} a -> s {phoneNumber = a} :: OptInPhoneNumber)

instance Prelude.AWSRequest OptInPhoneNumber where
  type Rs OptInPhoneNumber = OptInPhoneNumberResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "OptInPhoneNumberResult"
      ( \s h x ->
          OptInPhoneNumberResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable OptInPhoneNumber

instance Prelude.NFData OptInPhoneNumber

instance Prelude.ToHeaders OptInPhoneNumber where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath OptInPhoneNumber where
  toPath = Prelude.const "/"

instance Prelude.ToQuery OptInPhoneNumber where
  toQuery OptInPhoneNumber' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("OptInPhoneNumber" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-03-31" :: Prelude.ByteString),
        "phoneNumber" Prelude.=: phoneNumber
      ]

-- | The response for the OptInPhoneNumber action.
--
-- /See:/ 'newOptInPhoneNumberResponse' smart constructor.
data OptInPhoneNumberResponse = OptInPhoneNumberResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OptInPhoneNumberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'optInPhoneNumberResponse_httpStatus' - The response's http status code.
newOptInPhoneNumberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  OptInPhoneNumberResponse
newOptInPhoneNumberResponse pHttpStatus_ =
  OptInPhoneNumberResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
optInPhoneNumberResponse_httpStatus :: Lens.Lens' OptInPhoneNumberResponse Prelude.Int
optInPhoneNumberResponse_httpStatus = Lens.lens (\OptInPhoneNumberResponse' {httpStatus} -> httpStatus) (\s@OptInPhoneNumberResponse' {} a -> s {httpStatus = a} :: OptInPhoneNumberResponse)

instance Prelude.NFData OptInPhoneNumberResponse
