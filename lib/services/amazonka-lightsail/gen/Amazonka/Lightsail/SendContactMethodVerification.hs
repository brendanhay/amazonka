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
-- Module      : Amazonka.Lightsail.SendContactMethodVerification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a verification request to an email contact method to ensure it\'s
-- owned by the requester. SMS contact methods don\'t need to be verified.
--
-- A contact method is used to send you notifications about your Amazon
-- Lightsail resources. You can add one email address and one mobile phone
-- number contact method in each Amazon Web Services Region. However, SMS
-- text messaging is not supported in some Amazon Web Services Regions, and
-- SMS text messages cannot be sent to some countries\/regions. For more
-- information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail>.
--
-- A verification request is sent to the contact method when you initially
-- create it. Use this action to send another verification request if a
-- previous verification request was deleted, or has expired.
--
-- Notifications are not sent to an email contact method until after it is
-- verified, and confirmed as valid.
module Amazonka.Lightsail.SendContactMethodVerification
  ( -- * Creating a Request
    SendContactMethodVerification (..),
    newSendContactMethodVerification,

    -- * Request Lenses
    sendContactMethodVerification_protocol,

    -- * Destructuring the Response
    SendContactMethodVerificationResponse (..),
    newSendContactMethodVerificationResponse,

    -- * Response Lenses
    sendContactMethodVerificationResponse_operations,
    sendContactMethodVerificationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendContactMethodVerification' smart constructor.
data SendContactMethodVerification = SendContactMethodVerification'
  { -- | The protocol to verify, such as @Email@ or @SMS@ (text messaging).
    protocol :: ContactMethodVerificationProtocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendContactMethodVerification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protocol', 'sendContactMethodVerification_protocol' - The protocol to verify, such as @Email@ or @SMS@ (text messaging).
newSendContactMethodVerification ::
  -- | 'protocol'
  ContactMethodVerificationProtocol ->
  SendContactMethodVerification
newSendContactMethodVerification pProtocol_ =
  SendContactMethodVerification'
    { protocol =
        pProtocol_
    }

-- | The protocol to verify, such as @Email@ or @SMS@ (text messaging).
sendContactMethodVerification_protocol :: Lens.Lens' SendContactMethodVerification ContactMethodVerificationProtocol
sendContactMethodVerification_protocol = Lens.lens (\SendContactMethodVerification' {protocol} -> protocol) (\s@SendContactMethodVerification' {} a -> s {protocol = a} :: SendContactMethodVerification)

instance
  Core.AWSRequest
    SendContactMethodVerification
  where
  type
    AWSResponse SendContactMethodVerification =
      SendContactMethodVerificationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SendContactMethodVerificationResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    SendContactMethodVerification
  where
  hashWithSalt _salt SendContactMethodVerification' {..} =
    _salt `Prelude.hashWithSalt` protocol

instance Prelude.NFData SendContactMethodVerification where
  rnf SendContactMethodVerification' {..} =
    Prelude.rnf protocol

instance Data.ToHeaders SendContactMethodVerification where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.SendContactMethodVerification" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SendContactMethodVerification where
  toJSON SendContactMethodVerification' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("protocol" Data..= protocol)]
      )

instance Data.ToPath SendContactMethodVerification where
  toPath = Prelude.const "/"

instance Data.ToQuery SendContactMethodVerification where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendContactMethodVerificationResponse' smart constructor.
data SendContactMethodVerificationResponse = SendContactMethodVerificationResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendContactMethodVerificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'sendContactMethodVerificationResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'sendContactMethodVerificationResponse_httpStatus' - The response's http status code.
newSendContactMethodVerificationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendContactMethodVerificationResponse
newSendContactMethodVerificationResponse pHttpStatus_ =
  SendContactMethodVerificationResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
sendContactMethodVerificationResponse_operations :: Lens.Lens' SendContactMethodVerificationResponse (Prelude.Maybe [Operation])
sendContactMethodVerificationResponse_operations = Lens.lens (\SendContactMethodVerificationResponse' {operations} -> operations) (\s@SendContactMethodVerificationResponse' {} a -> s {operations = a} :: SendContactMethodVerificationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
sendContactMethodVerificationResponse_httpStatus :: Lens.Lens' SendContactMethodVerificationResponse Prelude.Int
sendContactMethodVerificationResponse_httpStatus = Lens.lens (\SendContactMethodVerificationResponse' {httpStatus} -> httpStatus) (\s@SendContactMethodVerificationResponse' {} a -> s {httpStatus = a} :: SendContactMethodVerificationResponse)

instance
  Prelude.NFData
    SendContactMethodVerificationResponse
  where
  rnf SendContactMethodVerificationResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
