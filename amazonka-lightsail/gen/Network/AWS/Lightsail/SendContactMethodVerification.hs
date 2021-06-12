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
-- Module      : Network.AWS.Lightsail.SendContactMethodVerification
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- number contact method in each AWS Region. However, SMS text messaging is
-- not supported in some AWS Regions, and SMS text messages cannot be sent
-- to some countries\/regions. For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail>.
--
-- A verification request is sent to the contact method when you initially
-- create it. Use this action to send another verification request if a
-- previous verification request was deleted, or has expired.
--
-- Notifications are not sent to an email contact method until after it is
-- verified, and confirmed as valid.
module Network.AWS.Lightsail.SendContactMethodVerification
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSendContactMethodVerification' smart constructor.
data SendContactMethodVerification = SendContactMethodVerification'
  { -- | The protocol to verify, such as @Email@ or @SMS@ (text messaging).
    protocol :: ContactMethodVerificationProtocol
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SendContactMethodVerificationResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SendContactMethodVerification

instance Core.NFData SendContactMethodVerification

instance Core.ToHeaders SendContactMethodVerification where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.SendContactMethodVerification" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SendContactMethodVerification where
  toJSON SendContactMethodVerification' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("protocol" Core..= protocol)]
      )

instance Core.ToPath SendContactMethodVerification where
  toPath = Core.const "/"

instance Core.ToQuery SendContactMethodVerification where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSendContactMethodVerificationResponse' smart constructor.
data SendContactMethodVerificationResponse = SendContactMethodVerificationResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  SendContactMethodVerificationResponse
newSendContactMethodVerificationResponse pHttpStatus_ =
  SendContactMethodVerificationResponse'
    { operations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
sendContactMethodVerificationResponse_operations :: Lens.Lens' SendContactMethodVerificationResponse (Core.Maybe [Operation])
sendContactMethodVerificationResponse_operations = Lens.lens (\SendContactMethodVerificationResponse' {operations} -> operations) (\s@SendContactMethodVerificationResponse' {} a -> s {operations = a} :: SendContactMethodVerificationResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
sendContactMethodVerificationResponse_httpStatus :: Lens.Lens' SendContactMethodVerificationResponse Core.Int
sendContactMethodVerificationResponse_httpStatus = Lens.lens (\SendContactMethodVerificationResponse' {httpStatus} -> httpStatus) (\s@SendContactMethodVerificationResponse' {} a -> s {httpStatus = a} :: SendContactMethodVerificationResponse)

instance
  Core.NFData
    SendContactMethodVerificationResponse
