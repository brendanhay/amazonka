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
-- Module      : Network.AWS.SES.SendCustomVerificationEmail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an email address to the list of identities for your Amazon SES
-- account in the current AWS Region and attempts to verify it. As a result
-- of executing this operation, a customized verification email is sent to
-- the specified address.
--
-- To use this operation, you must first create a custom verification email
-- template. For more information about creating and using custom
-- verification email templates, see
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates>
-- in the /Amazon SES Developer Guide/.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.SendCustomVerificationEmail
  ( -- * Creating a Request
    SendCustomVerificationEmail (..),
    newSendCustomVerificationEmail,

    -- * Request Lenses
    sendCustomVerificationEmail_configurationSetName,
    sendCustomVerificationEmail_emailAddress,
    sendCustomVerificationEmail_templateName,

    -- * Destructuring the Response
    SendCustomVerificationEmailResponse (..),
    newSendCustomVerificationEmailResponse,

    -- * Response Lenses
    sendCustomVerificationEmailResponse_messageId,
    sendCustomVerificationEmailResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to send a custom verification email to a specified
-- recipient.
--
-- /See:/ 'newSendCustomVerificationEmail' smart constructor.
data SendCustomVerificationEmail = SendCustomVerificationEmail'
  { -- | Name of a configuration set to use when sending the verification email.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | The email address to verify.
    emailAddress :: Prelude.Text,
    -- | The name of the custom verification email template to use when sending
    -- the verification email.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SendCustomVerificationEmail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'sendCustomVerificationEmail_configurationSetName' - Name of a configuration set to use when sending the verification email.
--
-- 'emailAddress', 'sendCustomVerificationEmail_emailAddress' - The email address to verify.
--
-- 'templateName', 'sendCustomVerificationEmail_templateName' - The name of the custom verification email template to use when sending
-- the verification email.
newSendCustomVerificationEmail ::
  -- | 'emailAddress'
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
  SendCustomVerificationEmail
newSendCustomVerificationEmail
  pEmailAddress_
  pTemplateName_ =
    SendCustomVerificationEmail'
      { configurationSetName =
          Prelude.Nothing,
        emailAddress = pEmailAddress_,
        templateName = pTemplateName_
      }

-- | Name of a configuration set to use when sending the verification email.
sendCustomVerificationEmail_configurationSetName :: Lens.Lens' SendCustomVerificationEmail (Prelude.Maybe Prelude.Text)
sendCustomVerificationEmail_configurationSetName = Lens.lens (\SendCustomVerificationEmail' {configurationSetName} -> configurationSetName) (\s@SendCustomVerificationEmail' {} a -> s {configurationSetName = a} :: SendCustomVerificationEmail)

-- | The email address to verify.
sendCustomVerificationEmail_emailAddress :: Lens.Lens' SendCustomVerificationEmail Prelude.Text
sendCustomVerificationEmail_emailAddress = Lens.lens (\SendCustomVerificationEmail' {emailAddress} -> emailAddress) (\s@SendCustomVerificationEmail' {} a -> s {emailAddress = a} :: SendCustomVerificationEmail)

-- | The name of the custom verification email template to use when sending
-- the verification email.
sendCustomVerificationEmail_templateName :: Lens.Lens' SendCustomVerificationEmail Prelude.Text
sendCustomVerificationEmail_templateName = Lens.lens (\SendCustomVerificationEmail' {templateName} -> templateName) (\s@SendCustomVerificationEmail' {} a -> s {templateName = a} :: SendCustomVerificationEmail)

instance
  Prelude.AWSRequest
    SendCustomVerificationEmail
  where
  type
    Rs SendCustomVerificationEmail =
      SendCustomVerificationEmailResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SendCustomVerificationEmailResult"
      ( \s h x ->
          SendCustomVerificationEmailResponse'
            Prelude.<$> (x Prelude..@? "MessageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendCustomVerificationEmail

instance Prelude.NFData SendCustomVerificationEmail

instance
  Prelude.ToHeaders
    SendCustomVerificationEmail
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath SendCustomVerificationEmail where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SendCustomVerificationEmail where
  toQuery SendCustomVerificationEmail' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "SendCustomVerificationEmail" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "ConfigurationSetName"
          Prelude.=: configurationSetName,
        "EmailAddress" Prelude.=: emailAddress,
        "TemplateName" Prelude.=: templateName
      ]

-- | The response received when attempting to send the custom verification
-- email.
--
-- /See:/ 'newSendCustomVerificationEmailResponse' smart constructor.
data SendCustomVerificationEmailResponse = SendCustomVerificationEmailResponse'
  { -- | The unique message identifier returned from the
    -- @SendCustomVerificationEmail@ operation.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SendCustomVerificationEmailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageId', 'sendCustomVerificationEmailResponse_messageId' - The unique message identifier returned from the
-- @SendCustomVerificationEmail@ operation.
--
-- 'httpStatus', 'sendCustomVerificationEmailResponse_httpStatus' - The response's http status code.
newSendCustomVerificationEmailResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendCustomVerificationEmailResponse
newSendCustomVerificationEmailResponse pHttpStatus_ =
  SendCustomVerificationEmailResponse'
    { messageId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique message identifier returned from the
-- @SendCustomVerificationEmail@ operation.
sendCustomVerificationEmailResponse_messageId :: Lens.Lens' SendCustomVerificationEmailResponse (Prelude.Maybe Prelude.Text)
sendCustomVerificationEmailResponse_messageId = Lens.lens (\SendCustomVerificationEmailResponse' {messageId} -> messageId) (\s@SendCustomVerificationEmailResponse' {} a -> s {messageId = a} :: SendCustomVerificationEmailResponse)

-- | The response's http status code.
sendCustomVerificationEmailResponse_httpStatus :: Lens.Lens' SendCustomVerificationEmailResponse Prelude.Int
sendCustomVerificationEmailResponse_httpStatus = Lens.lens (\SendCustomVerificationEmailResponse' {httpStatus} -> httpStatus) (\s@SendCustomVerificationEmailResponse' {} a -> s {httpStatus = a} :: SendCustomVerificationEmailResponse)

instance
  Prelude.NFData
    SendCustomVerificationEmailResponse
