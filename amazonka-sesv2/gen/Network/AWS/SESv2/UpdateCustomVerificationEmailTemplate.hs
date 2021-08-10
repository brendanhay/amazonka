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
-- Module      : Network.AWS.SESv2.UpdateCustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing custom verification email template.
--
-- For more information about custom verification email templates, see
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-verify-address-custom.html Using Custom Verification Email Templates>
-- in the /Amazon SES Developer Guide/.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SESv2.UpdateCustomVerificationEmailTemplate
  ( -- * Creating a Request
    UpdateCustomVerificationEmailTemplate (..),
    newUpdateCustomVerificationEmailTemplate,

    -- * Request Lenses
    updateCustomVerificationEmailTemplate_templateName,
    updateCustomVerificationEmailTemplate_fromEmailAddress,
    updateCustomVerificationEmailTemplate_templateSubject,
    updateCustomVerificationEmailTemplate_templateContent,
    updateCustomVerificationEmailTemplate_successRedirectionURL,
    updateCustomVerificationEmailTemplate_failureRedirectionURL,

    -- * Destructuring the Response
    UpdateCustomVerificationEmailTemplateResponse (..),
    newUpdateCustomVerificationEmailTemplateResponse,

    -- * Response Lenses
    updateCustomVerificationEmailTemplateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | Represents a request to update an existing custom verification email
-- template.
--
-- /See:/ 'newUpdateCustomVerificationEmailTemplate' smart constructor.
data UpdateCustomVerificationEmailTemplate = UpdateCustomVerificationEmailTemplate'
  { -- | The name of the custom verification email template that you want to
    -- update.
    templateName :: Prelude.Text,
    -- | The email address that the custom verification email is sent from.
    fromEmailAddress :: Prelude.Text,
    -- | The subject line of the custom verification email.
    templateSubject :: Prelude.Text,
    -- | The content of the custom verification email. The total size of the
    -- email must be less than 10 MB. The message body may contain HTML, with
    -- some limitations. For more information, see
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-verify-address-custom.html#custom-verification-emails-faq Custom Verification Email Frequently Asked Questions>
    -- in the /Amazon SES Developer Guide/.
    templateContent :: Prelude.Text,
    -- | The URL that the recipient of the verification email is sent to if his
    -- or her address is successfully verified.
    successRedirectionURL :: Prelude.Text,
    -- | The URL that the recipient of the verification email is sent to if his
    -- or her address is not successfully verified.
    failureRedirectionURL :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCustomVerificationEmailTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'updateCustomVerificationEmailTemplate_templateName' - The name of the custom verification email template that you want to
-- update.
--
-- 'fromEmailAddress', 'updateCustomVerificationEmailTemplate_fromEmailAddress' - The email address that the custom verification email is sent from.
--
-- 'templateSubject', 'updateCustomVerificationEmailTemplate_templateSubject' - The subject line of the custom verification email.
--
-- 'templateContent', 'updateCustomVerificationEmailTemplate_templateContent' - The content of the custom verification email. The total size of the
-- email must be less than 10 MB. The message body may contain HTML, with
-- some limitations. For more information, see
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-verify-address-custom.html#custom-verification-emails-faq Custom Verification Email Frequently Asked Questions>
-- in the /Amazon SES Developer Guide/.
--
-- 'successRedirectionURL', 'updateCustomVerificationEmailTemplate_successRedirectionURL' - The URL that the recipient of the verification email is sent to if his
-- or her address is successfully verified.
--
-- 'failureRedirectionURL', 'updateCustomVerificationEmailTemplate_failureRedirectionURL' - The URL that the recipient of the verification email is sent to if his
-- or her address is not successfully verified.
newUpdateCustomVerificationEmailTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'fromEmailAddress'
  Prelude.Text ->
  -- | 'templateSubject'
  Prelude.Text ->
  -- | 'templateContent'
  Prelude.Text ->
  -- | 'successRedirectionURL'
  Prelude.Text ->
  -- | 'failureRedirectionURL'
  Prelude.Text ->
  UpdateCustomVerificationEmailTemplate
newUpdateCustomVerificationEmailTemplate
  pTemplateName_
  pFromEmailAddress_
  pTemplateSubject_
  pTemplateContent_
  pSuccessRedirectionURL_
  pFailureRedirectionURL_ =
    UpdateCustomVerificationEmailTemplate'
      { templateName =
          pTemplateName_,
        fromEmailAddress =
          pFromEmailAddress_,
        templateSubject = pTemplateSubject_,
        templateContent = pTemplateContent_,
        successRedirectionURL =
          pSuccessRedirectionURL_,
        failureRedirectionURL =
          pFailureRedirectionURL_
      }

-- | The name of the custom verification email template that you want to
-- update.
updateCustomVerificationEmailTemplate_templateName :: Lens.Lens' UpdateCustomVerificationEmailTemplate Prelude.Text
updateCustomVerificationEmailTemplate_templateName = Lens.lens (\UpdateCustomVerificationEmailTemplate' {templateName} -> templateName) (\s@UpdateCustomVerificationEmailTemplate' {} a -> s {templateName = a} :: UpdateCustomVerificationEmailTemplate)

-- | The email address that the custom verification email is sent from.
updateCustomVerificationEmailTemplate_fromEmailAddress :: Lens.Lens' UpdateCustomVerificationEmailTemplate Prelude.Text
updateCustomVerificationEmailTemplate_fromEmailAddress = Lens.lens (\UpdateCustomVerificationEmailTemplate' {fromEmailAddress} -> fromEmailAddress) (\s@UpdateCustomVerificationEmailTemplate' {} a -> s {fromEmailAddress = a} :: UpdateCustomVerificationEmailTemplate)

-- | The subject line of the custom verification email.
updateCustomVerificationEmailTemplate_templateSubject :: Lens.Lens' UpdateCustomVerificationEmailTemplate Prelude.Text
updateCustomVerificationEmailTemplate_templateSubject = Lens.lens (\UpdateCustomVerificationEmailTemplate' {templateSubject} -> templateSubject) (\s@UpdateCustomVerificationEmailTemplate' {} a -> s {templateSubject = a} :: UpdateCustomVerificationEmailTemplate)

-- | The content of the custom verification email. The total size of the
-- email must be less than 10 MB. The message body may contain HTML, with
-- some limitations. For more information, see
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-email-verify-address-custom.html#custom-verification-emails-faq Custom Verification Email Frequently Asked Questions>
-- in the /Amazon SES Developer Guide/.
updateCustomVerificationEmailTemplate_templateContent :: Lens.Lens' UpdateCustomVerificationEmailTemplate Prelude.Text
updateCustomVerificationEmailTemplate_templateContent = Lens.lens (\UpdateCustomVerificationEmailTemplate' {templateContent} -> templateContent) (\s@UpdateCustomVerificationEmailTemplate' {} a -> s {templateContent = a} :: UpdateCustomVerificationEmailTemplate)

-- | The URL that the recipient of the verification email is sent to if his
-- or her address is successfully verified.
updateCustomVerificationEmailTemplate_successRedirectionURL :: Lens.Lens' UpdateCustomVerificationEmailTemplate Prelude.Text
updateCustomVerificationEmailTemplate_successRedirectionURL = Lens.lens (\UpdateCustomVerificationEmailTemplate' {successRedirectionURL} -> successRedirectionURL) (\s@UpdateCustomVerificationEmailTemplate' {} a -> s {successRedirectionURL = a} :: UpdateCustomVerificationEmailTemplate)

-- | The URL that the recipient of the verification email is sent to if his
-- or her address is not successfully verified.
updateCustomVerificationEmailTemplate_failureRedirectionURL :: Lens.Lens' UpdateCustomVerificationEmailTemplate Prelude.Text
updateCustomVerificationEmailTemplate_failureRedirectionURL = Lens.lens (\UpdateCustomVerificationEmailTemplate' {failureRedirectionURL} -> failureRedirectionURL) (\s@UpdateCustomVerificationEmailTemplate' {} a -> s {failureRedirectionURL = a} :: UpdateCustomVerificationEmailTemplate)

instance
  Core.AWSRequest
    UpdateCustomVerificationEmailTemplate
  where
  type
    AWSResponse
      UpdateCustomVerificationEmailTemplate =
      UpdateCustomVerificationEmailTemplateResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateCustomVerificationEmailTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateCustomVerificationEmailTemplate

instance
  Prelude.NFData
    UpdateCustomVerificationEmailTemplate

instance
  Core.ToHeaders
    UpdateCustomVerificationEmailTemplate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    UpdateCustomVerificationEmailTemplate
  where
  toJSON UpdateCustomVerificationEmailTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("FromEmailAddress" Core..= fromEmailAddress),
            Prelude.Just
              ("TemplateSubject" Core..= templateSubject),
            Prelude.Just
              ("TemplateContent" Core..= templateContent),
            Prelude.Just
              ( "SuccessRedirectionURL"
                  Core..= successRedirectionURL
              ),
            Prelude.Just
              ( "FailureRedirectionURL"
                  Core..= failureRedirectionURL
              )
          ]
      )

instance
  Core.ToPath
    UpdateCustomVerificationEmailTemplate
  where
  toPath UpdateCustomVerificationEmailTemplate' {..} =
    Prelude.mconcat
      [ "/v2/email/custom-verification-email-templates/",
        Core.toBS templateName
      ]

instance
  Core.ToQuery
    UpdateCustomVerificationEmailTemplate
  where
  toQuery = Prelude.const Prelude.mempty

-- | If the action is successful, the service sends back an HTTP 200 response
-- with an empty HTTP body.
--
-- /See:/ 'newUpdateCustomVerificationEmailTemplateResponse' smart constructor.
data UpdateCustomVerificationEmailTemplateResponse = UpdateCustomVerificationEmailTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCustomVerificationEmailTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateCustomVerificationEmailTemplateResponse_httpStatus' - The response's http status code.
newUpdateCustomVerificationEmailTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCustomVerificationEmailTemplateResponse
newUpdateCustomVerificationEmailTemplateResponse
  pHttpStatus_ =
    UpdateCustomVerificationEmailTemplateResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateCustomVerificationEmailTemplateResponse_httpStatus :: Lens.Lens' UpdateCustomVerificationEmailTemplateResponse Prelude.Int
updateCustomVerificationEmailTemplateResponse_httpStatus = Lens.lens (\UpdateCustomVerificationEmailTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdateCustomVerificationEmailTemplateResponse' {} a -> s {httpStatus = a} :: UpdateCustomVerificationEmailTemplateResponse)

instance
  Prelude.NFData
    UpdateCustomVerificationEmailTemplateResponse
