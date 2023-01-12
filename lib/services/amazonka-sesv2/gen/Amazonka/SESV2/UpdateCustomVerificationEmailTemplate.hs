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
-- Module      : Amazonka.SESV2.UpdateCustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing custom verification email template.
--
-- For more information about custom verification email templates, see
-- <https://docs.aws.amazon.com/ses/latest/dg/creating-identities.html#send-email-verify-address-custom Using custom verification email templates>
-- in the /Amazon SES Developer Guide/.
--
-- You can execute this operation no more than once per second.
module Amazonka.SESV2.UpdateCustomVerificationEmailTemplate
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

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
    -- <https://docs.aws.amazon.com/ses/latest/dg/creating-identities.html#send-email-verify-address-custom-faq Custom verification email frequently asked questions>
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
-- <https://docs.aws.amazon.com/ses/latest/dg/creating-identities.html#send-email-verify-address-custom-faq Custom verification email frequently asked questions>
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
-- <https://docs.aws.amazon.com/ses/latest/dg/creating-identities.html#send-email-verify-address-custom-faq Custom verification email frequently asked questions>
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
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateCustomVerificationEmailTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateCustomVerificationEmailTemplate
  where
  hashWithSalt
    _salt
    UpdateCustomVerificationEmailTemplate' {..} =
      _salt `Prelude.hashWithSalt` templateName
        `Prelude.hashWithSalt` fromEmailAddress
        `Prelude.hashWithSalt` templateSubject
        `Prelude.hashWithSalt` templateContent
        `Prelude.hashWithSalt` successRedirectionURL
        `Prelude.hashWithSalt` failureRedirectionURL

instance
  Prelude.NFData
    UpdateCustomVerificationEmailTemplate
  where
  rnf UpdateCustomVerificationEmailTemplate' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf fromEmailAddress
      `Prelude.seq` Prelude.rnf templateSubject
      `Prelude.seq` Prelude.rnf templateContent
      `Prelude.seq` Prelude.rnf successRedirectionURL
      `Prelude.seq` Prelude.rnf failureRedirectionURL

instance
  Data.ToHeaders
    UpdateCustomVerificationEmailTemplate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdateCustomVerificationEmailTemplate
  where
  toJSON UpdateCustomVerificationEmailTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("FromEmailAddress" Data..= fromEmailAddress),
            Prelude.Just
              ("TemplateSubject" Data..= templateSubject),
            Prelude.Just
              ("TemplateContent" Data..= templateContent),
            Prelude.Just
              ( "SuccessRedirectionURL"
                  Data..= successRedirectionURL
              ),
            Prelude.Just
              ( "FailureRedirectionURL"
                  Data..= failureRedirectionURL
              )
          ]
      )

instance
  Data.ToPath
    UpdateCustomVerificationEmailTemplate
  where
  toPath UpdateCustomVerificationEmailTemplate' {..} =
    Prelude.mconcat
      [ "/v2/email/custom-verification-email-templates/",
        Data.toBS templateName
      ]

instance
  Data.ToQuery
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
  where
  rnf
    UpdateCustomVerificationEmailTemplateResponse' {..} =
      Prelude.rnf httpStatus
