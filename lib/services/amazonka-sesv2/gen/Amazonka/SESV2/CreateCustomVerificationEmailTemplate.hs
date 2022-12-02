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
-- Module      : Amazonka.SESV2.CreateCustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom verification email template.
--
-- For more information about custom verification email templates, see
-- <https://docs.aws.amazon.com/ses/latest/dg/creating-identities.html#send-email-verify-address-custom Using custom verification email templates>
-- in the /Amazon SES Developer Guide/.
--
-- You can execute this operation no more than once per second.
module Amazonka.SESV2.CreateCustomVerificationEmailTemplate
  ( -- * Creating a Request
    CreateCustomVerificationEmailTemplate (..),
    newCreateCustomVerificationEmailTemplate,

    -- * Request Lenses
    createCustomVerificationEmailTemplate_templateName,
    createCustomVerificationEmailTemplate_fromEmailAddress,
    createCustomVerificationEmailTemplate_templateSubject,
    createCustomVerificationEmailTemplate_templateContent,
    createCustomVerificationEmailTemplate_successRedirectionURL,
    createCustomVerificationEmailTemplate_failureRedirectionURL,

    -- * Destructuring the Response
    CreateCustomVerificationEmailTemplateResponse (..),
    newCreateCustomVerificationEmailTemplateResponse,

    -- * Response Lenses
    createCustomVerificationEmailTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | Represents a request to create a custom verification email template.
--
-- /See:/ 'newCreateCustomVerificationEmailTemplate' smart constructor.
data CreateCustomVerificationEmailTemplate = CreateCustomVerificationEmailTemplate'
  { -- | The name of the custom verification email template.
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
-- Create a value of 'CreateCustomVerificationEmailTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'createCustomVerificationEmailTemplate_templateName' - The name of the custom verification email template.
--
-- 'fromEmailAddress', 'createCustomVerificationEmailTemplate_fromEmailAddress' - The email address that the custom verification email is sent from.
--
-- 'templateSubject', 'createCustomVerificationEmailTemplate_templateSubject' - The subject line of the custom verification email.
--
-- 'templateContent', 'createCustomVerificationEmailTemplate_templateContent' - The content of the custom verification email. The total size of the
-- email must be less than 10 MB. The message body may contain HTML, with
-- some limitations. For more information, see
-- <https://docs.aws.amazon.com/ses/latest/dg/creating-identities.html#send-email-verify-address-custom-faq Custom verification email frequently asked questions>
-- in the /Amazon SES Developer Guide/.
--
-- 'successRedirectionURL', 'createCustomVerificationEmailTemplate_successRedirectionURL' - The URL that the recipient of the verification email is sent to if his
-- or her address is successfully verified.
--
-- 'failureRedirectionURL', 'createCustomVerificationEmailTemplate_failureRedirectionURL' - The URL that the recipient of the verification email is sent to if his
-- or her address is not successfully verified.
newCreateCustomVerificationEmailTemplate ::
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
  CreateCustomVerificationEmailTemplate
newCreateCustomVerificationEmailTemplate
  pTemplateName_
  pFromEmailAddress_
  pTemplateSubject_
  pTemplateContent_
  pSuccessRedirectionURL_
  pFailureRedirectionURL_ =
    CreateCustomVerificationEmailTemplate'
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

-- | The name of the custom verification email template.
createCustomVerificationEmailTemplate_templateName :: Lens.Lens' CreateCustomVerificationEmailTemplate Prelude.Text
createCustomVerificationEmailTemplate_templateName = Lens.lens (\CreateCustomVerificationEmailTemplate' {templateName} -> templateName) (\s@CreateCustomVerificationEmailTemplate' {} a -> s {templateName = a} :: CreateCustomVerificationEmailTemplate)

-- | The email address that the custom verification email is sent from.
createCustomVerificationEmailTemplate_fromEmailAddress :: Lens.Lens' CreateCustomVerificationEmailTemplate Prelude.Text
createCustomVerificationEmailTemplate_fromEmailAddress = Lens.lens (\CreateCustomVerificationEmailTemplate' {fromEmailAddress} -> fromEmailAddress) (\s@CreateCustomVerificationEmailTemplate' {} a -> s {fromEmailAddress = a} :: CreateCustomVerificationEmailTemplate)

-- | The subject line of the custom verification email.
createCustomVerificationEmailTemplate_templateSubject :: Lens.Lens' CreateCustomVerificationEmailTemplate Prelude.Text
createCustomVerificationEmailTemplate_templateSubject = Lens.lens (\CreateCustomVerificationEmailTemplate' {templateSubject} -> templateSubject) (\s@CreateCustomVerificationEmailTemplate' {} a -> s {templateSubject = a} :: CreateCustomVerificationEmailTemplate)

-- | The content of the custom verification email. The total size of the
-- email must be less than 10 MB. The message body may contain HTML, with
-- some limitations. For more information, see
-- <https://docs.aws.amazon.com/ses/latest/dg/creating-identities.html#send-email-verify-address-custom-faq Custom verification email frequently asked questions>
-- in the /Amazon SES Developer Guide/.
createCustomVerificationEmailTemplate_templateContent :: Lens.Lens' CreateCustomVerificationEmailTemplate Prelude.Text
createCustomVerificationEmailTemplate_templateContent = Lens.lens (\CreateCustomVerificationEmailTemplate' {templateContent} -> templateContent) (\s@CreateCustomVerificationEmailTemplate' {} a -> s {templateContent = a} :: CreateCustomVerificationEmailTemplate)

-- | The URL that the recipient of the verification email is sent to if his
-- or her address is successfully verified.
createCustomVerificationEmailTemplate_successRedirectionURL :: Lens.Lens' CreateCustomVerificationEmailTemplate Prelude.Text
createCustomVerificationEmailTemplate_successRedirectionURL = Lens.lens (\CreateCustomVerificationEmailTemplate' {successRedirectionURL} -> successRedirectionURL) (\s@CreateCustomVerificationEmailTemplate' {} a -> s {successRedirectionURL = a} :: CreateCustomVerificationEmailTemplate)

-- | The URL that the recipient of the verification email is sent to if his
-- or her address is not successfully verified.
createCustomVerificationEmailTemplate_failureRedirectionURL :: Lens.Lens' CreateCustomVerificationEmailTemplate Prelude.Text
createCustomVerificationEmailTemplate_failureRedirectionURL = Lens.lens (\CreateCustomVerificationEmailTemplate' {failureRedirectionURL} -> failureRedirectionURL) (\s@CreateCustomVerificationEmailTemplate' {} a -> s {failureRedirectionURL = a} :: CreateCustomVerificationEmailTemplate)

instance
  Core.AWSRequest
    CreateCustomVerificationEmailTemplate
  where
  type
    AWSResponse
      CreateCustomVerificationEmailTemplate =
      CreateCustomVerificationEmailTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateCustomVerificationEmailTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateCustomVerificationEmailTemplate
  where
  hashWithSalt
    _salt
    CreateCustomVerificationEmailTemplate' {..} =
      _salt `Prelude.hashWithSalt` templateName
        `Prelude.hashWithSalt` fromEmailAddress
        `Prelude.hashWithSalt` templateSubject
        `Prelude.hashWithSalt` templateContent
        `Prelude.hashWithSalt` successRedirectionURL
        `Prelude.hashWithSalt` failureRedirectionURL

instance
  Prelude.NFData
    CreateCustomVerificationEmailTemplate
  where
  rnf CreateCustomVerificationEmailTemplate' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf fromEmailAddress
      `Prelude.seq` Prelude.rnf templateSubject
      `Prelude.seq` Prelude.rnf templateContent
      `Prelude.seq` Prelude.rnf successRedirectionURL
      `Prelude.seq` Prelude.rnf failureRedirectionURL

instance
  Data.ToHeaders
    CreateCustomVerificationEmailTemplate
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
    CreateCustomVerificationEmailTemplate
  where
  toJSON CreateCustomVerificationEmailTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TemplateName" Data..= templateName),
            Prelude.Just
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
    CreateCustomVerificationEmailTemplate
  where
  toPath =
    Prelude.const
      "/v2/email/custom-verification-email-templates"

instance
  Data.ToQuery
    CreateCustomVerificationEmailTemplate
  where
  toQuery = Prelude.const Prelude.mempty

-- | If the action is successful, the service sends back an HTTP 200 response
-- with an empty HTTP body.
--
-- /See:/ 'newCreateCustomVerificationEmailTemplateResponse' smart constructor.
data CreateCustomVerificationEmailTemplateResponse = CreateCustomVerificationEmailTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomVerificationEmailTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createCustomVerificationEmailTemplateResponse_httpStatus' - The response's http status code.
newCreateCustomVerificationEmailTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCustomVerificationEmailTemplateResponse
newCreateCustomVerificationEmailTemplateResponse
  pHttpStatus_ =
    CreateCustomVerificationEmailTemplateResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
createCustomVerificationEmailTemplateResponse_httpStatus :: Lens.Lens' CreateCustomVerificationEmailTemplateResponse Prelude.Int
createCustomVerificationEmailTemplateResponse_httpStatus = Lens.lens (\CreateCustomVerificationEmailTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateCustomVerificationEmailTemplateResponse' {} a -> s {httpStatus = a} :: CreateCustomVerificationEmailTemplateResponse)

instance
  Prelude.NFData
    CreateCustomVerificationEmailTemplateResponse
  where
  rnf
    CreateCustomVerificationEmailTemplateResponse' {..} =
      Prelude.rnf httpStatus
