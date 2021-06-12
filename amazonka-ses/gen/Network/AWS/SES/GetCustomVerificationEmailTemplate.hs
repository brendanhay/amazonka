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
-- Module      : Network.AWS.SES.GetCustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the custom email verification template for the template name you
-- specify.
--
-- For more information about custom verification email templates, see
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates>
-- in the /Amazon SES Developer Guide/.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.GetCustomVerificationEmailTemplate
  ( -- * Creating a Request
    GetCustomVerificationEmailTemplate (..),
    newGetCustomVerificationEmailTemplate,

    -- * Request Lenses
    getCustomVerificationEmailTemplate_templateName,

    -- * Destructuring the Response
    GetCustomVerificationEmailTemplateResponse (..),
    newGetCustomVerificationEmailTemplateResponse,

    -- * Response Lenses
    getCustomVerificationEmailTemplateResponse_templateName,
    getCustomVerificationEmailTemplateResponse_templateSubject,
    getCustomVerificationEmailTemplateResponse_fromEmailAddress,
    getCustomVerificationEmailTemplateResponse_templateContent,
    getCustomVerificationEmailTemplateResponse_successRedirectionURL,
    getCustomVerificationEmailTemplateResponse_failureRedirectionURL,
    getCustomVerificationEmailTemplateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to retrieve an existing custom verification email
-- template.
--
-- /See:/ 'newGetCustomVerificationEmailTemplate' smart constructor.
data GetCustomVerificationEmailTemplate = GetCustomVerificationEmailTemplate'
  { -- | The name of the custom verification email template that you want to
    -- retrieve.
    templateName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCustomVerificationEmailTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'getCustomVerificationEmailTemplate_templateName' - The name of the custom verification email template that you want to
-- retrieve.
newGetCustomVerificationEmailTemplate ::
  -- | 'templateName'
  Core.Text ->
  GetCustomVerificationEmailTemplate
newGetCustomVerificationEmailTemplate pTemplateName_ =
  GetCustomVerificationEmailTemplate'
    { templateName =
        pTemplateName_
    }

-- | The name of the custom verification email template that you want to
-- retrieve.
getCustomVerificationEmailTemplate_templateName :: Lens.Lens' GetCustomVerificationEmailTemplate Core.Text
getCustomVerificationEmailTemplate_templateName = Lens.lens (\GetCustomVerificationEmailTemplate' {templateName} -> templateName) (\s@GetCustomVerificationEmailTemplate' {} a -> s {templateName = a} :: GetCustomVerificationEmailTemplate)

instance
  Core.AWSRequest
    GetCustomVerificationEmailTemplate
  where
  type
    AWSResponse GetCustomVerificationEmailTemplate =
      GetCustomVerificationEmailTemplateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetCustomVerificationEmailTemplateResult"
      ( \s h x ->
          GetCustomVerificationEmailTemplateResponse'
            Core.<$> (x Core..@? "TemplateName")
            Core.<*> (x Core..@? "TemplateSubject")
            Core.<*> (x Core..@? "FromEmailAddress")
            Core.<*> (x Core..@? "TemplateContent")
            Core.<*> (x Core..@? "SuccessRedirectionURL")
            Core.<*> (x Core..@? "FailureRedirectionURL")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetCustomVerificationEmailTemplate

instance
  Core.NFData
    GetCustomVerificationEmailTemplate

instance
  Core.ToHeaders
    GetCustomVerificationEmailTemplate
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    GetCustomVerificationEmailTemplate
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetCustomVerificationEmailTemplate
  where
  toQuery GetCustomVerificationEmailTemplate' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "GetCustomVerificationEmailTemplate" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "TemplateName" Core.=: templateName
      ]

-- | The content of the custom verification email template.
--
-- /See:/ 'newGetCustomVerificationEmailTemplateResponse' smart constructor.
data GetCustomVerificationEmailTemplateResponse = GetCustomVerificationEmailTemplateResponse'
  { -- | The name of the custom verification email template.
    templateName :: Core.Maybe Core.Text,
    -- | The subject line of the custom verification email.
    templateSubject :: Core.Maybe Core.Text,
    -- | The email address that the custom verification email is sent from.
    fromEmailAddress :: Core.Maybe Core.Text,
    -- | The content of the custom verification email.
    templateContent :: Core.Maybe Core.Text,
    -- | The URL that the recipient of the verification email is sent to if his
    -- or her address is successfully verified.
    successRedirectionURL :: Core.Maybe Core.Text,
    -- | The URL that the recipient of the verification email is sent to if his
    -- or her address is not successfully verified.
    failureRedirectionURL :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCustomVerificationEmailTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'getCustomVerificationEmailTemplateResponse_templateName' - The name of the custom verification email template.
--
-- 'templateSubject', 'getCustomVerificationEmailTemplateResponse_templateSubject' - The subject line of the custom verification email.
--
-- 'fromEmailAddress', 'getCustomVerificationEmailTemplateResponse_fromEmailAddress' - The email address that the custom verification email is sent from.
--
-- 'templateContent', 'getCustomVerificationEmailTemplateResponse_templateContent' - The content of the custom verification email.
--
-- 'successRedirectionURL', 'getCustomVerificationEmailTemplateResponse_successRedirectionURL' - The URL that the recipient of the verification email is sent to if his
-- or her address is successfully verified.
--
-- 'failureRedirectionURL', 'getCustomVerificationEmailTemplateResponse_failureRedirectionURL' - The URL that the recipient of the verification email is sent to if his
-- or her address is not successfully verified.
--
-- 'httpStatus', 'getCustomVerificationEmailTemplateResponse_httpStatus' - The response's http status code.
newGetCustomVerificationEmailTemplateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetCustomVerificationEmailTemplateResponse
newGetCustomVerificationEmailTemplateResponse
  pHttpStatus_ =
    GetCustomVerificationEmailTemplateResponse'
      { templateName =
          Core.Nothing,
        templateSubject = Core.Nothing,
        fromEmailAddress = Core.Nothing,
        templateContent = Core.Nothing,
        successRedirectionURL =
          Core.Nothing,
        failureRedirectionURL =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The name of the custom verification email template.
getCustomVerificationEmailTemplateResponse_templateName :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Core.Maybe Core.Text)
getCustomVerificationEmailTemplateResponse_templateName = Lens.lens (\GetCustomVerificationEmailTemplateResponse' {templateName} -> templateName) (\s@GetCustomVerificationEmailTemplateResponse' {} a -> s {templateName = a} :: GetCustomVerificationEmailTemplateResponse)

-- | The subject line of the custom verification email.
getCustomVerificationEmailTemplateResponse_templateSubject :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Core.Maybe Core.Text)
getCustomVerificationEmailTemplateResponse_templateSubject = Lens.lens (\GetCustomVerificationEmailTemplateResponse' {templateSubject} -> templateSubject) (\s@GetCustomVerificationEmailTemplateResponse' {} a -> s {templateSubject = a} :: GetCustomVerificationEmailTemplateResponse)

-- | The email address that the custom verification email is sent from.
getCustomVerificationEmailTemplateResponse_fromEmailAddress :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Core.Maybe Core.Text)
getCustomVerificationEmailTemplateResponse_fromEmailAddress = Lens.lens (\GetCustomVerificationEmailTemplateResponse' {fromEmailAddress} -> fromEmailAddress) (\s@GetCustomVerificationEmailTemplateResponse' {} a -> s {fromEmailAddress = a} :: GetCustomVerificationEmailTemplateResponse)

-- | The content of the custom verification email.
getCustomVerificationEmailTemplateResponse_templateContent :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Core.Maybe Core.Text)
getCustomVerificationEmailTemplateResponse_templateContent = Lens.lens (\GetCustomVerificationEmailTemplateResponse' {templateContent} -> templateContent) (\s@GetCustomVerificationEmailTemplateResponse' {} a -> s {templateContent = a} :: GetCustomVerificationEmailTemplateResponse)

-- | The URL that the recipient of the verification email is sent to if his
-- or her address is successfully verified.
getCustomVerificationEmailTemplateResponse_successRedirectionURL :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Core.Maybe Core.Text)
getCustomVerificationEmailTemplateResponse_successRedirectionURL = Lens.lens (\GetCustomVerificationEmailTemplateResponse' {successRedirectionURL} -> successRedirectionURL) (\s@GetCustomVerificationEmailTemplateResponse' {} a -> s {successRedirectionURL = a} :: GetCustomVerificationEmailTemplateResponse)

-- | The URL that the recipient of the verification email is sent to if his
-- or her address is not successfully verified.
getCustomVerificationEmailTemplateResponse_failureRedirectionURL :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Core.Maybe Core.Text)
getCustomVerificationEmailTemplateResponse_failureRedirectionURL = Lens.lens (\GetCustomVerificationEmailTemplateResponse' {failureRedirectionURL} -> failureRedirectionURL) (\s@GetCustomVerificationEmailTemplateResponse' {} a -> s {failureRedirectionURL = a} :: GetCustomVerificationEmailTemplateResponse)

-- | The response's http status code.
getCustomVerificationEmailTemplateResponse_httpStatus :: Lens.Lens' GetCustomVerificationEmailTemplateResponse Core.Int
getCustomVerificationEmailTemplateResponse_httpStatus = Lens.lens (\GetCustomVerificationEmailTemplateResponse' {httpStatus} -> httpStatus) (\s@GetCustomVerificationEmailTemplateResponse' {} a -> s {httpStatus = a} :: GetCustomVerificationEmailTemplateResponse)

instance
  Core.NFData
    GetCustomVerificationEmailTemplateResponse
