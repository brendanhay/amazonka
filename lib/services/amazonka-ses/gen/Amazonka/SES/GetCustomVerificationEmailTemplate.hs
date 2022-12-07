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
-- Module      : Amazonka.SES.GetCustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.SES.GetCustomVerificationEmailTemplate
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
    getCustomVerificationEmailTemplateResponse_successRedirectionURL,
    getCustomVerificationEmailTemplateResponse_fromEmailAddress,
    getCustomVerificationEmailTemplateResponse_templateContent,
    getCustomVerificationEmailTemplateResponse_templateSubject,
    getCustomVerificationEmailTemplateResponse_failureRedirectionURL,
    getCustomVerificationEmailTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to retrieve an existing custom verification email
-- template.
--
-- /See:/ 'newGetCustomVerificationEmailTemplate' smart constructor.
data GetCustomVerificationEmailTemplate = GetCustomVerificationEmailTemplate'
  { -- | The name of the custom verification email template that you want to
    -- retrieve.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetCustomVerificationEmailTemplate
newGetCustomVerificationEmailTemplate pTemplateName_ =
  GetCustomVerificationEmailTemplate'
    { templateName =
        pTemplateName_
    }

-- | The name of the custom verification email template that you want to
-- retrieve.
getCustomVerificationEmailTemplate_templateName :: Lens.Lens' GetCustomVerificationEmailTemplate Prelude.Text
getCustomVerificationEmailTemplate_templateName = Lens.lens (\GetCustomVerificationEmailTemplate' {templateName} -> templateName) (\s@GetCustomVerificationEmailTemplate' {} a -> s {templateName = a} :: GetCustomVerificationEmailTemplate)

instance
  Core.AWSRequest
    GetCustomVerificationEmailTemplate
  where
  type
    AWSResponse GetCustomVerificationEmailTemplate =
      GetCustomVerificationEmailTemplateResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetCustomVerificationEmailTemplateResult"
      ( \s h x ->
          GetCustomVerificationEmailTemplateResponse'
            Prelude.<$> (x Data..@? "TemplateName")
              Prelude.<*> (x Data..@? "SuccessRedirectionURL")
              Prelude.<*> (x Data..@? "FromEmailAddress")
              Prelude.<*> (x Data..@? "TemplateContent")
              Prelude.<*> (x Data..@? "TemplateSubject")
              Prelude.<*> (x Data..@? "FailureRedirectionURL")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetCustomVerificationEmailTemplate
  where
  hashWithSalt
    _salt
    GetCustomVerificationEmailTemplate' {..} =
      _salt `Prelude.hashWithSalt` templateName

instance
  Prelude.NFData
    GetCustomVerificationEmailTemplate
  where
  rnf GetCustomVerificationEmailTemplate' {..} =
    Prelude.rnf templateName

instance
  Data.ToHeaders
    GetCustomVerificationEmailTemplate
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetCustomVerificationEmailTemplate
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetCustomVerificationEmailTemplate
  where
  toQuery GetCustomVerificationEmailTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetCustomVerificationEmailTemplate" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "TemplateName" Data.=: templateName
      ]

-- | The content of the custom verification email template.
--
-- /See:/ 'newGetCustomVerificationEmailTemplateResponse' smart constructor.
data GetCustomVerificationEmailTemplateResponse = GetCustomVerificationEmailTemplateResponse'
  { -- | The name of the custom verification email template.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The URL that the recipient of the verification email is sent to if his
    -- or her address is successfully verified.
    successRedirectionURL :: Prelude.Maybe Prelude.Text,
    -- | The email address that the custom verification email is sent from.
    fromEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | The content of the custom verification email.
    templateContent :: Prelude.Maybe Prelude.Text,
    -- | The subject line of the custom verification email.
    templateSubject :: Prelude.Maybe Prelude.Text,
    -- | The URL that the recipient of the verification email is sent to if his
    -- or her address is not successfully verified.
    failureRedirectionURL :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'successRedirectionURL', 'getCustomVerificationEmailTemplateResponse_successRedirectionURL' - The URL that the recipient of the verification email is sent to if his
-- or her address is successfully verified.
--
-- 'fromEmailAddress', 'getCustomVerificationEmailTemplateResponse_fromEmailAddress' - The email address that the custom verification email is sent from.
--
-- 'templateContent', 'getCustomVerificationEmailTemplateResponse_templateContent' - The content of the custom verification email.
--
-- 'templateSubject', 'getCustomVerificationEmailTemplateResponse_templateSubject' - The subject line of the custom verification email.
--
-- 'failureRedirectionURL', 'getCustomVerificationEmailTemplateResponse_failureRedirectionURL' - The URL that the recipient of the verification email is sent to if his
-- or her address is not successfully verified.
--
-- 'httpStatus', 'getCustomVerificationEmailTemplateResponse_httpStatus' - The response's http status code.
newGetCustomVerificationEmailTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCustomVerificationEmailTemplateResponse
newGetCustomVerificationEmailTemplateResponse
  pHttpStatus_ =
    GetCustomVerificationEmailTemplateResponse'
      { templateName =
          Prelude.Nothing,
        successRedirectionURL =
          Prelude.Nothing,
        fromEmailAddress =
          Prelude.Nothing,
        templateContent =
          Prelude.Nothing,
        templateSubject =
          Prelude.Nothing,
        failureRedirectionURL =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The name of the custom verification email template.
getCustomVerificationEmailTemplateResponse_templateName :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Prelude.Maybe Prelude.Text)
getCustomVerificationEmailTemplateResponse_templateName = Lens.lens (\GetCustomVerificationEmailTemplateResponse' {templateName} -> templateName) (\s@GetCustomVerificationEmailTemplateResponse' {} a -> s {templateName = a} :: GetCustomVerificationEmailTemplateResponse)

-- | The URL that the recipient of the verification email is sent to if his
-- or her address is successfully verified.
getCustomVerificationEmailTemplateResponse_successRedirectionURL :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Prelude.Maybe Prelude.Text)
getCustomVerificationEmailTemplateResponse_successRedirectionURL = Lens.lens (\GetCustomVerificationEmailTemplateResponse' {successRedirectionURL} -> successRedirectionURL) (\s@GetCustomVerificationEmailTemplateResponse' {} a -> s {successRedirectionURL = a} :: GetCustomVerificationEmailTemplateResponse)

-- | The email address that the custom verification email is sent from.
getCustomVerificationEmailTemplateResponse_fromEmailAddress :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Prelude.Maybe Prelude.Text)
getCustomVerificationEmailTemplateResponse_fromEmailAddress = Lens.lens (\GetCustomVerificationEmailTemplateResponse' {fromEmailAddress} -> fromEmailAddress) (\s@GetCustomVerificationEmailTemplateResponse' {} a -> s {fromEmailAddress = a} :: GetCustomVerificationEmailTemplateResponse)

-- | The content of the custom verification email.
getCustomVerificationEmailTemplateResponse_templateContent :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Prelude.Maybe Prelude.Text)
getCustomVerificationEmailTemplateResponse_templateContent = Lens.lens (\GetCustomVerificationEmailTemplateResponse' {templateContent} -> templateContent) (\s@GetCustomVerificationEmailTemplateResponse' {} a -> s {templateContent = a} :: GetCustomVerificationEmailTemplateResponse)

-- | The subject line of the custom verification email.
getCustomVerificationEmailTemplateResponse_templateSubject :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Prelude.Maybe Prelude.Text)
getCustomVerificationEmailTemplateResponse_templateSubject = Lens.lens (\GetCustomVerificationEmailTemplateResponse' {templateSubject} -> templateSubject) (\s@GetCustomVerificationEmailTemplateResponse' {} a -> s {templateSubject = a} :: GetCustomVerificationEmailTemplateResponse)

-- | The URL that the recipient of the verification email is sent to if his
-- or her address is not successfully verified.
getCustomVerificationEmailTemplateResponse_failureRedirectionURL :: Lens.Lens' GetCustomVerificationEmailTemplateResponse (Prelude.Maybe Prelude.Text)
getCustomVerificationEmailTemplateResponse_failureRedirectionURL = Lens.lens (\GetCustomVerificationEmailTemplateResponse' {failureRedirectionURL} -> failureRedirectionURL) (\s@GetCustomVerificationEmailTemplateResponse' {} a -> s {failureRedirectionURL = a} :: GetCustomVerificationEmailTemplateResponse)

-- | The response's http status code.
getCustomVerificationEmailTemplateResponse_httpStatus :: Lens.Lens' GetCustomVerificationEmailTemplateResponse Prelude.Int
getCustomVerificationEmailTemplateResponse_httpStatus = Lens.lens (\GetCustomVerificationEmailTemplateResponse' {httpStatus} -> httpStatus) (\s@GetCustomVerificationEmailTemplateResponse' {} a -> s {httpStatus = a} :: GetCustomVerificationEmailTemplateResponse)

instance
  Prelude.NFData
    GetCustomVerificationEmailTemplateResponse
  where
  rnf GetCustomVerificationEmailTemplateResponse' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf successRedirectionURL
      `Prelude.seq` Prelude.rnf fromEmailAddress
      `Prelude.seq` Prelude.rnf templateContent
      `Prelude.seq` Prelude.rnf templateSubject
      `Prelude.seq` Prelude.rnf failureRedirectionURL
      `Prelude.seq` Prelude.rnf httpStatus
