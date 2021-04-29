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
-- Module      : Network.AWS.SES.UpdateCustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing custom verification email template.
--
-- For more information about custom verification email templates, see
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates>
-- in the /Amazon SES Developer Guide/.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.UpdateCustomVerificationEmailTemplate
  ( -- * Creating a Request
    UpdateCustomVerificationEmailTemplate (..),
    newUpdateCustomVerificationEmailTemplate,

    -- * Request Lenses
    updateCustomVerificationEmailTemplate_templateSubject,
    updateCustomVerificationEmailTemplate_fromEmailAddress,
    updateCustomVerificationEmailTemplate_templateContent,
    updateCustomVerificationEmailTemplate_successRedirectionURL,
    updateCustomVerificationEmailTemplate_failureRedirectionURL,
    updateCustomVerificationEmailTemplate_templateName,

    -- * Destructuring the Response
    UpdateCustomVerificationEmailTemplateResponse (..),
    newUpdateCustomVerificationEmailTemplateResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to update an existing custom verification email
-- template.
--
-- /See:/ 'newUpdateCustomVerificationEmailTemplate' smart constructor.
data UpdateCustomVerificationEmailTemplate = UpdateCustomVerificationEmailTemplate'
  { -- | The subject line of the custom verification email.
    templateSubject :: Prelude.Maybe Prelude.Text,
    -- | The email address that the custom verification email is sent from.
    fromEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | The content of the custom verification email. The total size of the
    -- email must be less than 10 MB. The message body may contain HTML, with
    -- some limitations. For more information, see
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html#custom-verification-emails-faq Custom Verification Email Frequently Asked Questions>
    -- in the /Amazon SES Developer Guide/.
    templateContent :: Prelude.Maybe Prelude.Text,
    -- | The URL that the recipient of the verification email is sent to if his
    -- or her address is successfully verified.
    successRedirectionURL :: Prelude.Maybe Prelude.Text,
    -- | The URL that the recipient of the verification email is sent to if his
    -- or her address is not successfully verified.
    failureRedirectionURL :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom verification email template that you want to
    -- update.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateCustomVerificationEmailTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateSubject', 'updateCustomVerificationEmailTemplate_templateSubject' - The subject line of the custom verification email.
--
-- 'fromEmailAddress', 'updateCustomVerificationEmailTemplate_fromEmailAddress' - The email address that the custom verification email is sent from.
--
-- 'templateContent', 'updateCustomVerificationEmailTemplate_templateContent' - The content of the custom verification email. The total size of the
-- email must be less than 10 MB. The message body may contain HTML, with
-- some limitations. For more information, see
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html#custom-verification-emails-faq Custom Verification Email Frequently Asked Questions>
-- in the /Amazon SES Developer Guide/.
--
-- 'successRedirectionURL', 'updateCustomVerificationEmailTemplate_successRedirectionURL' - The URL that the recipient of the verification email is sent to if his
-- or her address is successfully verified.
--
-- 'failureRedirectionURL', 'updateCustomVerificationEmailTemplate_failureRedirectionURL' - The URL that the recipient of the verification email is sent to if his
-- or her address is not successfully verified.
--
-- 'templateName', 'updateCustomVerificationEmailTemplate_templateName' - The name of the custom verification email template that you want to
-- update.
newUpdateCustomVerificationEmailTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  UpdateCustomVerificationEmailTemplate
newUpdateCustomVerificationEmailTemplate
  pTemplateName_ =
    UpdateCustomVerificationEmailTemplate'
      { templateSubject =
          Prelude.Nothing,
        fromEmailAddress = Prelude.Nothing,
        templateContent = Prelude.Nothing,
        successRedirectionURL =
          Prelude.Nothing,
        failureRedirectionURL =
          Prelude.Nothing,
        templateName = pTemplateName_
      }

-- | The subject line of the custom verification email.
updateCustomVerificationEmailTemplate_templateSubject :: Lens.Lens' UpdateCustomVerificationEmailTemplate (Prelude.Maybe Prelude.Text)
updateCustomVerificationEmailTemplate_templateSubject = Lens.lens (\UpdateCustomVerificationEmailTemplate' {templateSubject} -> templateSubject) (\s@UpdateCustomVerificationEmailTemplate' {} a -> s {templateSubject = a} :: UpdateCustomVerificationEmailTemplate)

-- | The email address that the custom verification email is sent from.
updateCustomVerificationEmailTemplate_fromEmailAddress :: Lens.Lens' UpdateCustomVerificationEmailTemplate (Prelude.Maybe Prelude.Text)
updateCustomVerificationEmailTemplate_fromEmailAddress = Lens.lens (\UpdateCustomVerificationEmailTemplate' {fromEmailAddress} -> fromEmailAddress) (\s@UpdateCustomVerificationEmailTemplate' {} a -> s {fromEmailAddress = a} :: UpdateCustomVerificationEmailTemplate)

-- | The content of the custom verification email. The total size of the
-- email must be less than 10 MB. The message body may contain HTML, with
-- some limitations. For more information, see
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html#custom-verification-emails-faq Custom Verification Email Frequently Asked Questions>
-- in the /Amazon SES Developer Guide/.
updateCustomVerificationEmailTemplate_templateContent :: Lens.Lens' UpdateCustomVerificationEmailTemplate (Prelude.Maybe Prelude.Text)
updateCustomVerificationEmailTemplate_templateContent = Lens.lens (\UpdateCustomVerificationEmailTemplate' {templateContent} -> templateContent) (\s@UpdateCustomVerificationEmailTemplate' {} a -> s {templateContent = a} :: UpdateCustomVerificationEmailTemplate)

-- | The URL that the recipient of the verification email is sent to if his
-- or her address is successfully verified.
updateCustomVerificationEmailTemplate_successRedirectionURL :: Lens.Lens' UpdateCustomVerificationEmailTemplate (Prelude.Maybe Prelude.Text)
updateCustomVerificationEmailTemplate_successRedirectionURL = Lens.lens (\UpdateCustomVerificationEmailTemplate' {successRedirectionURL} -> successRedirectionURL) (\s@UpdateCustomVerificationEmailTemplate' {} a -> s {successRedirectionURL = a} :: UpdateCustomVerificationEmailTemplate)

-- | The URL that the recipient of the verification email is sent to if his
-- or her address is not successfully verified.
updateCustomVerificationEmailTemplate_failureRedirectionURL :: Lens.Lens' UpdateCustomVerificationEmailTemplate (Prelude.Maybe Prelude.Text)
updateCustomVerificationEmailTemplate_failureRedirectionURL = Lens.lens (\UpdateCustomVerificationEmailTemplate' {failureRedirectionURL} -> failureRedirectionURL) (\s@UpdateCustomVerificationEmailTemplate' {} a -> s {failureRedirectionURL = a} :: UpdateCustomVerificationEmailTemplate)

-- | The name of the custom verification email template that you want to
-- update.
updateCustomVerificationEmailTemplate_templateName :: Lens.Lens' UpdateCustomVerificationEmailTemplate Prelude.Text
updateCustomVerificationEmailTemplate_templateName = Lens.lens (\UpdateCustomVerificationEmailTemplate' {templateName} -> templateName) (\s@UpdateCustomVerificationEmailTemplate' {} a -> s {templateName = a} :: UpdateCustomVerificationEmailTemplate)

instance
  Prelude.AWSRequest
    UpdateCustomVerificationEmailTemplate
  where
  type
    Rs UpdateCustomVerificationEmailTemplate =
      UpdateCustomVerificationEmailTemplateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      UpdateCustomVerificationEmailTemplateResponse'

instance
  Prelude.Hashable
    UpdateCustomVerificationEmailTemplate

instance
  Prelude.NFData
    UpdateCustomVerificationEmailTemplate

instance
  Prelude.ToHeaders
    UpdateCustomVerificationEmailTemplate
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    UpdateCustomVerificationEmailTemplate
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    UpdateCustomVerificationEmailTemplate
  where
  toQuery UpdateCustomVerificationEmailTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "UpdateCustomVerificationEmailTemplate" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "TemplateSubject" Prelude.=: templateSubject,
        "FromEmailAddress" Prelude.=: fromEmailAddress,
        "TemplateContent" Prelude.=: templateContent,
        "SuccessRedirectionURL"
          Prelude.=: successRedirectionURL,
        "FailureRedirectionURL"
          Prelude.=: failureRedirectionURL,
        "TemplateName" Prelude.=: templateName
      ]

-- | /See:/ 'newUpdateCustomVerificationEmailTemplateResponse' smart constructor.
data UpdateCustomVerificationEmailTemplateResponse = UpdateCustomVerificationEmailTemplateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateCustomVerificationEmailTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateCustomVerificationEmailTemplateResponse ::
  UpdateCustomVerificationEmailTemplateResponse
newUpdateCustomVerificationEmailTemplateResponse =
  UpdateCustomVerificationEmailTemplateResponse'

instance
  Prelude.NFData
    UpdateCustomVerificationEmailTemplateResponse
