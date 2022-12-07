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
-- Module      : Amazonka.SESV2.CreateEmailTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an email template. Email templates enable you to send
-- personalized email to one or more destinations in a single API
-- operation. For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Amazonka.SESV2.CreateEmailTemplate
  ( -- * Creating a Request
    CreateEmailTemplate (..),
    newCreateEmailTemplate,

    -- * Request Lenses
    createEmailTemplate_templateName,
    createEmailTemplate_templateContent,

    -- * Destructuring the Response
    CreateEmailTemplateResponse (..),
    newCreateEmailTemplateResponse,

    -- * Response Lenses
    createEmailTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | Represents a request to create an email template. For more information,
-- see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide>.
--
-- /See:/ 'newCreateEmailTemplate' smart constructor.
data CreateEmailTemplate = CreateEmailTemplate'
  { -- | The name of the template.
    templateName :: Prelude.Text,
    -- | The content of the email template, composed of a subject line, an HTML
    -- part, and a text-only part.
    templateContent :: EmailTemplateContent
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEmailTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'createEmailTemplate_templateName' - The name of the template.
--
-- 'templateContent', 'createEmailTemplate_templateContent' - The content of the email template, composed of a subject line, an HTML
-- part, and a text-only part.
newCreateEmailTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'templateContent'
  EmailTemplateContent ->
  CreateEmailTemplate
newCreateEmailTemplate
  pTemplateName_
  pTemplateContent_ =
    CreateEmailTemplate'
      { templateName = pTemplateName_,
        templateContent = pTemplateContent_
      }

-- | The name of the template.
createEmailTemplate_templateName :: Lens.Lens' CreateEmailTemplate Prelude.Text
createEmailTemplate_templateName = Lens.lens (\CreateEmailTemplate' {templateName} -> templateName) (\s@CreateEmailTemplate' {} a -> s {templateName = a} :: CreateEmailTemplate)

-- | The content of the email template, composed of a subject line, an HTML
-- part, and a text-only part.
createEmailTemplate_templateContent :: Lens.Lens' CreateEmailTemplate EmailTemplateContent
createEmailTemplate_templateContent = Lens.lens (\CreateEmailTemplate' {templateContent} -> templateContent) (\s@CreateEmailTemplate' {} a -> s {templateContent = a} :: CreateEmailTemplate)

instance Core.AWSRequest CreateEmailTemplate where
  type
    AWSResponse CreateEmailTemplate =
      CreateEmailTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateEmailTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEmailTemplate where
  hashWithSalt _salt CreateEmailTemplate' {..} =
    _salt `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateContent

instance Prelude.NFData CreateEmailTemplate where
  rnf CreateEmailTemplate' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateContent

instance Data.ToHeaders CreateEmailTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEmailTemplate where
  toJSON CreateEmailTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TemplateName" Data..= templateName),
            Prelude.Just
              ("TemplateContent" Data..= templateContent)
          ]
      )

instance Data.ToPath CreateEmailTemplate where
  toPath = Prelude.const "/v2/email/templates"

instance Data.ToQuery CreateEmailTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | If the action is successful, the service sends back an HTTP 200 response
-- with an empty HTTP body.
--
-- /See:/ 'newCreateEmailTemplateResponse' smart constructor.
data CreateEmailTemplateResponse = CreateEmailTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEmailTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createEmailTemplateResponse_httpStatus' - The response's http status code.
newCreateEmailTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEmailTemplateResponse
newCreateEmailTemplateResponse pHttpStatus_ =
  CreateEmailTemplateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createEmailTemplateResponse_httpStatus :: Lens.Lens' CreateEmailTemplateResponse Prelude.Int
createEmailTemplateResponse_httpStatus = Lens.lens (\CreateEmailTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateEmailTemplateResponse' {} a -> s {httpStatus = a} :: CreateEmailTemplateResponse)

instance Prelude.NFData CreateEmailTemplateResponse where
  rnf CreateEmailTemplateResponse' {..} =
    Prelude.rnf httpStatus
