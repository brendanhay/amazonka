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
-- Module      : Amazonka.SESV2.UpdateEmailTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an email template. Email templates enable you to send
-- personalized email to one or more destinations in a single API
-- operation. For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Amazonka.SESV2.UpdateEmailTemplate
  ( -- * Creating a Request
    UpdateEmailTemplate (..),
    newUpdateEmailTemplate,

    -- * Request Lenses
    updateEmailTemplate_templateName,
    updateEmailTemplate_templateContent,

    -- * Destructuring the Response
    UpdateEmailTemplateResponse (..),
    newUpdateEmailTemplateResponse,

    -- * Response Lenses
    updateEmailTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | Represents a request to update an email template. For more information,
-- see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide>.
--
-- /See:/ 'newUpdateEmailTemplate' smart constructor.
data UpdateEmailTemplate = UpdateEmailTemplate'
  { -- | The name of the template.
    templateName :: Prelude.Text,
    -- | The content of the email template, composed of a subject line, an HTML
    -- part, and a text-only part.
    templateContent :: EmailTemplateContent
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEmailTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'updateEmailTemplate_templateName' - The name of the template.
--
-- 'templateContent', 'updateEmailTemplate_templateContent' - The content of the email template, composed of a subject line, an HTML
-- part, and a text-only part.
newUpdateEmailTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'templateContent'
  EmailTemplateContent ->
  UpdateEmailTemplate
newUpdateEmailTemplate
  pTemplateName_
  pTemplateContent_ =
    UpdateEmailTemplate'
      { templateName = pTemplateName_,
        templateContent = pTemplateContent_
      }

-- | The name of the template.
updateEmailTemplate_templateName :: Lens.Lens' UpdateEmailTemplate Prelude.Text
updateEmailTemplate_templateName = Lens.lens (\UpdateEmailTemplate' {templateName} -> templateName) (\s@UpdateEmailTemplate' {} a -> s {templateName = a} :: UpdateEmailTemplate)

-- | The content of the email template, composed of a subject line, an HTML
-- part, and a text-only part.
updateEmailTemplate_templateContent :: Lens.Lens' UpdateEmailTemplate EmailTemplateContent
updateEmailTemplate_templateContent = Lens.lens (\UpdateEmailTemplate' {templateContent} -> templateContent) (\s@UpdateEmailTemplate' {} a -> s {templateContent = a} :: UpdateEmailTemplate)

instance Core.AWSRequest UpdateEmailTemplate where
  type
    AWSResponse UpdateEmailTemplate =
      UpdateEmailTemplateResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateEmailTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateEmailTemplate where
  hashWithSalt _salt UpdateEmailTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateContent

instance Prelude.NFData UpdateEmailTemplate where
  rnf UpdateEmailTemplate' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateContent

instance Data.ToHeaders UpdateEmailTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateEmailTemplate where
  toJSON UpdateEmailTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TemplateContent" Data..= templateContent)
          ]
      )

instance Data.ToPath UpdateEmailTemplate where
  toPath UpdateEmailTemplate' {..} =
    Prelude.mconcat
      ["/v2/email/templates/", Data.toBS templateName]

instance Data.ToQuery UpdateEmailTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | If the action is successful, the service sends back an HTTP 200 response
-- with an empty HTTP body.
--
-- /See:/ 'newUpdateEmailTemplateResponse' smart constructor.
data UpdateEmailTemplateResponse = UpdateEmailTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEmailTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEmailTemplateResponse_httpStatus' - The response's http status code.
newUpdateEmailTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEmailTemplateResponse
newUpdateEmailTemplateResponse pHttpStatus_ =
  UpdateEmailTemplateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateEmailTemplateResponse_httpStatus :: Lens.Lens' UpdateEmailTemplateResponse Prelude.Int
updateEmailTemplateResponse_httpStatus = Lens.lens (\UpdateEmailTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdateEmailTemplateResponse' {} a -> s {httpStatus = a} :: UpdateEmailTemplateResponse)

instance Prelude.NFData UpdateEmailTemplateResponse where
  rnf UpdateEmailTemplateResponse' {..} =
    Prelude.rnf httpStatus
