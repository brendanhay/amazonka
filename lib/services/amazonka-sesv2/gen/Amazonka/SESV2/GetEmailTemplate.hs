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
-- Module      : Amazonka.SESV2.GetEmailTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the template object (which includes the subject line, HTML part
-- and text part) for the template you specify.
--
-- You can execute this operation no more than once per second.
module Amazonka.SESV2.GetEmailTemplate
  ( -- * Creating a Request
    GetEmailTemplate (..),
    newGetEmailTemplate,

    -- * Request Lenses
    getEmailTemplate_templateName,

    -- * Destructuring the Response
    GetEmailTemplateResponse (..),
    newGetEmailTemplateResponse,

    -- * Response Lenses
    getEmailTemplateResponse_httpStatus,
    getEmailTemplateResponse_templateName,
    getEmailTemplateResponse_templateContent,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | Represents a request to display the template object (which includes the
-- subject line, HTML part and text part) for the template you specify.
--
-- /See:/ 'newGetEmailTemplate' smart constructor.
data GetEmailTemplate = GetEmailTemplate'
  { -- | The name of the template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEmailTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'getEmailTemplate_templateName' - The name of the template.
newGetEmailTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  GetEmailTemplate
newGetEmailTemplate pTemplateName_ =
  GetEmailTemplate' {templateName = pTemplateName_}

-- | The name of the template.
getEmailTemplate_templateName :: Lens.Lens' GetEmailTemplate Prelude.Text
getEmailTemplate_templateName = Lens.lens (\GetEmailTemplate' {templateName} -> templateName) (\s@GetEmailTemplate' {} a -> s {templateName = a} :: GetEmailTemplate)

instance Core.AWSRequest GetEmailTemplate where
  type
    AWSResponse GetEmailTemplate =
      GetEmailTemplateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEmailTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "TemplateName")
            Prelude.<*> (x Data..:> "TemplateContent")
      )

instance Prelude.Hashable GetEmailTemplate where
  hashWithSalt _salt GetEmailTemplate' {..} =
    _salt `Prelude.hashWithSalt` templateName

instance Prelude.NFData GetEmailTemplate where
  rnf GetEmailTemplate' {..} = Prelude.rnf templateName

instance Data.ToHeaders GetEmailTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetEmailTemplate where
  toPath GetEmailTemplate' {..} =
    Prelude.mconcat
      ["/v2/email/templates/", Data.toBS templateName]

instance Data.ToQuery GetEmailTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | The following element is returned by the service.
--
-- /See:/ 'newGetEmailTemplateResponse' smart constructor.
data GetEmailTemplateResponse = GetEmailTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the template.
    templateName :: Prelude.Text,
    -- | The content of the email template, composed of a subject line, an HTML
    -- part, and a text-only part.
    templateContent :: EmailTemplateContent
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEmailTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getEmailTemplateResponse_httpStatus' - The response's http status code.
--
-- 'templateName', 'getEmailTemplateResponse_templateName' - The name of the template.
--
-- 'templateContent', 'getEmailTemplateResponse_templateContent' - The content of the email template, composed of a subject line, an HTML
-- part, and a text-only part.
newGetEmailTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'templateName'
  Prelude.Text ->
  -- | 'templateContent'
  EmailTemplateContent ->
  GetEmailTemplateResponse
newGetEmailTemplateResponse
  pHttpStatus_
  pTemplateName_
  pTemplateContent_ =
    GetEmailTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        templateName = pTemplateName_,
        templateContent = pTemplateContent_
      }

-- | The response's http status code.
getEmailTemplateResponse_httpStatus :: Lens.Lens' GetEmailTemplateResponse Prelude.Int
getEmailTemplateResponse_httpStatus = Lens.lens (\GetEmailTemplateResponse' {httpStatus} -> httpStatus) (\s@GetEmailTemplateResponse' {} a -> s {httpStatus = a} :: GetEmailTemplateResponse)

-- | The name of the template.
getEmailTemplateResponse_templateName :: Lens.Lens' GetEmailTemplateResponse Prelude.Text
getEmailTemplateResponse_templateName = Lens.lens (\GetEmailTemplateResponse' {templateName} -> templateName) (\s@GetEmailTemplateResponse' {} a -> s {templateName = a} :: GetEmailTemplateResponse)

-- | The content of the email template, composed of a subject line, an HTML
-- part, and a text-only part.
getEmailTemplateResponse_templateContent :: Lens.Lens' GetEmailTemplateResponse EmailTemplateContent
getEmailTemplateResponse_templateContent = Lens.lens (\GetEmailTemplateResponse' {templateContent} -> templateContent) (\s@GetEmailTemplateResponse' {} a -> s {templateContent = a} :: GetEmailTemplateResponse)

instance Prelude.NFData GetEmailTemplateResponse where
  rnf GetEmailTemplateResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf templateName `Prelude.seq`
        Prelude.rnf templateContent
