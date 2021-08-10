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
-- Module      : Network.AWS.SESv2.GetEmailTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the template object (which includes the subject line, HTML part
-- and text part) for the template you specify.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SESv2.GetEmailTemplate
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | Represents a request to display the template object (which includes the
-- subject line, HTML part and text part) for the template you specify.
--
-- /See:/ 'newGetEmailTemplate' smart constructor.
data GetEmailTemplate = GetEmailTemplate'
  { -- | The name of the template you want to retrieve.
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
-- 'templateName', 'getEmailTemplate_templateName' - The name of the template you want to retrieve.
newGetEmailTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  GetEmailTemplate
newGetEmailTemplate pTemplateName_ =
  GetEmailTemplate' {templateName = pTemplateName_}

-- | The name of the template you want to retrieve.
getEmailTemplate_templateName :: Lens.Lens' GetEmailTemplate Prelude.Text
getEmailTemplate_templateName = Lens.lens (\GetEmailTemplate' {templateName} -> templateName) (\s@GetEmailTemplate' {} a -> s {templateName = a} :: GetEmailTemplate)

instance Core.AWSRequest GetEmailTemplate where
  type
    AWSResponse GetEmailTemplate =
      GetEmailTemplateResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEmailTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "TemplateName")
            Prelude.<*> (x Core..:> "TemplateContent")
      )

instance Prelude.Hashable GetEmailTemplate

instance Prelude.NFData GetEmailTemplate

instance Core.ToHeaders GetEmailTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetEmailTemplate where
  toPath GetEmailTemplate' {..} =
    Prelude.mconcat
      ["/v2/email/templates/", Core.toBS templateName]

instance Core.ToQuery GetEmailTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | The following element is returned by the service.
--
-- /See:/ 'newGetEmailTemplateResponse' smart constructor.
data GetEmailTemplateResponse = GetEmailTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the template you want to retrieve.
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
-- 'templateName', 'getEmailTemplateResponse_templateName' - The name of the template you want to retrieve.
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

-- | The name of the template you want to retrieve.
getEmailTemplateResponse_templateName :: Lens.Lens' GetEmailTemplateResponse Prelude.Text
getEmailTemplateResponse_templateName = Lens.lens (\GetEmailTemplateResponse' {templateName} -> templateName) (\s@GetEmailTemplateResponse' {} a -> s {templateName = a} :: GetEmailTemplateResponse)

-- | The content of the email template, composed of a subject line, an HTML
-- part, and a text-only part.
getEmailTemplateResponse_templateContent :: Lens.Lens' GetEmailTemplateResponse EmailTemplateContent
getEmailTemplateResponse_templateContent = Lens.lens (\GetEmailTemplateResponse' {templateContent} -> templateContent) (\s@GetEmailTemplateResponse' {} a -> s {templateContent = a} :: GetEmailTemplateResponse)

instance Prelude.NFData GetEmailTemplateResponse
