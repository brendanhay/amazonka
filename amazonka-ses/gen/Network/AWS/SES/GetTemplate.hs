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
-- Module      : Network.AWS.SES.GetTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the template object (which includes the Subject line, HTML part
-- and text part) for the template you specify.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.GetTemplate
  ( -- * Creating a Request
    GetTemplate (..),
    newGetTemplate,

    -- * Request Lenses
    getTemplate_templateName,

    -- * Destructuring the Response
    GetTemplateResponse (..),
    newGetTemplateResponse,

    -- * Response Lenses
    getTemplateResponse_template,
    getTemplateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | /See:/ 'newGetTemplate' smart constructor.
data GetTemplate = GetTemplate'
  { -- | The name of the template you want to retrieve.
    templateName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'getTemplate_templateName' - The name of the template you want to retrieve.
newGetTemplate ::
  -- | 'templateName'
  Core.Text ->
  GetTemplate
newGetTemplate pTemplateName_ =
  GetTemplate' {templateName = pTemplateName_}

-- | The name of the template you want to retrieve.
getTemplate_templateName :: Lens.Lens' GetTemplate Core.Text
getTemplate_templateName = Lens.lens (\GetTemplate' {templateName} -> templateName) (\s@GetTemplate' {} a -> s {templateName = a} :: GetTemplate)

instance Core.AWSRequest GetTemplate where
  type AWSResponse GetTemplate = GetTemplateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetTemplateResult"
      ( \s h x ->
          GetTemplateResponse'
            Core.<$> (x Core..@? "Template")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetTemplate

instance Core.NFData GetTemplate

instance Core.ToHeaders GetTemplate where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetTemplate where
  toPath = Core.const "/"

instance Core.ToQuery GetTemplate where
  toQuery GetTemplate' {..} =
    Core.mconcat
      [ "Action" Core.=: ("GetTemplate" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "TemplateName" Core.=: templateName
      ]

-- | /See:/ 'newGetTemplateResponse' smart constructor.
data GetTemplateResponse = GetTemplateResponse'
  { template :: Core.Maybe Template,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'template', 'getTemplateResponse_template' - Undocumented member.
--
-- 'httpStatus', 'getTemplateResponse_httpStatus' - The response's http status code.
newGetTemplateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTemplateResponse
newGetTemplateResponse pHttpStatus_ =
  GetTemplateResponse'
    { template = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getTemplateResponse_template :: Lens.Lens' GetTemplateResponse (Core.Maybe Template)
getTemplateResponse_template = Lens.lens (\GetTemplateResponse' {template} -> template) (\s@GetTemplateResponse' {} a -> s {template = a} :: GetTemplateResponse)

-- | The response's http status code.
getTemplateResponse_httpStatus :: Lens.Lens' GetTemplateResponse Core.Int
getTemplateResponse_httpStatus = Lens.lens (\GetTemplateResponse' {httpStatus} -> httpStatus) (\s@GetTemplateResponse' {} a -> s {httpStatus = a} :: GetTemplateResponse)

instance Core.NFData GetTemplateResponse
