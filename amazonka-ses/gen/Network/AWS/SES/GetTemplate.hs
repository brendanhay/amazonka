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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | /See:/ 'newGetTemplate' smart constructor.
data GetTemplate = GetTemplate'
  { -- | The name of the template you want to retrieve.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetTemplate
newGetTemplate pTemplateName_ =
  GetTemplate' {templateName = pTemplateName_}

-- | The name of the template you want to retrieve.
getTemplate_templateName :: Lens.Lens' GetTemplate Prelude.Text
getTemplate_templateName = Lens.lens (\GetTemplate' {templateName} -> templateName) (\s@GetTemplate' {} a -> s {templateName = a} :: GetTemplate)

instance Core.AWSRequest GetTemplate where
  type AWSResponse GetTemplate = GetTemplateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetTemplateResult"
      ( \s h x ->
          GetTemplateResponse'
            Prelude.<$> (x Core..@? "Template")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTemplate

instance Prelude.NFData GetTemplate

instance Core.ToHeaders GetTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery GetTemplate where
  toQuery GetTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("GetTemplate" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "TemplateName" Core.=: templateName
      ]

-- | /See:/ 'newGetTemplateResponse' smart constructor.
data GetTemplateResponse = GetTemplateResponse'
  { template :: Prelude.Maybe Template,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetTemplateResponse
newGetTemplateResponse pHttpStatus_ =
  GetTemplateResponse'
    { template = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getTemplateResponse_template :: Lens.Lens' GetTemplateResponse (Prelude.Maybe Template)
getTemplateResponse_template = Lens.lens (\GetTemplateResponse' {template} -> template) (\s@GetTemplateResponse' {} a -> s {template = a} :: GetTemplateResponse)

-- | The response's http status code.
getTemplateResponse_httpStatus :: Lens.Lens' GetTemplateResponse Prelude.Int
getTemplateResponse_httpStatus = Lens.lens (\GetTemplateResponse' {httpStatus} -> httpStatus) (\s@GetTemplateResponse' {} a -> s {httpStatus = a} :: GetTemplateResponse)

instance Prelude.NFData GetTemplateResponse
