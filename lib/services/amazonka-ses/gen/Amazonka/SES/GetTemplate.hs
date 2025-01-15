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
-- Module      : Amazonka.SES.GetTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the template object (which includes the Subject line, HTML part
-- and text part) for the template you specify.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.GetTemplate
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetTemplateResult"
      ( \s h x ->
          GetTemplateResponse'
            Prelude.<$> (x Data..@? "Template")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTemplate where
  hashWithSalt _salt GetTemplate' {..} =
    _salt `Prelude.hashWithSalt` templateName

instance Prelude.NFData GetTemplate where
  rnf GetTemplate' {..} = Prelude.rnf templateName

instance Data.ToHeaders GetTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetTemplate where
  toPath = Prelude.const "/"

instance Data.ToQuery GetTemplate where
  toQuery GetTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetTemplate" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "TemplateName" Data.=: templateName
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

instance Prelude.NFData GetTemplateResponse where
  rnf GetTemplateResponse' {..} =
    Prelude.rnf template `Prelude.seq`
      Prelude.rnf httpStatus
