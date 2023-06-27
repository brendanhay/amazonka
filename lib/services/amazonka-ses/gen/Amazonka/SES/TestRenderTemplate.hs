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
-- Module      : Amazonka.SES.TestRenderTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a preview of the MIME content of an email when provided with a
-- template and a set of replacement data.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.TestRenderTemplate
  ( -- * Creating a Request
    TestRenderTemplate (..),
    newTestRenderTemplate,

    -- * Request Lenses
    testRenderTemplate_templateName,
    testRenderTemplate_templateData,

    -- * Destructuring the Response
    TestRenderTemplateResponse (..),
    newTestRenderTemplateResponse,

    -- * Response Lenses
    testRenderTemplateResponse_renderedTemplate,
    testRenderTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | /See:/ 'newTestRenderTemplate' smart constructor.
data TestRenderTemplate = TestRenderTemplate'
  { -- | The name of the template that you want to render.
    templateName :: Prelude.Text,
    -- | A list of replacement values to apply to the template. This parameter is
    -- a JSON object, typically consisting of key-value pairs in which the keys
    -- correspond to replacement tags in the email template.
    templateData :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestRenderTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'testRenderTemplate_templateName' - The name of the template that you want to render.
--
-- 'templateData', 'testRenderTemplate_templateData' - A list of replacement values to apply to the template. This parameter is
-- a JSON object, typically consisting of key-value pairs in which the keys
-- correspond to replacement tags in the email template.
newTestRenderTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'templateData'
  Prelude.Text ->
  TestRenderTemplate
newTestRenderTemplate pTemplateName_ pTemplateData_ =
  TestRenderTemplate'
    { templateName = pTemplateName_,
      templateData = pTemplateData_
    }

-- | The name of the template that you want to render.
testRenderTemplate_templateName :: Lens.Lens' TestRenderTemplate Prelude.Text
testRenderTemplate_templateName = Lens.lens (\TestRenderTemplate' {templateName} -> templateName) (\s@TestRenderTemplate' {} a -> s {templateName = a} :: TestRenderTemplate)

-- | A list of replacement values to apply to the template. This parameter is
-- a JSON object, typically consisting of key-value pairs in which the keys
-- correspond to replacement tags in the email template.
testRenderTemplate_templateData :: Lens.Lens' TestRenderTemplate Prelude.Text
testRenderTemplate_templateData = Lens.lens (\TestRenderTemplate' {templateData} -> templateData) (\s@TestRenderTemplate' {} a -> s {templateData = a} :: TestRenderTemplate)

instance Core.AWSRequest TestRenderTemplate where
  type
    AWSResponse TestRenderTemplate =
      TestRenderTemplateResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "TestRenderTemplateResult"
      ( \s h x ->
          TestRenderTemplateResponse'
            Prelude.<$> (x Data..@? "RenderedTemplate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TestRenderTemplate where
  hashWithSalt _salt TestRenderTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateData

instance Prelude.NFData TestRenderTemplate where
  rnf TestRenderTemplate' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateData

instance Data.ToHeaders TestRenderTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath TestRenderTemplate where
  toPath = Prelude.const "/"

instance Data.ToQuery TestRenderTemplate where
  toQuery TestRenderTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("TestRenderTemplate" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "TemplateName" Data.=: templateName,
        "TemplateData" Data.=: templateData
      ]

-- | /See:/ 'newTestRenderTemplateResponse' smart constructor.
data TestRenderTemplateResponse = TestRenderTemplateResponse'
  { -- | The complete MIME message rendered by applying the data in the
    -- TemplateData parameter to the template specified in the TemplateName
    -- parameter.
    renderedTemplate :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestRenderTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'renderedTemplate', 'testRenderTemplateResponse_renderedTemplate' - The complete MIME message rendered by applying the data in the
-- TemplateData parameter to the template specified in the TemplateName
-- parameter.
--
-- 'httpStatus', 'testRenderTemplateResponse_httpStatus' - The response's http status code.
newTestRenderTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TestRenderTemplateResponse
newTestRenderTemplateResponse pHttpStatus_ =
  TestRenderTemplateResponse'
    { renderedTemplate =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The complete MIME message rendered by applying the data in the
-- TemplateData parameter to the template specified in the TemplateName
-- parameter.
testRenderTemplateResponse_renderedTemplate :: Lens.Lens' TestRenderTemplateResponse (Prelude.Maybe Prelude.Text)
testRenderTemplateResponse_renderedTemplate = Lens.lens (\TestRenderTemplateResponse' {renderedTemplate} -> renderedTemplate) (\s@TestRenderTemplateResponse' {} a -> s {renderedTemplate = a} :: TestRenderTemplateResponse)

-- | The response's http status code.
testRenderTemplateResponse_httpStatus :: Lens.Lens' TestRenderTemplateResponse Prelude.Int
testRenderTemplateResponse_httpStatus = Lens.lens (\TestRenderTemplateResponse' {httpStatus} -> httpStatus) (\s@TestRenderTemplateResponse' {} a -> s {httpStatus = a} :: TestRenderTemplateResponse)

instance Prelude.NFData TestRenderTemplateResponse where
  rnf TestRenderTemplateResponse' {..} =
    Prelude.rnf renderedTemplate
      `Prelude.seq` Prelude.rnf httpStatus
