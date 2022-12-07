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
-- Module      : Amazonka.SESV2.TestRenderEmailTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a preview of the MIME content of an email when provided with a
-- template and a set of replacement data.
--
-- You can execute this operation no more than once per second.
module Amazonka.SESV2.TestRenderEmailTemplate
  ( -- * Creating a Request
    TestRenderEmailTemplate (..),
    newTestRenderEmailTemplate,

    -- * Request Lenses
    testRenderEmailTemplate_templateName,
    testRenderEmailTemplate_templateData,

    -- * Destructuring the Response
    TestRenderEmailTemplateResponse (..),
    newTestRenderEmailTemplateResponse,

    -- * Response Lenses
    testRenderEmailTemplateResponse_httpStatus,
    testRenderEmailTemplateResponse_renderedTemplate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | >Represents a request to create a preview of the MIME content of an
-- email when provided with a template and a set of replacement data.
--
-- /See:/ 'newTestRenderEmailTemplate' smart constructor.
data TestRenderEmailTemplate = TestRenderEmailTemplate'
  { -- | The name of the template.
    templateName :: Prelude.Text,
    -- | A list of replacement values to apply to the template. This parameter is
    -- a JSON object, typically consisting of key-value pairs in which the keys
    -- correspond to replacement tags in the email template.
    templateData :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestRenderEmailTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'testRenderEmailTemplate_templateName' - The name of the template.
--
-- 'templateData', 'testRenderEmailTemplate_templateData' - A list of replacement values to apply to the template. This parameter is
-- a JSON object, typically consisting of key-value pairs in which the keys
-- correspond to replacement tags in the email template.
newTestRenderEmailTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'templateData'
  Prelude.Text ->
  TestRenderEmailTemplate
newTestRenderEmailTemplate
  pTemplateName_
  pTemplateData_ =
    TestRenderEmailTemplate'
      { templateName =
          pTemplateName_,
        templateData = pTemplateData_
      }

-- | The name of the template.
testRenderEmailTemplate_templateName :: Lens.Lens' TestRenderEmailTemplate Prelude.Text
testRenderEmailTemplate_templateName = Lens.lens (\TestRenderEmailTemplate' {templateName} -> templateName) (\s@TestRenderEmailTemplate' {} a -> s {templateName = a} :: TestRenderEmailTemplate)

-- | A list of replacement values to apply to the template. This parameter is
-- a JSON object, typically consisting of key-value pairs in which the keys
-- correspond to replacement tags in the email template.
testRenderEmailTemplate_templateData :: Lens.Lens' TestRenderEmailTemplate Prelude.Text
testRenderEmailTemplate_templateData = Lens.lens (\TestRenderEmailTemplate' {templateData} -> templateData) (\s@TestRenderEmailTemplate' {} a -> s {templateData = a} :: TestRenderEmailTemplate)

instance Core.AWSRequest TestRenderEmailTemplate where
  type
    AWSResponse TestRenderEmailTemplate =
      TestRenderEmailTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TestRenderEmailTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "RenderedTemplate")
      )

instance Prelude.Hashable TestRenderEmailTemplate where
  hashWithSalt _salt TestRenderEmailTemplate' {..} =
    _salt `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateData

instance Prelude.NFData TestRenderEmailTemplate where
  rnf TestRenderEmailTemplate' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateData

instance Data.ToHeaders TestRenderEmailTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TestRenderEmailTemplate where
  toJSON TestRenderEmailTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TemplateData" Data..= templateData)]
      )

instance Data.ToPath TestRenderEmailTemplate where
  toPath TestRenderEmailTemplate' {..} =
    Prelude.mconcat
      [ "/v2/email/templates/",
        Data.toBS templateName,
        "/render"
      ]

instance Data.ToQuery TestRenderEmailTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | The following element is returned by the service.
--
-- /See:/ 'newTestRenderEmailTemplateResponse' smart constructor.
data TestRenderEmailTemplateResponse = TestRenderEmailTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The complete MIME message rendered by applying the data in the
    -- @TemplateData@ parameter to the template specified in the TemplateName
    -- parameter.
    renderedTemplate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestRenderEmailTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'testRenderEmailTemplateResponse_httpStatus' - The response's http status code.
--
-- 'renderedTemplate', 'testRenderEmailTemplateResponse_renderedTemplate' - The complete MIME message rendered by applying the data in the
-- @TemplateData@ parameter to the template specified in the TemplateName
-- parameter.
newTestRenderEmailTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'renderedTemplate'
  Prelude.Text ->
  TestRenderEmailTemplateResponse
newTestRenderEmailTemplateResponse
  pHttpStatus_
  pRenderedTemplate_ =
    TestRenderEmailTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        renderedTemplate = pRenderedTemplate_
      }

-- | The response's http status code.
testRenderEmailTemplateResponse_httpStatus :: Lens.Lens' TestRenderEmailTemplateResponse Prelude.Int
testRenderEmailTemplateResponse_httpStatus = Lens.lens (\TestRenderEmailTemplateResponse' {httpStatus} -> httpStatus) (\s@TestRenderEmailTemplateResponse' {} a -> s {httpStatus = a} :: TestRenderEmailTemplateResponse)

-- | The complete MIME message rendered by applying the data in the
-- @TemplateData@ parameter to the template specified in the TemplateName
-- parameter.
testRenderEmailTemplateResponse_renderedTemplate :: Lens.Lens' TestRenderEmailTemplateResponse Prelude.Text
testRenderEmailTemplateResponse_renderedTemplate = Lens.lens (\TestRenderEmailTemplateResponse' {renderedTemplate} -> renderedTemplate) (\s@TestRenderEmailTemplateResponse' {} a -> s {renderedTemplate = a} :: TestRenderEmailTemplateResponse)

instance
  Prelude.NFData
    TestRenderEmailTemplateResponse
  where
  rnf TestRenderEmailTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf renderedTemplate
