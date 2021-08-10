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
-- Module      : Network.AWS.SESv2.TestRenderEmailTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a preview of the MIME content of an email when provided with a
-- template and a set of replacement data.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SESv2.TestRenderEmailTemplate
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | >Represents a request to create a preview of the MIME content of an
-- email when provided with a template and a set of replacement data.
--
-- /See:/ 'newTestRenderEmailTemplate' smart constructor.
data TestRenderEmailTemplate = TestRenderEmailTemplate'
  { -- | The name of the template that you want to render.
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
-- 'templateName', 'testRenderEmailTemplate_templateName' - The name of the template that you want to render.
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

-- | The name of the template that you want to render.
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          TestRenderEmailTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "RenderedTemplate")
      )

instance Prelude.Hashable TestRenderEmailTemplate

instance Prelude.NFData TestRenderEmailTemplate

instance Core.ToHeaders TestRenderEmailTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON TestRenderEmailTemplate where
  toJSON TestRenderEmailTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("TemplateData" Core..= templateData)]
      )

instance Core.ToPath TestRenderEmailTemplate where
  toPath TestRenderEmailTemplate' {..} =
    Prelude.mconcat
      [ "/v2/email/templates/",
        Core.toBS templateName,
        "/render"
      ]

instance Core.ToQuery TestRenderEmailTemplate where
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
