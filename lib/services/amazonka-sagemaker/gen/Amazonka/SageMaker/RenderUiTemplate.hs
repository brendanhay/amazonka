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
-- Module      : Amazonka.SageMaker.RenderUiTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Renders the UI template so that you can preview the worker\'s
-- experience.
module Amazonka.SageMaker.RenderUiTemplate
  ( -- * Creating a Request
    RenderUiTemplate (..),
    newRenderUiTemplate,

    -- * Request Lenses
    renderUiTemplate_uiTemplate,
    renderUiTemplate_humanTaskUiArn,
    renderUiTemplate_task,
    renderUiTemplate_roleArn,

    -- * Destructuring the Response
    RenderUiTemplateResponse (..),
    newRenderUiTemplateResponse,

    -- * Response Lenses
    renderUiTemplateResponse_httpStatus,
    renderUiTemplateResponse_renderedContent,
    renderUiTemplateResponse_errors,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newRenderUiTemplate' smart constructor.
data RenderUiTemplate = RenderUiTemplate'
  { -- | A @Template@ object containing the worker UI template to render.
    uiTemplate :: Prelude.Maybe UiTemplate,
    -- | The @HumanTaskUiArn@ of the worker UI that you want to render. Do not
    -- provide a @HumanTaskUiArn@ if you use the @UiTemplate@ parameter.
    --
    -- See a list of available Human Ui Amazon Resource Names (ARNs) in
    -- UiConfig.
    humanTaskUiArn :: Prelude.Maybe Prelude.Text,
    -- | A @RenderableTask@ object containing a representative task to render.
    task :: RenderableTask,
    -- | The Amazon Resource Name (ARN) that has access to the S3 objects that
    -- are used by the template.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RenderUiTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uiTemplate', 'renderUiTemplate_uiTemplate' - A @Template@ object containing the worker UI template to render.
--
-- 'humanTaskUiArn', 'renderUiTemplate_humanTaskUiArn' - The @HumanTaskUiArn@ of the worker UI that you want to render. Do not
-- provide a @HumanTaskUiArn@ if you use the @UiTemplate@ parameter.
--
-- See a list of available Human Ui Amazon Resource Names (ARNs) in
-- UiConfig.
--
-- 'task', 'renderUiTemplate_task' - A @RenderableTask@ object containing a representative task to render.
--
-- 'roleArn', 'renderUiTemplate_roleArn' - The Amazon Resource Name (ARN) that has access to the S3 objects that
-- are used by the template.
newRenderUiTemplate ::
  -- | 'task'
  RenderableTask ->
  -- | 'roleArn'
  Prelude.Text ->
  RenderUiTemplate
newRenderUiTemplate pTask_ pRoleArn_ =
  RenderUiTemplate'
    { uiTemplate = Prelude.Nothing,
      humanTaskUiArn = Prelude.Nothing,
      task = pTask_,
      roleArn = pRoleArn_
    }

-- | A @Template@ object containing the worker UI template to render.
renderUiTemplate_uiTemplate :: Lens.Lens' RenderUiTemplate (Prelude.Maybe UiTemplate)
renderUiTemplate_uiTemplate = Lens.lens (\RenderUiTemplate' {uiTemplate} -> uiTemplate) (\s@RenderUiTemplate' {} a -> s {uiTemplate = a} :: RenderUiTemplate)

-- | The @HumanTaskUiArn@ of the worker UI that you want to render. Do not
-- provide a @HumanTaskUiArn@ if you use the @UiTemplate@ parameter.
--
-- See a list of available Human Ui Amazon Resource Names (ARNs) in
-- UiConfig.
renderUiTemplate_humanTaskUiArn :: Lens.Lens' RenderUiTemplate (Prelude.Maybe Prelude.Text)
renderUiTemplate_humanTaskUiArn = Lens.lens (\RenderUiTemplate' {humanTaskUiArn} -> humanTaskUiArn) (\s@RenderUiTemplate' {} a -> s {humanTaskUiArn = a} :: RenderUiTemplate)

-- | A @RenderableTask@ object containing a representative task to render.
renderUiTemplate_task :: Lens.Lens' RenderUiTemplate RenderableTask
renderUiTemplate_task = Lens.lens (\RenderUiTemplate' {task} -> task) (\s@RenderUiTemplate' {} a -> s {task = a} :: RenderUiTemplate)

-- | The Amazon Resource Name (ARN) that has access to the S3 objects that
-- are used by the template.
renderUiTemplate_roleArn :: Lens.Lens' RenderUiTemplate Prelude.Text
renderUiTemplate_roleArn = Lens.lens (\RenderUiTemplate' {roleArn} -> roleArn) (\s@RenderUiTemplate' {} a -> s {roleArn = a} :: RenderUiTemplate)

instance Core.AWSRequest RenderUiTemplate where
  type
    AWSResponse RenderUiTemplate =
      RenderUiTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RenderUiTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "RenderedContent")
            Prelude.<*> (x Data..?> "Errors" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable RenderUiTemplate where
  hashWithSalt _salt RenderUiTemplate' {..} =
    _salt `Prelude.hashWithSalt` uiTemplate
      `Prelude.hashWithSalt` humanTaskUiArn
      `Prelude.hashWithSalt` task
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData RenderUiTemplate where
  rnf RenderUiTemplate' {..} =
    Prelude.rnf uiTemplate
      `Prelude.seq` Prelude.rnf humanTaskUiArn
      `Prelude.seq` Prelude.rnf task
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders RenderUiTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.RenderUiTemplate" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RenderUiTemplate where
  toJSON RenderUiTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("UiTemplate" Data..=) Prelude.<$> uiTemplate,
            ("HumanTaskUiArn" Data..=)
              Prelude.<$> humanTaskUiArn,
            Prelude.Just ("Task" Data..= task),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath RenderUiTemplate where
  toPath = Prelude.const "/"

instance Data.ToQuery RenderUiTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRenderUiTemplateResponse' smart constructor.
data RenderUiTemplateResponse = RenderUiTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A Liquid template that renders the HTML for the worker UI.
    renderedContent :: Prelude.Text,
    -- | A list of one or more @RenderingError@ objects if any were encountered
    -- while rendering the template. If there were no errors, the list is
    -- empty.
    errors :: [RenderingError]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RenderUiTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'renderUiTemplateResponse_httpStatus' - The response's http status code.
--
-- 'renderedContent', 'renderUiTemplateResponse_renderedContent' - A Liquid template that renders the HTML for the worker UI.
--
-- 'errors', 'renderUiTemplateResponse_errors' - A list of one or more @RenderingError@ objects if any were encountered
-- while rendering the template. If there were no errors, the list is
-- empty.
newRenderUiTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'renderedContent'
  Prelude.Text ->
  RenderUiTemplateResponse
newRenderUiTemplateResponse
  pHttpStatus_
  pRenderedContent_ =
    RenderUiTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        renderedContent = pRenderedContent_,
        errors = Prelude.mempty
      }

-- | The response's http status code.
renderUiTemplateResponse_httpStatus :: Lens.Lens' RenderUiTemplateResponse Prelude.Int
renderUiTemplateResponse_httpStatus = Lens.lens (\RenderUiTemplateResponse' {httpStatus} -> httpStatus) (\s@RenderUiTemplateResponse' {} a -> s {httpStatus = a} :: RenderUiTemplateResponse)

-- | A Liquid template that renders the HTML for the worker UI.
renderUiTemplateResponse_renderedContent :: Lens.Lens' RenderUiTemplateResponse Prelude.Text
renderUiTemplateResponse_renderedContent = Lens.lens (\RenderUiTemplateResponse' {renderedContent} -> renderedContent) (\s@RenderUiTemplateResponse' {} a -> s {renderedContent = a} :: RenderUiTemplateResponse)

-- | A list of one or more @RenderingError@ objects if any were encountered
-- while rendering the template. If there were no errors, the list is
-- empty.
renderUiTemplateResponse_errors :: Lens.Lens' RenderUiTemplateResponse [RenderingError]
renderUiTemplateResponse_errors = Lens.lens (\RenderUiTemplateResponse' {errors} -> errors) (\s@RenderUiTemplateResponse' {} a -> s {errors = a} :: RenderUiTemplateResponse) Prelude.. Lens.coerced

instance Prelude.NFData RenderUiTemplateResponse where
  rnf RenderUiTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf renderedContent
      `Prelude.seq` Prelude.rnf errors
