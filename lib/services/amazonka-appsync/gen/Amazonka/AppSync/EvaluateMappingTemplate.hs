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
-- Module      : Amazonka.AppSync.EvaluateMappingTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Evaluates a given template and returns the response. The mapping
-- template can be a request or response template.
--
-- Request templates take the incoming request after a GraphQL operation is
-- parsed and convert it into a request configuration for the selected data
-- source operation. Response templates interpret responses from the data
-- source and map it to the shape of the GraphQL field output type.
--
-- Mapping templates are written in the Apache Velocity Template Language
-- (VTL).
module Amazonka.AppSync.EvaluateMappingTemplate
  ( -- * Creating a Request
    EvaluateMappingTemplate (..),
    newEvaluateMappingTemplate,

    -- * Request Lenses
    evaluateMappingTemplate_template,
    evaluateMappingTemplate_context,

    -- * Destructuring the Response
    EvaluateMappingTemplateResponse (..),
    newEvaluateMappingTemplateResponse,

    -- * Response Lenses
    evaluateMappingTemplateResponse_error,
    evaluateMappingTemplateResponse_evaluationResult,
    evaluateMappingTemplateResponse_logs,
    evaluateMappingTemplateResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEvaluateMappingTemplate' smart constructor.
data EvaluateMappingTemplate = EvaluateMappingTemplate'
  { -- | The mapping template; this can be a request or response template. A
    -- @template@ is required for this action.
    template :: Prelude.Text,
    -- | The map that holds all of the contextual information for your resolver
    -- invocation. A @context@ is required for this action.
    context :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluateMappingTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'template', 'evaluateMappingTemplate_template' - The mapping template; this can be a request or response template. A
-- @template@ is required for this action.
--
-- 'context', 'evaluateMappingTemplate_context' - The map that holds all of the contextual information for your resolver
-- invocation. A @context@ is required for this action.
newEvaluateMappingTemplate ::
  -- | 'template'
  Prelude.Text ->
  -- | 'context'
  Prelude.Text ->
  EvaluateMappingTemplate
newEvaluateMappingTemplate pTemplate_ pContext_ =
  EvaluateMappingTemplate'
    { template = pTemplate_,
      context = pContext_
    }

-- | The mapping template; this can be a request or response template. A
-- @template@ is required for this action.
evaluateMappingTemplate_template :: Lens.Lens' EvaluateMappingTemplate Prelude.Text
evaluateMappingTemplate_template = Lens.lens (\EvaluateMappingTemplate' {template} -> template) (\s@EvaluateMappingTemplate' {} a -> s {template = a} :: EvaluateMappingTemplate)

-- | The map that holds all of the contextual information for your resolver
-- invocation. A @context@ is required for this action.
evaluateMappingTemplate_context :: Lens.Lens' EvaluateMappingTemplate Prelude.Text
evaluateMappingTemplate_context = Lens.lens (\EvaluateMappingTemplate' {context} -> context) (\s@EvaluateMappingTemplate' {} a -> s {context = a} :: EvaluateMappingTemplate)

instance Core.AWSRequest EvaluateMappingTemplate where
  type
    AWSResponse EvaluateMappingTemplate =
      EvaluateMappingTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          EvaluateMappingTemplateResponse'
            Prelude.<$> (x Data..?> "error")
            Prelude.<*> (x Data..?> "evaluationResult")
            Prelude.<*> (x Data..?> "logs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EvaluateMappingTemplate where
  hashWithSalt _salt EvaluateMappingTemplate' {..} =
    _salt `Prelude.hashWithSalt` template
      `Prelude.hashWithSalt` context

instance Prelude.NFData EvaluateMappingTemplate where
  rnf EvaluateMappingTemplate' {..} =
    Prelude.rnf template
      `Prelude.seq` Prelude.rnf context

instance Data.ToHeaders EvaluateMappingTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EvaluateMappingTemplate where
  toJSON EvaluateMappingTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("template" Data..= template),
            Prelude.Just ("context" Data..= context)
          ]
      )

instance Data.ToPath EvaluateMappingTemplate where
  toPath =
    Prelude.const "/v1/dataplane-evaluatetemplate"

instance Data.ToQuery EvaluateMappingTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEvaluateMappingTemplateResponse' smart constructor.
data EvaluateMappingTemplateResponse = EvaluateMappingTemplateResponse'
  { -- | The @ErrorDetail@ object.
    error :: Prelude.Maybe ErrorDetail,
    -- | The mapping template; this can be a request or response template.
    evaluationResult :: Prelude.Maybe Prelude.Text,
    -- | A list of logs that were generated by calls to @util.log.info@ and
    -- @util.log.error@ in the evaluated code.
    logs :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluateMappingTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'evaluateMappingTemplateResponse_error' - The @ErrorDetail@ object.
--
-- 'evaluationResult', 'evaluateMappingTemplateResponse_evaluationResult' - The mapping template; this can be a request or response template.
--
-- 'logs', 'evaluateMappingTemplateResponse_logs' - A list of logs that were generated by calls to @util.log.info@ and
-- @util.log.error@ in the evaluated code.
--
-- 'httpStatus', 'evaluateMappingTemplateResponse_httpStatus' - The response's http status code.
newEvaluateMappingTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EvaluateMappingTemplateResponse
newEvaluateMappingTemplateResponse pHttpStatus_ =
  EvaluateMappingTemplateResponse'
    { error =
        Prelude.Nothing,
      evaluationResult = Prelude.Nothing,
      logs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ErrorDetail@ object.
evaluateMappingTemplateResponse_error :: Lens.Lens' EvaluateMappingTemplateResponse (Prelude.Maybe ErrorDetail)
evaluateMappingTemplateResponse_error = Lens.lens (\EvaluateMappingTemplateResponse' {error} -> error) (\s@EvaluateMappingTemplateResponse' {} a -> s {error = a} :: EvaluateMappingTemplateResponse)

-- | The mapping template; this can be a request or response template.
evaluateMappingTemplateResponse_evaluationResult :: Lens.Lens' EvaluateMappingTemplateResponse (Prelude.Maybe Prelude.Text)
evaluateMappingTemplateResponse_evaluationResult = Lens.lens (\EvaluateMappingTemplateResponse' {evaluationResult} -> evaluationResult) (\s@EvaluateMappingTemplateResponse' {} a -> s {evaluationResult = a} :: EvaluateMappingTemplateResponse)

-- | A list of logs that were generated by calls to @util.log.info@ and
-- @util.log.error@ in the evaluated code.
evaluateMappingTemplateResponse_logs :: Lens.Lens' EvaluateMappingTemplateResponse (Prelude.Maybe [Prelude.Text])
evaluateMappingTemplateResponse_logs = Lens.lens (\EvaluateMappingTemplateResponse' {logs} -> logs) (\s@EvaluateMappingTemplateResponse' {} a -> s {logs = a} :: EvaluateMappingTemplateResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
evaluateMappingTemplateResponse_httpStatus :: Lens.Lens' EvaluateMappingTemplateResponse Prelude.Int
evaluateMappingTemplateResponse_httpStatus = Lens.lens (\EvaluateMappingTemplateResponse' {httpStatus} -> httpStatus) (\s@EvaluateMappingTemplateResponse' {} a -> s {httpStatus = a} :: EvaluateMappingTemplateResponse)

instance
  Prelude.NFData
    EvaluateMappingTemplateResponse
  where
  rnf EvaluateMappingTemplateResponse' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf evaluationResult
      `Prelude.seq` Prelude.rnf logs
      `Prelude.seq` Prelude.rnf httpStatus
