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
-- Module      : Amazonka.AppSync.EvaluateCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Evaluates the given code and returns the response. The code definition
-- requirements depend on the specified runtime. For @APPSYNC_JS@ runtimes,
-- the code defines the request and response functions. The request
-- function takes the incoming request after a GraphQL operation is parsed
-- and converts it into a request configuration for the selected data
-- source operation. The response function interprets responses from the
-- data source and maps it to the shape of the GraphQL field output type.
module Amazonka.AppSync.EvaluateCode
  ( -- * Creating a Request
    EvaluateCode (..),
    newEvaluateCode,

    -- * Request Lenses
    evaluateCode_function,
    evaluateCode_runtime,
    evaluateCode_code,
    evaluateCode_context,

    -- * Destructuring the Response
    EvaluateCodeResponse (..),
    newEvaluateCodeResponse,

    -- * Response Lenses
    evaluateCodeResponse_logs,
    evaluateCodeResponse_evaluationResult,
    evaluateCodeResponse_error,
    evaluateCodeResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEvaluateCode' smart constructor.
data EvaluateCode = EvaluateCode'
  { -- | The function within the code to be evaluated. If provided, the valid
    -- values are @request@ and @response@.
    function :: Prelude.Maybe Prelude.Text,
    -- | The runtime to be used when evaluating the code. Currently, only the
    -- @APPSYNC_JS@ runtime is supported.
    runtime :: AppSyncRuntime,
    -- | The code definition to be evaluated. Note that @code@ and @runtime@ are
    -- both required for this action. The @runtime@ value must be @APPSYNC_JS@.
    code :: Prelude.Text,
    -- | The map that holds all of the contextual information for your resolver
    -- invocation. A @context@ is required for this action.
    context :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluateCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'function', 'evaluateCode_function' - The function within the code to be evaluated. If provided, the valid
-- values are @request@ and @response@.
--
-- 'runtime', 'evaluateCode_runtime' - The runtime to be used when evaluating the code. Currently, only the
-- @APPSYNC_JS@ runtime is supported.
--
-- 'code', 'evaluateCode_code' - The code definition to be evaluated. Note that @code@ and @runtime@ are
-- both required for this action. The @runtime@ value must be @APPSYNC_JS@.
--
-- 'context', 'evaluateCode_context' - The map that holds all of the contextual information for your resolver
-- invocation. A @context@ is required for this action.
newEvaluateCode ::
  -- | 'runtime'
  AppSyncRuntime ->
  -- | 'code'
  Prelude.Text ->
  -- | 'context'
  Prelude.Text ->
  EvaluateCode
newEvaluateCode pRuntime_ pCode_ pContext_ =
  EvaluateCode'
    { function = Prelude.Nothing,
      runtime = pRuntime_,
      code = pCode_,
      context = pContext_
    }

-- | The function within the code to be evaluated. If provided, the valid
-- values are @request@ and @response@.
evaluateCode_function :: Lens.Lens' EvaluateCode (Prelude.Maybe Prelude.Text)
evaluateCode_function = Lens.lens (\EvaluateCode' {function} -> function) (\s@EvaluateCode' {} a -> s {function = a} :: EvaluateCode)

-- | The runtime to be used when evaluating the code. Currently, only the
-- @APPSYNC_JS@ runtime is supported.
evaluateCode_runtime :: Lens.Lens' EvaluateCode AppSyncRuntime
evaluateCode_runtime = Lens.lens (\EvaluateCode' {runtime} -> runtime) (\s@EvaluateCode' {} a -> s {runtime = a} :: EvaluateCode)

-- | The code definition to be evaluated. Note that @code@ and @runtime@ are
-- both required for this action. The @runtime@ value must be @APPSYNC_JS@.
evaluateCode_code :: Lens.Lens' EvaluateCode Prelude.Text
evaluateCode_code = Lens.lens (\EvaluateCode' {code} -> code) (\s@EvaluateCode' {} a -> s {code = a} :: EvaluateCode)

-- | The map that holds all of the contextual information for your resolver
-- invocation. A @context@ is required for this action.
evaluateCode_context :: Lens.Lens' EvaluateCode Prelude.Text
evaluateCode_context = Lens.lens (\EvaluateCode' {context} -> context) (\s@EvaluateCode' {} a -> s {context = a} :: EvaluateCode)

instance Core.AWSRequest EvaluateCode where
  type AWSResponse EvaluateCode = EvaluateCodeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          EvaluateCodeResponse'
            Prelude.<$> (x Data..?> "logs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "evaluationResult")
            Prelude.<*> (x Data..?> "error")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EvaluateCode where
  hashWithSalt _salt EvaluateCode' {..} =
    _salt `Prelude.hashWithSalt` function
      `Prelude.hashWithSalt` runtime
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` context

instance Prelude.NFData EvaluateCode where
  rnf EvaluateCode' {..} =
    Prelude.rnf function
      `Prelude.seq` Prelude.rnf runtime
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf context

instance Data.ToHeaders EvaluateCode where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EvaluateCode where
  toJSON EvaluateCode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("function" Data..=) Prelude.<$> function,
            Prelude.Just ("runtime" Data..= runtime),
            Prelude.Just ("code" Data..= code),
            Prelude.Just ("context" Data..= context)
          ]
      )

instance Data.ToPath EvaluateCode where
  toPath = Prelude.const "/v1/dataplane-evaluatecode"

instance Data.ToQuery EvaluateCode where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEvaluateCodeResponse' smart constructor.
data EvaluateCodeResponse = EvaluateCodeResponse'
  { -- | A list of logs that were generated by calls to @util.log.info@ and
    -- @util.log.error@ in the evaluated code.
    logs :: Prelude.Maybe [Prelude.Text],
    -- | The result of the evaluation operation.
    evaluationResult :: Prelude.Maybe Prelude.Text,
    -- | Contains the payload of the response error.
    error :: Prelude.Maybe EvaluateCodeErrorDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluateCodeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logs', 'evaluateCodeResponse_logs' - A list of logs that were generated by calls to @util.log.info@ and
-- @util.log.error@ in the evaluated code.
--
-- 'evaluationResult', 'evaluateCodeResponse_evaluationResult' - The result of the evaluation operation.
--
-- 'error', 'evaluateCodeResponse_error' - Contains the payload of the response error.
--
-- 'httpStatus', 'evaluateCodeResponse_httpStatus' - The response's http status code.
newEvaluateCodeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EvaluateCodeResponse
newEvaluateCodeResponse pHttpStatus_ =
  EvaluateCodeResponse'
    { logs = Prelude.Nothing,
      evaluationResult = Prelude.Nothing,
      error = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of logs that were generated by calls to @util.log.info@ and
-- @util.log.error@ in the evaluated code.
evaluateCodeResponse_logs :: Lens.Lens' EvaluateCodeResponse (Prelude.Maybe [Prelude.Text])
evaluateCodeResponse_logs = Lens.lens (\EvaluateCodeResponse' {logs} -> logs) (\s@EvaluateCodeResponse' {} a -> s {logs = a} :: EvaluateCodeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The result of the evaluation operation.
evaluateCodeResponse_evaluationResult :: Lens.Lens' EvaluateCodeResponse (Prelude.Maybe Prelude.Text)
evaluateCodeResponse_evaluationResult = Lens.lens (\EvaluateCodeResponse' {evaluationResult} -> evaluationResult) (\s@EvaluateCodeResponse' {} a -> s {evaluationResult = a} :: EvaluateCodeResponse)

-- | Contains the payload of the response error.
evaluateCodeResponse_error :: Lens.Lens' EvaluateCodeResponse (Prelude.Maybe EvaluateCodeErrorDetail)
evaluateCodeResponse_error = Lens.lens (\EvaluateCodeResponse' {error} -> error) (\s@EvaluateCodeResponse' {} a -> s {error = a} :: EvaluateCodeResponse)

-- | The response's http status code.
evaluateCodeResponse_httpStatus :: Lens.Lens' EvaluateCodeResponse Prelude.Int
evaluateCodeResponse_httpStatus = Lens.lens (\EvaluateCodeResponse' {httpStatus} -> httpStatus) (\s@EvaluateCodeResponse' {} a -> s {httpStatus = a} :: EvaluateCodeResponse)

instance Prelude.NFData EvaluateCodeResponse where
  rnf EvaluateCodeResponse' {..} =
    Prelude.rnf logs
      `Prelude.seq` Prelude.rnf evaluationResult
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf httpStatus
