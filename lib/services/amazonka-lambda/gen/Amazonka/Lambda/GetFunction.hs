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
-- Module      : Amazonka.Lambda.GetFunction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the function or function version, with a link
-- to download the deployment package that\'s valid for 10 minutes. If you
-- specify a function version, only details that are specific to that
-- version are returned.
module Amazonka.Lambda.GetFunction
  ( -- * Creating a Request
    GetFunction (..),
    newGetFunction,

    -- * Request Lenses
    getFunction_qualifier,
    getFunction_functionName,

    -- * Destructuring the Response
    GetFunctionResponse (..),
    newGetFunctionResponse,

    -- * Response Lenses
    getFunctionResponse_tags,
    getFunctionResponse_code,
    getFunctionResponse_configuration,
    getFunctionResponse_concurrency,
    getFunctionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFunction' smart constructor.
data GetFunction = GetFunction'
  { -- | Specify a version or alias to get details about a published version of
    -- the function.
    qualifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the Lambda function, version, or alias.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
    --     (with alias).
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ - @123456789012:function:my-function@.
    --
    -- You can append a version number or alias to any of the formats. The
    -- length constraint applies only to the full ARN. If you specify only the
    -- function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualifier', 'getFunction_qualifier' - Specify a version or alias to get details about a published version of
-- the function.
--
-- 'functionName', 'getFunction_functionName' - The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
--     (with alias).
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- You can append a version number or alias to any of the formats. The
-- length constraint applies only to the full ARN. If you specify only the
-- function name, it is limited to 64 characters in length.
newGetFunction ::
  -- | 'functionName'
  Prelude.Text ->
  GetFunction
newGetFunction pFunctionName_ =
  GetFunction'
    { qualifier = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | Specify a version or alias to get details about a published version of
-- the function.
getFunction_qualifier :: Lens.Lens' GetFunction (Prelude.Maybe Prelude.Text)
getFunction_qualifier = Lens.lens (\GetFunction' {qualifier} -> qualifier) (\s@GetFunction' {} a -> s {qualifier = a} :: GetFunction)

-- | The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
--     (with alias).
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- You can append a version number or alias to any of the formats. The
-- length constraint applies only to the full ARN. If you specify only the
-- function name, it is limited to 64 characters in length.
getFunction_functionName :: Lens.Lens' GetFunction Prelude.Text
getFunction_functionName = Lens.lens (\GetFunction' {functionName} -> functionName) (\s@GetFunction' {} a -> s {functionName = a} :: GetFunction)

instance Core.AWSRequest GetFunction where
  type AWSResponse GetFunction = GetFunctionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFunctionResponse'
            Prelude.<$> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Code")
            Prelude.<*> (x Core..?> "Configuration")
            Prelude.<*> (x Core..?> "Concurrency")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFunction where
  hashWithSalt _salt GetFunction' {..} =
    _salt `Prelude.hashWithSalt` qualifier
      `Prelude.hashWithSalt` functionName

instance Prelude.NFData GetFunction where
  rnf GetFunction' {..} =
    Prelude.rnf qualifier
      `Prelude.seq` Prelude.rnf functionName

instance Core.ToHeaders GetFunction where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetFunction where
  toPath GetFunction' {..} =
    Prelude.mconcat
      ["/2015-03-31/functions/", Core.toBS functionName]

instance Core.ToQuery GetFunction where
  toQuery GetFunction' {..} =
    Prelude.mconcat ["Qualifier" Core.=: qualifier]

-- | /See:/ 'newGetFunctionResponse' smart constructor.
data GetFunctionResponse = GetFunctionResponse'
  { -- | The function\'s
    -- <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The deployment package of the function or version.
    code :: Prelude.Maybe FunctionCodeLocation,
    -- | The configuration of the function or version.
    configuration :: Prelude.Maybe FunctionConfiguration,
    -- | The function\'s
    -- <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html reserved concurrency>.
    concurrency :: Prelude.Maybe Concurrency,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getFunctionResponse_tags' - The function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags>.
--
-- 'code', 'getFunctionResponse_code' - The deployment package of the function or version.
--
-- 'configuration', 'getFunctionResponse_configuration' - The configuration of the function or version.
--
-- 'concurrency', 'getFunctionResponse_concurrency' - The function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html reserved concurrency>.
--
-- 'httpStatus', 'getFunctionResponse_httpStatus' - The response's http status code.
newGetFunctionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFunctionResponse
newGetFunctionResponse pHttpStatus_ =
  GetFunctionResponse'
    { tags = Prelude.Nothing,
      code = Prelude.Nothing,
      configuration = Prelude.Nothing,
      concurrency = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags>.
getFunctionResponse_tags :: Lens.Lens' GetFunctionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getFunctionResponse_tags = Lens.lens (\GetFunctionResponse' {tags} -> tags) (\s@GetFunctionResponse' {} a -> s {tags = a} :: GetFunctionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The deployment package of the function or version.
getFunctionResponse_code :: Lens.Lens' GetFunctionResponse (Prelude.Maybe FunctionCodeLocation)
getFunctionResponse_code = Lens.lens (\GetFunctionResponse' {code} -> code) (\s@GetFunctionResponse' {} a -> s {code = a} :: GetFunctionResponse)

-- | The configuration of the function or version.
getFunctionResponse_configuration :: Lens.Lens' GetFunctionResponse (Prelude.Maybe FunctionConfiguration)
getFunctionResponse_configuration = Lens.lens (\GetFunctionResponse' {configuration} -> configuration) (\s@GetFunctionResponse' {} a -> s {configuration = a} :: GetFunctionResponse)

-- | The function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html reserved concurrency>.
getFunctionResponse_concurrency :: Lens.Lens' GetFunctionResponse (Prelude.Maybe Concurrency)
getFunctionResponse_concurrency = Lens.lens (\GetFunctionResponse' {concurrency} -> concurrency) (\s@GetFunctionResponse' {} a -> s {concurrency = a} :: GetFunctionResponse)

-- | The response's http status code.
getFunctionResponse_httpStatus :: Lens.Lens' GetFunctionResponse Prelude.Int
getFunctionResponse_httpStatus = Lens.lens (\GetFunctionResponse' {httpStatus} -> httpStatus) (\s@GetFunctionResponse' {} a -> s {httpStatus = a} :: GetFunctionResponse)

instance Prelude.NFData GetFunctionResponse where
  rnf GetFunctionResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf concurrency
      `Prelude.seq` Prelude.rnf httpStatus
