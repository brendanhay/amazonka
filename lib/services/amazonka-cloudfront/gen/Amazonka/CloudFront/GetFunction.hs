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
-- Module      : Amazonka.CloudFront.GetFunction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the code of a CloudFront function. To get configuration information
-- and metadata about a function, use @DescribeFunction@.
--
-- To get a function\'s code, you must provide the function\'s name and
-- stage. To get these values, you can use @ListFunctions@.
module Amazonka.CloudFront.GetFunction
  ( -- * Creating a Request
    GetFunction (..),
    newGetFunction,

    -- * Request Lenses
    getFunction_stage,
    getFunction_name,

    -- * Destructuring the Response
    GetFunctionResponse (..),
    newGetFunctionResponse,

    -- * Response Lenses
    getFunctionResponse_contentType,
    getFunctionResponse_eTag,
    getFunctionResponse_functionCode,
    getFunctionResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFunction' smart constructor.
data GetFunction = GetFunction'
  { -- | The function\'s stage, either @DEVELOPMENT@ or @LIVE@.
    stage :: Prelude.Maybe FunctionStage,
    -- | The name of the function whose code you are getting.
    name :: Prelude.Text
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
-- 'stage', 'getFunction_stage' - The function\'s stage, either @DEVELOPMENT@ or @LIVE@.
--
-- 'name', 'getFunction_name' - The name of the function whose code you are getting.
newGetFunction ::
  -- | 'name'
  Prelude.Text ->
  GetFunction
newGetFunction pName_ =
  GetFunction'
    { stage = Prelude.Nothing,
      name = pName_
    }

-- | The function\'s stage, either @DEVELOPMENT@ or @LIVE@.
getFunction_stage :: Lens.Lens' GetFunction (Prelude.Maybe FunctionStage)
getFunction_stage = Lens.lens (\GetFunction' {stage} -> stage) (\s@GetFunction' {} a -> s {stage = a} :: GetFunction)

-- | The name of the function whose code you are getting.
getFunction_name :: Lens.Lens' GetFunction Prelude.Text
getFunction_name = Lens.lens (\GetFunction' {name} -> name) (\s@GetFunction' {} a -> s {name = a} :: GetFunction)

instance Core.AWSRequest GetFunction where
  type AWSResponse GetFunction = GetFunctionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBytes
      ( \s h x ->
          GetFunctionResponse'
            Prelude.<$> (h Data..#? "Content-Type")
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFunction where
  hashWithSalt _salt GetFunction' {..} =
    _salt
      `Prelude.hashWithSalt` stage
      `Prelude.hashWithSalt` name

instance Prelude.NFData GetFunction where
  rnf GetFunction' {..} =
    Prelude.rnf stage `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders GetFunction where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetFunction where
  toPath GetFunction' {..} =
    Prelude.mconcat
      ["/2020-05-31/function/", Data.toBS name]

instance Data.ToQuery GetFunction where
  toQuery GetFunction' {..} =
    Prelude.mconcat ["Stage" Data.=: stage]

-- | /See:/ 'newGetFunctionResponse' smart constructor.
data GetFunctionResponse = GetFunctionResponse'
  { -- | The content type (media type) of the response.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The version identifier for the current version of the CloudFront
    -- function.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The function code of a CloudFront function.
    functionCode :: Prelude.Maybe (Data.Sensitive Prelude.ByteString),
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
-- 'contentType', 'getFunctionResponse_contentType' - The content type (media type) of the response.
--
-- 'eTag', 'getFunctionResponse_eTag' - The version identifier for the current version of the CloudFront
-- function.
--
-- 'functionCode', 'getFunctionResponse_functionCode' - The function code of a CloudFront function.
--
-- 'httpStatus', 'getFunctionResponse_httpStatus' - The response's http status code.
newGetFunctionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFunctionResponse
newGetFunctionResponse pHttpStatus_ =
  GetFunctionResponse'
    { contentType = Prelude.Nothing,
      eTag = Prelude.Nothing,
      functionCode = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The content type (media type) of the response.
getFunctionResponse_contentType :: Lens.Lens' GetFunctionResponse (Prelude.Maybe Prelude.Text)
getFunctionResponse_contentType = Lens.lens (\GetFunctionResponse' {contentType} -> contentType) (\s@GetFunctionResponse' {} a -> s {contentType = a} :: GetFunctionResponse)

-- | The version identifier for the current version of the CloudFront
-- function.
getFunctionResponse_eTag :: Lens.Lens' GetFunctionResponse (Prelude.Maybe Prelude.Text)
getFunctionResponse_eTag = Lens.lens (\GetFunctionResponse' {eTag} -> eTag) (\s@GetFunctionResponse' {} a -> s {eTag = a} :: GetFunctionResponse)

-- | The function code of a CloudFront function.
getFunctionResponse_functionCode :: Lens.Lens' GetFunctionResponse (Prelude.Maybe Prelude.ByteString)
getFunctionResponse_functionCode = Lens.lens (\GetFunctionResponse' {functionCode} -> functionCode) (\s@GetFunctionResponse' {} a -> s {functionCode = a} :: GetFunctionResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
getFunctionResponse_httpStatus :: Lens.Lens' GetFunctionResponse Prelude.Int
getFunctionResponse_httpStatus = Lens.lens (\GetFunctionResponse' {httpStatus} -> httpStatus) (\s@GetFunctionResponse' {} a -> s {httpStatus = a} :: GetFunctionResponse)

instance Prelude.NFData GetFunctionResponse where
  rnf GetFunctionResponse' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf functionCode
      `Prelude.seq` Prelude.rnf httpStatus
