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
-- Module      : Network.AWS.AppSync.GetFunction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a @Function@.
module Network.AWS.AppSync.GetFunction
  ( -- * Creating a Request
    GetFunction (..),
    newGetFunction,

    -- * Request Lenses
    getFunction_apiId,
    getFunction_functionId,

    -- * Destructuring the Response
    GetFunctionResponse (..),
    newGetFunctionResponse,

    -- * Response Lenses
    getFunctionResponse_functionConfiguration,
    getFunctionResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetFunction' smart constructor.
data GetFunction = GetFunction'
  { -- | The GraphQL API ID.
    apiId :: Core.Text,
    -- | The @Function@ ID.
    functionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'getFunction_apiId' - The GraphQL API ID.
--
-- 'functionId', 'getFunction_functionId' - The @Function@ ID.
newGetFunction ::
  -- | 'apiId'
  Core.Text ->
  -- | 'functionId'
  Core.Text ->
  GetFunction
newGetFunction pApiId_ pFunctionId_ =
  GetFunction'
    { apiId = pApiId_,
      functionId = pFunctionId_
    }

-- | The GraphQL API ID.
getFunction_apiId :: Lens.Lens' GetFunction Core.Text
getFunction_apiId = Lens.lens (\GetFunction' {apiId} -> apiId) (\s@GetFunction' {} a -> s {apiId = a} :: GetFunction)

-- | The @Function@ ID.
getFunction_functionId :: Lens.Lens' GetFunction Core.Text
getFunction_functionId = Lens.lens (\GetFunction' {functionId} -> functionId) (\s@GetFunction' {} a -> s {functionId = a} :: GetFunction)

instance Core.AWSRequest GetFunction where
  type AWSResponse GetFunction = GetFunctionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFunctionResponse'
            Core.<$> (x Core..?> "functionConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetFunction

instance Core.NFData GetFunction

instance Core.ToHeaders GetFunction where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetFunction where
  toPath GetFunction' {..} =
    Core.mconcat
      [ "/v1/apis/",
        Core.toBS apiId,
        "/functions/",
        Core.toBS functionId
      ]

instance Core.ToQuery GetFunction where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetFunctionResponse' smart constructor.
data GetFunctionResponse = GetFunctionResponse'
  { -- | The @Function@ object.
    functionConfiguration :: Core.Maybe FunctionConfiguration,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionConfiguration', 'getFunctionResponse_functionConfiguration' - The @Function@ object.
--
-- 'httpStatus', 'getFunctionResponse_httpStatus' - The response's http status code.
newGetFunctionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetFunctionResponse
newGetFunctionResponse pHttpStatus_ =
  GetFunctionResponse'
    { functionConfiguration =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @Function@ object.
getFunctionResponse_functionConfiguration :: Lens.Lens' GetFunctionResponse (Core.Maybe FunctionConfiguration)
getFunctionResponse_functionConfiguration = Lens.lens (\GetFunctionResponse' {functionConfiguration} -> functionConfiguration) (\s@GetFunctionResponse' {} a -> s {functionConfiguration = a} :: GetFunctionResponse)

-- | The response's http status code.
getFunctionResponse_httpStatus :: Lens.Lens' GetFunctionResponse Core.Int
getFunctionResponse_httpStatus = Lens.lens (\GetFunctionResponse' {httpStatus} -> httpStatus) (\s@GetFunctionResponse' {} a -> s {httpStatus = a} :: GetFunctionResponse)

instance Core.NFData GetFunctionResponse
