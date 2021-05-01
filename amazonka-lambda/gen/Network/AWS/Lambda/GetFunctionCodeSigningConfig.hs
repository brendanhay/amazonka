{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Lambda.GetFunctionCodeSigningConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the code signing configuration for the specified function.
module Network.AWS.Lambda.GetFunctionCodeSigningConfig
  ( -- * Creating a Request
    GetFunctionCodeSigningConfig (..),
    newGetFunctionCodeSigningConfig,

    -- * Request Lenses
    getFunctionCodeSigningConfig_functionName,

    -- * Destructuring the Response
    GetFunctionCodeSigningConfigResponse (..),
    newGetFunctionCodeSigningConfigResponse,

    -- * Response Lenses
    getFunctionCodeSigningConfigResponse_httpStatus,
    getFunctionCodeSigningConfigResponse_codeSigningConfigArn,
    getFunctionCodeSigningConfigResponse_functionName,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetFunctionCodeSigningConfig' smart constructor.
data GetFunctionCodeSigningConfig = GetFunctionCodeSigningConfig'
  { -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @MyFunction@.
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
    --
    -- -   __Partial ARN__ - @123456789012:function:MyFunction@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetFunctionCodeSigningConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionName', 'getFunctionCodeSigningConfig_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @MyFunction@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
--
-- -   __Partial ARN__ - @123456789012:function:MyFunction@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
newGetFunctionCodeSigningConfig ::
  -- | 'functionName'
  Prelude.Text ->
  GetFunctionCodeSigningConfig
newGetFunctionCodeSigningConfig pFunctionName_ =
  GetFunctionCodeSigningConfig'
    { functionName =
        pFunctionName_
    }

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @MyFunction@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
--
-- -   __Partial ARN__ - @123456789012:function:MyFunction@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
getFunctionCodeSigningConfig_functionName :: Lens.Lens' GetFunctionCodeSigningConfig Prelude.Text
getFunctionCodeSigningConfig_functionName = Lens.lens (\GetFunctionCodeSigningConfig' {functionName} -> functionName) (\s@GetFunctionCodeSigningConfig' {} a -> s {functionName = a} :: GetFunctionCodeSigningConfig)

instance
  Prelude.AWSRequest
    GetFunctionCodeSigningConfig
  where
  type
    Rs GetFunctionCodeSigningConfig =
      GetFunctionCodeSigningConfigResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFunctionCodeSigningConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "CodeSigningConfigArn")
            Prelude.<*> (x Prelude..:> "FunctionName")
      )

instance
  Prelude.Hashable
    GetFunctionCodeSigningConfig

instance Prelude.NFData GetFunctionCodeSigningConfig

instance
  Prelude.ToHeaders
    GetFunctionCodeSigningConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetFunctionCodeSigningConfig where
  toPath GetFunctionCodeSigningConfig' {..} =
    Prelude.mconcat
      [ "/2020-06-30/functions/",
        Prelude.toBS functionName,
        "/code-signing-config"
      ]

instance Prelude.ToQuery GetFunctionCodeSigningConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFunctionCodeSigningConfigResponse' smart constructor.
data GetFunctionCodeSigningConfigResponse = GetFunctionCodeSigningConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The The Amazon Resource Name (ARN) of the code signing configuration.
    codeSigningConfigArn :: Prelude.Text,
    -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @MyFunction@.
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
    --
    -- -   __Partial ARN__ - @123456789012:function:MyFunction@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetFunctionCodeSigningConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getFunctionCodeSigningConfigResponse_httpStatus' - The response's http status code.
--
-- 'codeSigningConfigArn', 'getFunctionCodeSigningConfigResponse_codeSigningConfigArn' - The The Amazon Resource Name (ARN) of the code signing configuration.
--
-- 'functionName', 'getFunctionCodeSigningConfigResponse_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @MyFunction@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
--
-- -   __Partial ARN__ - @123456789012:function:MyFunction@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
newGetFunctionCodeSigningConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'codeSigningConfigArn'
  Prelude.Text ->
  -- | 'functionName'
  Prelude.Text ->
  GetFunctionCodeSigningConfigResponse
newGetFunctionCodeSigningConfigResponse
  pHttpStatus_
  pCodeSigningConfigArn_
  pFunctionName_ =
    GetFunctionCodeSigningConfigResponse'
      { httpStatus =
          pHttpStatus_,
        codeSigningConfigArn =
          pCodeSigningConfigArn_,
        functionName = pFunctionName_
      }

-- | The response's http status code.
getFunctionCodeSigningConfigResponse_httpStatus :: Lens.Lens' GetFunctionCodeSigningConfigResponse Prelude.Int
getFunctionCodeSigningConfigResponse_httpStatus = Lens.lens (\GetFunctionCodeSigningConfigResponse' {httpStatus} -> httpStatus) (\s@GetFunctionCodeSigningConfigResponse' {} a -> s {httpStatus = a} :: GetFunctionCodeSigningConfigResponse)

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
getFunctionCodeSigningConfigResponse_codeSigningConfigArn :: Lens.Lens' GetFunctionCodeSigningConfigResponse Prelude.Text
getFunctionCodeSigningConfigResponse_codeSigningConfigArn = Lens.lens (\GetFunctionCodeSigningConfigResponse' {codeSigningConfigArn} -> codeSigningConfigArn) (\s@GetFunctionCodeSigningConfigResponse' {} a -> s {codeSigningConfigArn = a} :: GetFunctionCodeSigningConfigResponse)

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @MyFunction@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
--
-- -   __Partial ARN__ - @123456789012:function:MyFunction@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
getFunctionCodeSigningConfigResponse_functionName :: Lens.Lens' GetFunctionCodeSigningConfigResponse Prelude.Text
getFunctionCodeSigningConfigResponse_functionName = Lens.lens (\GetFunctionCodeSigningConfigResponse' {functionName} -> functionName) (\s@GetFunctionCodeSigningConfigResponse' {} a -> s {functionName = a} :: GetFunctionCodeSigningConfigResponse)

instance
  Prelude.NFData
    GetFunctionCodeSigningConfigResponse
