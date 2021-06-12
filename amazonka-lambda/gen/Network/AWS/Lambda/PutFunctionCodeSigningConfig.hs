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
-- Module      : Network.AWS.Lambda.PutFunctionCodeSigningConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the code signing configuration for the function. Changes to the
-- code signing configuration take effect the next time a user tries to
-- deploy a code package to the function.
module Network.AWS.Lambda.PutFunctionCodeSigningConfig
  ( -- * Creating a Request
    PutFunctionCodeSigningConfig (..),
    newPutFunctionCodeSigningConfig,

    -- * Request Lenses
    putFunctionCodeSigningConfig_codeSigningConfigArn,
    putFunctionCodeSigningConfig_functionName,

    -- * Destructuring the Response
    PutFunctionCodeSigningConfigResponse (..),
    newPutFunctionCodeSigningConfigResponse,

    -- * Response Lenses
    putFunctionCodeSigningConfigResponse_httpStatus,
    putFunctionCodeSigningConfigResponse_codeSigningConfigArn,
    putFunctionCodeSigningConfigResponse_functionName,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutFunctionCodeSigningConfig' smart constructor.
data PutFunctionCodeSigningConfig = PutFunctionCodeSigningConfig'
  { -- | The The Amazon Resource Name (ARN) of the code signing configuration.
    codeSigningConfigArn :: Core.Text,
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
    functionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutFunctionCodeSigningConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeSigningConfigArn', 'putFunctionCodeSigningConfig_codeSigningConfigArn' - The The Amazon Resource Name (ARN) of the code signing configuration.
--
-- 'functionName', 'putFunctionCodeSigningConfig_functionName' - The name of the Lambda function.
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
newPutFunctionCodeSigningConfig ::
  -- | 'codeSigningConfigArn'
  Core.Text ->
  -- | 'functionName'
  Core.Text ->
  PutFunctionCodeSigningConfig
newPutFunctionCodeSigningConfig
  pCodeSigningConfigArn_
  pFunctionName_ =
    PutFunctionCodeSigningConfig'
      { codeSigningConfigArn =
          pCodeSigningConfigArn_,
        functionName = pFunctionName_
      }

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
putFunctionCodeSigningConfig_codeSigningConfigArn :: Lens.Lens' PutFunctionCodeSigningConfig Core.Text
putFunctionCodeSigningConfig_codeSigningConfigArn = Lens.lens (\PutFunctionCodeSigningConfig' {codeSigningConfigArn} -> codeSigningConfigArn) (\s@PutFunctionCodeSigningConfig' {} a -> s {codeSigningConfigArn = a} :: PutFunctionCodeSigningConfig)

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
putFunctionCodeSigningConfig_functionName :: Lens.Lens' PutFunctionCodeSigningConfig Core.Text
putFunctionCodeSigningConfig_functionName = Lens.lens (\PutFunctionCodeSigningConfig' {functionName} -> functionName) (\s@PutFunctionCodeSigningConfig' {} a -> s {functionName = a} :: PutFunctionCodeSigningConfig)

instance Core.AWSRequest PutFunctionCodeSigningConfig where
  type
    AWSResponse PutFunctionCodeSigningConfig =
      PutFunctionCodeSigningConfigResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutFunctionCodeSigningConfigResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "CodeSigningConfigArn")
            Core.<*> (x Core..:> "FunctionName")
      )

instance Core.Hashable PutFunctionCodeSigningConfig

instance Core.NFData PutFunctionCodeSigningConfig

instance Core.ToHeaders PutFunctionCodeSigningConfig where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON PutFunctionCodeSigningConfig where
  toJSON PutFunctionCodeSigningConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "CodeSigningConfigArn"
                  Core..= codeSigningConfigArn
              )
          ]
      )

instance Core.ToPath PutFunctionCodeSigningConfig where
  toPath PutFunctionCodeSigningConfig' {..} =
    Core.mconcat
      [ "/2020-06-30/functions/",
        Core.toBS functionName,
        "/code-signing-config"
      ]

instance Core.ToQuery PutFunctionCodeSigningConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutFunctionCodeSigningConfigResponse' smart constructor.
data PutFunctionCodeSigningConfigResponse = PutFunctionCodeSigningConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The The Amazon Resource Name (ARN) of the code signing configuration.
    codeSigningConfigArn :: Core.Text,
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
    functionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutFunctionCodeSigningConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putFunctionCodeSigningConfigResponse_httpStatus' - The response's http status code.
--
-- 'codeSigningConfigArn', 'putFunctionCodeSigningConfigResponse_codeSigningConfigArn' - The The Amazon Resource Name (ARN) of the code signing configuration.
--
-- 'functionName', 'putFunctionCodeSigningConfigResponse_functionName' - The name of the Lambda function.
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
newPutFunctionCodeSigningConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'codeSigningConfigArn'
  Core.Text ->
  -- | 'functionName'
  Core.Text ->
  PutFunctionCodeSigningConfigResponse
newPutFunctionCodeSigningConfigResponse
  pHttpStatus_
  pCodeSigningConfigArn_
  pFunctionName_ =
    PutFunctionCodeSigningConfigResponse'
      { httpStatus =
          pHttpStatus_,
        codeSigningConfigArn =
          pCodeSigningConfigArn_,
        functionName = pFunctionName_
      }

-- | The response's http status code.
putFunctionCodeSigningConfigResponse_httpStatus :: Lens.Lens' PutFunctionCodeSigningConfigResponse Core.Int
putFunctionCodeSigningConfigResponse_httpStatus = Lens.lens (\PutFunctionCodeSigningConfigResponse' {httpStatus} -> httpStatus) (\s@PutFunctionCodeSigningConfigResponse' {} a -> s {httpStatus = a} :: PutFunctionCodeSigningConfigResponse)

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
putFunctionCodeSigningConfigResponse_codeSigningConfigArn :: Lens.Lens' PutFunctionCodeSigningConfigResponse Core.Text
putFunctionCodeSigningConfigResponse_codeSigningConfigArn = Lens.lens (\PutFunctionCodeSigningConfigResponse' {codeSigningConfigArn} -> codeSigningConfigArn) (\s@PutFunctionCodeSigningConfigResponse' {} a -> s {codeSigningConfigArn = a} :: PutFunctionCodeSigningConfigResponse)

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
putFunctionCodeSigningConfigResponse_functionName :: Lens.Lens' PutFunctionCodeSigningConfigResponse Core.Text
putFunctionCodeSigningConfigResponse_functionName = Lens.lens (\PutFunctionCodeSigningConfigResponse' {functionName} -> functionName) (\s@PutFunctionCodeSigningConfigResponse' {} a -> s {functionName = a} :: PutFunctionCodeSigningConfigResponse)

instance
  Core.NFData
    PutFunctionCodeSigningConfigResponse
