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

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutFunctionCodeSigningConfig' smart constructor.
data PutFunctionCodeSigningConfig = PutFunctionCodeSigningConfig'
  { -- | The The Amazon Resource Name (ARN) of the code signing configuration.
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
  Prelude.Text ->
  -- | 'functionName'
  Prelude.Text ->
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
putFunctionCodeSigningConfig_codeSigningConfigArn :: Lens.Lens' PutFunctionCodeSigningConfig Prelude.Text
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
putFunctionCodeSigningConfig_functionName :: Lens.Lens' PutFunctionCodeSigningConfig Prelude.Text
putFunctionCodeSigningConfig_functionName = Lens.lens (\PutFunctionCodeSigningConfig' {functionName} -> functionName) (\s@PutFunctionCodeSigningConfig' {} a -> s {functionName = a} :: PutFunctionCodeSigningConfig)

instance
  Prelude.AWSRequest
    PutFunctionCodeSigningConfig
  where
  type
    Rs PutFunctionCodeSigningConfig =
      PutFunctionCodeSigningConfigResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutFunctionCodeSigningConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "CodeSigningConfigArn")
            Prelude.<*> (x Prelude..:> "FunctionName")
      )

instance
  Prelude.Hashable
    PutFunctionCodeSigningConfig

instance Prelude.NFData PutFunctionCodeSigningConfig

instance
  Prelude.ToHeaders
    PutFunctionCodeSigningConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON PutFunctionCodeSigningConfig where
  toJSON PutFunctionCodeSigningConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CodeSigningConfigArn"
                  Prelude..= codeSigningConfigArn
              )
          ]
      )

instance Prelude.ToPath PutFunctionCodeSigningConfig where
  toPath PutFunctionCodeSigningConfig' {..} =
    Prelude.mconcat
      [ "/2020-06-30/functions/",
        Prelude.toBS functionName,
        "/code-signing-config"
      ]

instance Prelude.ToQuery PutFunctionCodeSigningConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutFunctionCodeSigningConfigResponse' smart constructor.
data PutFunctionCodeSigningConfigResponse = PutFunctionCodeSigningConfigResponse'
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
  Prelude.Int ->
  -- | 'codeSigningConfigArn'
  Prelude.Text ->
  -- | 'functionName'
  Prelude.Text ->
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
putFunctionCodeSigningConfigResponse_httpStatus :: Lens.Lens' PutFunctionCodeSigningConfigResponse Prelude.Int
putFunctionCodeSigningConfigResponse_httpStatus = Lens.lens (\PutFunctionCodeSigningConfigResponse' {httpStatus} -> httpStatus) (\s@PutFunctionCodeSigningConfigResponse' {} a -> s {httpStatus = a} :: PutFunctionCodeSigningConfigResponse)

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
putFunctionCodeSigningConfigResponse_codeSigningConfigArn :: Lens.Lens' PutFunctionCodeSigningConfigResponse Prelude.Text
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
putFunctionCodeSigningConfigResponse_functionName :: Lens.Lens' PutFunctionCodeSigningConfigResponse Prelude.Text
putFunctionCodeSigningConfigResponse_functionName = Lens.lens (\PutFunctionCodeSigningConfigResponse' {functionName} -> functionName) (\s@PutFunctionCodeSigningConfigResponse' {} a -> s {functionName = a} :: PutFunctionCodeSigningConfigResponse)

instance
  Prelude.NFData
    PutFunctionCodeSigningConfigResponse
