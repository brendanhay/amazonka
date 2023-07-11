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
-- Module      : Amazonka.Lambda.PutFunctionCodeSigningConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the code signing configuration for the function. Changes to the
-- code signing configuration take effect the next time a user tries to
-- deploy a code package to the function.
module Amazonka.Lambda.PutFunctionCodeSigningConfig
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest PutFunctionCodeSigningConfig where
  type
    AWSResponse PutFunctionCodeSigningConfig =
      PutFunctionCodeSigningConfigResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutFunctionCodeSigningConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CodeSigningConfigArn")
            Prelude.<*> (x Data..:> "FunctionName")
      )

instance
  Prelude.Hashable
    PutFunctionCodeSigningConfig
  where
  hashWithSalt _salt PutFunctionCodeSigningConfig' {..} =
    _salt
      `Prelude.hashWithSalt` codeSigningConfigArn
      `Prelude.hashWithSalt` functionName

instance Prelude.NFData PutFunctionCodeSigningConfig where
  rnf PutFunctionCodeSigningConfig' {..} =
    Prelude.rnf codeSigningConfigArn
      `Prelude.seq` Prelude.rnf functionName

instance Data.ToHeaders PutFunctionCodeSigningConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON PutFunctionCodeSigningConfig where
  toJSON PutFunctionCodeSigningConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CodeSigningConfigArn"
                  Data..= codeSigningConfigArn
              )
          ]
      )

instance Data.ToPath PutFunctionCodeSigningConfig where
  toPath PutFunctionCodeSigningConfig' {..} =
    Prelude.mconcat
      [ "/2020-06-30/functions/",
        Data.toBS functionName,
        "/code-signing-config"
      ]

instance Data.ToQuery PutFunctionCodeSigningConfig where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf PutFunctionCodeSigningConfigResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf codeSigningConfigArn
      `Prelude.seq` Prelude.rnf functionName
