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
-- Module      : Amazonka.Lambda.GetRuntimeManagementConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the runtime management configuration for a function\'s
-- version. If the runtime update mode is __Manual__, this includes the ARN
-- of the runtime version and the runtime update mode. If the runtime
-- update mode is __Auto__ or __Function update__, this includes the
-- runtime update mode and @null@ is returned for the ARN. For more
-- information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-update.html Runtime updates>.
module Amazonka.Lambda.GetRuntimeManagementConfig
  ( -- * Creating a Request
    GetRuntimeManagementConfig (..),
    newGetRuntimeManagementConfig,

    -- * Request Lenses
    getRuntimeManagementConfig_qualifier,
    getRuntimeManagementConfig_functionName,

    -- * Destructuring the Response
    GetRuntimeManagementConfigResponse (..),
    newGetRuntimeManagementConfigResponse,

    -- * Response Lenses
    getRuntimeManagementConfigResponse_functionArn,
    getRuntimeManagementConfigResponse_runtimeVersionArn,
    getRuntimeManagementConfigResponse_updateRuntimeOn,
    getRuntimeManagementConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRuntimeManagementConfig' smart constructor.
data GetRuntimeManagementConfig = GetRuntimeManagementConfig'
  { -- | Specify a version of the function. This can be @$LATEST@ or a published
    -- version number. If no value is specified, the configuration for the
    -- @$LATEST@ version is returned.
    qualifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ – @my-function@.
    --
    -- -   __Function ARN__ –
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ – @123456789012:function:my-function@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRuntimeManagementConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualifier', 'getRuntimeManagementConfig_qualifier' - Specify a version of the function. This can be @$LATEST@ or a published
-- version number. If no value is specified, the configuration for the
-- @$LATEST@ version is returned.
--
-- 'functionName', 'getRuntimeManagementConfig_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@.
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
newGetRuntimeManagementConfig ::
  -- | 'functionName'
  Prelude.Text ->
  GetRuntimeManagementConfig
newGetRuntimeManagementConfig pFunctionName_ =
  GetRuntimeManagementConfig'
    { qualifier =
        Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | Specify a version of the function. This can be @$LATEST@ or a published
-- version number. If no value is specified, the configuration for the
-- @$LATEST@ version is returned.
getRuntimeManagementConfig_qualifier :: Lens.Lens' GetRuntimeManagementConfig (Prelude.Maybe Prelude.Text)
getRuntimeManagementConfig_qualifier = Lens.lens (\GetRuntimeManagementConfig' {qualifier} -> qualifier) (\s@GetRuntimeManagementConfig' {} a -> s {qualifier = a} :: GetRuntimeManagementConfig)

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@.
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
getRuntimeManagementConfig_functionName :: Lens.Lens' GetRuntimeManagementConfig Prelude.Text
getRuntimeManagementConfig_functionName = Lens.lens (\GetRuntimeManagementConfig' {functionName} -> functionName) (\s@GetRuntimeManagementConfig' {} a -> s {functionName = a} :: GetRuntimeManagementConfig)

instance Core.AWSRequest GetRuntimeManagementConfig where
  type
    AWSResponse GetRuntimeManagementConfig =
      GetRuntimeManagementConfigResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRuntimeManagementConfigResponse'
            Prelude.<$> (x Data..?> "FunctionArn")
            Prelude.<*> (x Data..?> "RuntimeVersionArn")
            Prelude.<*> (x Data..?> "UpdateRuntimeOn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRuntimeManagementConfig where
  hashWithSalt _salt GetRuntimeManagementConfig' {..} =
    _salt
      `Prelude.hashWithSalt` qualifier
      `Prelude.hashWithSalt` functionName

instance Prelude.NFData GetRuntimeManagementConfig where
  rnf GetRuntimeManagementConfig' {..} =
    Prelude.rnf qualifier
      `Prelude.seq` Prelude.rnf functionName

instance Data.ToHeaders GetRuntimeManagementConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetRuntimeManagementConfig where
  toPath GetRuntimeManagementConfig' {..} =
    Prelude.mconcat
      [ "/2021-07-20/functions/",
        Data.toBS functionName,
        "/runtime-management-config"
      ]

instance Data.ToQuery GetRuntimeManagementConfig where
  toQuery GetRuntimeManagementConfig' {..} =
    Prelude.mconcat ["Qualifier" Data.=: qualifier]

-- | /See:/ 'newGetRuntimeManagementConfigResponse' smart constructor.
data GetRuntimeManagementConfigResponse = GetRuntimeManagementConfigResponse'
  { -- | The Amazon Resource Name (ARN) of your function.
    functionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the runtime the function is configured to use. If the runtime
    -- update mode is __Manual__, the ARN is returned, otherwise @null@ is
    -- returned.
    runtimeVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The current runtime update mode of the function.
    updateRuntimeOn :: Prelude.Maybe UpdateRuntimeOn,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRuntimeManagementConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionArn', 'getRuntimeManagementConfigResponse_functionArn' - The Amazon Resource Name (ARN) of your function.
--
-- 'runtimeVersionArn', 'getRuntimeManagementConfigResponse_runtimeVersionArn' - The ARN of the runtime the function is configured to use. If the runtime
-- update mode is __Manual__, the ARN is returned, otherwise @null@ is
-- returned.
--
-- 'updateRuntimeOn', 'getRuntimeManagementConfigResponse_updateRuntimeOn' - The current runtime update mode of the function.
--
-- 'httpStatus', 'getRuntimeManagementConfigResponse_httpStatus' - The response's http status code.
newGetRuntimeManagementConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRuntimeManagementConfigResponse
newGetRuntimeManagementConfigResponse pHttpStatus_ =
  GetRuntimeManagementConfigResponse'
    { functionArn =
        Prelude.Nothing,
      runtimeVersionArn = Prelude.Nothing,
      updateRuntimeOn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of your function.
getRuntimeManagementConfigResponse_functionArn :: Lens.Lens' GetRuntimeManagementConfigResponse (Prelude.Maybe Prelude.Text)
getRuntimeManagementConfigResponse_functionArn = Lens.lens (\GetRuntimeManagementConfigResponse' {functionArn} -> functionArn) (\s@GetRuntimeManagementConfigResponse' {} a -> s {functionArn = a} :: GetRuntimeManagementConfigResponse)

-- | The ARN of the runtime the function is configured to use. If the runtime
-- update mode is __Manual__, the ARN is returned, otherwise @null@ is
-- returned.
getRuntimeManagementConfigResponse_runtimeVersionArn :: Lens.Lens' GetRuntimeManagementConfigResponse (Prelude.Maybe Prelude.Text)
getRuntimeManagementConfigResponse_runtimeVersionArn = Lens.lens (\GetRuntimeManagementConfigResponse' {runtimeVersionArn} -> runtimeVersionArn) (\s@GetRuntimeManagementConfigResponse' {} a -> s {runtimeVersionArn = a} :: GetRuntimeManagementConfigResponse)

-- | The current runtime update mode of the function.
getRuntimeManagementConfigResponse_updateRuntimeOn :: Lens.Lens' GetRuntimeManagementConfigResponse (Prelude.Maybe UpdateRuntimeOn)
getRuntimeManagementConfigResponse_updateRuntimeOn = Lens.lens (\GetRuntimeManagementConfigResponse' {updateRuntimeOn} -> updateRuntimeOn) (\s@GetRuntimeManagementConfigResponse' {} a -> s {updateRuntimeOn = a} :: GetRuntimeManagementConfigResponse)

-- | The response's http status code.
getRuntimeManagementConfigResponse_httpStatus :: Lens.Lens' GetRuntimeManagementConfigResponse Prelude.Int
getRuntimeManagementConfigResponse_httpStatus = Lens.lens (\GetRuntimeManagementConfigResponse' {httpStatus} -> httpStatus) (\s@GetRuntimeManagementConfigResponse' {} a -> s {httpStatus = a} :: GetRuntimeManagementConfigResponse)

instance
  Prelude.NFData
    GetRuntimeManagementConfigResponse
  where
  rnf GetRuntimeManagementConfigResponse' {..} =
    Prelude.rnf functionArn
      `Prelude.seq` Prelude.rnf runtimeVersionArn
      `Prelude.seq` Prelude.rnf updateRuntimeOn
      `Prelude.seq` Prelude.rnf httpStatus
