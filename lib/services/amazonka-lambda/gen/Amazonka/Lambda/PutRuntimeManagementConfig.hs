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
-- Module      : Amazonka.Lambda.PutRuntimeManagementConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the runtime management configuration for a function\'s version. For
-- more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-update.html Runtime updates>.
module Amazonka.Lambda.PutRuntimeManagementConfig
  ( -- * Creating a Request
    PutRuntimeManagementConfig (..),
    newPutRuntimeManagementConfig,

    -- * Request Lenses
    putRuntimeManagementConfig_qualifier,
    putRuntimeManagementConfig_runtimeVersionArn,
    putRuntimeManagementConfig_functionName,
    putRuntimeManagementConfig_updateRuntimeOn,

    -- * Destructuring the Response
    PutRuntimeManagementConfigResponse (..),
    newPutRuntimeManagementConfigResponse,

    -- * Response Lenses
    putRuntimeManagementConfigResponse_runtimeVersionArn,
    putRuntimeManagementConfigResponse_httpStatus,
    putRuntimeManagementConfigResponse_updateRuntimeOn,
    putRuntimeManagementConfigResponse_functionArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutRuntimeManagementConfig' smart constructor.
data PutRuntimeManagementConfig = PutRuntimeManagementConfig'
  { -- | Specify a version of the function. This can be @$LATEST@ or a published
    -- version number. If no value is specified, the configuration for the
    -- @$LATEST@ version is returned.
    qualifier :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the runtime version you want the function to use.
    --
    -- This is only required if you\'re using the __Manual__ runtime update
    -- mode.
    runtimeVersionArn :: Prelude.Maybe Prelude.Text,
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
    functionName :: Prelude.Text,
    -- | Specify the runtime update mode.
    --
    -- -   __Auto (default)__ - Automatically update to the most recent and
    --     secure runtime version using a
    --     <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-update.html#runtime-management-two-phase Two-phase runtime version rollout>.
    --     This is the best choice for most customers to ensure they always
    --     benefit from runtime updates.
    --
    -- -   __Function update__ - Lambda updates the runtime of your function to
    --     the most recent and secure runtime version when you update your
    --     function. This approach synchronizes runtime updates with function
    --     deployments, giving you control over when runtime updates are
    --     applied and allowing you to detect and mitigate rare runtime update
    --     incompatibilities early. When using this setting, you need to
    --     regularly update your functions to keep their runtime up-to-date.
    --
    -- -   __Manual__ - You specify a runtime version in your function
    --     configuration. The function will use this runtime version
    --     indefinitely. In the rare case where a new runtime version is
    --     incompatible with an existing function, this allows you to roll back
    --     your function to an earlier runtime version. For more information,
    --     see
    --     <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-update.html#runtime-management-rollback Roll back a runtime version>.
    updateRuntimeOn :: UpdateRuntimeOn
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRuntimeManagementConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualifier', 'putRuntimeManagementConfig_qualifier' - Specify a version of the function. This can be @$LATEST@ or a published
-- version number. If no value is specified, the configuration for the
-- @$LATEST@ version is returned.
--
-- 'runtimeVersionArn', 'putRuntimeManagementConfig_runtimeVersionArn' - The ARN of the runtime version you want the function to use.
--
-- This is only required if you\'re using the __Manual__ runtime update
-- mode.
--
-- 'functionName', 'putRuntimeManagementConfig_functionName' - The name of the Lambda function.
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
--
-- 'updateRuntimeOn', 'putRuntimeManagementConfig_updateRuntimeOn' - Specify the runtime update mode.
--
-- -   __Auto (default)__ - Automatically update to the most recent and
--     secure runtime version using a
--     <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-update.html#runtime-management-two-phase Two-phase runtime version rollout>.
--     This is the best choice for most customers to ensure they always
--     benefit from runtime updates.
--
-- -   __Function update__ - Lambda updates the runtime of your function to
--     the most recent and secure runtime version when you update your
--     function. This approach synchronizes runtime updates with function
--     deployments, giving you control over when runtime updates are
--     applied and allowing you to detect and mitigate rare runtime update
--     incompatibilities early. When using this setting, you need to
--     regularly update your functions to keep their runtime up-to-date.
--
-- -   __Manual__ - You specify a runtime version in your function
--     configuration. The function will use this runtime version
--     indefinitely. In the rare case where a new runtime version is
--     incompatible with an existing function, this allows you to roll back
--     your function to an earlier runtime version. For more information,
--     see
--     <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-update.html#runtime-management-rollback Roll back a runtime version>.
newPutRuntimeManagementConfig ::
  -- | 'functionName'
  Prelude.Text ->
  -- | 'updateRuntimeOn'
  UpdateRuntimeOn ->
  PutRuntimeManagementConfig
newPutRuntimeManagementConfig
  pFunctionName_
  pUpdateRuntimeOn_ =
    PutRuntimeManagementConfig'
      { qualifier =
          Prelude.Nothing,
        runtimeVersionArn = Prelude.Nothing,
        functionName = pFunctionName_,
        updateRuntimeOn = pUpdateRuntimeOn_
      }

-- | Specify a version of the function. This can be @$LATEST@ or a published
-- version number. If no value is specified, the configuration for the
-- @$LATEST@ version is returned.
putRuntimeManagementConfig_qualifier :: Lens.Lens' PutRuntimeManagementConfig (Prelude.Maybe Prelude.Text)
putRuntimeManagementConfig_qualifier = Lens.lens (\PutRuntimeManagementConfig' {qualifier} -> qualifier) (\s@PutRuntimeManagementConfig' {} a -> s {qualifier = a} :: PutRuntimeManagementConfig)

-- | The ARN of the runtime version you want the function to use.
--
-- This is only required if you\'re using the __Manual__ runtime update
-- mode.
putRuntimeManagementConfig_runtimeVersionArn :: Lens.Lens' PutRuntimeManagementConfig (Prelude.Maybe Prelude.Text)
putRuntimeManagementConfig_runtimeVersionArn = Lens.lens (\PutRuntimeManagementConfig' {runtimeVersionArn} -> runtimeVersionArn) (\s@PutRuntimeManagementConfig' {} a -> s {runtimeVersionArn = a} :: PutRuntimeManagementConfig)

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
putRuntimeManagementConfig_functionName :: Lens.Lens' PutRuntimeManagementConfig Prelude.Text
putRuntimeManagementConfig_functionName = Lens.lens (\PutRuntimeManagementConfig' {functionName} -> functionName) (\s@PutRuntimeManagementConfig' {} a -> s {functionName = a} :: PutRuntimeManagementConfig)

-- | Specify the runtime update mode.
--
-- -   __Auto (default)__ - Automatically update to the most recent and
--     secure runtime version using a
--     <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-update.html#runtime-management-two-phase Two-phase runtime version rollout>.
--     This is the best choice for most customers to ensure they always
--     benefit from runtime updates.
--
-- -   __Function update__ - Lambda updates the runtime of your function to
--     the most recent and secure runtime version when you update your
--     function. This approach synchronizes runtime updates with function
--     deployments, giving you control over when runtime updates are
--     applied and allowing you to detect and mitigate rare runtime update
--     incompatibilities early. When using this setting, you need to
--     regularly update your functions to keep their runtime up-to-date.
--
-- -   __Manual__ - You specify a runtime version in your function
--     configuration. The function will use this runtime version
--     indefinitely. In the rare case where a new runtime version is
--     incompatible with an existing function, this allows you to roll back
--     your function to an earlier runtime version. For more information,
--     see
--     <https://docs.aws.amazon.com/lambda/latest/dg/runtimes-update.html#runtime-management-rollback Roll back a runtime version>.
putRuntimeManagementConfig_updateRuntimeOn :: Lens.Lens' PutRuntimeManagementConfig UpdateRuntimeOn
putRuntimeManagementConfig_updateRuntimeOn = Lens.lens (\PutRuntimeManagementConfig' {updateRuntimeOn} -> updateRuntimeOn) (\s@PutRuntimeManagementConfig' {} a -> s {updateRuntimeOn = a} :: PutRuntimeManagementConfig)

instance Core.AWSRequest PutRuntimeManagementConfig where
  type
    AWSResponse PutRuntimeManagementConfig =
      PutRuntimeManagementConfigResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRuntimeManagementConfigResponse'
            Prelude.<$> (x Data..?> "RuntimeVersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "UpdateRuntimeOn")
            Prelude.<*> (x Data..:> "FunctionArn")
      )

instance Prelude.Hashable PutRuntimeManagementConfig where
  hashWithSalt _salt PutRuntimeManagementConfig' {..} =
    _salt
      `Prelude.hashWithSalt` qualifier
      `Prelude.hashWithSalt` runtimeVersionArn
      `Prelude.hashWithSalt` functionName
      `Prelude.hashWithSalt` updateRuntimeOn

instance Prelude.NFData PutRuntimeManagementConfig where
  rnf PutRuntimeManagementConfig' {..} =
    Prelude.rnf qualifier
      `Prelude.seq` Prelude.rnf runtimeVersionArn
      `Prelude.seq` Prelude.rnf functionName
      `Prelude.seq` Prelude.rnf updateRuntimeOn

instance Data.ToHeaders PutRuntimeManagementConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON PutRuntimeManagementConfig where
  toJSON PutRuntimeManagementConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RuntimeVersionArn" Data..=)
              Prelude.<$> runtimeVersionArn,
            Prelude.Just
              ("UpdateRuntimeOn" Data..= updateRuntimeOn)
          ]
      )

instance Data.ToPath PutRuntimeManagementConfig where
  toPath PutRuntimeManagementConfig' {..} =
    Prelude.mconcat
      [ "/2021-07-20/functions/",
        Data.toBS functionName,
        "/runtime-management-config"
      ]

instance Data.ToQuery PutRuntimeManagementConfig where
  toQuery PutRuntimeManagementConfig' {..} =
    Prelude.mconcat ["Qualifier" Data.=: qualifier]

-- | /See:/ 'newPutRuntimeManagementConfigResponse' smart constructor.
data PutRuntimeManagementConfigResponse = PutRuntimeManagementConfigResponse'
  { -- | The ARN of the runtime the function is configured to use. If the runtime
    -- update mode is __manual__, the ARN is returned, otherwise @null@ is
    -- returned.
    runtimeVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The runtime update mode.
    updateRuntimeOn :: UpdateRuntimeOn,
    -- | The ARN of the function
    functionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRuntimeManagementConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runtimeVersionArn', 'putRuntimeManagementConfigResponse_runtimeVersionArn' - The ARN of the runtime the function is configured to use. If the runtime
-- update mode is __manual__, the ARN is returned, otherwise @null@ is
-- returned.
--
-- 'httpStatus', 'putRuntimeManagementConfigResponse_httpStatus' - The response's http status code.
--
-- 'updateRuntimeOn', 'putRuntimeManagementConfigResponse_updateRuntimeOn' - The runtime update mode.
--
-- 'functionArn', 'putRuntimeManagementConfigResponse_functionArn' - The ARN of the function
newPutRuntimeManagementConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'updateRuntimeOn'
  UpdateRuntimeOn ->
  -- | 'functionArn'
  Prelude.Text ->
  PutRuntimeManagementConfigResponse
newPutRuntimeManagementConfigResponse
  pHttpStatus_
  pUpdateRuntimeOn_
  pFunctionArn_ =
    PutRuntimeManagementConfigResponse'
      { runtimeVersionArn =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        updateRuntimeOn = pUpdateRuntimeOn_,
        functionArn = pFunctionArn_
      }

-- | The ARN of the runtime the function is configured to use. If the runtime
-- update mode is __manual__, the ARN is returned, otherwise @null@ is
-- returned.
putRuntimeManagementConfigResponse_runtimeVersionArn :: Lens.Lens' PutRuntimeManagementConfigResponse (Prelude.Maybe Prelude.Text)
putRuntimeManagementConfigResponse_runtimeVersionArn = Lens.lens (\PutRuntimeManagementConfigResponse' {runtimeVersionArn} -> runtimeVersionArn) (\s@PutRuntimeManagementConfigResponse' {} a -> s {runtimeVersionArn = a} :: PutRuntimeManagementConfigResponse)

-- | The response's http status code.
putRuntimeManagementConfigResponse_httpStatus :: Lens.Lens' PutRuntimeManagementConfigResponse Prelude.Int
putRuntimeManagementConfigResponse_httpStatus = Lens.lens (\PutRuntimeManagementConfigResponse' {httpStatus} -> httpStatus) (\s@PutRuntimeManagementConfigResponse' {} a -> s {httpStatus = a} :: PutRuntimeManagementConfigResponse)

-- | The runtime update mode.
putRuntimeManagementConfigResponse_updateRuntimeOn :: Lens.Lens' PutRuntimeManagementConfigResponse UpdateRuntimeOn
putRuntimeManagementConfigResponse_updateRuntimeOn = Lens.lens (\PutRuntimeManagementConfigResponse' {updateRuntimeOn} -> updateRuntimeOn) (\s@PutRuntimeManagementConfigResponse' {} a -> s {updateRuntimeOn = a} :: PutRuntimeManagementConfigResponse)

-- | The ARN of the function
putRuntimeManagementConfigResponse_functionArn :: Lens.Lens' PutRuntimeManagementConfigResponse Prelude.Text
putRuntimeManagementConfigResponse_functionArn = Lens.lens (\PutRuntimeManagementConfigResponse' {functionArn} -> functionArn) (\s@PutRuntimeManagementConfigResponse' {} a -> s {functionArn = a} :: PutRuntimeManagementConfigResponse)

instance
  Prelude.NFData
    PutRuntimeManagementConfigResponse
  where
  rnf PutRuntimeManagementConfigResponse' {..} =
    Prelude.rnf runtimeVersionArn
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf updateRuntimeOn
      `Prelude.seq` Prelude.rnf functionArn
