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
-- Module      : Amazonka.Lambda.CreateFunctionUrlConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Lambda function URL with the specified configuration
-- parameters. A function URL is a dedicated HTTP(S) endpoint that you can
-- use to invoke your function.
module Amazonka.Lambda.CreateFunctionUrlConfig
  ( -- * Creating a Request
    CreateFunctionUrlConfig (..),
    newCreateFunctionUrlConfig,

    -- * Request Lenses
    createFunctionUrlConfig_cors,
    createFunctionUrlConfig_invokeMode,
    createFunctionUrlConfig_qualifier,
    createFunctionUrlConfig_functionName,
    createFunctionUrlConfig_authType,

    -- * Destructuring the Response
    CreateFunctionUrlConfigResponse (..),
    newCreateFunctionUrlConfigResponse,

    -- * Response Lenses
    createFunctionUrlConfigResponse_cors,
    createFunctionUrlConfigResponse_invokeMode,
    createFunctionUrlConfigResponse_httpStatus,
    createFunctionUrlConfigResponse_functionUrl,
    createFunctionUrlConfigResponse_functionArn,
    createFunctionUrlConfigResponse_authType,
    createFunctionUrlConfigResponse_creationTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFunctionUrlConfig' smart constructor.
data CreateFunctionUrlConfig = CreateFunctionUrlConfig'
  { -- | The
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS cross-origin resource sharing (CORS)>
    -- settings for your function URL.
    cors :: Prelude.Maybe Cors,
    -- | Use one of the following options:
    --
    -- -   @BUFFERED@ – This is the default option. Lambda invokes your
    --     function using the @Invoke@ API operation. Invocation results are
    --     available when the payload is complete. The maximum payload size is
    --     6 MB.
    --
    -- -   @RESPONSE_STREAM@ – Your function streams payload results as they
    --     become available. Lambda invokes your function using the
    --     @InvokeWithResponseStream@ API operation. The maximum response
    --     payload size is 20 MB, however, you can
    --     <https://docs.aws.amazon.com/servicequotas/latest/userguide/request-quota-increase.html request a quota increase>.
    invokeMode :: Prelude.Maybe InvokeMode,
    -- | The alias name.
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
    functionName :: Prelude.Text,
    -- | The type of authentication that your function URL uses. Set to @AWS_IAM@
    -- if you want to restrict access to authenticated users only. Set to
    -- @NONE@ if you want to bypass IAM authentication to create a public
    -- endpoint. For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/urls-auth.html Security and auth model for Lambda function URLs>.
    authType :: FunctionUrlAuthType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFunctionUrlConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cors', 'createFunctionUrlConfig_cors' - The
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS cross-origin resource sharing (CORS)>
-- settings for your function URL.
--
-- 'invokeMode', 'createFunctionUrlConfig_invokeMode' - Use one of the following options:
--
-- -   @BUFFERED@ – This is the default option. Lambda invokes your
--     function using the @Invoke@ API operation. Invocation results are
--     available when the payload is complete. The maximum payload size is
--     6 MB.
--
-- -   @RESPONSE_STREAM@ – Your function streams payload results as they
--     become available. Lambda invokes your function using the
--     @InvokeWithResponseStream@ API operation. The maximum response
--     payload size is 20 MB, however, you can
--     <https://docs.aws.amazon.com/servicequotas/latest/userguide/request-quota-increase.html request a quota increase>.
--
-- 'qualifier', 'createFunctionUrlConfig_qualifier' - The alias name.
--
-- 'functionName', 'createFunctionUrlConfig_functionName' - The name of the Lambda function.
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
-- 'authType', 'createFunctionUrlConfig_authType' - The type of authentication that your function URL uses. Set to @AWS_IAM@
-- if you want to restrict access to authenticated users only. Set to
-- @NONE@ if you want to bypass IAM authentication to create a public
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/urls-auth.html Security and auth model for Lambda function URLs>.
newCreateFunctionUrlConfig ::
  -- | 'functionName'
  Prelude.Text ->
  -- | 'authType'
  FunctionUrlAuthType ->
  CreateFunctionUrlConfig
newCreateFunctionUrlConfig pFunctionName_ pAuthType_ =
  CreateFunctionUrlConfig'
    { cors = Prelude.Nothing,
      invokeMode = Prelude.Nothing,
      qualifier = Prelude.Nothing,
      functionName = pFunctionName_,
      authType = pAuthType_
    }

-- | The
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS cross-origin resource sharing (CORS)>
-- settings for your function URL.
createFunctionUrlConfig_cors :: Lens.Lens' CreateFunctionUrlConfig (Prelude.Maybe Cors)
createFunctionUrlConfig_cors = Lens.lens (\CreateFunctionUrlConfig' {cors} -> cors) (\s@CreateFunctionUrlConfig' {} a -> s {cors = a} :: CreateFunctionUrlConfig)

-- | Use one of the following options:
--
-- -   @BUFFERED@ – This is the default option. Lambda invokes your
--     function using the @Invoke@ API operation. Invocation results are
--     available when the payload is complete. The maximum payload size is
--     6 MB.
--
-- -   @RESPONSE_STREAM@ – Your function streams payload results as they
--     become available. Lambda invokes your function using the
--     @InvokeWithResponseStream@ API operation. The maximum response
--     payload size is 20 MB, however, you can
--     <https://docs.aws.amazon.com/servicequotas/latest/userguide/request-quota-increase.html request a quota increase>.
createFunctionUrlConfig_invokeMode :: Lens.Lens' CreateFunctionUrlConfig (Prelude.Maybe InvokeMode)
createFunctionUrlConfig_invokeMode = Lens.lens (\CreateFunctionUrlConfig' {invokeMode} -> invokeMode) (\s@CreateFunctionUrlConfig' {} a -> s {invokeMode = a} :: CreateFunctionUrlConfig)

-- | The alias name.
createFunctionUrlConfig_qualifier :: Lens.Lens' CreateFunctionUrlConfig (Prelude.Maybe Prelude.Text)
createFunctionUrlConfig_qualifier = Lens.lens (\CreateFunctionUrlConfig' {qualifier} -> qualifier) (\s@CreateFunctionUrlConfig' {} a -> s {qualifier = a} :: CreateFunctionUrlConfig)

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
createFunctionUrlConfig_functionName :: Lens.Lens' CreateFunctionUrlConfig Prelude.Text
createFunctionUrlConfig_functionName = Lens.lens (\CreateFunctionUrlConfig' {functionName} -> functionName) (\s@CreateFunctionUrlConfig' {} a -> s {functionName = a} :: CreateFunctionUrlConfig)

-- | The type of authentication that your function URL uses. Set to @AWS_IAM@
-- if you want to restrict access to authenticated users only. Set to
-- @NONE@ if you want to bypass IAM authentication to create a public
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/urls-auth.html Security and auth model for Lambda function URLs>.
createFunctionUrlConfig_authType :: Lens.Lens' CreateFunctionUrlConfig FunctionUrlAuthType
createFunctionUrlConfig_authType = Lens.lens (\CreateFunctionUrlConfig' {authType} -> authType) (\s@CreateFunctionUrlConfig' {} a -> s {authType = a} :: CreateFunctionUrlConfig)

instance Core.AWSRequest CreateFunctionUrlConfig where
  type
    AWSResponse CreateFunctionUrlConfig =
      CreateFunctionUrlConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFunctionUrlConfigResponse'
            Prelude.<$> (x Data..?> "Cors")
            Prelude.<*> (x Data..?> "InvokeMode")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "FunctionUrl")
            Prelude.<*> (x Data..:> "FunctionArn")
            Prelude.<*> (x Data..:> "AuthType")
            Prelude.<*> (x Data..:> "CreationTime")
      )

instance Prelude.Hashable CreateFunctionUrlConfig where
  hashWithSalt _salt CreateFunctionUrlConfig' {..} =
    _salt
      `Prelude.hashWithSalt` cors
      `Prelude.hashWithSalt` invokeMode
      `Prelude.hashWithSalt` qualifier
      `Prelude.hashWithSalt` functionName
      `Prelude.hashWithSalt` authType

instance Prelude.NFData CreateFunctionUrlConfig where
  rnf CreateFunctionUrlConfig' {..} =
    Prelude.rnf cors
      `Prelude.seq` Prelude.rnf invokeMode
      `Prelude.seq` Prelude.rnf qualifier
      `Prelude.seq` Prelude.rnf functionName
      `Prelude.seq` Prelude.rnf authType

instance Data.ToHeaders CreateFunctionUrlConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateFunctionUrlConfig where
  toJSON CreateFunctionUrlConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Cors" Data..=) Prelude.<$> cors,
            ("InvokeMode" Data..=) Prelude.<$> invokeMode,
            Prelude.Just ("AuthType" Data..= authType)
          ]
      )

instance Data.ToPath CreateFunctionUrlConfig where
  toPath CreateFunctionUrlConfig' {..} =
    Prelude.mconcat
      [ "/2021-10-31/functions/",
        Data.toBS functionName,
        "/url"
      ]

instance Data.ToQuery CreateFunctionUrlConfig where
  toQuery CreateFunctionUrlConfig' {..} =
    Prelude.mconcat ["Qualifier" Data.=: qualifier]

-- | /See:/ 'newCreateFunctionUrlConfigResponse' smart constructor.
data CreateFunctionUrlConfigResponse = CreateFunctionUrlConfigResponse'
  { -- | The
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS cross-origin resource sharing (CORS)>
    -- settings for your function URL.
    cors :: Prelude.Maybe Cors,
    -- | Use one of the following options:
    --
    -- -   @BUFFERED@ – This is the default option. Lambda invokes your
    --     function using the @Invoke@ API operation. Invocation results are
    --     available when the payload is complete. The maximum payload size is
    --     6 MB.
    --
    -- -   @RESPONSE_STREAM@ – Your function streams payload results as they
    --     become available. Lambda invokes your function using the
    --     @InvokeWithResponseStream@ API operation. The maximum response
    --     payload size is 20 MB, however, you can
    --     <https://docs.aws.amazon.com/servicequotas/latest/userguide/request-quota-increase.html request a quota increase>.
    invokeMode :: Prelude.Maybe InvokeMode,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The HTTP URL endpoint for your function.
    functionUrl :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of your function.
    functionArn :: Prelude.Text,
    -- | The type of authentication that your function URL uses. Set to @AWS_IAM@
    -- if you want to restrict access to authenticated users only. Set to
    -- @NONE@ if you want to bypass IAM authentication to create a public
    -- endpoint. For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/urls-auth.html Security and auth model for Lambda function URLs>.
    authType :: FunctionUrlAuthType,
    -- | When the function URL was created, in
    -- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
    -- (YYYY-MM-DDThh:mm:ss.sTZD).
    creationTime :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFunctionUrlConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cors', 'createFunctionUrlConfigResponse_cors' - The
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS cross-origin resource sharing (CORS)>
-- settings for your function URL.
--
-- 'invokeMode', 'createFunctionUrlConfigResponse_invokeMode' - Use one of the following options:
--
-- -   @BUFFERED@ – This is the default option. Lambda invokes your
--     function using the @Invoke@ API operation. Invocation results are
--     available when the payload is complete. The maximum payload size is
--     6 MB.
--
-- -   @RESPONSE_STREAM@ – Your function streams payload results as they
--     become available. Lambda invokes your function using the
--     @InvokeWithResponseStream@ API operation. The maximum response
--     payload size is 20 MB, however, you can
--     <https://docs.aws.amazon.com/servicequotas/latest/userguide/request-quota-increase.html request a quota increase>.
--
-- 'httpStatus', 'createFunctionUrlConfigResponse_httpStatus' - The response's http status code.
--
-- 'functionUrl', 'createFunctionUrlConfigResponse_functionUrl' - The HTTP URL endpoint for your function.
--
-- 'functionArn', 'createFunctionUrlConfigResponse_functionArn' - The Amazon Resource Name (ARN) of your function.
--
-- 'authType', 'createFunctionUrlConfigResponse_authType' - The type of authentication that your function URL uses. Set to @AWS_IAM@
-- if you want to restrict access to authenticated users only. Set to
-- @NONE@ if you want to bypass IAM authentication to create a public
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/urls-auth.html Security and auth model for Lambda function URLs>.
--
-- 'creationTime', 'createFunctionUrlConfigResponse_creationTime' - When the function URL was created, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
newCreateFunctionUrlConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'functionUrl'
  Prelude.Text ->
  -- | 'functionArn'
  Prelude.Text ->
  -- | 'authType'
  FunctionUrlAuthType ->
  -- | 'creationTime'
  Prelude.Text ->
  CreateFunctionUrlConfigResponse
newCreateFunctionUrlConfigResponse
  pHttpStatus_
  pFunctionUrl_
  pFunctionArn_
  pAuthType_
  pCreationTime_ =
    CreateFunctionUrlConfigResponse'
      { cors =
          Prelude.Nothing,
        invokeMode = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        functionUrl = pFunctionUrl_,
        functionArn = pFunctionArn_,
        authType = pAuthType_,
        creationTime = pCreationTime_
      }

-- | The
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS cross-origin resource sharing (CORS)>
-- settings for your function URL.
createFunctionUrlConfigResponse_cors :: Lens.Lens' CreateFunctionUrlConfigResponse (Prelude.Maybe Cors)
createFunctionUrlConfigResponse_cors = Lens.lens (\CreateFunctionUrlConfigResponse' {cors} -> cors) (\s@CreateFunctionUrlConfigResponse' {} a -> s {cors = a} :: CreateFunctionUrlConfigResponse)

-- | Use one of the following options:
--
-- -   @BUFFERED@ – This is the default option. Lambda invokes your
--     function using the @Invoke@ API operation. Invocation results are
--     available when the payload is complete. The maximum payload size is
--     6 MB.
--
-- -   @RESPONSE_STREAM@ – Your function streams payload results as they
--     become available. Lambda invokes your function using the
--     @InvokeWithResponseStream@ API operation. The maximum response
--     payload size is 20 MB, however, you can
--     <https://docs.aws.amazon.com/servicequotas/latest/userguide/request-quota-increase.html request a quota increase>.
createFunctionUrlConfigResponse_invokeMode :: Lens.Lens' CreateFunctionUrlConfigResponse (Prelude.Maybe InvokeMode)
createFunctionUrlConfigResponse_invokeMode = Lens.lens (\CreateFunctionUrlConfigResponse' {invokeMode} -> invokeMode) (\s@CreateFunctionUrlConfigResponse' {} a -> s {invokeMode = a} :: CreateFunctionUrlConfigResponse)

-- | The response's http status code.
createFunctionUrlConfigResponse_httpStatus :: Lens.Lens' CreateFunctionUrlConfigResponse Prelude.Int
createFunctionUrlConfigResponse_httpStatus = Lens.lens (\CreateFunctionUrlConfigResponse' {httpStatus} -> httpStatus) (\s@CreateFunctionUrlConfigResponse' {} a -> s {httpStatus = a} :: CreateFunctionUrlConfigResponse)

-- | The HTTP URL endpoint for your function.
createFunctionUrlConfigResponse_functionUrl :: Lens.Lens' CreateFunctionUrlConfigResponse Prelude.Text
createFunctionUrlConfigResponse_functionUrl = Lens.lens (\CreateFunctionUrlConfigResponse' {functionUrl} -> functionUrl) (\s@CreateFunctionUrlConfigResponse' {} a -> s {functionUrl = a} :: CreateFunctionUrlConfigResponse)

-- | The Amazon Resource Name (ARN) of your function.
createFunctionUrlConfigResponse_functionArn :: Lens.Lens' CreateFunctionUrlConfigResponse Prelude.Text
createFunctionUrlConfigResponse_functionArn = Lens.lens (\CreateFunctionUrlConfigResponse' {functionArn} -> functionArn) (\s@CreateFunctionUrlConfigResponse' {} a -> s {functionArn = a} :: CreateFunctionUrlConfigResponse)

-- | The type of authentication that your function URL uses. Set to @AWS_IAM@
-- if you want to restrict access to authenticated users only. Set to
-- @NONE@ if you want to bypass IAM authentication to create a public
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/urls-auth.html Security and auth model for Lambda function URLs>.
createFunctionUrlConfigResponse_authType :: Lens.Lens' CreateFunctionUrlConfigResponse FunctionUrlAuthType
createFunctionUrlConfigResponse_authType = Lens.lens (\CreateFunctionUrlConfigResponse' {authType} -> authType) (\s@CreateFunctionUrlConfigResponse' {} a -> s {authType = a} :: CreateFunctionUrlConfigResponse)

-- | When the function URL was created, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
createFunctionUrlConfigResponse_creationTime :: Lens.Lens' CreateFunctionUrlConfigResponse Prelude.Text
createFunctionUrlConfigResponse_creationTime = Lens.lens (\CreateFunctionUrlConfigResponse' {creationTime} -> creationTime) (\s@CreateFunctionUrlConfigResponse' {} a -> s {creationTime = a} :: CreateFunctionUrlConfigResponse)

instance
  Prelude.NFData
    CreateFunctionUrlConfigResponse
  where
  rnf CreateFunctionUrlConfigResponse' {..} =
    Prelude.rnf cors
      `Prelude.seq` Prelude.rnf invokeMode
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf functionUrl
      `Prelude.seq` Prelude.rnf functionArn
      `Prelude.seq` Prelude.rnf authType
      `Prelude.seq` Prelude.rnf creationTime
