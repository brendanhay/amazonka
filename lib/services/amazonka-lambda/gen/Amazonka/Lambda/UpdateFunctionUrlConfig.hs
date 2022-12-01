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
-- Module      : Amazonka.Lambda.UpdateFunctionUrlConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration for a Lambda function URL.
module Amazonka.Lambda.UpdateFunctionUrlConfig
  ( -- * Creating a Request
    UpdateFunctionUrlConfig (..),
    newUpdateFunctionUrlConfig,

    -- * Request Lenses
    updateFunctionUrlConfig_cors,
    updateFunctionUrlConfig_qualifier,
    updateFunctionUrlConfig_authType,
    updateFunctionUrlConfig_functionName,

    -- * Destructuring the Response
    UpdateFunctionUrlConfigResponse (..),
    newUpdateFunctionUrlConfigResponse,

    -- * Response Lenses
    updateFunctionUrlConfigResponse_cors,
    updateFunctionUrlConfigResponse_httpStatus,
    updateFunctionUrlConfigResponse_functionUrl,
    updateFunctionUrlConfigResponse_functionArn,
    updateFunctionUrlConfigResponse_authType,
    updateFunctionUrlConfigResponse_creationTime,
    updateFunctionUrlConfigResponse_lastModifiedTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFunctionUrlConfig' smart constructor.
data UpdateFunctionUrlConfig = UpdateFunctionUrlConfig'
  { -- | The
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS cross-origin resource sharing (CORS)>
    -- settings for your function URL.
    cors :: Prelude.Maybe Cors,
    -- | The alias name.
    qualifier :: Prelude.Maybe Prelude.Text,
    -- | The type of authentication that your function URL uses. Set to @AWS_IAM@
    -- if you want to restrict access to authenticated @IAM@ users only. Set to
    -- @NONE@ if you want to bypass IAM authentication to create a public
    -- endpoint. For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/urls-auth.html Security and auth model for Lambda function URLs>.
    authType :: Prelude.Maybe FunctionUrlAuthType,
    -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @my-function@.
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ - @123456789012:function:my-function@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFunctionUrlConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cors', 'updateFunctionUrlConfig_cors' - The
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS cross-origin resource sharing (CORS)>
-- settings for your function URL.
--
-- 'qualifier', 'updateFunctionUrlConfig_qualifier' - The alias name.
--
-- 'authType', 'updateFunctionUrlConfig_authType' - The type of authentication that your function URL uses. Set to @AWS_IAM@
-- if you want to restrict access to authenticated @IAM@ users only. Set to
-- @NONE@ if you want to bypass IAM authentication to create a public
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/urls-auth.html Security and auth model for Lambda function URLs>.
--
-- 'functionName', 'updateFunctionUrlConfig_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
newUpdateFunctionUrlConfig ::
  -- | 'functionName'
  Prelude.Text ->
  UpdateFunctionUrlConfig
newUpdateFunctionUrlConfig pFunctionName_ =
  UpdateFunctionUrlConfig'
    { cors = Prelude.Nothing,
      qualifier = Prelude.Nothing,
      authType = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | The
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS cross-origin resource sharing (CORS)>
-- settings for your function URL.
updateFunctionUrlConfig_cors :: Lens.Lens' UpdateFunctionUrlConfig (Prelude.Maybe Cors)
updateFunctionUrlConfig_cors = Lens.lens (\UpdateFunctionUrlConfig' {cors} -> cors) (\s@UpdateFunctionUrlConfig' {} a -> s {cors = a} :: UpdateFunctionUrlConfig)

-- | The alias name.
updateFunctionUrlConfig_qualifier :: Lens.Lens' UpdateFunctionUrlConfig (Prelude.Maybe Prelude.Text)
updateFunctionUrlConfig_qualifier = Lens.lens (\UpdateFunctionUrlConfig' {qualifier} -> qualifier) (\s@UpdateFunctionUrlConfig' {} a -> s {qualifier = a} :: UpdateFunctionUrlConfig)

-- | The type of authentication that your function URL uses. Set to @AWS_IAM@
-- if you want to restrict access to authenticated @IAM@ users only. Set to
-- @NONE@ if you want to bypass IAM authentication to create a public
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/urls-auth.html Security and auth model for Lambda function URLs>.
updateFunctionUrlConfig_authType :: Lens.Lens' UpdateFunctionUrlConfig (Prelude.Maybe FunctionUrlAuthType)
updateFunctionUrlConfig_authType = Lens.lens (\UpdateFunctionUrlConfig' {authType} -> authType) (\s@UpdateFunctionUrlConfig' {} a -> s {authType = a} :: UpdateFunctionUrlConfig)

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it is limited to 64 characters in length.
updateFunctionUrlConfig_functionName :: Lens.Lens' UpdateFunctionUrlConfig Prelude.Text
updateFunctionUrlConfig_functionName = Lens.lens (\UpdateFunctionUrlConfig' {functionName} -> functionName) (\s@UpdateFunctionUrlConfig' {} a -> s {functionName = a} :: UpdateFunctionUrlConfig)

instance Core.AWSRequest UpdateFunctionUrlConfig where
  type
    AWSResponse UpdateFunctionUrlConfig =
      UpdateFunctionUrlConfigResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFunctionUrlConfigResponse'
            Prelude.<$> (x Core..?> "Cors")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "FunctionUrl")
            Prelude.<*> (x Core..:> "FunctionArn")
            Prelude.<*> (x Core..:> "AuthType")
            Prelude.<*> (x Core..:> "CreationTime")
            Prelude.<*> (x Core..:> "LastModifiedTime")
      )

instance Prelude.Hashable UpdateFunctionUrlConfig where
  hashWithSalt _salt UpdateFunctionUrlConfig' {..} =
    _salt `Prelude.hashWithSalt` cors
      `Prelude.hashWithSalt` qualifier
      `Prelude.hashWithSalt` authType
      `Prelude.hashWithSalt` functionName

instance Prelude.NFData UpdateFunctionUrlConfig where
  rnf UpdateFunctionUrlConfig' {..} =
    Prelude.rnf cors
      `Prelude.seq` Prelude.rnf qualifier
      `Prelude.seq` Prelude.rnf authType
      `Prelude.seq` Prelude.rnf functionName

instance Core.ToHeaders UpdateFunctionUrlConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateFunctionUrlConfig where
  toJSON UpdateFunctionUrlConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Cors" Core..=) Prelude.<$> cors,
            ("AuthType" Core..=) Prelude.<$> authType
          ]
      )

instance Core.ToPath UpdateFunctionUrlConfig where
  toPath UpdateFunctionUrlConfig' {..} =
    Prelude.mconcat
      [ "/2021-10-31/functions/",
        Core.toBS functionName,
        "/url"
      ]

instance Core.ToQuery UpdateFunctionUrlConfig where
  toQuery UpdateFunctionUrlConfig' {..} =
    Prelude.mconcat ["Qualifier" Core.=: qualifier]

-- | /See:/ 'newUpdateFunctionUrlConfigResponse' smart constructor.
data UpdateFunctionUrlConfigResponse = UpdateFunctionUrlConfigResponse'
  { -- | The
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS cross-origin resource sharing (CORS)>
    -- settings for your function URL.
    cors :: Prelude.Maybe Cors,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The HTTP URL endpoint for your function.
    functionUrl :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of your function.
    functionArn :: Prelude.Text,
    -- | The type of authentication that your function URL uses. Set to @AWS_IAM@
    -- if you want to restrict access to authenticated @IAM@ users only. Set to
    -- @NONE@ if you want to bypass IAM authentication to create a public
    -- endpoint. For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/urls-auth.html Security and auth model for Lambda function URLs>.
    authType :: FunctionUrlAuthType,
    -- | When the function URL was created, in
    -- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
    -- (YYYY-MM-DDThh:mm:ss.sTZD).
    creationTime :: Prelude.Text,
    -- | When the function URL configuration was last updated, in
    -- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
    -- (YYYY-MM-DDThh:mm:ss.sTZD).
    lastModifiedTime :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFunctionUrlConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cors', 'updateFunctionUrlConfigResponse_cors' - The
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS cross-origin resource sharing (CORS)>
-- settings for your function URL.
--
-- 'httpStatus', 'updateFunctionUrlConfigResponse_httpStatus' - The response's http status code.
--
-- 'functionUrl', 'updateFunctionUrlConfigResponse_functionUrl' - The HTTP URL endpoint for your function.
--
-- 'functionArn', 'updateFunctionUrlConfigResponse_functionArn' - The Amazon Resource Name (ARN) of your function.
--
-- 'authType', 'updateFunctionUrlConfigResponse_authType' - The type of authentication that your function URL uses. Set to @AWS_IAM@
-- if you want to restrict access to authenticated @IAM@ users only. Set to
-- @NONE@ if you want to bypass IAM authentication to create a public
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/urls-auth.html Security and auth model for Lambda function URLs>.
--
-- 'creationTime', 'updateFunctionUrlConfigResponse_creationTime' - When the function URL was created, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- 'lastModifiedTime', 'updateFunctionUrlConfigResponse_lastModifiedTime' - When the function URL configuration was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
newUpdateFunctionUrlConfigResponse ::
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
  -- | 'lastModifiedTime'
  Prelude.Text ->
  UpdateFunctionUrlConfigResponse
newUpdateFunctionUrlConfigResponse
  pHttpStatus_
  pFunctionUrl_
  pFunctionArn_
  pAuthType_
  pCreationTime_
  pLastModifiedTime_ =
    UpdateFunctionUrlConfigResponse'
      { cors =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        functionUrl = pFunctionUrl_,
        functionArn = pFunctionArn_,
        authType = pAuthType_,
        creationTime = pCreationTime_,
        lastModifiedTime = pLastModifiedTime_
      }

-- | The
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS cross-origin resource sharing (CORS)>
-- settings for your function URL.
updateFunctionUrlConfigResponse_cors :: Lens.Lens' UpdateFunctionUrlConfigResponse (Prelude.Maybe Cors)
updateFunctionUrlConfigResponse_cors = Lens.lens (\UpdateFunctionUrlConfigResponse' {cors} -> cors) (\s@UpdateFunctionUrlConfigResponse' {} a -> s {cors = a} :: UpdateFunctionUrlConfigResponse)

-- | The response's http status code.
updateFunctionUrlConfigResponse_httpStatus :: Lens.Lens' UpdateFunctionUrlConfigResponse Prelude.Int
updateFunctionUrlConfigResponse_httpStatus = Lens.lens (\UpdateFunctionUrlConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateFunctionUrlConfigResponse' {} a -> s {httpStatus = a} :: UpdateFunctionUrlConfigResponse)

-- | The HTTP URL endpoint for your function.
updateFunctionUrlConfigResponse_functionUrl :: Lens.Lens' UpdateFunctionUrlConfigResponse Prelude.Text
updateFunctionUrlConfigResponse_functionUrl = Lens.lens (\UpdateFunctionUrlConfigResponse' {functionUrl} -> functionUrl) (\s@UpdateFunctionUrlConfigResponse' {} a -> s {functionUrl = a} :: UpdateFunctionUrlConfigResponse)

-- | The Amazon Resource Name (ARN) of your function.
updateFunctionUrlConfigResponse_functionArn :: Lens.Lens' UpdateFunctionUrlConfigResponse Prelude.Text
updateFunctionUrlConfigResponse_functionArn = Lens.lens (\UpdateFunctionUrlConfigResponse' {functionArn} -> functionArn) (\s@UpdateFunctionUrlConfigResponse' {} a -> s {functionArn = a} :: UpdateFunctionUrlConfigResponse)

-- | The type of authentication that your function URL uses. Set to @AWS_IAM@
-- if you want to restrict access to authenticated @IAM@ users only. Set to
-- @NONE@ if you want to bypass IAM authentication to create a public
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/urls-auth.html Security and auth model for Lambda function URLs>.
updateFunctionUrlConfigResponse_authType :: Lens.Lens' UpdateFunctionUrlConfigResponse FunctionUrlAuthType
updateFunctionUrlConfigResponse_authType = Lens.lens (\UpdateFunctionUrlConfigResponse' {authType} -> authType) (\s@UpdateFunctionUrlConfigResponse' {} a -> s {authType = a} :: UpdateFunctionUrlConfigResponse)

-- | When the function URL was created, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
updateFunctionUrlConfigResponse_creationTime :: Lens.Lens' UpdateFunctionUrlConfigResponse Prelude.Text
updateFunctionUrlConfigResponse_creationTime = Lens.lens (\UpdateFunctionUrlConfigResponse' {creationTime} -> creationTime) (\s@UpdateFunctionUrlConfigResponse' {} a -> s {creationTime = a} :: UpdateFunctionUrlConfigResponse)

-- | When the function URL configuration was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
updateFunctionUrlConfigResponse_lastModifiedTime :: Lens.Lens' UpdateFunctionUrlConfigResponse Prelude.Text
updateFunctionUrlConfigResponse_lastModifiedTime = Lens.lens (\UpdateFunctionUrlConfigResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdateFunctionUrlConfigResponse' {} a -> s {lastModifiedTime = a} :: UpdateFunctionUrlConfigResponse)

instance
  Prelude.NFData
    UpdateFunctionUrlConfigResponse
  where
  rnf UpdateFunctionUrlConfigResponse' {..} =
    Prelude.rnf cors
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf functionUrl
      `Prelude.seq` Prelude.rnf functionArn
      `Prelude.seq` Prelude.rnf authType
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
