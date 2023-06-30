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
-- Module      : Amazonka.Lambda.GetFunctionUrlConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about a Lambda function URL.
module Amazonka.Lambda.GetFunctionUrlConfig
  ( -- * Creating a Request
    GetFunctionUrlConfig (..),
    newGetFunctionUrlConfig,

    -- * Request Lenses
    getFunctionUrlConfig_qualifier,
    getFunctionUrlConfig_functionName,

    -- * Destructuring the Response
    GetFunctionUrlConfigResponse (..),
    newGetFunctionUrlConfigResponse,

    -- * Response Lenses
    getFunctionUrlConfigResponse_cors,
    getFunctionUrlConfigResponse_httpStatus,
    getFunctionUrlConfigResponse_functionUrl,
    getFunctionUrlConfigResponse_functionArn,
    getFunctionUrlConfigResponse_authType,
    getFunctionUrlConfigResponse_creationTime,
    getFunctionUrlConfigResponse_lastModifiedTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFunctionUrlConfig' smart constructor.
data GetFunctionUrlConfig = GetFunctionUrlConfig'
  { -- | The alias name.
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
-- Create a value of 'GetFunctionUrlConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualifier', 'getFunctionUrlConfig_qualifier' - The alias name.
--
-- 'functionName', 'getFunctionUrlConfig_functionName' - The name of the Lambda function.
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
newGetFunctionUrlConfig ::
  -- | 'functionName'
  Prelude.Text ->
  GetFunctionUrlConfig
newGetFunctionUrlConfig pFunctionName_ =
  GetFunctionUrlConfig'
    { qualifier = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | The alias name.
getFunctionUrlConfig_qualifier :: Lens.Lens' GetFunctionUrlConfig (Prelude.Maybe Prelude.Text)
getFunctionUrlConfig_qualifier = Lens.lens (\GetFunctionUrlConfig' {qualifier} -> qualifier) (\s@GetFunctionUrlConfig' {} a -> s {qualifier = a} :: GetFunctionUrlConfig)

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
getFunctionUrlConfig_functionName :: Lens.Lens' GetFunctionUrlConfig Prelude.Text
getFunctionUrlConfig_functionName = Lens.lens (\GetFunctionUrlConfig' {functionName} -> functionName) (\s@GetFunctionUrlConfig' {} a -> s {functionName = a} :: GetFunctionUrlConfig)

instance Core.AWSRequest GetFunctionUrlConfig where
  type
    AWSResponse GetFunctionUrlConfig =
      GetFunctionUrlConfigResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFunctionUrlConfigResponse'
            Prelude.<$> (x Data..?> "Cors")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "FunctionUrl")
            Prelude.<*> (x Data..:> "FunctionArn")
            Prelude.<*> (x Data..:> "AuthType")
            Prelude.<*> (x Data..:> "CreationTime")
            Prelude.<*> (x Data..:> "LastModifiedTime")
      )

instance Prelude.Hashable GetFunctionUrlConfig where
  hashWithSalt _salt GetFunctionUrlConfig' {..} =
    _salt
      `Prelude.hashWithSalt` qualifier
      `Prelude.hashWithSalt` functionName

instance Prelude.NFData GetFunctionUrlConfig where
  rnf GetFunctionUrlConfig' {..} =
    Prelude.rnf qualifier
      `Prelude.seq` Prelude.rnf functionName

instance Data.ToHeaders GetFunctionUrlConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetFunctionUrlConfig where
  toPath GetFunctionUrlConfig' {..} =
    Prelude.mconcat
      [ "/2021-10-31/functions/",
        Data.toBS functionName,
        "/url"
      ]

instance Data.ToQuery GetFunctionUrlConfig where
  toQuery GetFunctionUrlConfig' {..} =
    Prelude.mconcat ["Qualifier" Data.=: qualifier]

-- | /See:/ 'newGetFunctionUrlConfigResponse' smart constructor.
data GetFunctionUrlConfigResponse = GetFunctionUrlConfigResponse'
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
    -- if you want to restrict access to authenticated IAM users only. Set to
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
-- Create a value of 'GetFunctionUrlConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cors', 'getFunctionUrlConfigResponse_cors' - The
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS cross-origin resource sharing (CORS)>
-- settings for your function URL.
--
-- 'httpStatus', 'getFunctionUrlConfigResponse_httpStatus' - The response's http status code.
--
-- 'functionUrl', 'getFunctionUrlConfigResponse_functionUrl' - The HTTP URL endpoint for your function.
--
-- 'functionArn', 'getFunctionUrlConfigResponse_functionArn' - The Amazon Resource Name (ARN) of your function.
--
-- 'authType', 'getFunctionUrlConfigResponse_authType' - The type of authentication that your function URL uses. Set to @AWS_IAM@
-- if you want to restrict access to authenticated IAM users only. Set to
-- @NONE@ if you want to bypass IAM authentication to create a public
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/urls-auth.html Security and auth model for Lambda function URLs>.
--
-- 'creationTime', 'getFunctionUrlConfigResponse_creationTime' - When the function URL was created, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- 'lastModifiedTime', 'getFunctionUrlConfigResponse_lastModifiedTime' - When the function URL configuration was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
newGetFunctionUrlConfigResponse ::
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
  GetFunctionUrlConfigResponse
newGetFunctionUrlConfigResponse
  pHttpStatus_
  pFunctionUrl_
  pFunctionArn_
  pAuthType_
  pCreationTime_
  pLastModifiedTime_ =
    GetFunctionUrlConfigResponse'
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
getFunctionUrlConfigResponse_cors :: Lens.Lens' GetFunctionUrlConfigResponse (Prelude.Maybe Cors)
getFunctionUrlConfigResponse_cors = Lens.lens (\GetFunctionUrlConfigResponse' {cors} -> cors) (\s@GetFunctionUrlConfigResponse' {} a -> s {cors = a} :: GetFunctionUrlConfigResponse)

-- | The response's http status code.
getFunctionUrlConfigResponse_httpStatus :: Lens.Lens' GetFunctionUrlConfigResponse Prelude.Int
getFunctionUrlConfigResponse_httpStatus = Lens.lens (\GetFunctionUrlConfigResponse' {httpStatus} -> httpStatus) (\s@GetFunctionUrlConfigResponse' {} a -> s {httpStatus = a} :: GetFunctionUrlConfigResponse)

-- | The HTTP URL endpoint for your function.
getFunctionUrlConfigResponse_functionUrl :: Lens.Lens' GetFunctionUrlConfigResponse Prelude.Text
getFunctionUrlConfigResponse_functionUrl = Lens.lens (\GetFunctionUrlConfigResponse' {functionUrl} -> functionUrl) (\s@GetFunctionUrlConfigResponse' {} a -> s {functionUrl = a} :: GetFunctionUrlConfigResponse)

-- | The Amazon Resource Name (ARN) of your function.
getFunctionUrlConfigResponse_functionArn :: Lens.Lens' GetFunctionUrlConfigResponse Prelude.Text
getFunctionUrlConfigResponse_functionArn = Lens.lens (\GetFunctionUrlConfigResponse' {functionArn} -> functionArn) (\s@GetFunctionUrlConfigResponse' {} a -> s {functionArn = a} :: GetFunctionUrlConfigResponse)

-- | The type of authentication that your function URL uses. Set to @AWS_IAM@
-- if you want to restrict access to authenticated IAM users only. Set to
-- @NONE@ if you want to bypass IAM authentication to create a public
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/urls-auth.html Security and auth model for Lambda function URLs>.
getFunctionUrlConfigResponse_authType :: Lens.Lens' GetFunctionUrlConfigResponse FunctionUrlAuthType
getFunctionUrlConfigResponse_authType = Lens.lens (\GetFunctionUrlConfigResponse' {authType} -> authType) (\s@GetFunctionUrlConfigResponse' {} a -> s {authType = a} :: GetFunctionUrlConfigResponse)

-- | When the function URL was created, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
getFunctionUrlConfigResponse_creationTime :: Lens.Lens' GetFunctionUrlConfigResponse Prelude.Text
getFunctionUrlConfigResponse_creationTime = Lens.lens (\GetFunctionUrlConfigResponse' {creationTime} -> creationTime) (\s@GetFunctionUrlConfigResponse' {} a -> s {creationTime = a} :: GetFunctionUrlConfigResponse)

-- | When the function URL configuration was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
getFunctionUrlConfigResponse_lastModifiedTime :: Lens.Lens' GetFunctionUrlConfigResponse Prelude.Text
getFunctionUrlConfigResponse_lastModifiedTime = Lens.lens (\GetFunctionUrlConfigResponse' {lastModifiedTime} -> lastModifiedTime) (\s@GetFunctionUrlConfigResponse' {} a -> s {lastModifiedTime = a} :: GetFunctionUrlConfigResponse)

instance Prelude.NFData GetFunctionUrlConfigResponse where
  rnf GetFunctionUrlConfigResponse' {..} =
    Prelude.rnf cors
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf functionUrl
      `Prelude.seq` Prelude.rnf functionArn
      `Prelude.seq` Prelude.rnf authType
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
