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
-- Module      : Amazonka.APIGateway.GetSdk
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a client SDK for a RestApi and Stage.
module Amazonka.APIGateway.GetSdk
  ( -- * Creating a Request
    GetSdk (..),
    newGetSdk,

    -- * Request Lenses
    getSdk_parameters,
    getSdk_restApiId,
    getSdk_stageName,
    getSdk_sdkType,

    -- * Destructuring the Response
    GetSdkResponse (..),
    newGetSdkResponse,

    -- * Response Lenses
    getSdkResponse_body,
    getSdkResponse_contentDisposition,
    getSdkResponse_contentType,
    getSdkResponse_httpStatus,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request a new generated client SDK for a RestApi and Stage.
--
-- /See:/ 'newGetSdk' smart constructor.
data GetSdk = GetSdk'
  { -- | A string-to-string key-value map of query parameters @sdkType@-dependent
    -- properties of the SDK. For @sdkType@ of @objectivec@ or @swift@, a
    -- parameter named @classPrefix@ is required. For @sdkType@ of @android@,
    -- parameters named @groupId@, @artifactId@, @artifactVersion@, and
    -- @invokerPackage@ are required. For @sdkType@ of @java@, parameters named
    -- @serviceName@ and @javaPackageName@ are required.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The name of the Stage that the SDK will use.
    stageName :: Prelude.Text,
    -- | The language for the generated SDK. Currently @java@, @javascript@,
    -- @android@, @objectivec@ (for iOS), @swift@ (for iOS), and @ruby@ are
    -- supported.
    sdkType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSdk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'getSdk_parameters' - A string-to-string key-value map of query parameters @sdkType@-dependent
-- properties of the SDK. For @sdkType@ of @objectivec@ or @swift@, a
-- parameter named @classPrefix@ is required. For @sdkType@ of @android@,
-- parameters named @groupId@, @artifactId@, @artifactVersion@, and
-- @invokerPackage@ are required. For @sdkType@ of @java@, parameters named
-- @serviceName@ and @javaPackageName@ are required.
--
-- 'restApiId', 'getSdk_restApiId' - The string identifier of the associated RestApi.
--
-- 'stageName', 'getSdk_stageName' - The name of the Stage that the SDK will use.
--
-- 'sdkType', 'getSdk_sdkType' - The language for the generated SDK. Currently @java@, @javascript@,
-- @android@, @objectivec@ (for iOS), @swift@ (for iOS), and @ruby@ are
-- supported.
newGetSdk ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  -- | 'sdkType'
  Prelude.Text ->
  GetSdk
newGetSdk pRestApiId_ pStageName_ pSdkType_ =
  GetSdk'
    { parameters = Prelude.Nothing,
      restApiId = pRestApiId_,
      stageName = pStageName_,
      sdkType = pSdkType_
    }

-- | A string-to-string key-value map of query parameters @sdkType@-dependent
-- properties of the SDK. For @sdkType@ of @objectivec@ or @swift@, a
-- parameter named @classPrefix@ is required. For @sdkType@ of @android@,
-- parameters named @groupId@, @artifactId@, @artifactVersion@, and
-- @invokerPackage@ are required. For @sdkType@ of @java@, parameters named
-- @serviceName@ and @javaPackageName@ are required.
getSdk_parameters :: Lens.Lens' GetSdk (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getSdk_parameters = Lens.lens (\GetSdk' {parameters} -> parameters) (\s@GetSdk' {} a -> s {parameters = a} :: GetSdk) Prelude.. Lens.mapping Lens.coerced

-- | The string identifier of the associated RestApi.
getSdk_restApiId :: Lens.Lens' GetSdk Prelude.Text
getSdk_restApiId = Lens.lens (\GetSdk' {restApiId} -> restApiId) (\s@GetSdk' {} a -> s {restApiId = a} :: GetSdk)

-- | The name of the Stage that the SDK will use.
getSdk_stageName :: Lens.Lens' GetSdk Prelude.Text
getSdk_stageName = Lens.lens (\GetSdk' {stageName} -> stageName) (\s@GetSdk' {} a -> s {stageName = a} :: GetSdk)

-- | The language for the generated SDK. Currently @java@, @javascript@,
-- @android@, @objectivec@ (for iOS), @swift@ (for iOS), and @ruby@ are
-- supported.
getSdk_sdkType :: Lens.Lens' GetSdk Prelude.Text
getSdk_sdkType = Lens.lens (\GetSdk' {sdkType} -> sdkType) (\s@GetSdk' {} a -> s {sdkType = a} :: GetSdk)

instance Core.AWSRequest GetSdk where
  type AWSResponse GetSdk = GetSdkResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBytes
      ( \s h x ->
          GetSdkResponse'
            Prelude.<$> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (h Data..#? "Content-Disposition")
            Prelude.<*> (h Data..#? "Content-Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSdk where
  hashWithSalt _salt GetSdk' {..} =
    _salt
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` sdkType

instance Prelude.NFData GetSdk where
  rnf GetSdk' {..} =
    Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf stageName
      `Prelude.seq` Prelude.rnf sdkType

instance Data.ToHeaders GetSdk where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetSdk where
  toPath GetSdk' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/stages/",
        Data.toBS stageName,
        "/sdks/",
        Data.toBS sdkType
      ]

instance Data.ToQuery GetSdk where
  toQuery GetSdk' {..} =
    Prelude.mconcat
      [ "parameters"
          Data.=: Data.toQuery
            ( Data.toQueryMap "entry" "key" "value"
                Prelude.<$> parameters
            )
      ]

-- | The binary blob response to GetSdk, which contains the generated SDK.
--
-- /See:/ 'newGetSdkResponse' smart constructor.
data GetSdkResponse = GetSdkResponse'
  { -- | The binary blob response to GetSdk, which contains the generated SDK.
    body :: Prelude.Maybe Prelude.ByteString,
    -- | The content-disposition header value in the HTTP response.
    contentDisposition :: Prelude.Maybe Prelude.Text,
    -- | The content-type header value in the HTTP response.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSdkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'body', 'getSdkResponse_body' - The binary blob response to GetSdk, which contains the generated SDK.
--
-- 'contentDisposition', 'getSdkResponse_contentDisposition' - The content-disposition header value in the HTTP response.
--
-- 'contentType', 'getSdkResponse_contentType' - The content-type header value in the HTTP response.
--
-- 'httpStatus', 'getSdkResponse_httpStatus' - The response's http status code.
newGetSdkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSdkResponse
newGetSdkResponse pHttpStatus_ =
  GetSdkResponse'
    { body = Prelude.Nothing,
      contentDisposition = Prelude.Nothing,
      contentType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The binary blob response to GetSdk, which contains the generated SDK.
getSdkResponse_body :: Lens.Lens' GetSdkResponse (Prelude.Maybe Prelude.ByteString)
getSdkResponse_body = Lens.lens (\GetSdkResponse' {body} -> body) (\s@GetSdkResponse' {} a -> s {body = a} :: GetSdkResponse)

-- | The content-disposition header value in the HTTP response.
getSdkResponse_contentDisposition :: Lens.Lens' GetSdkResponse (Prelude.Maybe Prelude.Text)
getSdkResponse_contentDisposition = Lens.lens (\GetSdkResponse' {contentDisposition} -> contentDisposition) (\s@GetSdkResponse' {} a -> s {contentDisposition = a} :: GetSdkResponse)

-- | The content-type header value in the HTTP response.
getSdkResponse_contentType :: Lens.Lens' GetSdkResponse (Prelude.Maybe Prelude.Text)
getSdkResponse_contentType = Lens.lens (\GetSdkResponse' {contentType} -> contentType) (\s@GetSdkResponse' {} a -> s {contentType = a} :: GetSdkResponse)

-- | The response's http status code.
getSdkResponse_httpStatus :: Lens.Lens' GetSdkResponse Prelude.Int
getSdkResponse_httpStatus = Lens.lens (\GetSdkResponse' {httpStatus} -> httpStatus) (\s@GetSdkResponse' {} a -> s {httpStatus = a} :: GetSdkResponse)

instance Prelude.NFData GetSdkResponse where
  rnf GetSdkResponse' {..} =
    Prelude.rnf body
      `Prelude.seq` Prelude.rnf contentDisposition
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf httpStatus
