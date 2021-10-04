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
-- Module      : Network.AWS.APIGateway.GetSdk
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a client SDK for a RestApi and Stage.
module Network.AWS.APIGateway.GetSdk
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
    getSdkResponse_contentType,
    getSdkResponse_contentDisposition,
    getSdkResponse_body,
    getSdkResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The name of the Stage that the SDK will use.
    stageName :: Prelude.Text,
    -- | [Required] The language for the generated SDK. Currently @java@,
    -- @javascript@, @android@, @objectivec@ (for iOS), @swift@ (for iOS), and
    -- @ruby@ are supported.
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
-- 'restApiId', 'getSdk_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'stageName', 'getSdk_stageName' - [Required] The name of the Stage that the SDK will use.
--
-- 'sdkType', 'getSdk_sdkType' - [Required] The language for the generated SDK. Currently @java@,
-- @javascript@, @android@, @objectivec@ (for iOS), @swift@ (for iOS), and
-- @ruby@ are supported.
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
getSdk_parameters = Lens.lens (\GetSdk' {parameters} -> parameters) (\s@GetSdk' {} a -> s {parameters = a} :: GetSdk) Prelude.. Lens.mapping Lens._Coerce

-- | [Required] The string identifier of the associated RestApi.
getSdk_restApiId :: Lens.Lens' GetSdk Prelude.Text
getSdk_restApiId = Lens.lens (\GetSdk' {restApiId} -> restApiId) (\s@GetSdk' {} a -> s {restApiId = a} :: GetSdk)

-- | [Required] The name of the Stage that the SDK will use.
getSdk_stageName :: Lens.Lens' GetSdk Prelude.Text
getSdk_stageName = Lens.lens (\GetSdk' {stageName} -> stageName) (\s@GetSdk' {} a -> s {stageName = a} :: GetSdk)

-- | [Required] The language for the generated SDK. Currently @java@,
-- @javascript@, @android@, @objectivec@ (for iOS), @swift@ (for iOS), and
-- @ruby@ are supported.
getSdk_sdkType :: Lens.Lens' GetSdk Prelude.Text
getSdk_sdkType = Lens.lens (\GetSdk' {sdkType} -> sdkType) (\s@GetSdk' {} a -> s {sdkType = a} :: GetSdk)

instance Core.AWSRequest GetSdk where
  type AWSResponse GetSdk = GetSdkResponse
  request = Request.get defaultService
  response =
    Response.receiveBytes
      ( \s h x ->
          GetSdkResponse'
            Prelude.<$> (h Core..#? "Content-Type")
            Prelude.<*> (h Core..#? "Content-Disposition")
            Prelude.<*> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSdk

instance Prelude.NFData GetSdk

instance Core.ToHeaders GetSdk where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetSdk where
  toPath GetSdk' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/stages/",
        Core.toBS stageName,
        "/sdks/",
        Core.toBS sdkType
      ]

instance Core.ToQuery GetSdk where
  toQuery GetSdk' {..} =
    Prelude.mconcat
      [ "parameters"
          Core.=: Core.toQuery
            ( Core.toQueryMap "entry" "key" "value"
                Prelude.<$> parameters
            )
      ]

-- | The binary blob response to GetSdk, which contains the generated SDK.
--
-- /See:/ 'newGetSdkResponse' smart constructor.
data GetSdkResponse = GetSdkResponse'
  { -- | The content-type header value in the HTTP response.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The content-disposition header value in the HTTP response.
    contentDisposition :: Prelude.Maybe Prelude.Text,
    -- | The binary blob response to GetSdk, which contains the generated SDK.
    body :: Prelude.Maybe Prelude.ByteString,
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
-- 'contentType', 'getSdkResponse_contentType' - The content-type header value in the HTTP response.
--
-- 'contentDisposition', 'getSdkResponse_contentDisposition' - The content-disposition header value in the HTTP response.
--
-- 'body', 'getSdkResponse_body' - The binary blob response to GetSdk, which contains the generated SDK.
--
-- 'httpStatus', 'getSdkResponse_httpStatus' - The response's http status code.
newGetSdkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSdkResponse
newGetSdkResponse pHttpStatus_ =
  GetSdkResponse'
    { contentType = Prelude.Nothing,
      contentDisposition = Prelude.Nothing,
      body = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The content-type header value in the HTTP response.
getSdkResponse_contentType :: Lens.Lens' GetSdkResponse (Prelude.Maybe Prelude.Text)
getSdkResponse_contentType = Lens.lens (\GetSdkResponse' {contentType} -> contentType) (\s@GetSdkResponse' {} a -> s {contentType = a} :: GetSdkResponse)

-- | The content-disposition header value in the HTTP response.
getSdkResponse_contentDisposition :: Lens.Lens' GetSdkResponse (Prelude.Maybe Prelude.Text)
getSdkResponse_contentDisposition = Lens.lens (\GetSdkResponse' {contentDisposition} -> contentDisposition) (\s@GetSdkResponse' {} a -> s {contentDisposition = a} :: GetSdkResponse)

-- | The binary blob response to GetSdk, which contains the generated SDK.
getSdkResponse_body :: Lens.Lens' GetSdkResponse (Prelude.Maybe Prelude.ByteString)
getSdkResponse_body = Lens.lens (\GetSdkResponse' {body} -> body) (\s@GetSdkResponse' {} a -> s {body = a} :: GetSdkResponse)

-- | The response's http status code.
getSdkResponse_httpStatus :: Lens.Lens' GetSdkResponse Prelude.Int
getSdkResponse_httpStatus = Lens.lens (\GetSdkResponse' {httpStatus} -> httpStatus) (\s@GetSdkResponse' {} a -> s {httpStatus = a} :: GetSdkResponse)

instance Prelude.NFData GetSdkResponse
