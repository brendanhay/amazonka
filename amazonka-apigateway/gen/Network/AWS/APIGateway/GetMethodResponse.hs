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
-- Module      : Network.AWS.APIGateway.GetMethodResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a MethodResponse resource.
module Network.AWS.APIGateway.GetMethodResponse
  ( -- * Creating a Request
    GetMethodResponse (..),
    newGetMethodResponse,

    -- * Request Lenses
    getMethodResponse_restApiId,
    getMethodResponse_resourceId,
    getMethodResponse_httpMethod,
    getMethodResponse_statusCode,

    -- * Destructuring the Response
    MethodResponse (..),
    newMethodResponse,

    -- * Response Lenses
    methodResponse_responseModels,
    methodResponse_statusCode,
    methodResponse_responseParameters,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to describe a MethodResponse resource.
--
-- /See:/ 'newGetMethodResponse' smart constructor.
data GetMethodResponse = GetMethodResponse'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] The Resource identifier for the MethodResponse resource.
    resourceId :: Core.Text,
    -- | [Required] The HTTP verb of the Method resource.
    httpMethod :: Core.Text,
    -- | [Required] The status code for the MethodResponse resource.
    statusCode :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMethodResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getMethodResponse_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'resourceId', 'getMethodResponse_resourceId' - [Required] The Resource identifier for the MethodResponse resource.
--
-- 'httpMethod', 'getMethodResponse_httpMethod' - [Required] The HTTP verb of the Method resource.
--
-- 'statusCode', 'getMethodResponse_statusCode' - [Required] The status code for the MethodResponse resource.
newGetMethodResponse ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'resourceId'
  Core.Text ->
  -- | 'httpMethod'
  Core.Text ->
  -- | 'statusCode'
  Core.Text ->
  GetMethodResponse
newGetMethodResponse
  pRestApiId_
  pResourceId_
  pHttpMethod_
  pStatusCode_ =
    GetMethodResponse'
      { restApiId = pRestApiId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_,
        statusCode = pStatusCode_
      }

-- | [Required] The string identifier of the associated RestApi.
getMethodResponse_restApiId :: Lens.Lens' GetMethodResponse Core.Text
getMethodResponse_restApiId = Lens.lens (\GetMethodResponse' {restApiId} -> restApiId) (\s@GetMethodResponse' {} a -> s {restApiId = a} :: GetMethodResponse)

-- | [Required] The Resource identifier for the MethodResponse resource.
getMethodResponse_resourceId :: Lens.Lens' GetMethodResponse Core.Text
getMethodResponse_resourceId = Lens.lens (\GetMethodResponse' {resourceId} -> resourceId) (\s@GetMethodResponse' {} a -> s {resourceId = a} :: GetMethodResponse)

-- | [Required] The HTTP verb of the Method resource.
getMethodResponse_httpMethod :: Lens.Lens' GetMethodResponse Core.Text
getMethodResponse_httpMethod = Lens.lens (\GetMethodResponse' {httpMethod} -> httpMethod) (\s@GetMethodResponse' {} a -> s {httpMethod = a} :: GetMethodResponse)

-- | [Required] The status code for the MethodResponse resource.
getMethodResponse_statusCode :: Lens.Lens' GetMethodResponse Core.Text
getMethodResponse_statusCode = Lens.lens (\GetMethodResponse' {statusCode} -> statusCode) (\s@GetMethodResponse' {} a -> s {statusCode = a} :: GetMethodResponse)

instance Core.AWSRequest GetMethodResponse where
  type AWSResponse GetMethodResponse = MethodResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable GetMethodResponse

instance Core.NFData GetMethodResponse

instance Core.ToHeaders GetMethodResponse where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetMethodResponse where
  toPath GetMethodResponse' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/resources/",
        Core.toBS resourceId,
        "/methods/",
        Core.toBS httpMethod,
        "/responses/",
        Core.toBS statusCode
      ]

instance Core.ToQuery GetMethodResponse where
  toQuery = Core.const Core.mempty
