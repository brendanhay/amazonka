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
-- Module      : Network.AWS.APIGateway.GetIntegrationResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a get integration response.
module Network.AWS.APIGateway.GetIntegrationResponse
  ( -- * Creating a Request
    GetIntegrationResponse (..),
    newGetIntegrationResponse,

    -- * Request Lenses
    getIntegrationResponse_restApiId,
    getIntegrationResponse_resourceId,
    getIntegrationResponse_httpMethod,
    getIntegrationResponse_statusCode,

    -- * Destructuring the Response
    IntegrationResponse (..),
    newIntegrationResponse,

    -- * Response Lenses
    integrationResponse_contentHandling,
    integrationResponse_responseTemplates,
    integrationResponse_statusCode,
    integrationResponse_responseParameters,
    integrationResponse_selectionPattern,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a get integration response request.
--
-- /See:/ 'newGetIntegrationResponse' smart constructor.
data GetIntegrationResponse = GetIntegrationResponse'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] Specifies a get integration response request\'s resource
    -- identifier.
    resourceId :: Core.Text,
    -- | [Required] Specifies a get integration response request\'s HTTP method.
    httpMethod :: Core.Text,
    -- | [Required] Specifies a get integration response request\'s status code.
    statusCode :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetIntegrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getIntegrationResponse_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'resourceId', 'getIntegrationResponse_resourceId' - [Required] Specifies a get integration response request\'s resource
-- identifier.
--
-- 'httpMethod', 'getIntegrationResponse_httpMethod' - [Required] Specifies a get integration response request\'s HTTP method.
--
-- 'statusCode', 'getIntegrationResponse_statusCode' - [Required] Specifies a get integration response request\'s status code.
newGetIntegrationResponse ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'resourceId'
  Core.Text ->
  -- | 'httpMethod'
  Core.Text ->
  -- | 'statusCode'
  Core.Text ->
  GetIntegrationResponse
newGetIntegrationResponse
  pRestApiId_
  pResourceId_
  pHttpMethod_
  pStatusCode_ =
    GetIntegrationResponse'
      { restApiId = pRestApiId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_,
        statusCode = pStatusCode_
      }

-- | [Required] The string identifier of the associated RestApi.
getIntegrationResponse_restApiId :: Lens.Lens' GetIntegrationResponse Core.Text
getIntegrationResponse_restApiId = Lens.lens (\GetIntegrationResponse' {restApiId} -> restApiId) (\s@GetIntegrationResponse' {} a -> s {restApiId = a} :: GetIntegrationResponse)

-- | [Required] Specifies a get integration response request\'s resource
-- identifier.
getIntegrationResponse_resourceId :: Lens.Lens' GetIntegrationResponse Core.Text
getIntegrationResponse_resourceId = Lens.lens (\GetIntegrationResponse' {resourceId} -> resourceId) (\s@GetIntegrationResponse' {} a -> s {resourceId = a} :: GetIntegrationResponse)

-- | [Required] Specifies a get integration response request\'s HTTP method.
getIntegrationResponse_httpMethod :: Lens.Lens' GetIntegrationResponse Core.Text
getIntegrationResponse_httpMethod = Lens.lens (\GetIntegrationResponse' {httpMethod} -> httpMethod) (\s@GetIntegrationResponse' {} a -> s {httpMethod = a} :: GetIntegrationResponse)

-- | [Required] Specifies a get integration response request\'s status code.
getIntegrationResponse_statusCode :: Lens.Lens' GetIntegrationResponse Core.Text
getIntegrationResponse_statusCode = Lens.lens (\GetIntegrationResponse' {statusCode} -> statusCode) (\s@GetIntegrationResponse' {} a -> s {statusCode = a} :: GetIntegrationResponse)

instance Core.AWSRequest GetIntegrationResponse where
  type
    AWSResponse GetIntegrationResponse =
      IntegrationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable GetIntegrationResponse

instance Core.NFData GetIntegrationResponse

instance Core.ToHeaders GetIntegrationResponse where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetIntegrationResponse where
  toPath GetIntegrationResponse' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/resources/",
        Core.toBS resourceId,
        "/methods/",
        Core.toBS httpMethod,
        "/integration/responses/",
        Core.toBS statusCode
      ]

instance Core.ToQuery GetIntegrationResponse where
  toQuery = Core.const Core.mempty
