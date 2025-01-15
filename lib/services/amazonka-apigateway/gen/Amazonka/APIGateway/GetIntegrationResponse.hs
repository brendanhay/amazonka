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
-- Module      : Amazonka.APIGateway.GetIntegrationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a get integration response.
module Amazonka.APIGateway.GetIntegrationResponse
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
    integrationResponse_responseParameters,
    integrationResponse_responseTemplates,
    integrationResponse_selectionPattern,
    integrationResponse_statusCode,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents a get integration response request.
--
-- /See:/ 'newGetIntegrationResponse' smart constructor.
data GetIntegrationResponse = GetIntegrationResponse'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | Specifies a get integration response request\'s resource identifier.
    resourceId :: Prelude.Text,
    -- | Specifies a get integration response request\'s HTTP method.
    httpMethod :: Prelude.Text,
    -- | Specifies a get integration response request\'s status code.
    statusCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIntegrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getIntegrationResponse_restApiId' - The string identifier of the associated RestApi.
--
-- 'resourceId', 'getIntegrationResponse_resourceId' - Specifies a get integration response request\'s resource identifier.
--
-- 'httpMethod', 'getIntegrationResponse_httpMethod' - Specifies a get integration response request\'s HTTP method.
--
-- 'statusCode', 'getIntegrationResponse_statusCode' - Specifies a get integration response request\'s status code.
newGetIntegrationResponse ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'httpMethod'
  Prelude.Text ->
  -- | 'statusCode'
  Prelude.Text ->
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

-- | The string identifier of the associated RestApi.
getIntegrationResponse_restApiId :: Lens.Lens' GetIntegrationResponse Prelude.Text
getIntegrationResponse_restApiId = Lens.lens (\GetIntegrationResponse' {restApiId} -> restApiId) (\s@GetIntegrationResponse' {} a -> s {restApiId = a} :: GetIntegrationResponse)

-- | Specifies a get integration response request\'s resource identifier.
getIntegrationResponse_resourceId :: Lens.Lens' GetIntegrationResponse Prelude.Text
getIntegrationResponse_resourceId = Lens.lens (\GetIntegrationResponse' {resourceId} -> resourceId) (\s@GetIntegrationResponse' {} a -> s {resourceId = a} :: GetIntegrationResponse)

-- | Specifies a get integration response request\'s HTTP method.
getIntegrationResponse_httpMethod :: Lens.Lens' GetIntegrationResponse Prelude.Text
getIntegrationResponse_httpMethod = Lens.lens (\GetIntegrationResponse' {httpMethod} -> httpMethod) (\s@GetIntegrationResponse' {} a -> s {httpMethod = a} :: GetIntegrationResponse)

-- | Specifies a get integration response request\'s status code.
getIntegrationResponse_statusCode :: Lens.Lens' GetIntegrationResponse Prelude.Text
getIntegrationResponse_statusCode = Lens.lens (\GetIntegrationResponse' {statusCode} -> statusCode) (\s@GetIntegrationResponse' {} a -> s {statusCode = a} :: GetIntegrationResponse)

instance Core.AWSRequest GetIntegrationResponse where
  type
    AWSResponse GetIntegrationResponse =
      IntegrationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetIntegrationResponse where
  hashWithSalt _salt GetIntegrationResponse' {..} =
    _salt
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` httpMethod
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData GetIntegrationResponse where
  rnf GetIntegrationResponse' {..} =
    Prelude.rnf restApiId `Prelude.seq`
      Prelude.rnf resourceId `Prelude.seq`
        Prelude.rnf httpMethod `Prelude.seq`
          Prelude.rnf statusCode

instance Data.ToHeaders GetIntegrationResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetIntegrationResponse where
  toPath GetIntegrationResponse' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/resources/",
        Data.toBS resourceId,
        "/methods/",
        Data.toBS httpMethod,
        "/integration/responses/",
        Data.toBS statusCode
      ]

instance Data.ToQuery GetIntegrationResponse where
  toQuery = Prelude.const Prelude.mempty
