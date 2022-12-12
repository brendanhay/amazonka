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
-- Module      : Amazonka.APIGateway.GetMethodResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a MethodResponse resource.
module Amazonka.APIGateway.GetMethodResponse
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
    methodResponse_responseParameters,
    methodResponse_statusCode,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to describe a MethodResponse resource.
--
-- /See:/ 'newGetMethodResponse' smart constructor.
data GetMethodResponse = GetMethodResponse'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The Resource identifier for the MethodResponse resource.
    resourceId :: Prelude.Text,
    -- | The HTTP verb of the Method resource.
    httpMethod :: Prelude.Text,
    -- | The status code for the MethodResponse resource.
    statusCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMethodResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getMethodResponse_restApiId' - The string identifier of the associated RestApi.
--
-- 'resourceId', 'getMethodResponse_resourceId' - The Resource identifier for the MethodResponse resource.
--
-- 'httpMethod', 'getMethodResponse_httpMethod' - The HTTP verb of the Method resource.
--
-- 'statusCode', 'getMethodResponse_statusCode' - The status code for the MethodResponse resource.
newGetMethodResponse ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'httpMethod'
  Prelude.Text ->
  -- | 'statusCode'
  Prelude.Text ->
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

-- | The string identifier of the associated RestApi.
getMethodResponse_restApiId :: Lens.Lens' GetMethodResponse Prelude.Text
getMethodResponse_restApiId = Lens.lens (\GetMethodResponse' {restApiId} -> restApiId) (\s@GetMethodResponse' {} a -> s {restApiId = a} :: GetMethodResponse)

-- | The Resource identifier for the MethodResponse resource.
getMethodResponse_resourceId :: Lens.Lens' GetMethodResponse Prelude.Text
getMethodResponse_resourceId = Lens.lens (\GetMethodResponse' {resourceId} -> resourceId) (\s@GetMethodResponse' {} a -> s {resourceId = a} :: GetMethodResponse)

-- | The HTTP verb of the Method resource.
getMethodResponse_httpMethod :: Lens.Lens' GetMethodResponse Prelude.Text
getMethodResponse_httpMethod = Lens.lens (\GetMethodResponse' {httpMethod} -> httpMethod) (\s@GetMethodResponse' {} a -> s {httpMethod = a} :: GetMethodResponse)

-- | The status code for the MethodResponse resource.
getMethodResponse_statusCode :: Lens.Lens' GetMethodResponse Prelude.Text
getMethodResponse_statusCode = Lens.lens (\GetMethodResponse' {statusCode} -> statusCode) (\s@GetMethodResponse' {} a -> s {statusCode = a} :: GetMethodResponse)

instance Core.AWSRequest GetMethodResponse where
  type AWSResponse GetMethodResponse = MethodResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetMethodResponse where
  hashWithSalt _salt GetMethodResponse' {..} =
    _salt `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` httpMethod
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData GetMethodResponse where
  rnf GetMethodResponse' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf httpMethod
      `Prelude.seq` Prelude.rnf statusCode

instance Data.ToHeaders GetMethodResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetMethodResponse where
  toPath GetMethodResponse' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/resources/",
        Data.toBS resourceId,
        "/methods/",
        Data.toBS httpMethod,
        "/responses/",
        Data.toBS statusCode
      ]

instance Data.ToQuery GetMethodResponse where
  toQuery = Prelude.const Prelude.mempty
