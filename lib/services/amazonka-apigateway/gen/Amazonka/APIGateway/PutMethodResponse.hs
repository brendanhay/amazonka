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
-- Module      : Amazonka.APIGateway.PutMethodResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a MethodResponse to an existing Method resource.
module Amazonka.APIGateway.PutMethodResponse
  ( -- * Creating a Request
    PutMethodResponse (..),
    newPutMethodResponse,

    -- * Request Lenses
    putMethodResponse_responseModels,
    putMethodResponse_responseParameters,
    putMethodResponse_restApiId,
    putMethodResponse_resourceId,
    putMethodResponse_httpMethod,
    putMethodResponse_statusCode,

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

-- | Request to add a MethodResponse to an existing Method resource.
--
-- /See:/ 'newPutMethodResponse' smart constructor.
data PutMethodResponse = PutMethodResponse'
  { -- | Specifies the Model resources used for the response\'s content type.
    -- Response models are represented as a key\/value map, with a content type
    -- as the key and a Model name as the value.
    responseModels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A key-value map specifying required or optional response parameters that
    -- API Gateway can send back to the caller. A key defines a method response
    -- header name and the associated value is a Boolean flag indicating
    -- whether the method response parameter is required or not. The method
    -- response header names must match the pattern of
    -- @method.response.header.{name}@, where @name@ is a valid and unique
    -- header name. The response parameter names defined here are available in
    -- the integration response to be mapped from an integration response
    -- header expressed in @integration.response.header.{name}@, a static value
    -- enclosed within a pair of single quotes (e.g., @\'application\/json\'@),
    -- or a JSON expression from the back-end response payload in the form of
    -- @integration.response.body.{JSON-expression}@, where @JSON-expression@
    -- is a valid JSON expression without the @$@ prefix.)
    responseParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Bool),
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The Resource identifier for the Method resource.
    resourceId :: Prelude.Text,
    -- | The HTTP verb of the Method resource.
    httpMethod :: Prelude.Text,
    -- | The method response\'s status code.
    statusCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMethodResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseModels', 'putMethodResponse_responseModels' - Specifies the Model resources used for the response\'s content type.
-- Response models are represented as a key\/value map, with a content type
-- as the key and a Model name as the value.
--
-- 'responseParameters', 'putMethodResponse_responseParameters' - A key-value map specifying required or optional response parameters that
-- API Gateway can send back to the caller. A key defines a method response
-- header name and the associated value is a Boolean flag indicating
-- whether the method response parameter is required or not. The method
-- response header names must match the pattern of
-- @method.response.header.{name}@, where @name@ is a valid and unique
-- header name. The response parameter names defined here are available in
-- the integration response to be mapped from an integration response
-- header expressed in @integration.response.header.{name}@, a static value
-- enclosed within a pair of single quotes (e.g., @\'application\/json\'@),
-- or a JSON expression from the back-end response payload in the form of
-- @integration.response.body.{JSON-expression}@, where @JSON-expression@
-- is a valid JSON expression without the @$@ prefix.)
--
-- 'restApiId', 'putMethodResponse_restApiId' - The string identifier of the associated RestApi.
--
-- 'resourceId', 'putMethodResponse_resourceId' - The Resource identifier for the Method resource.
--
-- 'httpMethod', 'putMethodResponse_httpMethod' - The HTTP verb of the Method resource.
--
-- 'statusCode', 'putMethodResponse_statusCode' - The method response\'s status code.
newPutMethodResponse ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'httpMethod'
  Prelude.Text ->
  -- | 'statusCode'
  Prelude.Text ->
  PutMethodResponse
newPutMethodResponse
  pRestApiId_
  pResourceId_
  pHttpMethod_
  pStatusCode_ =
    PutMethodResponse'
      { responseModels =
          Prelude.Nothing,
        responseParameters = Prelude.Nothing,
        restApiId = pRestApiId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_,
        statusCode = pStatusCode_
      }

-- | Specifies the Model resources used for the response\'s content type.
-- Response models are represented as a key\/value map, with a content type
-- as the key and a Model name as the value.
putMethodResponse_responseModels :: Lens.Lens' PutMethodResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putMethodResponse_responseModels = Lens.lens (\PutMethodResponse' {responseModels} -> responseModels) (\s@PutMethodResponse' {} a -> s {responseModels = a} :: PutMethodResponse) Prelude.. Lens.mapping Lens.coerced

-- | A key-value map specifying required or optional response parameters that
-- API Gateway can send back to the caller. A key defines a method response
-- header name and the associated value is a Boolean flag indicating
-- whether the method response parameter is required or not. The method
-- response header names must match the pattern of
-- @method.response.header.{name}@, where @name@ is a valid and unique
-- header name. The response parameter names defined here are available in
-- the integration response to be mapped from an integration response
-- header expressed in @integration.response.header.{name}@, a static value
-- enclosed within a pair of single quotes (e.g., @\'application\/json\'@),
-- or a JSON expression from the back-end response payload in the form of
-- @integration.response.body.{JSON-expression}@, where @JSON-expression@
-- is a valid JSON expression without the @$@ prefix.)
putMethodResponse_responseParameters :: Lens.Lens' PutMethodResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Bool))
putMethodResponse_responseParameters = Lens.lens (\PutMethodResponse' {responseParameters} -> responseParameters) (\s@PutMethodResponse' {} a -> s {responseParameters = a} :: PutMethodResponse) Prelude.. Lens.mapping Lens.coerced

-- | The string identifier of the associated RestApi.
putMethodResponse_restApiId :: Lens.Lens' PutMethodResponse Prelude.Text
putMethodResponse_restApiId = Lens.lens (\PutMethodResponse' {restApiId} -> restApiId) (\s@PutMethodResponse' {} a -> s {restApiId = a} :: PutMethodResponse)

-- | The Resource identifier for the Method resource.
putMethodResponse_resourceId :: Lens.Lens' PutMethodResponse Prelude.Text
putMethodResponse_resourceId = Lens.lens (\PutMethodResponse' {resourceId} -> resourceId) (\s@PutMethodResponse' {} a -> s {resourceId = a} :: PutMethodResponse)

-- | The HTTP verb of the Method resource.
putMethodResponse_httpMethod :: Lens.Lens' PutMethodResponse Prelude.Text
putMethodResponse_httpMethod = Lens.lens (\PutMethodResponse' {httpMethod} -> httpMethod) (\s@PutMethodResponse' {} a -> s {httpMethod = a} :: PutMethodResponse)

-- | The method response\'s status code.
putMethodResponse_statusCode :: Lens.Lens' PutMethodResponse Prelude.Text
putMethodResponse_statusCode = Lens.lens (\PutMethodResponse' {statusCode} -> statusCode) (\s@PutMethodResponse' {} a -> s {statusCode = a} :: PutMethodResponse)

instance Core.AWSRequest PutMethodResponse where
  type AWSResponse PutMethodResponse = MethodResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable PutMethodResponse where
  hashWithSalt _salt PutMethodResponse' {..} =
    _salt
      `Prelude.hashWithSalt` responseModels
      `Prelude.hashWithSalt` responseParameters
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` httpMethod
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData PutMethodResponse where
  rnf PutMethodResponse' {..} =
    Prelude.rnf responseModels `Prelude.seq`
      Prelude.rnf responseParameters `Prelude.seq`
        Prelude.rnf restApiId `Prelude.seq`
          Prelude.rnf resourceId `Prelude.seq`
            Prelude.rnf httpMethod `Prelude.seq`
              Prelude.rnf statusCode

instance Data.ToHeaders PutMethodResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON PutMethodResponse where
  toJSON PutMethodResponse' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("responseModels" Data..=)
              Prelude.<$> responseModels,
            ("responseParameters" Data..=)
              Prelude.<$> responseParameters
          ]
      )

instance Data.ToPath PutMethodResponse where
  toPath PutMethodResponse' {..} =
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

instance Data.ToQuery PutMethodResponse where
  toQuery = Prelude.const Prelude.mempty
