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
-- Module      : Network.AWS.APIGateway.PutMethodResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a MethodResponse to an existing Method resource.
module Network.AWS.APIGateway.PutMethodResponse
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
    methodResponse_statusCode,
    methodResponse_responseParameters,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to add a MethodResponse to an existing Method resource.
--
-- /See:/ 'newPutMethodResponse' smart constructor.
data PutMethodResponse = PutMethodResponse'
  { -- | Specifies the Model resources used for the response\'s content type.
    -- Response models are represented as a key\/value map, with a content type
    -- as the key and a Model name as the value.
    responseModels :: Core.Maybe (Core.HashMap Core.Text Core.Text),
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
    responseParameters :: Core.Maybe (Core.HashMap Core.Text Core.Bool),
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] The Resource identifier for the Method resource.
    resourceId :: Core.Text,
    -- | [Required] The HTTP verb of the Method resource.
    httpMethod :: Core.Text,
    -- | [Required] The method response\'s status code.
    statusCode :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'restApiId', 'putMethodResponse_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'resourceId', 'putMethodResponse_resourceId' - [Required] The Resource identifier for the Method resource.
--
-- 'httpMethod', 'putMethodResponse_httpMethod' - [Required] The HTTP verb of the Method resource.
--
-- 'statusCode', 'putMethodResponse_statusCode' - [Required] The method response\'s status code.
newPutMethodResponse ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'resourceId'
  Core.Text ->
  -- | 'httpMethod'
  Core.Text ->
  -- | 'statusCode'
  Core.Text ->
  PutMethodResponse
newPutMethodResponse
  pRestApiId_
  pResourceId_
  pHttpMethod_
  pStatusCode_ =
    PutMethodResponse'
      { responseModels = Core.Nothing,
        responseParameters = Core.Nothing,
        restApiId = pRestApiId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_,
        statusCode = pStatusCode_
      }

-- | Specifies the Model resources used for the response\'s content type.
-- Response models are represented as a key\/value map, with a content type
-- as the key and a Model name as the value.
putMethodResponse_responseModels :: Lens.Lens' PutMethodResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
putMethodResponse_responseModels = Lens.lens (\PutMethodResponse' {responseModels} -> responseModels) (\s@PutMethodResponse' {} a -> s {responseModels = a} :: PutMethodResponse) Core.. Lens.mapping Lens._Coerce

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
putMethodResponse_responseParameters :: Lens.Lens' PutMethodResponse (Core.Maybe (Core.HashMap Core.Text Core.Bool))
putMethodResponse_responseParameters = Lens.lens (\PutMethodResponse' {responseParameters} -> responseParameters) (\s@PutMethodResponse' {} a -> s {responseParameters = a} :: PutMethodResponse) Core.. Lens.mapping Lens._Coerce

-- | [Required] The string identifier of the associated RestApi.
putMethodResponse_restApiId :: Lens.Lens' PutMethodResponse Core.Text
putMethodResponse_restApiId = Lens.lens (\PutMethodResponse' {restApiId} -> restApiId) (\s@PutMethodResponse' {} a -> s {restApiId = a} :: PutMethodResponse)

-- | [Required] The Resource identifier for the Method resource.
putMethodResponse_resourceId :: Lens.Lens' PutMethodResponse Core.Text
putMethodResponse_resourceId = Lens.lens (\PutMethodResponse' {resourceId} -> resourceId) (\s@PutMethodResponse' {} a -> s {resourceId = a} :: PutMethodResponse)

-- | [Required] The HTTP verb of the Method resource.
putMethodResponse_httpMethod :: Lens.Lens' PutMethodResponse Core.Text
putMethodResponse_httpMethod = Lens.lens (\PutMethodResponse' {httpMethod} -> httpMethod) (\s@PutMethodResponse' {} a -> s {httpMethod = a} :: PutMethodResponse)

-- | [Required] The method response\'s status code.
putMethodResponse_statusCode :: Lens.Lens' PutMethodResponse Core.Text
putMethodResponse_statusCode = Lens.lens (\PutMethodResponse' {statusCode} -> statusCode) (\s@PutMethodResponse' {} a -> s {statusCode = a} :: PutMethodResponse)

instance Core.AWSRequest PutMethodResponse where
  type AWSResponse PutMethodResponse = MethodResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable PutMethodResponse

instance Core.NFData PutMethodResponse

instance Core.ToHeaders PutMethodResponse where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutMethodResponse where
  toJSON PutMethodResponse' {..} =
    Core.object
      ( Core.catMaybes
          [ ("responseModels" Core..=) Core.<$> responseModels,
            ("responseParameters" Core..=)
              Core.<$> responseParameters
          ]
      )

instance Core.ToPath PutMethodResponse where
  toPath PutMethodResponse' {..} =
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

instance Core.ToQuery PutMethodResponse where
  toQuery = Core.const Core.mempty
