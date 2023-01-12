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
-- Module      : Amazonka.APIGateway.PutIntegrationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a put integration.
module Amazonka.APIGateway.PutIntegrationResponse
  ( -- * Creating a Request
    PutIntegrationResponse (..),
    newPutIntegrationResponse,

    -- * Request Lenses
    putIntegrationResponse_contentHandling,
    putIntegrationResponse_responseParameters,
    putIntegrationResponse_responseTemplates,
    putIntegrationResponse_selectionPattern,
    putIntegrationResponse_restApiId,
    putIntegrationResponse_resourceId,
    putIntegrationResponse_httpMethod,
    putIntegrationResponse_statusCode,

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

-- | Represents a put integration response request.
--
-- /See:/ 'newPutIntegrationResponse' smart constructor.
data PutIntegrationResponse = PutIntegrationResponse'
  { -- | Specifies how to handle response payload content type conversions.
    -- Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@, with the
    -- following behaviors:
    --
    -- If this property is not defined, the response payload will be passed
    -- through from the integration response to the method response without
    -- modification.
    contentHandling :: Prelude.Maybe ContentHandlingStrategy,
    -- | A key-value map specifying response parameters that are passed to the
    -- method response from the back end. The key is a method response header
    -- parameter name and the mapped value is an integration response header
    -- value, a static value enclosed within a pair of single quotes, or a JSON
    -- expression from the integration response body. The mapping key must
    -- match the pattern of @method.response.header.{name}@, where @name@ is a
    -- valid and unique header name. The mapped non-static value must match the
    -- pattern of @integration.response.header.{name}@ or
    -- @integration.response.body.{JSON-expression}@, where @name@ must be a
    -- valid and unique response header name and @JSON-expression@ a valid JSON
    -- expression without the @$@ prefix.
    responseParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies a put integration response\'s templates.
    responseTemplates :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies the selection pattern of a put integration response.
    selectionPattern :: Prelude.Maybe Prelude.Text,
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | Specifies a put integration response request\'s resource identifier.
    resourceId :: Prelude.Text,
    -- | Specifies a put integration response request\'s HTTP method.
    httpMethod :: Prelude.Text,
    -- | Specifies the status code that is used to map the integration response
    -- to an existing MethodResponse.
    statusCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutIntegrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentHandling', 'putIntegrationResponse_contentHandling' - Specifies how to handle response payload content type conversions.
-- Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@, with the
-- following behaviors:
--
-- If this property is not defined, the response payload will be passed
-- through from the integration response to the method response without
-- modification.
--
-- 'responseParameters', 'putIntegrationResponse_responseParameters' - A key-value map specifying response parameters that are passed to the
-- method response from the back end. The key is a method response header
-- parameter name and the mapped value is an integration response header
-- value, a static value enclosed within a pair of single quotes, or a JSON
-- expression from the integration response body. The mapping key must
-- match the pattern of @method.response.header.{name}@, where @name@ is a
-- valid and unique header name. The mapped non-static value must match the
-- pattern of @integration.response.header.{name}@ or
-- @integration.response.body.{JSON-expression}@, where @name@ must be a
-- valid and unique response header name and @JSON-expression@ a valid JSON
-- expression without the @$@ prefix.
--
-- 'responseTemplates', 'putIntegrationResponse_responseTemplates' - Specifies a put integration response\'s templates.
--
-- 'selectionPattern', 'putIntegrationResponse_selectionPattern' - Specifies the selection pattern of a put integration response.
--
-- 'restApiId', 'putIntegrationResponse_restApiId' - The string identifier of the associated RestApi.
--
-- 'resourceId', 'putIntegrationResponse_resourceId' - Specifies a put integration response request\'s resource identifier.
--
-- 'httpMethod', 'putIntegrationResponse_httpMethod' - Specifies a put integration response request\'s HTTP method.
--
-- 'statusCode', 'putIntegrationResponse_statusCode' - Specifies the status code that is used to map the integration response
-- to an existing MethodResponse.
newPutIntegrationResponse ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'httpMethod'
  Prelude.Text ->
  -- | 'statusCode'
  Prelude.Text ->
  PutIntegrationResponse
newPutIntegrationResponse
  pRestApiId_
  pResourceId_
  pHttpMethod_
  pStatusCode_ =
    PutIntegrationResponse'
      { contentHandling =
          Prelude.Nothing,
        responseParameters = Prelude.Nothing,
        responseTemplates = Prelude.Nothing,
        selectionPattern = Prelude.Nothing,
        restApiId = pRestApiId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_,
        statusCode = pStatusCode_
      }

-- | Specifies how to handle response payload content type conversions.
-- Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@, with the
-- following behaviors:
--
-- If this property is not defined, the response payload will be passed
-- through from the integration response to the method response without
-- modification.
putIntegrationResponse_contentHandling :: Lens.Lens' PutIntegrationResponse (Prelude.Maybe ContentHandlingStrategy)
putIntegrationResponse_contentHandling = Lens.lens (\PutIntegrationResponse' {contentHandling} -> contentHandling) (\s@PutIntegrationResponse' {} a -> s {contentHandling = a} :: PutIntegrationResponse)

-- | A key-value map specifying response parameters that are passed to the
-- method response from the back end. The key is a method response header
-- parameter name and the mapped value is an integration response header
-- value, a static value enclosed within a pair of single quotes, or a JSON
-- expression from the integration response body. The mapping key must
-- match the pattern of @method.response.header.{name}@, where @name@ is a
-- valid and unique header name. The mapped non-static value must match the
-- pattern of @integration.response.header.{name}@ or
-- @integration.response.body.{JSON-expression}@, where @name@ must be a
-- valid and unique response header name and @JSON-expression@ a valid JSON
-- expression without the @$@ prefix.
putIntegrationResponse_responseParameters :: Lens.Lens' PutIntegrationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putIntegrationResponse_responseParameters = Lens.lens (\PutIntegrationResponse' {responseParameters} -> responseParameters) (\s@PutIntegrationResponse' {} a -> s {responseParameters = a} :: PutIntegrationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specifies a put integration response\'s templates.
putIntegrationResponse_responseTemplates :: Lens.Lens' PutIntegrationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putIntegrationResponse_responseTemplates = Lens.lens (\PutIntegrationResponse' {responseTemplates} -> responseTemplates) (\s@PutIntegrationResponse' {} a -> s {responseTemplates = a} :: PutIntegrationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the selection pattern of a put integration response.
putIntegrationResponse_selectionPattern :: Lens.Lens' PutIntegrationResponse (Prelude.Maybe Prelude.Text)
putIntegrationResponse_selectionPattern = Lens.lens (\PutIntegrationResponse' {selectionPattern} -> selectionPattern) (\s@PutIntegrationResponse' {} a -> s {selectionPattern = a} :: PutIntegrationResponse)

-- | The string identifier of the associated RestApi.
putIntegrationResponse_restApiId :: Lens.Lens' PutIntegrationResponse Prelude.Text
putIntegrationResponse_restApiId = Lens.lens (\PutIntegrationResponse' {restApiId} -> restApiId) (\s@PutIntegrationResponse' {} a -> s {restApiId = a} :: PutIntegrationResponse)

-- | Specifies a put integration response request\'s resource identifier.
putIntegrationResponse_resourceId :: Lens.Lens' PutIntegrationResponse Prelude.Text
putIntegrationResponse_resourceId = Lens.lens (\PutIntegrationResponse' {resourceId} -> resourceId) (\s@PutIntegrationResponse' {} a -> s {resourceId = a} :: PutIntegrationResponse)

-- | Specifies a put integration response request\'s HTTP method.
putIntegrationResponse_httpMethod :: Lens.Lens' PutIntegrationResponse Prelude.Text
putIntegrationResponse_httpMethod = Lens.lens (\PutIntegrationResponse' {httpMethod} -> httpMethod) (\s@PutIntegrationResponse' {} a -> s {httpMethod = a} :: PutIntegrationResponse)

-- | Specifies the status code that is used to map the integration response
-- to an existing MethodResponse.
putIntegrationResponse_statusCode :: Lens.Lens' PutIntegrationResponse Prelude.Text
putIntegrationResponse_statusCode = Lens.lens (\PutIntegrationResponse' {statusCode} -> statusCode) (\s@PutIntegrationResponse' {} a -> s {statusCode = a} :: PutIntegrationResponse)

instance Core.AWSRequest PutIntegrationResponse where
  type
    AWSResponse PutIntegrationResponse =
      IntegrationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable PutIntegrationResponse where
  hashWithSalt _salt PutIntegrationResponse' {..} =
    _salt `Prelude.hashWithSalt` contentHandling
      `Prelude.hashWithSalt` responseParameters
      `Prelude.hashWithSalt` responseTemplates
      `Prelude.hashWithSalt` selectionPattern
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` httpMethod
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData PutIntegrationResponse where
  rnf PutIntegrationResponse' {..} =
    Prelude.rnf contentHandling
      `Prelude.seq` Prelude.rnf responseParameters
      `Prelude.seq` Prelude.rnf responseTemplates
      `Prelude.seq` Prelude.rnf selectionPattern
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf httpMethod
      `Prelude.seq` Prelude.rnf statusCode

instance Data.ToHeaders PutIntegrationResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON PutIntegrationResponse where
  toJSON PutIntegrationResponse' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("contentHandling" Data..=)
              Prelude.<$> contentHandling,
            ("responseParameters" Data..=)
              Prelude.<$> responseParameters,
            ("responseTemplates" Data..=)
              Prelude.<$> responseTemplates,
            ("selectionPattern" Data..=)
              Prelude.<$> selectionPattern
          ]
      )

instance Data.ToPath PutIntegrationResponse where
  toPath PutIntegrationResponse' {..} =
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

instance Data.ToQuery PutIntegrationResponse where
  toQuery = Prelude.const Prelude.mempty
