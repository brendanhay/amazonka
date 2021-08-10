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
-- Module      : Network.AWS.APIGateway.PutIntegrationResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a put integration.
module Network.AWS.APIGateway.PutIntegrationResponse
  ( -- * Creating a Request
    PutIntegrationResponse (..),
    newPutIntegrationResponse,

    -- * Request Lenses
    putIntegrationResponse_contentHandling,
    putIntegrationResponse_responseTemplates,
    putIntegrationResponse_responseParameters,
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
    integrationResponse_responseTemplates,
    integrationResponse_statusCode,
    integrationResponse_responseParameters,
    integrationResponse_selectionPattern,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a put integration response request.
--
-- /See:/ 'newPutIntegrationResponse' smart constructor.
data PutIntegrationResponse = PutIntegrationResponse'
  { -- | Specifies how to handle response payload content type conversions.
    -- Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@, with the
    -- following behaviors:
    --
    -- -   @CONVERT_TO_BINARY@: Converts a response payload from a
    --     Base64-encoded string to the corresponding binary blob.
    --
    -- -   @CONVERT_TO_TEXT@: Converts a response payload from a binary blob to
    --     a Base64-encoded string.
    --
    -- If this property is not defined, the response payload will be passed
    -- through from the integration response to the method response without
    -- modification.
    contentHandling :: Prelude.Maybe ContentHandlingStrategy,
    -- | Specifies a put integration response\'s templates.
    responseTemplates :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
    -- | Specifies the selection pattern of a put integration response.
    selectionPattern :: Prelude.Maybe Prelude.Text,
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] Specifies a put integration response request\'s resource
    -- identifier.
    resourceId :: Prelude.Text,
    -- | [Required] Specifies a put integration response request\'s HTTP method.
    httpMethod :: Prelude.Text,
    -- | [Required] Specifies the status code that is used to map the integration
    -- response to an existing MethodResponse.
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
-- -   @CONVERT_TO_BINARY@: Converts a response payload from a
--     Base64-encoded string to the corresponding binary blob.
--
-- -   @CONVERT_TO_TEXT@: Converts a response payload from a binary blob to
--     a Base64-encoded string.
--
-- If this property is not defined, the response payload will be passed
-- through from the integration response to the method response without
-- modification.
--
-- 'responseTemplates', 'putIntegrationResponse_responseTemplates' - Specifies a put integration response\'s templates.
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
-- 'selectionPattern', 'putIntegrationResponse_selectionPattern' - Specifies the selection pattern of a put integration response.
--
-- 'restApiId', 'putIntegrationResponse_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'resourceId', 'putIntegrationResponse_resourceId' - [Required] Specifies a put integration response request\'s resource
-- identifier.
--
-- 'httpMethod', 'putIntegrationResponse_httpMethod' - [Required] Specifies a put integration response request\'s HTTP method.
--
-- 'statusCode', 'putIntegrationResponse_statusCode' - [Required] Specifies the status code that is used to map the integration
-- response to an existing MethodResponse.
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
        responseTemplates = Prelude.Nothing,
        responseParameters = Prelude.Nothing,
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
-- -   @CONVERT_TO_BINARY@: Converts a response payload from a
--     Base64-encoded string to the corresponding binary blob.
--
-- -   @CONVERT_TO_TEXT@: Converts a response payload from a binary blob to
--     a Base64-encoded string.
--
-- If this property is not defined, the response payload will be passed
-- through from the integration response to the method response without
-- modification.
putIntegrationResponse_contentHandling :: Lens.Lens' PutIntegrationResponse (Prelude.Maybe ContentHandlingStrategy)
putIntegrationResponse_contentHandling = Lens.lens (\PutIntegrationResponse' {contentHandling} -> contentHandling) (\s@PutIntegrationResponse' {} a -> s {contentHandling = a} :: PutIntegrationResponse)

-- | Specifies a put integration response\'s templates.
putIntegrationResponse_responseTemplates :: Lens.Lens' PutIntegrationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putIntegrationResponse_responseTemplates = Lens.lens (\PutIntegrationResponse' {responseTemplates} -> responseTemplates) (\s@PutIntegrationResponse' {} a -> s {responseTemplates = a} :: PutIntegrationResponse) Prelude.. Lens.mapping Lens._Coerce

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
putIntegrationResponse_responseParameters = Lens.lens (\PutIntegrationResponse' {responseParameters} -> responseParameters) (\s@PutIntegrationResponse' {} a -> s {responseParameters = a} :: PutIntegrationResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies the selection pattern of a put integration response.
putIntegrationResponse_selectionPattern :: Lens.Lens' PutIntegrationResponse (Prelude.Maybe Prelude.Text)
putIntegrationResponse_selectionPattern = Lens.lens (\PutIntegrationResponse' {selectionPattern} -> selectionPattern) (\s@PutIntegrationResponse' {} a -> s {selectionPattern = a} :: PutIntegrationResponse)

-- | [Required] The string identifier of the associated RestApi.
putIntegrationResponse_restApiId :: Lens.Lens' PutIntegrationResponse Prelude.Text
putIntegrationResponse_restApiId = Lens.lens (\PutIntegrationResponse' {restApiId} -> restApiId) (\s@PutIntegrationResponse' {} a -> s {restApiId = a} :: PutIntegrationResponse)

-- | [Required] Specifies a put integration response request\'s resource
-- identifier.
putIntegrationResponse_resourceId :: Lens.Lens' PutIntegrationResponse Prelude.Text
putIntegrationResponse_resourceId = Lens.lens (\PutIntegrationResponse' {resourceId} -> resourceId) (\s@PutIntegrationResponse' {} a -> s {resourceId = a} :: PutIntegrationResponse)

-- | [Required] Specifies a put integration response request\'s HTTP method.
putIntegrationResponse_httpMethod :: Lens.Lens' PutIntegrationResponse Prelude.Text
putIntegrationResponse_httpMethod = Lens.lens (\PutIntegrationResponse' {httpMethod} -> httpMethod) (\s@PutIntegrationResponse' {} a -> s {httpMethod = a} :: PutIntegrationResponse)

-- | [Required] Specifies the status code that is used to map the integration
-- response to an existing MethodResponse.
putIntegrationResponse_statusCode :: Lens.Lens' PutIntegrationResponse Prelude.Text
putIntegrationResponse_statusCode = Lens.lens (\PutIntegrationResponse' {statusCode} -> statusCode) (\s@PutIntegrationResponse' {} a -> s {statusCode = a} :: PutIntegrationResponse)

instance Core.AWSRequest PutIntegrationResponse where
  type
    AWSResponse PutIntegrationResponse =
      IntegrationResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable PutIntegrationResponse

instance Prelude.NFData PutIntegrationResponse

instance Core.ToHeaders PutIntegrationResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON PutIntegrationResponse where
  toJSON PutIntegrationResponse' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("contentHandling" Core..=)
              Prelude.<$> contentHandling,
            ("responseTemplates" Core..=)
              Prelude.<$> responseTemplates,
            ("responseParameters" Core..=)
              Prelude.<$> responseParameters,
            ("selectionPattern" Core..=)
              Prelude.<$> selectionPattern
          ]
      )

instance Core.ToPath PutIntegrationResponse where
  toPath PutIntegrationResponse' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/resources/",
        Core.toBS resourceId,
        "/methods/",
        Core.toBS httpMethod,
        "/integration/responses/",
        Core.toBS statusCode
      ]

instance Core.ToQuery PutIntegrationResponse where
  toQuery = Prelude.const Prelude.mempty
