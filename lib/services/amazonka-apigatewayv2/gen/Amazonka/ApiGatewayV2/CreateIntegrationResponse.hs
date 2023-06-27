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
-- Module      : Amazonka.ApiGatewayV2.CreateIntegrationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an IntegrationResponses.
module Amazonka.ApiGatewayV2.CreateIntegrationResponse
  ( -- * Creating a Request
    CreateIntegrationResponse (..),
    newCreateIntegrationResponse,

    -- * Request Lenses
    createIntegrationResponse_contentHandlingStrategy,
    createIntegrationResponse_responseParameters,
    createIntegrationResponse_responseTemplates,
    createIntegrationResponse_templateSelectionExpression,
    createIntegrationResponse_apiId,
    createIntegrationResponse_integrationId,
    createIntegrationResponse_integrationResponseKey,

    -- * Destructuring the Response
    CreateIntegrationResponseResponse (..),
    newCreateIntegrationResponseResponse,

    -- * Response Lenses
    createIntegrationResponseResponse_contentHandlingStrategy,
    createIntegrationResponseResponse_integrationResponseId,
    createIntegrationResponseResponse_integrationResponseKey,
    createIntegrationResponseResponse_responseParameters,
    createIntegrationResponseResponse_responseTemplates,
    createIntegrationResponseResponse_templateSelectionExpression,
    createIntegrationResponseResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates a new IntegrationResponse resource to represent an integration
-- response.
--
-- /See:/ 'newCreateIntegrationResponse' smart constructor.
data CreateIntegrationResponse = CreateIntegrationResponse'
  { -- | Specifies how to handle response payload content type conversions.
    -- Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the
    -- following behaviors:
    --
    -- CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded
    -- string to the corresponding binary blob.
    --
    -- CONVERT_TO_TEXT: Converts a response payload from a binary blob to a
    -- Base64-encoded string.
    --
    -- If this property is not defined, the response payload will be passed
    -- through from the integration response to the route response or method
    -- response without modification.
    contentHandlingStrategy :: Prelude.Maybe ContentHandlingStrategy,
    -- | A key-value map specifying response parameters that are passed to the
    -- method response from the backend. The key is a method response header
    -- parameter name and the mapped value is an integration response header
    -- value, a static value enclosed within a pair of single quotes, or a JSON
    -- expression from the integration response body. The mapping key must
    -- match the pattern of method.response.header.{name}, where {name} is a
    -- valid and unique header name. The mapped non-static value must match the
    -- pattern of integration.response.header.{name} or
    -- integration.response.body.{JSON-expression}, where {name} is a valid and
    -- unique response header name and {JSON-expression} is a valid JSON
    -- expression without the $ prefix.
    responseParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The collection of response templates for the integration response as a
    -- string-to-string map of key-value pairs. Response templates are
    -- represented as a key\/value map, with a content-type as the key and a
    -- template as the value.
    responseTemplates :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The template selection expression for the integration response.
    -- Supported only for WebSocket APIs.
    templateSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text,
    -- | The integration ID.
    integrationId :: Prelude.Text,
    -- | The integration response key.
    integrationResponseKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIntegrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentHandlingStrategy', 'createIntegrationResponse_contentHandlingStrategy' - Specifies how to handle response payload content type conversions.
-- Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the
-- following behaviors:
--
-- CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded
-- string to the corresponding binary blob.
--
-- CONVERT_TO_TEXT: Converts a response payload from a binary blob to a
-- Base64-encoded string.
--
-- If this property is not defined, the response payload will be passed
-- through from the integration response to the route response or method
-- response without modification.
--
-- 'responseParameters', 'createIntegrationResponse_responseParameters' - A key-value map specifying response parameters that are passed to the
-- method response from the backend. The key is a method response header
-- parameter name and the mapped value is an integration response header
-- value, a static value enclosed within a pair of single quotes, or a JSON
-- expression from the integration response body. The mapping key must
-- match the pattern of method.response.header.{name}, where {name} is a
-- valid and unique header name. The mapped non-static value must match the
-- pattern of integration.response.header.{name} or
-- integration.response.body.{JSON-expression}, where {name} is a valid and
-- unique response header name and {JSON-expression} is a valid JSON
-- expression without the $ prefix.
--
-- 'responseTemplates', 'createIntegrationResponse_responseTemplates' - The collection of response templates for the integration response as a
-- string-to-string map of key-value pairs. Response templates are
-- represented as a key\/value map, with a content-type as the key and a
-- template as the value.
--
-- 'templateSelectionExpression', 'createIntegrationResponse_templateSelectionExpression' - The template selection expression for the integration response.
-- Supported only for WebSocket APIs.
--
-- 'apiId', 'createIntegrationResponse_apiId' - The API identifier.
--
-- 'integrationId', 'createIntegrationResponse_integrationId' - The integration ID.
--
-- 'integrationResponseKey', 'createIntegrationResponse_integrationResponseKey' - The integration response key.
newCreateIntegrationResponse ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'integrationId'
  Prelude.Text ->
  -- | 'integrationResponseKey'
  Prelude.Text ->
  CreateIntegrationResponse
newCreateIntegrationResponse
  pApiId_
  pIntegrationId_
  pIntegrationResponseKey_ =
    CreateIntegrationResponse'
      { contentHandlingStrategy =
          Prelude.Nothing,
        responseParameters = Prelude.Nothing,
        responseTemplates = Prelude.Nothing,
        templateSelectionExpression = Prelude.Nothing,
        apiId = pApiId_,
        integrationId = pIntegrationId_,
        integrationResponseKey =
          pIntegrationResponseKey_
      }

-- | Specifies how to handle response payload content type conversions.
-- Supported values are CONVERT_TO_BINARY and CONVERT_TO_TEXT, with the
-- following behaviors:
--
-- CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded
-- string to the corresponding binary blob.
--
-- CONVERT_TO_TEXT: Converts a response payload from a binary blob to a
-- Base64-encoded string.
--
-- If this property is not defined, the response payload will be passed
-- through from the integration response to the route response or method
-- response without modification.
createIntegrationResponse_contentHandlingStrategy :: Lens.Lens' CreateIntegrationResponse (Prelude.Maybe ContentHandlingStrategy)
createIntegrationResponse_contentHandlingStrategy = Lens.lens (\CreateIntegrationResponse' {contentHandlingStrategy} -> contentHandlingStrategy) (\s@CreateIntegrationResponse' {} a -> s {contentHandlingStrategy = a} :: CreateIntegrationResponse)

-- | A key-value map specifying response parameters that are passed to the
-- method response from the backend. The key is a method response header
-- parameter name and the mapped value is an integration response header
-- value, a static value enclosed within a pair of single quotes, or a JSON
-- expression from the integration response body. The mapping key must
-- match the pattern of method.response.header.{name}, where {name} is a
-- valid and unique header name. The mapped non-static value must match the
-- pattern of integration.response.header.{name} or
-- integration.response.body.{JSON-expression}, where {name} is a valid and
-- unique response header name and {JSON-expression} is a valid JSON
-- expression without the $ prefix.
createIntegrationResponse_responseParameters :: Lens.Lens' CreateIntegrationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createIntegrationResponse_responseParameters = Lens.lens (\CreateIntegrationResponse' {responseParameters} -> responseParameters) (\s@CreateIntegrationResponse' {} a -> s {responseParameters = a} :: CreateIntegrationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The collection of response templates for the integration response as a
-- string-to-string map of key-value pairs. Response templates are
-- represented as a key\/value map, with a content-type as the key and a
-- template as the value.
createIntegrationResponse_responseTemplates :: Lens.Lens' CreateIntegrationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createIntegrationResponse_responseTemplates = Lens.lens (\CreateIntegrationResponse' {responseTemplates} -> responseTemplates) (\s@CreateIntegrationResponse' {} a -> s {responseTemplates = a} :: CreateIntegrationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The template selection expression for the integration response.
-- Supported only for WebSocket APIs.
createIntegrationResponse_templateSelectionExpression :: Lens.Lens' CreateIntegrationResponse (Prelude.Maybe Prelude.Text)
createIntegrationResponse_templateSelectionExpression = Lens.lens (\CreateIntegrationResponse' {templateSelectionExpression} -> templateSelectionExpression) (\s@CreateIntegrationResponse' {} a -> s {templateSelectionExpression = a} :: CreateIntegrationResponse)

-- | The API identifier.
createIntegrationResponse_apiId :: Lens.Lens' CreateIntegrationResponse Prelude.Text
createIntegrationResponse_apiId = Lens.lens (\CreateIntegrationResponse' {apiId} -> apiId) (\s@CreateIntegrationResponse' {} a -> s {apiId = a} :: CreateIntegrationResponse)

-- | The integration ID.
createIntegrationResponse_integrationId :: Lens.Lens' CreateIntegrationResponse Prelude.Text
createIntegrationResponse_integrationId = Lens.lens (\CreateIntegrationResponse' {integrationId} -> integrationId) (\s@CreateIntegrationResponse' {} a -> s {integrationId = a} :: CreateIntegrationResponse)

-- | The integration response key.
createIntegrationResponse_integrationResponseKey :: Lens.Lens' CreateIntegrationResponse Prelude.Text
createIntegrationResponse_integrationResponseKey = Lens.lens (\CreateIntegrationResponse' {integrationResponseKey} -> integrationResponseKey) (\s@CreateIntegrationResponse' {} a -> s {integrationResponseKey = a} :: CreateIntegrationResponse)

instance Core.AWSRequest CreateIntegrationResponse where
  type
    AWSResponse CreateIntegrationResponse =
      CreateIntegrationResponseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIntegrationResponseResponse'
            Prelude.<$> (x Data..?> "contentHandlingStrategy")
            Prelude.<*> (x Data..?> "integrationResponseId")
            Prelude.<*> (x Data..?> "integrationResponseKey")
            Prelude.<*> ( x
                            Data..?> "responseParameters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "responseTemplates"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "templateSelectionExpression")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateIntegrationResponse where
  hashWithSalt _salt CreateIntegrationResponse' {..} =
    _salt
      `Prelude.hashWithSalt` contentHandlingStrategy
      `Prelude.hashWithSalt` responseParameters
      `Prelude.hashWithSalt` responseTemplates
      `Prelude.hashWithSalt` templateSelectionExpression
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` integrationId
      `Prelude.hashWithSalt` integrationResponseKey

instance Prelude.NFData CreateIntegrationResponse where
  rnf CreateIntegrationResponse' {..} =
    Prelude.rnf contentHandlingStrategy
      `Prelude.seq` Prelude.rnf responseParameters
      `Prelude.seq` Prelude.rnf responseTemplates
      `Prelude.seq` Prelude.rnf templateSelectionExpression
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf integrationId
      `Prelude.seq` Prelude.rnf integrationResponseKey

instance Data.ToHeaders CreateIntegrationResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateIntegrationResponse where
  toJSON CreateIntegrationResponse' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("contentHandlingStrategy" Data..=)
              Prelude.<$> contentHandlingStrategy,
            ("responseParameters" Data..=)
              Prelude.<$> responseParameters,
            ("responseTemplates" Data..=)
              Prelude.<$> responseTemplates,
            ("templateSelectionExpression" Data..=)
              Prelude.<$> templateSelectionExpression,
            Prelude.Just
              ( "integrationResponseKey"
                  Data..= integrationResponseKey
              )
          ]
      )

instance Data.ToPath CreateIntegrationResponse where
  toPath CreateIntegrationResponse' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Data.toBS apiId,
        "/integrations/",
        Data.toBS integrationId,
        "/integrationresponses"
      ]

instance Data.ToQuery CreateIntegrationResponse where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateIntegrationResponseResponse' smart constructor.
data CreateIntegrationResponseResponse = CreateIntegrationResponseResponse'
  { -- | Supported only for WebSocket APIs. Specifies how to handle response
    -- payload content type conversions. Supported values are CONVERT_TO_BINARY
    -- and CONVERT_TO_TEXT, with the following behaviors:
    --
    -- CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded
    -- string to the corresponding binary blob.
    --
    -- CONVERT_TO_TEXT: Converts a response payload from a binary blob to a
    -- Base64-encoded string.
    --
    -- If this property is not defined, the response payload will be passed
    -- through from the integration response to the route response or method
    -- response without modification.
    contentHandlingStrategy :: Prelude.Maybe ContentHandlingStrategy,
    -- | The integration response ID.
    integrationResponseId :: Prelude.Maybe Prelude.Text,
    -- | The integration response key.
    integrationResponseKey :: Prelude.Maybe Prelude.Text,
    -- | A key-value map specifying response parameters that are passed to the
    -- method response from the backend. The key is a method response header
    -- parameter name and the mapped value is an integration response header
    -- value, a static value enclosed within a pair of single quotes, or a JSON
    -- expression from the integration response body. The mapping key must
    -- match the pattern of method.response.header.{name}, where name is a
    -- valid and unique header name. The mapped non-static value must match the
    -- pattern of integration.response.header.{name} or
    -- integration.response.body.{JSON-expression}, where name is a valid and
    -- unique response header name and JSON-expression is a valid JSON
    -- expression without the $ prefix.
    responseParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The collection of response templates for the integration response as a
    -- string-to-string map of key-value pairs. Response templates are
    -- represented as a key\/value map, with a content-type as the key and a
    -- template as the value.
    responseTemplates :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The template selection expressions for the integration response.
    templateSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIntegrationResponseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentHandlingStrategy', 'createIntegrationResponseResponse_contentHandlingStrategy' - Supported only for WebSocket APIs. Specifies how to handle response
-- payload content type conversions. Supported values are CONVERT_TO_BINARY
-- and CONVERT_TO_TEXT, with the following behaviors:
--
-- CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded
-- string to the corresponding binary blob.
--
-- CONVERT_TO_TEXT: Converts a response payload from a binary blob to a
-- Base64-encoded string.
--
-- If this property is not defined, the response payload will be passed
-- through from the integration response to the route response or method
-- response without modification.
--
-- 'integrationResponseId', 'createIntegrationResponseResponse_integrationResponseId' - The integration response ID.
--
-- 'integrationResponseKey', 'createIntegrationResponseResponse_integrationResponseKey' - The integration response key.
--
-- 'responseParameters', 'createIntegrationResponseResponse_responseParameters' - A key-value map specifying response parameters that are passed to the
-- method response from the backend. The key is a method response header
-- parameter name and the mapped value is an integration response header
-- value, a static value enclosed within a pair of single quotes, or a JSON
-- expression from the integration response body. The mapping key must
-- match the pattern of method.response.header.{name}, where name is a
-- valid and unique header name. The mapped non-static value must match the
-- pattern of integration.response.header.{name} or
-- integration.response.body.{JSON-expression}, where name is a valid and
-- unique response header name and JSON-expression is a valid JSON
-- expression without the $ prefix.
--
-- 'responseTemplates', 'createIntegrationResponseResponse_responseTemplates' - The collection of response templates for the integration response as a
-- string-to-string map of key-value pairs. Response templates are
-- represented as a key\/value map, with a content-type as the key and a
-- template as the value.
--
-- 'templateSelectionExpression', 'createIntegrationResponseResponse_templateSelectionExpression' - The template selection expressions for the integration response.
--
-- 'httpStatus', 'createIntegrationResponseResponse_httpStatus' - The response's http status code.
newCreateIntegrationResponseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateIntegrationResponseResponse
newCreateIntegrationResponseResponse pHttpStatus_ =
  CreateIntegrationResponseResponse'
    { contentHandlingStrategy =
        Prelude.Nothing,
      integrationResponseId = Prelude.Nothing,
      integrationResponseKey = Prelude.Nothing,
      responseParameters = Prelude.Nothing,
      responseTemplates = Prelude.Nothing,
      templateSelectionExpression =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Supported only for WebSocket APIs. Specifies how to handle response
-- payload content type conversions. Supported values are CONVERT_TO_BINARY
-- and CONVERT_TO_TEXT, with the following behaviors:
--
-- CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded
-- string to the corresponding binary blob.
--
-- CONVERT_TO_TEXT: Converts a response payload from a binary blob to a
-- Base64-encoded string.
--
-- If this property is not defined, the response payload will be passed
-- through from the integration response to the route response or method
-- response without modification.
createIntegrationResponseResponse_contentHandlingStrategy :: Lens.Lens' CreateIntegrationResponseResponse (Prelude.Maybe ContentHandlingStrategy)
createIntegrationResponseResponse_contentHandlingStrategy = Lens.lens (\CreateIntegrationResponseResponse' {contentHandlingStrategy} -> contentHandlingStrategy) (\s@CreateIntegrationResponseResponse' {} a -> s {contentHandlingStrategy = a} :: CreateIntegrationResponseResponse)

-- | The integration response ID.
createIntegrationResponseResponse_integrationResponseId :: Lens.Lens' CreateIntegrationResponseResponse (Prelude.Maybe Prelude.Text)
createIntegrationResponseResponse_integrationResponseId = Lens.lens (\CreateIntegrationResponseResponse' {integrationResponseId} -> integrationResponseId) (\s@CreateIntegrationResponseResponse' {} a -> s {integrationResponseId = a} :: CreateIntegrationResponseResponse)

-- | The integration response key.
createIntegrationResponseResponse_integrationResponseKey :: Lens.Lens' CreateIntegrationResponseResponse (Prelude.Maybe Prelude.Text)
createIntegrationResponseResponse_integrationResponseKey = Lens.lens (\CreateIntegrationResponseResponse' {integrationResponseKey} -> integrationResponseKey) (\s@CreateIntegrationResponseResponse' {} a -> s {integrationResponseKey = a} :: CreateIntegrationResponseResponse)

-- | A key-value map specifying response parameters that are passed to the
-- method response from the backend. The key is a method response header
-- parameter name and the mapped value is an integration response header
-- value, a static value enclosed within a pair of single quotes, or a JSON
-- expression from the integration response body. The mapping key must
-- match the pattern of method.response.header.{name}, where name is a
-- valid and unique header name. The mapped non-static value must match the
-- pattern of integration.response.header.{name} or
-- integration.response.body.{JSON-expression}, where name is a valid and
-- unique response header name and JSON-expression is a valid JSON
-- expression without the $ prefix.
createIntegrationResponseResponse_responseParameters :: Lens.Lens' CreateIntegrationResponseResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createIntegrationResponseResponse_responseParameters = Lens.lens (\CreateIntegrationResponseResponse' {responseParameters} -> responseParameters) (\s@CreateIntegrationResponseResponse' {} a -> s {responseParameters = a} :: CreateIntegrationResponseResponse) Prelude.. Lens.mapping Lens.coerced

-- | The collection of response templates for the integration response as a
-- string-to-string map of key-value pairs. Response templates are
-- represented as a key\/value map, with a content-type as the key and a
-- template as the value.
createIntegrationResponseResponse_responseTemplates :: Lens.Lens' CreateIntegrationResponseResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createIntegrationResponseResponse_responseTemplates = Lens.lens (\CreateIntegrationResponseResponse' {responseTemplates} -> responseTemplates) (\s@CreateIntegrationResponseResponse' {} a -> s {responseTemplates = a} :: CreateIntegrationResponseResponse) Prelude.. Lens.mapping Lens.coerced

-- | The template selection expressions for the integration response.
createIntegrationResponseResponse_templateSelectionExpression :: Lens.Lens' CreateIntegrationResponseResponse (Prelude.Maybe Prelude.Text)
createIntegrationResponseResponse_templateSelectionExpression = Lens.lens (\CreateIntegrationResponseResponse' {templateSelectionExpression} -> templateSelectionExpression) (\s@CreateIntegrationResponseResponse' {} a -> s {templateSelectionExpression = a} :: CreateIntegrationResponseResponse)

-- | The response's http status code.
createIntegrationResponseResponse_httpStatus :: Lens.Lens' CreateIntegrationResponseResponse Prelude.Int
createIntegrationResponseResponse_httpStatus = Lens.lens (\CreateIntegrationResponseResponse' {httpStatus} -> httpStatus) (\s@CreateIntegrationResponseResponse' {} a -> s {httpStatus = a} :: CreateIntegrationResponseResponse)

instance
  Prelude.NFData
    CreateIntegrationResponseResponse
  where
  rnf CreateIntegrationResponseResponse' {..} =
    Prelude.rnf contentHandlingStrategy
      `Prelude.seq` Prelude.rnf integrationResponseId
      `Prelude.seq` Prelude.rnf integrationResponseKey
      `Prelude.seq` Prelude.rnf responseParameters
      `Prelude.seq` Prelude.rnf responseTemplates
      `Prelude.seq` Prelude.rnf templateSelectionExpression
      `Prelude.seq` Prelude.rnf httpStatus
