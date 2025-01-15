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
-- Module      : Amazonka.ApiGatewayV2.UpdateIntegrationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an IntegrationResponses.
module Amazonka.ApiGatewayV2.UpdateIntegrationResponse
  ( -- * Creating a Request
    UpdateIntegrationResponse (..),
    newUpdateIntegrationResponse,

    -- * Request Lenses
    updateIntegrationResponse_contentHandlingStrategy,
    updateIntegrationResponse_integrationResponseKey,
    updateIntegrationResponse_responseParameters,
    updateIntegrationResponse_responseTemplates,
    updateIntegrationResponse_templateSelectionExpression,
    updateIntegrationResponse_apiId,
    updateIntegrationResponse_integrationResponseId,
    updateIntegrationResponse_integrationId,

    -- * Destructuring the Response
    UpdateIntegrationResponseResponse (..),
    newUpdateIntegrationResponseResponse,

    -- * Response Lenses
    updateIntegrationResponseResponse_contentHandlingStrategy,
    updateIntegrationResponseResponse_integrationResponseId,
    updateIntegrationResponseResponse_integrationResponseKey,
    updateIntegrationResponseResponse_responseParameters,
    updateIntegrationResponseResponse_responseTemplates,
    updateIntegrationResponseResponse_templateSelectionExpression,
    updateIntegrationResponseResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Updates an IntegrationResponses.
--
-- /See:/ 'newUpdateIntegrationResponse' smart constructor.
data UpdateIntegrationResponse = UpdateIntegrationResponse'
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
    -- | The integration response key.
    integrationResponseKey :: Prelude.Maybe Prelude.Text,
    -- | A key-value map specifying response parameters that are passed to the
    -- method response from the backend. The key is a method response header
    -- parameter name and the mapped value is an integration response header
    -- value, a static value enclosed within a pair of single quotes, or a JSON
    -- expression from the integration response body. The mapping key must
    -- match the pattern of method.response.header.{name} , where name is a
    -- valid and unique header name. The mapped non-static value must match the
    -- pattern of integration.response.header.{name} or
    -- integration.response.body.{JSON-expression} , where {name} is a valid
    -- and unique response header name and {JSON-expression} is a valid JSON
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
    -- | The integration response ID.
    integrationResponseId :: Prelude.Text,
    -- | The integration ID.
    integrationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIntegrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentHandlingStrategy', 'updateIntegrationResponse_contentHandlingStrategy' - Supported only for WebSocket APIs. Specifies how to handle response
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
-- 'integrationResponseKey', 'updateIntegrationResponse_integrationResponseKey' - The integration response key.
--
-- 'responseParameters', 'updateIntegrationResponse_responseParameters' - A key-value map specifying response parameters that are passed to the
-- method response from the backend. The key is a method response header
-- parameter name and the mapped value is an integration response header
-- value, a static value enclosed within a pair of single quotes, or a JSON
-- expression from the integration response body. The mapping key must
-- match the pattern of method.response.header.{name} , where name is a
-- valid and unique header name. The mapped non-static value must match the
-- pattern of integration.response.header.{name} or
-- integration.response.body.{JSON-expression} , where {name} is a valid
-- and unique response header name and {JSON-expression} is a valid JSON
-- expression without the $ prefix.
--
-- 'responseTemplates', 'updateIntegrationResponse_responseTemplates' - The collection of response templates for the integration response as a
-- string-to-string map of key-value pairs. Response templates are
-- represented as a key\/value map, with a content-type as the key and a
-- template as the value.
--
-- 'templateSelectionExpression', 'updateIntegrationResponse_templateSelectionExpression' - The template selection expression for the integration response.
-- Supported only for WebSocket APIs.
--
-- 'apiId', 'updateIntegrationResponse_apiId' - The API identifier.
--
-- 'integrationResponseId', 'updateIntegrationResponse_integrationResponseId' - The integration response ID.
--
-- 'integrationId', 'updateIntegrationResponse_integrationId' - The integration ID.
newUpdateIntegrationResponse ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'integrationResponseId'
  Prelude.Text ->
  -- | 'integrationId'
  Prelude.Text ->
  UpdateIntegrationResponse
newUpdateIntegrationResponse
  pApiId_
  pIntegrationResponseId_
  pIntegrationId_ =
    UpdateIntegrationResponse'
      { contentHandlingStrategy =
          Prelude.Nothing,
        integrationResponseKey = Prelude.Nothing,
        responseParameters = Prelude.Nothing,
        responseTemplates = Prelude.Nothing,
        templateSelectionExpression = Prelude.Nothing,
        apiId = pApiId_,
        integrationResponseId = pIntegrationResponseId_,
        integrationId = pIntegrationId_
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
updateIntegrationResponse_contentHandlingStrategy :: Lens.Lens' UpdateIntegrationResponse (Prelude.Maybe ContentHandlingStrategy)
updateIntegrationResponse_contentHandlingStrategy = Lens.lens (\UpdateIntegrationResponse' {contentHandlingStrategy} -> contentHandlingStrategy) (\s@UpdateIntegrationResponse' {} a -> s {contentHandlingStrategy = a} :: UpdateIntegrationResponse)

-- | The integration response key.
updateIntegrationResponse_integrationResponseKey :: Lens.Lens' UpdateIntegrationResponse (Prelude.Maybe Prelude.Text)
updateIntegrationResponse_integrationResponseKey = Lens.lens (\UpdateIntegrationResponse' {integrationResponseKey} -> integrationResponseKey) (\s@UpdateIntegrationResponse' {} a -> s {integrationResponseKey = a} :: UpdateIntegrationResponse)

-- | A key-value map specifying response parameters that are passed to the
-- method response from the backend. The key is a method response header
-- parameter name and the mapped value is an integration response header
-- value, a static value enclosed within a pair of single quotes, or a JSON
-- expression from the integration response body. The mapping key must
-- match the pattern of method.response.header.{name} , where name is a
-- valid and unique header name. The mapped non-static value must match the
-- pattern of integration.response.header.{name} or
-- integration.response.body.{JSON-expression} , where {name} is a valid
-- and unique response header name and {JSON-expression} is a valid JSON
-- expression without the $ prefix.
updateIntegrationResponse_responseParameters :: Lens.Lens' UpdateIntegrationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateIntegrationResponse_responseParameters = Lens.lens (\UpdateIntegrationResponse' {responseParameters} -> responseParameters) (\s@UpdateIntegrationResponse' {} a -> s {responseParameters = a} :: UpdateIntegrationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The collection of response templates for the integration response as a
-- string-to-string map of key-value pairs. Response templates are
-- represented as a key\/value map, with a content-type as the key and a
-- template as the value.
updateIntegrationResponse_responseTemplates :: Lens.Lens' UpdateIntegrationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateIntegrationResponse_responseTemplates = Lens.lens (\UpdateIntegrationResponse' {responseTemplates} -> responseTemplates) (\s@UpdateIntegrationResponse' {} a -> s {responseTemplates = a} :: UpdateIntegrationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The template selection expression for the integration response.
-- Supported only for WebSocket APIs.
updateIntegrationResponse_templateSelectionExpression :: Lens.Lens' UpdateIntegrationResponse (Prelude.Maybe Prelude.Text)
updateIntegrationResponse_templateSelectionExpression = Lens.lens (\UpdateIntegrationResponse' {templateSelectionExpression} -> templateSelectionExpression) (\s@UpdateIntegrationResponse' {} a -> s {templateSelectionExpression = a} :: UpdateIntegrationResponse)

-- | The API identifier.
updateIntegrationResponse_apiId :: Lens.Lens' UpdateIntegrationResponse Prelude.Text
updateIntegrationResponse_apiId = Lens.lens (\UpdateIntegrationResponse' {apiId} -> apiId) (\s@UpdateIntegrationResponse' {} a -> s {apiId = a} :: UpdateIntegrationResponse)

-- | The integration response ID.
updateIntegrationResponse_integrationResponseId :: Lens.Lens' UpdateIntegrationResponse Prelude.Text
updateIntegrationResponse_integrationResponseId = Lens.lens (\UpdateIntegrationResponse' {integrationResponseId} -> integrationResponseId) (\s@UpdateIntegrationResponse' {} a -> s {integrationResponseId = a} :: UpdateIntegrationResponse)

-- | The integration ID.
updateIntegrationResponse_integrationId :: Lens.Lens' UpdateIntegrationResponse Prelude.Text
updateIntegrationResponse_integrationId = Lens.lens (\UpdateIntegrationResponse' {integrationId} -> integrationId) (\s@UpdateIntegrationResponse' {} a -> s {integrationId = a} :: UpdateIntegrationResponse)

instance Core.AWSRequest UpdateIntegrationResponse where
  type
    AWSResponse UpdateIntegrationResponse =
      UpdateIntegrationResponseResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateIntegrationResponseResponse'
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

instance Prelude.Hashable UpdateIntegrationResponse where
  hashWithSalt _salt UpdateIntegrationResponse' {..} =
    _salt
      `Prelude.hashWithSalt` contentHandlingStrategy
      `Prelude.hashWithSalt` integrationResponseKey
      `Prelude.hashWithSalt` responseParameters
      `Prelude.hashWithSalt` responseTemplates
      `Prelude.hashWithSalt` templateSelectionExpression
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` integrationResponseId
      `Prelude.hashWithSalt` integrationId

instance Prelude.NFData UpdateIntegrationResponse where
  rnf UpdateIntegrationResponse' {..} =
    Prelude.rnf contentHandlingStrategy `Prelude.seq`
      Prelude.rnf integrationResponseKey `Prelude.seq`
        Prelude.rnf responseParameters `Prelude.seq`
          Prelude.rnf responseTemplates `Prelude.seq`
            Prelude.rnf templateSelectionExpression `Prelude.seq`
              Prelude.rnf apiId `Prelude.seq`
                Prelude.rnf integrationResponseId `Prelude.seq`
                  Prelude.rnf integrationId

instance Data.ToHeaders UpdateIntegrationResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateIntegrationResponse where
  toJSON UpdateIntegrationResponse' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("contentHandlingStrategy" Data..=)
              Prelude.<$> contentHandlingStrategy,
            ("integrationResponseKey" Data..=)
              Prelude.<$> integrationResponseKey,
            ("responseParameters" Data..=)
              Prelude.<$> responseParameters,
            ("responseTemplates" Data..=)
              Prelude.<$> responseTemplates,
            ("templateSelectionExpression" Data..=)
              Prelude.<$> templateSelectionExpression
          ]
      )

instance Data.ToPath UpdateIntegrationResponse where
  toPath UpdateIntegrationResponse' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Data.toBS apiId,
        "/integrations/",
        Data.toBS integrationId,
        "/integrationresponses/",
        Data.toBS integrationResponseId
      ]

instance Data.ToQuery UpdateIntegrationResponse where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateIntegrationResponseResponse' smart constructor.
data UpdateIntegrationResponseResponse = UpdateIntegrationResponseResponse'
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
-- Create a value of 'UpdateIntegrationResponseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentHandlingStrategy', 'updateIntegrationResponseResponse_contentHandlingStrategy' - Supported only for WebSocket APIs. Specifies how to handle response
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
-- 'integrationResponseId', 'updateIntegrationResponseResponse_integrationResponseId' - The integration response ID.
--
-- 'integrationResponseKey', 'updateIntegrationResponseResponse_integrationResponseKey' - The integration response key.
--
-- 'responseParameters', 'updateIntegrationResponseResponse_responseParameters' - A key-value map specifying response parameters that are passed to the
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
-- 'responseTemplates', 'updateIntegrationResponseResponse_responseTemplates' - The collection of response templates for the integration response as a
-- string-to-string map of key-value pairs. Response templates are
-- represented as a key\/value map, with a content-type as the key and a
-- template as the value.
--
-- 'templateSelectionExpression', 'updateIntegrationResponseResponse_templateSelectionExpression' - The template selection expressions for the integration response.
--
-- 'httpStatus', 'updateIntegrationResponseResponse_httpStatus' - The response's http status code.
newUpdateIntegrationResponseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateIntegrationResponseResponse
newUpdateIntegrationResponseResponse pHttpStatus_ =
  UpdateIntegrationResponseResponse'
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
updateIntegrationResponseResponse_contentHandlingStrategy :: Lens.Lens' UpdateIntegrationResponseResponse (Prelude.Maybe ContentHandlingStrategy)
updateIntegrationResponseResponse_contentHandlingStrategy = Lens.lens (\UpdateIntegrationResponseResponse' {contentHandlingStrategy} -> contentHandlingStrategy) (\s@UpdateIntegrationResponseResponse' {} a -> s {contentHandlingStrategy = a} :: UpdateIntegrationResponseResponse)

-- | The integration response ID.
updateIntegrationResponseResponse_integrationResponseId :: Lens.Lens' UpdateIntegrationResponseResponse (Prelude.Maybe Prelude.Text)
updateIntegrationResponseResponse_integrationResponseId = Lens.lens (\UpdateIntegrationResponseResponse' {integrationResponseId} -> integrationResponseId) (\s@UpdateIntegrationResponseResponse' {} a -> s {integrationResponseId = a} :: UpdateIntegrationResponseResponse)

-- | The integration response key.
updateIntegrationResponseResponse_integrationResponseKey :: Lens.Lens' UpdateIntegrationResponseResponse (Prelude.Maybe Prelude.Text)
updateIntegrationResponseResponse_integrationResponseKey = Lens.lens (\UpdateIntegrationResponseResponse' {integrationResponseKey} -> integrationResponseKey) (\s@UpdateIntegrationResponseResponse' {} a -> s {integrationResponseKey = a} :: UpdateIntegrationResponseResponse)

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
updateIntegrationResponseResponse_responseParameters :: Lens.Lens' UpdateIntegrationResponseResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateIntegrationResponseResponse_responseParameters = Lens.lens (\UpdateIntegrationResponseResponse' {responseParameters} -> responseParameters) (\s@UpdateIntegrationResponseResponse' {} a -> s {responseParameters = a} :: UpdateIntegrationResponseResponse) Prelude.. Lens.mapping Lens.coerced

-- | The collection of response templates for the integration response as a
-- string-to-string map of key-value pairs. Response templates are
-- represented as a key\/value map, with a content-type as the key and a
-- template as the value.
updateIntegrationResponseResponse_responseTemplates :: Lens.Lens' UpdateIntegrationResponseResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateIntegrationResponseResponse_responseTemplates = Lens.lens (\UpdateIntegrationResponseResponse' {responseTemplates} -> responseTemplates) (\s@UpdateIntegrationResponseResponse' {} a -> s {responseTemplates = a} :: UpdateIntegrationResponseResponse) Prelude.. Lens.mapping Lens.coerced

-- | The template selection expressions for the integration response.
updateIntegrationResponseResponse_templateSelectionExpression :: Lens.Lens' UpdateIntegrationResponseResponse (Prelude.Maybe Prelude.Text)
updateIntegrationResponseResponse_templateSelectionExpression = Lens.lens (\UpdateIntegrationResponseResponse' {templateSelectionExpression} -> templateSelectionExpression) (\s@UpdateIntegrationResponseResponse' {} a -> s {templateSelectionExpression = a} :: UpdateIntegrationResponseResponse)

-- | The response's http status code.
updateIntegrationResponseResponse_httpStatus :: Lens.Lens' UpdateIntegrationResponseResponse Prelude.Int
updateIntegrationResponseResponse_httpStatus = Lens.lens (\UpdateIntegrationResponseResponse' {httpStatus} -> httpStatus) (\s@UpdateIntegrationResponseResponse' {} a -> s {httpStatus = a} :: UpdateIntegrationResponseResponse)

instance
  Prelude.NFData
    UpdateIntegrationResponseResponse
  where
  rnf UpdateIntegrationResponseResponse' {..} =
    Prelude.rnf contentHandlingStrategy `Prelude.seq`
      Prelude.rnf integrationResponseId `Prelude.seq`
        Prelude.rnf integrationResponseKey `Prelude.seq`
          Prelude.rnf responseParameters `Prelude.seq`
            Prelude.rnf responseTemplates `Prelude.seq`
              Prelude.rnf templateSelectionExpression `Prelude.seq`
                Prelude.rnf httpStatus
