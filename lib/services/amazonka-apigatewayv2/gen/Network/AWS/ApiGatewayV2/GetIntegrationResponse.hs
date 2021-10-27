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
-- Module      : Network.AWS.ApiGatewayV2.GetIntegrationResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an IntegrationResponses.
module Network.AWS.ApiGatewayV2.GetIntegrationResponse
  ( -- * Creating a Request
    GetIntegrationResponse (..),
    newGetIntegrationResponse,

    -- * Request Lenses
    getIntegrationResponse_apiId,
    getIntegrationResponse_integrationResponseId,
    getIntegrationResponse_integrationId,

    -- * Destructuring the Response
    GetIntegrationResponseResponse (..),
    newGetIntegrationResponseResponse,

    -- * Response Lenses
    getIntegrationResponseResponse_integrationResponseId,
    getIntegrationResponseResponse_integrationResponseKey,
    getIntegrationResponseResponse_templateSelectionExpression,
    getIntegrationResponseResponse_contentHandlingStrategy,
    getIntegrationResponseResponse_responseTemplates,
    getIntegrationResponseResponse_responseParameters,
    getIntegrationResponseResponse_httpStatus,
  )
where

import Network.AWS.ApiGatewayV2.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetIntegrationResponse' smart constructor.
data GetIntegrationResponse = GetIntegrationResponse'
  { -- | The API identifier.
    apiId :: Prelude.Text,
    -- | The integration response ID.
    integrationResponseId :: Prelude.Text,
    -- | The integration ID.
    integrationId :: Prelude.Text
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
-- 'apiId', 'getIntegrationResponse_apiId' - The API identifier.
--
-- 'integrationResponseId', 'getIntegrationResponse_integrationResponseId' - The integration response ID.
--
-- 'integrationId', 'getIntegrationResponse_integrationId' - The integration ID.
newGetIntegrationResponse ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'integrationResponseId'
  Prelude.Text ->
  -- | 'integrationId'
  Prelude.Text ->
  GetIntegrationResponse
newGetIntegrationResponse
  pApiId_
  pIntegrationResponseId_
  pIntegrationId_ =
    GetIntegrationResponse'
      { apiId = pApiId_,
        integrationResponseId = pIntegrationResponseId_,
        integrationId = pIntegrationId_
      }

-- | The API identifier.
getIntegrationResponse_apiId :: Lens.Lens' GetIntegrationResponse Prelude.Text
getIntegrationResponse_apiId = Lens.lens (\GetIntegrationResponse' {apiId} -> apiId) (\s@GetIntegrationResponse' {} a -> s {apiId = a} :: GetIntegrationResponse)

-- | The integration response ID.
getIntegrationResponse_integrationResponseId :: Lens.Lens' GetIntegrationResponse Prelude.Text
getIntegrationResponse_integrationResponseId = Lens.lens (\GetIntegrationResponse' {integrationResponseId} -> integrationResponseId) (\s@GetIntegrationResponse' {} a -> s {integrationResponseId = a} :: GetIntegrationResponse)

-- | The integration ID.
getIntegrationResponse_integrationId :: Lens.Lens' GetIntegrationResponse Prelude.Text
getIntegrationResponse_integrationId = Lens.lens (\GetIntegrationResponse' {integrationId} -> integrationId) (\s@GetIntegrationResponse' {} a -> s {integrationId = a} :: GetIntegrationResponse)

instance Core.AWSRequest GetIntegrationResponse where
  type
    AWSResponse GetIntegrationResponse =
      GetIntegrationResponseResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIntegrationResponseResponse'
            Prelude.<$> (x Core..?> "integrationResponseId")
            Prelude.<*> (x Core..?> "integrationResponseKey")
            Prelude.<*> (x Core..?> "templateSelectionExpression")
            Prelude.<*> (x Core..?> "contentHandlingStrategy")
            Prelude.<*> ( x Core..?> "responseTemplates"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Core..?> "responseParameters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIntegrationResponse

instance Prelude.NFData GetIntegrationResponse

instance Core.ToHeaders GetIntegrationResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetIntegrationResponse where
  toPath GetIntegrationResponse' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Core.toBS apiId,
        "/integrations/",
        Core.toBS integrationId,
        "/integrationresponses/",
        Core.toBS integrationResponseId
      ]

instance Core.ToQuery GetIntegrationResponse where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetIntegrationResponseResponse' smart constructor.
data GetIntegrationResponseResponse = GetIntegrationResponseResponse'
  { -- | The integration response ID.
    integrationResponseId :: Prelude.Maybe Prelude.Text,
    -- | The integration response key.
    integrationResponseKey :: Prelude.Maybe Prelude.Text,
    -- | The template selection expressions for the integration response.
    templateSelectionExpression :: Prelude.Maybe Prelude.Text,
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
    contentHandlingStrategy :: Prelude.Maybe ContentHandlingStrategy,
    -- | The collection of response templates for the integration response as a
    -- string-to-string map of key-value pairs. Response templates are
    -- represented as a key\/value map, with a content-type as the key and a
    -- template as the value.
    responseTemplates :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIntegrationResponseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'integrationResponseId', 'getIntegrationResponseResponse_integrationResponseId' - The integration response ID.
--
-- 'integrationResponseKey', 'getIntegrationResponseResponse_integrationResponseKey' - The integration response key.
--
-- 'templateSelectionExpression', 'getIntegrationResponseResponse_templateSelectionExpression' - The template selection expressions for the integration response.
--
-- 'contentHandlingStrategy', 'getIntegrationResponseResponse_contentHandlingStrategy' - Supported only for WebSocket APIs. Specifies how to handle response
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
-- 'responseTemplates', 'getIntegrationResponseResponse_responseTemplates' - The collection of response templates for the integration response as a
-- string-to-string map of key-value pairs. Response templates are
-- represented as a key\/value map, with a content-type as the key and a
-- template as the value.
--
-- 'responseParameters', 'getIntegrationResponseResponse_responseParameters' - A key-value map specifying response parameters that are passed to the
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
-- 'httpStatus', 'getIntegrationResponseResponse_httpStatus' - The response's http status code.
newGetIntegrationResponseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIntegrationResponseResponse
newGetIntegrationResponseResponse pHttpStatus_ =
  GetIntegrationResponseResponse'
    { integrationResponseId =
        Prelude.Nothing,
      integrationResponseKey = Prelude.Nothing,
      templateSelectionExpression =
        Prelude.Nothing,
      contentHandlingStrategy = Prelude.Nothing,
      responseTemplates = Prelude.Nothing,
      responseParameters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The integration response ID.
getIntegrationResponseResponse_integrationResponseId :: Lens.Lens' GetIntegrationResponseResponse (Prelude.Maybe Prelude.Text)
getIntegrationResponseResponse_integrationResponseId = Lens.lens (\GetIntegrationResponseResponse' {integrationResponseId} -> integrationResponseId) (\s@GetIntegrationResponseResponse' {} a -> s {integrationResponseId = a} :: GetIntegrationResponseResponse)

-- | The integration response key.
getIntegrationResponseResponse_integrationResponseKey :: Lens.Lens' GetIntegrationResponseResponse (Prelude.Maybe Prelude.Text)
getIntegrationResponseResponse_integrationResponseKey = Lens.lens (\GetIntegrationResponseResponse' {integrationResponseKey} -> integrationResponseKey) (\s@GetIntegrationResponseResponse' {} a -> s {integrationResponseKey = a} :: GetIntegrationResponseResponse)

-- | The template selection expressions for the integration response.
getIntegrationResponseResponse_templateSelectionExpression :: Lens.Lens' GetIntegrationResponseResponse (Prelude.Maybe Prelude.Text)
getIntegrationResponseResponse_templateSelectionExpression = Lens.lens (\GetIntegrationResponseResponse' {templateSelectionExpression} -> templateSelectionExpression) (\s@GetIntegrationResponseResponse' {} a -> s {templateSelectionExpression = a} :: GetIntegrationResponseResponse)

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
getIntegrationResponseResponse_contentHandlingStrategy :: Lens.Lens' GetIntegrationResponseResponse (Prelude.Maybe ContentHandlingStrategy)
getIntegrationResponseResponse_contentHandlingStrategy = Lens.lens (\GetIntegrationResponseResponse' {contentHandlingStrategy} -> contentHandlingStrategy) (\s@GetIntegrationResponseResponse' {} a -> s {contentHandlingStrategy = a} :: GetIntegrationResponseResponse)

-- | The collection of response templates for the integration response as a
-- string-to-string map of key-value pairs. Response templates are
-- represented as a key\/value map, with a content-type as the key and a
-- template as the value.
getIntegrationResponseResponse_responseTemplates :: Lens.Lens' GetIntegrationResponseResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getIntegrationResponseResponse_responseTemplates = Lens.lens (\GetIntegrationResponseResponse' {responseTemplates} -> responseTemplates) (\s@GetIntegrationResponseResponse' {} a -> s {responseTemplates = a} :: GetIntegrationResponseResponse) Prelude.. Lens.mapping Lens.coerced

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
getIntegrationResponseResponse_responseParameters :: Lens.Lens' GetIntegrationResponseResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getIntegrationResponseResponse_responseParameters = Lens.lens (\GetIntegrationResponseResponse' {responseParameters} -> responseParameters) (\s@GetIntegrationResponseResponse' {} a -> s {responseParameters = a} :: GetIntegrationResponseResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getIntegrationResponseResponse_httpStatus :: Lens.Lens' GetIntegrationResponseResponse Prelude.Int
getIntegrationResponseResponse_httpStatus = Lens.lens (\GetIntegrationResponseResponse' {httpStatus} -> httpStatus) (\s@GetIntegrationResponseResponse' {} a -> s {httpStatus = a} :: GetIntegrationResponseResponse)

instance
  Prelude.NFData
    GetIntegrationResponseResponse
