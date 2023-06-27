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
-- Module      : Amazonka.ApiGatewayV2.GetIntegrationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an IntegrationResponses.
module Amazonka.ApiGatewayV2.GetIntegrationResponse
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
    getIntegrationResponseResponse_contentHandlingStrategy,
    getIntegrationResponseResponse_integrationResponseId,
    getIntegrationResponseResponse_integrationResponseKey,
    getIntegrationResponseResponse_responseParameters,
    getIntegrationResponseResponse_responseTemplates,
    getIntegrationResponseResponse_templateSelectionExpression,
    getIntegrationResponseResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIntegrationResponseResponse'
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

instance Prelude.Hashable GetIntegrationResponse where
  hashWithSalt _salt GetIntegrationResponse' {..} =
    _salt
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` integrationResponseId
      `Prelude.hashWithSalt` integrationId

instance Prelude.NFData GetIntegrationResponse where
  rnf GetIntegrationResponse' {..} =
    Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf integrationResponseId
      `Prelude.seq` Prelude.rnf integrationId

instance Data.ToHeaders GetIntegrationResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetIntegrationResponse where
  toPath GetIntegrationResponse' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Data.toBS apiId,
        "/integrations/",
        Data.toBS integrationId,
        "/integrationresponses/",
        Data.toBS integrationResponseId
      ]

instance Data.ToQuery GetIntegrationResponse where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetIntegrationResponseResponse' smart constructor.
data GetIntegrationResponseResponse = GetIntegrationResponseResponse'
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
-- Create a value of 'GetIntegrationResponseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'integrationResponseId', 'getIntegrationResponseResponse_integrationResponseId' - The integration response ID.
--
-- 'integrationResponseKey', 'getIntegrationResponseResponse_integrationResponseKey' - The integration response key.
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
-- 'responseTemplates', 'getIntegrationResponseResponse_responseTemplates' - The collection of response templates for the integration response as a
-- string-to-string map of key-value pairs. Response templates are
-- represented as a key\/value map, with a content-type as the key and a
-- template as the value.
--
-- 'templateSelectionExpression', 'getIntegrationResponseResponse_templateSelectionExpression' - The template selection expressions for the integration response.
--
-- 'httpStatus', 'getIntegrationResponseResponse_httpStatus' - The response's http status code.
newGetIntegrationResponseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIntegrationResponseResponse
newGetIntegrationResponseResponse pHttpStatus_ =
  GetIntegrationResponseResponse'
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
getIntegrationResponseResponse_contentHandlingStrategy :: Lens.Lens' GetIntegrationResponseResponse (Prelude.Maybe ContentHandlingStrategy)
getIntegrationResponseResponse_contentHandlingStrategy = Lens.lens (\GetIntegrationResponseResponse' {contentHandlingStrategy} -> contentHandlingStrategy) (\s@GetIntegrationResponseResponse' {} a -> s {contentHandlingStrategy = a} :: GetIntegrationResponseResponse)

-- | The integration response ID.
getIntegrationResponseResponse_integrationResponseId :: Lens.Lens' GetIntegrationResponseResponse (Prelude.Maybe Prelude.Text)
getIntegrationResponseResponse_integrationResponseId = Lens.lens (\GetIntegrationResponseResponse' {integrationResponseId} -> integrationResponseId) (\s@GetIntegrationResponseResponse' {} a -> s {integrationResponseId = a} :: GetIntegrationResponseResponse)

-- | The integration response key.
getIntegrationResponseResponse_integrationResponseKey :: Lens.Lens' GetIntegrationResponseResponse (Prelude.Maybe Prelude.Text)
getIntegrationResponseResponse_integrationResponseKey = Lens.lens (\GetIntegrationResponseResponse' {integrationResponseKey} -> integrationResponseKey) (\s@GetIntegrationResponseResponse' {} a -> s {integrationResponseKey = a} :: GetIntegrationResponseResponse)

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

-- | The collection of response templates for the integration response as a
-- string-to-string map of key-value pairs. Response templates are
-- represented as a key\/value map, with a content-type as the key and a
-- template as the value.
getIntegrationResponseResponse_responseTemplates :: Lens.Lens' GetIntegrationResponseResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getIntegrationResponseResponse_responseTemplates = Lens.lens (\GetIntegrationResponseResponse' {responseTemplates} -> responseTemplates) (\s@GetIntegrationResponseResponse' {} a -> s {responseTemplates = a} :: GetIntegrationResponseResponse) Prelude.. Lens.mapping Lens.coerced

-- | The template selection expressions for the integration response.
getIntegrationResponseResponse_templateSelectionExpression :: Lens.Lens' GetIntegrationResponseResponse (Prelude.Maybe Prelude.Text)
getIntegrationResponseResponse_templateSelectionExpression = Lens.lens (\GetIntegrationResponseResponse' {templateSelectionExpression} -> templateSelectionExpression) (\s@GetIntegrationResponseResponse' {} a -> s {templateSelectionExpression = a} :: GetIntegrationResponseResponse)

-- | The response's http status code.
getIntegrationResponseResponse_httpStatus :: Lens.Lens' GetIntegrationResponseResponse Prelude.Int
getIntegrationResponseResponse_httpStatus = Lens.lens (\GetIntegrationResponseResponse' {httpStatus} -> httpStatus) (\s@GetIntegrationResponseResponse' {} a -> s {httpStatus = a} :: GetIntegrationResponseResponse)

instance
  Prelude.NFData
    GetIntegrationResponseResponse
  where
  rnf GetIntegrationResponseResponse' {..} =
    Prelude.rnf contentHandlingStrategy
      `Prelude.seq` Prelude.rnf integrationResponseId
      `Prelude.seq` Prelude.rnf integrationResponseKey
      `Prelude.seq` Prelude.rnf responseParameters
      `Prelude.seq` Prelude.rnf responseTemplates
      `Prelude.seq` Prelude.rnf templateSelectionExpression
      `Prelude.seq` Prelude.rnf httpStatus
