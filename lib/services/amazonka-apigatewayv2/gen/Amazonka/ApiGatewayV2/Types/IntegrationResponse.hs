{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ApiGatewayV2.Types.IntegrationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.IntegrationResponse where

import Amazonka.ApiGatewayV2.Types.ContentHandlingStrategy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents an integration response.
--
-- /See:/ 'newIntegrationResponse' smart constructor.
data IntegrationResponse = IntegrationResponse'
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
    -- | The integration response key.
    integrationResponseKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntegrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentHandlingStrategy', 'integrationResponse_contentHandlingStrategy' - Supported only for WebSocket APIs. Specifies how to handle response
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
-- 'integrationResponseId', 'integrationResponse_integrationResponseId' - The integration response ID.
--
-- 'responseParameters', 'integrationResponse_responseParameters' - A key-value map specifying response parameters that are passed to the
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
-- 'responseTemplates', 'integrationResponse_responseTemplates' - The collection of response templates for the integration response as a
-- string-to-string map of key-value pairs. Response templates are
-- represented as a key\/value map, with a content-type as the key and a
-- template as the value.
--
-- 'templateSelectionExpression', 'integrationResponse_templateSelectionExpression' - The template selection expressions for the integration response.
--
-- 'integrationResponseKey', 'integrationResponse_integrationResponseKey' - The integration response key.
newIntegrationResponse ::
  -- | 'integrationResponseKey'
  Prelude.Text ->
  IntegrationResponse
newIntegrationResponse pIntegrationResponseKey_ =
  IntegrationResponse'
    { contentHandlingStrategy =
        Prelude.Nothing,
      integrationResponseId = Prelude.Nothing,
      responseParameters = Prelude.Nothing,
      responseTemplates = Prelude.Nothing,
      templateSelectionExpression = Prelude.Nothing,
      integrationResponseKey = pIntegrationResponseKey_
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
integrationResponse_contentHandlingStrategy :: Lens.Lens' IntegrationResponse (Prelude.Maybe ContentHandlingStrategy)
integrationResponse_contentHandlingStrategy = Lens.lens (\IntegrationResponse' {contentHandlingStrategy} -> contentHandlingStrategy) (\s@IntegrationResponse' {} a -> s {contentHandlingStrategy = a} :: IntegrationResponse)

-- | The integration response ID.
integrationResponse_integrationResponseId :: Lens.Lens' IntegrationResponse (Prelude.Maybe Prelude.Text)
integrationResponse_integrationResponseId = Lens.lens (\IntegrationResponse' {integrationResponseId} -> integrationResponseId) (\s@IntegrationResponse' {} a -> s {integrationResponseId = a} :: IntegrationResponse)

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
integrationResponse_responseParameters :: Lens.Lens' IntegrationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
integrationResponse_responseParameters = Lens.lens (\IntegrationResponse' {responseParameters} -> responseParameters) (\s@IntegrationResponse' {} a -> s {responseParameters = a} :: IntegrationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The collection of response templates for the integration response as a
-- string-to-string map of key-value pairs. Response templates are
-- represented as a key\/value map, with a content-type as the key and a
-- template as the value.
integrationResponse_responseTemplates :: Lens.Lens' IntegrationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
integrationResponse_responseTemplates = Lens.lens (\IntegrationResponse' {responseTemplates} -> responseTemplates) (\s@IntegrationResponse' {} a -> s {responseTemplates = a} :: IntegrationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The template selection expressions for the integration response.
integrationResponse_templateSelectionExpression :: Lens.Lens' IntegrationResponse (Prelude.Maybe Prelude.Text)
integrationResponse_templateSelectionExpression = Lens.lens (\IntegrationResponse' {templateSelectionExpression} -> templateSelectionExpression) (\s@IntegrationResponse' {} a -> s {templateSelectionExpression = a} :: IntegrationResponse)

-- | The integration response key.
integrationResponse_integrationResponseKey :: Lens.Lens' IntegrationResponse Prelude.Text
integrationResponse_integrationResponseKey = Lens.lens (\IntegrationResponse' {integrationResponseKey} -> integrationResponseKey) (\s@IntegrationResponse' {} a -> s {integrationResponseKey = a} :: IntegrationResponse)

instance Data.FromJSON IntegrationResponse where
  parseJSON =
    Data.withObject
      "IntegrationResponse"
      ( \x ->
          IntegrationResponse'
            Prelude.<$> (x Data..:? "contentHandlingStrategy")
            Prelude.<*> (x Data..:? "integrationResponseId")
            Prelude.<*> ( x Data..:? "responseParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "responseTemplates"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "templateSelectionExpression")
            Prelude.<*> (x Data..: "integrationResponseKey")
      )

instance Prelude.Hashable IntegrationResponse where
  hashWithSalt _salt IntegrationResponse' {..} =
    _salt
      `Prelude.hashWithSalt` contentHandlingStrategy
      `Prelude.hashWithSalt` integrationResponseId
      `Prelude.hashWithSalt` responseParameters
      `Prelude.hashWithSalt` responseTemplates
      `Prelude.hashWithSalt` templateSelectionExpression
      `Prelude.hashWithSalt` integrationResponseKey

instance Prelude.NFData IntegrationResponse where
  rnf IntegrationResponse' {..} =
    Prelude.rnf contentHandlingStrategy
      `Prelude.seq` Prelude.rnf integrationResponseId
      `Prelude.seq` Prelude.rnf responseParameters
      `Prelude.seq` Prelude.rnf responseTemplates
      `Prelude.seq` Prelude.rnf templateSelectionExpression
      `Prelude.seq` Prelude.rnf integrationResponseKey
