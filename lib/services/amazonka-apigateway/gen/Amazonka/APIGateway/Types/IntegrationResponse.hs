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
-- Module      : Amazonka.APIGateway.Types.IntegrationResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.IntegrationResponse where

import Amazonka.APIGateway.Types.ContentHandlingStrategy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents an integration response. The status code must map to an
-- existing MethodResponse, and parameters and templates can be used to
-- transform the back-end response.
--
-- /See:/ 'newIntegrationResponse' smart constructor.
data IntegrationResponse = IntegrationResponse'
  { -- | A key-value map specifying response parameters that are passed to the
    -- method response from the back end. The key is a method response header
    -- parameter name and the mapped value is an integration response header
    -- value, a static value enclosed within a pair of single quotes, or a JSON
    -- expression from the integration response body. The mapping key must
    -- match the pattern of @method.response.header.{name}@, where @name@ is a
    -- valid and unique header name. The mapped non-static value must match the
    -- pattern of @integration.response.header.{name}@ or
    -- @integration.response.body.{JSON-expression}@, where @name@ is a valid
    -- and unique response header name and @JSON-expression@ is a valid JSON
    -- expression without the @$@ prefix.
    responseParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies the templates used to transform the integration response body.
    -- Response templates are represented as a key\/value map, with a
    -- content-type as the key and a template as the value.
    responseTemplates :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies the regular expression (regex) pattern used to choose an
    -- integration response based on the response from the back end. For
    -- example, if the success response returns nothing and the error response
    -- returns some string, you could use the @.+@ regex to match error
    -- response. However, make sure that the error response does not contain
    -- any newline (@\\n@) character in such cases. If the back end is an AWS
    -- Lambda function, the AWS Lambda function error header is matched. For
    -- all other HTTP and AWS back ends, the HTTP status code is matched.
    selectionPattern :: Prelude.Maybe Prelude.Text,
    -- | Specifies how to handle response payload content type conversions.
    -- Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@, with the
    -- following behaviors:
    --
    -- If this property is not defined, the response payload will be passed
    -- through from the integration response to the method response without
    -- modification.
    contentHandling :: Prelude.Maybe ContentHandlingStrategy,
    -- | Specifies the status code that is used to map the integration response
    -- to an existing MethodResponse.
    statusCode :: Prelude.Maybe Prelude.Text
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
-- 'responseParameters', 'integrationResponse_responseParameters' - A key-value map specifying response parameters that are passed to the
-- method response from the back end. The key is a method response header
-- parameter name and the mapped value is an integration response header
-- value, a static value enclosed within a pair of single quotes, or a JSON
-- expression from the integration response body. The mapping key must
-- match the pattern of @method.response.header.{name}@, where @name@ is a
-- valid and unique header name. The mapped non-static value must match the
-- pattern of @integration.response.header.{name}@ or
-- @integration.response.body.{JSON-expression}@, where @name@ is a valid
-- and unique response header name and @JSON-expression@ is a valid JSON
-- expression without the @$@ prefix.
--
-- 'responseTemplates', 'integrationResponse_responseTemplates' - Specifies the templates used to transform the integration response body.
-- Response templates are represented as a key\/value map, with a
-- content-type as the key and a template as the value.
--
-- 'selectionPattern', 'integrationResponse_selectionPattern' - Specifies the regular expression (regex) pattern used to choose an
-- integration response based on the response from the back end. For
-- example, if the success response returns nothing and the error response
-- returns some string, you could use the @.+@ regex to match error
-- response. However, make sure that the error response does not contain
-- any newline (@\\n@) character in such cases. If the back end is an AWS
-- Lambda function, the AWS Lambda function error header is matched. For
-- all other HTTP and AWS back ends, the HTTP status code is matched.
--
-- 'contentHandling', 'integrationResponse_contentHandling' - Specifies how to handle response payload content type conversions.
-- Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@, with the
-- following behaviors:
--
-- If this property is not defined, the response payload will be passed
-- through from the integration response to the method response without
-- modification.
--
-- 'statusCode', 'integrationResponse_statusCode' - Specifies the status code that is used to map the integration response
-- to an existing MethodResponse.
newIntegrationResponse ::
  IntegrationResponse
newIntegrationResponse =
  IntegrationResponse'
    { responseParameters =
        Prelude.Nothing,
      responseTemplates = Prelude.Nothing,
      selectionPattern = Prelude.Nothing,
      contentHandling = Prelude.Nothing,
      statusCode = Prelude.Nothing
    }

-- | A key-value map specifying response parameters that are passed to the
-- method response from the back end. The key is a method response header
-- parameter name and the mapped value is an integration response header
-- value, a static value enclosed within a pair of single quotes, or a JSON
-- expression from the integration response body. The mapping key must
-- match the pattern of @method.response.header.{name}@, where @name@ is a
-- valid and unique header name. The mapped non-static value must match the
-- pattern of @integration.response.header.{name}@ or
-- @integration.response.body.{JSON-expression}@, where @name@ is a valid
-- and unique response header name and @JSON-expression@ is a valid JSON
-- expression without the @$@ prefix.
integrationResponse_responseParameters :: Lens.Lens' IntegrationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
integrationResponse_responseParameters = Lens.lens (\IntegrationResponse' {responseParameters} -> responseParameters) (\s@IntegrationResponse' {} a -> s {responseParameters = a} :: IntegrationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the templates used to transform the integration response body.
-- Response templates are represented as a key\/value map, with a
-- content-type as the key and a template as the value.
integrationResponse_responseTemplates :: Lens.Lens' IntegrationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
integrationResponse_responseTemplates = Lens.lens (\IntegrationResponse' {responseTemplates} -> responseTemplates) (\s@IntegrationResponse' {} a -> s {responseTemplates = a} :: IntegrationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the regular expression (regex) pattern used to choose an
-- integration response based on the response from the back end. For
-- example, if the success response returns nothing and the error response
-- returns some string, you could use the @.+@ regex to match error
-- response. However, make sure that the error response does not contain
-- any newline (@\\n@) character in such cases. If the back end is an AWS
-- Lambda function, the AWS Lambda function error header is matched. For
-- all other HTTP and AWS back ends, the HTTP status code is matched.
integrationResponse_selectionPattern :: Lens.Lens' IntegrationResponse (Prelude.Maybe Prelude.Text)
integrationResponse_selectionPattern = Lens.lens (\IntegrationResponse' {selectionPattern} -> selectionPattern) (\s@IntegrationResponse' {} a -> s {selectionPattern = a} :: IntegrationResponse)

-- | Specifies how to handle response payload content type conversions.
-- Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@, with the
-- following behaviors:
--
-- If this property is not defined, the response payload will be passed
-- through from the integration response to the method response without
-- modification.
integrationResponse_contentHandling :: Lens.Lens' IntegrationResponse (Prelude.Maybe ContentHandlingStrategy)
integrationResponse_contentHandling = Lens.lens (\IntegrationResponse' {contentHandling} -> contentHandling) (\s@IntegrationResponse' {} a -> s {contentHandling = a} :: IntegrationResponse)

-- | Specifies the status code that is used to map the integration response
-- to an existing MethodResponse.
integrationResponse_statusCode :: Lens.Lens' IntegrationResponse (Prelude.Maybe Prelude.Text)
integrationResponse_statusCode = Lens.lens (\IntegrationResponse' {statusCode} -> statusCode) (\s@IntegrationResponse' {} a -> s {statusCode = a} :: IntegrationResponse)

instance Core.FromJSON IntegrationResponse where
  parseJSON =
    Core.withObject
      "IntegrationResponse"
      ( \x ->
          IntegrationResponse'
            Prelude.<$> ( x Core..:? "responseParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "responseTemplates"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "selectionPattern")
            Prelude.<*> (x Core..:? "contentHandling")
            Prelude.<*> (x Core..:? "statusCode")
      )

instance Prelude.Hashable IntegrationResponse where
  hashWithSalt _salt IntegrationResponse' {..} =
    _salt `Prelude.hashWithSalt` responseParameters
      `Prelude.hashWithSalt` responseTemplates
      `Prelude.hashWithSalt` selectionPattern
      `Prelude.hashWithSalt` contentHandling
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData IntegrationResponse where
  rnf IntegrationResponse' {..} =
    Prelude.rnf responseParameters
      `Prelude.seq` Prelude.rnf responseTemplates
      `Prelude.seq` Prelude.rnf selectionPattern
      `Prelude.seq` Prelude.rnf contentHandling
      `Prelude.seq` Prelude.rnf statusCode
