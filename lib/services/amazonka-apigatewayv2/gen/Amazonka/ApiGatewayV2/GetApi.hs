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
-- Module      : Amazonka.ApiGatewayV2.GetApi
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an Api resource.
module Amazonka.ApiGatewayV2.GetApi
  ( -- * Creating a Request
    GetApi (..),
    newGetApi,

    -- * Request Lenses
    getApi_apiId,

    -- * Destructuring the Response
    GetApiResponse (..),
    newGetApiResponse,

    -- * Response Lenses
    getApiResponse_tags,
    getApiResponse_name,
    getApiResponse_apiEndpoint,
    getApiResponse_apiId,
    getApiResponse_routeSelectionExpression,
    getApiResponse_description,
    getApiResponse_warnings,
    getApiResponse_apiKeySelectionExpression,
    getApiResponse_protocolType,
    getApiResponse_disableExecuteApiEndpoint,
    getApiResponse_createdDate,
    getApiResponse_disableSchemaValidation,
    getApiResponse_importInfo,
    getApiResponse_corsConfiguration,
    getApiResponse_apiGatewayManaged,
    getApiResponse_version,
    getApiResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetApi' smart constructor.
data GetApi = GetApi'
  { -- | The API identifier.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'getApi_apiId' - The API identifier.
newGetApi ::
  -- | 'apiId'
  Prelude.Text ->
  GetApi
newGetApi pApiId_ = GetApi' {apiId = pApiId_}

-- | The API identifier.
getApi_apiId :: Lens.Lens' GetApi Prelude.Text
getApi_apiId = Lens.lens (\GetApi' {apiId} -> apiId) (\s@GetApi' {} a -> s {apiId = a} :: GetApi)

instance Core.AWSRequest GetApi where
  type AWSResponse GetApi = GetApiResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApiResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "apiEndpoint")
            Prelude.<*> (x Core..?> "apiId")
            Prelude.<*> (x Core..?> "routeSelectionExpression")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "warnings" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "apiKeySelectionExpression")
            Prelude.<*> (x Core..?> "protocolType")
            Prelude.<*> (x Core..?> "disableExecuteApiEndpoint")
            Prelude.<*> (x Core..?> "createdDate")
            Prelude.<*> (x Core..?> "disableSchemaValidation")
            Prelude.<*> (x Core..?> "importInfo" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "corsConfiguration")
            Prelude.<*> (x Core..?> "apiGatewayManaged")
            Prelude.<*> (x Core..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetApi where
  hashWithSalt _salt GetApi' {..} =
    _salt `Prelude.hashWithSalt` apiId

instance Prelude.NFData GetApi where
  rnf GetApi' {..} = Prelude.rnf apiId

instance Core.ToHeaders GetApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetApi where
  toPath GetApi' {..} =
    Prelude.mconcat ["/v2/apis/", Core.toBS apiId]

instance Core.ToQuery GetApi where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetApiResponse' smart constructor.
data GetApiResponse = GetApiResponse'
  { -- | A collection of tags associated with the API.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the API.
    name :: Prelude.Maybe Prelude.Text,
    -- | The URI of the API, of the form
    -- {api-id}.execute-api.{region}.amazonaws.com. The stage name is typically
    -- appended to this URI to form a complete path to a deployed API stage.
    apiEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The API ID.
    apiId :: Prelude.Maybe Prelude.Text,
    -- | The route selection expression for the API. For HTTP APIs, the
    -- routeSelectionExpression must be ${request.method} ${request.path}. If
    -- not provided, this will be the default for HTTP APIs. This property is
    -- required for WebSocket APIs.
    routeSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | The description of the API.
    description :: Prelude.Maybe Prelude.Text,
    -- | The warning messages reported when failonwarnings is turned on during
    -- API import.
    warnings :: Prelude.Maybe [Prelude.Text],
    -- | An API key selection expression. Supported only for WebSocket APIs. See
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions>.
    apiKeySelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | The API protocol.
    protocolType :: Prelude.Maybe ProtocolType,
    -- | Specifies whether clients can invoke your API by using the default
    -- execute-api endpoint. By default, clients can invoke your API with the
    -- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
    -- To require that clients use a custom domain name to invoke your API,
    -- disable the default endpoint.
    disableExecuteApiEndpoint :: Prelude.Maybe Prelude.Bool,
    -- | The timestamp when the API was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | Avoid validating models when creating a deployment. Supported only for
    -- WebSocket APIs.
    disableSchemaValidation :: Prelude.Maybe Prelude.Bool,
    -- | The validation information during API import. This may include
    -- particular properties of your OpenAPI definition which are ignored
    -- during import. Supported only for HTTP APIs.
    importInfo :: Prelude.Maybe [Prelude.Text],
    -- | A CORS configuration. Supported only for HTTP APIs.
    corsConfiguration :: Prelude.Maybe Cors,
    -- | Specifies whether an API is managed by API Gateway. You can\'t update or
    -- delete a managed API by using API Gateway. A managed API can be deleted
    -- only through the tooling or service that created it.
    apiGatewayManaged :: Prelude.Maybe Prelude.Bool,
    -- | A version identifier for the API.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getApiResponse_tags' - A collection of tags associated with the API.
--
-- 'name', 'getApiResponse_name' - The name of the API.
--
-- 'apiEndpoint', 'getApiResponse_apiEndpoint' - The URI of the API, of the form
-- {api-id}.execute-api.{region}.amazonaws.com. The stage name is typically
-- appended to this URI to form a complete path to a deployed API stage.
--
-- 'apiId', 'getApiResponse_apiId' - The API ID.
--
-- 'routeSelectionExpression', 'getApiResponse_routeSelectionExpression' - The route selection expression for the API. For HTTP APIs, the
-- routeSelectionExpression must be ${request.method} ${request.path}. If
-- not provided, this will be the default for HTTP APIs. This property is
-- required for WebSocket APIs.
--
-- 'description', 'getApiResponse_description' - The description of the API.
--
-- 'warnings', 'getApiResponse_warnings' - The warning messages reported when failonwarnings is turned on during
-- API import.
--
-- 'apiKeySelectionExpression', 'getApiResponse_apiKeySelectionExpression' - An API key selection expression. Supported only for WebSocket APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions>.
--
-- 'protocolType', 'getApiResponse_protocolType' - The API protocol.
--
-- 'disableExecuteApiEndpoint', 'getApiResponse_disableExecuteApiEndpoint' - Specifies whether clients can invoke your API by using the default
-- execute-api endpoint. By default, clients can invoke your API with the
-- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
-- To require that clients use a custom domain name to invoke your API,
-- disable the default endpoint.
--
-- 'createdDate', 'getApiResponse_createdDate' - The timestamp when the API was created.
--
-- 'disableSchemaValidation', 'getApiResponse_disableSchemaValidation' - Avoid validating models when creating a deployment. Supported only for
-- WebSocket APIs.
--
-- 'importInfo', 'getApiResponse_importInfo' - The validation information during API import. This may include
-- particular properties of your OpenAPI definition which are ignored
-- during import. Supported only for HTTP APIs.
--
-- 'corsConfiguration', 'getApiResponse_corsConfiguration' - A CORS configuration. Supported only for HTTP APIs.
--
-- 'apiGatewayManaged', 'getApiResponse_apiGatewayManaged' - Specifies whether an API is managed by API Gateway. You can\'t update or
-- delete a managed API by using API Gateway. A managed API can be deleted
-- only through the tooling or service that created it.
--
-- 'version', 'getApiResponse_version' - A version identifier for the API.
--
-- 'httpStatus', 'getApiResponse_httpStatus' - The response's http status code.
newGetApiResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetApiResponse
newGetApiResponse pHttpStatus_ =
  GetApiResponse'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      apiEndpoint = Prelude.Nothing,
      apiId = Prelude.Nothing,
      routeSelectionExpression = Prelude.Nothing,
      description = Prelude.Nothing,
      warnings = Prelude.Nothing,
      apiKeySelectionExpression = Prelude.Nothing,
      protocolType = Prelude.Nothing,
      disableExecuteApiEndpoint = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      disableSchemaValidation = Prelude.Nothing,
      importInfo = Prelude.Nothing,
      corsConfiguration = Prelude.Nothing,
      apiGatewayManaged = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of tags associated with the API.
getApiResponse_tags :: Lens.Lens' GetApiResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getApiResponse_tags = Lens.lens (\GetApiResponse' {tags} -> tags) (\s@GetApiResponse' {} a -> s {tags = a} :: GetApiResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the API.
getApiResponse_name :: Lens.Lens' GetApiResponse (Prelude.Maybe Prelude.Text)
getApiResponse_name = Lens.lens (\GetApiResponse' {name} -> name) (\s@GetApiResponse' {} a -> s {name = a} :: GetApiResponse)

-- | The URI of the API, of the form
-- {api-id}.execute-api.{region}.amazonaws.com. The stage name is typically
-- appended to this URI to form a complete path to a deployed API stage.
getApiResponse_apiEndpoint :: Lens.Lens' GetApiResponse (Prelude.Maybe Prelude.Text)
getApiResponse_apiEndpoint = Lens.lens (\GetApiResponse' {apiEndpoint} -> apiEndpoint) (\s@GetApiResponse' {} a -> s {apiEndpoint = a} :: GetApiResponse)

-- | The API ID.
getApiResponse_apiId :: Lens.Lens' GetApiResponse (Prelude.Maybe Prelude.Text)
getApiResponse_apiId = Lens.lens (\GetApiResponse' {apiId} -> apiId) (\s@GetApiResponse' {} a -> s {apiId = a} :: GetApiResponse)

-- | The route selection expression for the API. For HTTP APIs, the
-- routeSelectionExpression must be ${request.method} ${request.path}. If
-- not provided, this will be the default for HTTP APIs. This property is
-- required for WebSocket APIs.
getApiResponse_routeSelectionExpression :: Lens.Lens' GetApiResponse (Prelude.Maybe Prelude.Text)
getApiResponse_routeSelectionExpression = Lens.lens (\GetApiResponse' {routeSelectionExpression} -> routeSelectionExpression) (\s@GetApiResponse' {} a -> s {routeSelectionExpression = a} :: GetApiResponse)

-- | The description of the API.
getApiResponse_description :: Lens.Lens' GetApiResponse (Prelude.Maybe Prelude.Text)
getApiResponse_description = Lens.lens (\GetApiResponse' {description} -> description) (\s@GetApiResponse' {} a -> s {description = a} :: GetApiResponse)

-- | The warning messages reported when failonwarnings is turned on during
-- API import.
getApiResponse_warnings :: Lens.Lens' GetApiResponse (Prelude.Maybe [Prelude.Text])
getApiResponse_warnings = Lens.lens (\GetApiResponse' {warnings} -> warnings) (\s@GetApiResponse' {} a -> s {warnings = a} :: GetApiResponse) Prelude.. Lens.mapping Lens.coerced

-- | An API key selection expression. Supported only for WebSocket APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions>.
getApiResponse_apiKeySelectionExpression :: Lens.Lens' GetApiResponse (Prelude.Maybe Prelude.Text)
getApiResponse_apiKeySelectionExpression = Lens.lens (\GetApiResponse' {apiKeySelectionExpression} -> apiKeySelectionExpression) (\s@GetApiResponse' {} a -> s {apiKeySelectionExpression = a} :: GetApiResponse)

-- | The API protocol.
getApiResponse_protocolType :: Lens.Lens' GetApiResponse (Prelude.Maybe ProtocolType)
getApiResponse_protocolType = Lens.lens (\GetApiResponse' {protocolType} -> protocolType) (\s@GetApiResponse' {} a -> s {protocolType = a} :: GetApiResponse)

-- | Specifies whether clients can invoke your API by using the default
-- execute-api endpoint. By default, clients can invoke your API with the
-- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
-- To require that clients use a custom domain name to invoke your API,
-- disable the default endpoint.
getApiResponse_disableExecuteApiEndpoint :: Lens.Lens' GetApiResponse (Prelude.Maybe Prelude.Bool)
getApiResponse_disableExecuteApiEndpoint = Lens.lens (\GetApiResponse' {disableExecuteApiEndpoint} -> disableExecuteApiEndpoint) (\s@GetApiResponse' {} a -> s {disableExecuteApiEndpoint = a} :: GetApiResponse)

-- | The timestamp when the API was created.
getApiResponse_createdDate :: Lens.Lens' GetApiResponse (Prelude.Maybe Prelude.UTCTime)
getApiResponse_createdDate = Lens.lens (\GetApiResponse' {createdDate} -> createdDate) (\s@GetApiResponse' {} a -> s {createdDate = a} :: GetApiResponse) Prelude.. Lens.mapping Core._Time

-- | Avoid validating models when creating a deployment. Supported only for
-- WebSocket APIs.
getApiResponse_disableSchemaValidation :: Lens.Lens' GetApiResponse (Prelude.Maybe Prelude.Bool)
getApiResponse_disableSchemaValidation = Lens.lens (\GetApiResponse' {disableSchemaValidation} -> disableSchemaValidation) (\s@GetApiResponse' {} a -> s {disableSchemaValidation = a} :: GetApiResponse)

-- | The validation information during API import. This may include
-- particular properties of your OpenAPI definition which are ignored
-- during import. Supported only for HTTP APIs.
getApiResponse_importInfo :: Lens.Lens' GetApiResponse (Prelude.Maybe [Prelude.Text])
getApiResponse_importInfo = Lens.lens (\GetApiResponse' {importInfo} -> importInfo) (\s@GetApiResponse' {} a -> s {importInfo = a} :: GetApiResponse) Prelude.. Lens.mapping Lens.coerced

-- | A CORS configuration. Supported only for HTTP APIs.
getApiResponse_corsConfiguration :: Lens.Lens' GetApiResponse (Prelude.Maybe Cors)
getApiResponse_corsConfiguration = Lens.lens (\GetApiResponse' {corsConfiguration} -> corsConfiguration) (\s@GetApiResponse' {} a -> s {corsConfiguration = a} :: GetApiResponse)

-- | Specifies whether an API is managed by API Gateway. You can\'t update or
-- delete a managed API by using API Gateway. A managed API can be deleted
-- only through the tooling or service that created it.
getApiResponse_apiGatewayManaged :: Lens.Lens' GetApiResponse (Prelude.Maybe Prelude.Bool)
getApiResponse_apiGatewayManaged = Lens.lens (\GetApiResponse' {apiGatewayManaged} -> apiGatewayManaged) (\s@GetApiResponse' {} a -> s {apiGatewayManaged = a} :: GetApiResponse)

-- | A version identifier for the API.
getApiResponse_version :: Lens.Lens' GetApiResponse (Prelude.Maybe Prelude.Text)
getApiResponse_version = Lens.lens (\GetApiResponse' {version} -> version) (\s@GetApiResponse' {} a -> s {version = a} :: GetApiResponse)

-- | The response's http status code.
getApiResponse_httpStatus :: Lens.Lens' GetApiResponse Prelude.Int
getApiResponse_httpStatus = Lens.lens (\GetApiResponse' {httpStatus} -> httpStatus) (\s@GetApiResponse' {} a -> s {httpStatus = a} :: GetApiResponse)

instance Prelude.NFData GetApiResponse where
  rnf GetApiResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf apiEndpoint
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf routeSelectionExpression
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf warnings
      `Prelude.seq` Prelude.rnf apiKeySelectionExpression
      `Prelude.seq` Prelude.rnf protocolType
      `Prelude.seq` Prelude.rnf disableExecuteApiEndpoint
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf disableSchemaValidation
      `Prelude.seq` Prelude.rnf importInfo
      `Prelude.seq` Prelude.rnf corsConfiguration
      `Prelude.seq` Prelude.rnf apiGatewayManaged
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
