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
-- Module      : Amazonka.ApiGatewayV2.ImportApi
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports an API.
module Amazonka.ApiGatewayV2.ImportApi
  ( -- * Creating a Request
    ImportApi (..),
    newImportApi,

    -- * Request Lenses
    importApi_basepath,
    importApi_failOnWarnings,
    importApi_body,

    -- * Destructuring the Response
    ImportApiResponse (..),
    newImportApiResponse,

    -- * Response Lenses
    importApiResponse_apiEndpoint,
    importApiResponse_apiGatewayManaged,
    importApiResponse_apiId,
    importApiResponse_apiKeySelectionExpression,
    importApiResponse_corsConfiguration,
    importApiResponse_createdDate,
    importApiResponse_description,
    importApiResponse_disableExecuteApiEndpoint,
    importApiResponse_disableSchemaValidation,
    importApiResponse_importInfo,
    importApiResponse_name,
    importApiResponse_protocolType,
    importApiResponse_routeSelectionExpression,
    importApiResponse_tags,
    importApiResponse_version,
    importApiResponse_warnings,
    importApiResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newImportApi' smart constructor.
data ImportApi = ImportApi'
  { -- | Specifies how to interpret the base path of the API during import. Valid
    -- values are ignore, prepend, and split. The default value is ignore. To
    -- learn more, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-import-api-basePath.html Set the OpenAPI basePath Property>.
    -- Supported only for HTTP APIs.
    basepath :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to rollback the API creation when a warning is
    -- encountered. By default, API creation continues if a warning is
    -- encountered.
    failOnWarnings :: Prelude.Maybe Prelude.Bool,
    -- | The OpenAPI definition. Supported only for HTTP APIs.
    body :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'basepath', 'importApi_basepath' - Specifies how to interpret the base path of the API during import. Valid
-- values are ignore, prepend, and split. The default value is ignore. To
-- learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-import-api-basePath.html Set the OpenAPI basePath Property>.
-- Supported only for HTTP APIs.
--
-- 'failOnWarnings', 'importApi_failOnWarnings' - Specifies whether to rollback the API creation when a warning is
-- encountered. By default, API creation continues if a warning is
-- encountered.
--
-- 'body', 'importApi_body' - The OpenAPI definition. Supported only for HTTP APIs.
newImportApi ::
  -- | 'body'
  Prelude.Text ->
  ImportApi
newImportApi pBody_ =
  ImportApi'
    { basepath = Prelude.Nothing,
      failOnWarnings = Prelude.Nothing,
      body = pBody_
    }

-- | Specifies how to interpret the base path of the API during import. Valid
-- values are ignore, prepend, and split. The default value is ignore. To
-- learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-import-api-basePath.html Set the OpenAPI basePath Property>.
-- Supported only for HTTP APIs.
importApi_basepath :: Lens.Lens' ImportApi (Prelude.Maybe Prelude.Text)
importApi_basepath = Lens.lens (\ImportApi' {basepath} -> basepath) (\s@ImportApi' {} a -> s {basepath = a} :: ImportApi)

-- | Specifies whether to rollback the API creation when a warning is
-- encountered. By default, API creation continues if a warning is
-- encountered.
importApi_failOnWarnings :: Lens.Lens' ImportApi (Prelude.Maybe Prelude.Bool)
importApi_failOnWarnings = Lens.lens (\ImportApi' {failOnWarnings} -> failOnWarnings) (\s@ImportApi' {} a -> s {failOnWarnings = a} :: ImportApi)

-- | The OpenAPI definition. Supported only for HTTP APIs.
importApi_body :: Lens.Lens' ImportApi Prelude.Text
importApi_body = Lens.lens (\ImportApi' {body} -> body) (\s@ImportApi' {} a -> s {body = a} :: ImportApi)

instance Core.AWSRequest ImportApi where
  type AWSResponse ImportApi = ImportApiResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportApiResponse'
            Prelude.<$> (x Data..?> "apiEndpoint")
            Prelude.<*> (x Data..?> "apiGatewayManaged")
            Prelude.<*> (x Data..?> "apiId")
            Prelude.<*> (x Data..?> "apiKeySelectionExpression")
            Prelude.<*> (x Data..?> "corsConfiguration")
            Prelude.<*> (x Data..?> "createdDate")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "disableExecuteApiEndpoint")
            Prelude.<*> (x Data..?> "disableSchemaValidation")
            Prelude.<*> (x Data..?> "importInfo" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "protocolType")
            Prelude.<*> (x Data..?> "routeSelectionExpression")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (x Data..?> "warnings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportApi where
  hashWithSalt _salt ImportApi' {..} =
    _salt
      `Prelude.hashWithSalt` basepath
      `Prelude.hashWithSalt` failOnWarnings
      `Prelude.hashWithSalt` body

instance Prelude.NFData ImportApi where
  rnf ImportApi' {..} =
    Prelude.rnf basepath
      `Prelude.seq` Prelude.rnf failOnWarnings
      `Prelude.seq` Prelude.rnf body

instance Data.ToHeaders ImportApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportApi where
  toJSON ImportApi' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("body" Data..= body)]
      )

instance Data.ToPath ImportApi where
  toPath = Prelude.const "/v2/apis"

instance Data.ToQuery ImportApi where
  toQuery ImportApi' {..} =
    Prelude.mconcat
      [ "basepath" Data.=: basepath,
        "failOnWarnings" Data.=: failOnWarnings
      ]

-- | /See:/ 'newImportApiResponse' smart constructor.
data ImportApiResponse = ImportApiResponse'
  { -- | The URI of the API, of the form
    -- {api-id}.execute-api.{region}.amazonaws.com. The stage name is typically
    -- appended to this URI to form a complete path to a deployed API stage.
    apiEndpoint :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether an API is managed by API Gateway. You can\'t update or
    -- delete a managed API by using API Gateway. A managed API can be deleted
    -- only through the tooling or service that created it.
    apiGatewayManaged :: Prelude.Maybe Prelude.Bool,
    -- | The API ID.
    apiId :: Prelude.Maybe Prelude.Text,
    -- | An API key selection expression. Supported only for WebSocket APIs. See
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions>.
    apiKeySelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | A CORS configuration. Supported only for HTTP APIs.
    corsConfiguration :: Prelude.Maybe Cors,
    -- | The timestamp when the API was created.
    createdDate :: Prelude.Maybe Data.ISO8601,
    -- | The description of the API.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether clients can invoke your API by using the default
    -- execute-api endpoint. By default, clients can invoke your API with the
    -- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
    -- To require that clients use a custom domain name to invoke your API,
    -- disable the default endpoint.
    disableExecuteApiEndpoint :: Prelude.Maybe Prelude.Bool,
    -- | Avoid validating models when creating a deployment. Supported only for
    -- WebSocket APIs.
    disableSchemaValidation :: Prelude.Maybe Prelude.Bool,
    -- | The validation information during API import. This may include
    -- particular properties of your OpenAPI definition which are ignored
    -- during import. Supported only for HTTP APIs.
    importInfo :: Prelude.Maybe [Prelude.Text],
    -- | The name of the API.
    name :: Prelude.Maybe Prelude.Text,
    -- | The API protocol.
    protocolType :: Prelude.Maybe ProtocolType,
    -- | The route selection expression for the API. For HTTP APIs, the
    -- routeSelectionExpression must be ${request.method} ${request.path}. If
    -- not provided, this will be the default for HTTP APIs. This property is
    -- required for WebSocket APIs.
    routeSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | A collection of tags associated with the API.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A version identifier for the API.
    version :: Prelude.Maybe Prelude.Text,
    -- | The warning messages reported when failonwarnings is turned on during
    -- API import.
    warnings :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportApiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiEndpoint', 'importApiResponse_apiEndpoint' - The URI of the API, of the form
-- {api-id}.execute-api.{region}.amazonaws.com. The stage name is typically
-- appended to this URI to form a complete path to a deployed API stage.
--
-- 'apiGatewayManaged', 'importApiResponse_apiGatewayManaged' - Specifies whether an API is managed by API Gateway. You can\'t update or
-- delete a managed API by using API Gateway. A managed API can be deleted
-- only through the tooling or service that created it.
--
-- 'apiId', 'importApiResponse_apiId' - The API ID.
--
-- 'apiKeySelectionExpression', 'importApiResponse_apiKeySelectionExpression' - An API key selection expression. Supported only for WebSocket APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions>.
--
-- 'corsConfiguration', 'importApiResponse_corsConfiguration' - A CORS configuration. Supported only for HTTP APIs.
--
-- 'createdDate', 'importApiResponse_createdDate' - The timestamp when the API was created.
--
-- 'description', 'importApiResponse_description' - The description of the API.
--
-- 'disableExecuteApiEndpoint', 'importApiResponse_disableExecuteApiEndpoint' - Specifies whether clients can invoke your API by using the default
-- execute-api endpoint. By default, clients can invoke your API with the
-- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
-- To require that clients use a custom domain name to invoke your API,
-- disable the default endpoint.
--
-- 'disableSchemaValidation', 'importApiResponse_disableSchemaValidation' - Avoid validating models when creating a deployment. Supported only for
-- WebSocket APIs.
--
-- 'importInfo', 'importApiResponse_importInfo' - The validation information during API import. This may include
-- particular properties of your OpenAPI definition which are ignored
-- during import. Supported only for HTTP APIs.
--
-- 'name', 'importApiResponse_name' - The name of the API.
--
-- 'protocolType', 'importApiResponse_protocolType' - The API protocol.
--
-- 'routeSelectionExpression', 'importApiResponse_routeSelectionExpression' - The route selection expression for the API. For HTTP APIs, the
-- routeSelectionExpression must be ${request.method} ${request.path}. If
-- not provided, this will be the default for HTTP APIs. This property is
-- required for WebSocket APIs.
--
-- 'tags', 'importApiResponse_tags' - A collection of tags associated with the API.
--
-- 'version', 'importApiResponse_version' - A version identifier for the API.
--
-- 'warnings', 'importApiResponse_warnings' - The warning messages reported when failonwarnings is turned on during
-- API import.
--
-- 'httpStatus', 'importApiResponse_httpStatus' - The response's http status code.
newImportApiResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportApiResponse
newImportApiResponse pHttpStatus_ =
  ImportApiResponse'
    { apiEndpoint = Prelude.Nothing,
      apiGatewayManaged = Prelude.Nothing,
      apiId = Prelude.Nothing,
      apiKeySelectionExpression = Prelude.Nothing,
      corsConfiguration = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      disableExecuteApiEndpoint = Prelude.Nothing,
      disableSchemaValidation = Prelude.Nothing,
      importInfo = Prelude.Nothing,
      name = Prelude.Nothing,
      protocolType = Prelude.Nothing,
      routeSelectionExpression = Prelude.Nothing,
      tags = Prelude.Nothing,
      version = Prelude.Nothing,
      warnings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The URI of the API, of the form
-- {api-id}.execute-api.{region}.amazonaws.com. The stage name is typically
-- appended to this URI to form a complete path to a deployed API stage.
importApiResponse_apiEndpoint :: Lens.Lens' ImportApiResponse (Prelude.Maybe Prelude.Text)
importApiResponse_apiEndpoint = Lens.lens (\ImportApiResponse' {apiEndpoint} -> apiEndpoint) (\s@ImportApiResponse' {} a -> s {apiEndpoint = a} :: ImportApiResponse)

-- | Specifies whether an API is managed by API Gateway. You can\'t update or
-- delete a managed API by using API Gateway. A managed API can be deleted
-- only through the tooling or service that created it.
importApiResponse_apiGatewayManaged :: Lens.Lens' ImportApiResponse (Prelude.Maybe Prelude.Bool)
importApiResponse_apiGatewayManaged = Lens.lens (\ImportApiResponse' {apiGatewayManaged} -> apiGatewayManaged) (\s@ImportApiResponse' {} a -> s {apiGatewayManaged = a} :: ImportApiResponse)

-- | The API ID.
importApiResponse_apiId :: Lens.Lens' ImportApiResponse (Prelude.Maybe Prelude.Text)
importApiResponse_apiId = Lens.lens (\ImportApiResponse' {apiId} -> apiId) (\s@ImportApiResponse' {} a -> s {apiId = a} :: ImportApiResponse)

-- | An API key selection expression. Supported only for WebSocket APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions>.
importApiResponse_apiKeySelectionExpression :: Lens.Lens' ImportApiResponse (Prelude.Maybe Prelude.Text)
importApiResponse_apiKeySelectionExpression = Lens.lens (\ImportApiResponse' {apiKeySelectionExpression} -> apiKeySelectionExpression) (\s@ImportApiResponse' {} a -> s {apiKeySelectionExpression = a} :: ImportApiResponse)

-- | A CORS configuration. Supported only for HTTP APIs.
importApiResponse_corsConfiguration :: Lens.Lens' ImportApiResponse (Prelude.Maybe Cors)
importApiResponse_corsConfiguration = Lens.lens (\ImportApiResponse' {corsConfiguration} -> corsConfiguration) (\s@ImportApiResponse' {} a -> s {corsConfiguration = a} :: ImportApiResponse)

-- | The timestamp when the API was created.
importApiResponse_createdDate :: Lens.Lens' ImportApiResponse (Prelude.Maybe Prelude.UTCTime)
importApiResponse_createdDate = Lens.lens (\ImportApiResponse' {createdDate} -> createdDate) (\s@ImportApiResponse' {} a -> s {createdDate = a} :: ImportApiResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the API.
importApiResponse_description :: Lens.Lens' ImportApiResponse (Prelude.Maybe Prelude.Text)
importApiResponse_description = Lens.lens (\ImportApiResponse' {description} -> description) (\s@ImportApiResponse' {} a -> s {description = a} :: ImportApiResponse)

-- | Specifies whether clients can invoke your API by using the default
-- execute-api endpoint. By default, clients can invoke your API with the
-- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
-- To require that clients use a custom domain name to invoke your API,
-- disable the default endpoint.
importApiResponse_disableExecuteApiEndpoint :: Lens.Lens' ImportApiResponse (Prelude.Maybe Prelude.Bool)
importApiResponse_disableExecuteApiEndpoint = Lens.lens (\ImportApiResponse' {disableExecuteApiEndpoint} -> disableExecuteApiEndpoint) (\s@ImportApiResponse' {} a -> s {disableExecuteApiEndpoint = a} :: ImportApiResponse)

-- | Avoid validating models when creating a deployment. Supported only for
-- WebSocket APIs.
importApiResponse_disableSchemaValidation :: Lens.Lens' ImportApiResponse (Prelude.Maybe Prelude.Bool)
importApiResponse_disableSchemaValidation = Lens.lens (\ImportApiResponse' {disableSchemaValidation} -> disableSchemaValidation) (\s@ImportApiResponse' {} a -> s {disableSchemaValidation = a} :: ImportApiResponse)

-- | The validation information during API import. This may include
-- particular properties of your OpenAPI definition which are ignored
-- during import. Supported only for HTTP APIs.
importApiResponse_importInfo :: Lens.Lens' ImportApiResponse (Prelude.Maybe [Prelude.Text])
importApiResponse_importInfo = Lens.lens (\ImportApiResponse' {importInfo} -> importInfo) (\s@ImportApiResponse' {} a -> s {importInfo = a} :: ImportApiResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the API.
importApiResponse_name :: Lens.Lens' ImportApiResponse (Prelude.Maybe Prelude.Text)
importApiResponse_name = Lens.lens (\ImportApiResponse' {name} -> name) (\s@ImportApiResponse' {} a -> s {name = a} :: ImportApiResponse)

-- | The API protocol.
importApiResponse_protocolType :: Lens.Lens' ImportApiResponse (Prelude.Maybe ProtocolType)
importApiResponse_protocolType = Lens.lens (\ImportApiResponse' {protocolType} -> protocolType) (\s@ImportApiResponse' {} a -> s {protocolType = a} :: ImportApiResponse)

-- | The route selection expression for the API. For HTTP APIs, the
-- routeSelectionExpression must be ${request.method} ${request.path}. If
-- not provided, this will be the default for HTTP APIs. This property is
-- required for WebSocket APIs.
importApiResponse_routeSelectionExpression :: Lens.Lens' ImportApiResponse (Prelude.Maybe Prelude.Text)
importApiResponse_routeSelectionExpression = Lens.lens (\ImportApiResponse' {routeSelectionExpression} -> routeSelectionExpression) (\s@ImportApiResponse' {} a -> s {routeSelectionExpression = a} :: ImportApiResponse)

-- | A collection of tags associated with the API.
importApiResponse_tags :: Lens.Lens' ImportApiResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
importApiResponse_tags = Lens.lens (\ImportApiResponse' {tags} -> tags) (\s@ImportApiResponse' {} a -> s {tags = a} :: ImportApiResponse) Prelude.. Lens.mapping Lens.coerced

-- | A version identifier for the API.
importApiResponse_version :: Lens.Lens' ImportApiResponse (Prelude.Maybe Prelude.Text)
importApiResponse_version = Lens.lens (\ImportApiResponse' {version} -> version) (\s@ImportApiResponse' {} a -> s {version = a} :: ImportApiResponse)

-- | The warning messages reported when failonwarnings is turned on during
-- API import.
importApiResponse_warnings :: Lens.Lens' ImportApiResponse (Prelude.Maybe [Prelude.Text])
importApiResponse_warnings = Lens.lens (\ImportApiResponse' {warnings} -> warnings) (\s@ImportApiResponse' {} a -> s {warnings = a} :: ImportApiResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
importApiResponse_httpStatus :: Lens.Lens' ImportApiResponse Prelude.Int
importApiResponse_httpStatus = Lens.lens (\ImportApiResponse' {httpStatus} -> httpStatus) (\s@ImportApiResponse' {} a -> s {httpStatus = a} :: ImportApiResponse)

instance Prelude.NFData ImportApiResponse where
  rnf ImportApiResponse' {..} =
    Prelude.rnf apiEndpoint
      `Prelude.seq` Prelude.rnf apiGatewayManaged
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf apiKeySelectionExpression
      `Prelude.seq` Prelude.rnf corsConfiguration
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf disableExecuteApiEndpoint
      `Prelude.seq` Prelude.rnf disableSchemaValidation
      `Prelude.seq` Prelude.rnf importInfo
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf protocolType
      `Prelude.seq` Prelude.rnf routeSelectionExpression
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf warnings
      `Prelude.seq` Prelude.rnf httpStatus
