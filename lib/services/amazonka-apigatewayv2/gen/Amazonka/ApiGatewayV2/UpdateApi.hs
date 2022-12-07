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
-- Module      : Amazonka.ApiGatewayV2.UpdateApi
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Api resource.
module Amazonka.ApiGatewayV2.UpdateApi
  ( -- * Creating a Request
    UpdateApi (..),
    newUpdateApi,

    -- * Request Lenses
    updateApi_credentialsArn,
    updateApi_name,
    updateApi_routeSelectionExpression,
    updateApi_target,
    updateApi_description,
    updateApi_routeKey,
    updateApi_apiKeySelectionExpression,
    updateApi_disableExecuteApiEndpoint,
    updateApi_disableSchemaValidation,
    updateApi_corsConfiguration,
    updateApi_version,
    updateApi_apiId,

    -- * Destructuring the Response
    UpdateApiResponse (..),
    newUpdateApiResponse,

    -- * Response Lenses
    updateApiResponse_tags,
    updateApiResponse_name,
    updateApiResponse_apiEndpoint,
    updateApiResponse_apiId,
    updateApiResponse_routeSelectionExpression,
    updateApiResponse_description,
    updateApiResponse_warnings,
    updateApiResponse_apiKeySelectionExpression,
    updateApiResponse_protocolType,
    updateApiResponse_disableExecuteApiEndpoint,
    updateApiResponse_createdDate,
    updateApiResponse_disableSchemaValidation,
    updateApiResponse_importInfo,
    updateApiResponse_corsConfiguration,
    updateApiResponse_apiGatewayManaged,
    updateApiResponse_version,
    updateApiResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Updates an Api.
--
-- /See:/ 'newUpdateApi' smart constructor.
data UpdateApi = UpdateApi'
  { -- | This property is part of quick create. It specifies the credentials
    -- required for the integration, if any. For a Lambda integration, three
    -- options are available. To specify an IAM Role for API Gateway to assume,
    -- use the role\'s Amazon Resource Name (ARN). To require that the
    -- caller\'s identity be passed through from the request, specify
    -- arn:aws:iam::*:user\/*. To use resource-based permissions on supported
    -- AWS services, don\'t specify this parameter. Currently, this property is
    -- not used for HTTP integrations. If provided, this value replaces the
    -- credentials associated with the quick create integration. Supported only
    -- for HTTP APIs.
    credentialsArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the API.
    name :: Prelude.Maybe Prelude.Text,
    -- | The route selection expression for the API. For HTTP APIs, the
    -- routeSelectionExpression must be ${request.method} ${request.path}. If
    -- not provided, this will be the default for HTTP APIs. This property is
    -- required for WebSocket APIs.
    routeSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | This property is part of quick create. For HTTP integrations, specify a
    -- fully qualified URL. For Lambda integrations, specify a function ARN.
    -- The type of the integration will be HTTP_PROXY or AWS_PROXY,
    -- respectively. The value provided updates the integration URI and
    -- integration type. You can update a quick-created target, but you can\'t
    -- remove it from an API. Supported only for HTTP APIs.
    target :: Prelude.Maybe Prelude.Text,
    -- | The description of the API.
    description :: Prelude.Maybe Prelude.Text,
    -- | This property is part of quick create. If not specified, the route
    -- created using quick create is kept. Otherwise, this value replaces the
    -- route key of the quick create route. Additional routes may still be
    -- added after the API is updated. Supported only for HTTP APIs.
    routeKey :: Prelude.Maybe Prelude.Text,
    -- | An API key selection expression. Supported only for WebSocket APIs. See
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions>.
    apiKeySelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether clients can invoke your API by using the default
    -- execute-api endpoint. By default, clients can invoke your API with the
    -- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
    -- To require that clients use a custom domain name to invoke your API,
    -- disable the default endpoint.
    disableExecuteApiEndpoint :: Prelude.Maybe Prelude.Bool,
    -- | Avoid validating models when creating a deployment. Supported only for
    -- WebSocket APIs.
    disableSchemaValidation :: Prelude.Maybe Prelude.Bool,
    -- | A CORS configuration. Supported only for HTTP APIs.
    corsConfiguration :: Prelude.Maybe Cors,
    -- | A version identifier for the API.
    version :: Prelude.Maybe Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentialsArn', 'updateApi_credentialsArn' - This property is part of quick create. It specifies the credentials
-- required for the integration, if any. For a Lambda integration, three
-- options are available. To specify an IAM Role for API Gateway to assume,
-- use the role\'s Amazon Resource Name (ARN). To require that the
-- caller\'s identity be passed through from the request, specify
-- arn:aws:iam::*:user\/*. To use resource-based permissions on supported
-- AWS services, don\'t specify this parameter. Currently, this property is
-- not used for HTTP integrations. If provided, this value replaces the
-- credentials associated with the quick create integration. Supported only
-- for HTTP APIs.
--
-- 'name', 'updateApi_name' - The name of the API.
--
-- 'routeSelectionExpression', 'updateApi_routeSelectionExpression' - The route selection expression for the API. For HTTP APIs, the
-- routeSelectionExpression must be ${request.method} ${request.path}. If
-- not provided, this will be the default for HTTP APIs. This property is
-- required for WebSocket APIs.
--
-- 'target', 'updateApi_target' - This property is part of quick create. For HTTP integrations, specify a
-- fully qualified URL. For Lambda integrations, specify a function ARN.
-- The type of the integration will be HTTP_PROXY or AWS_PROXY,
-- respectively. The value provided updates the integration URI and
-- integration type. You can update a quick-created target, but you can\'t
-- remove it from an API. Supported only for HTTP APIs.
--
-- 'description', 'updateApi_description' - The description of the API.
--
-- 'routeKey', 'updateApi_routeKey' - This property is part of quick create. If not specified, the route
-- created using quick create is kept. Otherwise, this value replaces the
-- route key of the quick create route. Additional routes may still be
-- added after the API is updated. Supported only for HTTP APIs.
--
-- 'apiKeySelectionExpression', 'updateApi_apiKeySelectionExpression' - An API key selection expression. Supported only for WebSocket APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions>.
--
-- 'disableExecuteApiEndpoint', 'updateApi_disableExecuteApiEndpoint' - Specifies whether clients can invoke your API by using the default
-- execute-api endpoint. By default, clients can invoke your API with the
-- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
-- To require that clients use a custom domain name to invoke your API,
-- disable the default endpoint.
--
-- 'disableSchemaValidation', 'updateApi_disableSchemaValidation' - Avoid validating models when creating a deployment. Supported only for
-- WebSocket APIs.
--
-- 'corsConfiguration', 'updateApi_corsConfiguration' - A CORS configuration. Supported only for HTTP APIs.
--
-- 'version', 'updateApi_version' - A version identifier for the API.
--
-- 'apiId', 'updateApi_apiId' - The API identifier.
newUpdateApi ::
  -- | 'apiId'
  Prelude.Text ->
  UpdateApi
newUpdateApi pApiId_ =
  UpdateApi'
    { credentialsArn = Prelude.Nothing,
      name = Prelude.Nothing,
      routeSelectionExpression = Prelude.Nothing,
      target = Prelude.Nothing,
      description = Prelude.Nothing,
      routeKey = Prelude.Nothing,
      apiKeySelectionExpression = Prelude.Nothing,
      disableExecuteApiEndpoint = Prelude.Nothing,
      disableSchemaValidation = Prelude.Nothing,
      corsConfiguration = Prelude.Nothing,
      version = Prelude.Nothing,
      apiId = pApiId_
    }

-- | This property is part of quick create. It specifies the credentials
-- required for the integration, if any. For a Lambda integration, three
-- options are available. To specify an IAM Role for API Gateway to assume,
-- use the role\'s Amazon Resource Name (ARN). To require that the
-- caller\'s identity be passed through from the request, specify
-- arn:aws:iam::*:user\/*. To use resource-based permissions on supported
-- AWS services, don\'t specify this parameter. Currently, this property is
-- not used for HTTP integrations. If provided, this value replaces the
-- credentials associated with the quick create integration. Supported only
-- for HTTP APIs.
updateApi_credentialsArn :: Lens.Lens' UpdateApi (Prelude.Maybe Prelude.Text)
updateApi_credentialsArn = Lens.lens (\UpdateApi' {credentialsArn} -> credentialsArn) (\s@UpdateApi' {} a -> s {credentialsArn = a} :: UpdateApi)

-- | The name of the API.
updateApi_name :: Lens.Lens' UpdateApi (Prelude.Maybe Prelude.Text)
updateApi_name = Lens.lens (\UpdateApi' {name} -> name) (\s@UpdateApi' {} a -> s {name = a} :: UpdateApi)

-- | The route selection expression for the API. For HTTP APIs, the
-- routeSelectionExpression must be ${request.method} ${request.path}. If
-- not provided, this will be the default for HTTP APIs. This property is
-- required for WebSocket APIs.
updateApi_routeSelectionExpression :: Lens.Lens' UpdateApi (Prelude.Maybe Prelude.Text)
updateApi_routeSelectionExpression = Lens.lens (\UpdateApi' {routeSelectionExpression} -> routeSelectionExpression) (\s@UpdateApi' {} a -> s {routeSelectionExpression = a} :: UpdateApi)

-- | This property is part of quick create. For HTTP integrations, specify a
-- fully qualified URL. For Lambda integrations, specify a function ARN.
-- The type of the integration will be HTTP_PROXY or AWS_PROXY,
-- respectively. The value provided updates the integration URI and
-- integration type. You can update a quick-created target, but you can\'t
-- remove it from an API. Supported only for HTTP APIs.
updateApi_target :: Lens.Lens' UpdateApi (Prelude.Maybe Prelude.Text)
updateApi_target = Lens.lens (\UpdateApi' {target} -> target) (\s@UpdateApi' {} a -> s {target = a} :: UpdateApi)

-- | The description of the API.
updateApi_description :: Lens.Lens' UpdateApi (Prelude.Maybe Prelude.Text)
updateApi_description = Lens.lens (\UpdateApi' {description} -> description) (\s@UpdateApi' {} a -> s {description = a} :: UpdateApi)

-- | This property is part of quick create. If not specified, the route
-- created using quick create is kept. Otherwise, this value replaces the
-- route key of the quick create route. Additional routes may still be
-- added after the API is updated. Supported only for HTTP APIs.
updateApi_routeKey :: Lens.Lens' UpdateApi (Prelude.Maybe Prelude.Text)
updateApi_routeKey = Lens.lens (\UpdateApi' {routeKey} -> routeKey) (\s@UpdateApi' {} a -> s {routeKey = a} :: UpdateApi)

-- | An API key selection expression. Supported only for WebSocket APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions>.
updateApi_apiKeySelectionExpression :: Lens.Lens' UpdateApi (Prelude.Maybe Prelude.Text)
updateApi_apiKeySelectionExpression = Lens.lens (\UpdateApi' {apiKeySelectionExpression} -> apiKeySelectionExpression) (\s@UpdateApi' {} a -> s {apiKeySelectionExpression = a} :: UpdateApi)

-- | Specifies whether clients can invoke your API by using the default
-- execute-api endpoint. By default, clients can invoke your API with the
-- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
-- To require that clients use a custom domain name to invoke your API,
-- disable the default endpoint.
updateApi_disableExecuteApiEndpoint :: Lens.Lens' UpdateApi (Prelude.Maybe Prelude.Bool)
updateApi_disableExecuteApiEndpoint = Lens.lens (\UpdateApi' {disableExecuteApiEndpoint} -> disableExecuteApiEndpoint) (\s@UpdateApi' {} a -> s {disableExecuteApiEndpoint = a} :: UpdateApi)

-- | Avoid validating models when creating a deployment. Supported only for
-- WebSocket APIs.
updateApi_disableSchemaValidation :: Lens.Lens' UpdateApi (Prelude.Maybe Prelude.Bool)
updateApi_disableSchemaValidation = Lens.lens (\UpdateApi' {disableSchemaValidation} -> disableSchemaValidation) (\s@UpdateApi' {} a -> s {disableSchemaValidation = a} :: UpdateApi)

-- | A CORS configuration. Supported only for HTTP APIs.
updateApi_corsConfiguration :: Lens.Lens' UpdateApi (Prelude.Maybe Cors)
updateApi_corsConfiguration = Lens.lens (\UpdateApi' {corsConfiguration} -> corsConfiguration) (\s@UpdateApi' {} a -> s {corsConfiguration = a} :: UpdateApi)

-- | A version identifier for the API.
updateApi_version :: Lens.Lens' UpdateApi (Prelude.Maybe Prelude.Text)
updateApi_version = Lens.lens (\UpdateApi' {version} -> version) (\s@UpdateApi' {} a -> s {version = a} :: UpdateApi)

-- | The API identifier.
updateApi_apiId :: Lens.Lens' UpdateApi Prelude.Text
updateApi_apiId = Lens.lens (\UpdateApi' {apiId} -> apiId) (\s@UpdateApi' {} a -> s {apiId = a} :: UpdateApi)

instance Core.AWSRequest UpdateApi where
  type AWSResponse UpdateApi = UpdateApiResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApiResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "apiEndpoint")
            Prelude.<*> (x Data..?> "apiId")
            Prelude.<*> (x Data..?> "routeSelectionExpression")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "warnings" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "apiKeySelectionExpression")
            Prelude.<*> (x Data..?> "protocolType")
            Prelude.<*> (x Data..?> "disableExecuteApiEndpoint")
            Prelude.<*> (x Data..?> "createdDate")
            Prelude.<*> (x Data..?> "disableSchemaValidation")
            Prelude.<*> (x Data..?> "importInfo" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "corsConfiguration")
            Prelude.<*> (x Data..?> "apiGatewayManaged")
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateApi where
  hashWithSalt _salt UpdateApi' {..} =
    _salt `Prelude.hashWithSalt` credentialsArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` routeSelectionExpression
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` routeKey
      `Prelude.hashWithSalt` apiKeySelectionExpression
      `Prelude.hashWithSalt` disableExecuteApiEndpoint
      `Prelude.hashWithSalt` disableSchemaValidation
      `Prelude.hashWithSalt` corsConfiguration
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData UpdateApi where
  rnf UpdateApi' {..} =
    Prelude.rnf credentialsArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf routeSelectionExpression
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf routeKey
      `Prelude.seq` Prelude.rnf apiKeySelectionExpression
      `Prelude.seq` Prelude.rnf disableExecuteApiEndpoint
      `Prelude.seq` Prelude.rnf disableSchemaValidation
      `Prelude.seq` Prelude.rnf corsConfiguration
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf apiId

instance Data.ToHeaders UpdateApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateApi where
  toJSON UpdateApi' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("credentialsArn" Data..=)
              Prelude.<$> credentialsArn,
            ("name" Data..=) Prelude.<$> name,
            ("routeSelectionExpression" Data..=)
              Prelude.<$> routeSelectionExpression,
            ("target" Data..=) Prelude.<$> target,
            ("description" Data..=) Prelude.<$> description,
            ("routeKey" Data..=) Prelude.<$> routeKey,
            ("apiKeySelectionExpression" Data..=)
              Prelude.<$> apiKeySelectionExpression,
            ("disableExecuteApiEndpoint" Data..=)
              Prelude.<$> disableExecuteApiEndpoint,
            ("disableSchemaValidation" Data..=)
              Prelude.<$> disableSchemaValidation,
            ("corsConfiguration" Data..=)
              Prelude.<$> corsConfiguration,
            ("version" Data..=) Prelude.<$> version
          ]
      )

instance Data.ToPath UpdateApi where
  toPath UpdateApi' {..} =
    Prelude.mconcat ["/v2/apis/", Data.toBS apiId]

instance Data.ToQuery UpdateApi where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApiResponse' smart constructor.
data UpdateApiResponse = UpdateApiResponse'
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
    createdDate :: Prelude.Maybe Data.POSIX,
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
-- Create a value of 'UpdateApiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'updateApiResponse_tags' - A collection of tags associated with the API.
--
-- 'name', 'updateApiResponse_name' - The name of the API.
--
-- 'apiEndpoint', 'updateApiResponse_apiEndpoint' - The URI of the API, of the form
-- {api-id}.execute-api.{region}.amazonaws.com. The stage name is typically
-- appended to this URI to form a complete path to a deployed API stage.
--
-- 'apiId', 'updateApiResponse_apiId' - The API ID.
--
-- 'routeSelectionExpression', 'updateApiResponse_routeSelectionExpression' - The route selection expression for the API. For HTTP APIs, the
-- routeSelectionExpression must be ${request.method} ${request.path}. If
-- not provided, this will be the default for HTTP APIs. This property is
-- required for WebSocket APIs.
--
-- 'description', 'updateApiResponse_description' - The description of the API.
--
-- 'warnings', 'updateApiResponse_warnings' - The warning messages reported when failonwarnings is turned on during
-- API import.
--
-- 'apiKeySelectionExpression', 'updateApiResponse_apiKeySelectionExpression' - An API key selection expression. Supported only for WebSocket APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions>.
--
-- 'protocolType', 'updateApiResponse_protocolType' - The API protocol.
--
-- 'disableExecuteApiEndpoint', 'updateApiResponse_disableExecuteApiEndpoint' - Specifies whether clients can invoke your API by using the default
-- execute-api endpoint. By default, clients can invoke your API with the
-- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
-- To require that clients use a custom domain name to invoke your API,
-- disable the default endpoint.
--
-- 'createdDate', 'updateApiResponse_createdDate' - The timestamp when the API was created.
--
-- 'disableSchemaValidation', 'updateApiResponse_disableSchemaValidation' - Avoid validating models when creating a deployment. Supported only for
-- WebSocket APIs.
--
-- 'importInfo', 'updateApiResponse_importInfo' - The validation information during API import. This may include
-- particular properties of your OpenAPI definition which are ignored
-- during import. Supported only for HTTP APIs.
--
-- 'corsConfiguration', 'updateApiResponse_corsConfiguration' - A CORS configuration. Supported only for HTTP APIs.
--
-- 'apiGatewayManaged', 'updateApiResponse_apiGatewayManaged' - Specifies whether an API is managed by API Gateway. You can\'t update or
-- delete a managed API by using API Gateway. A managed API can be deleted
-- only through the tooling or service that created it.
--
-- 'version', 'updateApiResponse_version' - A version identifier for the API.
--
-- 'httpStatus', 'updateApiResponse_httpStatus' - The response's http status code.
newUpdateApiResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateApiResponse
newUpdateApiResponse pHttpStatus_ =
  UpdateApiResponse'
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
updateApiResponse_tags :: Lens.Lens' UpdateApiResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateApiResponse_tags = Lens.lens (\UpdateApiResponse' {tags} -> tags) (\s@UpdateApiResponse' {} a -> s {tags = a} :: UpdateApiResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the API.
updateApiResponse_name :: Lens.Lens' UpdateApiResponse (Prelude.Maybe Prelude.Text)
updateApiResponse_name = Lens.lens (\UpdateApiResponse' {name} -> name) (\s@UpdateApiResponse' {} a -> s {name = a} :: UpdateApiResponse)

-- | The URI of the API, of the form
-- {api-id}.execute-api.{region}.amazonaws.com. The stage name is typically
-- appended to this URI to form a complete path to a deployed API stage.
updateApiResponse_apiEndpoint :: Lens.Lens' UpdateApiResponse (Prelude.Maybe Prelude.Text)
updateApiResponse_apiEndpoint = Lens.lens (\UpdateApiResponse' {apiEndpoint} -> apiEndpoint) (\s@UpdateApiResponse' {} a -> s {apiEndpoint = a} :: UpdateApiResponse)

-- | The API ID.
updateApiResponse_apiId :: Lens.Lens' UpdateApiResponse (Prelude.Maybe Prelude.Text)
updateApiResponse_apiId = Lens.lens (\UpdateApiResponse' {apiId} -> apiId) (\s@UpdateApiResponse' {} a -> s {apiId = a} :: UpdateApiResponse)

-- | The route selection expression for the API. For HTTP APIs, the
-- routeSelectionExpression must be ${request.method} ${request.path}. If
-- not provided, this will be the default for HTTP APIs. This property is
-- required for WebSocket APIs.
updateApiResponse_routeSelectionExpression :: Lens.Lens' UpdateApiResponse (Prelude.Maybe Prelude.Text)
updateApiResponse_routeSelectionExpression = Lens.lens (\UpdateApiResponse' {routeSelectionExpression} -> routeSelectionExpression) (\s@UpdateApiResponse' {} a -> s {routeSelectionExpression = a} :: UpdateApiResponse)

-- | The description of the API.
updateApiResponse_description :: Lens.Lens' UpdateApiResponse (Prelude.Maybe Prelude.Text)
updateApiResponse_description = Lens.lens (\UpdateApiResponse' {description} -> description) (\s@UpdateApiResponse' {} a -> s {description = a} :: UpdateApiResponse)

-- | The warning messages reported when failonwarnings is turned on during
-- API import.
updateApiResponse_warnings :: Lens.Lens' UpdateApiResponse (Prelude.Maybe [Prelude.Text])
updateApiResponse_warnings = Lens.lens (\UpdateApiResponse' {warnings} -> warnings) (\s@UpdateApiResponse' {} a -> s {warnings = a} :: UpdateApiResponse) Prelude.. Lens.mapping Lens.coerced

-- | An API key selection expression. Supported only for WebSocket APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions>.
updateApiResponse_apiKeySelectionExpression :: Lens.Lens' UpdateApiResponse (Prelude.Maybe Prelude.Text)
updateApiResponse_apiKeySelectionExpression = Lens.lens (\UpdateApiResponse' {apiKeySelectionExpression} -> apiKeySelectionExpression) (\s@UpdateApiResponse' {} a -> s {apiKeySelectionExpression = a} :: UpdateApiResponse)

-- | The API protocol.
updateApiResponse_protocolType :: Lens.Lens' UpdateApiResponse (Prelude.Maybe ProtocolType)
updateApiResponse_protocolType = Lens.lens (\UpdateApiResponse' {protocolType} -> protocolType) (\s@UpdateApiResponse' {} a -> s {protocolType = a} :: UpdateApiResponse)

-- | Specifies whether clients can invoke your API by using the default
-- execute-api endpoint. By default, clients can invoke your API with the
-- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
-- To require that clients use a custom domain name to invoke your API,
-- disable the default endpoint.
updateApiResponse_disableExecuteApiEndpoint :: Lens.Lens' UpdateApiResponse (Prelude.Maybe Prelude.Bool)
updateApiResponse_disableExecuteApiEndpoint = Lens.lens (\UpdateApiResponse' {disableExecuteApiEndpoint} -> disableExecuteApiEndpoint) (\s@UpdateApiResponse' {} a -> s {disableExecuteApiEndpoint = a} :: UpdateApiResponse)

-- | The timestamp when the API was created.
updateApiResponse_createdDate :: Lens.Lens' UpdateApiResponse (Prelude.Maybe Prelude.UTCTime)
updateApiResponse_createdDate = Lens.lens (\UpdateApiResponse' {createdDate} -> createdDate) (\s@UpdateApiResponse' {} a -> s {createdDate = a} :: UpdateApiResponse) Prelude.. Lens.mapping Data._Time

-- | Avoid validating models when creating a deployment. Supported only for
-- WebSocket APIs.
updateApiResponse_disableSchemaValidation :: Lens.Lens' UpdateApiResponse (Prelude.Maybe Prelude.Bool)
updateApiResponse_disableSchemaValidation = Lens.lens (\UpdateApiResponse' {disableSchemaValidation} -> disableSchemaValidation) (\s@UpdateApiResponse' {} a -> s {disableSchemaValidation = a} :: UpdateApiResponse)

-- | The validation information during API import. This may include
-- particular properties of your OpenAPI definition which are ignored
-- during import. Supported only for HTTP APIs.
updateApiResponse_importInfo :: Lens.Lens' UpdateApiResponse (Prelude.Maybe [Prelude.Text])
updateApiResponse_importInfo = Lens.lens (\UpdateApiResponse' {importInfo} -> importInfo) (\s@UpdateApiResponse' {} a -> s {importInfo = a} :: UpdateApiResponse) Prelude.. Lens.mapping Lens.coerced

-- | A CORS configuration. Supported only for HTTP APIs.
updateApiResponse_corsConfiguration :: Lens.Lens' UpdateApiResponse (Prelude.Maybe Cors)
updateApiResponse_corsConfiguration = Lens.lens (\UpdateApiResponse' {corsConfiguration} -> corsConfiguration) (\s@UpdateApiResponse' {} a -> s {corsConfiguration = a} :: UpdateApiResponse)

-- | Specifies whether an API is managed by API Gateway. You can\'t update or
-- delete a managed API by using API Gateway. A managed API can be deleted
-- only through the tooling or service that created it.
updateApiResponse_apiGatewayManaged :: Lens.Lens' UpdateApiResponse (Prelude.Maybe Prelude.Bool)
updateApiResponse_apiGatewayManaged = Lens.lens (\UpdateApiResponse' {apiGatewayManaged} -> apiGatewayManaged) (\s@UpdateApiResponse' {} a -> s {apiGatewayManaged = a} :: UpdateApiResponse)

-- | A version identifier for the API.
updateApiResponse_version :: Lens.Lens' UpdateApiResponse (Prelude.Maybe Prelude.Text)
updateApiResponse_version = Lens.lens (\UpdateApiResponse' {version} -> version) (\s@UpdateApiResponse' {} a -> s {version = a} :: UpdateApiResponse)

-- | The response's http status code.
updateApiResponse_httpStatus :: Lens.Lens' UpdateApiResponse Prelude.Int
updateApiResponse_httpStatus = Lens.lens (\UpdateApiResponse' {httpStatus} -> httpStatus) (\s@UpdateApiResponse' {} a -> s {httpStatus = a} :: UpdateApiResponse)

instance Prelude.NFData UpdateApiResponse where
  rnf UpdateApiResponse' {..} =
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
