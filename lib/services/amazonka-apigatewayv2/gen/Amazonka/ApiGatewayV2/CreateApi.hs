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
-- Module      : Amazonka.ApiGatewayV2.CreateApi
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Api resource.
module Amazonka.ApiGatewayV2.CreateApi
  ( -- * Creating a Request
    CreateApi (..),
    newCreateApi,

    -- * Request Lenses
    createApi_apiKeySelectionExpression,
    createApi_corsConfiguration,
    createApi_credentialsArn,
    createApi_description,
    createApi_disableExecuteApiEndpoint,
    createApi_disableSchemaValidation,
    createApi_routeKey,
    createApi_routeSelectionExpression,
    createApi_tags,
    createApi_target,
    createApi_version,
    createApi_protocolType,
    createApi_name,

    -- * Destructuring the Response
    CreateApiResponse (..),
    newCreateApiResponse,

    -- * Response Lenses
    createApiResponse_apiEndpoint,
    createApiResponse_apiGatewayManaged,
    createApiResponse_apiId,
    createApiResponse_apiKeySelectionExpression,
    createApiResponse_corsConfiguration,
    createApiResponse_createdDate,
    createApiResponse_description,
    createApiResponse_disableExecuteApiEndpoint,
    createApiResponse_disableSchemaValidation,
    createApiResponse_importInfo,
    createApiResponse_name,
    createApiResponse_protocolType,
    createApiResponse_routeSelectionExpression,
    createApiResponse_tags,
    createApiResponse_version,
    createApiResponse_warnings,
    createApiResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates a new Api resource to represent an API.
--
-- /See:/ 'newCreateApi' smart constructor.
data CreateApi = CreateApi'
  { -- | An API key selection expression. Supported only for WebSocket APIs. See
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions>.
    apiKeySelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | A CORS configuration. Supported only for HTTP APIs. See
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-cors.html Configuring CORS>
    -- for more information.
    corsConfiguration :: Prelude.Maybe Cors,
    -- | This property is part of quick create. It specifies the credentials
    -- required for the integration, if any. For a Lambda integration, three
    -- options are available. To specify an IAM Role for API Gateway to assume,
    -- use the role\'s Amazon Resource Name (ARN). To require that the
    -- caller\'s identity be passed through from the request, specify
    -- arn:aws:iam::*:user\/*. To use resource-based permissions on supported
    -- AWS services, specify null. Currently, this property is not used for
    -- HTTP integrations. Supported only for HTTP APIs.
    credentialsArn :: Prelude.Maybe Prelude.Text,
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
    -- | This property is part of quick create. If you don\'t specify a routeKey,
    -- a default route of $default is created. The $default route acts as a
    -- catch-all for any request made to your API, for a particular stage. The
    -- \$default route key can\'t be modified. You can add routes after creating
    -- the API, and you can update the route keys of additional routes.
    -- Supported only for HTTP APIs.
    routeKey :: Prelude.Maybe Prelude.Text,
    -- | The route selection expression for the API. For HTTP APIs, the
    -- routeSelectionExpression must be ${request.method} ${request.path}. If
    -- not provided, this will be the default for HTTP APIs. This property is
    -- required for WebSocket APIs.
    routeSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | This property is part of quick create. Quick create produces an API with
    -- an integration, a default catch-all route, and a default stage which is
    -- configured to automatically deploy changes. For HTTP integrations,
    -- specify a fully qualified URL. For Lambda integrations, specify a
    -- function ARN. The type of the integration will be HTTP_PROXY or
    -- AWS_PROXY, respectively. Supported only for HTTP APIs.
    target :: Prelude.Maybe Prelude.Text,
    -- | A version identifier for the API.
    version :: Prelude.Maybe Prelude.Text,
    -- | The API protocol.
    protocolType :: ProtocolType,
    -- | The name of the API.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiKeySelectionExpression', 'createApi_apiKeySelectionExpression' - An API key selection expression. Supported only for WebSocket APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions>.
--
-- 'corsConfiguration', 'createApi_corsConfiguration' - A CORS configuration. Supported only for HTTP APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-cors.html Configuring CORS>
-- for more information.
--
-- 'credentialsArn', 'createApi_credentialsArn' - This property is part of quick create. It specifies the credentials
-- required for the integration, if any. For a Lambda integration, three
-- options are available. To specify an IAM Role for API Gateway to assume,
-- use the role\'s Amazon Resource Name (ARN). To require that the
-- caller\'s identity be passed through from the request, specify
-- arn:aws:iam::*:user\/*. To use resource-based permissions on supported
-- AWS services, specify null. Currently, this property is not used for
-- HTTP integrations. Supported only for HTTP APIs.
--
-- 'description', 'createApi_description' - The description of the API.
--
-- 'disableExecuteApiEndpoint', 'createApi_disableExecuteApiEndpoint' - Specifies whether clients can invoke your API by using the default
-- execute-api endpoint. By default, clients can invoke your API with the
-- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
-- To require that clients use a custom domain name to invoke your API,
-- disable the default endpoint.
--
-- 'disableSchemaValidation', 'createApi_disableSchemaValidation' - Avoid validating models when creating a deployment. Supported only for
-- WebSocket APIs.
--
-- 'routeKey', 'createApi_routeKey' - This property is part of quick create. If you don\'t specify a routeKey,
-- a default route of $default is created. The $default route acts as a
-- catch-all for any request made to your API, for a particular stage. The
-- \$default route key can\'t be modified. You can add routes after creating
-- the API, and you can update the route keys of additional routes.
-- Supported only for HTTP APIs.
--
-- 'routeSelectionExpression', 'createApi_routeSelectionExpression' - The route selection expression for the API. For HTTP APIs, the
-- routeSelectionExpression must be ${request.method} ${request.path}. If
-- not provided, this will be the default for HTTP APIs. This property is
-- required for WebSocket APIs.
--
-- 'tags', 'createApi_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
--
-- 'target', 'createApi_target' - This property is part of quick create. Quick create produces an API with
-- an integration, a default catch-all route, and a default stage which is
-- configured to automatically deploy changes. For HTTP integrations,
-- specify a fully qualified URL. For Lambda integrations, specify a
-- function ARN. The type of the integration will be HTTP_PROXY or
-- AWS_PROXY, respectively. Supported only for HTTP APIs.
--
-- 'version', 'createApi_version' - A version identifier for the API.
--
-- 'protocolType', 'createApi_protocolType' - The API protocol.
--
-- 'name', 'createApi_name' - The name of the API.
newCreateApi ::
  -- | 'protocolType'
  ProtocolType ->
  -- | 'name'
  Prelude.Text ->
  CreateApi
newCreateApi pProtocolType_ pName_ =
  CreateApi'
    { apiKeySelectionExpression =
        Prelude.Nothing,
      corsConfiguration = Prelude.Nothing,
      credentialsArn = Prelude.Nothing,
      description = Prelude.Nothing,
      disableExecuteApiEndpoint = Prelude.Nothing,
      disableSchemaValidation = Prelude.Nothing,
      routeKey = Prelude.Nothing,
      routeSelectionExpression = Prelude.Nothing,
      tags = Prelude.Nothing,
      target = Prelude.Nothing,
      version = Prelude.Nothing,
      protocolType = pProtocolType_,
      name = pName_
    }

-- | An API key selection expression. Supported only for WebSocket APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions>.
createApi_apiKeySelectionExpression :: Lens.Lens' CreateApi (Prelude.Maybe Prelude.Text)
createApi_apiKeySelectionExpression = Lens.lens (\CreateApi' {apiKeySelectionExpression} -> apiKeySelectionExpression) (\s@CreateApi' {} a -> s {apiKeySelectionExpression = a} :: CreateApi)

-- | A CORS configuration. Supported only for HTTP APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-cors.html Configuring CORS>
-- for more information.
createApi_corsConfiguration :: Lens.Lens' CreateApi (Prelude.Maybe Cors)
createApi_corsConfiguration = Lens.lens (\CreateApi' {corsConfiguration} -> corsConfiguration) (\s@CreateApi' {} a -> s {corsConfiguration = a} :: CreateApi)

-- | This property is part of quick create. It specifies the credentials
-- required for the integration, if any. For a Lambda integration, three
-- options are available. To specify an IAM Role for API Gateway to assume,
-- use the role\'s Amazon Resource Name (ARN). To require that the
-- caller\'s identity be passed through from the request, specify
-- arn:aws:iam::*:user\/*. To use resource-based permissions on supported
-- AWS services, specify null. Currently, this property is not used for
-- HTTP integrations. Supported only for HTTP APIs.
createApi_credentialsArn :: Lens.Lens' CreateApi (Prelude.Maybe Prelude.Text)
createApi_credentialsArn = Lens.lens (\CreateApi' {credentialsArn} -> credentialsArn) (\s@CreateApi' {} a -> s {credentialsArn = a} :: CreateApi)

-- | The description of the API.
createApi_description :: Lens.Lens' CreateApi (Prelude.Maybe Prelude.Text)
createApi_description = Lens.lens (\CreateApi' {description} -> description) (\s@CreateApi' {} a -> s {description = a} :: CreateApi)

-- | Specifies whether clients can invoke your API by using the default
-- execute-api endpoint. By default, clients can invoke your API with the
-- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
-- To require that clients use a custom domain name to invoke your API,
-- disable the default endpoint.
createApi_disableExecuteApiEndpoint :: Lens.Lens' CreateApi (Prelude.Maybe Prelude.Bool)
createApi_disableExecuteApiEndpoint = Lens.lens (\CreateApi' {disableExecuteApiEndpoint} -> disableExecuteApiEndpoint) (\s@CreateApi' {} a -> s {disableExecuteApiEndpoint = a} :: CreateApi)

-- | Avoid validating models when creating a deployment. Supported only for
-- WebSocket APIs.
createApi_disableSchemaValidation :: Lens.Lens' CreateApi (Prelude.Maybe Prelude.Bool)
createApi_disableSchemaValidation = Lens.lens (\CreateApi' {disableSchemaValidation} -> disableSchemaValidation) (\s@CreateApi' {} a -> s {disableSchemaValidation = a} :: CreateApi)

-- | This property is part of quick create. If you don\'t specify a routeKey,
-- a default route of $default is created. The $default route acts as a
-- catch-all for any request made to your API, for a particular stage. The
-- \$default route key can\'t be modified. You can add routes after creating
-- the API, and you can update the route keys of additional routes.
-- Supported only for HTTP APIs.
createApi_routeKey :: Lens.Lens' CreateApi (Prelude.Maybe Prelude.Text)
createApi_routeKey = Lens.lens (\CreateApi' {routeKey} -> routeKey) (\s@CreateApi' {} a -> s {routeKey = a} :: CreateApi)

-- | The route selection expression for the API. For HTTP APIs, the
-- routeSelectionExpression must be ${request.method} ${request.path}. If
-- not provided, this will be the default for HTTP APIs. This property is
-- required for WebSocket APIs.
createApi_routeSelectionExpression :: Lens.Lens' CreateApi (Prelude.Maybe Prelude.Text)
createApi_routeSelectionExpression = Lens.lens (\CreateApi' {routeSelectionExpression} -> routeSelectionExpression) (\s@CreateApi' {} a -> s {routeSelectionExpression = a} :: CreateApi)

-- | The collection of tags. Each tag element is associated with a given
-- resource.
createApi_tags :: Lens.Lens' CreateApi (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createApi_tags = Lens.lens (\CreateApi' {tags} -> tags) (\s@CreateApi' {} a -> s {tags = a} :: CreateApi) Prelude.. Lens.mapping Lens.coerced

-- | This property is part of quick create. Quick create produces an API with
-- an integration, a default catch-all route, and a default stage which is
-- configured to automatically deploy changes. For HTTP integrations,
-- specify a fully qualified URL. For Lambda integrations, specify a
-- function ARN. The type of the integration will be HTTP_PROXY or
-- AWS_PROXY, respectively. Supported only for HTTP APIs.
createApi_target :: Lens.Lens' CreateApi (Prelude.Maybe Prelude.Text)
createApi_target = Lens.lens (\CreateApi' {target} -> target) (\s@CreateApi' {} a -> s {target = a} :: CreateApi)

-- | A version identifier for the API.
createApi_version :: Lens.Lens' CreateApi (Prelude.Maybe Prelude.Text)
createApi_version = Lens.lens (\CreateApi' {version} -> version) (\s@CreateApi' {} a -> s {version = a} :: CreateApi)

-- | The API protocol.
createApi_protocolType :: Lens.Lens' CreateApi ProtocolType
createApi_protocolType = Lens.lens (\CreateApi' {protocolType} -> protocolType) (\s@CreateApi' {} a -> s {protocolType = a} :: CreateApi)

-- | The name of the API.
createApi_name :: Lens.Lens' CreateApi Prelude.Text
createApi_name = Lens.lens (\CreateApi' {name} -> name) (\s@CreateApi' {} a -> s {name = a} :: CreateApi)

instance Core.AWSRequest CreateApi where
  type AWSResponse CreateApi = CreateApiResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApiResponse'
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

instance Prelude.Hashable CreateApi where
  hashWithSalt _salt CreateApi' {..} =
    _salt
      `Prelude.hashWithSalt` apiKeySelectionExpression
      `Prelude.hashWithSalt` corsConfiguration
      `Prelude.hashWithSalt` credentialsArn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` disableExecuteApiEndpoint
      `Prelude.hashWithSalt` disableSchemaValidation
      `Prelude.hashWithSalt` routeKey
      `Prelude.hashWithSalt` routeSelectionExpression
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` protocolType
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateApi where
  rnf CreateApi' {..} =
    Prelude.rnf apiKeySelectionExpression
      `Prelude.seq` Prelude.rnf corsConfiguration
      `Prelude.seq` Prelude.rnf credentialsArn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf disableExecuteApiEndpoint
      `Prelude.seq` Prelude.rnf disableSchemaValidation
      `Prelude.seq` Prelude.rnf routeKey
      `Prelude.seq` Prelude.rnf routeSelectionExpression
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf protocolType
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateApi where
  toJSON CreateApi' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("apiKeySelectionExpression" Data..=)
              Prelude.<$> apiKeySelectionExpression,
            ("corsConfiguration" Data..=)
              Prelude.<$> corsConfiguration,
            ("credentialsArn" Data..=)
              Prelude.<$> credentialsArn,
            ("description" Data..=) Prelude.<$> description,
            ("disableExecuteApiEndpoint" Data..=)
              Prelude.<$> disableExecuteApiEndpoint,
            ("disableSchemaValidation" Data..=)
              Prelude.<$> disableSchemaValidation,
            ("routeKey" Data..=) Prelude.<$> routeKey,
            ("routeSelectionExpression" Data..=)
              Prelude.<$> routeSelectionExpression,
            ("tags" Data..=) Prelude.<$> tags,
            ("target" Data..=) Prelude.<$> target,
            ("version" Data..=) Prelude.<$> version,
            Prelude.Just ("protocolType" Data..= protocolType),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateApi where
  toPath = Prelude.const "/v2/apis"

instance Data.ToQuery CreateApi where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateApiResponse' smart constructor.
data CreateApiResponse = CreateApiResponse'
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
-- Create a value of 'CreateApiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiEndpoint', 'createApiResponse_apiEndpoint' - The URI of the API, of the form
-- {api-id}.execute-api.{region}.amazonaws.com. The stage name is typically
-- appended to this URI to form a complete path to a deployed API stage.
--
-- 'apiGatewayManaged', 'createApiResponse_apiGatewayManaged' - Specifies whether an API is managed by API Gateway. You can\'t update or
-- delete a managed API by using API Gateway. A managed API can be deleted
-- only through the tooling or service that created it.
--
-- 'apiId', 'createApiResponse_apiId' - The API ID.
--
-- 'apiKeySelectionExpression', 'createApiResponse_apiKeySelectionExpression' - An API key selection expression. Supported only for WebSocket APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions>.
--
-- 'corsConfiguration', 'createApiResponse_corsConfiguration' - A CORS configuration. Supported only for HTTP APIs.
--
-- 'createdDate', 'createApiResponse_createdDate' - The timestamp when the API was created.
--
-- 'description', 'createApiResponse_description' - The description of the API.
--
-- 'disableExecuteApiEndpoint', 'createApiResponse_disableExecuteApiEndpoint' - Specifies whether clients can invoke your API by using the default
-- execute-api endpoint. By default, clients can invoke your API with the
-- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
-- To require that clients use a custom domain name to invoke your API,
-- disable the default endpoint.
--
-- 'disableSchemaValidation', 'createApiResponse_disableSchemaValidation' - Avoid validating models when creating a deployment. Supported only for
-- WebSocket APIs.
--
-- 'importInfo', 'createApiResponse_importInfo' - The validation information during API import. This may include
-- particular properties of your OpenAPI definition which are ignored
-- during import. Supported only for HTTP APIs.
--
-- 'name', 'createApiResponse_name' - The name of the API.
--
-- 'protocolType', 'createApiResponse_protocolType' - The API protocol.
--
-- 'routeSelectionExpression', 'createApiResponse_routeSelectionExpression' - The route selection expression for the API. For HTTP APIs, the
-- routeSelectionExpression must be ${request.method} ${request.path}. If
-- not provided, this will be the default for HTTP APIs. This property is
-- required for WebSocket APIs.
--
-- 'tags', 'createApiResponse_tags' - A collection of tags associated with the API.
--
-- 'version', 'createApiResponse_version' - A version identifier for the API.
--
-- 'warnings', 'createApiResponse_warnings' - The warning messages reported when failonwarnings is turned on during
-- API import.
--
-- 'httpStatus', 'createApiResponse_httpStatus' - The response's http status code.
newCreateApiResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateApiResponse
newCreateApiResponse pHttpStatus_ =
  CreateApiResponse'
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
createApiResponse_apiEndpoint :: Lens.Lens' CreateApiResponse (Prelude.Maybe Prelude.Text)
createApiResponse_apiEndpoint = Lens.lens (\CreateApiResponse' {apiEndpoint} -> apiEndpoint) (\s@CreateApiResponse' {} a -> s {apiEndpoint = a} :: CreateApiResponse)

-- | Specifies whether an API is managed by API Gateway. You can\'t update or
-- delete a managed API by using API Gateway. A managed API can be deleted
-- only through the tooling or service that created it.
createApiResponse_apiGatewayManaged :: Lens.Lens' CreateApiResponse (Prelude.Maybe Prelude.Bool)
createApiResponse_apiGatewayManaged = Lens.lens (\CreateApiResponse' {apiGatewayManaged} -> apiGatewayManaged) (\s@CreateApiResponse' {} a -> s {apiGatewayManaged = a} :: CreateApiResponse)

-- | The API ID.
createApiResponse_apiId :: Lens.Lens' CreateApiResponse (Prelude.Maybe Prelude.Text)
createApiResponse_apiId = Lens.lens (\CreateApiResponse' {apiId} -> apiId) (\s@CreateApiResponse' {} a -> s {apiId = a} :: CreateApiResponse)

-- | An API key selection expression. Supported only for WebSocket APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions>.
createApiResponse_apiKeySelectionExpression :: Lens.Lens' CreateApiResponse (Prelude.Maybe Prelude.Text)
createApiResponse_apiKeySelectionExpression = Lens.lens (\CreateApiResponse' {apiKeySelectionExpression} -> apiKeySelectionExpression) (\s@CreateApiResponse' {} a -> s {apiKeySelectionExpression = a} :: CreateApiResponse)

-- | A CORS configuration. Supported only for HTTP APIs.
createApiResponse_corsConfiguration :: Lens.Lens' CreateApiResponse (Prelude.Maybe Cors)
createApiResponse_corsConfiguration = Lens.lens (\CreateApiResponse' {corsConfiguration} -> corsConfiguration) (\s@CreateApiResponse' {} a -> s {corsConfiguration = a} :: CreateApiResponse)

-- | The timestamp when the API was created.
createApiResponse_createdDate :: Lens.Lens' CreateApiResponse (Prelude.Maybe Prelude.UTCTime)
createApiResponse_createdDate = Lens.lens (\CreateApiResponse' {createdDate} -> createdDate) (\s@CreateApiResponse' {} a -> s {createdDate = a} :: CreateApiResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the API.
createApiResponse_description :: Lens.Lens' CreateApiResponse (Prelude.Maybe Prelude.Text)
createApiResponse_description = Lens.lens (\CreateApiResponse' {description} -> description) (\s@CreateApiResponse' {} a -> s {description = a} :: CreateApiResponse)

-- | Specifies whether clients can invoke your API by using the default
-- execute-api endpoint. By default, clients can invoke your API with the
-- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
-- To require that clients use a custom domain name to invoke your API,
-- disable the default endpoint.
createApiResponse_disableExecuteApiEndpoint :: Lens.Lens' CreateApiResponse (Prelude.Maybe Prelude.Bool)
createApiResponse_disableExecuteApiEndpoint = Lens.lens (\CreateApiResponse' {disableExecuteApiEndpoint} -> disableExecuteApiEndpoint) (\s@CreateApiResponse' {} a -> s {disableExecuteApiEndpoint = a} :: CreateApiResponse)

-- | Avoid validating models when creating a deployment. Supported only for
-- WebSocket APIs.
createApiResponse_disableSchemaValidation :: Lens.Lens' CreateApiResponse (Prelude.Maybe Prelude.Bool)
createApiResponse_disableSchemaValidation = Lens.lens (\CreateApiResponse' {disableSchemaValidation} -> disableSchemaValidation) (\s@CreateApiResponse' {} a -> s {disableSchemaValidation = a} :: CreateApiResponse)

-- | The validation information during API import. This may include
-- particular properties of your OpenAPI definition which are ignored
-- during import. Supported only for HTTP APIs.
createApiResponse_importInfo :: Lens.Lens' CreateApiResponse (Prelude.Maybe [Prelude.Text])
createApiResponse_importInfo = Lens.lens (\CreateApiResponse' {importInfo} -> importInfo) (\s@CreateApiResponse' {} a -> s {importInfo = a} :: CreateApiResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the API.
createApiResponse_name :: Lens.Lens' CreateApiResponse (Prelude.Maybe Prelude.Text)
createApiResponse_name = Lens.lens (\CreateApiResponse' {name} -> name) (\s@CreateApiResponse' {} a -> s {name = a} :: CreateApiResponse)

-- | The API protocol.
createApiResponse_protocolType :: Lens.Lens' CreateApiResponse (Prelude.Maybe ProtocolType)
createApiResponse_protocolType = Lens.lens (\CreateApiResponse' {protocolType} -> protocolType) (\s@CreateApiResponse' {} a -> s {protocolType = a} :: CreateApiResponse)

-- | The route selection expression for the API. For HTTP APIs, the
-- routeSelectionExpression must be ${request.method} ${request.path}. If
-- not provided, this will be the default for HTTP APIs. This property is
-- required for WebSocket APIs.
createApiResponse_routeSelectionExpression :: Lens.Lens' CreateApiResponse (Prelude.Maybe Prelude.Text)
createApiResponse_routeSelectionExpression = Lens.lens (\CreateApiResponse' {routeSelectionExpression} -> routeSelectionExpression) (\s@CreateApiResponse' {} a -> s {routeSelectionExpression = a} :: CreateApiResponse)

-- | A collection of tags associated with the API.
createApiResponse_tags :: Lens.Lens' CreateApiResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createApiResponse_tags = Lens.lens (\CreateApiResponse' {tags} -> tags) (\s@CreateApiResponse' {} a -> s {tags = a} :: CreateApiResponse) Prelude.. Lens.mapping Lens.coerced

-- | A version identifier for the API.
createApiResponse_version :: Lens.Lens' CreateApiResponse (Prelude.Maybe Prelude.Text)
createApiResponse_version = Lens.lens (\CreateApiResponse' {version} -> version) (\s@CreateApiResponse' {} a -> s {version = a} :: CreateApiResponse)

-- | The warning messages reported when failonwarnings is turned on during
-- API import.
createApiResponse_warnings :: Lens.Lens' CreateApiResponse (Prelude.Maybe [Prelude.Text])
createApiResponse_warnings = Lens.lens (\CreateApiResponse' {warnings} -> warnings) (\s@CreateApiResponse' {} a -> s {warnings = a} :: CreateApiResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createApiResponse_httpStatus :: Lens.Lens' CreateApiResponse Prelude.Int
createApiResponse_httpStatus = Lens.lens (\CreateApiResponse' {httpStatus} -> httpStatus) (\s@CreateApiResponse' {} a -> s {httpStatus = a} :: CreateApiResponse)

instance Prelude.NFData CreateApiResponse where
  rnf CreateApiResponse' {..} =
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
