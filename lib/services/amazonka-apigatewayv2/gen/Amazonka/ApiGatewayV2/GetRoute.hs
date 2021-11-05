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
-- Module      : Amazonka.ApiGatewayV2.GetRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a Route.
module Amazonka.ApiGatewayV2.GetRoute
  ( -- * Creating a Request
    GetRoute (..),
    newGetRoute,

    -- * Request Lenses
    getRoute_apiId,
    getRoute_routeId,

    -- * Destructuring the Response
    GetRouteResponse' (..),
    newGetRouteResponse',

    -- * Response Lenses
    getRouteResponse'_authorizationScopes,
    getRouteResponse'_modelSelectionExpression,
    getRouteResponse'_requestModels,
    getRouteResponse'_routeResponseSelectionExpression,
    getRouteResponse'_requestParameters,
    getRouteResponse'_routeId,
    getRouteResponse'_authorizerId,
    getRouteResponse'_operationName,
    getRouteResponse'_apiGatewayManaged,
    getRouteResponse'_authorizationType,
    getRouteResponse'_apiKeyRequired,
    getRouteResponse'_routeKey,
    getRouteResponse'_target,
    getRouteResponse'_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRoute' smart constructor.
data GetRoute = GetRoute'
  { -- | The API identifier.
    apiId :: Prelude.Text,
    -- | The route ID.
    routeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'getRoute_apiId' - The API identifier.
--
-- 'routeId', 'getRoute_routeId' - The route ID.
newGetRoute ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'routeId'
  Prelude.Text ->
  GetRoute
newGetRoute pApiId_ pRouteId_ =
  GetRoute' {apiId = pApiId_, routeId = pRouteId_}

-- | The API identifier.
getRoute_apiId :: Lens.Lens' GetRoute Prelude.Text
getRoute_apiId = Lens.lens (\GetRoute' {apiId} -> apiId) (\s@GetRoute' {} a -> s {apiId = a} :: GetRoute)

-- | The route ID.
getRoute_routeId :: Lens.Lens' GetRoute Prelude.Text
getRoute_routeId = Lens.lens (\GetRoute' {routeId} -> routeId) (\s@GetRoute' {} a -> s {routeId = a} :: GetRoute)

instance Core.AWSRequest GetRoute where
  type AWSResponse GetRoute = GetRouteResponse'
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRouteResponse''
            Prelude.<$> ( x Core..?> "authorizationScopes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "modelSelectionExpression")
            Prelude.<*> (x Core..?> "requestModels" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "routeResponseSelectionExpression")
            Prelude.<*> ( x Core..?> "requestParameters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "routeId")
            Prelude.<*> (x Core..?> "authorizerId")
            Prelude.<*> (x Core..?> "operationName")
            Prelude.<*> (x Core..?> "apiGatewayManaged")
            Prelude.<*> (x Core..?> "authorizationType")
            Prelude.<*> (x Core..?> "apiKeyRequired")
            Prelude.<*> (x Core..?> "routeKey")
            Prelude.<*> (x Core..?> "target")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRoute

instance Prelude.NFData GetRoute

instance Core.ToHeaders GetRoute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetRoute where
  toPath GetRoute' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Core.toBS apiId,
        "/routes/",
        Core.toBS routeId
      ]

instance Core.ToQuery GetRoute where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRouteResponse'' smart constructor.
data GetRouteResponse' = GetRouteResponse''
  { -- | A list of authorization scopes configured on a route. The scopes are
    -- used with a JWT authorizer to authorize the method invocation. The
    -- authorization works by matching the route scopes against the scopes
    -- parsed from the access token in the incoming request. The method
    -- invocation is authorized if any route scope matches a claimed scope in
    -- the access token. Otherwise, the invocation is not authorized. When the
    -- route scope is configured, the client must provide an access token
    -- instead of an identity token for authorization purposes.
    authorizationScopes :: Prelude.Maybe [Prelude.Text],
    -- | The model selection expression for the route. Supported only for
    -- WebSocket APIs.
    modelSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | The request models for the route. Supported only for WebSocket APIs.
    requestModels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The route response selection expression for the route. Supported only
    -- for WebSocket APIs.
    routeResponseSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | The request parameters for the route. Supported only for WebSocket APIs.
    requestParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text ParameterConstraints),
    -- | The route ID.
    routeId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Authorizer resource to be associated with this
    -- route. The authorizer identifier is generated by API Gateway when you
    -- created the authorizer.
    authorizerId :: Prelude.Maybe Prelude.Text,
    -- | The operation name for the route.
    operationName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether a route is managed by API Gateway. If you created an
    -- API using quick create, the $default route is managed by API Gateway.
    -- You can\'t modify the $default route key.
    apiGatewayManaged :: Prelude.Maybe Prelude.Bool,
    -- | The authorization type for the route. For WebSocket APIs, valid values
    -- are NONE for open access, AWS_IAM for using AWS IAM permissions, and
    -- CUSTOM for using a Lambda authorizer For HTTP APIs, valid values are
    -- NONE for open access, JWT for using JSON Web Tokens, AWS_IAM for using
    -- AWS IAM permissions, and CUSTOM for using a Lambda authorizer.
    authorizationType :: Prelude.Maybe AuthorizationType,
    -- | Specifies whether an API key is required for this route. Supported only
    -- for WebSocket APIs.
    apiKeyRequired :: Prelude.Maybe Prelude.Bool,
    -- | The route key for the route.
    routeKey :: Prelude.Maybe Prelude.Text,
    -- | The target for the route.
    target :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRouteResponse'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizationScopes', 'getRouteResponse'_authorizationScopes' - A list of authorization scopes configured on a route. The scopes are
-- used with a JWT authorizer to authorize the method invocation. The
-- authorization works by matching the route scopes against the scopes
-- parsed from the access token in the incoming request. The method
-- invocation is authorized if any route scope matches a claimed scope in
-- the access token. Otherwise, the invocation is not authorized. When the
-- route scope is configured, the client must provide an access token
-- instead of an identity token for authorization purposes.
--
-- 'modelSelectionExpression', 'getRouteResponse'_modelSelectionExpression' - The model selection expression for the route. Supported only for
-- WebSocket APIs.
--
-- 'requestModels', 'getRouteResponse'_requestModels' - The request models for the route. Supported only for WebSocket APIs.
--
-- 'routeResponseSelectionExpression', 'getRouteResponse'_routeResponseSelectionExpression' - The route response selection expression for the route. Supported only
-- for WebSocket APIs.
--
-- 'requestParameters', 'getRouteResponse'_requestParameters' - The request parameters for the route. Supported only for WebSocket APIs.
--
-- 'routeId', 'getRouteResponse'_routeId' - The route ID.
--
-- 'authorizerId', 'getRouteResponse'_authorizerId' - The identifier of the Authorizer resource to be associated with this
-- route. The authorizer identifier is generated by API Gateway when you
-- created the authorizer.
--
-- 'operationName', 'getRouteResponse'_operationName' - The operation name for the route.
--
-- 'apiGatewayManaged', 'getRouteResponse'_apiGatewayManaged' - Specifies whether a route is managed by API Gateway. If you created an
-- API using quick create, the $default route is managed by API Gateway.
-- You can\'t modify the $default route key.
--
-- 'authorizationType', 'getRouteResponse'_authorizationType' - The authorization type for the route. For WebSocket APIs, valid values
-- are NONE for open access, AWS_IAM for using AWS IAM permissions, and
-- CUSTOM for using a Lambda authorizer For HTTP APIs, valid values are
-- NONE for open access, JWT for using JSON Web Tokens, AWS_IAM for using
-- AWS IAM permissions, and CUSTOM for using a Lambda authorizer.
--
-- 'apiKeyRequired', 'getRouteResponse'_apiKeyRequired' - Specifies whether an API key is required for this route. Supported only
-- for WebSocket APIs.
--
-- 'routeKey', 'getRouteResponse'_routeKey' - The route key for the route.
--
-- 'target', 'getRouteResponse'_target' - The target for the route.
--
-- 'httpStatus', 'getRouteResponse'_httpStatus' - The response's http status code.
newGetRouteResponse' ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRouteResponse'
newGetRouteResponse' pHttpStatus_ =
  GetRouteResponse''
    { authorizationScopes =
        Prelude.Nothing,
      modelSelectionExpression = Prelude.Nothing,
      requestModels = Prelude.Nothing,
      routeResponseSelectionExpression = Prelude.Nothing,
      requestParameters = Prelude.Nothing,
      routeId = Prelude.Nothing,
      authorizerId = Prelude.Nothing,
      operationName = Prelude.Nothing,
      apiGatewayManaged = Prelude.Nothing,
      authorizationType = Prelude.Nothing,
      apiKeyRequired = Prelude.Nothing,
      routeKey = Prelude.Nothing,
      target = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of authorization scopes configured on a route. The scopes are
-- used with a JWT authorizer to authorize the method invocation. The
-- authorization works by matching the route scopes against the scopes
-- parsed from the access token in the incoming request. The method
-- invocation is authorized if any route scope matches a claimed scope in
-- the access token. Otherwise, the invocation is not authorized. When the
-- route scope is configured, the client must provide an access token
-- instead of an identity token for authorization purposes.
getRouteResponse'_authorizationScopes :: Lens.Lens' GetRouteResponse' (Prelude.Maybe [Prelude.Text])
getRouteResponse'_authorizationScopes = Lens.lens (\GetRouteResponse'' {authorizationScopes} -> authorizationScopes) (\s@GetRouteResponse'' {} a -> s {authorizationScopes = a} :: GetRouteResponse') Prelude.. Lens.mapping Lens.coerced

-- | The model selection expression for the route. Supported only for
-- WebSocket APIs.
getRouteResponse'_modelSelectionExpression :: Lens.Lens' GetRouteResponse' (Prelude.Maybe Prelude.Text)
getRouteResponse'_modelSelectionExpression = Lens.lens (\GetRouteResponse'' {modelSelectionExpression} -> modelSelectionExpression) (\s@GetRouteResponse'' {} a -> s {modelSelectionExpression = a} :: GetRouteResponse')

-- | The request models for the route. Supported only for WebSocket APIs.
getRouteResponse'_requestModels :: Lens.Lens' GetRouteResponse' (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getRouteResponse'_requestModels = Lens.lens (\GetRouteResponse'' {requestModels} -> requestModels) (\s@GetRouteResponse'' {} a -> s {requestModels = a} :: GetRouteResponse') Prelude.. Lens.mapping Lens.coerced

-- | The route response selection expression for the route. Supported only
-- for WebSocket APIs.
getRouteResponse'_routeResponseSelectionExpression :: Lens.Lens' GetRouteResponse' (Prelude.Maybe Prelude.Text)
getRouteResponse'_routeResponseSelectionExpression = Lens.lens (\GetRouteResponse'' {routeResponseSelectionExpression} -> routeResponseSelectionExpression) (\s@GetRouteResponse'' {} a -> s {routeResponseSelectionExpression = a} :: GetRouteResponse')

-- | The request parameters for the route. Supported only for WebSocket APIs.
getRouteResponse'_requestParameters :: Lens.Lens' GetRouteResponse' (Prelude.Maybe (Prelude.HashMap Prelude.Text ParameterConstraints))
getRouteResponse'_requestParameters = Lens.lens (\GetRouteResponse'' {requestParameters} -> requestParameters) (\s@GetRouteResponse'' {} a -> s {requestParameters = a} :: GetRouteResponse') Prelude.. Lens.mapping Lens.coerced

-- | The route ID.
getRouteResponse'_routeId :: Lens.Lens' GetRouteResponse' (Prelude.Maybe Prelude.Text)
getRouteResponse'_routeId = Lens.lens (\GetRouteResponse'' {routeId} -> routeId) (\s@GetRouteResponse'' {} a -> s {routeId = a} :: GetRouteResponse')

-- | The identifier of the Authorizer resource to be associated with this
-- route. The authorizer identifier is generated by API Gateway when you
-- created the authorizer.
getRouteResponse'_authorizerId :: Lens.Lens' GetRouteResponse' (Prelude.Maybe Prelude.Text)
getRouteResponse'_authorizerId = Lens.lens (\GetRouteResponse'' {authorizerId} -> authorizerId) (\s@GetRouteResponse'' {} a -> s {authorizerId = a} :: GetRouteResponse')

-- | The operation name for the route.
getRouteResponse'_operationName :: Lens.Lens' GetRouteResponse' (Prelude.Maybe Prelude.Text)
getRouteResponse'_operationName = Lens.lens (\GetRouteResponse'' {operationName} -> operationName) (\s@GetRouteResponse'' {} a -> s {operationName = a} :: GetRouteResponse')

-- | Specifies whether a route is managed by API Gateway. If you created an
-- API using quick create, the $default route is managed by API Gateway.
-- You can\'t modify the $default route key.
getRouteResponse'_apiGatewayManaged :: Lens.Lens' GetRouteResponse' (Prelude.Maybe Prelude.Bool)
getRouteResponse'_apiGatewayManaged = Lens.lens (\GetRouteResponse'' {apiGatewayManaged} -> apiGatewayManaged) (\s@GetRouteResponse'' {} a -> s {apiGatewayManaged = a} :: GetRouteResponse')

-- | The authorization type for the route. For WebSocket APIs, valid values
-- are NONE for open access, AWS_IAM for using AWS IAM permissions, and
-- CUSTOM for using a Lambda authorizer For HTTP APIs, valid values are
-- NONE for open access, JWT for using JSON Web Tokens, AWS_IAM for using
-- AWS IAM permissions, and CUSTOM for using a Lambda authorizer.
getRouteResponse'_authorizationType :: Lens.Lens' GetRouteResponse' (Prelude.Maybe AuthorizationType)
getRouteResponse'_authorizationType = Lens.lens (\GetRouteResponse'' {authorizationType} -> authorizationType) (\s@GetRouteResponse'' {} a -> s {authorizationType = a} :: GetRouteResponse')

-- | Specifies whether an API key is required for this route. Supported only
-- for WebSocket APIs.
getRouteResponse'_apiKeyRequired :: Lens.Lens' GetRouteResponse' (Prelude.Maybe Prelude.Bool)
getRouteResponse'_apiKeyRequired = Lens.lens (\GetRouteResponse'' {apiKeyRequired} -> apiKeyRequired) (\s@GetRouteResponse'' {} a -> s {apiKeyRequired = a} :: GetRouteResponse')

-- | The route key for the route.
getRouteResponse'_routeKey :: Lens.Lens' GetRouteResponse' (Prelude.Maybe Prelude.Text)
getRouteResponse'_routeKey = Lens.lens (\GetRouteResponse'' {routeKey} -> routeKey) (\s@GetRouteResponse'' {} a -> s {routeKey = a} :: GetRouteResponse')

-- | The target for the route.
getRouteResponse'_target :: Lens.Lens' GetRouteResponse' (Prelude.Maybe Prelude.Text)
getRouteResponse'_target = Lens.lens (\GetRouteResponse'' {target} -> target) (\s@GetRouteResponse'' {} a -> s {target = a} :: GetRouteResponse')

-- | The response's http status code.
getRouteResponse'_httpStatus :: Lens.Lens' GetRouteResponse' Prelude.Int
getRouteResponse'_httpStatus = Lens.lens (\GetRouteResponse'' {httpStatus} -> httpStatus) (\s@GetRouteResponse'' {} a -> s {httpStatus = a} :: GetRouteResponse')

instance Prelude.NFData GetRouteResponse'
