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
-- Module      : Amazonka.ApiGatewayV2.Types.Route
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.Route where

import Amazonka.ApiGatewayV2.Types.AuthorizationType
import Amazonka.ApiGatewayV2.Types.ParameterConstraints
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a route.
--
-- /See:/ 'newRoute' smart constructor.
data Route = Route'
  { -- | Specifies whether a route is managed by API Gateway. If you created an
    -- API using quick create, the $default route is managed by API Gateway.
    -- You can\'t modify the $default route key.
    apiGatewayManaged :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether an API key is required for this route. Supported only
    -- for WebSocket APIs.
    apiKeyRequired :: Prelude.Maybe Prelude.Bool,
    -- | A list of authorization scopes configured on a route. The scopes are
    -- used with a JWT authorizer to authorize the method invocation. The
    -- authorization works by matching the route scopes against the scopes
    -- parsed from the access token in the incoming request. The method
    -- invocation is authorized if any route scope matches a claimed scope in
    -- the access token. Otherwise, the invocation is not authorized. When the
    -- route scope is configured, the client must provide an access token
    -- instead of an identity token for authorization purposes.
    authorizationScopes :: Prelude.Maybe [Prelude.Text],
    -- | The authorization type for the route. For WebSocket APIs, valid values
    -- are NONE for open access, AWS_IAM for using AWS IAM permissions, and
    -- CUSTOM for using a Lambda authorizer For HTTP APIs, valid values are
    -- NONE for open access, JWT for using JSON Web Tokens, AWS_IAM for using
    -- AWS IAM permissions, and CUSTOM for using a Lambda authorizer.
    authorizationType :: Prelude.Maybe AuthorizationType,
    -- | The identifier of the Authorizer resource to be associated with this
    -- route. The authorizer identifier is generated by API Gateway when you
    -- created the authorizer.
    authorizerId :: Prelude.Maybe Prelude.Text,
    -- | The model selection expression for the route. Supported only for
    -- WebSocket APIs.
    modelSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | The operation name for the route.
    operationName :: Prelude.Maybe Prelude.Text,
    -- | The request models for the route. Supported only for WebSocket APIs.
    requestModels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The request parameters for the route. Supported only for WebSocket APIs.
    requestParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text ParameterConstraints),
    -- | The route ID.
    routeId :: Prelude.Maybe Prelude.Text,
    -- | The route response selection expression for the route. Supported only
    -- for WebSocket APIs.
    routeResponseSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | The target for the route.
    target :: Prelude.Maybe Prelude.Text,
    -- | The route key for the route.
    routeKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Route' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiGatewayManaged', 'route_apiGatewayManaged' - Specifies whether a route is managed by API Gateway. If you created an
-- API using quick create, the $default route is managed by API Gateway.
-- You can\'t modify the $default route key.
--
-- 'apiKeyRequired', 'route_apiKeyRequired' - Specifies whether an API key is required for this route. Supported only
-- for WebSocket APIs.
--
-- 'authorizationScopes', 'route_authorizationScopes' - A list of authorization scopes configured on a route. The scopes are
-- used with a JWT authorizer to authorize the method invocation. The
-- authorization works by matching the route scopes against the scopes
-- parsed from the access token in the incoming request. The method
-- invocation is authorized if any route scope matches a claimed scope in
-- the access token. Otherwise, the invocation is not authorized. When the
-- route scope is configured, the client must provide an access token
-- instead of an identity token for authorization purposes.
--
-- 'authorizationType', 'route_authorizationType' - The authorization type for the route. For WebSocket APIs, valid values
-- are NONE for open access, AWS_IAM for using AWS IAM permissions, and
-- CUSTOM for using a Lambda authorizer For HTTP APIs, valid values are
-- NONE for open access, JWT for using JSON Web Tokens, AWS_IAM for using
-- AWS IAM permissions, and CUSTOM for using a Lambda authorizer.
--
-- 'authorizerId', 'route_authorizerId' - The identifier of the Authorizer resource to be associated with this
-- route. The authorizer identifier is generated by API Gateway when you
-- created the authorizer.
--
-- 'modelSelectionExpression', 'route_modelSelectionExpression' - The model selection expression for the route. Supported only for
-- WebSocket APIs.
--
-- 'operationName', 'route_operationName' - The operation name for the route.
--
-- 'requestModels', 'route_requestModels' - The request models for the route. Supported only for WebSocket APIs.
--
-- 'requestParameters', 'route_requestParameters' - The request parameters for the route. Supported only for WebSocket APIs.
--
-- 'routeId', 'route_routeId' - The route ID.
--
-- 'routeResponseSelectionExpression', 'route_routeResponseSelectionExpression' - The route response selection expression for the route. Supported only
-- for WebSocket APIs.
--
-- 'target', 'route_target' - The target for the route.
--
-- 'routeKey', 'route_routeKey' - The route key for the route.
newRoute ::
  -- | 'routeKey'
  Prelude.Text ->
  Route
newRoute pRouteKey_ =
  Route'
    { apiGatewayManaged = Prelude.Nothing,
      apiKeyRequired = Prelude.Nothing,
      authorizationScopes = Prelude.Nothing,
      authorizationType = Prelude.Nothing,
      authorizerId = Prelude.Nothing,
      modelSelectionExpression = Prelude.Nothing,
      operationName = Prelude.Nothing,
      requestModels = Prelude.Nothing,
      requestParameters = Prelude.Nothing,
      routeId = Prelude.Nothing,
      routeResponseSelectionExpression = Prelude.Nothing,
      target = Prelude.Nothing,
      routeKey = pRouteKey_
    }

-- | Specifies whether a route is managed by API Gateway. If you created an
-- API using quick create, the $default route is managed by API Gateway.
-- You can\'t modify the $default route key.
route_apiGatewayManaged :: Lens.Lens' Route (Prelude.Maybe Prelude.Bool)
route_apiGatewayManaged = Lens.lens (\Route' {apiGatewayManaged} -> apiGatewayManaged) (\s@Route' {} a -> s {apiGatewayManaged = a} :: Route)

-- | Specifies whether an API key is required for this route. Supported only
-- for WebSocket APIs.
route_apiKeyRequired :: Lens.Lens' Route (Prelude.Maybe Prelude.Bool)
route_apiKeyRequired = Lens.lens (\Route' {apiKeyRequired} -> apiKeyRequired) (\s@Route' {} a -> s {apiKeyRequired = a} :: Route)

-- | A list of authorization scopes configured on a route. The scopes are
-- used with a JWT authorizer to authorize the method invocation. The
-- authorization works by matching the route scopes against the scopes
-- parsed from the access token in the incoming request. The method
-- invocation is authorized if any route scope matches a claimed scope in
-- the access token. Otherwise, the invocation is not authorized. When the
-- route scope is configured, the client must provide an access token
-- instead of an identity token for authorization purposes.
route_authorizationScopes :: Lens.Lens' Route (Prelude.Maybe [Prelude.Text])
route_authorizationScopes = Lens.lens (\Route' {authorizationScopes} -> authorizationScopes) (\s@Route' {} a -> s {authorizationScopes = a} :: Route) Prelude.. Lens.mapping Lens.coerced

-- | The authorization type for the route. For WebSocket APIs, valid values
-- are NONE for open access, AWS_IAM for using AWS IAM permissions, and
-- CUSTOM for using a Lambda authorizer For HTTP APIs, valid values are
-- NONE for open access, JWT for using JSON Web Tokens, AWS_IAM for using
-- AWS IAM permissions, and CUSTOM for using a Lambda authorizer.
route_authorizationType :: Lens.Lens' Route (Prelude.Maybe AuthorizationType)
route_authorizationType = Lens.lens (\Route' {authorizationType} -> authorizationType) (\s@Route' {} a -> s {authorizationType = a} :: Route)

-- | The identifier of the Authorizer resource to be associated with this
-- route. The authorizer identifier is generated by API Gateway when you
-- created the authorizer.
route_authorizerId :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_authorizerId = Lens.lens (\Route' {authorizerId} -> authorizerId) (\s@Route' {} a -> s {authorizerId = a} :: Route)

-- | The model selection expression for the route. Supported only for
-- WebSocket APIs.
route_modelSelectionExpression :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_modelSelectionExpression = Lens.lens (\Route' {modelSelectionExpression} -> modelSelectionExpression) (\s@Route' {} a -> s {modelSelectionExpression = a} :: Route)

-- | The operation name for the route.
route_operationName :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_operationName = Lens.lens (\Route' {operationName} -> operationName) (\s@Route' {} a -> s {operationName = a} :: Route)

-- | The request models for the route. Supported only for WebSocket APIs.
route_requestModels :: Lens.Lens' Route (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
route_requestModels = Lens.lens (\Route' {requestModels} -> requestModels) (\s@Route' {} a -> s {requestModels = a} :: Route) Prelude.. Lens.mapping Lens.coerced

-- | The request parameters for the route. Supported only for WebSocket APIs.
route_requestParameters :: Lens.Lens' Route (Prelude.Maybe (Prelude.HashMap Prelude.Text ParameterConstraints))
route_requestParameters = Lens.lens (\Route' {requestParameters} -> requestParameters) (\s@Route' {} a -> s {requestParameters = a} :: Route) Prelude.. Lens.mapping Lens.coerced

-- | The route ID.
route_routeId :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_routeId = Lens.lens (\Route' {routeId} -> routeId) (\s@Route' {} a -> s {routeId = a} :: Route)

-- | The route response selection expression for the route. Supported only
-- for WebSocket APIs.
route_routeResponseSelectionExpression :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_routeResponseSelectionExpression = Lens.lens (\Route' {routeResponseSelectionExpression} -> routeResponseSelectionExpression) (\s@Route' {} a -> s {routeResponseSelectionExpression = a} :: Route)

-- | The target for the route.
route_target :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_target = Lens.lens (\Route' {target} -> target) (\s@Route' {} a -> s {target = a} :: Route)

-- | The route key for the route.
route_routeKey :: Lens.Lens' Route Prelude.Text
route_routeKey = Lens.lens (\Route' {routeKey} -> routeKey) (\s@Route' {} a -> s {routeKey = a} :: Route)

instance Data.FromJSON Route where
  parseJSON =
    Data.withObject
      "Route"
      ( \x ->
          Route'
            Prelude.<$> (x Data..:? "apiGatewayManaged")
            Prelude.<*> (x Data..:? "apiKeyRequired")
            Prelude.<*> ( x
                            Data..:? "authorizationScopes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "authorizationType")
            Prelude.<*> (x Data..:? "authorizerId")
            Prelude.<*> (x Data..:? "modelSelectionExpression")
            Prelude.<*> (x Data..:? "operationName")
            Prelude.<*> (x Data..:? "requestModels" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "requestParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "routeId")
            Prelude.<*> (x Data..:? "routeResponseSelectionExpression")
            Prelude.<*> (x Data..:? "target")
            Prelude.<*> (x Data..: "routeKey")
      )

instance Prelude.Hashable Route where
  hashWithSalt _salt Route' {..} =
    _salt
      `Prelude.hashWithSalt` apiGatewayManaged
      `Prelude.hashWithSalt` apiKeyRequired
      `Prelude.hashWithSalt` authorizationScopes
      `Prelude.hashWithSalt` authorizationType
      `Prelude.hashWithSalt` authorizerId
      `Prelude.hashWithSalt` modelSelectionExpression
      `Prelude.hashWithSalt` operationName
      `Prelude.hashWithSalt` requestModels
      `Prelude.hashWithSalt` requestParameters
      `Prelude.hashWithSalt` routeId
      `Prelude.hashWithSalt` routeResponseSelectionExpression
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` routeKey

instance Prelude.NFData Route where
  rnf Route' {..} =
    Prelude.rnf apiGatewayManaged `Prelude.seq`
      Prelude.rnf apiKeyRequired `Prelude.seq`
        Prelude.rnf authorizationScopes `Prelude.seq`
          Prelude.rnf authorizationType `Prelude.seq`
            Prelude.rnf authorizerId `Prelude.seq`
              Prelude.rnf modelSelectionExpression `Prelude.seq`
                Prelude.rnf operationName `Prelude.seq`
                  Prelude.rnf requestModels `Prelude.seq`
                    Prelude.rnf requestParameters `Prelude.seq`
                      Prelude.rnf routeId `Prelude.seq`
                        Prelude.rnf routeResponseSelectionExpression `Prelude.seq`
                          Prelude.rnf target `Prelude.seq`
                            Prelude.rnf routeKey
