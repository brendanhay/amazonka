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
-- Module      : Amazonka.APIGateway.Types.Method
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.Method where

import Amazonka.APIGateway.Types.Integration
import Amazonka.APIGateway.Types.MethodResponse
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a client-facing interface by which the client calls the API
-- to access back-end resources. A Method resource is integrated with an
-- Integration resource. Both consist of a request and one or more
-- responses. The method request takes the client input that is passed to
-- the back end through the integration request. A method response returns
-- the output from the back end to the client through an integration
-- response. A method request is embodied in a Method resource, whereas an
-- integration request is embodied in an Integration resource. On the other
-- hand, a method response is represented by a MethodResponse resource,
-- whereas an integration response is represented by an IntegrationResponse
-- resource.
--
-- /See:/ 'newMethod' smart constructor.
data Method = Method'
  { -- | A boolean flag specifying whether a valid ApiKey is required to invoke
    -- this method.
    apiKeyRequired :: Prelude.Maybe Prelude.Bool,
    -- | A list of authorization scopes configured on the method. The scopes are
    -- used with a @COGNITO_USER_POOLS@ authorizer to authorize the method
    -- invocation. The authorization works by matching the method scopes
    -- against the scopes parsed from the access token in the incoming request.
    -- The method invocation is authorized if any method scopes matches a
    -- claimed scope in the access token. Otherwise, the invocation is not
    -- authorized. When the method scope is configured, the client must provide
    -- an access token instead of an identity token for authorization purposes.
    authorizationScopes :: Prelude.Maybe [Prelude.Text],
    -- | The method\'s authorization type. Valid values are @NONE@ for open
    -- access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a
    -- custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user
    -- pool.
    authorizationType :: Prelude.Maybe Prelude.Text,
    -- | The identifier of an Authorizer to use on this method. The
    -- @authorizationType@ must be @CUSTOM@.
    authorizerId :: Prelude.Maybe Prelude.Text,
    -- | The method\'s HTTP verb.
    httpMethod :: Prelude.Maybe Prelude.Text,
    -- | Gets the method\'s integration responsible for passing the
    -- client-submitted request to the back end and performing necessary
    -- transformations to make the request compliant with the back end.
    methodIntegration :: Prelude.Maybe Integration,
    -- | Gets a method response associated with a given HTTP status code.
    methodResponses :: Prelude.Maybe (Prelude.HashMap Prelude.Text MethodResponse),
    -- | A human-friendly operation identifier for the method. For example, you
    -- can assign the @operationName@ of @ListPets@ for the @GET \/pets@ method
    -- in the @PetStore@ example.
    operationName :: Prelude.Maybe Prelude.Text,
    -- | A key-value map specifying data schemas, represented by Model resources,
    -- (as the mapped value) of the request payloads of given content types (as
    -- the mapping key).
    requestModels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A key-value map defining required or optional method request parameters
    -- that can be accepted by API Gateway. A key is a method request parameter
    -- name matching the pattern of @method.request.{location}.{name}@, where
    -- @location@ is @querystring@, @path@, or @header@ and @name@ is a valid
    -- and unique parameter name. The value associated with the key is a
    -- Boolean flag indicating whether the parameter is required (@true@) or
    -- optional (@false@). The method request parameter names defined here are
    -- available in Integration to be mapped to integration request parameters
    -- or templates.
    requestParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Bool),
    -- | The identifier of a RequestValidator for request validation.
    requestValidatorId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Method' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiKeyRequired', 'method_apiKeyRequired' - A boolean flag specifying whether a valid ApiKey is required to invoke
-- this method.
--
-- 'authorizationScopes', 'method_authorizationScopes' - A list of authorization scopes configured on the method. The scopes are
-- used with a @COGNITO_USER_POOLS@ authorizer to authorize the method
-- invocation. The authorization works by matching the method scopes
-- against the scopes parsed from the access token in the incoming request.
-- The method invocation is authorized if any method scopes matches a
-- claimed scope in the access token. Otherwise, the invocation is not
-- authorized. When the method scope is configured, the client must provide
-- an access token instead of an identity token for authorization purposes.
--
-- 'authorizationType', 'method_authorizationType' - The method\'s authorization type. Valid values are @NONE@ for open
-- access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a
-- custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user
-- pool.
--
-- 'authorizerId', 'method_authorizerId' - The identifier of an Authorizer to use on this method. The
-- @authorizationType@ must be @CUSTOM@.
--
-- 'httpMethod', 'method_httpMethod' - The method\'s HTTP verb.
--
-- 'methodIntegration', 'method_methodIntegration' - Gets the method\'s integration responsible for passing the
-- client-submitted request to the back end and performing necessary
-- transformations to make the request compliant with the back end.
--
-- 'methodResponses', 'method_methodResponses' - Gets a method response associated with a given HTTP status code.
--
-- 'operationName', 'method_operationName' - A human-friendly operation identifier for the method. For example, you
-- can assign the @operationName@ of @ListPets@ for the @GET \/pets@ method
-- in the @PetStore@ example.
--
-- 'requestModels', 'method_requestModels' - A key-value map specifying data schemas, represented by Model resources,
-- (as the mapped value) of the request payloads of given content types (as
-- the mapping key).
--
-- 'requestParameters', 'method_requestParameters' - A key-value map defining required or optional method request parameters
-- that can be accepted by API Gateway. A key is a method request parameter
-- name matching the pattern of @method.request.{location}.{name}@, where
-- @location@ is @querystring@, @path@, or @header@ and @name@ is a valid
-- and unique parameter name. The value associated with the key is a
-- Boolean flag indicating whether the parameter is required (@true@) or
-- optional (@false@). The method request parameter names defined here are
-- available in Integration to be mapped to integration request parameters
-- or templates.
--
-- 'requestValidatorId', 'method_requestValidatorId' - The identifier of a RequestValidator for request validation.
newMethod ::
  Method
newMethod =
  Method'
    { apiKeyRequired = Prelude.Nothing,
      authorizationScopes = Prelude.Nothing,
      authorizationType = Prelude.Nothing,
      authorizerId = Prelude.Nothing,
      httpMethod = Prelude.Nothing,
      methodIntegration = Prelude.Nothing,
      methodResponses = Prelude.Nothing,
      operationName = Prelude.Nothing,
      requestModels = Prelude.Nothing,
      requestParameters = Prelude.Nothing,
      requestValidatorId = Prelude.Nothing
    }

-- | A boolean flag specifying whether a valid ApiKey is required to invoke
-- this method.
method_apiKeyRequired :: Lens.Lens' Method (Prelude.Maybe Prelude.Bool)
method_apiKeyRequired = Lens.lens (\Method' {apiKeyRequired} -> apiKeyRequired) (\s@Method' {} a -> s {apiKeyRequired = a} :: Method)

-- | A list of authorization scopes configured on the method. The scopes are
-- used with a @COGNITO_USER_POOLS@ authorizer to authorize the method
-- invocation. The authorization works by matching the method scopes
-- against the scopes parsed from the access token in the incoming request.
-- The method invocation is authorized if any method scopes matches a
-- claimed scope in the access token. Otherwise, the invocation is not
-- authorized. When the method scope is configured, the client must provide
-- an access token instead of an identity token for authorization purposes.
method_authorizationScopes :: Lens.Lens' Method (Prelude.Maybe [Prelude.Text])
method_authorizationScopes = Lens.lens (\Method' {authorizationScopes} -> authorizationScopes) (\s@Method' {} a -> s {authorizationScopes = a} :: Method) Prelude.. Lens.mapping Lens.coerced

-- | The method\'s authorization type. Valid values are @NONE@ for open
-- access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a
-- custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user
-- pool.
method_authorizationType :: Lens.Lens' Method (Prelude.Maybe Prelude.Text)
method_authorizationType = Lens.lens (\Method' {authorizationType} -> authorizationType) (\s@Method' {} a -> s {authorizationType = a} :: Method)

-- | The identifier of an Authorizer to use on this method. The
-- @authorizationType@ must be @CUSTOM@.
method_authorizerId :: Lens.Lens' Method (Prelude.Maybe Prelude.Text)
method_authorizerId = Lens.lens (\Method' {authorizerId} -> authorizerId) (\s@Method' {} a -> s {authorizerId = a} :: Method)

-- | The method\'s HTTP verb.
method_httpMethod :: Lens.Lens' Method (Prelude.Maybe Prelude.Text)
method_httpMethod = Lens.lens (\Method' {httpMethod} -> httpMethod) (\s@Method' {} a -> s {httpMethod = a} :: Method)

-- | Gets the method\'s integration responsible for passing the
-- client-submitted request to the back end and performing necessary
-- transformations to make the request compliant with the back end.
method_methodIntegration :: Lens.Lens' Method (Prelude.Maybe Integration)
method_methodIntegration = Lens.lens (\Method' {methodIntegration} -> methodIntegration) (\s@Method' {} a -> s {methodIntegration = a} :: Method)

-- | Gets a method response associated with a given HTTP status code.
method_methodResponses :: Lens.Lens' Method (Prelude.Maybe (Prelude.HashMap Prelude.Text MethodResponse))
method_methodResponses = Lens.lens (\Method' {methodResponses} -> methodResponses) (\s@Method' {} a -> s {methodResponses = a} :: Method) Prelude.. Lens.mapping Lens.coerced

-- | A human-friendly operation identifier for the method. For example, you
-- can assign the @operationName@ of @ListPets@ for the @GET \/pets@ method
-- in the @PetStore@ example.
method_operationName :: Lens.Lens' Method (Prelude.Maybe Prelude.Text)
method_operationName = Lens.lens (\Method' {operationName} -> operationName) (\s@Method' {} a -> s {operationName = a} :: Method)

-- | A key-value map specifying data schemas, represented by Model resources,
-- (as the mapped value) of the request payloads of given content types (as
-- the mapping key).
method_requestModels :: Lens.Lens' Method (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
method_requestModels = Lens.lens (\Method' {requestModels} -> requestModels) (\s@Method' {} a -> s {requestModels = a} :: Method) Prelude.. Lens.mapping Lens.coerced

-- | A key-value map defining required or optional method request parameters
-- that can be accepted by API Gateway. A key is a method request parameter
-- name matching the pattern of @method.request.{location}.{name}@, where
-- @location@ is @querystring@, @path@, or @header@ and @name@ is a valid
-- and unique parameter name. The value associated with the key is a
-- Boolean flag indicating whether the parameter is required (@true@) or
-- optional (@false@). The method request parameter names defined here are
-- available in Integration to be mapped to integration request parameters
-- or templates.
method_requestParameters :: Lens.Lens' Method (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Bool))
method_requestParameters = Lens.lens (\Method' {requestParameters} -> requestParameters) (\s@Method' {} a -> s {requestParameters = a} :: Method) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of a RequestValidator for request validation.
method_requestValidatorId :: Lens.Lens' Method (Prelude.Maybe Prelude.Text)
method_requestValidatorId = Lens.lens (\Method' {requestValidatorId} -> requestValidatorId) (\s@Method' {} a -> s {requestValidatorId = a} :: Method)

instance Data.FromJSON Method where
  parseJSON =
    Data.withObject
      "Method"
      ( \x ->
          Method'
            Prelude.<$> (x Data..:? "apiKeyRequired")
            Prelude.<*> ( x
                            Data..:? "authorizationScopes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "authorizationType")
            Prelude.<*> (x Data..:? "authorizerId")
            Prelude.<*> (x Data..:? "httpMethod")
            Prelude.<*> (x Data..:? "methodIntegration")
            Prelude.<*> ( x
                            Data..:? "methodResponses"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "operationName")
            Prelude.<*> (x Data..:? "requestModels" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "requestParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "requestValidatorId")
      )

instance Prelude.Hashable Method where
  hashWithSalt _salt Method' {..} =
    _salt
      `Prelude.hashWithSalt` apiKeyRequired
      `Prelude.hashWithSalt` authorizationScopes
      `Prelude.hashWithSalt` authorizationType
      `Prelude.hashWithSalt` authorizerId
      `Prelude.hashWithSalt` httpMethod
      `Prelude.hashWithSalt` methodIntegration
      `Prelude.hashWithSalt` methodResponses
      `Prelude.hashWithSalt` operationName
      `Prelude.hashWithSalt` requestModels
      `Prelude.hashWithSalt` requestParameters
      `Prelude.hashWithSalt` requestValidatorId

instance Prelude.NFData Method where
  rnf Method' {..} =
    Prelude.rnf apiKeyRequired
      `Prelude.seq` Prelude.rnf authorizationScopes
      `Prelude.seq` Prelude.rnf authorizationType
      `Prelude.seq` Prelude.rnf authorizerId
      `Prelude.seq` Prelude.rnf httpMethod
      `Prelude.seq` Prelude.rnf methodIntegration
      `Prelude.seq` Prelude.rnf methodResponses
      `Prelude.seq` Prelude.rnf operationName
      `Prelude.seq` Prelude.rnf requestModels
      `Prelude.seq` Prelude.rnf requestParameters
      `Prelude.seq` Prelude.rnf requestValidatorId
