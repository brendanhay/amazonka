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
-- Module      : Amazonka.APIGateway.PutIntegration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets up a method\'s integration.
module Amazonka.APIGateway.PutIntegration
  ( -- * Creating a Request
    PutIntegration (..),
    newPutIntegration,

    -- * Request Lenses
    putIntegration_cacheKeyParameters,
    putIntegration_cacheNamespace,
    putIntegration_connectionId,
    putIntegration_connectionType,
    putIntegration_contentHandling,
    putIntegration_credentials,
    putIntegration_integrationHttpMethod,
    putIntegration_passthroughBehavior,
    putIntegration_requestParameters,
    putIntegration_requestTemplates,
    putIntegration_timeoutInMillis,
    putIntegration_tlsConfig,
    putIntegration_uri,
    putIntegration_restApiId,
    putIntegration_resourceId,
    putIntegration_httpMethod,
    putIntegration_type,

    -- * Destructuring the Response
    Integration (..),
    newIntegration,

    -- * Response Lenses
    integration_cacheKeyParameters,
    integration_cacheNamespace,
    integration_connectionId,
    integration_connectionType,
    integration_contentHandling,
    integration_credentials,
    integration_httpMethod,
    integration_integrationResponses,
    integration_passthroughBehavior,
    integration_requestParameters,
    integration_requestTemplates,
    integration_timeoutInMillis,
    integration_tlsConfig,
    integration_type,
    integration_uri,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Sets up a method\'s integration.
--
-- /See:/ 'newPutIntegration' smart constructor.
data PutIntegration = PutIntegration'
  { -- | A list of request parameters whose values API Gateway caches. To be
    -- valid values for @cacheKeyParameters@, these parameters must also be
    -- specified for Method @requestParameters@.
    cacheKeyParameters :: Prelude.Maybe [Prelude.Text],
    -- | Specifies a group of related cached parameters. By default, API Gateway
    -- uses the resource ID as the @cacheNamespace@. You can specify the same
    -- @cacheNamespace@ across resources to return the same cached data for
    -- requests to different resources.
    cacheNamespace :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VpcLink used for the integration. Specify this value only
    -- if you specify @VPC_LINK@ as the connection type.
    connectionId :: Prelude.Maybe Prelude.Text,
    -- | The type of the network connection to the integration endpoint. The
    -- valid value is @INTERNET@ for connections through the public routable
    -- internet or @VPC_LINK@ for private connections between API Gateway and a
    -- network load balancer in a VPC. The default value is @INTERNET@.
    connectionType :: Prelude.Maybe ConnectionType,
    -- | Specifies how to handle request payload content type conversions.
    -- Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@, with the
    -- following behaviors:
    --
    -- If this property is not defined, the request payload will be passed
    -- through from the method request to integration request without
    -- modification, provided that the @passthroughBehavior@ is configured to
    -- support payload pass-through.
    contentHandling :: Prelude.Maybe ContentHandlingStrategy,
    -- | Specifies whether credentials are required for a put integration.
    credentials :: Prelude.Maybe Prelude.Text,
    -- | The HTTP method for the integration.
    integrationHttpMethod :: Prelude.Maybe Prelude.Text,
    -- | Specifies the pass-through behavior for incoming requests based on the
    -- Content-Type header in the request, and the available mapping templates
    -- specified as the @requestTemplates@ property on the Integration
    -- resource. There are three valid values: @WHEN_NO_MATCH@,
    -- @WHEN_NO_TEMPLATES@, and @NEVER@.
    passthroughBehavior :: Prelude.Maybe Prelude.Text,
    -- | A key-value map specifying request parameters that are passed from the
    -- method request to the back end. The key is an integration request
    -- parameter name and the associated value is a method request parameter
    -- value or static value that must be enclosed within single quotes and
    -- pre-encoded as required by the back end. The method request parameter
    -- value must match the pattern of @method.request.{location}.{name}@,
    -- where @location@ is @querystring@, @path@, or @header@ and @name@ must
    -- be a valid and unique method request parameter name.
    requestParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Represents a map of Velocity templates that are applied on the request
    -- payload based on the value of the Content-Type header sent by the
    -- client. The content type value is the key in this map, and the template
    -- (as a String) is the value.
    requestTemplates :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Custom timeout between 50 and 29,000 milliseconds. The default value is
    -- 29,000 milliseconds or 29 seconds.
    timeoutInMillis :: Prelude.Maybe Prelude.Int,
    tlsConfig :: Prelude.Maybe TlsConfig,
    -- | Specifies Uniform Resource Identifier (URI) of the integration endpoint.
    -- For HTTP or @HTTP_PROXY@ integrations, the URI must be a fully formed,
    -- encoded HTTP(S) URL according to the RFC-3986 specification, for either
    -- standard integration, where @connectionType@ is not @VPC_LINK@, or
    -- private integration, where @connectionType@ is @VPC_LINK@. For a private
    -- HTTP integration, the URI is not used for routing. For @AWS@ or
    -- @AWS_PROXY@ integrations, the URI is of the form
    -- @arn:aws:apigateway:{region}:{subdomain.service|service}:path|action\/{service_api@}.
    -- Here, {Region} is the API Gateway region (e.g., us-east-1); {service} is
    -- the name of the integrated Amazon Web Services service (e.g., s3); and
    -- {subdomain} is a designated subdomain supported by certain Amazon Web
    -- Services service for fast host-name lookup. action can be used for an
    -- Amazon Web Services service action-based API, using an
    -- Action={name}&{p1}={v1}&p2={v2}... query string. The ensuing
    -- {service_api} refers to a supported action {name} plus any required
    -- input parameters. Alternatively, path can be used for an Amazon Web
    -- Services service path-based API. The ensuing service_api refers to the
    -- path to an Amazon Web Services service resource, including the region of
    -- the integrated Amazon Web Services service, if applicable. For example,
    -- for integration with the S3 API of @GetObject@, the @uri@ can be either
    -- @arn:aws:apigateway:us-west-2:s3:action\/GetObject&Bucket={bucket}&Key={key}@
    -- or @arn:aws:apigateway:us-west-2:s3:path\/{bucket}\/{key}@.
    uri :: Prelude.Maybe Prelude.Text,
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | Specifies a put integration request\'s resource ID.
    resourceId :: Prelude.Text,
    -- | Specifies the HTTP method for the integration.
    httpMethod :: Prelude.Text,
    -- | Specifies a put integration input\'s type.
    type' :: IntegrationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheKeyParameters', 'putIntegration_cacheKeyParameters' - A list of request parameters whose values API Gateway caches. To be
-- valid values for @cacheKeyParameters@, these parameters must also be
-- specified for Method @requestParameters@.
--
-- 'cacheNamespace', 'putIntegration_cacheNamespace' - Specifies a group of related cached parameters. By default, API Gateway
-- uses the resource ID as the @cacheNamespace@. You can specify the same
-- @cacheNamespace@ across resources to return the same cached data for
-- requests to different resources.
--
-- 'connectionId', 'putIntegration_connectionId' - The ID of the VpcLink used for the integration. Specify this value only
-- if you specify @VPC_LINK@ as the connection type.
--
-- 'connectionType', 'putIntegration_connectionType' - The type of the network connection to the integration endpoint. The
-- valid value is @INTERNET@ for connections through the public routable
-- internet or @VPC_LINK@ for private connections between API Gateway and a
-- network load balancer in a VPC. The default value is @INTERNET@.
--
-- 'contentHandling', 'putIntegration_contentHandling' - Specifies how to handle request payload content type conversions.
-- Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@, with the
-- following behaviors:
--
-- If this property is not defined, the request payload will be passed
-- through from the method request to integration request without
-- modification, provided that the @passthroughBehavior@ is configured to
-- support payload pass-through.
--
-- 'credentials', 'putIntegration_credentials' - Specifies whether credentials are required for a put integration.
--
-- 'integrationHttpMethod', 'putIntegration_integrationHttpMethod' - The HTTP method for the integration.
--
-- 'passthroughBehavior', 'putIntegration_passthroughBehavior' - Specifies the pass-through behavior for incoming requests based on the
-- Content-Type header in the request, and the available mapping templates
-- specified as the @requestTemplates@ property on the Integration
-- resource. There are three valid values: @WHEN_NO_MATCH@,
-- @WHEN_NO_TEMPLATES@, and @NEVER@.
--
-- 'requestParameters', 'putIntegration_requestParameters' - A key-value map specifying request parameters that are passed from the
-- method request to the back end. The key is an integration request
-- parameter name and the associated value is a method request parameter
-- value or static value that must be enclosed within single quotes and
-- pre-encoded as required by the back end. The method request parameter
-- value must match the pattern of @method.request.{location}.{name}@,
-- where @location@ is @querystring@, @path@, or @header@ and @name@ must
-- be a valid and unique method request parameter name.
--
-- 'requestTemplates', 'putIntegration_requestTemplates' - Represents a map of Velocity templates that are applied on the request
-- payload based on the value of the Content-Type header sent by the
-- client. The content type value is the key in this map, and the template
-- (as a String) is the value.
--
-- 'timeoutInMillis', 'putIntegration_timeoutInMillis' - Custom timeout between 50 and 29,000 milliseconds. The default value is
-- 29,000 milliseconds or 29 seconds.
--
-- 'tlsConfig', 'putIntegration_tlsConfig' - Undocumented member.
--
-- 'uri', 'putIntegration_uri' - Specifies Uniform Resource Identifier (URI) of the integration endpoint.
-- For HTTP or @HTTP_PROXY@ integrations, the URI must be a fully formed,
-- encoded HTTP(S) URL according to the RFC-3986 specification, for either
-- standard integration, where @connectionType@ is not @VPC_LINK@, or
-- private integration, where @connectionType@ is @VPC_LINK@. For a private
-- HTTP integration, the URI is not used for routing. For @AWS@ or
-- @AWS_PROXY@ integrations, the URI is of the form
-- @arn:aws:apigateway:{region}:{subdomain.service|service}:path|action\/{service_api@}.
-- Here, {Region} is the API Gateway region (e.g., us-east-1); {service} is
-- the name of the integrated Amazon Web Services service (e.g., s3); and
-- {subdomain} is a designated subdomain supported by certain Amazon Web
-- Services service for fast host-name lookup. action can be used for an
-- Amazon Web Services service action-based API, using an
-- Action={name}&{p1}={v1}&p2={v2}... query string. The ensuing
-- {service_api} refers to a supported action {name} plus any required
-- input parameters. Alternatively, path can be used for an Amazon Web
-- Services service path-based API. The ensuing service_api refers to the
-- path to an Amazon Web Services service resource, including the region of
-- the integrated Amazon Web Services service, if applicable. For example,
-- for integration with the S3 API of @GetObject@, the @uri@ can be either
-- @arn:aws:apigateway:us-west-2:s3:action\/GetObject&Bucket={bucket}&Key={key}@
-- or @arn:aws:apigateway:us-west-2:s3:path\/{bucket}\/{key}@.
--
-- 'restApiId', 'putIntegration_restApiId' - The string identifier of the associated RestApi.
--
-- 'resourceId', 'putIntegration_resourceId' - Specifies a put integration request\'s resource ID.
--
-- 'httpMethod', 'putIntegration_httpMethod' - Specifies the HTTP method for the integration.
--
-- 'type'', 'putIntegration_type' - Specifies a put integration input\'s type.
newPutIntegration ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'httpMethod'
  Prelude.Text ->
  -- | 'type''
  IntegrationType ->
  PutIntegration
newPutIntegration
  pRestApiId_
  pResourceId_
  pHttpMethod_
  pType_ =
    PutIntegration'
      { cacheKeyParameters =
          Prelude.Nothing,
        cacheNamespace = Prelude.Nothing,
        connectionId = Prelude.Nothing,
        connectionType = Prelude.Nothing,
        contentHandling = Prelude.Nothing,
        credentials = Prelude.Nothing,
        integrationHttpMethod = Prelude.Nothing,
        passthroughBehavior = Prelude.Nothing,
        requestParameters = Prelude.Nothing,
        requestTemplates = Prelude.Nothing,
        timeoutInMillis = Prelude.Nothing,
        tlsConfig = Prelude.Nothing,
        uri = Prelude.Nothing,
        restApiId = pRestApiId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_,
        type' = pType_
      }

-- | A list of request parameters whose values API Gateway caches. To be
-- valid values for @cacheKeyParameters@, these parameters must also be
-- specified for Method @requestParameters@.
putIntegration_cacheKeyParameters :: Lens.Lens' PutIntegration (Prelude.Maybe [Prelude.Text])
putIntegration_cacheKeyParameters = Lens.lens (\PutIntegration' {cacheKeyParameters} -> cacheKeyParameters) (\s@PutIntegration' {} a -> s {cacheKeyParameters = a} :: PutIntegration) Prelude.. Lens.mapping Lens.coerced

-- | Specifies a group of related cached parameters. By default, API Gateway
-- uses the resource ID as the @cacheNamespace@. You can specify the same
-- @cacheNamespace@ across resources to return the same cached data for
-- requests to different resources.
putIntegration_cacheNamespace :: Lens.Lens' PutIntegration (Prelude.Maybe Prelude.Text)
putIntegration_cacheNamespace = Lens.lens (\PutIntegration' {cacheNamespace} -> cacheNamespace) (\s@PutIntegration' {} a -> s {cacheNamespace = a} :: PutIntegration)

-- | The ID of the VpcLink used for the integration. Specify this value only
-- if you specify @VPC_LINK@ as the connection type.
putIntegration_connectionId :: Lens.Lens' PutIntegration (Prelude.Maybe Prelude.Text)
putIntegration_connectionId = Lens.lens (\PutIntegration' {connectionId} -> connectionId) (\s@PutIntegration' {} a -> s {connectionId = a} :: PutIntegration)

-- | The type of the network connection to the integration endpoint. The
-- valid value is @INTERNET@ for connections through the public routable
-- internet or @VPC_LINK@ for private connections between API Gateway and a
-- network load balancer in a VPC. The default value is @INTERNET@.
putIntegration_connectionType :: Lens.Lens' PutIntegration (Prelude.Maybe ConnectionType)
putIntegration_connectionType = Lens.lens (\PutIntegration' {connectionType} -> connectionType) (\s@PutIntegration' {} a -> s {connectionType = a} :: PutIntegration)

-- | Specifies how to handle request payload content type conversions.
-- Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@, with the
-- following behaviors:
--
-- If this property is not defined, the request payload will be passed
-- through from the method request to integration request without
-- modification, provided that the @passthroughBehavior@ is configured to
-- support payload pass-through.
putIntegration_contentHandling :: Lens.Lens' PutIntegration (Prelude.Maybe ContentHandlingStrategy)
putIntegration_contentHandling = Lens.lens (\PutIntegration' {contentHandling} -> contentHandling) (\s@PutIntegration' {} a -> s {contentHandling = a} :: PutIntegration)

-- | Specifies whether credentials are required for a put integration.
putIntegration_credentials :: Lens.Lens' PutIntegration (Prelude.Maybe Prelude.Text)
putIntegration_credentials = Lens.lens (\PutIntegration' {credentials} -> credentials) (\s@PutIntegration' {} a -> s {credentials = a} :: PutIntegration)

-- | The HTTP method for the integration.
putIntegration_integrationHttpMethod :: Lens.Lens' PutIntegration (Prelude.Maybe Prelude.Text)
putIntegration_integrationHttpMethod = Lens.lens (\PutIntegration' {integrationHttpMethod} -> integrationHttpMethod) (\s@PutIntegration' {} a -> s {integrationHttpMethod = a} :: PutIntegration)

-- | Specifies the pass-through behavior for incoming requests based on the
-- Content-Type header in the request, and the available mapping templates
-- specified as the @requestTemplates@ property on the Integration
-- resource. There are three valid values: @WHEN_NO_MATCH@,
-- @WHEN_NO_TEMPLATES@, and @NEVER@.
putIntegration_passthroughBehavior :: Lens.Lens' PutIntegration (Prelude.Maybe Prelude.Text)
putIntegration_passthroughBehavior = Lens.lens (\PutIntegration' {passthroughBehavior} -> passthroughBehavior) (\s@PutIntegration' {} a -> s {passthroughBehavior = a} :: PutIntegration)

-- | A key-value map specifying request parameters that are passed from the
-- method request to the back end. The key is an integration request
-- parameter name and the associated value is a method request parameter
-- value or static value that must be enclosed within single quotes and
-- pre-encoded as required by the back end. The method request parameter
-- value must match the pattern of @method.request.{location}.{name}@,
-- where @location@ is @querystring@, @path@, or @header@ and @name@ must
-- be a valid and unique method request parameter name.
putIntegration_requestParameters :: Lens.Lens' PutIntegration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putIntegration_requestParameters = Lens.lens (\PutIntegration' {requestParameters} -> requestParameters) (\s@PutIntegration' {} a -> s {requestParameters = a} :: PutIntegration) Prelude.. Lens.mapping Lens.coerced

-- | Represents a map of Velocity templates that are applied on the request
-- payload based on the value of the Content-Type header sent by the
-- client. The content type value is the key in this map, and the template
-- (as a String) is the value.
putIntegration_requestTemplates :: Lens.Lens' PutIntegration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putIntegration_requestTemplates = Lens.lens (\PutIntegration' {requestTemplates} -> requestTemplates) (\s@PutIntegration' {} a -> s {requestTemplates = a} :: PutIntegration) Prelude.. Lens.mapping Lens.coerced

-- | Custom timeout between 50 and 29,000 milliseconds. The default value is
-- 29,000 milliseconds or 29 seconds.
putIntegration_timeoutInMillis :: Lens.Lens' PutIntegration (Prelude.Maybe Prelude.Int)
putIntegration_timeoutInMillis = Lens.lens (\PutIntegration' {timeoutInMillis} -> timeoutInMillis) (\s@PutIntegration' {} a -> s {timeoutInMillis = a} :: PutIntegration)

-- | Undocumented member.
putIntegration_tlsConfig :: Lens.Lens' PutIntegration (Prelude.Maybe TlsConfig)
putIntegration_tlsConfig = Lens.lens (\PutIntegration' {tlsConfig} -> tlsConfig) (\s@PutIntegration' {} a -> s {tlsConfig = a} :: PutIntegration)

-- | Specifies Uniform Resource Identifier (URI) of the integration endpoint.
-- For HTTP or @HTTP_PROXY@ integrations, the URI must be a fully formed,
-- encoded HTTP(S) URL according to the RFC-3986 specification, for either
-- standard integration, where @connectionType@ is not @VPC_LINK@, or
-- private integration, where @connectionType@ is @VPC_LINK@. For a private
-- HTTP integration, the URI is not used for routing. For @AWS@ or
-- @AWS_PROXY@ integrations, the URI is of the form
-- @arn:aws:apigateway:{region}:{subdomain.service|service}:path|action\/{service_api@}.
-- Here, {Region} is the API Gateway region (e.g., us-east-1); {service} is
-- the name of the integrated Amazon Web Services service (e.g., s3); and
-- {subdomain} is a designated subdomain supported by certain Amazon Web
-- Services service for fast host-name lookup. action can be used for an
-- Amazon Web Services service action-based API, using an
-- Action={name}&{p1}={v1}&p2={v2}... query string. The ensuing
-- {service_api} refers to a supported action {name} plus any required
-- input parameters. Alternatively, path can be used for an Amazon Web
-- Services service path-based API. The ensuing service_api refers to the
-- path to an Amazon Web Services service resource, including the region of
-- the integrated Amazon Web Services service, if applicable. For example,
-- for integration with the S3 API of @GetObject@, the @uri@ can be either
-- @arn:aws:apigateway:us-west-2:s3:action\/GetObject&Bucket={bucket}&Key={key}@
-- or @arn:aws:apigateway:us-west-2:s3:path\/{bucket}\/{key}@.
putIntegration_uri :: Lens.Lens' PutIntegration (Prelude.Maybe Prelude.Text)
putIntegration_uri = Lens.lens (\PutIntegration' {uri} -> uri) (\s@PutIntegration' {} a -> s {uri = a} :: PutIntegration)

-- | The string identifier of the associated RestApi.
putIntegration_restApiId :: Lens.Lens' PutIntegration Prelude.Text
putIntegration_restApiId = Lens.lens (\PutIntegration' {restApiId} -> restApiId) (\s@PutIntegration' {} a -> s {restApiId = a} :: PutIntegration)

-- | Specifies a put integration request\'s resource ID.
putIntegration_resourceId :: Lens.Lens' PutIntegration Prelude.Text
putIntegration_resourceId = Lens.lens (\PutIntegration' {resourceId} -> resourceId) (\s@PutIntegration' {} a -> s {resourceId = a} :: PutIntegration)

-- | Specifies the HTTP method for the integration.
putIntegration_httpMethod :: Lens.Lens' PutIntegration Prelude.Text
putIntegration_httpMethod = Lens.lens (\PutIntegration' {httpMethod} -> httpMethod) (\s@PutIntegration' {} a -> s {httpMethod = a} :: PutIntegration)

-- | Specifies a put integration input\'s type.
putIntegration_type :: Lens.Lens' PutIntegration IntegrationType
putIntegration_type = Lens.lens (\PutIntegration' {type'} -> type') (\s@PutIntegration' {} a -> s {type' = a} :: PutIntegration)

instance Core.AWSRequest PutIntegration where
  type AWSResponse PutIntegration = Integration
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable PutIntegration where
  hashWithSalt _salt PutIntegration' {..} =
    _salt `Prelude.hashWithSalt` cacheKeyParameters
      `Prelude.hashWithSalt` cacheNamespace
      `Prelude.hashWithSalt` connectionId
      `Prelude.hashWithSalt` connectionType
      `Prelude.hashWithSalt` contentHandling
      `Prelude.hashWithSalt` credentials
      `Prelude.hashWithSalt` integrationHttpMethod
      `Prelude.hashWithSalt` passthroughBehavior
      `Prelude.hashWithSalt` requestParameters
      `Prelude.hashWithSalt` requestTemplates
      `Prelude.hashWithSalt` timeoutInMillis
      `Prelude.hashWithSalt` tlsConfig
      `Prelude.hashWithSalt` uri
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` httpMethod
      `Prelude.hashWithSalt` type'

instance Prelude.NFData PutIntegration where
  rnf PutIntegration' {..} =
    Prelude.rnf cacheKeyParameters
      `Prelude.seq` Prelude.rnf cacheNamespace
      `Prelude.seq` Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf connectionType
      `Prelude.seq` Prelude.rnf contentHandling
      `Prelude.seq` Prelude.rnf credentials
      `Prelude.seq` Prelude.rnf integrationHttpMethod
      `Prelude.seq` Prelude.rnf passthroughBehavior
      `Prelude.seq` Prelude.rnf requestParameters
      `Prelude.seq` Prelude.rnf requestTemplates
      `Prelude.seq` Prelude.rnf timeoutInMillis
      `Prelude.seq` Prelude.rnf tlsConfig
      `Prelude.seq` Prelude.rnf uri
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf httpMethod
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders PutIntegration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON PutIntegration where
  toJSON PutIntegration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cacheKeyParameters" Data..=)
              Prelude.<$> cacheKeyParameters,
            ("cacheNamespace" Data..=)
              Prelude.<$> cacheNamespace,
            ("connectionId" Data..=) Prelude.<$> connectionId,
            ("connectionType" Data..=)
              Prelude.<$> connectionType,
            ("contentHandling" Data..=)
              Prelude.<$> contentHandling,
            ("credentials" Data..=) Prelude.<$> credentials,
            ("httpMethod" Data..=)
              Prelude.<$> integrationHttpMethod,
            ("passthroughBehavior" Data..=)
              Prelude.<$> passthroughBehavior,
            ("requestParameters" Data..=)
              Prelude.<$> requestParameters,
            ("requestTemplates" Data..=)
              Prelude.<$> requestTemplates,
            ("timeoutInMillis" Data..=)
              Prelude.<$> timeoutInMillis,
            ("tlsConfig" Data..=) Prelude.<$> tlsConfig,
            ("uri" Data..=) Prelude.<$> uri,
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath PutIntegration where
  toPath PutIntegration' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/resources/",
        Data.toBS resourceId,
        "/methods/",
        Data.toBS httpMethod,
        "/integration"
      ]

instance Data.ToQuery PutIntegration where
  toQuery = Prelude.const Prelude.mempty
