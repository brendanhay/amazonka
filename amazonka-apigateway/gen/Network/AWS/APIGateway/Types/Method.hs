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
-- Module      : Network.AWS.APIGateway.Types.Method
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Method where

import Network.AWS.APIGateway.Types.Integration
import Network.AWS.APIGateway.Types.MethodResponse
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a client-facing interface by which the client calls the API
-- to access back-end resources. A __Method__ resource is integrated with
-- an Integration resource. Both consist of a request and one or more
-- responses. The method request takes the client input that is passed to
-- the back end through the integration request. A method response returns
-- the output from the back end to the client through an integration
-- response. A method request is embodied in a __Method__ resource, whereas
-- an integration request is embodied in an Integration resource. On the
-- other hand, a method response is represented by a MethodResponse
-- resource, whereas an integration response is represented by an
-- IntegrationResponse resource.
--
-- ==== Example: Retrive the GET method on a specified resource
--
-- ===== Request
--
-- The following example request retrieves the information about the GET
-- method on an API resource (@3kzxbg5sa2@) of an API (@fugvjdxtri@).
--
-- > GET /restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160603T210259Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160603/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}
--
-- ===== Response
--
-- The successful response returns a @200 OK@ status code and a payload
-- similar to the following:
--
-- > { "_links": { "curies": [ { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-{rel}.html", "name": "integration", "templated": true }, { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true }, { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-{rel}.html", "name": "method", "templated": true }, { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-response-{rel}.html", "name": "methodresponse", "templated": true } ], "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET", "name": "GET", "title": "GET" }, "integration:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "method:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET" }, "method:integration": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "method:responses": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200", "name": "200", "title": "200" }, "method:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET" }, "methodresponse:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/{status_code}", "templated": true } }, "apiKeyRequired": true, "authorizationType": "NONE", "httpMethod": "GET", "_embedded": { "method:integration": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integration:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integration:responses": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integration:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integrationresponse:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/{status_code}", "templated": true } }, "cacheKeyParameters": [], "cacheNamespace": "3kzxbg5sa2", "credentials": "arn:aws:iam::123456789012:role/apigAwsProxyRole", "httpMethod": "POST", "passthroughBehavior": "WHEN_NO_MATCH", "requestParameters": { "integration.request.header.Content-Type": "'application/x-amz-json-1.1'" }, "requestTemplates": { "application/json": "{\n}" }, "type": "AWS", "uri": "arn:aws:apigateway:us-east-1:kinesis:action/ListStreams", "_embedded": { "integration:responses": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.Content-Type": "'application/xml'" }, "responseTemplates": { "application/json": "$util.urlDecode(\"%3CkinesisStreams%3E%23foreach(%24stream%20in%20%24input.path(%27%24.StreamNames%27))%3Cstream%3E%3Cname%3E%24stream%3C%2Fname%3E%3C%2Fstream%3E%23end%3C%2FkinesisStreams%3E\")" }, "statusCode": "200" } } }, "method:responses": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200", "name": "200", "title": "200" }, "methodresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" }, "methodresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" } }, "responseModels": { "application/json": "Empty" }, "responseParameters": { "method.response.header.Content-Type": false }, "statusCode": "200" } } }
--
-- In the example above, the response template for the @200 OK@ response
-- maps the JSON output from the @ListStreams@ action in the back end to an
-- XML output. The mapping template is URL-encoded as
-- @%3CkinesisStreams%3E%23foreach(%24stream%20in%20%24input.path(%27%24.StreamNames%27))%3Cstream%3E%3Cname%3E%24stream%3C%2Fname%3E%3C%2Fstream%3E%23end%3C%2FkinesisStreams%3E@
-- and the output is decoded using the
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-mapping-template-reference.html#util-templat-reference $util.urlDecode()>
-- helper function.
--
-- MethodResponse, Integration, IntegrationResponse, Resource,
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-method-settings.html Set up an API\'s method>
--
-- /See:/ 'newMethod' smart constructor.
data Method = Method'
  { -- | The method\'s HTTP verb.
    httpMethod :: Core.Maybe Core.Text,
    -- | Gets the method\'s integration responsible for passing the
    -- client-submitted request to the back end and performing necessary
    -- transformations to make the request compliant with the back end.
    --
    -- ==== Example:
    --
    -- ===== Request
    --
    -- > GET /restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com Content-Length: 117 X-Amz-Date: 20160613T213210Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160613/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}
    --
    -- ===== Response
    --
    -- The successful response returns a @200 OK@ status code and a payload
    -- similar to the following:
    --
    -- > { "_links": { "curies": [ { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-{rel}.html", "name": "integration", "templated": true }, { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true } ], "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integration:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integration:responses": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integration:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integrationresponse:put": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/{status_code}", "templated": true } }, "cacheKeyParameters": [], "cacheNamespace": "0cjtch", "credentials": "arn:aws:iam::123456789012:role/apigAwsProxyRole", "httpMethod": "POST", "passthroughBehavior": "WHEN_NO_MATCH", "requestTemplates": { "application/json": "{\n \"a\": \"$input.params('operand1')\",\n \"b\": \"$input.params('operand2')\", \n \"op\": \"$input.params('operator')\" \n}" }, "type": "AWS", "uri": "arn:aws:apigateway:us-west-2:lambda:path//2015-03-31/functions/arn:aws:lambda:us-west-2:123456789012:function:Calc/invocations", "_embedded": { "integration:responses": { "_links": { "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.operator": "integration.response.body.op", "method.response.header.operand_2": "integration.response.body.b", "method.response.header.operand_1": "integration.response.body.a" }, "responseTemplates": { "application/json": "#set($res = $input.path('$'))\n{\n \"result\": \"$res.a, $res.b, $res.op => $res.c\",\n \"a\" : \"$res.a\",\n \"b\" : \"$res.b\",\n \"op\" : \"$res.op\",\n \"c\" : \"$res.c\"\n}" }, "selectionPattern": "", "statusCode": "200" } } }
    --
    -- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-integration.html AWS CLI>
    methodIntegration :: Core.Maybe Integration,
    -- | A boolean flag specifying whether a valid ApiKey is required to invoke
    -- this method.
    apiKeyRequired :: Core.Maybe Core.Bool,
    -- | The method\'s authorization type. Valid values are @NONE@ for open
    -- access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a
    -- custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user
    -- pool.
    authorizationType :: Core.Maybe Core.Text,
    -- | A key-value map specifying data schemas, represented by Model resources,
    -- (as the mapped value) of the request payloads of given content types (as
    -- the mapping key).
    requestModels :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A human-friendly operation identifier for the method. For example, you
    -- can assign the @operationName@ of @ListPets@ for the @GET \/pets@ method
    -- in the @PetStore@ example.
    operationName :: Core.Maybe Core.Text,
    -- | The identifier of a RequestValidator for request validation.
    requestValidatorId :: Core.Maybe Core.Text,
    -- | Gets a method response associated with a given HTTP status code.
    --
    -- The collection of method responses are encapsulated in a key-value map,
    -- where the key is a response\'s HTTP status code and the value is a
    -- MethodResponse resource that specifies the response returned to the
    -- caller from the back end through the integration response.
    --
    -- ==== Example: Get a 200 OK response of a GET method
    --
    -- ===== Request
    --
    -- > GET /restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200 HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com Content-Length: 117 X-Amz-Date: 20160613T215008Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160613/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}
    --
    -- ===== Response
    --
    -- The successful response returns a @200 OK@ status code and a payload
    -- similar to the following:
    --
    -- > { "_links": { "curies": { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-response-{rel}.html", "name": "methodresponse", "templated": true }, "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200", "title": "200" }, "methodresponse:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200" }, "methodresponse:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200" } }, "responseModels": { "application/json": "Empty" }, "responseParameters": { "method.response.header.operator": false, "method.response.header.operand_2": false, "method.response.header.operand_1": false }, "statusCode": "200" }
    --
    -- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-method-response.html AWS CLI>
    methodResponses :: Core.Maybe (Core.HashMap Core.Text MethodResponse),
    -- | The identifier of an Authorizer to use on this method. The
    -- @authorizationType@ must be @CUSTOM@.
    authorizerId :: Core.Maybe Core.Text,
    -- | A key-value map defining required or optional method request parameters
    -- that can be accepted by API Gateway. A key is a method request parameter
    -- name matching the pattern of @method.request.{location}.{name}@, where
    -- @location@ is @querystring@, @path@, or @header@ and @name@ is a valid
    -- and unique parameter name. The value associated with the key is a
    -- Boolean flag indicating whether the parameter is required (@true@) or
    -- optional (@false@). The method request parameter names defined here are
    -- available in Integration to be mapped to integration request parameters
    -- or templates.
    requestParameters :: Core.Maybe (Core.HashMap Core.Text Core.Bool),
    -- | A list of authorization scopes configured on the method. The scopes are
    -- used with a @COGNITO_USER_POOLS@ authorizer to authorize the method
    -- invocation. The authorization works by matching the method scopes
    -- against the scopes parsed from the access token in the incoming request.
    -- The method invocation is authorized if any method scopes matches a
    -- claimed scope in the access token. Otherwise, the invocation is not
    -- authorized. When the method scope is configured, the client must provide
    -- an access token instead of an identity token for authorization purposes.
    authorizationScopes :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Method' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpMethod', 'method_httpMethod' - The method\'s HTTP verb.
--
-- 'methodIntegration', 'method_methodIntegration' - Gets the method\'s integration responsible for passing the
-- client-submitted request to the back end and performing necessary
-- transformations to make the request compliant with the back end.
--
-- ==== Example:
--
-- ===== Request
--
-- > GET /restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com Content-Length: 117 X-Amz-Date: 20160613T213210Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160613/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}
--
-- ===== Response
--
-- The successful response returns a @200 OK@ status code and a payload
-- similar to the following:
--
-- > { "_links": { "curies": [ { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-{rel}.html", "name": "integration", "templated": true }, { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true } ], "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integration:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integration:responses": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integration:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integrationresponse:put": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/{status_code}", "templated": true } }, "cacheKeyParameters": [], "cacheNamespace": "0cjtch", "credentials": "arn:aws:iam::123456789012:role/apigAwsProxyRole", "httpMethod": "POST", "passthroughBehavior": "WHEN_NO_MATCH", "requestTemplates": { "application/json": "{\n \"a\": \"$input.params('operand1')\",\n \"b\": \"$input.params('operand2')\", \n \"op\": \"$input.params('operator')\" \n}" }, "type": "AWS", "uri": "arn:aws:apigateway:us-west-2:lambda:path//2015-03-31/functions/arn:aws:lambda:us-west-2:123456789012:function:Calc/invocations", "_embedded": { "integration:responses": { "_links": { "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.operator": "integration.response.body.op", "method.response.header.operand_2": "integration.response.body.b", "method.response.header.operand_1": "integration.response.body.a" }, "responseTemplates": { "application/json": "#set($res = $input.path('$'))\n{\n \"result\": \"$res.a, $res.b, $res.op => $res.c\",\n \"a\" : \"$res.a\",\n \"b\" : \"$res.b\",\n \"op\" : \"$res.op\",\n \"c\" : \"$res.c\"\n}" }, "selectionPattern": "", "statusCode": "200" } } }
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-integration.html AWS CLI>
--
-- 'apiKeyRequired', 'method_apiKeyRequired' - A boolean flag specifying whether a valid ApiKey is required to invoke
-- this method.
--
-- 'authorizationType', 'method_authorizationType' - The method\'s authorization type. Valid values are @NONE@ for open
-- access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a
-- custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user
-- pool.
--
-- 'requestModels', 'method_requestModels' - A key-value map specifying data schemas, represented by Model resources,
-- (as the mapped value) of the request payloads of given content types (as
-- the mapping key).
--
-- 'operationName', 'method_operationName' - A human-friendly operation identifier for the method. For example, you
-- can assign the @operationName@ of @ListPets@ for the @GET \/pets@ method
-- in the @PetStore@ example.
--
-- 'requestValidatorId', 'method_requestValidatorId' - The identifier of a RequestValidator for request validation.
--
-- 'methodResponses', 'method_methodResponses' - Gets a method response associated with a given HTTP status code.
--
-- The collection of method responses are encapsulated in a key-value map,
-- where the key is a response\'s HTTP status code and the value is a
-- MethodResponse resource that specifies the response returned to the
-- caller from the back end through the integration response.
--
-- ==== Example: Get a 200 OK response of a GET method
--
-- ===== Request
--
-- > GET /restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200 HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com Content-Length: 117 X-Amz-Date: 20160613T215008Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160613/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}
--
-- ===== Response
--
-- The successful response returns a @200 OK@ status code and a payload
-- similar to the following:
--
-- > { "_links": { "curies": { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-response-{rel}.html", "name": "methodresponse", "templated": true }, "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200", "title": "200" }, "methodresponse:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200" }, "methodresponse:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200" } }, "responseModels": { "application/json": "Empty" }, "responseParameters": { "method.response.header.operator": false, "method.response.header.operand_2": false, "method.response.header.operand_1": false }, "statusCode": "200" }
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-method-response.html AWS CLI>
--
-- 'authorizerId', 'method_authorizerId' - The identifier of an Authorizer to use on this method. The
-- @authorizationType@ must be @CUSTOM@.
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
-- 'authorizationScopes', 'method_authorizationScopes' - A list of authorization scopes configured on the method. The scopes are
-- used with a @COGNITO_USER_POOLS@ authorizer to authorize the method
-- invocation. The authorization works by matching the method scopes
-- against the scopes parsed from the access token in the incoming request.
-- The method invocation is authorized if any method scopes matches a
-- claimed scope in the access token. Otherwise, the invocation is not
-- authorized. When the method scope is configured, the client must provide
-- an access token instead of an identity token for authorization purposes.
newMethod ::
  Method
newMethod =
  Method'
    { httpMethod = Core.Nothing,
      methodIntegration = Core.Nothing,
      apiKeyRequired = Core.Nothing,
      authorizationType = Core.Nothing,
      requestModels = Core.Nothing,
      operationName = Core.Nothing,
      requestValidatorId = Core.Nothing,
      methodResponses = Core.Nothing,
      authorizerId = Core.Nothing,
      requestParameters = Core.Nothing,
      authorizationScopes = Core.Nothing
    }

-- | The method\'s HTTP verb.
method_httpMethod :: Lens.Lens' Method (Core.Maybe Core.Text)
method_httpMethod = Lens.lens (\Method' {httpMethod} -> httpMethod) (\s@Method' {} a -> s {httpMethod = a} :: Method)

-- | Gets the method\'s integration responsible for passing the
-- client-submitted request to the back end and performing necessary
-- transformations to make the request compliant with the back end.
--
-- ==== Example:
--
-- ===== Request
--
-- > GET /restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com Content-Length: 117 X-Amz-Date: 20160613T213210Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160613/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}
--
-- ===== Response
--
-- The successful response returns a @200 OK@ status code and a payload
-- similar to the following:
--
-- > { "_links": { "curies": [ { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-{rel}.html", "name": "integration", "templated": true }, { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true } ], "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integration:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integration:responses": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integration:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integrationresponse:put": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/{status_code}", "templated": true } }, "cacheKeyParameters": [], "cacheNamespace": "0cjtch", "credentials": "arn:aws:iam::123456789012:role/apigAwsProxyRole", "httpMethod": "POST", "passthroughBehavior": "WHEN_NO_MATCH", "requestTemplates": { "application/json": "{\n \"a\": \"$input.params('operand1')\",\n \"b\": \"$input.params('operand2')\", \n \"op\": \"$input.params('operator')\" \n}" }, "type": "AWS", "uri": "arn:aws:apigateway:us-west-2:lambda:path//2015-03-31/functions/arn:aws:lambda:us-west-2:123456789012:function:Calc/invocations", "_embedded": { "integration:responses": { "_links": { "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.operator": "integration.response.body.op", "method.response.header.operand_2": "integration.response.body.b", "method.response.header.operand_1": "integration.response.body.a" }, "responseTemplates": { "application/json": "#set($res = $input.path('$'))\n{\n \"result\": \"$res.a, $res.b, $res.op => $res.c\",\n \"a\" : \"$res.a\",\n \"b\" : \"$res.b\",\n \"op\" : \"$res.op\",\n \"c\" : \"$res.c\"\n}" }, "selectionPattern": "", "statusCode": "200" } } }
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-integration.html AWS CLI>
method_methodIntegration :: Lens.Lens' Method (Core.Maybe Integration)
method_methodIntegration = Lens.lens (\Method' {methodIntegration} -> methodIntegration) (\s@Method' {} a -> s {methodIntegration = a} :: Method)

-- | A boolean flag specifying whether a valid ApiKey is required to invoke
-- this method.
method_apiKeyRequired :: Lens.Lens' Method (Core.Maybe Core.Bool)
method_apiKeyRequired = Lens.lens (\Method' {apiKeyRequired} -> apiKeyRequired) (\s@Method' {} a -> s {apiKeyRequired = a} :: Method)

-- | The method\'s authorization type. Valid values are @NONE@ for open
-- access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a
-- custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user
-- pool.
method_authorizationType :: Lens.Lens' Method (Core.Maybe Core.Text)
method_authorizationType = Lens.lens (\Method' {authorizationType} -> authorizationType) (\s@Method' {} a -> s {authorizationType = a} :: Method)

-- | A key-value map specifying data schemas, represented by Model resources,
-- (as the mapped value) of the request payloads of given content types (as
-- the mapping key).
method_requestModels :: Lens.Lens' Method (Core.Maybe (Core.HashMap Core.Text Core.Text))
method_requestModels = Lens.lens (\Method' {requestModels} -> requestModels) (\s@Method' {} a -> s {requestModels = a} :: Method) Core.. Lens.mapping Lens._Coerce

-- | A human-friendly operation identifier for the method. For example, you
-- can assign the @operationName@ of @ListPets@ for the @GET \/pets@ method
-- in the @PetStore@ example.
method_operationName :: Lens.Lens' Method (Core.Maybe Core.Text)
method_operationName = Lens.lens (\Method' {operationName} -> operationName) (\s@Method' {} a -> s {operationName = a} :: Method)

-- | The identifier of a RequestValidator for request validation.
method_requestValidatorId :: Lens.Lens' Method (Core.Maybe Core.Text)
method_requestValidatorId = Lens.lens (\Method' {requestValidatorId} -> requestValidatorId) (\s@Method' {} a -> s {requestValidatorId = a} :: Method)

-- | Gets a method response associated with a given HTTP status code.
--
-- The collection of method responses are encapsulated in a key-value map,
-- where the key is a response\'s HTTP status code and the value is a
-- MethodResponse resource that specifies the response returned to the
-- caller from the back end through the integration response.
--
-- ==== Example: Get a 200 OK response of a GET method
--
-- ===== Request
--
-- > GET /restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200 HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com Content-Length: 117 X-Amz-Date: 20160613T215008Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160613/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}
--
-- ===== Response
--
-- The successful response returns a @200 OK@ status code and a payload
-- similar to the following:
--
-- > { "_links": { "curies": { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-response-{rel}.html", "name": "methodresponse", "templated": true }, "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200", "title": "200" }, "methodresponse:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200" }, "methodresponse:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200" } }, "responseModels": { "application/json": "Empty" }, "responseParameters": { "method.response.header.operator": false, "method.response.header.operand_2": false, "method.response.header.operand_1": false }, "statusCode": "200" }
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-method-response.html AWS CLI>
method_methodResponses :: Lens.Lens' Method (Core.Maybe (Core.HashMap Core.Text MethodResponse))
method_methodResponses = Lens.lens (\Method' {methodResponses} -> methodResponses) (\s@Method' {} a -> s {methodResponses = a} :: Method) Core.. Lens.mapping Lens._Coerce

-- | The identifier of an Authorizer to use on this method. The
-- @authorizationType@ must be @CUSTOM@.
method_authorizerId :: Lens.Lens' Method (Core.Maybe Core.Text)
method_authorizerId = Lens.lens (\Method' {authorizerId} -> authorizerId) (\s@Method' {} a -> s {authorizerId = a} :: Method)

-- | A key-value map defining required or optional method request parameters
-- that can be accepted by API Gateway. A key is a method request parameter
-- name matching the pattern of @method.request.{location}.{name}@, where
-- @location@ is @querystring@, @path@, or @header@ and @name@ is a valid
-- and unique parameter name. The value associated with the key is a
-- Boolean flag indicating whether the parameter is required (@true@) or
-- optional (@false@). The method request parameter names defined here are
-- available in Integration to be mapped to integration request parameters
-- or templates.
method_requestParameters :: Lens.Lens' Method (Core.Maybe (Core.HashMap Core.Text Core.Bool))
method_requestParameters = Lens.lens (\Method' {requestParameters} -> requestParameters) (\s@Method' {} a -> s {requestParameters = a} :: Method) Core.. Lens.mapping Lens._Coerce

-- | A list of authorization scopes configured on the method. The scopes are
-- used with a @COGNITO_USER_POOLS@ authorizer to authorize the method
-- invocation. The authorization works by matching the method scopes
-- against the scopes parsed from the access token in the incoming request.
-- The method invocation is authorized if any method scopes matches a
-- claimed scope in the access token. Otherwise, the invocation is not
-- authorized. When the method scope is configured, the client must provide
-- an access token instead of an identity token for authorization purposes.
method_authorizationScopes :: Lens.Lens' Method (Core.Maybe [Core.Text])
method_authorizationScopes = Lens.lens (\Method' {authorizationScopes} -> authorizationScopes) (\s@Method' {} a -> s {authorizationScopes = a} :: Method) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON Method where
  parseJSON =
    Core.withObject
      "Method"
      ( \x ->
          Method'
            Core.<$> (x Core..:? "httpMethod")
            Core.<*> (x Core..:? "methodIntegration")
            Core.<*> (x Core..:? "apiKeyRequired")
            Core.<*> (x Core..:? "authorizationType")
            Core.<*> (x Core..:? "requestModels" Core..!= Core.mempty)
            Core.<*> (x Core..:? "operationName")
            Core.<*> (x Core..:? "requestValidatorId")
            Core.<*> (x Core..:? "methodResponses" Core..!= Core.mempty)
            Core.<*> (x Core..:? "authorizerId")
            Core.<*> (x Core..:? "requestParameters" Core..!= Core.mempty)
            Core.<*> ( x Core..:? "authorizationScopes"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable Method

instance Core.NFData Method
