{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Method
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Method
  ( Method (..),

    -- * Smart constructor
    mkMethod,

    -- * Lenses
    mMethodResponses,
    mHttpMethod,
    mAuthorizationScopes,
    mRequestValidatorId,
    mRequestModels,
    mRequestParameters,
    mAuthorizerId,
    mOperationName,
    mAuthorizationType,
    mApiKeyRequired,
    mMethodIntegration,
  )
where

import Network.AWS.APIGateway.Types.Integration
import Network.AWS.APIGateway.Types.MethodResponse
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a client-facing interface by which the client calls the API to access back-end resources. A __Method__ resource is integrated with an 'Integration' resource. Both consist of a request and one or more responses. The method request takes the client input that is passed to the back end through the integration request. A method response returns the output from the back end to the client through an integration response. A method request is embodied in a __Method__ resource, whereas an integration request is embodied in an 'Integration' resource. On the other hand, a method response is represented by a 'MethodResponse' resource, whereas an integration response is represented by an 'IntegrationResponse' resource.
--
--
-- __Example: Retrive the GET method on a specified resource__
-- __Request__
-- The following example request retrieves the information about the GET method on an API resource (@3kzxbg5sa2@ ) of an API (@fugvjdxtri@ ).
-- @@GET /restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160603T210259Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160603/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}@ @ __Response__
-- The successful response returns a @200 OK@ status code and a payload similar to the following:
-- @@{ "_links": { "curies": [ { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-{rel}.html", "name": "integration", "templated": true }, { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true }, { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-{rel}.html", "name": "method", "templated": true }, { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-response-{rel}.html", "name": "methodresponse", "templated": true } ], "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET", "name": "GET", "title": "GET" }, "integration:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "method:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET" }, "method:integration": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "method:responses": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200", "name": "200", "title": "200" }, "method:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET" }, "methodresponse:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/{status_code}", "templated": true } }, "apiKeyRequired": true, "authorizationType": "NONE", "httpMethod": "GET", "_embedded": { "method:integration": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integration:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integration:responses": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integration:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration" }, "integrationresponse:put": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/{status_code}", "templated": true } }, "cacheKeyParameters": [], "cacheNamespace": "3kzxbg5sa2", "credentials": "arn:aws:iam::123456789012:role/apigAwsProxyRole", "httpMethod": "POST", "passthroughBehavior": "WHEN_NO_MATCH", "requestParameters": { "integration.request.header.Content-Type": "'application/x-amz-json-1.1'" }, "requestTemplates": { "application/json": "{\n}" }, "type": "AWS", "uri": "arn:aws:apigateway:us-east-1:kinesis:action/ListStreams", "_embedded": { "integration:responses": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.Content-Type": "'application/xml'" }, "responseTemplates": { "application/json": "$util.urlDecode(\"%3CkinesisStreams%3E%23foreach(%24stream%20in%20%24input.path(%27%24.StreamNames%27))%3Cstream%3E%3Cname%3E%24stream%3C%2Fname%3E%3C%2Fstream%3E%23end%3C%2FkinesisStreams%3E\")" }, "statusCode": "200" } } }, "method:responses": { "_links": { "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200", "name": "200", "title": "200" }, "methodresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" }, "methodresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/responses/200" } }, "responseModels": { "application/json": "Empty" }, "responseParameters": { "method.response.header.Content-Type": false }, "statusCode": "200" } } }@ @ In the example above, the response template for the @200 OK@ response maps the JSON output from the @ListStreams@ action in the back end to an XML output. The mapping template is URL-encoded as @%3CkinesisStreams%3E%23foreach(%24stream%20in%20%24input.path(%27%24.StreamNames%27))%3Cstream%3E%3Cname%3E%24stream%3C%2Fname%3E%3C%2Fstream%3E%23end%3C%2FkinesisStreams%3E@ and the output is decoded using the <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-mapping-template-reference.html#util-templat-reference > util.urlDecode()> helper function.
-- 'MethodResponse' , 'Integration' , 'IntegrationResponse' , 'Resource' , <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-method-settings.html Set up an API's method>
--
-- /See:/ 'mkMethod' smart constructor.
data Method = Method'
  { methodResponses ::
      Lude.Maybe (Lude.HashMap Lude.Text (MethodResponse)),
    httpMethod :: Lude.Maybe Lude.Text,
    authorizationScopes :: Lude.Maybe [Lude.Text],
    requestValidatorId :: Lude.Maybe Lude.Text,
    requestModels :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    requestParameters ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Bool)),
    authorizerId :: Lude.Maybe Lude.Text,
    operationName :: Lude.Maybe Lude.Text,
    authorizationType :: Lude.Maybe Lude.Text,
    apiKeyRequired :: Lude.Maybe Lude.Bool,
    methodIntegration :: Lude.Maybe Integration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Method' with the minimum fields required to make a request.
--
-- * 'apiKeyRequired' - A boolean flag specifying whether a valid 'ApiKey' is required to invoke this method.
-- * 'authorizationScopes' - A list of authorization scopes configured on the method. The scopes are used with a @COGNITO_USER_POOLS@ authorizer to authorize the method invocation. The authorization works by matching the method scopes against the scopes parsed from the access token in the incoming request. The method invocation is authorized if any method scopes matches a claimed scope in the access token. Otherwise, the invocation is not authorized. When the method scope is configured, the client must provide an access token instead of an identity token for authorization purposes.
-- * 'authorizationType' - The method's authorization type. Valid values are @NONE@ for open access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user pool.
-- * 'authorizerId' - The identifier of an 'Authorizer' to use on this method. The @authorizationType@ must be @CUSTOM@ .
-- * 'httpMethod' - The method's HTTP verb.
-- * 'methodIntegration' - Gets the method's integration responsible for passing the client-submitted request to the back end and performing necessary transformations to make the request compliant with the back end.
--
--
-- __Example: __
-- __Request__
--
-- @@GET /restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com Content-Length: 117 X-Amz-Date: 20160613T213210Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160613/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}@ @ __Response__
-- The successful response returns a @200 OK@ status code and a payload similar to the following:
-- @@{ "_links": { "curies": [ { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-{rel}.html", "name": "integration", "templated": true }, { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true } ], "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integration:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integration:responses": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integration:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integrationresponse:put": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/{status_code}", "templated": true } }, "cacheKeyParameters": [], "cacheNamespace": "0cjtch", "credentials": "arn:aws:iam::123456789012:role/apigAwsProxyRole", "httpMethod": "POST", "passthroughBehavior": "WHEN_NO_MATCH", "requestTemplates": { "application/json": "{\n \"a\": \"$input.params('operand1')\",\n \"b\": \"$input.params('operand2')\", \n \"op\": \"$input.params('operator')\" \n}" }, "type": "AWS", "uri": "arn:aws:apigateway:us-west-2:lambda:path//2015-03-31/functions/arn:aws:lambda:us-west-2:123456789012:function:Calc/invocations", "_embedded": { "integration:responses": { "_links": { "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.operator": "integration.response.body.op", "method.response.header.operand_2": "integration.response.body.b", "method.response.header.operand_1": "integration.response.body.a" }, "responseTemplates": { "application/json": "#set($res = $input.path('$'))\n{\n \"result\": \"$res.a, $res.b, $res.op => $res.c\",\n \"a\" : \"$res.a\",\n \"b\" : \"$res.b\",\n \"op\" : \"$res.op\",\n \"c\" : \"$res.c\"\n}" }, "selectionPattern": "", "statusCode": "200" } } }@ @
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-integration.html AWS CLI>
-- * 'methodResponses' - Gets a method response associated with a given HTTP status code.
--
-- The collection of method responses are encapsulated in a key-value map, where the key is a response's HTTP status code and the value is a 'MethodResponse' resource that specifies the response returned to the caller from the back end through the integration response.
-- __Example: Get a 200 OK response of a GET method__
-- __Request__
--
-- @@GET /restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200 HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com Content-Length: 117 X-Amz-Date: 20160613T215008Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160613/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}@ @ __Response__
-- The successful response returns a @200 OK@ status code and a payload similar to the following:
-- @@{ "_links": { "curies": { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-response-{rel}.html", "name": "methodresponse", "templated": true }, "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200", "title": "200" }, "methodresponse:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200" }, "methodresponse:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200" } }, "responseModels": { "application/json": "Empty" }, "responseParameters": { "method.response.header.operator": false, "method.response.header.operand_2": false, "method.response.header.operand_1": false }, "statusCode": "200" }@ @
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-method-response.html AWS CLI>
-- * 'operationName' - A human-friendly operation identifier for the method. For example, you can assign the @operationName@ of @ListPets@ for the @GET /pets@ method in the @PetStore@ example.
-- * 'requestModels' - A key-value map specifying data schemas, represented by 'Model' resources, (as the mapped value) of the request payloads of given content types (as the mapping key).
-- * 'requestParameters' - A key-value map defining required or optional method request parameters that can be accepted by API Gateway. A key is a method request parameter name matching the pattern of @method.request.{location}.{name}@ , where @location@ is @querystring@ , @path@ , or @header@ and @name@ is a valid and unique parameter name. The value associated with the key is a Boolean flag indicating whether the parameter is required (@true@ ) or optional (@false@ ). The method request parameter names defined here are available in 'Integration' to be mapped to integration request parameters or templates.
-- * 'requestValidatorId' - The identifier of a 'RequestValidator' for request validation.
mkMethod ::
  Method
mkMethod =
  Method'
    { methodResponses = Lude.Nothing,
      httpMethod = Lude.Nothing,
      authorizationScopes = Lude.Nothing,
      requestValidatorId = Lude.Nothing,
      requestModels = Lude.Nothing,
      requestParameters = Lude.Nothing,
      authorizerId = Lude.Nothing,
      operationName = Lude.Nothing,
      authorizationType = Lude.Nothing,
      apiKeyRequired = Lude.Nothing,
      methodIntegration = Lude.Nothing
    }

-- | Gets a method response associated with a given HTTP status code.
--
-- The collection of method responses are encapsulated in a key-value map, where the key is a response's HTTP status code and the value is a 'MethodResponse' resource that specifies the response returned to the caller from the back end through the integration response.
-- __Example: Get a 200 OK response of a GET method__
-- __Request__
--
-- @@GET /restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200 HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com Content-Length: 117 X-Amz-Date: 20160613T215008Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160613/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}@ @ __Response__
-- The successful response returns a @200 OK@ status code and a payload similar to the following:
-- @@{ "_links": { "curies": { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-method-response-{rel}.html", "name": "methodresponse", "templated": true }, "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200", "title": "200" }, "methodresponse:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200" }, "methodresponse:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/responses/200" } }, "responseModels": { "application/json": "Empty" }, "responseParameters": { "method.response.header.operator": false, "method.response.header.operand_2": false, "method.response.header.operand_1": false }, "statusCode": "200" }@ @
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-method-response.html AWS CLI>
--
-- /Note:/ Consider using 'methodResponses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMethodResponses :: Lens.Lens' Method (Lude.Maybe (Lude.HashMap Lude.Text (MethodResponse)))
mMethodResponses = Lens.lens (methodResponses :: Method -> Lude.Maybe (Lude.HashMap Lude.Text (MethodResponse))) (\s a -> s {methodResponses = a} :: Method)
{-# DEPRECATED mMethodResponses "Use generic-lens or generic-optics with 'methodResponses' instead." #-}

-- | The method's HTTP verb.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mHttpMethod :: Lens.Lens' Method (Lude.Maybe Lude.Text)
mHttpMethod = Lens.lens (httpMethod :: Method -> Lude.Maybe Lude.Text) (\s a -> s {httpMethod = a} :: Method)
{-# DEPRECATED mHttpMethod "Use generic-lens or generic-optics with 'httpMethod' instead." #-}

-- | A list of authorization scopes configured on the method. The scopes are used with a @COGNITO_USER_POOLS@ authorizer to authorize the method invocation. The authorization works by matching the method scopes against the scopes parsed from the access token in the incoming request. The method invocation is authorized if any method scopes matches a claimed scope in the access token. Otherwise, the invocation is not authorized. When the method scope is configured, the client must provide an access token instead of an identity token for authorization purposes.
--
-- /Note:/ Consider using 'authorizationScopes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAuthorizationScopes :: Lens.Lens' Method (Lude.Maybe [Lude.Text])
mAuthorizationScopes = Lens.lens (authorizationScopes :: Method -> Lude.Maybe [Lude.Text]) (\s a -> s {authorizationScopes = a} :: Method)
{-# DEPRECATED mAuthorizationScopes "Use generic-lens or generic-optics with 'authorizationScopes' instead." #-}

-- | The identifier of a 'RequestValidator' for request validation.
--
-- /Note:/ Consider using 'requestValidatorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mRequestValidatorId :: Lens.Lens' Method (Lude.Maybe Lude.Text)
mRequestValidatorId = Lens.lens (requestValidatorId :: Method -> Lude.Maybe Lude.Text) (\s a -> s {requestValidatorId = a} :: Method)
{-# DEPRECATED mRequestValidatorId "Use generic-lens or generic-optics with 'requestValidatorId' instead." #-}

-- | A key-value map specifying data schemas, represented by 'Model' resources, (as the mapped value) of the request payloads of given content types (as the mapping key).
--
-- /Note:/ Consider using 'requestModels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mRequestModels :: Lens.Lens' Method (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
mRequestModels = Lens.lens (requestModels :: Method -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {requestModels = a} :: Method)
{-# DEPRECATED mRequestModels "Use generic-lens or generic-optics with 'requestModels' instead." #-}

-- | A key-value map defining required or optional method request parameters that can be accepted by API Gateway. A key is a method request parameter name matching the pattern of @method.request.{location}.{name}@ , where @location@ is @querystring@ , @path@ , or @header@ and @name@ is a valid and unique parameter name. The value associated with the key is a Boolean flag indicating whether the parameter is required (@true@ ) or optional (@false@ ). The method request parameter names defined here are available in 'Integration' to be mapped to integration request parameters or templates.
--
-- /Note:/ Consider using 'requestParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mRequestParameters :: Lens.Lens' Method (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Bool)))
mRequestParameters = Lens.lens (requestParameters :: Method -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Bool))) (\s a -> s {requestParameters = a} :: Method)
{-# DEPRECATED mRequestParameters "Use generic-lens or generic-optics with 'requestParameters' instead." #-}

-- | The identifier of an 'Authorizer' to use on this method. The @authorizationType@ must be @CUSTOM@ .
--
-- /Note:/ Consider using 'authorizerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAuthorizerId :: Lens.Lens' Method (Lude.Maybe Lude.Text)
mAuthorizerId = Lens.lens (authorizerId :: Method -> Lude.Maybe Lude.Text) (\s a -> s {authorizerId = a} :: Method)
{-# DEPRECATED mAuthorizerId "Use generic-lens or generic-optics with 'authorizerId' instead." #-}

-- | A human-friendly operation identifier for the method. For example, you can assign the @operationName@ of @ListPets@ for the @GET /pets@ method in the @PetStore@ example.
--
-- /Note:/ Consider using 'operationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mOperationName :: Lens.Lens' Method (Lude.Maybe Lude.Text)
mOperationName = Lens.lens (operationName :: Method -> Lude.Maybe Lude.Text) (\s a -> s {operationName = a} :: Method)
{-# DEPRECATED mOperationName "Use generic-lens or generic-optics with 'operationName' instead." #-}

-- | The method's authorization type. Valid values are @NONE@ for open access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user pool.
--
-- /Note:/ Consider using 'authorizationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAuthorizationType :: Lens.Lens' Method (Lude.Maybe Lude.Text)
mAuthorizationType = Lens.lens (authorizationType :: Method -> Lude.Maybe Lude.Text) (\s a -> s {authorizationType = a} :: Method)
{-# DEPRECATED mAuthorizationType "Use generic-lens or generic-optics with 'authorizationType' instead." #-}

-- | A boolean flag specifying whether a valid 'ApiKey' is required to invoke this method.
--
-- /Note:/ Consider using 'apiKeyRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mApiKeyRequired :: Lens.Lens' Method (Lude.Maybe Lude.Bool)
mApiKeyRequired = Lens.lens (apiKeyRequired :: Method -> Lude.Maybe Lude.Bool) (\s a -> s {apiKeyRequired = a} :: Method)
{-# DEPRECATED mApiKeyRequired "Use generic-lens or generic-optics with 'apiKeyRequired' instead." #-}

-- | Gets the method's integration responsible for passing the client-submitted request to the back end and performing necessary transformations to make the request compliant with the back end.
--
--
-- __Example: __
-- __Request__
--
-- @@GET /restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com Content-Length: 117 X-Amz-Date: 20160613T213210Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160613/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash}@ @ __Response__
-- The successful response returns a @200 OK@ status code and a payload similar to the following:
-- @@{ "_links": { "curies": [ { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-{rel}.html", "name": "integration", "templated": true }, { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true } ], "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integration:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integration:responses": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integration:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration" }, "integrationresponse:put": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/{status_code}", "templated": true } }, "cacheKeyParameters": [], "cacheNamespace": "0cjtch", "credentials": "arn:aws:iam::123456789012:role/apigAwsProxyRole", "httpMethod": "POST", "passthroughBehavior": "WHEN_NO_MATCH", "requestTemplates": { "application/json": "{\n \"a\": \"$input.params('operand1')\",\n \"b\": \"$input.params('operand2')\", \n \"op\": \"$input.params('operator')\" \n}" }, "type": "AWS", "uri": "arn:aws:apigateway:us-west-2:lambda:path//2015-03-31/functions/arn:aws:lambda:us-west-2:123456789012:function:Calc/invocations", "_embedded": { "integration:responses": { "_links": { "self": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200", "name": "200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/uojnr9hd57/resources/0cjtch/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.operator": "integration.response.body.op", "method.response.header.operand_2": "integration.response.body.b", "method.response.header.operand_1": "integration.response.body.a" }, "responseTemplates": { "application/json": "#set($res = $input.path('$'))\n{\n \"result\": \"$res.a, $res.b, $res.op => $res.c\",\n \"a\" : \"$res.a\",\n \"b\" : \"$res.b\",\n \"op\" : \"$res.op\",\n \"c\" : \"$res.c\"\n}" }, "selectionPattern": "", "statusCode": "200" } } }@ @
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-integration.html AWS CLI>
--
-- /Note:/ Consider using 'methodIntegration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMethodIntegration :: Lens.Lens' Method (Lude.Maybe Integration)
mMethodIntegration = Lens.lens (methodIntegration :: Method -> Lude.Maybe Integration) (\s a -> s {methodIntegration = a} :: Method)
{-# DEPRECATED mMethodIntegration "Use generic-lens or generic-optics with 'methodIntegration' instead." #-}

instance Lude.FromJSON Method where
  parseJSON =
    Lude.withObject
      "Method"
      ( \x ->
          Method'
            Lude.<$> (x Lude..:? "methodResponses" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "httpMethod")
            Lude.<*> (x Lude..:? "authorizationScopes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "requestValidatorId")
            Lude.<*> (x Lude..:? "requestModels" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "requestParameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "authorizerId")
            Lude.<*> (x Lude..:? "operationName")
            Lude.<*> (x Lude..:? "authorizationType")
            Lude.<*> (x Lude..:? "apiKeyRequired")
            Lude.<*> (x Lude..:? "methodIntegration")
      )
