{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Integration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Integration
  ( Integration (..),

    -- * Smart constructor
    mkIntegration,

    -- * Lenses
    iHttpMethod,
    iRequestTemplates,
    iCredentials,
    iConnectionId,
    iRequestParameters,
    iContentHandling,
    iPassthroughBehavior,
    iUri,
    iIntegrationResponses,
    iTlsConfig,
    iCacheNamespace,
    iTimeoutInMillis,
    iType,
    iConnectionType,
    iCacheKeyParameters,
  )
where

import Network.AWS.APIGateway.Types.ConnectionType
import Network.AWS.APIGateway.Types.ContentHandlingStrategy
import Network.AWS.APIGateway.Types.IntegrationResponse
import Network.AWS.APIGateway.Types.IntegrationType
import Network.AWS.APIGateway.Types.TLSConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an HTTP, HTTP_PROXY, AWS, AWS_PROXY, or Mock integration.
--
-- In the API Gateway console, the built-in Lambda integration is an AWS integration.<https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Creating an API>
--
-- /See:/ 'mkIntegration' smart constructor.
data Integration = Integration'
  { httpMethod :: Lude.Maybe Lude.Text,
    requestTemplates ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    credentials :: Lude.Maybe Lude.Text,
    connectionId :: Lude.Maybe Lude.Text,
    requestParameters ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    contentHandling :: Lude.Maybe ContentHandlingStrategy,
    passthroughBehavior :: Lude.Maybe Lude.Text,
    uri :: Lude.Maybe Lude.Text,
    integrationResponses ::
      Lude.Maybe (Lude.HashMap Lude.Text (IntegrationResponse)),
    tlsConfig :: Lude.Maybe TLSConfig,
    cacheNamespace :: Lude.Maybe Lude.Text,
    timeoutInMillis :: Lude.Maybe Lude.Int,
    type' :: Lude.Maybe IntegrationType,
    connectionType :: Lude.Maybe ConnectionType,
    cacheKeyParameters :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Integration' with the minimum fields required to make a request.
--
-- * 'cacheKeyParameters' - A list of request parameters whose values API Gateway caches. To be valid values for @cacheKeyParameters@ , these parameters must also be specified for 'Method' @requestParameters@ .
-- * 'cacheNamespace' - Specifies a group of related cached parameters. By default, API Gateway uses the resource ID as the @cacheNamespace@ . You can specify the same @cacheNamespace@ across resources to return the same cached data for requests to different resources.
-- * 'connectionId' - The (<https://docs.aws.amazon.com/apigateway/api-reference/resource/vpc-link/#id @id@ > ) of the 'VpcLink' used for the integration when @connectionType=VPC_LINK@ and undefined, otherwise.
-- * 'connectionType' - The type of the network connection to the integration endpoint. The valid value is @INTERNET@ for connections through the public routable internet or @VPC_LINK@ for private connections between API Gateway and a network load balancer in a VPC. The default value is @INTERNET@ .
-- * 'contentHandling' - Specifies how to handle request payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:
--
--
--     * @CONVERT_TO_BINARY@ : Converts a request payload from a Base64-encoded string to the corresponding binary blob.
--
--
--     * @CONVERT_TO_TEXT@ : Converts a request payload from a binary blob to a Base64-encoded string.
--
--
-- If this property is not defined, the request payload will be passed through from the method request to integration request without modification, provided that the @passthroughBehavior@ is configured to support payload pass-through.
-- * 'credentials' - Specifies the credentials required for the integration, if any. For AWS integrations, three options are available. To specify an IAM Role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To require that the caller's identity be passed through from the request, specify the string @arn:aws:iam::\*:user/\*@ . To use resource-based permissions on supported AWS services, specify null.
-- * 'httpMethod' - Specifies the integration's HTTP method type.
-- * 'integrationResponses' - Specifies the integration's responses.
--
--
-- __Example: Get integration responses of a method__
-- __Request__
--
-- @@GET /restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200 HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160607T191449Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160607/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash} @ @ __Response__
-- The successful response returns @200 OK@ status and a payload as follows:
-- @@{ "_links": { "curies": { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true }, "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.Content-Type": "'application/xml'" }, "responseTemplates": { "application/json": "$util.urlDecode(\"%3CkinesisStreams%3E#foreach($stream in $input.path('$.StreamNames'))%3Cstream%3E%3Cname%3E$stream%3C/name%3E%3C/stream%3E#end%3C/kinesisStreams%3E\")\n" }, "statusCode": "200" }@ @
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Creating an API>
-- * 'passthroughBehavior' - Specifies how the method request body of an unmapped content type will be passed through the integration request to the back end without transformation. A content type is unmapped if no mapping template is defined in the integration or the content type does not match any of the mapped content types, as specified in @requestTemplates@ . The valid value is one of the following:
--
--
--     * @WHEN_NO_MATCH@ : passes the method request body through the integration request to the back end without transformation when the method request content type does not match any content type associated with the mapping templates defined in the integration request.
--
--     * @WHEN_NO_TEMPLATES@ : passes the method request body through the integration request to the back end without transformation when no mapping template is defined in the integration request. If a template is defined when this option is selected, the method request of an unmapped content-type will be rejected with an HTTP @415 Unsupported Media Type@ response.
--
--     * @NEVER@ : rejects the method request with an HTTP @415 Unsupported Media Type@ response when either the method request content type does not match any content type associated with the mapping templates defined in the integration request or no mapping template is defined in the integration request.
--
-- * 'requestParameters' - A key-value map specifying request parameters that are passed from the method request to the back end. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the back end. The method request parameter value must match the pattern of @method.request.{location}.{name}@ , where @location@ is @querystring@ , @path@ , or @header@ and @name@ must be a valid and unique method request parameter name.
-- * 'requestTemplates' - Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
-- * 'timeoutInMillis' - Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
-- * 'tlsConfig' - Specifies the TLS configuration for an integration.
-- * 'type'' - Specifies an API method integration type. The valid value is one of the following:
--
--
--     * @AWS@ : for integrating the API method request with an AWS service action, including the Lambda function-invoking action. With the Lambda function-invoking action, this is referred to as the Lambda custom integration. With any other AWS service action, this is known as AWS integration.
--
--     * @AWS_PROXY@ : for integrating the API method request with the Lambda function-invoking action with the client request passed through as-is. This integration is also referred to as the Lambda proxy integration.
--
--     * @HTTP@ : for integrating the API method request with an HTTP endpoint, including a private HTTP endpoint within a VPC. This integration is also referred to as the HTTP custom integration.
--
--     * @HTTP_PROXY@ : for integrating the API method request with an HTTP endpoint, including a private HTTP endpoint within a VPC, with the client request passed through as-is. This is also referred to as the HTTP proxy integration.
--
--     * @MOCK@ : for integrating the API method request with API Gateway as a "loop-back" endpoint without invoking any backend.
--
-- For the HTTP and HTTP proxy integrations, each integration can specify a protocol (@http/https@ ), port and path. Standard 80 and 443 ports are supported as well as custom ports above 1024. An HTTP or HTTP proxy integration with a @connectionType@ of @VPC_LINK@ is referred to as a private integration and uses a 'VpcLink' to connect API Gateway to a network load balancer of a VPC.
-- * 'uri' - Specifies Uniform Resource Identifier (URI) of the integration endpoint.
--
--
--     * For @HTTP@ or @HTTP_PROXY@ integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> , for either standard integration, where @connectionType@ is not @VPC_LINK@ , or private integration, where @connectionType@ is @VPC_LINK@ . For a private HTTP integration, the URI is not used for routing.
--
--
--     * For @AWS@ or @AWS_PROXY@ integrations, the URI is of the form @arn:aws:apigateway:{region}:{subdomain.service|service}:path|action/{service_api}@ . Here, @{Region}@ is the API Gateway region (e.g., @us-east-1@ ); @{service}@ is the name of the integrated AWS service (e.g., @s3@ ); and @{subdomain}@ is a designated subdomain supported by certain AWS service for fast host-name lookup. @action@ can be used for an AWS service action-based API, using an @Action={name}&{p1}={v1}&p2={v2}...@ query string. The ensuing @{service_api}@ refers to a supported action @{name}@ plus any required input parameters. Alternatively, @path@ can be used for an AWS service path-based API. The ensuing @service_api@ refers to the path to an AWS service resource, including the region of the integrated AWS service, if applicable. For example, for integration with the S3 API of @<https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html GetObject> @ , the @uri@ can be either @arn:aws:apigateway:us-west-2:s3:action/GetObject&Bucket={bucket}&Key={key}@ or @arn:aws:apigateway:us-west-2:s3:path/{bucket}/{key}@
mkIntegration ::
  Integration
mkIntegration =
  Integration'
    { httpMethod = Lude.Nothing,
      requestTemplates = Lude.Nothing,
      credentials = Lude.Nothing,
      connectionId = Lude.Nothing,
      requestParameters = Lude.Nothing,
      contentHandling = Lude.Nothing,
      passthroughBehavior = Lude.Nothing,
      uri = Lude.Nothing,
      integrationResponses = Lude.Nothing,
      tlsConfig = Lude.Nothing,
      cacheNamespace = Lude.Nothing,
      timeoutInMillis = Lude.Nothing,
      type' = Lude.Nothing,
      connectionType = Lude.Nothing,
      cacheKeyParameters = Lude.Nothing
    }

-- | Specifies the integration's HTTP method type.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iHttpMethod :: Lens.Lens' Integration (Lude.Maybe Lude.Text)
iHttpMethod = Lens.lens (httpMethod :: Integration -> Lude.Maybe Lude.Text) (\s a -> s {httpMethod = a} :: Integration)
{-# DEPRECATED iHttpMethod "Use generic-lens or generic-optics with 'httpMethod' instead." #-}

-- | Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
--
-- /Note:/ Consider using 'requestTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRequestTemplates :: Lens.Lens' Integration (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
iRequestTemplates = Lens.lens (requestTemplates :: Integration -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {requestTemplates = a} :: Integration)
{-# DEPRECATED iRequestTemplates "Use generic-lens or generic-optics with 'requestTemplates' instead." #-}

-- | Specifies the credentials required for the integration, if any. For AWS integrations, three options are available. To specify an IAM Role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To require that the caller's identity be passed through from the request, specify the string @arn:aws:iam::\*:user/\*@ . To use resource-based permissions on supported AWS services, specify null.
--
-- /Note:/ Consider using 'credentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCredentials :: Lens.Lens' Integration (Lude.Maybe Lude.Text)
iCredentials = Lens.lens (credentials :: Integration -> Lude.Maybe Lude.Text) (\s a -> s {credentials = a} :: Integration)
{-# DEPRECATED iCredentials "Use generic-lens or generic-optics with 'credentials' instead." #-}

-- | The (<https://docs.aws.amazon.com/apigateway/api-reference/resource/vpc-link/#id @id@ > ) of the 'VpcLink' used for the integration when @connectionType=VPC_LINK@ and undefined, otherwise.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iConnectionId :: Lens.Lens' Integration (Lude.Maybe Lude.Text)
iConnectionId = Lens.lens (connectionId :: Integration -> Lude.Maybe Lude.Text) (\s a -> s {connectionId = a} :: Integration)
{-# DEPRECATED iConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | A key-value map specifying request parameters that are passed from the method request to the back end. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the back end. The method request parameter value must match the pattern of @method.request.{location}.{name}@ , where @location@ is @querystring@ , @path@ , or @header@ and @name@ must be a valid and unique method request parameter name.
--
-- /Note:/ Consider using 'requestParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRequestParameters :: Lens.Lens' Integration (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
iRequestParameters = Lens.lens (requestParameters :: Integration -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {requestParameters = a} :: Integration)
{-# DEPRECATED iRequestParameters "Use generic-lens or generic-optics with 'requestParameters' instead." #-}

-- | Specifies how to handle request payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:
--
--
--     * @CONVERT_TO_BINARY@ : Converts a request payload from a Base64-encoded string to the corresponding binary blob.
--
--
--     * @CONVERT_TO_TEXT@ : Converts a request payload from a binary blob to a Base64-encoded string.
--
--
-- If this property is not defined, the request payload will be passed through from the method request to integration request without modification, provided that the @passthroughBehavior@ is configured to support payload pass-through.
--
-- /Note:/ Consider using 'contentHandling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iContentHandling :: Lens.Lens' Integration (Lude.Maybe ContentHandlingStrategy)
iContentHandling = Lens.lens (contentHandling :: Integration -> Lude.Maybe ContentHandlingStrategy) (\s a -> s {contentHandling = a} :: Integration)
{-# DEPRECATED iContentHandling "Use generic-lens or generic-optics with 'contentHandling' instead." #-}

-- | Specifies how the method request body of an unmapped content type will be passed through the integration request to the back end without transformation. A content type is unmapped if no mapping template is defined in the integration or the content type does not match any of the mapped content types, as specified in @requestTemplates@ . The valid value is one of the following:
--
--
--     * @WHEN_NO_MATCH@ : passes the method request body through the integration request to the back end without transformation when the method request content type does not match any content type associated with the mapping templates defined in the integration request.
--
--     * @WHEN_NO_TEMPLATES@ : passes the method request body through the integration request to the back end without transformation when no mapping template is defined in the integration request. If a template is defined when this option is selected, the method request of an unmapped content-type will be rejected with an HTTP @415 Unsupported Media Type@ response.
--
--     * @NEVER@ : rejects the method request with an HTTP @415 Unsupported Media Type@ response when either the method request content type does not match any content type associated with the mapping templates defined in the integration request or no mapping template is defined in the integration request.
--
--
-- /Note:/ Consider using 'passthroughBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPassthroughBehavior :: Lens.Lens' Integration (Lude.Maybe Lude.Text)
iPassthroughBehavior = Lens.lens (passthroughBehavior :: Integration -> Lude.Maybe Lude.Text) (\s a -> s {passthroughBehavior = a} :: Integration)
{-# DEPRECATED iPassthroughBehavior "Use generic-lens or generic-optics with 'passthroughBehavior' instead." #-}

-- | Specifies Uniform Resource Identifier (URI) of the integration endpoint.
--
--
--     * For @HTTP@ or @HTTP_PROXY@ integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> , for either standard integration, where @connectionType@ is not @VPC_LINK@ , or private integration, where @connectionType@ is @VPC_LINK@ . For a private HTTP integration, the URI is not used for routing.
--
--
--     * For @AWS@ or @AWS_PROXY@ integrations, the URI is of the form @arn:aws:apigateway:{region}:{subdomain.service|service}:path|action/{service_api}@ . Here, @{Region}@ is the API Gateway region (e.g., @us-east-1@ ); @{service}@ is the name of the integrated AWS service (e.g., @s3@ ); and @{subdomain}@ is a designated subdomain supported by certain AWS service for fast host-name lookup. @action@ can be used for an AWS service action-based API, using an @Action={name}&{p1}={v1}&p2={v2}...@ query string. The ensuing @{service_api}@ refers to a supported action @{name}@ plus any required input parameters. Alternatively, @path@ can be used for an AWS service path-based API. The ensuing @service_api@ refers to the path to an AWS service resource, including the region of the integrated AWS service, if applicable. For example, for integration with the S3 API of @<https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html GetObject> @ , the @uri@ can be either @arn:aws:apigateway:us-west-2:s3:action/GetObject&Bucket={bucket}&Key={key}@ or @arn:aws:apigateway:us-west-2:s3:path/{bucket}/{key}@
--
--
--
-- /Note:/ Consider using 'uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iUri :: Lens.Lens' Integration (Lude.Maybe Lude.Text)
iUri = Lens.lens (uri :: Integration -> Lude.Maybe Lude.Text) (\s a -> s {uri = a} :: Integration)
{-# DEPRECATED iUri "Use generic-lens or generic-optics with 'uri' instead." #-}

-- | Specifies the integration's responses.
--
--
-- __Example: Get integration responses of a method__
-- __Request__
--
-- @@GET /restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200 HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160607T191449Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160607/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash} @ @ __Response__
-- The successful response returns @200 OK@ status and a payload as follows:
-- @@{ "_links": { "curies": { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true }, "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.Content-Type": "'application/xml'" }, "responseTemplates": { "application/json": "$util.urlDecode(\"%3CkinesisStreams%3E#foreach($stream in $input.path('$.StreamNames'))%3Cstream%3E%3Cname%3E$stream%3C/name%3E%3C/stream%3E#end%3C/kinesisStreams%3E\")\n" }, "statusCode": "200" }@ @
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Creating an API>
--
-- /Note:/ Consider using 'integrationResponses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iIntegrationResponses :: Lens.Lens' Integration (Lude.Maybe (Lude.HashMap Lude.Text (IntegrationResponse)))
iIntegrationResponses = Lens.lens (integrationResponses :: Integration -> Lude.Maybe (Lude.HashMap Lude.Text (IntegrationResponse))) (\s a -> s {integrationResponses = a} :: Integration)
{-# DEPRECATED iIntegrationResponses "Use generic-lens or generic-optics with 'integrationResponses' instead." #-}

-- | Specifies the TLS configuration for an integration.
--
-- /Note:/ Consider using 'tlsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTlsConfig :: Lens.Lens' Integration (Lude.Maybe TLSConfig)
iTlsConfig = Lens.lens (tlsConfig :: Integration -> Lude.Maybe TLSConfig) (\s a -> s {tlsConfig = a} :: Integration)
{-# DEPRECATED iTlsConfig "Use generic-lens or generic-optics with 'tlsConfig' instead." #-}

-- | Specifies a group of related cached parameters. By default, API Gateway uses the resource ID as the @cacheNamespace@ . You can specify the same @cacheNamespace@ across resources to return the same cached data for requests to different resources.
--
-- /Note:/ Consider using 'cacheNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCacheNamespace :: Lens.Lens' Integration (Lude.Maybe Lude.Text)
iCacheNamespace = Lens.lens (cacheNamespace :: Integration -> Lude.Maybe Lude.Text) (\s a -> s {cacheNamespace = a} :: Integration)
{-# DEPRECATED iCacheNamespace "Use generic-lens or generic-optics with 'cacheNamespace' instead." #-}

-- | Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
--
-- /Note:/ Consider using 'timeoutInMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTimeoutInMillis :: Lens.Lens' Integration (Lude.Maybe Lude.Int)
iTimeoutInMillis = Lens.lens (timeoutInMillis :: Integration -> Lude.Maybe Lude.Int) (\s a -> s {timeoutInMillis = a} :: Integration)
{-# DEPRECATED iTimeoutInMillis "Use generic-lens or generic-optics with 'timeoutInMillis' instead." #-}

-- | Specifies an API method integration type. The valid value is one of the following:
--
--
--     * @AWS@ : for integrating the API method request with an AWS service action, including the Lambda function-invoking action. With the Lambda function-invoking action, this is referred to as the Lambda custom integration. With any other AWS service action, this is known as AWS integration.
--
--     * @AWS_PROXY@ : for integrating the API method request with the Lambda function-invoking action with the client request passed through as-is. This integration is also referred to as the Lambda proxy integration.
--
--     * @HTTP@ : for integrating the API method request with an HTTP endpoint, including a private HTTP endpoint within a VPC. This integration is also referred to as the HTTP custom integration.
--
--     * @HTTP_PROXY@ : for integrating the API method request with an HTTP endpoint, including a private HTTP endpoint within a VPC, with the client request passed through as-is. This is also referred to as the HTTP proxy integration.
--
--     * @MOCK@ : for integrating the API method request with API Gateway as a "loop-back" endpoint without invoking any backend.
--
-- For the HTTP and HTTP proxy integrations, each integration can specify a protocol (@http/https@ ), port and path. Standard 80 and 443 ports are supported as well as custom ports above 1024. An HTTP or HTTP proxy integration with a @connectionType@ of @VPC_LINK@ is referred to as a private integration and uses a 'VpcLink' to connect API Gateway to a network load balancer of a VPC.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iType :: Lens.Lens' Integration (Lude.Maybe IntegrationType)
iType = Lens.lens (type' :: Integration -> Lude.Maybe IntegrationType) (\s a -> s {type' = a} :: Integration)
{-# DEPRECATED iType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The type of the network connection to the integration endpoint. The valid value is @INTERNET@ for connections through the public routable internet or @VPC_LINK@ for private connections between API Gateway and a network load balancer in a VPC. The default value is @INTERNET@ .
--
-- /Note:/ Consider using 'connectionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iConnectionType :: Lens.Lens' Integration (Lude.Maybe ConnectionType)
iConnectionType = Lens.lens (connectionType :: Integration -> Lude.Maybe ConnectionType) (\s a -> s {connectionType = a} :: Integration)
{-# DEPRECATED iConnectionType "Use generic-lens or generic-optics with 'connectionType' instead." #-}

-- | A list of request parameters whose values API Gateway caches. To be valid values for @cacheKeyParameters@ , these parameters must also be specified for 'Method' @requestParameters@ .
--
-- /Note:/ Consider using 'cacheKeyParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCacheKeyParameters :: Lens.Lens' Integration (Lude.Maybe [Lude.Text])
iCacheKeyParameters = Lens.lens (cacheKeyParameters :: Integration -> Lude.Maybe [Lude.Text]) (\s a -> s {cacheKeyParameters = a} :: Integration)
{-# DEPRECATED iCacheKeyParameters "Use generic-lens or generic-optics with 'cacheKeyParameters' instead." #-}

instance Lude.FromJSON Integration where
  parseJSON =
    Lude.withObject
      "Integration"
      ( \x ->
          Integration'
            Lude.<$> (x Lude..:? "httpMethod")
            Lude.<*> (x Lude..:? "requestTemplates" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "credentials")
            Lude.<*> (x Lude..:? "connectionId")
            Lude.<*> (x Lude..:? "requestParameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "contentHandling")
            Lude.<*> (x Lude..:? "passthroughBehavior")
            Lude.<*> (x Lude..:? "uri")
            Lude.<*> (x Lude..:? "integrationResponses" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "tlsConfig")
            Lude.<*> (x Lude..:? "cacheNamespace")
            Lude.<*> (x Lude..:? "timeoutInMillis")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "connectionType")
            Lude.<*> (x Lude..:? "cacheKeyParameters" Lude..!= Lude.mempty)
      )
