{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.Integration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.Integration
  ( Integration (..)
  -- * Smart constructor
  , mkIntegration
  -- * Lenses
  , iCacheKeyParameters
  , iCacheNamespace
  , iConnectionId
  , iConnectionType
  , iContentHandling
  , iCredentials
  , iHttpMethod
  , iIntegrationResponses
  , iPassthroughBehavior
  , iRequestParameters
  , iRequestTemplates
  , iTimeoutInMillis
  , iTlsConfig
  , iType
  , iUri
  ) where

import qualified Network.AWS.ApiGateway.Types.ConnectionType as Types
import qualified Network.AWS.ApiGateway.Types.ContentHandlingStrategy as Types
import qualified Network.AWS.ApiGateway.Types.IntegrationResponse as Types
import qualified Network.AWS.ApiGateway.Types.IntegrationType as Types
import qualified Network.AWS.ApiGateway.Types.TlsConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents an HTTP, HTTP_PROXY, AWS, AWS_PROXY, or Mock integration.
--
-- In the API Gateway console, the built-in Lambda integration is an AWS integration.<https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Creating an API> 
--
-- /See:/ 'mkIntegration' smart constructor.
data Integration = Integration'
  { cacheKeyParameters :: Core.Maybe [Core.Text]
    -- ^ A list of request parameters whose values API Gateway caches. To be valid values for @cacheKeyParameters@ , these parameters must also be specified for 'Method' @requestParameters@ .
  , cacheNamespace :: Core.Maybe Core.Text
    -- ^ Specifies a group of related cached parameters. By default, API Gateway uses the resource ID as the @cacheNamespace@ . You can specify the same @cacheNamespace@ across resources to return the same cached data for requests to different resources.
  , connectionId :: Core.Maybe Core.Text
    -- ^ The (<https://docs.aws.amazon.com/apigateway/api-reference/resource/vpc-link/#id @id@ > ) of the 'VpcLink' used for the integration when @connectionType=VPC_LINK@ and undefined, otherwise.
  , connectionType :: Core.Maybe Types.ConnectionType
    -- ^ The type of the network connection to the integration endpoint. The valid value is @INTERNET@ for connections through the public routable internet or @VPC_LINK@ for private connections between API Gateway and a network load balancer in a VPC. The default value is @INTERNET@ .
  , contentHandling :: Core.Maybe Types.ContentHandlingStrategy
    -- ^ Specifies how to handle request payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:
--
--
--     * @CONVERT_TO_BINARY@ : Converts a request payload from a Base64-encoded string to the corresponding binary blob.
--
--
--     * @CONVERT_TO_TEXT@ : Converts a request payload from a binary blob to a Base64-encoded string.
--
--
-- If this property is not defined, the request payload will be passed through from the method request to integration request without modification, provided that the @passthroughBehavior@ is configured to support payload pass-through.
  , credentials :: Core.Maybe Core.Text
    -- ^ Specifies the credentials required for the integration, if any. For AWS integrations, three options are available. To specify an IAM Role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To require that the caller's identity be passed through from the request, specify the string @arn:aws:iam::\*:user/\*@ . To use resource-based permissions on supported AWS services, specify null.
  , httpMethod :: Core.Maybe Core.Text
    -- ^ Specifies the integration's HTTP method type.
  , integrationResponses :: Core.Maybe (Core.HashMap Core.Text Types.IntegrationResponse)
    -- ^ Specifies the integration's responses.
--
--
-- __Example: Get integration responses of a method__ 
-- __Request__ 
--
-- @@GET /restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200 HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160607T191449Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160607/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash} @ @ __Response__ 
-- The successful response returns @200 OK@ status and a payload as follows:
-- @@{ "_links": { "curies": { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true }, "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.Content-Type": "'application/xml'" }, "responseTemplates": { "application/json": "$util.urlDecode(\"%3CkinesisStreams%3E#foreach($stream in $input.path('$.StreamNames'))%3Cstream%3E%3Cname%3E$stream%3C/name%3E%3C/stream%3E#end%3C/kinesisStreams%3E\")\n" }, "statusCode": "200" }@ @ 
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Creating an API> 
  , passthroughBehavior :: Core.Maybe Core.Text
    -- ^ Specifies how the method request body of an unmapped content type will be passed through the integration request to the back end without transformation. A content type is unmapped if no mapping template is defined in the integration or the content type does not match any of the mapped content types, as specified in @requestTemplates@ . The valid value is one of the following: 
--
--
--     * @WHEN_NO_MATCH@ : passes the method request body through the integration request to the back end without transformation when the method request content type does not match any content type associated with the mapping templates defined in the integration request. 
--
--     * @WHEN_NO_TEMPLATES@ : passes the method request body through the integration request to the back end without transformation when no mapping template is defined in the integration request. If a template is defined when this option is selected, the method request of an unmapped content-type will be rejected with an HTTP @415 Unsupported Media Type@ response. 
--
--     * @NEVER@ : rejects the method request with an HTTP @415 Unsupported Media Type@ response when either the method request content type does not match any content type associated with the mapping templates defined in the integration request or no mapping template is defined in the integration request. 
--
  , requestParameters :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A key-value map specifying request parameters that are passed from the method request to the back end. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the back end. The method request parameter value must match the pattern of @method.request.{location}.{name}@ , where @location@ is @querystring@ , @path@ , or @header@ and @name@ must be a valid and unique method request parameter name.
  , requestTemplates :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
  , timeoutInMillis :: Core.Maybe Core.Int
    -- ^ Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
  , tlsConfig :: Core.Maybe Types.TlsConfig
    -- ^ Specifies the TLS configuration for an integration.
  , type' :: Core.Maybe Types.IntegrationType
    -- ^ Specifies an API method integration type. The valid value is one of the following:
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
  , uri :: Core.Maybe Core.Text
    -- ^ Specifies Uniform Resource Identifier (URI) of the integration endpoint.
--
--
--     * For @HTTP@ or @HTTP_PROXY@ integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> , for either standard integration, where @connectionType@ is not @VPC_LINK@ , or private integration, where @connectionType@ is @VPC_LINK@ . For a private HTTP integration, the URI is not used for routing. 
--
--
--     * For @AWS@ or @AWS_PROXY@ integrations, the URI is of the form @arn:aws:apigateway:{region}:{subdomain.service|service}:path|action/{service_api}@ . Here, @{Region}@ is the API Gateway region (e.g., @us-east-1@ ); @{service}@ is the name of the integrated AWS service (e.g., @s3@ ); and @{subdomain}@ is a designated subdomain supported by certain AWS service for fast host-name lookup. @action@ can be used for an AWS service action-based API, using an @Action={name}&{p1}={v1}&p2={v2}...@ query string. The ensuing @{service_api}@ refers to a supported action @{name}@ plus any required input parameters. Alternatively, @path@ can be used for an AWS service path-based API. The ensuing @service_api@ refers to the path to an AWS service resource, including the region of the integrated AWS service, if applicable. For example, for integration with the S3 API of @<https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html GetObject> @ , the @uri@ can be either @arn:aws:apigateway:us-west-2:s3:action/GetObject&Bucket={bucket}&Key={key}@ or @arn:aws:apigateway:us-west-2:s3:path/{bucket}/{key}@ 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Integration' value with any optional fields omitted.
mkIntegration
    :: Integration
mkIntegration
  = Integration'{cacheKeyParameters = Core.Nothing,
                 cacheNamespace = Core.Nothing, connectionId = Core.Nothing,
                 connectionType = Core.Nothing, contentHandling = Core.Nothing,
                 credentials = Core.Nothing, httpMethod = Core.Nothing,
                 integrationResponses = Core.Nothing,
                 passthroughBehavior = Core.Nothing,
                 requestParameters = Core.Nothing, requestTemplates = Core.Nothing,
                 timeoutInMillis = Core.Nothing, tlsConfig = Core.Nothing,
                 type' = Core.Nothing, uri = Core.Nothing}

-- | A list of request parameters whose values API Gateway caches. To be valid values for @cacheKeyParameters@ , these parameters must also be specified for 'Method' @requestParameters@ .
--
-- /Note:/ Consider using 'cacheKeyParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCacheKeyParameters :: Lens.Lens' Integration (Core.Maybe [Core.Text])
iCacheKeyParameters = Lens.field @"cacheKeyParameters"
{-# INLINEABLE iCacheKeyParameters #-}
{-# DEPRECATED cacheKeyParameters "Use generic-lens or generic-optics with 'cacheKeyParameters' instead"  #-}

-- | Specifies a group of related cached parameters. By default, API Gateway uses the resource ID as the @cacheNamespace@ . You can specify the same @cacheNamespace@ across resources to return the same cached data for requests to different resources.
--
-- /Note:/ Consider using 'cacheNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCacheNamespace :: Lens.Lens' Integration (Core.Maybe Core.Text)
iCacheNamespace = Lens.field @"cacheNamespace"
{-# INLINEABLE iCacheNamespace #-}
{-# DEPRECATED cacheNamespace "Use generic-lens or generic-optics with 'cacheNamespace' instead"  #-}

-- | The (<https://docs.aws.amazon.com/apigateway/api-reference/resource/vpc-link/#id @id@ > ) of the 'VpcLink' used for the integration when @connectionType=VPC_LINK@ and undefined, otherwise.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iConnectionId :: Lens.Lens' Integration (Core.Maybe Core.Text)
iConnectionId = Lens.field @"connectionId"
{-# INLINEABLE iConnectionId #-}
{-# DEPRECATED connectionId "Use generic-lens or generic-optics with 'connectionId' instead"  #-}

-- | The type of the network connection to the integration endpoint. The valid value is @INTERNET@ for connections through the public routable internet or @VPC_LINK@ for private connections between API Gateway and a network load balancer in a VPC. The default value is @INTERNET@ .
--
-- /Note:/ Consider using 'connectionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iConnectionType :: Lens.Lens' Integration (Core.Maybe Types.ConnectionType)
iConnectionType = Lens.field @"connectionType"
{-# INLINEABLE iConnectionType #-}
{-# DEPRECATED connectionType "Use generic-lens or generic-optics with 'connectionType' instead"  #-}

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
iContentHandling :: Lens.Lens' Integration (Core.Maybe Types.ContentHandlingStrategy)
iContentHandling = Lens.field @"contentHandling"
{-# INLINEABLE iContentHandling #-}
{-# DEPRECATED contentHandling "Use generic-lens or generic-optics with 'contentHandling' instead"  #-}

-- | Specifies the credentials required for the integration, if any. For AWS integrations, three options are available. To specify an IAM Role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To require that the caller's identity be passed through from the request, specify the string @arn:aws:iam::\*:user/\*@ . To use resource-based permissions on supported AWS services, specify null.
--
-- /Note:/ Consider using 'credentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCredentials :: Lens.Lens' Integration (Core.Maybe Core.Text)
iCredentials = Lens.field @"credentials"
{-# INLINEABLE iCredentials #-}
{-# DEPRECATED credentials "Use generic-lens or generic-optics with 'credentials' instead"  #-}

-- | Specifies the integration's HTTP method type.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iHttpMethod :: Lens.Lens' Integration (Core.Maybe Core.Text)
iHttpMethod = Lens.field @"httpMethod"
{-# INLINEABLE iHttpMethod #-}
{-# DEPRECATED httpMethod "Use generic-lens or generic-optics with 'httpMethod' instead"  #-}

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
iIntegrationResponses :: Lens.Lens' Integration (Core.Maybe (Core.HashMap Core.Text Types.IntegrationResponse))
iIntegrationResponses = Lens.field @"integrationResponses"
{-# INLINEABLE iIntegrationResponses #-}
{-# DEPRECATED integrationResponses "Use generic-lens or generic-optics with 'integrationResponses' instead"  #-}

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
iPassthroughBehavior :: Lens.Lens' Integration (Core.Maybe Core.Text)
iPassthroughBehavior = Lens.field @"passthroughBehavior"
{-# INLINEABLE iPassthroughBehavior #-}
{-# DEPRECATED passthroughBehavior "Use generic-lens or generic-optics with 'passthroughBehavior' instead"  #-}

-- | A key-value map specifying request parameters that are passed from the method request to the back end. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the back end. The method request parameter value must match the pattern of @method.request.{location}.{name}@ , where @location@ is @querystring@ , @path@ , or @header@ and @name@ must be a valid and unique method request parameter name.
--
-- /Note:/ Consider using 'requestParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRequestParameters :: Lens.Lens' Integration (Core.Maybe (Core.HashMap Core.Text Core.Text))
iRequestParameters = Lens.field @"requestParameters"
{-# INLINEABLE iRequestParameters #-}
{-# DEPRECATED requestParameters "Use generic-lens or generic-optics with 'requestParameters' instead"  #-}

-- | Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
--
-- /Note:/ Consider using 'requestTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRequestTemplates :: Lens.Lens' Integration (Core.Maybe (Core.HashMap Core.Text Core.Text))
iRequestTemplates = Lens.field @"requestTemplates"
{-# INLINEABLE iRequestTemplates #-}
{-# DEPRECATED requestTemplates "Use generic-lens or generic-optics with 'requestTemplates' instead"  #-}

-- | Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
--
-- /Note:/ Consider using 'timeoutInMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTimeoutInMillis :: Lens.Lens' Integration (Core.Maybe Core.Int)
iTimeoutInMillis = Lens.field @"timeoutInMillis"
{-# INLINEABLE iTimeoutInMillis #-}
{-# DEPRECATED timeoutInMillis "Use generic-lens or generic-optics with 'timeoutInMillis' instead"  #-}

-- | Specifies the TLS configuration for an integration.
--
-- /Note:/ Consider using 'tlsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTlsConfig :: Lens.Lens' Integration (Core.Maybe Types.TlsConfig)
iTlsConfig = Lens.field @"tlsConfig"
{-# INLINEABLE iTlsConfig #-}
{-# DEPRECATED tlsConfig "Use generic-lens or generic-optics with 'tlsConfig' instead"  #-}

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
iType :: Lens.Lens' Integration (Core.Maybe Types.IntegrationType)
iType = Lens.field @"type'"
{-# INLINEABLE iType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

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
iUri :: Lens.Lens' Integration (Core.Maybe Core.Text)
iUri = Lens.field @"uri"
{-# INLINEABLE iUri #-}
{-# DEPRECATED uri "Use generic-lens or generic-optics with 'uri' instead"  #-}

instance Core.FromJSON Integration where
        parseJSON
          = Core.withObject "Integration" Core.$
              \ x ->
                Integration' Core.<$>
                  (x Core..:? "cacheKeyParameters") Core.<*>
                    x Core..:? "cacheNamespace"
                    Core.<*> x Core..:? "connectionId"
                    Core.<*> x Core..:? "connectionType"
                    Core.<*> x Core..:? "contentHandling"
                    Core.<*> x Core..:? "credentials"
                    Core.<*> x Core..:? "httpMethod"
                    Core.<*> x Core..:? "integrationResponses"
                    Core.<*> x Core..:? "passthroughBehavior"
                    Core.<*> x Core..:? "requestParameters"
                    Core.<*> x Core..:? "requestTemplates"
                    Core.<*> x Core..:? "timeoutInMillis"
                    Core.<*> x Core..:? "tlsConfig"
                    Core.<*> x Core..:? "type"
                    Core.<*> x Core..:? "uri"
