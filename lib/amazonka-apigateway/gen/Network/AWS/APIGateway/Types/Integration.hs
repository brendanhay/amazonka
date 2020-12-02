{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Integration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Integration where

import Network.AWS.APIGateway.Types.ConnectionType
import Network.AWS.APIGateway.Types.ContentHandlingStrategy
import Network.AWS.APIGateway.Types.IntegrationResponse
import Network.AWS.APIGateway.Types.IntegrationType
import Network.AWS.APIGateway.Types.TLSConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an HTTP, HTTP_PROXY, AWS, AWS_PROXY, or Mock integration.
--
--
-- In the API Gateway console, the built-in Lambda integration is an AWS integration.<https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Creating an API>
--
-- /See:/ 'integration' smart constructor.
data Integration = Integration'
  { _iHttpMethod :: !(Maybe Text),
    _iRequestTemplates :: !(Maybe (Map Text (Text))),
    _iCredentials :: !(Maybe Text),
    _iConnectionId :: !(Maybe Text),
    _iRequestParameters :: !(Maybe (Map Text (Text))),
    _iContentHandling :: !(Maybe ContentHandlingStrategy),
    _iPassthroughBehavior :: !(Maybe Text),
    _iUri :: !(Maybe Text),
    _iIntegrationResponses ::
      !(Maybe (Map Text (IntegrationResponse))),
    _iTlsConfig :: !(Maybe TLSConfig),
    _iCacheNamespace :: !(Maybe Text),
    _iTimeoutInMillis :: !(Maybe Int),
    _iType :: !(Maybe IntegrationType),
    _iConnectionType :: !(Maybe ConnectionType),
    _iCacheKeyParameters :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Integration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iHttpMethod' - Specifies the integration's HTTP method type.
--
-- * 'iRequestTemplates' - Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
--
-- * 'iCredentials' - Specifies the credentials required for the integration, if any. For AWS integrations, three options are available. To specify an IAM Role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To require that the caller's identity be passed through from the request, specify the string @arn:aws:iam::\*:user/\*@ . To use resource-based permissions on supported AWS services, specify null.
--
-- * 'iConnectionId' - The (<https://docs.aws.amazon.com/apigateway/api-reference/resource/vpc-link/#id @id@ > ) of the 'VpcLink' used for the integration when @connectionType=VPC_LINK@ and undefined, otherwise.
--
-- * 'iRequestParameters' - A key-value map specifying request parameters that are passed from the method request to the back end. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the back end. The method request parameter value must match the pattern of @method.request.{location}.{name}@ , where @location@ is @querystring@ , @path@ , or @header@ and @name@ must be a valid and unique method request parameter name.
--
-- * 'iContentHandling' - Specifies how to handle request payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:     * @CONVERT_TO_BINARY@ : Converts a request payload from a Base64-encoded string to the corresponding binary blob.     * @CONVERT_TO_TEXT@ : Converts a request payload from a binary blob to a Base64-encoded string. If this property is not defined, the request payload will be passed through from the method request to integration request without modification, provided that the @passthroughBehavior@ is configured to support payload pass-through.
--
-- * 'iPassthroughBehavior' - Specifies how the method request body of an unmapped content type will be passed through the integration request to the back end without transformation. A content type is unmapped if no mapping template is defined in the integration or the content type does not match any of the mapped content types, as specified in @requestTemplates@ . The valid value is one of the following:      * @WHEN_NO_MATCH@ : passes the method request body through the integration request to the back end without transformation when the method request content type does not match any content type associated with the mapping templates defined in the integration request.     * @WHEN_NO_TEMPLATES@ : passes the method request body through the integration request to the back end without transformation when no mapping template is defined in the integration request. If a template is defined when this option is selected, the method request of an unmapped content-type will be rejected with an HTTP @415 Unsupported Media Type@ response.     * @NEVER@ : rejects the method request with an HTTP @415 Unsupported Media Type@ response when either the method request content type does not match any content type associated with the mapping templates defined in the integration request or no mapping template is defined in the integration request.
--
-- * 'iUri' - Specifies Uniform Resource Identifier (URI) of the integration endpoint.     * For @HTTP@ or @HTTP_PROXY@ integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> , for either standard integration, where @connectionType@ is not @VPC_LINK@ , or private integration, where @connectionType@ is @VPC_LINK@ . For a private HTTP integration, the URI is not used for routing.      * For @AWS@ or @AWS_PROXY@ integrations, the URI is of the form @arn:aws:apigateway:{region}:{subdomain.service|service}:path|action/{service_api}@ . Here, @{Region}@ is the API Gateway region (e.g., @us-east-1@ ); @{service}@ is the name of the integrated AWS service (e.g., @s3@ ); and @{subdomain}@ is a designated subdomain supported by certain AWS service for fast host-name lookup. @action@ can be used for an AWS service action-based API, using an @Action={name}&{p1}={v1}&p2={v2}...@ query string. The ensuing @{service_api}@ refers to a supported action @{name}@ plus any required input parameters. Alternatively, @path@ can be used for an AWS service path-based API. The ensuing @service_api@ refers to the path to an AWS service resource, including the region of the integrated AWS service, if applicable. For example, for integration with the S3 API of @<https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html GetObject> @ , the @uri@ can be either @arn:aws:apigateway:us-west-2:s3:action/GetObject&Bucket={bucket}&Key={key}@ or @arn:aws:apigateway:us-west-2:s3:path/{bucket}/{key}@
--
-- * 'iIntegrationResponses' - Specifies the integration's responses. __Example: Get integration responses of a method__  __Request__  @@GET /restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200 HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160607T191449Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160607/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash} @ @ __Response__  The successful response returns @200 OK@ status and a payload as follows: @@{ "_links": { "curies": { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true }, "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.Content-Type": "'application/xml'" }, "responseTemplates": { "application/json": "$util.urlDecode(\"%3CkinesisStreams%3E#foreach($stream in $input.path('$.StreamNames'))%3Cstream%3E%3Cname%3E$stream%3C/name%3E%3C/stream%3E#end%3C/kinesisStreams%3E\")\n" }, "statusCode": "200" }@ @  <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Creating an API>
--
-- * 'iTlsConfig' - Specifies the TLS configuration for an integration.
--
-- * 'iCacheNamespace' - Specifies a group of related cached parameters. By default, API Gateway uses the resource ID as the @cacheNamespace@ . You can specify the same @cacheNamespace@ across resources to return the same cached data for requests to different resources.
--
-- * 'iTimeoutInMillis' - Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
--
-- * 'iType' - Specifies an API method integration type. The valid value is one of the following:     * @AWS@ : for integrating the API method request with an AWS service action, including the Lambda function-invoking action. With the Lambda function-invoking action, this is referred to as the Lambda custom integration. With any other AWS service action, this is known as AWS integration.    * @AWS_PROXY@ : for integrating the API method request with the Lambda function-invoking action with the client request passed through as-is. This integration is also referred to as the Lambda proxy integration.    * @HTTP@ : for integrating the API method request with an HTTP endpoint, including a private HTTP endpoint within a VPC. This integration is also referred to as the HTTP custom integration.    * @HTTP_PROXY@ : for integrating the API method request with an HTTP endpoint, including a private HTTP endpoint within a VPC, with the client request passed through as-is. This is also referred to as the HTTP proxy integration.    * @MOCK@ : for integrating the API method request with API Gateway as a "loop-back" endpoint without invoking any backend. For the HTTP and HTTP proxy integrations, each integration can specify a protocol (@http/https@ ), port and path. Standard 80 and 443 ports are supported as well as custom ports above 1024. An HTTP or HTTP proxy integration with a @connectionType@ of @VPC_LINK@ is referred to as a private integration and uses a 'VpcLink' to connect API Gateway to a network load balancer of a VPC.
--
-- * 'iConnectionType' - The type of the network connection to the integration endpoint. The valid value is @INTERNET@ for connections through the public routable internet or @VPC_LINK@ for private connections between API Gateway and a network load balancer in a VPC. The default value is @INTERNET@ .
--
-- * 'iCacheKeyParameters' - A list of request parameters whose values API Gateway caches. To be valid values for @cacheKeyParameters@ , these parameters must also be specified for 'Method' @requestParameters@ .
integration ::
  Integration
integration =
  Integration'
    { _iHttpMethod = Nothing,
      _iRequestTemplates = Nothing,
      _iCredentials = Nothing,
      _iConnectionId = Nothing,
      _iRequestParameters = Nothing,
      _iContentHandling = Nothing,
      _iPassthroughBehavior = Nothing,
      _iUri = Nothing,
      _iIntegrationResponses = Nothing,
      _iTlsConfig = Nothing,
      _iCacheNamespace = Nothing,
      _iTimeoutInMillis = Nothing,
      _iType = Nothing,
      _iConnectionType = Nothing,
      _iCacheKeyParameters = Nothing
    }

-- | Specifies the integration's HTTP method type.
iHttpMethod :: Lens' Integration (Maybe Text)
iHttpMethod = lens _iHttpMethod (\s a -> s {_iHttpMethod = a})

-- | Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
iRequestTemplates :: Lens' Integration (HashMap Text (Text))
iRequestTemplates = lens _iRequestTemplates (\s a -> s {_iRequestTemplates = a}) . _Default . _Map

-- | Specifies the credentials required for the integration, if any. For AWS integrations, three options are available. To specify an IAM Role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To require that the caller's identity be passed through from the request, specify the string @arn:aws:iam::\*:user/\*@ . To use resource-based permissions on supported AWS services, specify null.
iCredentials :: Lens' Integration (Maybe Text)
iCredentials = lens _iCredentials (\s a -> s {_iCredentials = a})

-- | The (<https://docs.aws.amazon.com/apigateway/api-reference/resource/vpc-link/#id @id@ > ) of the 'VpcLink' used for the integration when @connectionType=VPC_LINK@ and undefined, otherwise.
iConnectionId :: Lens' Integration (Maybe Text)
iConnectionId = lens _iConnectionId (\s a -> s {_iConnectionId = a})

-- | A key-value map specifying request parameters that are passed from the method request to the back end. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the back end. The method request parameter value must match the pattern of @method.request.{location}.{name}@ , where @location@ is @querystring@ , @path@ , or @header@ and @name@ must be a valid and unique method request parameter name.
iRequestParameters :: Lens' Integration (HashMap Text (Text))
iRequestParameters = lens _iRequestParameters (\s a -> s {_iRequestParameters = a}) . _Default . _Map

-- | Specifies how to handle request payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:     * @CONVERT_TO_BINARY@ : Converts a request payload from a Base64-encoded string to the corresponding binary blob.     * @CONVERT_TO_TEXT@ : Converts a request payload from a binary blob to a Base64-encoded string. If this property is not defined, the request payload will be passed through from the method request to integration request without modification, provided that the @passthroughBehavior@ is configured to support payload pass-through.
iContentHandling :: Lens' Integration (Maybe ContentHandlingStrategy)
iContentHandling = lens _iContentHandling (\s a -> s {_iContentHandling = a})

-- | Specifies how the method request body of an unmapped content type will be passed through the integration request to the back end without transformation. A content type is unmapped if no mapping template is defined in the integration or the content type does not match any of the mapped content types, as specified in @requestTemplates@ . The valid value is one of the following:      * @WHEN_NO_MATCH@ : passes the method request body through the integration request to the back end without transformation when the method request content type does not match any content type associated with the mapping templates defined in the integration request.     * @WHEN_NO_TEMPLATES@ : passes the method request body through the integration request to the back end without transformation when no mapping template is defined in the integration request. If a template is defined when this option is selected, the method request of an unmapped content-type will be rejected with an HTTP @415 Unsupported Media Type@ response.     * @NEVER@ : rejects the method request with an HTTP @415 Unsupported Media Type@ response when either the method request content type does not match any content type associated with the mapping templates defined in the integration request or no mapping template is defined in the integration request.
iPassthroughBehavior :: Lens' Integration (Maybe Text)
iPassthroughBehavior = lens _iPassthroughBehavior (\s a -> s {_iPassthroughBehavior = a})

-- | Specifies Uniform Resource Identifier (URI) of the integration endpoint.     * For @HTTP@ or @HTTP_PROXY@ integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> , for either standard integration, where @connectionType@ is not @VPC_LINK@ , or private integration, where @connectionType@ is @VPC_LINK@ . For a private HTTP integration, the URI is not used for routing.      * For @AWS@ or @AWS_PROXY@ integrations, the URI is of the form @arn:aws:apigateway:{region}:{subdomain.service|service}:path|action/{service_api}@ . Here, @{Region}@ is the API Gateway region (e.g., @us-east-1@ ); @{service}@ is the name of the integrated AWS service (e.g., @s3@ ); and @{subdomain}@ is a designated subdomain supported by certain AWS service for fast host-name lookup. @action@ can be used for an AWS service action-based API, using an @Action={name}&{p1}={v1}&p2={v2}...@ query string. The ensuing @{service_api}@ refers to a supported action @{name}@ plus any required input parameters. Alternatively, @path@ can be used for an AWS service path-based API. The ensuing @service_api@ refers to the path to an AWS service resource, including the region of the integrated AWS service, if applicable. For example, for integration with the S3 API of @<https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html GetObject> @ , the @uri@ can be either @arn:aws:apigateway:us-west-2:s3:action/GetObject&Bucket={bucket}&Key={key}@ or @arn:aws:apigateway:us-west-2:s3:path/{bucket}/{key}@
iUri :: Lens' Integration (Maybe Text)
iUri = lens _iUri (\s a -> s {_iUri = a})

-- | Specifies the integration's responses. __Example: Get integration responses of a method__  __Request__  @@GET /restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200 HTTP/1.1 Content-Type: application/json Host: apigateway.us-east-1.amazonaws.com X-Amz-Date: 20160607T191449Z Authorization: AWS4-HMAC-SHA256 Credential={access_key_ID}/20160607/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature={sig4_hash} @ @ __Response__  The successful response returns @200 OK@ status and a payload as follows: @@{ "_links": { "curies": { "href": "https://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-integration-response-{rel}.html", "name": "integrationresponse", "templated": true }, "self": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200", "title": "200" }, "integrationresponse:delete": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" }, "integrationresponse:update": { "href": "/restapis/fugvjdxtri/resources/3kzxbg5sa2/methods/GET/integration/responses/200" } }, "responseParameters": { "method.response.header.Content-Type": "'application/xml'" }, "responseTemplates": { "application/json": "$util.urlDecode(\"%3CkinesisStreams%3E#foreach($stream in $input.path('$.StreamNames'))%3Cstream%3E%3Cname%3E$stream%3C/name%3E%3C/stream%3E#end%3C/kinesisStreams%3E\")\n" }, "statusCode": "200" }@ @  <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Creating an API>
iIntegrationResponses :: Lens' Integration (HashMap Text (IntegrationResponse))
iIntegrationResponses = lens _iIntegrationResponses (\s a -> s {_iIntegrationResponses = a}) . _Default . _Map

-- | Specifies the TLS configuration for an integration.
iTlsConfig :: Lens' Integration (Maybe TLSConfig)
iTlsConfig = lens _iTlsConfig (\s a -> s {_iTlsConfig = a})

-- | Specifies a group of related cached parameters. By default, API Gateway uses the resource ID as the @cacheNamespace@ . You can specify the same @cacheNamespace@ across resources to return the same cached data for requests to different resources.
iCacheNamespace :: Lens' Integration (Maybe Text)
iCacheNamespace = lens _iCacheNamespace (\s a -> s {_iCacheNamespace = a})

-- | Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
iTimeoutInMillis :: Lens' Integration (Maybe Int)
iTimeoutInMillis = lens _iTimeoutInMillis (\s a -> s {_iTimeoutInMillis = a})

-- | Specifies an API method integration type. The valid value is one of the following:     * @AWS@ : for integrating the API method request with an AWS service action, including the Lambda function-invoking action. With the Lambda function-invoking action, this is referred to as the Lambda custom integration. With any other AWS service action, this is known as AWS integration.    * @AWS_PROXY@ : for integrating the API method request with the Lambda function-invoking action with the client request passed through as-is. This integration is also referred to as the Lambda proxy integration.    * @HTTP@ : for integrating the API method request with an HTTP endpoint, including a private HTTP endpoint within a VPC. This integration is also referred to as the HTTP custom integration.    * @HTTP_PROXY@ : for integrating the API method request with an HTTP endpoint, including a private HTTP endpoint within a VPC, with the client request passed through as-is. This is also referred to as the HTTP proxy integration.    * @MOCK@ : for integrating the API method request with API Gateway as a "loop-back" endpoint without invoking any backend. For the HTTP and HTTP proxy integrations, each integration can specify a protocol (@http/https@ ), port and path. Standard 80 and 443 ports are supported as well as custom ports above 1024. An HTTP or HTTP proxy integration with a @connectionType@ of @VPC_LINK@ is referred to as a private integration and uses a 'VpcLink' to connect API Gateway to a network load balancer of a VPC.
iType :: Lens' Integration (Maybe IntegrationType)
iType = lens _iType (\s a -> s {_iType = a})

-- | The type of the network connection to the integration endpoint. The valid value is @INTERNET@ for connections through the public routable internet or @VPC_LINK@ for private connections between API Gateway and a network load balancer in a VPC. The default value is @INTERNET@ .
iConnectionType :: Lens' Integration (Maybe ConnectionType)
iConnectionType = lens _iConnectionType (\s a -> s {_iConnectionType = a})

-- | A list of request parameters whose values API Gateway caches. To be valid values for @cacheKeyParameters@ , these parameters must also be specified for 'Method' @requestParameters@ .
iCacheKeyParameters :: Lens' Integration [Text]
iCacheKeyParameters = lens _iCacheKeyParameters (\s a -> s {_iCacheKeyParameters = a}) . _Default . _Coerce

instance FromJSON Integration where
  parseJSON =
    withObject
      "Integration"
      ( \x ->
          Integration'
            <$> (x .:? "httpMethod")
            <*> (x .:? "requestTemplates" .!= mempty)
            <*> (x .:? "credentials")
            <*> (x .:? "connectionId")
            <*> (x .:? "requestParameters" .!= mempty)
            <*> (x .:? "contentHandling")
            <*> (x .:? "passthroughBehavior")
            <*> (x .:? "uri")
            <*> (x .:? "integrationResponses" .!= mempty)
            <*> (x .:? "tlsConfig")
            <*> (x .:? "cacheNamespace")
            <*> (x .:? "timeoutInMillis")
            <*> (x .:? "type")
            <*> (x .:? "connectionType")
            <*> (x .:? "cacheKeyParameters" .!= mempty)
      )

instance Hashable Integration

instance NFData Integration
