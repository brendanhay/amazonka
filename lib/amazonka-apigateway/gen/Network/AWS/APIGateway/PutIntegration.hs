{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.PutIntegration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets up a method's integration.
module Network.AWS.APIGateway.PutIntegration
  ( -- * Creating a request
    PutIntegration (..),
    mkPutIntegration,

    -- ** Request lenses
    pRequestTemplates,
    pCredentials,
    pConnectionId,
    pRequestParameters,
    pContentHandling,
    pPassthroughBehavior,
    pUri,
    pTlsConfig,
    pCacheNamespace,
    pTimeoutInMillis,
    pConnectionType,
    pIntegrationHTTPMethod,
    pCacheKeyParameters,
    pRestAPIId,
    pResourceId,
    pHttpMethod,
    pType,

    -- * Destructuring the response
    Integration (..),
    mkIntegration,

    -- ** Response lenses
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

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Sets up a method's integration.
--
-- /See:/ 'mkPutIntegration' smart constructor.
data PutIntegration = PutIntegration'
  { requestTemplates ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    credentials :: Lude.Maybe Lude.Text,
    connectionId :: Lude.Maybe Lude.Text,
    requestParameters ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    contentHandling :: Lude.Maybe ContentHandlingStrategy,
    passthroughBehavior :: Lude.Maybe Lude.Text,
    uri :: Lude.Maybe Lude.Text,
    tlsConfig :: Lude.Maybe TLSConfig,
    cacheNamespace :: Lude.Maybe Lude.Text,
    timeoutInMillis :: Lude.Maybe Lude.Int,
    connectionType :: Lude.Maybe ConnectionType,
    integrationHTTPMethod :: Lude.Maybe Lude.Text,
    cacheKeyParameters :: Lude.Maybe [Lude.Text],
    restAPIId :: Lude.Text,
    resourceId :: Lude.Text,
    httpMethod :: Lude.Text,
    type' :: IntegrationType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutIntegration' with the minimum fields required to make a request.
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
-- * 'credentials' - Specifies whether credentials are required for a put integration.
-- * 'httpMethod' - [Required] Specifies a put integration request's HTTP method.
-- * 'integrationHTTPMethod' - Specifies a put integration HTTP method. When the integration type is HTTP or AWS, this field is required.
-- * 'passthroughBehavior' - Specifies the pass-through behavior for incoming requests based on the Content-Type header in the request, and the available mapping templates specified as the @requestTemplates@ property on the Integration resource. There are three valid values: @WHEN_NO_MATCH@ , @WHEN_NO_TEMPLATES@ , and @NEVER@ .
--
--
--     * @WHEN_NO_MATCH@ passes the request body for unmapped content types through to the integration back end without transformation.
--
--
--     * @NEVER@ rejects unmapped content types with an HTTP 415 'Unsupported Media Type' response.
--
--
--     * @WHEN_NO_TEMPLATES@ allows pass-through when the integration has NO content types mapped to templates. However if there is at least one content type defined, unmapped content types will be rejected with the same 415 response.
--
--
-- * 'requestParameters' - A key-value map specifying request parameters that are passed from the method request to the back end. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the back end. The method request parameter value must match the pattern of @method.request.{location}.{name}@ , where @location@ is @querystring@ , @path@ , or @header@ and @name@ must be a valid and unique method request parameter name.
-- * 'requestTemplates' - Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
-- * 'resourceId' - [Required] Specifies a put integration request's resource ID.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'timeoutInMillis' - Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
-- * 'tlsConfig' - Undocumented field.
-- * 'type'' - [Required] Specifies a put integration input's type.
-- * 'uri' - Specifies Uniform Resource Identifier (URI) of the integration endpoint.
--
--
--     * For @HTTP@ or @HTTP_PROXY@ integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> , for either standard integration, where @connectionType@ is not @VPC_LINK@ , or private integration, where @connectionType@ is @VPC_LINK@ . For a private HTTP integration, the URI is not used for routing.
--
--
--     * For @AWS@ or @AWS_PROXY@ integrations, the URI is of the form @arn:aws:apigateway:{region}:{subdomain.service|service}:path|action/{service_api}@ . Here, @{Region}@ is the API Gateway region (e.g., @us-east-1@ ); @{service}@ is the name of the integrated AWS service (e.g., @s3@ ); and @{subdomain}@ is a designated subdomain supported by certain AWS service for fast host-name lookup. @action@ can be used for an AWS service action-based API, using an @Action={name}&{p1}={v1}&p2={v2}...@ query string. The ensuing @{service_api}@ refers to a supported action @{name}@ plus any required input parameters. Alternatively, @path@ can be used for an AWS service path-based API. The ensuing @service_api@ refers to the path to an AWS service resource, including the region of the integrated AWS service, if applicable. For example, for integration with the S3 API of @<https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html GetObject> @ , the @uri@ can be either @arn:aws:apigateway:us-west-2:s3:action/GetObject&Bucket={bucket}&Key={key}@ or @arn:aws:apigateway:us-west-2:s3:path/{bucket}/{key}@
mkPutIntegration ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  -- | 'httpMethod'
  Lude.Text ->
  -- | 'type''
  IntegrationType ->
  PutIntegration
mkPutIntegration pRestAPIId_ pResourceId_ pHttpMethod_ pType_ =
  PutIntegration'
    { requestTemplates = Lude.Nothing,
      credentials = Lude.Nothing,
      connectionId = Lude.Nothing,
      requestParameters = Lude.Nothing,
      contentHandling = Lude.Nothing,
      passthroughBehavior = Lude.Nothing,
      uri = Lude.Nothing,
      tlsConfig = Lude.Nothing,
      cacheNamespace = Lude.Nothing,
      timeoutInMillis = Lude.Nothing,
      connectionType = Lude.Nothing,
      integrationHTTPMethod = Lude.Nothing,
      cacheKeyParameters = Lude.Nothing,
      restAPIId = pRestAPIId_,
      resourceId = pResourceId_,
      httpMethod = pHttpMethod_,
      type' = pType_
    }

-- | Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
--
-- /Note:/ Consider using 'requestTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pRequestTemplates :: Lens.Lens' PutIntegration (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
pRequestTemplates = Lens.lens (requestTemplates :: PutIntegration -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {requestTemplates = a} :: PutIntegration)
{-# DEPRECATED pRequestTemplates "Use generic-lens or generic-optics with 'requestTemplates' instead." #-}

-- | Specifies whether credentials are required for a put integration.
--
-- /Note:/ Consider using 'credentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCredentials :: Lens.Lens' PutIntegration (Lude.Maybe Lude.Text)
pCredentials = Lens.lens (credentials :: PutIntegration -> Lude.Maybe Lude.Text) (\s a -> s {credentials = a} :: PutIntegration)
{-# DEPRECATED pCredentials "Use generic-lens or generic-optics with 'credentials' instead." #-}

-- | The (<https://docs.aws.amazon.com/apigateway/api-reference/resource/vpc-link/#id @id@ > ) of the 'VpcLink' used for the integration when @connectionType=VPC_LINK@ and undefined, otherwise.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pConnectionId :: Lens.Lens' PutIntegration (Lude.Maybe Lude.Text)
pConnectionId = Lens.lens (connectionId :: PutIntegration -> Lude.Maybe Lude.Text) (\s a -> s {connectionId = a} :: PutIntegration)
{-# DEPRECATED pConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | A key-value map specifying request parameters that are passed from the method request to the back end. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the back end. The method request parameter value must match the pattern of @method.request.{location}.{name}@ , where @location@ is @querystring@ , @path@ , or @header@ and @name@ must be a valid and unique method request parameter name.
--
-- /Note:/ Consider using 'requestParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pRequestParameters :: Lens.Lens' PutIntegration (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
pRequestParameters = Lens.lens (requestParameters :: PutIntegration -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {requestParameters = a} :: PutIntegration)
{-# DEPRECATED pRequestParameters "Use generic-lens or generic-optics with 'requestParameters' instead." #-}

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
pContentHandling :: Lens.Lens' PutIntegration (Lude.Maybe ContentHandlingStrategy)
pContentHandling = Lens.lens (contentHandling :: PutIntegration -> Lude.Maybe ContentHandlingStrategy) (\s a -> s {contentHandling = a} :: PutIntegration)
{-# DEPRECATED pContentHandling "Use generic-lens or generic-optics with 'contentHandling' instead." #-}

-- | Specifies the pass-through behavior for incoming requests based on the Content-Type header in the request, and the available mapping templates specified as the @requestTemplates@ property on the Integration resource. There are three valid values: @WHEN_NO_MATCH@ , @WHEN_NO_TEMPLATES@ , and @NEVER@ .
--
--
--     * @WHEN_NO_MATCH@ passes the request body for unmapped content types through to the integration back end without transformation.
--
--
--     * @NEVER@ rejects unmapped content types with an HTTP 415 'Unsupported Media Type' response.
--
--
--     * @WHEN_NO_TEMPLATES@ allows pass-through when the integration has NO content types mapped to templates. However if there is at least one content type defined, unmapped content types will be rejected with the same 415 response.
--
--
--
-- /Note:/ Consider using 'passthroughBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPassthroughBehavior :: Lens.Lens' PutIntegration (Lude.Maybe Lude.Text)
pPassthroughBehavior = Lens.lens (passthroughBehavior :: PutIntegration -> Lude.Maybe Lude.Text) (\s a -> s {passthroughBehavior = a} :: PutIntegration)
{-# DEPRECATED pPassthroughBehavior "Use generic-lens or generic-optics with 'passthroughBehavior' instead." #-}

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
pUri :: Lens.Lens' PutIntegration (Lude.Maybe Lude.Text)
pUri = Lens.lens (uri :: PutIntegration -> Lude.Maybe Lude.Text) (\s a -> s {uri = a} :: PutIntegration)
{-# DEPRECATED pUri "Use generic-lens or generic-optics with 'uri' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tlsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTlsConfig :: Lens.Lens' PutIntegration (Lude.Maybe TLSConfig)
pTlsConfig = Lens.lens (tlsConfig :: PutIntegration -> Lude.Maybe TLSConfig) (\s a -> s {tlsConfig = a} :: PutIntegration)
{-# DEPRECATED pTlsConfig "Use generic-lens or generic-optics with 'tlsConfig' instead." #-}

-- | Specifies a group of related cached parameters. By default, API Gateway uses the resource ID as the @cacheNamespace@ . You can specify the same @cacheNamespace@ across resources to return the same cached data for requests to different resources.
--
-- /Note:/ Consider using 'cacheNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCacheNamespace :: Lens.Lens' PutIntegration (Lude.Maybe Lude.Text)
pCacheNamespace = Lens.lens (cacheNamespace :: PutIntegration -> Lude.Maybe Lude.Text) (\s a -> s {cacheNamespace = a} :: PutIntegration)
{-# DEPRECATED pCacheNamespace "Use generic-lens or generic-optics with 'cacheNamespace' instead." #-}

-- | Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
--
-- /Note:/ Consider using 'timeoutInMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTimeoutInMillis :: Lens.Lens' PutIntegration (Lude.Maybe Lude.Int)
pTimeoutInMillis = Lens.lens (timeoutInMillis :: PutIntegration -> Lude.Maybe Lude.Int) (\s a -> s {timeoutInMillis = a} :: PutIntegration)
{-# DEPRECATED pTimeoutInMillis "Use generic-lens or generic-optics with 'timeoutInMillis' instead." #-}

-- | The type of the network connection to the integration endpoint. The valid value is @INTERNET@ for connections through the public routable internet or @VPC_LINK@ for private connections between API Gateway and a network load balancer in a VPC. The default value is @INTERNET@ .
--
-- /Note:/ Consider using 'connectionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pConnectionType :: Lens.Lens' PutIntegration (Lude.Maybe ConnectionType)
pConnectionType = Lens.lens (connectionType :: PutIntegration -> Lude.Maybe ConnectionType) (\s a -> s {connectionType = a} :: PutIntegration)
{-# DEPRECATED pConnectionType "Use generic-lens or generic-optics with 'connectionType' instead." #-}

-- | Specifies a put integration HTTP method. When the integration type is HTTP or AWS, this field is required.
--
-- /Note:/ Consider using 'integrationHTTPMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pIntegrationHTTPMethod :: Lens.Lens' PutIntegration (Lude.Maybe Lude.Text)
pIntegrationHTTPMethod = Lens.lens (integrationHTTPMethod :: PutIntegration -> Lude.Maybe Lude.Text) (\s a -> s {integrationHTTPMethod = a} :: PutIntegration)
{-# DEPRECATED pIntegrationHTTPMethod "Use generic-lens or generic-optics with 'integrationHTTPMethod' instead." #-}

-- | A list of request parameters whose values API Gateway caches. To be valid values for @cacheKeyParameters@ , these parameters must also be specified for 'Method' @requestParameters@ .
--
-- /Note:/ Consider using 'cacheKeyParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCacheKeyParameters :: Lens.Lens' PutIntegration (Lude.Maybe [Lude.Text])
pCacheKeyParameters = Lens.lens (cacheKeyParameters :: PutIntegration -> Lude.Maybe [Lude.Text]) (\s a -> s {cacheKeyParameters = a} :: PutIntegration)
{-# DEPRECATED pCacheKeyParameters "Use generic-lens or generic-optics with 'cacheKeyParameters' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pRestAPIId :: Lens.Lens' PutIntegration Lude.Text
pRestAPIId = Lens.lens (restAPIId :: PutIntegration -> Lude.Text) (\s a -> s {restAPIId = a} :: PutIntegration)
{-# DEPRECATED pRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] Specifies a put integration request's resource ID.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pResourceId :: Lens.Lens' PutIntegration Lude.Text
pResourceId = Lens.lens (resourceId :: PutIntegration -> Lude.Text) (\s a -> s {resourceId = a} :: PutIntegration)
{-# DEPRECATED pResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | [Required] Specifies a put integration request's HTTP method.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pHttpMethod :: Lens.Lens' PutIntegration Lude.Text
pHttpMethod = Lens.lens (httpMethod :: PutIntegration -> Lude.Text) (\s a -> s {httpMethod = a} :: PutIntegration)
{-# DEPRECATED pHttpMethod "Use generic-lens or generic-optics with 'httpMethod' instead." #-}

-- | [Required] Specifies a put integration input's type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pType :: Lens.Lens' PutIntegration IntegrationType
pType = Lens.lens (type' :: PutIntegration -> IntegrationType) (\s a -> s {type' = a} :: PutIntegration)
{-# DEPRECATED pType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.AWSRequest PutIntegration where
  type Rs PutIntegration = Integration
  request = Req.putJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders PutIntegration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON PutIntegration where
  toJSON PutIntegration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("requestTemplates" Lude..=) Lude.<$> requestTemplates,
            ("credentials" Lude..=) Lude.<$> credentials,
            ("connectionId" Lude..=) Lude.<$> connectionId,
            ("requestParameters" Lude..=) Lude.<$> requestParameters,
            ("contentHandling" Lude..=) Lude.<$> contentHandling,
            ("passthroughBehavior" Lude..=) Lude.<$> passthroughBehavior,
            ("uri" Lude..=) Lude.<$> uri,
            ("tlsConfig" Lude..=) Lude.<$> tlsConfig,
            ("cacheNamespace" Lude..=) Lude.<$> cacheNamespace,
            ("timeoutInMillis" Lude..=) Lude.<$> timeoutInMillis,
            ("connectionType" Lude..=) Lude.<$> connectionType,
            ("httpMethod" Lude..=) Lude.<$> integrationHTTPMethod,
            ("cacheKeyParameters" Lude..=) Lude.<$> cacheKeyParameters,
            Lude.Just ("type" Lude..= type')
          ]
      )

instance Lude.ToPath PutIntegration where
  toPath PutIntegration' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/resources/",
        Lude.toBS resourceId,
        "/methods/",
        Lude.toBS httpMethod,
        "/integration"
      ]

instance Lude.ToQuery PutIntegration where
  toQuery = Lude.const Lude.mempty
