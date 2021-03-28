{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.PutIntegration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets up a method's integration.
module Network.AWS.ApiGateway.PutIntegration
    (
    -- * Creating a request
      PutIntegration (..)
    , mkPutIntegration
    -- ** Request lenses
    , piRestApiId
    , piResourceId
    , piHttpMethod
    , piType
    , piCacheKeyParameters
    , piCacheNamespace
    , piConnectionId
    , piConnectionType
    , piContentHandling
    , piCredentials
    , piIntegrationHttpMethod
    , piPassthroughBehavior
    , piRequestParameters
    , piRequestTemplates
    , piTimeoutInMillis
    , piTlsConfig
    , piUri

     -- * Destructuring the response
    , Types.Integration (..)
    , Types.mkIntegration
    -- ** Response lenses
    , Types.iCacheKeyParameters
    , Types.iCacheNamespace
    , Types.iConnectionId
    , Types.iConnectionType
    , Types.iContentHandling
    , Types.iCredentials
    , Types.iHttpMethod
    , Types.iIntegrationResponses
    , Types.iPassthroughBehavior
    , Types.iRequestParameters
    , Types.iRequestTemplates
    , Types.iTimeoutInMillis
    , Types.iTlsConfig
    , Types.iType
    , Types.iUri
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Sets up a method's integration.
--
-- /See:/ 'mkPutIntegration' smart constructor.
data PutIntegration = PutIntegration'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , resourceId :: Core.Text
    -- ^ [Required] Specifies a put integration request's resource ID.
  , httpMethod :: Core.Text
    -- ^ [Required] Specifies a put integration request's HTTP method.
  , type' :: Types.IntegrationType
    -- ^ [Required] Specifies a put integration input's type.
  , cacheKeyParameters :: Core.Maybe [Core.Text]
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
    -- ^ Specifies whether credentials are required for a put integration.
  , integrationHttpMethod :: Core.Maybe Core.Text
    -- ^ Specifies a put integration HTTP method. When the integration type is HTTP or AWS, this field is required.
  , passthroughBehavior :: Core.Maybe Core.Text
    -- ^ Specifies the pass-through behavior for incoming requests based on the Content-Type header in the request, and the available mapping templates specified as the @requestTemplates@ property on the Integration resource. There are three valid values: @WHEN_NO_MATCH@ , @WHEN_NO_TEMPLATES@ , and @NEVER@ . 
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
  , requestParameters :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A key-value map specifying request parameters that are passed from the method request to the back end. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the back end. The method request parameter value must match the pattern of @method.request.{location}.{name}@ , where @location@ is @querystring@ , @path@ , or @header@ and @name@ must be a valid and unique method request parameter name.
  , requestTemplates :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
  , timeoutInMillis :: Core.Maybe Core.Int
    -- ^ Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
  , tlsConfig :: Core.Maybe Types.TlsConfig
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

-- | Creates a 'PutIntegration' value with any optional fields omitted.
mkPutIntegration
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'resourceId'
    -> Core.Text -- ^ 'httpMethod'
    -> Types.IntegrationType -- ^ 'type\''
    -> PutIntegration
mkPutIntegration restApiId resourceId httpMethod type'
  = PutIntegration'{restApiId, resourceId, httpMethod, type',
                    cacheKeyParameters = Core.Nothing, cacheNamespace = Core.Nothing,
                    connectionId = Core.Nothing, connectionType = Core.Nothing,
                    contentHandling = Core.Nothing, credentials = Core.Nothing,
                    integrationHttpMethod = Core.Nothing,
                    passthroughBehavior = Core.Nothing,
                    requestParameters = Core.Nothing, requestTemplates = Core.Nothing,
                    timeoutInMillis = Core.Nothing, tlsConfig = Core.Nothing,
                    uri = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piRestApiId :: Lens.Lens' PutIntegration Core.Text
piRestApiId = Lens.field @"restApiId"
{-# INLINEABLE piRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] Specifies a put integration request's resource ID.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piResourceId :: Lens.Lens' PutIntegration Core.Text
piResourceId = Lens.field @"resourceId"
{-# INLINEABLE piResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | [Required] Specifies a put integration request's HTTP method.
--
-- /Note:/ Consider using 'httpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piHttpMethod :: Lens.Lens' PutIntegration Core.Text
piHttpMethod = Lens.field @"httpMethod"
{-# INLINEABLE piHttpMethod #-}
{-# DEPRECATED httpMethod "Use generic-lens or generic-optics with 'httpMethod' instead"  #-}

-- | [Required] Specifies a put integration input's type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piType :: Lens.Lens' PutIntegration Types.IntegrationType
piType = Lens.field @"type'"
{-# INLINEABLE piType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | A list of request parameters whose values API Gateway caches. To be valid values for @cacheKeyParameters@ , these parameters must also be specified for 'Method' @requestParameters@ .
--
-- /Note:/ Consider using 'cacheKeyParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piCacheKeyParameters :: Lens.Lens' PutIntegration (Core.Maybe [Core.Text])
piCacheKeyParameters = Lens.field @"cacheKeyParameters"
{-# INLINEABLE piCacheKeyParameters #-}
{-# DEPRECATED cacheKeyParameters "Use generic-lens or generic-optics with 'cacheKeyParameters' instead"  #-}

-- | Specifies a group of related cached parameters. By default, API Gateway uses the resource ID as the @cacheNamespace@ . You can specify the same @cacheNamespace@ across resources to return the same cached data for requests to different resources.
--
-- /Note:/ Consider using 'cacheNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piCacheNamespace :: Lens.Lens' PutIntegration (Core.Maybe Core.Text)
piCacheNamespace = Lens.field @"cacheNamespace"
{-# INLINEABLE piCacheNamespace #-}
{-# DEPRECATED cacheNamespace "Use generic-lens or generic-optics with 'cacheNamespace' instead"  #-}

-- | The (<https://docs.aws.amazon.com/apigateway/api-reference/resource/vpc-link/#id @id@ > ) of the 'VpcLink' used for the integration when @connectionType=VPC_LINK@ and undefined, otherwise.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piConnectionId :: Lens.Lens' PutIntegration (Core.Maybe Core.Text)
piConnectionId = Lens.field @"connectionId"
{-# INLINEABLE piConnectionId #-}
{-# DEPRECATED connectionId "Use generic-lens or generic-optics with 'connectionId' instead"  #-}

-- | The type of the network connection to the integration endpoint. The valid value is @INTERNET@ for connections through the public routable internet or @VPC_LINK@ for private connections between API Gateway and a network load balancer in a VPC. The default value is @INTERNET@ .
--
-- /Note:/ Consider using 'connectionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piConnectionType :: Lens.Lens' PutIntegration (Core.Maybe Types.ConnectionType)
piConnectionType = Lens.field @"connectionType"
{-# INLINEABLE piConnectionType #-}
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
piContentHandling :: Lens.Lens' PutIntegration (Core.Maybe Types.ContentHandlingStrategy)
piContentHandling = Lens.field @"contentHandling"
{-# INLINEABLE piContentHandling #-}
{-# DEPRECATED contentHandling "Use generic-lens or generic-optics with 'contentHandling' instead"  #-}

-- | Specifies whether credentials are required for a put integration.
--
-- /Note:/ Consider using 'credentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piCredentials :: Lens.Lens' PutIntegration (Core.Maybe Core.Text)
piCredentials = Lens.field @"credentials"
{-# INLINEABLE piCredentials #-}
{-# DEPRECATED credentials "Use generic-lens or generic-optics with 'credentials' instead"  #-}

-- | Specifies a put integration HTTP method. When the integration type is HTTP or AWS, this field is required.
--
-- /Note:/ Consider using 'integrationHttpMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piIntegrationHttpMethod :: Lens.Lens' PutIntegration (Core.Maybe Core.Text)
piIntegrationHttpMethod = Lens.field @"integrationHttpMethod"
{-# INLINEABLE piIntegrationHttpMethod #-}
{-# DEPRECATED integrationHttpMethod "Use generic-lens or generic-optics with 'integrationHttpMethod' instead"  #-}

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
piPassthroughBehavior :: Lens.Lens' PutIntegration (Core.Maybe Core.Text)
piPassthroughBehavior = Lens.field @"passthroughBehavior"
{-# INLINEABLE piPassthroughBehavior #-}
{-# DEPRECATED passthroughBehavior "Use generic-lens or generic-optics with 'passthroughBehavior' instead"  #-}

-- | A key-value map specifying request parameters that are passed from the method request to the back end. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the back end. The method request parameter value must match the pattern of @method.request.{location}.{name}@ , where @location@ is @querystring@ , @path@ , or @header@ and @name@ must be a valid and unique method request parameter name.
--
-- /Note:/ Consider using 'requestParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piRequestParameters :: Lens.Lens' PutIntegration (Core.Maybe (Core.HashMap Core.Text Core.Text))
piRequestParameters = Lens.field @"requestParameters"
{-# INLINEABLE piRequestParameters #-}
{-# DEPRECATED requestParameters "Use generic-lens or generic-optics with 'requestParameters' instead"  #-}

-- | Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
--
-- /Note:/ Consider using 'requestTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piRequestTemplates :: Lens.Lens' PutIntegration (Core.Maybe (Core.HashMap Core.Text Core.Text))
piRequestTemplates = Lens.field @"requestTemplates"
{-# INLINEABLE piRequestTemplates #-}
{-# DEPRECATED requestTemplates "Use generic-lens or generic-optics with 'requestTemplates' instead"  #-}

-- | Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
--
-- /Note:/ Consider using 'timeoutInMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piTimeoutInMillis :: Lens.Lens' PutIntegration (Core.Maybe Core.Int)
piTimeoutInMillis = Lens.field @"timeoutInMillis"
{-# INLINEABLE piTimeoutInMillis #-}
{-# DEPRECATED timeoutInMillis "Use generic-lens or generic-optics with 'timeoutInMillis' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tlsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piTlsConfig :: Lens.Lens' PutIntegration (Core.Maybe Types.TlsConfig)
piTlsConfig = Lens.field @"tlsConfig"
{-# INLINEABLE piTlsConfig #-}
{-# DEPRECATED tlsConfig "Use generic-lens or generic-optics with 'tlsConfig' instead"  #-}

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
piUri :: Lens.Lens' PutIntegration (Core.Maybe Core.Text)
piUri = Lens.field @"uri"
{-# INLINEABLE piUri #-}
{-# DEPRECATED uri "Use generic-lens or generic-optics with 'uri' instead"  #-}

instance Core.ToQuery PutIntegration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutIntegration where
        toHeaders PutIntegration{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON PutIntegration where
        toJSON PutIntegration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("type" Core..= type'),
                  ("cacheKeyParameters" Core..=) Core.<$> cacheKeyParameters,
                  ("cacheNamespace" Core..=) Core.<$> cacheNamespace,
                  ("connectionId" Core..=) Core.<$> connectionId,
                  ("connectionType" Core..=) Core.<$> connectionType,
                  ("contentHandling" Core..=) Core.<$> contentHandling,
                  ("credentials" Core..=) Core.<$> credentials,
                  ("httpMethod" Core..=) Core.<$> integrationHttpMethod,
                  ("passthroughBehavior" Core..=) Core.<$> passthroughBehavior,
                  ("requestParameters" Core..=) Core.<$> requestParameters,
                  ("requestTemplates" Core..=) Core.<$> requestTemplates,
                  ("timeoutInMillis" Core..=) Core.<$> timeoutInMillis,
                  ("tlsConfig" Core..=) Core.<$> tlsConfig,
                  ("uri" Core..=) Core.<$> uri])

instance Core.AWSRequest PutIntegration where
        type Rs PutIntegration = Types.Integration
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/resources/"
                             Core.<> Core.toText resourceId
                             Core.<> "/methods/"
                             Core.<> Core.toText httpMethod
                             Core.<> "/integration",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
