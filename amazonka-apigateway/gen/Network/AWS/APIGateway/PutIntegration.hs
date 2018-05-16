{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.PutIntegration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets up a method's integration.
--
--
module Network.AWS.APIGateway.PutIntegration
    (
    -- * Creating a Request
      putIntegration
    , PutIntegration
    -- * Request Lenses
    , pRequestTemplates
    , pCredentials
    , pConnectionId
    , pRequestParameters
    , pContentHandling
    , pPassthroughBehavior
    , pUri
    , pCacheNamespace
    , pTimeoutInMillis
    , pConnectionType
    , pIntegrationHTTPMethod
    , pCacheKeyParameters
    , pRestAPIId
    , pResourceId
    , pHttpMethod
    , pType

    -- * Destructuring the Response
    , integration
    , Integration
    -- * Response Lenses
    , iHttpMethod
    , iRequestTemplates
    , iCredentials
    , iConnectionId
    , iRequestParameters
    , iContentHandling
    , iPassthroughBehavior
    , iUri
    , iIntegrationResponses
    , iCacheNamespace
    , iTimeoutInMillis
    , iType
    , iConnectionType
    , iCacheKeyParameters
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Sets up a method's integration.
--
--
--
-- /See:/ 'putIntegration' smart constructor.
data PutIntegration = PutIntegration'
  { _pRequestTemplates      :: !(Maybe (Map Text Text))
  , _pCredentials           :: !(Maybe Text)
  , _pConnectionId          :: !(Maybe Text)
  , _pRequestParameters     :: !(Maybe (Map Text Text))
  , _pContentHandling       :: !(Maybe ContentHandlingStrategy)
  , _pPassthroughBehavior   :: !(Maybe Text)
  , _pUri                   :: !(Maybe Text)
  , _pCacheNamespace        :: !(Maybe Text)
  , _pTimeoutInMillis       :: !(Maybe Int)
  , _pConnectionType        :: !(Maybe ConnectionType)
  , _pIntegrationHTTPMethod :: !(Maybe Text)
  , _pCacheKeyParameters    :: !(Maybe [Text])
  , _pRestAPIId             :: !Text
  , _pResourceId            :: !Text
  , _pHttpMethod            :: !Text
  , _pType                  :: !IntegrationType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutIntegration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pRequestTemplates' - Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
--
-- * 'pCredentials' - Specifies whether credentials are required for a put integration.
--
-- * 'pConnectionId' - The (<http://docs.aws.amazon.com/apigateway/api-reference/resource/vpc-link/#id @id@ > ) of the 'VpcLink' used for the integration when @connectionType=VPC_LINK@ and undefined, otherwise.
--
-- * 'pRequestParameters' - A key-value map specifying request parameters that are passed from the method request to the back end. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the back end. The method request parameter value must match the pattern of @method.request.{location}.{name}@ , where @location@ is @querystring@ , @path@ , or @header@ and @name@ must be a valid and unique method request parameter name.
--
-- * 'pContentHandling' - Specifies how to handle request payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:     * @CONVERT_TO_BINARY@ : Converts a request payload from a Base64-encoded string to the corresponding binary blob.     * @CONVERT_TO_TEXT@ : Converts a request payload from a binary blob to a Base64-encoded string. If this property is not defined, the request payload will be passed through from the method request to integration request without modification, provided that the @passthroughBehaviors@ is configured to support payload pass-through.
--
-- * 'pPassthroughBehavior' - Specifies the pass-through behavior for incoming requests based on the Content-Type header in the request, and the available mapping templates specified as the @requestTemplates@ property on the Integration resource. There are three valid values: @WHEN_NO_MATCH@ , @WHEN_NO_TEMPLATES@ , and @NEVER@ .      * @WHEN_NO_MATCH@ passes the request body for unmapped content types through to the integration back end without transformation.     * @NEVER@ rejects unmapped content types with an HTTP 415 'Unsupported Media Type' response.     * @WHEN_NO_TEMPLATES@ allows pass-through when the integration has NO content types mapped to templates. However if there is at least one content type defined, unmapped content types will be rejected with the same 415 response.
--
-- * 'pUri' - Specifies Uniform Resource Identifier (URI) of the integration endpoint.     * For @HTTP@ or @HTTP_PROXY@ integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> , for either standard integration, where @connectionType@ is not @VPC_LINK@ , or private integration, where @connectionType@ is @VPC_LINK@ . For a private HTTP integration, the URI is not used for routing.      * For @AWS@ or @AWS_PROXY@ integrations, the URI is of the form @arn:aws:apigateway:{region}:{subdomain.service|service}:path|action/{service_api}@ . Here, @{Region}@ is the API Gateway region (e.g., @us-east-1@ ); @{service}@ is the name of the integrated AWS service (e.g., @s3@ ); and @{subdomain}@ is a designated subdomain supported by certain AWS service for fast host-name lookup. @action@ can be used for an AWS service action-based API, using an @Action={name}&{p1}={v1}&p2={v2}...@ query string. The ensuing @{service_api}@ refers to a supported action @{name}@ plus any required input parameters. Alternatively, @path@ can be used for an AWS service path-based API. The ensuing @service_api@ refers to the path to an AWS service resource, including the region of the integrated AWS service, if applicable. For example, for integration with the S3 API of @<http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html GetObject> @ , the @uri@ can be either @arn:aws:apigateway:us-west-2:s3:action/GetObject&Bucket={bucket}&Key={key}@ or @arn:aws:apigateway:us-west-2:s3:path/{bucket}/{key}@
--
-- * 'pCacheNamespace' - Specifies a put integration input's cache namespace.
--
-- * 'pTimeoutInMillis' - Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
--
-- * 'pConnectionType' - The type of the network connection to the integration endpoint. The valid value is @INTERNET@ for connections through the public routable internet or @VPC_LINK@ for private connections between API Gateway and a network load balancer in a VPC. The default value is @INTERNET@ .
--
-- * 'pIntegrationHTTPMethod' - Specifies a put integration HTTP method. When the integration type is HTTP or AWS, this field is required.
--
-- * 'pCacheKeyParameters' - Specifies a put integration input's cache key parameters.
--
-- * 'pRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'pResourceId' - [Required] Specifies a put integration request's resource ID.
--
-- * 'pHttpMethod' - [Required] Specifies a put integration request's HTTP method.
--
-- * 'pType' - [Required] Specifies a put integration input's type.
putIntegration
    :: Text -- ^ 'pRestAPIId'
    -> Text -- ^ 'pResourceId'
    -> Text -- ^ 'pHttpMethod'
    -> IntegrationType -- ^ 'pType'
    -> PutIntegration
putIntegration pRestAPIId_ pResourceId_ pHttpMethod_ pType_ =
  PutIntegration'
    { _pRequestTemplates = Nothing
    , _pCredentials = Nothing
    , _pConnectionId = Nothing
    , _pRequestParameters = Nothing
    , _pContentHandling = Nothing
    , _pPassthroughBehavior = Nothing
    , _pUri = Nothing
    , _pCacheNamespace = Nothing
    , _pTimeoutInMillis = Nothing
    , _pConnectionType = Nothing
    , _pIntegrationHTTPMethod = Nothing
    , _pCacheKeyParameters = Nothing
    , _pRestAPIId = pRestAPIId_
    , _pResourceId = pResourceId_
    , _pHttpMethod = pHttpMethod_
    , _pType = pType_
    }


-- | Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
pRequestTemplates :: Lens' PutIntegration (HashMap Text Text)
pRequestTemplates = lens _pRequestTemplates (\ s a -> s{_pRequestTemplates = a}) . _Default . _Map

-- | Specifies whether credentials are required for a put integration.
pCredentials :: Lens' PutIntegration (Maybe Text)
pCredentials = lens _pCredentials (\ s a -> s{_pCredentials = a})

-- | The (<http://docs.aws.amazon.com/apigateway/api-reference/resource/vpc-link/#id @id@ > ) of the 'VpcLink' used for the integration when @connectionType=VPC_LINK@ and undefined, otherwise.
pConnectionId :: Lens' PutIntegration (Maybe Text)
pConnectionId = lens _pConnectionId (\ s a -> s{_pConnectionId = a})

-- | A key-value map specifying request parameters that are passed from the method request to the back end. The key is an integration request parameter name and the associated value is a method request parameter value or static value that must be enclosed within single quotes and pre-encoded as required by the back end. The method request parameter value must match the pattern of @method.request.{location}.{name}@ , where @location@ is @querystring@ , @path@ , or @header@ and @name@ must be a valid and unique method request parameter name.
pRequestParameters :: Lens' PutIntegration (HashMap Text Text)
pRequestParameters = lens _pRequestParameters (\ s a -> s{_pRequestParameters = a}) . _Default . _Map

-- | Specifies how to handle request payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:     * @CONVERT_TO_BINARY@ : Converts a request payload from a Base64-encoded string to the corresponding binary blob.     * @CONVERT_TO_TEXT@ : Converts a request payload from a binary blob to a Base64-encoded string. If this property is not defined, the request payload will be passed through from the method request to integration request without modification, provided that the @passthroughBehaviors@ is configured to support payload pass-through.
pContentHandling :: Lens' PutIntegration (Maybe ContentHandlingStrategy)
pContentHandling = lens _pContentHandling (\ s a -> s{_pContentHandling = a})

-- | Specifies the pass-through behavior for incoming requests based on the Content-Type header in the request, and the available mapping templates specified as the @requestTemplates@ property on the Integration resource. There are three valid values: @WHEN_NO_MATCH@ , @WHEN_NO_TEMPLATES@ , and @NEVER@ .      * @WHEN_NO_MATCH@ passes the request body for unmapped content types through to the integration back end without transformation.     * @NEVER@ rejects unmapped content types with an HTTP 415 'Unsupported Media Type' response.     * @WHEN_NO_TEMPLATES@ allows pass-through when the integration has NO content types mapped to templates. However if there is at least one content type defined, unmapped content types will be rejected with the same 415 response.
pPassthroughBehavior :: Lens' PutIntegration (Maybe Text)
pPassthroughBehavior = lens _pPassthroughBehavior (\ s a -> s{_pPassthroughBehavior = a})

-- | Specifies Uniform Resource Identifier (URI) of the integration endpoint.     * For @HTTP@ or @HTTP_PROXY@ integrations, the URI must be a fully formed, encoded HTTP(S) URL according to the <https://en.wikipedia.org/wiki/Uniform_Resource_Identifier RFC-3986 specification> , for either standard integration, where @connectionType@ is not @VPC_LINK@ , or private integration, where @connectionType@ is @VPC_LINK@ . For a private HTTP integration, the URI is not used for routing.      * For @AWS@ or @AWS_PROXY@ integrations, the URI is of the form @arn:aws:apigateway:{region}:{subdomain.service|service}:path|action/{service_api}@ . Here, @{Region}@ is the API Gateway region (e.g., @us-east-1@ ); @{service}@ is the name of the integrated AWS service (e.g., @s3@ ); and @{subdomain}@ is a designated subdomain supported by certain AWS service for fast host-name lookup. @action@ can be used for an AWS service action-based API, using an @Action={name}&{p1}={v1}&p2={v2}...@ query string. The ensuing @{service_api}@ refers to a supported action @{name}@ plus any required input parameters. Alternatively, @path@ can be used for an AWS service path-based API. The ensuing @service_api@ refers to the path to an AWS service resource, including the region of the integrated AWS service, if applicable. For example, for integration with the S3 API of @<http://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectGET.html GetObject> @ , the @uri@ can be either @arn:aws:apigateway:us-west-2:s3:action/GetObject&Bucket={bucket}&Key={key}@ or @arn:aws:apigateway:us-west-2:s3:path/{bucket}/{key}@
pUri :: Lens' PutIntegration (Maybe Text)
pUri = lens _pUri (\ s a -> s{_pUri = a})

-- | Specifies a put integration input's cache namespace.
pCacheNamespace :: Lens' PutIntegration (Maybe Text)
pCacheNamespace = lens _pCacheNamespace (\ s a -> s{_pCacheNamespace = a})

-- | Custom timeout between 50 and 29,000 milliseconds. The default value is 29,000 milliseconds or 29 seconds.
pTimeoutInMillis :: Lens' PutIntegration (Maybe Int)
pTimeoutInMillis = lens _pTimeoutInMillis (\ s a -> s{_pTimeoutInMillis = a})

-- | The type of the network connection to the integration endpoint. The valid value is @INTERNET@ for connections through the public routable internet or @VPC_LINK@ for private connections between API Gateway and a network load balancer in a VPC. The default value is @INTERNET@ .
pConnectionType :: Lens' PutIntegration (Maybe ConnectionType)
pConnectionType = lens _pConnectionType (\ s a -> s{_pConnectionType = a})

-- | Specifies a put integration HTTP method. When the integration type is HTTP or AWS, this field is required.
pIntegrationHTTPMethod :: Lens' PutIntegration (Maybe Text)
pIntegrationHTTPMethod = lens _pIntegrationHTTPMethod (\ s a -> s{_pIntegrationHTTPMethod = a})

-- | Specifies a put integration input's cache key parameters.
pCacheKeyParameters :: Lens' PutIntegration [Text]
pCacheKeyParameters = lens _pCacheKeyParameters (\ s a -> s{_pCacheKeyParameters = a}) . _Default . _Coerce

-- | [Required] The string identifier of the associated 'RestApi' .
pRestAPIId :: Lens' PutIntegration Text
pRestAPIId = lens _pRestAPIId (\ s a -> s{_pRestAPIId = a})

-- | [Required] Specifies a put integration request's resource ID.
pResourceId :: Lens' PutIntegration Text
pResourceId = lens _pResourceId (\ s a -> s{_pResourceId = a})

-- | [Required] Specifies a put integration request's HTTP method.
pHttpMethod :: Lens' PutIntegration Text
pHttpMethod = lens _pHttpMethod (\ s a -> s{_pHttpMethod = a})

-- | [Required] Specifies a put integration input's type.
pType :: Lens' PutIntegration IntegrationType
pType = lens _pType (\ s a -> s{_pType = a})

instance AWSRequest PutIntegration where
        type Rs PutIntegration = Integration
        request = putJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable PutIntegration where

instance NFData PutIntegration where

instance ToHeaders PutIntegration where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON PutIntegration where
        toJSON PutIntegration'{..}
          = object
              (catMaybes
                 [("requestTemplates" .=) <$> _pRequestTemplates,
                  ("credentials" .=) <$> _pCredentials,
                  ("connectionId" .=) <$> _pConnectionId,
                  ("requestParameters" .=) <$> _pRequestParameters,
                  ("contentHandling" .=) <$> _pContentHandling,
                  ("passthroughBehavior" .=) <$> _pPassthroughBehavior,
                  ("uri" .=) <$> _pUri,
                  ("cacheNamespace" .=) <$> _pCacheNamespace,
                  ("timeoutInMillis" .=) <$> _pTimeoutInMillis,
                  ("connectionType" .=) <$> _pConnectionType,
                  ("httpMethod" .=) <$> _pIntegrationHTTPMethod,
                  ("cacheKeyParameters" .=) <$> _pCacheKeyParameters,
                  Just ("type" .= _pType)])

instance ToPath PutIntegration where
        toPath PutIntegration'{..}
          = mconcat
              ["/restapis/", toBS _pRestAPIId, "/resources/",
               toBS _pResourceId, "/methods/", toBS _pHttpMethod,
               "/integration"]

instance ToQuery PutIntegration where
        toQuery = const mempty
