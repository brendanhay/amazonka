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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a put integration.
module Network.AWS.APIGateway.PutIntegration
    (
    -- * Creating a Request
      putIntegration
    , PutIntegration
    -- * Request Lenses
    , pRequestTemplates
    , pCredentials
    , pRequestParameters
    , pPassthroughBehavior
    , pUri
    , pCacheNamespace
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
    , iRequestParameters
    , iPassthroughBehavior
    , iUri
    , iIntegrationResponses
    , iCacheNamespace
    , iType
    , iCacheKeyParameters
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a put integration request.
--
-- /See:/ 'putIntegration' smart constructor.
data PutIntegration = PutIntegration'
    { _pRequestTemplates      :: !(Maybe (Map Text Text))
    , _pCredentials           :: !(Maybe Text)
    , _pRequestParameters     :: !(Maybe (Map Text Text))
    , _pPassthroughBehavior   :: !(Maybe Text)
    , _pUri                   :: !(Maybe Text)
    , _pCacheNamespace        :: !(Maybe Text)
    , _pIntegrationHTTPMethod :: !(Maybe Text)
    , _pCacheKeyParameters    :: !(Maybe [Text])
    , _pRestAPIId             :: !Text
    , _pResourceId            :: !Text
    , _pHttpMethod            :: !Text
    , _pType                  :: !IntegrationType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutIntegration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pRequestTemplates'
--
-- * 'pCredentials'
--
-- * 'pRequestParameters'
--
-- * 'pPassthroughBehavior'
--
-- * 'pUri'
--
-- * 'pCacheNamespace'
--
-- * 'pIntegrationHTTPMethod'
--
-- * 'pCacheKeyParameters'
--
-- * 'pRestAPIId'
--
-- * 'pResourceId'
--
-- * 'pHttpMethod'
--
-- * 'pType'
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
    , _pRequestParameters = Nothing
    , _pPassthroughBehavior = Nothing
    , _pUri = Nothing
    , _pCacheNamespace = Nothing
    , _pIntegrationHTTPMethod = Nothing
    , _pCacheKeyParameters = Nothing
    , _pRestAPIId = pRestAPIId_
    , _pResourceId = pResourceId_
    , _pHttpMethod = pHttpMethod_
    , _pType = pType_
    }

-- | Represents a map of Velocity templates that are applied on the request payload based on the value of the Content-Type header sent by the client. The content type value is the key in this map, and the template (as a String) is the value.
pRequestTemplates :: Lens' PutIntegration (HashMap Text Text)
pRequestTemplates = lens _pRequestTemplates (\ s a -> s{_pRequestTemplates = a}) . _Default . _Map;

-- | Specifies whether credentials are required for a put integration.
pCredentials :: Lens' PutIntegration (Maybe Text)
pCredentials = lens _pCredentials (\ s a -> s{_pCredentials = a});

-- | Represents request parameters that are sent with the backend request. Request parameters are represented as a key\/value map, with a destination as the key and a source as the value. A source must match an existing method request parameter, or a static value. Static values must be enclosed with single quotes, and be pre-encoded based on their destination in the request. The destination must match the pattern 'integration.request.{location}.{name}', where 'location' is either querystring, path, or header. 'name' must be a valid, unique parameter name.
pRequestParameters :: Lens' PutIntegration (HashMap Text Text)
pRequestParameters = lens _pRequestParameters (\ s a -> s{_pRequestParameters = a}) . _Default . _Map;

-- | Specifies the pass-through behavior for incoming requests based on the Content-Type header in the request, and the available requestTemplates defined on the Integration. There are three valid values: 'WHEN_NO_MATCH', 'WHEN_NO_TEMPLATES', and 'NEVER'.
--
-- 'WHEN_NO_MATCH' passes the request body for unmapped content types through to the Integration backend without transformation.
--
-- 'NEVER' rejects unmapped content types with an HTTP 415 \'Unsupported Media Type\' response.
--
-- 'WHEN_NO_TEMPLATES' will allow pass-through when the Integration has NO content types mapped to templates. However if there is at least one content type defined, unmapped content types will be rejected with the same 415 response.
pPassthroughBehavior :: Lens' PutIntegration (Maybe Text)
pPassthroughBehavior = lens _pPassthroughBehavior (\ s a -> s{_pPassthroughBehavior = a});

-- | Specifies a put integration input\'s Uniform Resource Identifier (URI). When the integration type is HTTP or AWS, this field is required. For integration with Lambda as an AWS service proxy, this value is of the \'arn:aws:apigateway:\<region>:lambda:path\/2015-03-31\/functions\/\<functionArn>\/invocations\' format.
pUri :: Lens' PutIntegration (Maybe Text)
pUri = lens _pUri (\ s a -> s{_pUri = a});

-- | Specifies a put integration input\'s cache namespace.
pCacheNamespace :: Lens' PutIntegration (Maybe Text)
pCacheNamespace = lens _pCacheNamespace (\ s a -> s{_pCacheNamespace = a});

-- | Specifies a put integration HTTP method. When the integration type is HTTP or AWS, this field is required.
pIntegrationHTTPMethod :: Lens' PutIntegration (Maybe Text)
pIntegrationHTTPMethod = lens _pIntegrationHTTPMethod (\ s a -> s{_pIntegrationHTTPMethod = a});

-- | Specifies a put integration input\'s cache key parameters.
pCacheKeyParameters :: Lens' PutIntegration [Text]
pCacheKeyParameters = lens _pCacheKeyParameters (\ s a -> s{_pCacheKeyParameters = a}) . _Default . _Coerce;

-- | Specifies a put integration request\'s API identifier.
pRestAPIId :: Lens' PutIntegration Text
pRestAPIId = lens _pRestAPIId (\ s a -> s{_pRestAPIId = a});

-- | Specifies a put integration request\'s resource ID.
pResourceId :: Lens' PutIntegration Text
pResourceId = lens _pResourceId (\ s a -> s{_pResourceId = a});

-- | Specifies a put integration request\'s HTTP method.
pHttpMethod :: Lens' PutIntegration Text
pHttpMethod = lens _pHttpMethod (\ s a -> s{_pHttpMethod = a});

-- | Specifies a put integration input\'s type.
pType :: Lens' PutIntegration IntegrationType
pType = lens _pType (\ s a -> s{_pType = a});

instance AWSRequest PutIntegration where
        type Rs PutIntegration = Integration
        request = putJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable PutIntegration

instance NFData PutIntegration

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
                  ("requestParameters" .=) <$> _pRequestParameters,
                  ("passthroughBehavior" .=) <$> _pPassthroughBehavior,
                  ("uri" .=) <$> _pUri,
                  ("cacheNamespace" .=) <$> _pCacheNamespace,
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
