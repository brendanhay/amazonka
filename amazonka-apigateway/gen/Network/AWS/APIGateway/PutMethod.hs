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
-- Module      : Network.AWS.APIGateway.PutMethod
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add a method to an existing Resource resource.
--
-- /See:/ <http://docs.aws.amazon.com/apigateway/api-reference/resource/PutMethod.html AWS API Reference> for PutMethod.
module Network.AWS.APIGateway.PutMethod
    (
    -- * Creating a Request
      putMethod
    , PutMethod
    -- * Request Lenses
    , putRequestModels
    , putRequestParameters
    , putApiKeyRequired
    , putRestAPIId
    , putResourceId
    , putHttpMethod
    , putAuthorizationType

    -- * Destructuring the Response
    , method
    , Method
    -- * Response Lenses
    , mMethodResponses
    , mHttpMethod
    , mRequestModels
    , mRequestParameters
    , mAuthorizationType
    , mApiKeyRequired
    , mMethodIntegration
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Request to add a method to an existing Resource resource.
--
-- /See:/ 'putMethod' smart constructor.
data PutMethod = PutMethod'
    { _putRequestModels     :: !(Maybe (Map Text Text))
    , _putRequestParameters :: !(Maybe (Map Text Bool))
    , _putApiKeyRequired    :: !(Maybe Bool)
    , _putRestAPIId         :: !Text
    , _putResourceId        :: !Text
    , _putHttpMethod        :: !Text
    , _putAuthorizationType :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutMethod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'putRequestModels'
--
-- * 'putRequestParameters'
--
-- * 'putApiKeyRequired'
--
-- * 'putRestAPIId'
--
-- * 'putResourceId'
--
-- * 'putHttpMethod'
--
-- * 'putAuthorizationType'
putMethod
    :: Text -- ^ 'putRestAPIId'
    -> Text -- ^ 'putResourceId'
    -> Text -- ^ 'putHttpMethod'
    -> Text -- ^ 'putAuthorizationType'
    -> PutMethod
putMethod pRestAPIId_ pResourceId_ pHttpMethod_ pAuthorizationType_ =
    PutMethod'
    { _putRequestModels = Nothing
    , _putRequestParameters = Nothing
    , _putApiKeyRequired = Nothing
    , _putRestAPIId = pRestAPIId_
    , _putResourceId = pResourceId_
    , _putHttpMethod = pHttpMethod_
    , _putAuthorizationType = pAuthorizationType_
    }

-- | Specifies the Model resources used for the request\'s content type.
-- Request models are represented as a key\/value map, with a content type
-- as the key and a Model name as the value.
putRequestModels :: Lens' PutMethod (HashMap Text Text)
putRequestModels = lens _putRequestModels (\ s a -> s{_putRequestModels = a}) . _Default . _Map;

-- | Represents requests parameters that are sent with the backend request.
-- Request parameters are represented as a key\/value map, with a
-- destination as the key and a source as the value. A source must match an
-- existing method request parameter, or a static value. Static values must
-- be enclosed with single quotes, and be pre-encoded based on their
-- destination in the request. The destination must match the pattern
-- 'integration.request.{location}.{name}', where 'location' is either
-- querystring, path, or header. 'name' must be a valid, unique parameter
-- name.
putRequestParameters :: Lens' PutMethod (HashMap Text Bool)
putRequestParameters = lens _putRequestParameters (\ s a -> s{_putRequestParameters = a}) . _Default . _Map;

-- | Specifies whether the method required a valid ApiKey.
putApiKeyRequired :: Lens' PutMethod (Maybe Bool)
putApiKeyRequired = lens _putApiKeyRequired (\ s a -> s{_putApiKeyRequired = a});

-- | The RestApi identifier for the new Method resource.
putRestAPIId :: Lens' PutMethod Text
putRestAPIId = lens _putRestAPIId (\ s a -> s{_putRestAPIId = a});

-- | The Resource identifier for the new Method resource.
putResourceId :: Lens' PutMethod Text
putResourceId = lens _putResourceId (\ s a -> s{_putResourceId = a});

-- | Specifies the put method request\'s HTTP method type.
putHttpMethod :: Lens' PutMethod Text
putHttpMethod = lens _putHttpMethod (\ s a -> s{_putHttpMethod = a});

-- | Specifies the type of authorization used for the method.
putAuthorizationType :: Lens' PutMethod Text
putAuthorizationType = lens _putAuthorizationType (\ s a -> s{_putAuthorizationType = a});

instance AWSRequest PutMethod where
        type Rs PutMethod = Method
        request = putJSON aPIGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders PutMethod where
        toHeaders = const mempty

instance ToJSON PutMethod where
        toJSON PutMethod'{..}
          = object
              (catMaybes
                 [("requestModels" .=) <$> _putRequestModels,
                  ("requestParameters" .=) <$> _putRequestParameters,
                  ("apiKeyRequired" .=) <$> _putApiKeyRequired,
                  Just ("authorizationType" .= _putAuthorizationType)])

instance ToPath PutMethod where
        toPath PutMethod'{..}
          = mconcat
              ["/restapis/", toBS _putRestAPIId, "/resources/",
               toBS _putResourceId, "/methods/",
               toBS _putHttpMethod]

instance ToQuery PutMethod where
        toQuery = const mempty
