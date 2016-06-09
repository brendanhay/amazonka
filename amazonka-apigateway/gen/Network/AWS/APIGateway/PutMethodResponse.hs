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
-- Module      : Network.AWS.APIGateway.PutMethodResponse
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a < MethodResponse> to an existing < Method> resource.
module Network.AWS.APIGateway.PutMethodResponse
    (
    -- * Creating a Request
      putMethodResponse
    , PutMethodResponse
    -- * Request Lenses
    , pmResponseModels
    , pmResponseParameters
    , pmRestAPIId
    , pmResourceId
    , pmHttpMethod
    , pmStatusCode

    -- * Destructuring the Response
    , methodResponse
    , MethodResponse
    -- * Response Lenses
    , mResponseModels
    , mStatusCode
    , mResponseParameters
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Request to add a < MethodResponse> to an existing < Method> resource.
--
-- /See:/ 'putMethodResponse' smart constructor.
data PutMethodResponse = PutMethodResponse'
    { _pmResponseModels     :: !(Maybe (Map Text Text))
    , _pmResponseParameters :: !(Maybe (Map Text Bool))
    , _pmRestAPIId          :: !Text
    , _pmResourceId         :: !Text
    , _pmHttpMethod         :: !Text
    , _pmStatusCode         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutMethodResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmResponseModels'
--
-- * 'pmResponseParameters'
--
-- * 'pmRestAPIId'
--
-- * 'pmResourceId'
--
-- * 'pmHttpMethod'
--
-- * 'pmStatusCode'
putMethodResponse
    :: Text -- ^ 'pmRestAPIId'
    -> Text -- ^ 'pmResourceId'
    -> Text -- ^ 'pmHttpMethod'
    -> Text -- ^ 'pmStatusCode'
    -> PutMethodResponse
putMethodResponse pRestAPIId_ pResourceId_ pHttpMethod_ pStatusCode_ =
    PutMethodResponse'
    { _pmResponseModels = Nothing
    , _pmResponseParameters = Nothing
    , _pmRestAPIId = pRestAPIId_
    , _pmResourceId = pResourceId_
    , _pmHttpMethod = pHttpMethod_
    , _pmStatusCode = pStatusCode_
    }

-- | Specifies the < Model> resources used for the response\'s content type. Response models are represented as a key\/value map, with a content type as the key and a < Model> name as the value.
pmResponseModels :: Lens' PutMethodResponse (HashMap Text Text)
pmResponseModels = lens _pmResponseModels (\ s a -> s{_pmResponseModels = a}) . _Default . _Map;

-- | Represents response parameters that can be sent back to the caller by Amazon API Gateway. Response parameters are represented as a key\/value map, with a destination as the key and a Boolean flag as the value. The Boolean flag is used to specify whether the parameter is required. A destination must match the pattern 'method.response.header.{name}', where 'name' is a valid, unique header name. Destinations specified here are available to the integration for mapping from integration response parameters.
pmResponseParameters :: Lens' PutMethodResponse (HashMap Text Bool)
pmResponseParameters = lens _pmResponseParameters (\ s a -> s{_pmResponseParameters = a}) . _Default . _Map;

-- | The < RestApi> identifier for the < Method> resource.
pmRestAPIId :: Lens' PutMethodResponse Text
pmRestAPIId = lens _pmRestAPIId (\ s a -> s{_pmRestAPIId = a});

-- | The < Resource> identifier for the < Method> resource.
pmResourceId :: Lens' PutMethodResponse Text
pmResourceId = lens _pmResourceId (\ s a -> s{_pmResourceId = a});

-- | The HTTP verb that identifies the < Method> resource.
pmHttpMethod :: Lens' PutMethodResponse Text
pmHttpMethod = lens _pmHttpMethod (\ s a -> s{_pmHttpMethod = a});

-- | The method response\'s status code.
pmStatusCode :: Lens' PutMethodResponse Text
pmStatusCode = lens _pmStatusCode (\ s a -> s{_pmStatusCode = a});

instance AWSRequest PutMethodResponse where
        type Rs PutMethodResponse = MethodResponse
        request = putJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable PutMethodResponse

instance NFData PutMethodResponse

instance ToHeaders PutMethodResponse where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON PutMethodResponse where
        toJSON PutMethodResponse'{..}
          = object
              (catMaybes
                 [("responseModels" .=) <$> _pmResponseModels,
                  ("responseParameters" .=) <$> _pmResponseParameters])

instance ToPath PutMethodResponse where
        toPath PutMethodResponse'{..}
          = mconcat
              ["/restapis/", toBS _pmRestAPIId, "/resources/",
               toBS _pmResourceId, "/methods/", toBS _pmHttpMethod,
               "/responses/", toBS _pmStatusCode]

instance ToQuery PutMethodResponse where
        toQuery = const mempty
