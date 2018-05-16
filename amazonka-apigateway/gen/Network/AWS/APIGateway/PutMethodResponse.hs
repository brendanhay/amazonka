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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a 'MethodResponse' to an existing 'Method' resource.
--
--
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

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to add a 'MethodResponse' to an existing 'Method' resource.
--
--
--
-- /See:/ 'putMethodResponse' smart constructor.
data PutMethodResponse = PutMethodResponse'
  { _pmResponseModels     :: !(Maybe (Map Text Text))
  , _pmResponseParameters :: !(Maybe (Map Text Bool))
  , _pmRestAPIId          :: !Text
  , _pmResourceId         :: !Text
  , _pmHttpMethod         :: !Text
  , _pmStatusCode         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutMethodResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmResponseModels' - Specifies the 'Model' resources used for the response's content type. Response models are represented as a key/value map, with a content type as the key and a 'Model' name as the value.
--
-- * 'pmResponseParameters' - A key-value map specifying required or optional response parameters that API Gateway can send back to the caller. A key defines a method response header name and the associated value is a Boolean flag indicating whether the method response parameter is required or not. The method response header names must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The response parameter names defined here are available in the integration response to be mapped from an integration response header expressed in @integration.response.header.{name}@ , a static value enclosed within a pair of single quotes (e.g., @'application/json'@ ), or a JSON expression from the back-end response payload in the form of @integration.response.body.{JSON-expression}@ , where @JSON-expression@ is a valid JSON expression without the @> @ prefix.)
--
-- * 'pmRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'pmResourceId' - [Required] The 'Resource' identifier for the 'Method' resource.
--
-- * 'pmHttpMethod' - [Required] The HTTP verb of the 'Method' resource.
--
-- * 'pmStatusCode' - [Required] The method response's status code.
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


-- | Specifies the 'Model' resources used for the response's content type. Response models are represented as a key/value map, with a content type as the key and a 'Model' name as the value.
pmResponseModels :: Lens' PutMethodResponse (HashMap Text Text)
pmResponseModels = lens _pmResponseModels (\ s a -> s{_pmResponseModels = a}) . _Default . _Map

-- | A key-value map specifying required or optional response parameters that API Gateway can send back to the caller. A key defines a method response header name and the associated value is a Boolean flag indicating whether the method response parameter is required or not. The method response header names must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The response parameter names defined here are available in the integration response to be mapped from an integration response header expressed in @integration.response.header.{name}@ , a static value enclosed within a pair of single quotes (e.g., @'application/json'@ ), or a JSON expression from the back-end response payload in the form of @integration.response.body.{JSON-expression}@ , where @JSON-expression@ is a valid JSON expression without the @> @ prefix.)
pmResponseParameters :: Lens' PutMethodResponse (HashMap Text Bool)
pmResponseParameters = lens _pmResponseParameters (\ s a -> s{_pmResponseParameters = a}) . _Default . _Map

-- | [Required] The string identifier of the associated 'RestApi' .
pmRestAPIId :: Lens' PutMethodResponse Text
pmRestAPIId = lens _pmRestAPIId (\ s a -> s{_pmRestAPIId = a})

-- | [Required] The 'Resource' identifier for the 'Method' resource.
pmResourceId :: Lens' PutMethodResponse Text
pmResourceId = lens _pmResourceId (\ s a -> s{_pmResourceId = a})

-- | [Required] The HTTP verb of the 'Method' resource.
pmHttpMethod :: Lens' PutMethodResponse Text
pmHttpMethod = lens _pmHttpMethod (\ s a -> s{_pmHttpMethod = a})

-- | [Required] The method response's status code.
pmStatusCode :: Lens' PutMethodResponse Text
pmStatusCode = lens _pmStatusCode (\ s a -> s{_pmStatusCode = a})

instance AWSRequest PutMethodResponse where
        type Rs PutMethodResponse = MethodResponse
        request = putJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable PutMethodResponse where

instance NFData PutMethodResponse where

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
