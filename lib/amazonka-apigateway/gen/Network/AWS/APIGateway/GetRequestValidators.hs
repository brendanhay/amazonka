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
-- Module      : Network.AWS.APIGateway.GetRequestValidators
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the 'RequestValidators' collection of a given 'RestApi' .
--
--
module Network.AWS.APIGateway.GetRequestValidators
    (
    -- * Creating a Request
      getRequestValidators
    , GetRequestValidators
    -- * Request Lenses
    , grvLimit
    , grvPosition
    , grvRestAPIId

    -- * Destructuring the Response
    , getRequestValidatorsResponse
    , GetRequestValidatorsResponse
    -- * Response Lenses
    , grvrsItems
    , grvrsPosition
    , grvrsResponseStatus
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Gets the 'RequestValidators' collection of a given 'RestApi' .
--
--
--
-- /See:/ 'getRequestValidators' smart constructor.
data GetRequestValidators = GetRequestValidators'
  { _grvLimit     :: !(Maybe Int)
  , _grvPosition  :: !(Maybe Text)
  , _grvRestAPIId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRequestValidators' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grvLimit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- * 'grvPosition' - The current pagination position in the paged result set.
--
-- * 'grvRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
getRequestValidators
    :: Text -- ^ 'grvRestAPIId'
    -> GetRequestValidators
getRequestValidators pRestAPIId_ =
  GetRequestValidators'
    {_grvLimit = Nothing, _grvPosition = Nothing, _grvRestAPIId = pRestAPIId_}


-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
grvLimit :: Lens' GetRequestValidators (Maybe Int)
grvLimit = lens _grvLimit (\ s a -> s{_grvLimit = a})

-- | The current pagination position in the paged result set.
grvPosition :: Lens' GetRequestValidators (Maybe Text)
grvPosition = lens _grvPosition (\ s a -> s{_grvPosition = a})

-- | [Required] The string identifier of the associated 'RestApi' .
grvRestAPIId :: Lens' GetRequestValidators Text
grvRestAPIId = lens _grvRestAPIId (\ s a -> s{_grvRestAPIId = a})

instance AWSRequest GetRequestValidators where
        type Rs GetRequestValidators =
             GetRequestValidatorsResponse
        request = get apiGateway
        response
          = receiveJSON
              (\ s h x ->
                 GetRequestValidatorsResponse' <$>
                   (x .?> "item" .!@ mempty) <*> (x .?> "position") <*>
                     (pure (fromEnum s)))

instance Hashable GetRequestValidators where

instance NFData GetRequestValidators where

instance ToHeaders GetRequestValidators where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetRequestValidators where
        toPath GetRequestValidators'{..}
          = mconcat
              ["/restapis/", toBS _grvRestAPIId,
               "/requestvalidators"]

instance ToQuery GetRequestValidators where
        toQuery GetRequestValidators'{..}
          = mconcat
              ["limit" =: _grvLimit, "position" =: _grvPosition]

-- | A collection of 'RequestValidator' resources of a given 'RestApi' .
--
--
-- In Swagger, the 'RequestValidators' of an API is defined by the <http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-swagger-extensions.html#api-gateway-swagger-extensions-request-validators.html x-amazon-apigateway-request-validators> extension.
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-method-request-validation.html Enable Basic Request Validation in API Gateway>
--
-- /See:/ 'getRequestValidatorsResponse' smart constructor.
data GetRequestValidatorsResponse = GetRequestValidatorsResponse'
  { _grvrsItems          :: !(Maybe [RequestValidator])
  , _grvrsPosition       :: !(Maybe Text)
  , _grvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRequestValidatorsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grvrsItems' - The current page of elements from this collection.
--
-- * 'grvrsPosition' - Undocumented member.
--
-- * 'grvrsResponseStatus' - -- | The response status code.
getRequestValidatorsResponse
    :: Int -- ^ 'grvrsResponseStatus'
    -> GetRequestValidatorsResponse
getRequestValidatorsResponse pResponseStatus_ =
  GetRequestValidatorsResponse'
    { _grvrsItems = Nothing
    , _grvrsPosition = Nothing
    , _grvrsResponseStatus = pResponseStatus_
    }


-- | The current page of elements from this collection.
grvrsItems :: Lens' GetRequestValidatorsResponse [RequestValidator]
grvrsItems = lens _grvrsItems (\ s a -> s{_grvrsItems = a}) . _Default . _Coerce

-- | Undocumented member.
grvrsPosition :: Lens' GetRequestValidatorsResponse (Maybe Text)
grvrsPosition = lens _grvrsPosition (\ s a -> s{_grvrsPosition = a})

-- | -- | The response status code.
grvrsResponseStatus :: Lens' GetRequestValidatorsResponse Int
grvrsResponseStatus = lens _grvrsResponseStatus (\ s a -> s{_grvrsResponseStatus = a})

instance NFData GetRequestValidatorsResponse where
