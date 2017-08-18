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
-- Module      : Network.AWS.APIGateway.GetMethod
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an existing 'Method' resource.
--
--
module Network.AWS.APIGateway.GetMethod
    (
    -- * Creating a Request
      getMethod
    , GetMethod
    -- * Request Lenses
    , gmmRestAPIId
    , gmmResourceId
    , gmmHttpMethod

    -- * Destructuring the Response
    , method
    , Method
    -- * Response Lenses
    , mMethodResponses
    , mHttpMethod
    , mRequestValidatorId
    , mRequestModels
    , mRequestParameters
    , mAuthorizerId
    , mOperationName
    , mAuthorizationType
    , mApiKeyRequired
    , mMethodIntegration
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Request to describe an existing 'Method' resource.
--
--
--
-- /See:/ 'getMethod' smart constructor.
data GetMethod = GetMethod'
    { _gmmRestAPIId  :: !Text
    , _gmmResourceId :: !Text
    , _gmmHttpMethod :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetMethod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmmRestAPIId' - The string identifier of the associated 'RestApi' .
--
-- * 'gmmResourceId' - The 'Resource' identifier for the 'Method' resource.
--
-- * 'gmmHttpMethod' - Specifies the method request's HTTP method type.
getMethod
    :: Text -- ^ 'gmmRestAPIId'
    -> Text -- ^ 'gmmResourceId'
    -> Text -- ^ 'gmmHttpMethod'
    -> GetMethod
getMethod pRestAPIId_ pResourceId_ pHttpMethod_ =
    GetMethod'
    { _gmmRestAPIId = pRestAPIId_
    , _gmmResourceId = pResourceId_
    , _gmmHttpMethod = pHttpMethod_
    }

-- | The string identifier of the associated 'RestApi' .
gmmRestAPIId :: Lens' GetMethod Text
gmmRestAPIId = lens _gmmRestAPIId (\ s a -> s{_gmmRestAPIId = a});

-- | The 'Resource' identifier for the 'Method' resource.
gmmResourceId :: Lens' GetMethod Text
gmmResourceId = lens _gmmResourceId (\ s a -> s{_gmmResourceId = a});

-- | Specifies the method request's HTTP method type.
gmmHttpMethod :: Lens' GetMethod Text
gmmHttpMethod = lens _gmmHttpMethod (\ s a -> s{_gmmHttpMethod = a});

instance AWSRequest GetMethod where
        type Rs GetMethod = Method
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetMethod

instance NFData GetMethod

instance ToHeaders GetMethod where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetMethod where
        toPath GetMethod'{..}
          = mconcat
              ["/restapis/", toBS _gmmRestAPIId, "/resources/",
               toBS _gmmResourceId, "/methods/",
               toBS _gmmHttpMethod]

instance ToQuery GetMethod where
        toQuery = const mempty
