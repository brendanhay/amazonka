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
-- Module      : Network.AWS.APIGateway.GetIntegrationResponse
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a get integration response.
module Network.AWS.APIGateway.GetIntegrationResponse
    (
    -- * Creating a Request
      getIntegrationResponse
    , GetIntegrationResponse
    -- * Request Lenses
    , getRestAPIId
    , getResourceId
    , getHttpMethod
    , getStatusCode

    -- * Destructuring the Response
    , integrationResponse
    , IntegrationResponse
    -- * Response Lenses
    , iResponseTemplates
    , iSelectionPattern
    , iStatusCode
    , iResponseParameters
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a get integration response request.
--
-- /See:/ 'getIntegrationResponse' smart constructor.
data GetIntegrationResponse = GetIntegrationResponse'
    { _getRestAPIId  :: !Text
    , _getResourceId :: !Text
    , _getHttpMethod :: !Text
    , _getStatusCode :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetIntegrationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getRestAPIId'
--
-- * 'getResourceId'
--
-- * 'getHttpMethod'
--
-- * 'getStatusCode'
getIntegrationResponse
    :: Text -- ^ 'getRestAPIId'
    -> Text -- ^ 'getResourceId'
    -> Text -- ^ 'getHttpMethod'
    -> Text -- ^ 'getStatusCode'
    -> GetIntegrationResponse
getIntegrationResponse pRestAPIId_ pResourceId_ pHttpMethod_ pStatusCode_ =
    GetIntegrationResponse'
    { _getRestAPIId = pRestAPIId_
    , _getResourceId = pResourceId_
    , _getHttpMethod = pHttpMethod_
    , _getStatusCode = pStatusCode_
    }

-- | Specifies a get integration response request\'s API identifier.
getRestAPIId :: Lens' GetIntegrationResponse Text
getRestAPIId = lens _getRestAPIId (\ s a -> s{_getRestAPIId = a});

-- | Specifies a get integration response request\'s resource identifier.
getResourceId :: Lens' GetIntegrationResponse Text
getResourceId = lens _getResourceId (\ s a -> s{_getResourceId = a});

-- | Specifies a get integration response request\'s HTTP method.
getHttpMethod :: Lens' GetIntegrationResponse Text
getHttpMethod = lens _getHttpMethod (\ s a -> s{_getHttpMethod = a});

-- | Specifies a get integration response request\'s status code.
getStatusCode :: Lens' GetIntegrationResponse Text
getStatusCode = lens _getStatusCode (\ s a -> s{_getStatusCode = a});

instance AWSRequest GetIntegrationResponse where
        type Rs GetIntegrationResponse = IntegrationResponse
        request = get aPIGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetIntegrationResponse

instance ToHeaders GetIntegrationResponse where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetIntegrationResponse where
        toPath GetIntegrationResponse'{..}
          = mconcat
              ["/restapis/", toBS _getRestAPIId, "/resources/",
               toBS _getResourceId, "/methods/",
               toBS _getHttpMethod, "/integration/responses/",
               toBS _getStatusCode]

instance ToQuery GetIntegrationResponse where
        toQuery = const mempty
