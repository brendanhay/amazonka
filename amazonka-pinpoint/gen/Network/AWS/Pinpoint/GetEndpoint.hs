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
-- Module      : Network.AWS.Pinpoint.GetEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an endpoint.
module Network.AWS.Pinpoint.GetEndpoint
    (
    -- * Creating a Request
      getEndpoint
    , GetEndpoint
    -- * Request Lenses
    , geApplicationId
    , geEndpointId

    -- * Destructuring the Response
    , getEndpointResponse
    , GetEndpointResponse
    -- * Response Lenses
    , gersResponseStatus
    , gersEndpointResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getEndpoint' smart constructor.
data GetEndpoint = GetEndpoint'
  { _geApplicationId :: !Text
  , _geEndpointId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'geApplicationId' - Undocumented member.
--
-- * 'geEndpointId' - Undocumented member.
getEndpoint
    :: Text -- ^ 'geApplicationId'
    -> Text -- ^ 'geEndpointId'
    -> GetEndpoint
getEndpoint pApplicationId_ pEndpointId_ =
  GetEndpoint'
    {_geApplicationId = pApplicationId_, _geEndpointId = pEndpointId_}


-- | Undocumented member.
geApplicationId :: Lens' GetEndpoint Text
geApplicationId = lens _geApplicationId (\ s a -> s{_geApplicationId = a})

-- | Undocumented member.
geEndpointId :: Lens' GetEndpoint Text
geEndpointId = lens _geEndpointId (\ s a -> s{_geEndpointId = a})

instance AWSRequest GetEndpoint where
        type Rs GetEndpoint = GetEndpointResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetEndpointResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetEndpoint where

instance NFData GetEndpoint where

instance ToHeaders GetEndpoint where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetEndpoint where
        toPath GetEndpoint'{..}
          = mconcat
              ["/v1/apps/", toBS _geApplicationId, "/endpoints/",
               toBS _geEndpointId]

instance ToQuery GetEndpoint where
        toQuery = const mempty

-- | /See:/ 'getEndpointResponse' smart constructor.
data GetEndpointResponse = GetEndpointResponse'
  { _gersResponseStatus   :: !Int
  , _gersEndpointResponse :: !EndpointResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gersResponseStatus' - -- | The response status code.
--
-- * 'gersEndpointResponse' - Undocumented member.
getEndpointResponse
    :: Int -- ^ 'gersResponseStatus'
    -> EndpointResponse -- ^ 'gersEndpointResponse'
    -> GetEndpointResponse
getEndpointResponse pResponseStatus_ pEndpointResponse_ =
  GetEndpointResponse'
    { _gersResponseStatus = pResponseStatus_
    , _gersEndpointResponse = pEndpointResponse_
    }


-- | -- | The response status code.
gersResponseStatus :: Lens' GetEndpointResponse Int
gersResponseStatus = lens _gersResponseStatus (\ s a -> s{_gersResponseStatus = a})

-- | Undocumented member.
gersEndpointResponse :: Lens' GetEndpointResponse EndpointResponse
gersEndpointResponse = lens _gersEndpointResponse (\ s a -> s{_gersEndpointResponse = a})

instance NFData GetEndpointResponse where
