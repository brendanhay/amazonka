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
-- Module      : Network.AWS.Pinpoint.GetUserEndpoints
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the endpoints that are associated with a User ID.
module Network.AWS.Pinpoint.GetUserEndpoints
    (
    -- * Creating a Request
      getUserEndpoints
    , GetUserEndpoints
    -- * Request Lenses
    , gueApplicationId
    , gueUserId

    -- * Destructuring the Response
    , getUserEndpointsResponse
    , GetUserEndpointsResponse
    -- * Response Lenses
    , guersResponseStatus
    , guersEndpointsResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getUserEndpoints' smart constructor.
data GetUserEndpoints = GetUserEndpoints'
  { _gueApplicationId :: !Text
  , _gueUserId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUserEndpoints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gueApplicationId' - The unique ID of your Amazon Pinpoint application.
--
-- * 'gueUserId' - The unique ID of the user.
getUserEndpoints
    :: Text -- ^ 'gueApplicationId'
    -> Text -- ^ 'gueUserId'
    -> GetUserEndpoints
getUserEndpoints pApplicationId_ pUserId_ =
  GetUserEndpoints' {_gueApplicationId = pApplicationId_, _gueUserId = pUserId_}


-- | The unique ID of your Amazon Pinpoint application.
gueApplicationId :: Lens' GetUserEndpoints Text
gueApplicationId = lens _gueApplicationId (\ s a -> s{_gueApplicationId = a})

-- | The unique ID of the user.
gueUserId :: Lens' GetUserEndpoints Text
gueUserId = lens _gueUserId (\ s a -> s{_gueUserId = a})

instance AWSRequest GetUserEndpoints where
        type Rs GetUserEndpoints = GetUserEndpointsResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetUserEndpointsResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetUserEndpoints where

instance NFData GetUserEndpoints where

instance ToHeaders GetUserEndpoints where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetUserEndpoints where
        toPath GetUserEndpoints'{..}
          = mconcat
              ["/v1/apps/", toBS _gueApplicationId, "/users/",
               toBS _gueUserId]

instance ToQuery GetUserEndpoints where
        toQuery = const mempty

-- | /See:/ 'getUserEndpointsResponse' smart constructor.
data GetUserEndpointsResponse = GetUserEndpointsResponse'
  { _guersResponseStatus    :: !Int
  , _guersEndpointsResponse :: !EndpointsResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUserEndpointsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'guersResponseStatus' - -- | The response status code.
--
-- * 'guersEndpointsResponse' - Undocumented member.
getUserEndpointsResponse
    :: Int -- ^ 'guersResponseStatus'
    -> EndpointsResponse -- ^ 'guersEndpointsResponse'
    -> GetUserEndpointsResponse
getUserEndpointsResponse pResponseStatus_ pEndpointsResponse_ =
  GetUserEndpointsResponse'
    { _guersResponseStatus = pResponseStatus_
    , _guersEndpointsResponse = pEndpointsResponse_
    }


-- | -- | The response status code.
guersResponseStatus :: Lens' GetUserEndpointsResponse Int
guersResponseStatus = lens _guersResponseStatus (\ s a -> s{_guersResponseStatus = a})

-- | Undocumented member.
guersEndpointsResponse :: Lens' GetUserEndpointsResponse EndpointsResponse
guersEndpointsResponse = lens _guersEndpointsResponse (\ s a -> s{_guersEndpointsResponse = a})

instance NFData GetUserEndpointsResponse where
