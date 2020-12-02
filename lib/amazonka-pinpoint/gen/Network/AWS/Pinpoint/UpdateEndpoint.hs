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
-- Module      : Network.AWS.Pinpoint.UpdateEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use to update an endpoint.
module Network.AWS.Pinpoint.UpdateEndpoint
    (
    -- * Creating a Request
      updateEndpoint
    , UpdateEndpoint
    -- * Request Lenses
    , ueApplicationId
    , ueEndpointId
    , ueEndpointRequest

    -- * Destructuring the Response
    , updateEndpointResponse
    , UpdateEndpointResponse
    -- * Response Lenses
    , uersResponseStatus
    , uersMessageBody
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateEndpoint' smart constructor.
data UpdateEndpoint = UpdateEndpoint'
  { _ueApplicationId   :: !Text
  , _ueEndpointId      :: !Text
  , _ueEndpointRequest :: !EndpointRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ueApplicationId' - Undocumented member.
--
-- * 'ueEndpointId' - Undocumented member.
--
-- * 'ueEndpointRequest' - Undocumented member.
updateEndpoint
    :: Text -- ^ 'ueApplicationId'
    -> Text -- ^ 'ueEndpointId'
    -> EndpointRequest -- ^ 'ueEndpointRequest'
    -> UpdateEndpoint
updateEndpoint pApplicationId_ pEndpointId_ pEndpointRequest_ =
  UpdateEndpoint'
    { _ueApplicationId = pApplicationId_
    , _ueEndpointId = pEndpointId_
    , _ueEndpointRequest = pEndpointRequest_
    }


-- | Undocumented member.
ueApplicationId :: Lens' UpdateEndpoint Text
ueApplicationId = lens _ueApplicationId (\ s a -> s{_ueApplicationId = a})

-- | Undocumented member.
ueEndpointId :: Lens' UpdateEndpoint Text
ueEndpointId = lens _ueEndpointId (\ s a -> s{_ueEndpointId = a})

-- | Undocumented member.
ueEndpointRequest :: Lens' UpdateEndpoint EndpointRequest
ueEndpointRequest = lens _ueEndpointRequest (\ s a -> s{_ueEndpointRequest = a})

instance AWSRequest UpdateEndpoint where
        type Rs UpdateEndpoint = UpdateEndpointResponse
        request = putJSON pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 UpdateEndpointResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable UpdateEndpoint where

instance NFData UpdateEndpoint where

instance ToHeaders UpdateEndpoint where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateEndpoint where
        toJSON UpdateEndpoint'{..}
          = object
              (catMaybes
                 [Just ("EndpointRequest" .= _ueEndpointRequest)])

instance ToPath UpdateEndpoint where
        toPath UpdateEndpoint'{..}
          = mconcat
              ["/v1/apps/", toBS _ueApplicationId, "/endpoints/",
               toBS _ueEndpointId]

instance ToQuery UpdateEndpoint where
        toQuery = const mempty

-- | /See:/ 'updateEndpointResponse' smart constructor.
data UpdateEndpointResponse = UpdateEndpointResponse'
  { _uersResponseStatus :: !Int
  , _uersMessageBody    :: !MessageBody
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uersResponseStatus' - -- | The response status code.
--
-- * 'uersMessageBody' - Undocumented member.
updateEndpointResponse
    :: Int -- ^ 'uersResponseStatus'
    -> MessageBody -- ^ 'uersMessageBody'
    -> UpdateEndpointResponse
updateEndpointResponse pResponseStatus_ pMessageBody_ =
  UpdateEndpointResponse'
    {_uersResponseStatus = pResponseStatus_, _uersMessageBody = pMessageBody_}


-- | -- | The response status code.
uersResponseStatus :: Lens' UpdateEndpointResponse Int
uersResponseStatus = lens _uersResponseStatus (\ s a -> s{_uersResponseStatus = a})

-- | Undocumented member.
uersMessageBody :: Lens' UpdateEndpointResponse MessageBody
uersMessageBody = lens _uersMessageBody (\ s a -> s{_uersMessageBody = a})

instance NFData UpdateEndpointResponse where
