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
-- Module      : Network.AWS.Pinpoint.UpdateAPNSVoipChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an APNS VoIP channel
module Network.AWS.Pinpoint.UpdateAPNSVoipChannel
    (
    -- * Creating a Request
      updateAPNSVoipChannel
    , UpdateAPNSVoipChannel
    -- * Request Lenses
    , uavcApplicationId
    , uavcAPNSVoipChannelRequest

    -- * Destructuring the Response
    , updateAPNSVoipChannelResponse
    , UpdateAPNSVoipChannelResponse
    -- * Response Lenses
    , uavcrsResponseStatus
    , uavcrsAPNSVoipChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAPNSVoipChannel' smart constructor.
data UpdateAPNSVoipChannel = UpdateAPNSVoipChannel'
  { _uavcApplicationId          :: !Text
  , _uavcAPNSVoipChannelRequest :: !APNSVoipChannelRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAPNSVoipChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uavcApplicationId' - Undocumented member.
--
-- * 'uavcAPNSVoipChannelRequest' - Undocumented member.
updateAPNSVoipChannel
    :: Text -- ^ 'uavcApplicationId'
    -> APNSVoipChannelRequest -- ^ 'uavcAPNSVoipChannelRequest'
    -> UpdateAPNSVoipChannel
updateAPNSVoipChannel pApplicationId_ pAPNSVoipChannelRequest_ =
  UpdateAPNSVoipChannel'
    { _uavcApplicationId = pApplicationId_
    , _uavcAPNSVoipChannelRequest = pAPNSVoipChannelRequest_
    }


-- | Undocumented member.
uavcApplicationId :: Lens' UpdateAPNSVoipChannel Text
uavcApplicationId = lens _uavcApplicationId (\ s a -> s{_uavcApplicationId = a})

-- | Undocumented member.
uavcAPNSVoipChannelRequest :: Lens' UpdateAPNSVoipChannel APNSVoipChannelRequest
uavcAPNSVoipChannelRequest = lens _uavcAPNSVoipChannelRequest (\ s a -> s{_uavcAPNSVoipChannelRequest = a})

instance AWSRequest UpdateAPNSVoipChannel where
        type Rs UpdateAPNSVoipChannel =
             UpdateAPNSVoipChannelResponse
        request = putJSON pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 UpdateAPNSVoipChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable UpdateAPNSVoipChannel where

instance NFData UpdateAPNSVoipChannel where

instance ToHeaders UpdateAPNSVoipChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateAPNSVoipChannel where
        toJSON UpdateAPNSVoipChannel'{..}
          = object
              (catMaybes
                 [Just
                    ("APNSVoipChannelRequest" .=
                       _uavcAPNSVoipChannelRequest)])

instance ToPath UpdateAPNSVoipChannel where
        toPath UpdateAPNSVoipChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _uavcApplicationId,
               "/channels/apns_voip"]

instance ToQuery UpdateAPNSVoipChannel where
        toQuery = const mempty

-- | /See:/ 'updateAPNSVoipChannelResponse' smart constructor.
data UpdateAPNSVoipChannelResponse = UpdateAPNSVoipChannelResponse'
  { _uavcrsResponseStatus          :: !Int
  , _uavcrsAPNSVoipChannelResponse :: !APNSVoipChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAPNSVoipChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uavcrsResponseStatus' - -- | The response status code.
--
-- * 'uavcrsAPNSVoipChannelResponse' - Undocumented member.
updateAPNSVoipChannelResponse
    :: Int -- ^ 'uavcrsResponseStatus'
    -> APNSVoipChannelResponse -- ^ 'uavcrsAPNSVoipChannelResponse'
    -> UpdateAPNSVoipChannelResponse
updateAPNSVoipChannelResponse pResponseStatus_ pAPNSVoipChannelResponse_ =
  UpdateAPNSVoipChannelResponse'
    { _uavcrsResponseStatus = pResponseStatus_
    , _uavcrsAPNSVoipChannelResponse = pAPNSVoipChannelResponse_
    }


-- | -- | The response status code.
uavcrsResponseStatus :: Lens' UpdateAPNSVoipChannelResponse Int
uavcrsResponseStatus = lens _uavcrsResponseStatus (\ s a -> s{_uavcrsResponseStatus = a})

-- | Undocumented member.
uavcrsAPNSVoipChannelResponse :: Lens' UpdateAPNSVoipChannelResponse APNSVoipChannelResponse
uavcrsAPNSVoipChannelResponse = lens _uavcrsAPNSVoipChannelResponse (\ s a -> s{_uavcrsAPNSVoipChannelResponse = a})

instance NFData UpdateAPNSVoipChannelResponse where
