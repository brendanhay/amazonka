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
-- Module      : Network.AWS.Pinpoint.UpdateAPNSVoipSandboxChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an APNS VoIP sandbox channel
module Network.AWS.Pinpoint.UpdateAPNSVoipSandboxChannel
    (
    -- * Creating a Request
      updateAPNSVoipSandboxChannel
    , UpdateAPNSVoipSandboxChannel
    -- * Request Lenses
    , uavscApplicationId
    , uavscAPNSVoipSandboxChannelRequest

    -- * Destructuring the Response
    , updateAPNSVoipSandboxChannelResponse
    , UpdateAPNSVoipSandboxChannelResponse
    -- * Response Lenses
    , uavscrsResponseStatus
    , uavscrsAPNSVoipSandboxChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAPNSVoipSandboxChannel' smart constructor.
data UpdateAPNSVoipSandboxChannel = UpdateAPNSVoipSandboxChannel'
  { _uavscApplicationId                 :: !Text
  , _uavscAPNSVoipSandboxChannelRequest :: !APNSVoipSandboxChannelRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAPNSVoipSandboxChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uavscApplicationId' - Undocumented member.
--
-- * 'uavscAPNSVoipSandboxChannelRequest' - Undocumented member.
updateAPNSVoipSandboxChannel
    :: Text -- ^ 'uavscApplicationId'
    -> APNSVoipSandboxChannelRequest -- ^ 'uavscAPNSVoipSandboxChannelRequest'
    -> UpdateAPNSVoipSandboxChannel
updateAPNSVoipSandboxChannel pApplicationId_ pAPNSVoipSandboxChannelRequest_ =
  UpdateAPNSVoipSandboxChannel'
    { _uavscApplicationId = pApplicationId_
    , _uavscAPNSVoipSandboxChannelRequest = pAPNSVoipSandboxChannelRequest_
    }


-- | Undocumented member.
uavscApplicationId :: Lens' UpdateAPNSVoipSandboxChannel Text
uavscApplicationId = lens _uavscApplicationId (\ s a -> s{_uavscApplicationId = a})

-- | Undocumented member.
uavscAPNSVoipSandboxChannelRequest :: Lens' UpdateAPNSVoipSandboxChannel APNSVoipSandboxChannelRequest
uavscAPNSVoipSandboxChannelRequest = lens _uavscAPNSVoipSandboxChannelRequest (\ s a -> s{_uavscAPNSVoipSandboxChannelRequest = a})

instance AWSRequest UpdateAPNSVoipSandboxChannel
         where
        type Rs UpdateAPNSVoipSandboxChannel =
             UpdateAPNSVoipSandboxChannelResponse
        request = putJSON pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 UpdateAPNSVoipSandboxChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable UpdateAPNSVoipSandboxChannel where

instance NFData UpdateAPNSVoipSandboxChannel where

instance ToHeaders UpdateAPNSVoipSandboxChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateAPNSVoipSandboxChannel where
        toJSON UpdateAPNSVoipSandboxChannel'{..}
          = object
              (catMaybes
                 [Just
                    ("APNSVoipSandboxChannelRequest" .=
                       _uavscAPNSVoipSandboxChannelRequest)])

instance ToPath UpdateAPNSVoipSandboxChannel where
        toPath UpdateAPNSVoipSandboxChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _uavscApplicationId,
               "/channels/apns_voip_sandbox"]

instance ToQuery UpdateAPNSVoipSandboxChannel where
        toQuery = const mempty

-- | /See:/ 'updateAPNSVoipSandboxChannelResponse' smart constructor.
data UpdateAPNSVoipSandboxChannelResponse = UpdateAPNSVoipSandboxChannelResponse'
  { _uavscrsResponseStatus                 :: !Int
  , _uavscrsAPNSVoipSandboxChannelResponse :: !APNSVoipSandboxChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAPNSVoipSandboxChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uavscrsResponseStatus' - -- | The response status code.
--
-- * 'uavscrsAPNSVoipSandboxChannelResponse' - Undocumented member.
updateAPNSVoipSandboxChannelResponse
    :: Int -- ^ 'uavscrsResponseStatus'
    -> APNSVoipSandboxChannelResponse -- ^ 'uavscrsAPNSVoipSandboxChannelResponse'
    -> UpdateAPNSVoipSandboxChannelResponse
updateAPNSVoipSandboxChannelResponse pResponseStatus_ pAPNSVoipSandboxChannelResponse_ =
  UpdateAPNSVoipSandboxChannelResponse'
    { _uavscrsResponseStatus = pResponseStatus_
    , _uavscrsAPNSVoipSandboxChannelResponse = pAPNSVoipSandboxChannelResponse_
    }


-- | -- | The response status code.
uavscrsResponseStatus :: Lens' UpdateAPNSVoipSandboxChannelResponse Int
uavscrsResponseStatus = lens _uavscrsResponseStatus (\ s a -> s{_uavscrsResponseStatus = a})

-- | Undocumented member.
uavscrsAPNSVoipSandboxChannelResponse :: Lens' UpdateAPNSVoipSandboxChannelResponse APNSVoipSandboxChannelResponse
uavscrsAPNSVoipSandboxChannelResponse = lens _uavscrsAPNSVoipSandboxChannelResponse (\ s a -> s{_uavscrsAPNSVoipSandboxChannelResponse = a})

instance NFData UpdateAPNSVoipSandboxChannelResponse
         where
