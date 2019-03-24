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
-- Module      : Network.AWS.Pinpoint.UpdateVoiceChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an Voice channel
module Network.AWS.Pinpoint.UpdateVoiceChannel
    (
    -- * Creating a Request
      updateVoiceChannel
    , UpdateVoiceChannel
    -- * Request Lenses
    , uvcApplicationId
    , uvcVoiceChannelRequest

    -- * Destructuring the Response
    , updateVoiceChannelResponse
    , UpdateVoiceChannelResponse
    -- * Response Lenses
    , uvcrsResponseStatus
    , uvcrsVoiceChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateVoiceChannel' smart constructor.
data UpdateVoiceChannel = UpdateVoiceChannel'
  { _uvcApplicationId       :: !Text
  , _uvcVoiceChannelRequest :: !VoiceChannelRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateVoiceChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uvcApplicationId' - The unique ID of your Amazon Pinpoint application.
--
-- * 'uvcVoiceChannelRequest' - Undocumented member.
updateVoiceChannel
    :: Text -- ^ 'uvcApplicationId'
    -> VoiceChannelRequest -- ^ 'uvcVoiceChannelRequest'
    -> UpdateVoiceChannel
updateVoiceChannel pApplicationId_ pVoiceChannelRequest_ =
  UpdateVoiceChannel'
    { _uvcApplicationId = pApplicationId_
    , _uvcVoiceChannelRequest = pVoiceChannelRequest_
    }


-- | The unique ID of your Amazon Pinpoint application.
uvcApplicationId :: Lens' UpdateVoiceChannel Text
uvcApplicationId = lens _uvcApplicationId (\ s a -> s{_uvcApplicationId = a})

-- | Undocumented member.
uvcVoiceChannelRequest :: Lens' UpdateVoiceChannel VoiceChannelRequest
uvcVoiceChannelRequest = lens _uvcVoiceChannelRequest (\ s a -> s{_uvcVoiceChannelRequest = a})

instance AWSRequest UpdateVoiceChannel where
        type Rs UpdateVoiceChannel =
             UpdateVoiceChannelResponse
        request = putJSON pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 UpdateVoiceChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable UpdateVoiceChannel where

instance NFData UpdateVoiceChannel where

instance ToHeaders UpdateVoiceChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateVoiceChannel where
        toJSON UpdateVoiceChannel'{..}
          = object
              (catMaybes
                 [Just
                    ("VoiceChannelRequest" .= _uvcVoiceChannelRequest)])

instance ToPath UpdateVoiceChannel where
        toPath UpdateVoiceChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _uvcApplicationId,
               "/channels/voice"]

instance ToQuery UpdateVoiceChannel where
        toQuery = const mempty

-- | /See:/ 'updateVoiceChannelResponse' smart constructor.
data UpdateVoiceChannelResponse = UpdateVoiceChannelResponse'
  { _uvcrsResponseStatus       :: !Int
  , _uvcrsVoiceChannelResponse :: !VoiceChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateVoiceChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uvcrsResponseStatus' - -- | The response status code.
--
-- * 'uvcrsVoiceChannelResponse' - Undocumented member.
updateVoiceChannelResponse
    :: Int -- ^ 'uvcrsResponseStatus'
    -> VoiceChannelResponse -- ^ 'uvcrsVoiceChannelResponse'
    -> UpdateVoiceChannelResponse
updateVoiceChannelResponse pResponseStatus_ pVoiceChannelResponse_ =
  UpdateVoiceChannelResponse'
    { _uvcrsResponseStatus = pResponseStatus_
    , _uvcrsVoiceChannelResponse = pVoiceChannelResponse_
    }


-- | -- | The response status code.
uvcrsResponseStatus :: Lens' UpdateVoiceChannelResponse Int
uvcrsResponseStatus = lens _uvcrsResponseStatus (\ s a -> s{_uvcrsResponseStatus = a})

-- | Undocumented member.
uvcrsVoiceChannelResponse :: Lens' UpdateVoiceChannelResponse VoiceChannelResponse
uvcrsVoiceChannelResponse = lens _uvcrsVoiceChannelResponse (\ s a -> s{_uvcrsVoiceChannelResponse = a})

instance NFData UpdateVoiceChannelResponse where
