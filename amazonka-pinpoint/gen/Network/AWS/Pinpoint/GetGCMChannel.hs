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
-- Module      : Network.AWS.Pinpoint.GetGCMChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the GCM channel for an app.
module Network.AWS.Pinpoint.GetGCMChannel
    (
    -- * Creating a Request
      getGCMChannel
    , GetGCMChannel
    -- * Request Lenses
    , ggcApplicationId

    -- * Destructuring the Response
    , getGCMChannelResponse
    , GetGCMChannelResponse
    -- * Response Lenses
    , ggcrsResponseStatus
    , ggcrsGCMChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getGCMChannel' smart constructor.
newtype GetGCMChannel = GetGCMChannel'
  { _ggcApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGCMChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggcApplicationId' - Undocumented member.
getGCMChannel
    :: Text -- ^ 'ggcApplicationId'
    -> GetGCMChannel
getGCMChannel pApplicationId_ =
  GetGCMChannel' {_ggcApplicationId = pApplicationId_}


-- | Undocumented member.
ggcApplicationId :: Lens' GetGCMChannel Text
ggcApplicationId = lens _ggcApplicationId (\ s a -> s{_ggcApplicationId = a})

instance AWSRequest GetGCMChannel where
        type Rs GetGCMChannel = GetGCMChannelResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetGCMChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetGCMChannel where

instance NFData GetGCMChannel where

instance ToHeaders GetGCMChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetGCMChannel where
        toPath GetGCMChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _ggcApplicationId,
               "/channels/gcm"]

instance ToQuery GetGCMChannel where
        toQuery = const mempty

-- | /See:/ 'getGCMChannelResponse' smart constructor.
data GetGCMChannelResponse = GetGCMChannelResponse'
  { _ggcrsResponseStatus     :: !Int
  , _ggcrsGCMChannelResponse :: !GCMChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGCMChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggcrsResponseStatus' - -- | The response status code.
--
-- * 'ggcrsGCMChannelResponse' - Undocumented member.
getGCMChannelResponse
    :: Int -- ^ 'ggcrsResponseStatus'
    -> GCMChannelResponse -- ^ 'ggcrsGCMChannelResponse'
    -> GetGCMChannelResponse
getGCMChannelResponse pResponseStatus_ pGCMChannelResponse_ =
  GetGCMChannelResponse'
    { _ggcrsResponseStatus = pResponseStatus_
    , _ggcrsGCMChannelResponse = pGCMChannelResponse_
    }


-- | -- | The response status code.
ggcrsResponseStatus :: Lens' GetGCMChannelResponse Int
ggcrsResponseStatus = lens _ggcrsResponseStatus (\ s a -> s{_ggcrsResponseStatus = a})

-- | Undocumented member.
ggcrsGCMChannelResponse :: Lens' GetGCMChannelResponse GCMChannelResponse
ggcrsGCMChannelResponse = lens _ggcrsGCMChannelResponse (\ s a -> s{_ggcrsGCMChannelResponse = a})

instance NFData GetGCMChannelResponse where
