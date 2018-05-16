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
-- Module      : Network.AWS.Pinpoint.UpdateBaiduChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a BAIDU GCM channel
module Network.AWS.Pinpoint.UpdateBaiduChannel
    (
    -- * Creating a Request
      updateBaiduChannel
    , UpdateBaiduChannel
    -- * Request Lenses
    , ubcApplicationId
    , ubcBaiduChannelRequest

    -- * Destructuring the Response
    , updateBaiduChannelResponse
    , UpdateBaiduChannelResponse
    -- * Response Lenses
    , ubcrsResponseStatus
    , ubcrsBaiduChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateBaiduChannel' smart constructor.
data UpdateBaiduChannel = UpdateBaiduChannel'
  { _ubcApplicationId       :: !Text
  , _ubcBaiduChannelRequest :: !BaiduChannelRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateBaiduChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubcApplicationId' - Undocumented member.
--
-- * 'ubcBaiduChannelRequest' - Undocumented member.
updateBaiduChannel
    :: Text -- ^ 'ubcApplicationId'
    -> BaiduChannelRequest -- ^ 'ubcBaiduChannelRequest'
    -> UpdateBaiduChannel
updateBaiduChannel pApplicationId_ pBaiduChannelRequest_ =
  UpdateBaiduChannel'
    { _ubcApplicationId = pApplicationId_
    , _ubcBaiduChannelRequest = pBaiduChannelRequest_
    }


-- | Undocumented member.
ubcApplicationId :: Lens' UpdateBaiduChannel Text
ubcApplicationId = lens _ubcApplicationId (\ s a -> s{_ubcApplicationId = a})

-- | Undocumented member.
ubcBaiduChannelRequest :: Lens' UpdateBaiduChannel BaiduChannelRequest
ubcBaiduChannelRequest = lens _ubcBaiduChannelRequest (\ s a -> s{_ubcBaiduChannelRequest = a})

instance AWSRequest UpdateBaiduChannel where
        type Rs UpdateBaiduChannel =
             UpdateBaiduChannelResponse
        request = putJSON pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 UpdateBaiduChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable UpdateBaiduChannel where

instance NFData UpdateBaiduChannel where

instance ToHeaders UpdateBaiduChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateBaiduChannel where
        toJSON UpdateBaiduChannel'{..}
          = object
              (catMaybes
                 [Just
                    ("BaiduChannelRequest" .= _ubcBaiduChannelRequest)])

instance ToPath UpdateBaiduChannel where
        toPath UpdateBaiduChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _ubcApplicationId,
               "/channels/baidu"]

instance ToQuery UpdateBaiduChannel where
        toQuery = const mempty

-- | /See:/ 'updateBaiduChannelResponse' smart constructor.
data UpdateBaiduChannelResponse = UpdateBaiduChannelResponse'
  { _ubcrsResponseStatus       :: !Int
  , _ubcrsBaiduChannelResponse :: !BaiduChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateBaiduChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubcrsResponseStatus' - -- | The response status code.
--
-- * 'ubcrsBaiduChannelResponse' - Undocumented member.
updateBaiduChannelResponse
    :: Int -- ^ 'ubcrsResponseStatus'
    -> BaiduChannelResponse -- ^ 'ubcrsBaiduChannelResponse'
    -> UpdateBaiduChannelResponse
updateBaiduChannelResponse pResponseStatus_ pBaiduChannelResponse_ =
  UpdateBaiduChannelResponse'
    { _ubcrsResponseStatus = pResponseStatus_
    , _ubcrsBaiduChannelResponse = pBaiduChannelResponse_
    }


-- | -- | The response status code.
ubcrsResponseStatus :: Lens' UpdateBaiduChannelResponse Int
ubcrsResponseStatus = lens _ubcrsResponseStatus (\ s a -> s{_ubcrsResponseStatus = a})

-- | Undocumented member.
ubcrsBaiduChannelResponse :: Lens' UpdateBaiduChannelResponse BaiduChannelResponse
ubcrsBaiduChannelResponse = lens _ubcrsBaiduChannelResponse (\ s a -> s{_ubcrsBaiduChannelResponse = a})

instance NFData UpdateBaiduChannelResponse where
