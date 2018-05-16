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
-- Module      : Network.AWS.Pinpoint.GetBaiduChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a BAIDU GCM channel
module Network.AWS.Pinpoint.GetBaiduChannel
    (
    -- * Creating a Request
      getBaiduChannel
    , GetBaiduChannel
    -- * Request Lenses
    , gbcApplicationId

    -- * Destructuring the Response
    , getBaiduChannelResponse
    , GetBaiduChannelResponse
    -- * Response Lenses
    , gbcrsResponseStatus
    , gbcrsBaiduChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBaiduChannel' smart constructor.
newtype GetBaiduChannel = GetBaiduChannel'
  { _gbcApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBaiduChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbcApplicationId' - Undocumented member.
getBaiduChannel
    :: Text -- ^ 'gbcApplicationId'
    -> GetBaiduChannel
getBaiduChannel pApplicationId_ =
  GetBaiduChannel' {_gbcApplicationId = pApplicationId_}


-- | Undocumented member.
gbcApplicationId :: Lens' GetBaiduChannel Text
gbcApplicationId = lens _gbcApplicationId (\ s a -> s{_gbcApplicationId = a})

instance AWSRequest GetBaiduChannel where
        type Rs GetBaiduChannel = GetBaiduChannelResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetBaiduChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetBaiduChannel where

instance NFData GetBaiduChannel where

instance ToHeaders GetBaiduChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetBaiduChannel where
        toPath GetBaiduChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _gbcApplicationId,
               "/channels/baidu"]

instance ToQuery GetBaiduChannel where
        toQuery = const mempty

-- | /See:/ 'getBaiduChannelResponse' smart constructor.
data GetBaiduChannelResponse = GetBaiduChannelResponse'
  { _gbcrsResponseStatus       :: !Int
  , _gbcrsBaiduChannelResponse :: !BaiduChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBaiduChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbcrsResponseStatus' - -- | The response status code.
--
-- * 'gbcrsBaiduChannelResponse' - Undocumented member.
getBaiduChannelResponse
    :: Int -- ^ 'gbcrsResponseStatus'
    -> BaiduChannelResponse -- ^ 'gbcrsBaiduChannelResponse'
    -> GetBaiduChannelResponse
getBaiduChannelResponse pResponseStatus_ pBaiduChannelResponse_ =
  GetBaiduChannelResponse'
    { _gbcrsResponseStatus = pResponseStatus_
    , _gbcrsBaiduChannelResponse = pBaiduChannelResponse_
    }


-- | -- | The response status code.
gbcrsResponseStatus :: Lens' GetBaiduChannelResponse Int
gbcrsResponseStatus = lens _gbcrsResponseStatus (\ s a -> s{_gbcrsResponseStatus = a})

-- | Undocumented member.
gbcrsBaiduChannelResponse :: Lens' GetBaiduChannelResponse BaiduChannelResponse
gbcrsBaiduChannelResponse = lens _gbcrsBaiduChannelResponse (\ s a -> s{_gbcrsBaiduChannelResponse = a})

instance NFData GetBaiduChannelResponse where
