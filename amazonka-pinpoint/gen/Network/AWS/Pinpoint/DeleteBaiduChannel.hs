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
-- Module      : Network.AWS.Pinpoint.DeleteBaiduChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a BAIDU GCM channel
module Network.AWS.Pinpoint.DeleteBaiduChannel
    (
    -- * Creating a Request
      deleteBaiduChannel
    , DeleteBaiduChannel
    -- * Request Lenses
    , dbcApplicationId

    -- * Destructuring the Response
    , deleteBaiduChannelResponse
    , DeleteBaiduChannelResponse
    -- * Response Lenses
    , dbcrsResponseStatus
    , dbcrsBaiduChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteBaiduChannel' smart constructor.
newtype DeleteBaiduChannel = DeleteBaiduChannel'
  { _dbcApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBaiduChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbcApplicationId' - Undocumented member.
deleteBaiduChannel
    :: Text -- ^ 'dbcApplicationId'
    -> DeleteBaiduChannel
deleteBaiduChannel pApplicationId_ =
  DeleteBaiduChannel' {_dbcApplicationId = pApplicationId_}


-- | Undocumented member.
dbcApplicationId :: Lens' DeleteBaiduChannel Text
dbcApplicationId = lens _dbcApplicationId (\ s a -> s{_dbcApplicationId = a})

instance AWSRequest DeleteBaiduChannel where
        type Rs DeleteBaiduChannel =
             DeleteBaiduChannelResponse
        request = delete pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 DeleteBaiduChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable DeleteBaiduChannel where

instance NFData DeleteBaiduChannel where

instance ToHeaders DeleteBaiduChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteBaiduChannel where
        toPath DeleteBaiduChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _dbcApplicationId,
               "/channels/baidu"]

instance ToQuery DeleteBaiduChannel where
        toQuery = const mempty

-- | /See:/ 'deleteBaiduChannelResponse' smart constructor.
data DeleteBaiduChannelResponse = DeleteBaiduChannelResponse'
  { _dbcrsResponseStatus       :: !Int
  , _dbcrsBaiduChannelResponse :: !BaiduChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBaiduChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbcrsResponseStatus' - -- | The response status code.
--
-- * 'dbcrsBaiduChannelResponse' - Undocumented member.
deleteBaiduChannelResponse
    :: Int -- ^ 'dbcrsResponseStatus'
    -> BaiduChannelResponse -- ^ 'dbcrsBaiduChannelResponse'
    -> DeleteBaiduChannelResponse
deleteBaiduChannelResponse pResponseStatus_ pBaiduChannelResponse_ =
  DeleteBaiduChannelResponse'
    { _dbcrsResponseStatus = pResponseStatus_
    , _dbcrsBaiduChannelResponse = pBaiduChannelResponse_
    }


-- | -- | The response status code.
dbcrsResponseStatus :: Lens' DeleteBaiduChannelResponse Int
dbcrsResponseStatus = lens _dbcrsResponseStatus (\ s a -> s{_dbcrsResponseStatus = a})

-- | Undocumented member.
dbcrsBaiduChannelResponse :: Lens' DeleteBaiduChannelResponse BaiduChannelResponse
dbcrsBaiduChannelResponse = lens _dbcrsBaiduChannelResponse (\ s a -> s{_dbcrsBaiduChannelResponse = a})

instance NFData DeleteBaiduChannelResponse where
