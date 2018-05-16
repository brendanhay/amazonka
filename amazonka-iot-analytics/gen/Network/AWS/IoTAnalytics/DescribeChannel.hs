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
-- Module      : Network.AWS.IoTAnalytics.DescribeChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a channel.
--
--
module Network.AWS.IoTAnalytics.DescribeChannel
    (
    -- * Creating a Request
      describeChannel
    , DescribeChannel
    -- * Request Lenses
    , dChannelName

    -- * Destructuring the Response
    , describeChannelResponse
    , DescribeChannelResponse
    -- * Response Lenses
    , dcrsChannel
    , dcrsResponseStatus
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeChannel' smart constructor.
newtype DescribeChannel = DescribeChannel'
  { _dChannelName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dChannelName' - The name of the channel whose information is retrieved.
describeChannel
    :: Text -- ^ 'dChannelName'
    -> DescribeChannel
describeChannel pChannelName_ = DescribeChannel' {_dChannelName = pChannelName_}


-- | The name of the channel whose information is retrieved.
dChannelName :: Lens' DescribeChannel Text
dChannelName = lens _dChannelName (\ s a -> s{_dChannelName = a})

instance AWSRequest DescribeChannel where
        type Rs DescribeChannel = DescribeChannelResponse
        request = get ioTAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 DescribeChannelResponse' <$>
                   (x .?> "channel") <*> (pure (fromEnum s)))

instance Hashable DescribeChannel where

instance NFData DescribeChannel where

instance ToHeaders DescribeChannel where
        toHeaders = const mempty

instance ToPath DescribeChannel where
        toPath DescribeChannel'{..}
          = mconcat ["/channels/", toBS _dChannelName]

instance ToQuery DescribeChannel where
        toQuery = const mempty

-- | /See:/ 'describeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
  { _dcrsChannel        :: !(Maybe Channel)
  , _dcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsChannel' - An object that contains information about the channel.
--
-- * 'dcrsResponseStatus' - -- | The response status code.
describeChannelResponse
    :: Int -- ^ 'dcrsResponseStatus'
    -> DescribeChannelResponse
describeChannelResponse pResponseStatus_ =
  DescribeChannelResponse'
    {_dcrsChannel = Nothing, _dcrsResponseStatus = pResponseStatus_}


-- | An object that contains information about the channel.
dcrsChannel :: Lens' DescribeChannelResponse (Maybe Channel)
dcrsChannel = lens _dcrsChannel (\ s a -> s{_dcrsChannel = a})

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DescribeChannelResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\ s a -> s{_dcrsResponseStatus = a})

instance NFData DescribeChannelResponse where
