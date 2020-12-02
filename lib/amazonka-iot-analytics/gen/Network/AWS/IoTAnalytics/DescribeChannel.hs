{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.DescribeChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a channel.
module Network.AWS.IoTAnalytics.DescribeChannel
  ( -- * Creating a Request
    describeChannel,
    DescribeChannel,

    -- * Request Lenses
    desIncludeStatistics,
    desChannelName,

    -- * Destructuring the Response
    describeChannelResponse,
    DescribeChannelResponse,

    -- * Response Lenses
    dcrsChannel,
    dcrsStatistics,
    dcrsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeChannel' smart constructor.
data DescribeChannel = DescribeChannel'
  { _desIncludeStatistics ::
      !(Maybe Bool),
    _desChannelName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desIncludeStatistics' - If true, additional statistical information about the channel is included in the response. This feature cannot be used with a channel whose S3 storage is customer-managed.
--
-- * 'desChannelName' - The name of the channel whose information is retrieved.
describeChannel ::
  -- | 'desChannelName'
  Text ->
  DescribeChannel
describeChannel pChannelName_ =
  DescribeChannel'
    { _desIncludeStatistics = Nothing,
      _desChannelName = pChannelName_
    }

-- | If true, additional statistical information about the channel is included in the response. This feature cannot be used with a channel whose S3 storage is customer-managed.
desIncludeStatistics :: Lens' DescribeChannel (Maybe Bool)
desIncludeStatistics = lens _desIncludeStatistics (\s a -> s {_desIncludeStatistics = a})

-- | The name of the channel whose information is retrieved.
desChannelName :: Lens' DescribeChannel Text
desChannelName = lens _desChannelName (\s a -> s {_desChannelName = a})

instance AWSRequest DescribeChannel where
  type Rs DescribeChannel = DescribeChannelResponse
  request = get ioTAnalytics
  response =
    receiveJSON
      ( \s h x ->
          DescribeChannelResponse'
            <$> (x .?> "channel") <*> (x .?> "statistics") <*> (pure (fromEnum s))
      )

instance Hashable DescribeChannel

instance NFData DescribeChannel

instance ToHeaders DescribeChannel where
  toHeaders = const mempty

instance ToPath DescribeChannel where
  toPath DescribeChannel' {..} =
    mconcat ["/channels/", toBS _desChannelName]

instance ToQuery DescribeChannel where
  toQuery DescribeChannel' {..} =
    mconcat ["includeStatistics" =: _desIncludeStatistics]

-- | /See:/ 'describeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
  { _dcrsChannel ::
      !(Maybe Channel),
    _dcrsStatistics ::
      !(Maybe ChannelStatistics),
    _dcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsChannel' - An object that contains information about the channel.
--
-- * 'dcrsStatistics' - Statistics about the channel. Included if the @includeStatistics@ parameter is set to @true@ in the request.
--
-- * 'dcrsResponseStatus' - -- | The response status code.
describeChannelResponse ::
  -- | 'dcrsResponseStatus'
  Int ->
  DescribeChannelResponse
describeChannelResponse pResponseStatus_ =
  DescribeChannelResponse'
    { _dcrsChannel = Nothing,
      _dcrsStatistics = Nothing,
      _dcrsResponseStatus = pResponseStatus_
    }

-- | An object that contains information about the channel.
dcrsChannel :: Lens' DescribeChannelResponse (Maybe Channel)
dcrsChannel = lens _dcrsChannel (\s a -> s {_dcrsChannel = a})

-- | Statistics about the channel. Included if the @includeStatistics@ parameter is set to @true@ in the request.
dcrsStatistics :: Lens' DescribeChannelResponse (Maybe ChannelStatistics)
dcrsStatistics = lens _dcrsStatistics (\s a -> s {_dcrsStatistics = a})

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DescribeChannelResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\s a -> s {_dcrsResponseStatus = a})

instance NFData DescribeChannelResponse
