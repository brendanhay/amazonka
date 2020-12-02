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
-- Module      : Network.AWS.KinesisVideo.DescribeSignalingChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the most current information about the signaling channel. You must specify either the name or the Amazon Resource Name (ARN) of the channel that you want to describe.
module Network.AWS.KinesisVideo.DescribeSignalingChannel
  ( -- * Creating a Request
    describeSignalingChannel,
    DescribeSignalingChannel,

    -- * Request Lenses
    dChannelARN,
    dChannelName,

    -- * Destructuring the Response
    describeSignalingChannelResponse,
    DescribeSignalingChannelResponse,

    -- * Response Lenses
    desrsChannelInfo,
    desrsResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeSignalingChannel' smart constructor.
data DescribeSignalingChannel = DescribeSignalingChannel'
  { _dChannelARN ::
      !(Maybe Text),
    _dChannelName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeSignalingChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dChannelARN' - The ARN of the signaling channel that you want to describe.
--
-- * 'dChannelName' - The name of the signaling channel that you want to describe.
describeSignalingChannel ::
  DescribeSignalingChannel
describeSignalingChannel =
  DescribeSignalingChannel'
    { _dChannelARN = Nothing,
      _dChannelName = Nothing
    }

-- | The ARN of the signaling channel that you want to describe.
dChannelARN :: Lens' DescribeSignalingChannel (Maybe Text)
dChannelARN = lens _dChannelARN (\s a -> s {_dChannelARN = a})

-- | The name of the signaling channel that you want to describe.
dChannelName :: Lens' DescribeSignalingChannel (Maybe Text)
dChannelName = lens _dChannelName (\s a -> s {_dChannelName = a})

instance AWSRequest DescribeSignalingChannel where
  type Rs DescribeSignalingChannel = DescribeSignalingChannelResponse
  request = postJSON kinesisVideo
  response =
    receiveJSON
      ( \s h x ->
          DescribeSignalingChannelResponse'
            <$> (x .?> "ChannelInfo") <*> (pure (fromEnum s))
      )

instance Hashable DescribeSignalingChannel

instance NFData DescribeSignalingChannel

instance ToHeaders DescribeSignalingChannel where
  toHeaders = const mempty

instance ToJSON DescribeSignalingChannel where
  toJSON DescribeSignalingChannel' {..} =
    object
      ( catMaybes
          [ ("ChannelARN" .=) <$> _dChannelARN,
            ("ChannelName" .=) <$> _dChannelName
          ]
      )

instance ToPath DescribeSignalingChannel where
  toPath = const "/describeSignalingChannel"

instance ToQuery DescribeSignalingChannel where
  toQuery = const mempty

-- | /See:/ 'describeSignalingChannelResponse' smart constructor.
data DescribeSignalingChannelResponse = DescribeSignalingChannelResponse'
  { _desrsChannelInfo ::
      !(Maybe ChannelInfo),
    _desrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeSignalingChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsChannelInfo' - A structure that encapsulates the specified signaling channel's metadata and properties.
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeSignalingChannelResponse ::
  -- | 'desrsResponseStatus'
  Int ->
  DescribeSignalingChannelResponse
describeSignalingChannelResponse pResponseStatus_ =
  DescribeSignalingChannelResponse'
    { _desrsChannelInfo = Nothing,
      _desrsResponseStatus = pResponseStatus_
    }

-- | A structure that encapsulates the specified signaling channel's metadata and properties.
desrsChannelInfo :: Lens' DescribeSignalingChannelResponse (Maybe ChannelInfo)
desrsChannelInfo = lens _desrsChannelInfo (\s a -> s {_desrsChannelInfo = a})

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeSignalingChannelResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\s a -> s {_desrsResponseStatus = a})

instance NFData DescribeSignalingChannelResponse
