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
-- Module      : Network.AWS.KinesisVideo.ListSignalingChannels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @ChannelInfo@ objects. Each object describes a signaling channel. To retrieve only those channels that satisfy a specific condition, you can specify a @ChannelNameCondition@ .
--
--
--
-- This operation returns paginated results.
module Network.AWS.KinesisVideo.ListSignalingChannels
  ( -- * Creating a Request
    listSignalingChannels,
    ListSignalingChannels,

    -- * Request Lenses
    lscChannelNameCondition,
    lscNextToken,
    lscMaxResults,

    -- * Destructuring the Response
    listSignalingChannelsResponse,
    ListSignalingChannelsResponse,

    -- * Response Lenses
    lscrsChannelInfoList,
    lscrsNextToken,
    lscrsResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSignalingChannels' smart constructor.
data ListSignalingChannels = ListSignalingChannels'
  { _lscChannelNameCondition ::
      !(Maybe ChannelNameCondition),
    _lscNextToken :: !(Maybe Text),
    _lscMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSignalingChannels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lscChannelNameCondition' - Optional: Returns only the channels that satisfy a specific condition.
--
-- * 'lscNextToken' - If you specify this parameter, when the result of a @ListSignalingChannels@ operation is truncated, the call returns the @NextToken@ in the response. To get another batch of channels, provide this token in your next request.
--
-- * 'lscMaxResults' - The maximum number of channels to return in the response. The default is 500.
listSignalingChannels ::
  ListSignalingChannels
listSignalingChannels =
  ListSignalingChannels'
    { _lscChannelNameCondition = Nothing,
      _lscNextToken = Nothing,
      _lscMaxResults = Nothing
    }

-- | Optional: Returns only the channels that satisfy a specific condition.
lscChannelNameCondition :: Lens' ListSignalingChannels (Maybe ChannelNameCondition)
lscChannelNameCondition = lens _lscChannelNameCondition (\s a -> s {_lscChannelNameCondition = a})

-- | If you specify this parameter, when the result of a @ListSignalingChannels@ operation is truncated, the call returns the @NextToken@ in the response. To get another batch of channels, provide this token in your next request.
lscNextToken :: Lens' ListSignalingChannels (Maybe Text)
lscNextToken = lens _lscNextToken (\s a -> s {_lscNextToken = a})

-- | The maximum number of channels to return in the response. The default is 500.
lscMaxResults :: Lens' ListSignalingChannels (Maybe Natural)
lscMaxResults = lens _lscMaxResults (\s a -> s {_lscMaxResults = a}) . mapping _Nat

instance AWSPager ListSignalingChannels where
  page rq rs
    | stop (rs ^. lscrsNextToken) = Nothing
    | stop (rs ^. lscrsChannelInfoList) = Nothing
    | otherwise = Just $ rq & lscNextToken .~ rs ^. lscrsNextToken

instance AWSRequest ListSignalingChannels where
  type Rs ListSignalingChannels = ListSignalingChannelsResponse
  request = postJSON kinesisVideo
  response =
    receiveJSON
      ( \s h x ->
          ListSignalingChannelsResponse'
            <$> (x .?> "ChannelInfoList" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListSignalingChannels

instance NFData ListSignalingChannels

instance ToHeaders ListSignalingChannels where
  toHeaders = const mempty

instance ToJSON ListSignalingChannels where
  toJSON ListSignalingChannels' {..} =
    object
      ( catMaybes
          [ ("ChannelNameCondition" .=) <$> _lscChannelNameCondition,
            ("NextToken" .=) <$> _lscNextToken,
            ("MaxResults" .=) <$> _lscMaxResults
          ]
      )

instance ToPath ListSignalingChannels where
  toPath = const "/listSignalingChannels"

instance ToQuery ListSignalingChannels where
  toQuery = const mempty

-- | /See:/ 'listSignalingChannelsResponse' smart constructor.
data ListSignalingChannelsResponse = ListSignalingChannelsResponse'
  { _lscrsChannelInfoList ::
      !(Maybe [ChannelInfo]),
    _lscrsNextToken ::
      !(Maybe Text),
    _lscrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSignalingChannelsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lscrsChannelInfoList' - An array of @ChannelInfo@ objects.
--
-- * 'lscrsNextToken' - If the response is truncated, the call returns this element with a token. To get the next batch of streams, use this token in your next request.
--
-- * 'lscrsResponseStatus' - -- | The response status code.
listSignalingChannelsResponse ::
  -- | 'lscrsResponseStatus'
  Int ->
  ListSignalingChannelsResponse
listSignalingChannelsResponse pResponseStatus_ =
  ListSignalingChannelsResponse'
    { _lscrsChannelInfoList = Nothing,
      _lscrsNextToken = Nothing,
      _lscrsResponseStatus = pResponseStatus_
    }

-- | An array of @ChannelInfo@ objects.
lscrsChannelInfoList :: Lens' ListSignalingChannelsResponse [ChannelInfo]
lscrsChannelInfoList = lens _lscrsChannelInfoList (\s a -> s {_lscrsChannelInfoList = a}) . _Default . _Coerce

-- | If the response is truncated, the call returns this element with a token. To get the next batch of streams, use this token in your next request.
lscrsNextToken :: Lens' ListSignalingChannelsResponse (Maybe Text)
lscrsNextToken = lens _lscrsNextToken (\s a -> s {_lscrsNextToken = a})

-- | -- | The response status code.
lscrsResponseStatus :: Lens' ListSignalingChannelsResponse Int
lscrsResponseStatus = lens _lscrsResponseStatus (\s a -> s {_lscrsResponseStatus = a})

instance NFData ListSignalingChannelsResponse
