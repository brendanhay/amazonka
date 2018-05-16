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
-- Module      : Network.AWS.IoTAnalytics.ListChannels
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of channels.
--
--
module Network.AWS.IoTAnalytics.ListChannels
    (
    -- * Creating a Request
      listChannels
    , ListChannels
    -- * Request Lenses
    , lcNextToken
    , lcMaxResults

    -- * Destructuring the Response
    , listChannelsResponse
    , ListChannelsResponse
    -- * Response Lenses
    , lcrsChannelSummaries
    , lcrsNextToken
    , lcrsResponseStatus
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listChannels' smart constructor.
data ListChannels = ListChannels'
  { _lcNextToken  :: !(Maybe Text)
  , _lcMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListChannels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcNextToken' - The token for the next set of results.
--
-- * 'lcMaxResults' - The maximum number of results to return in this request. The default value is 100.
listChannels
    :: ListChannels
listChannels = ListChannels' {_lcNextToken = Nothing, _lcMaxResults = Nothing}


-- | The token for the next set of results.
lcNextToken :: Lens' ListChannels (Maybe Text)
lcNextToken = lens _lcNextToken (\ s a -> s{_lcNextToken = a})

-- | The maximum number of results to return in this request. The default value is 100.
lcMaxResults :: Lens' ListChannels (Maybe Natural)
lcMaxResults = lens _lcMaxResults (\ s a -> s{_lcMaxResults = a}) . mapping _Nat

instance AWSRequest ListChannels where
        type Rs ListChannels = ListChannelsResponse
        request = get ioTAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 ListChannelsResponse' <$>
                   (x .?> "channelSummaries" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListChannels where

instance NFData ListChannels where

instance ToHeaders ListChannels where
        toHeaders = const mempty

instance ToPath ListChannels where
        toPath = const "/channels"

instance ToQuery ListChannels where
        toQuery ListChannels'{..}
          = mconcat
              ["nextToken" =: _lcNextToken,
               "maxResults" =: _lcMaxResults]

-- | /See:/ 'listChannelsResponse' smart constructor.
data ListChannelsResponse = ListChannelsResponse'
  { _lcrsChannelSummaries :: !(Maybe [ChannelSummary])
  , _lcrsNextToken        :: !(Maybe Text)
  , _lcrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListChannelsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrsChannelSummaries' - A list of "ChannelSummary" objects.
--
-- * 'lcrsNextToken' - The token to retrieve the next set of results, or @null@ if there are no more results.
--
-- * 'lcrsResponseStatus' - -- | The response status code.
listChannelsResponse
    :: Int -- ^ 'lcrsResponseStatus'
    -> ListChannelsResponse
listChannelsResponse pResponseStatus_ =
  ListChannelsResponse'
    { _lcrsChannelSummaries = Nothing
    , _lcrsNextToken = Nothing
    , _lcrsResponseStatus = pResponseStatus_
    }


-- | A list of "ChannelSummary" objects.
lcrsChannelSummaries :: Lens' ListChannelsResponse [ChannelSummary]
lcrsChannelSummaries = lens _lcrsChannelSummaries (\ s a -> s{_lcrsChannelSummaries = a}) . _Default . _Coerce

-- | The token to retrieve the next set of results, or @null@ if there are no more results.
lcrsNextToken :: Lens' ListChannelsResponse (Maybe Text)
lcrsNextToken = lens _lcrsNextToken (\ s a -> s{_lcrsNextToken = a})

-- | -- | The response status code.
lcrsResponseStatus :: Lens' ListChannelsResponse Int
lcrsResponseStatus = lens _lcrsResponseStatus (\ s a -> s{_lcrsResponseStatus = a})

instance NFData ListChannelsResponse where
