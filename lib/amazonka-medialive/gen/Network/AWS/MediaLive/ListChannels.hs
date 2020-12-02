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
-- Module      : Network.AWS.MediaLive.ListChannels
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces list of channels that have been created
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListChannels
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
    , lcrsChannels
    , lcrsNextToken
    , lcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for ListChannelsRequest
--
-- /See:/ 'listChannels' smart constructor.
data ListChannels = ListChannels'
  { _lcNextToken  :: !(Maybe Text)
  , _lcMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListChannels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcNextToken' - Undocumented member.
--
-- * 'lcMaxResults' - Undocumented member.
listChannels
    :: ListChannels
listChannels = ListChannels' {_lcNextToken = Nothing, _lcMaxResults = Nothing}


-- | Undocumented member.
lcNextToken :: Lens' ListChannels (Maybe Text)
lcNextToken = lens _lcNextToken (\ s a -> s{_lcNextToken = a})

-- | Undocumented member.
lcMaxResults :: Lens' ListChannels (Maybe Natural)
lcMaxResults = lens _lcMaxResults (\ s a -> s{_lcMaxResults = a}) . mapping _Nat

instance AWSPager ListChannels where
        page rq rs
          | stop (rs ^. lcrsNextToken) = Nothing
          | stop (rs ^. lcrsChannels) = Nothing
          | otherwise =
            Just $ rq & lcNextToken .~ rs ^. lcrsNextToken

instance AWSRequest ListChannels where
        type Rs ListChannels = ListChannelsResponse
        request = get mediaLive
        response
          = receiveJSON
              (\ s h x ->
                 ListChannelsResponse' <$>
                   (x .?> "channels" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListChannels where

instance NFData ListChannels where

instance ToHeaders ListChannels where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListChannels where
        toPath = const "/prod/channels"

instance ToQuery ListChannels where
        toQuery ListChannels'{..}
          = mconcat
              ["nextToken" =: _lcNextToken,
               "maxResults" =: _lcMaxResults]

-- | Placeholder documentation for ListChannelsResponse
--
-- /See:/ 'listChannelsResponse' smart constructor.
data ListChannelsResponse = ListChannelsResponse'
  { _lcrsChannels       :: !(Maybe [ChannelSummary])
  , _lcrsNextToken      :: !(Maybe Text)
  , _lcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListChannelsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrsChannels' - Undocumented member.
--
-- * 'lcrsNextToken' - Undocumented member.
--
-- * 'lcrsResponseStatus' - -- | The response status code.
listChannelsResponse
    :: Int -- ^ 'lcrsResponseStatus'
    -> ListChannelsResponse
listChannelsResponse pResponseStatus_ =
  ListChannelsResponse'
    { _lcrsChannels = Nothing
    , _lcrsNextToken = Nothing
    , _lcrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
lcrsChannels :: Lens' ListChannelsResponse [ChannelSummary]
lcrsChannels = lens _lcrsChannels (\ s a -> s{_lcrsChannels = a}) . _Default . _Coerce

-- | Undocumented member.
lcrsNextToken :: Lens' ListChannelsResponse (Maybe Text)
lcrsNextToken = lens _lcrsNextToken (\ s a -> s{_lcrsNextToken = a})

-- | -- | The response status code.
lcrsResponseStatus :: Lens' ListChannelsResponse Int
lcrsResponseStatus = lens _lcrsResponseStatus (\ s a -> s{_lcrsResponseStatus = a})

instance NFData ListChannelsResponse where
