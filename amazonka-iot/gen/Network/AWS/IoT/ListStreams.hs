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
-- Module      : Network.AWS.IoT.ListStreams
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the streams in your AWS account.
--
--
module Network.AWS.IoT.ListStreams
    (
    -- * Creating a Request
      listStreams
    , ListStreams
    -- * Request Lenses
    , lsNextToken
    , lsAscendingOrder
    , lsMaxResults

    -- * Destructuring the Response
    , listStreamsResponse
    , ListStreamsResponse
    -- * Response Lenses
    , lsrsNextToken
    , lsrsStreams
    , lsrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listStreams' smart constructor.
data ListStreams = ListStreams'
  { _lsNextToken      :: !(Maybe Text)
  , _lsAscendingOrder :: !(Maybe Bool)
  , _lsMaxResults     :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStreams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsNextToken' - A token used to get the next set of results.
--
-- * 'lsAscendingOrder' - Set to true to return the list of streams in ascending order.
--
-- * 'lsMaxResults' - The maximum number of results to return at a time.
listStreams
    :: ListStreams
listStreams =
  ListStreams'
    { _lsNextToken = Nothing
    , _lsAscendingOrder = Nothing
    , _lsMaxResults = Nothing
    }


-- | A token used to get the next set of results.
lsNextToken :: Lens' ListStreams (Maybe Text)
lsNextToken = lens _lsNextToken (\ s a -> s{_lsNextToken = a})

-- | Set to true to return the list of streams in ascending order.
lsAscendingOrder :: Lens' ListStreams (Maybe Bool)
lsAscendingOrder = lens _lsAscendingOrder (\ s a -> s{_lsAscendingOrder = a})

-- | The maximum number of results to return at a time.
lsMaxResults :: Lens' ListStreams (Maybe Natural)
lsMaxResults = lens _lsMaxResults (\ s a -> s{_lsMaxResults = a}) . mapping _Nat

instance AWSRequest ListStreams where
        type Rs ListStreams = ListStreamsResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListStreamsResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "streams" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListStreams where

instance NFData ListStreams where

instance ToHeaders ListStreams where
        toHeaders = const mempty

instance ToPath ListStreams where
        toPath = const "/streams"

instance ToQuery ListStreams where
        toQuery ListStreams'{..}
          = mconcat
              ["nextToken" =: _lsNextToken,
               "isAscendingOrder" =: _lsAscendingOrder,
               "maxResults" =: _lsMaxResults]

-- | /See:/ 'listStreamsResponse' smart constructor.
data ListStreamsResponse = ListStreamsResponse'
  { _lsrsNextToken      :: !(Maybe Text)
  , _lsrsStreams        :: !(Maybe [StreamSummary])
  , _lsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStreamsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrsNextToken' - A token used to get the next set of results.
--
-- * 'lsrsStreams' - A list of streams.
--
-- * 'lsrsResponseStatus' - -- | The response status code.
listStreamsResponse
    :: Int -- ^ 'lsrsResponseStatus'
    -> ListStreamsResponse
listStreamsResponse pResponseStatus_ =
  ListStreamsResponse'
    { _lsrsNextToken = Nothing
    , _lsrsStreams = Nothing
    , _lsrsResponseStatus = pResponseStatus_
    }


-- | A token used to get the next set of results.
lsrsNextToken :: Lens' ListStreamsResponse (Maybe Text)
lsrsNextToken = lens _lsrsNextToken (\ s a -> s{_lsrsNextToken = a})

-- | A list of streams.
lsrsStreams :: Lens' ListStreamsResponse [StreamSummary]
lsrsStreams = lens _lsrsStreams (\ s a -> s{_lsrsStreams = a}) . _Default . _Coerce

-- | -- | The response status code.
lsrsResponseStatus :: Lens' ListStreamsResponse Int
lsrsResponseStatus = lens _lsrsResponseStatus (\ s a -> s{_lsrsResponseStatus = a})

instance NFData ListStreamsResponse where
